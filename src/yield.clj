(ns yield
  (:use clojure.test iterate))

;; ********************************************************************************
;; 

(deftype garbage_monitor
  [obj f]
  Object
  (finalize [this] (f obj))
  clojure.lang.IDeref
  (deref [this] (.obj this)))

(defmacro upon-gc [obj & body]
  "Returns a derefable object that contains 'obj'. The body will run
when the returned object becomes garbage collectable."
  `(garbage_monitor.
    ~obj
    (fn [o#]
      ~@body)))

;; ********************************************************************************

;; can't put a nil into a linked blocking queue, so use this object to mark them
(defonce *nil-marker* (Object.))
;; marker in the BlockingQueue that signals an exception in the
;; generating thread. The next element will be the exception that
;; occurred
(defonce *exception-marker* (Object.))
;; end of the sequence
(defonce *end-marker* (Object.))

(defn yield [yseq x]
  "Append an element to the sequence 'yseq', created with
'with-yielding'. May block if there is no capacity in yseq.

If, while blocking, the output sequence is garbage collected, yield
will throw a java.lang.InterruptedException. The body of the
with-yielding should return."
  (try 
    (.offer ^java.util.concurrent.LinkedBlockingQueue @yseq (if (nil? x) *nil-marker* x)
            10 java.util.concurrent.TimeUnit/DAYS)
    (catch NullPointerException e 
      (if (nil? @yseq)
        (throw (java.lang.InterruptedException. "Computing Garbage"))
        (throw e)))))

(defmacro with-yielding [[name n & {position :position}] & body]
  "Construct and return a sequence that is filled using 'yield' from
  within the body. The body can get up to 'n' elements ahead of the
  sequence consumer before blocking on 'yield'. For example:

user> (def *test-sequence*
        (yield/with-yielding [out 5]
          (loop [n 10]
            (if (pos? n)
              (do (println \"Yielding\" n)
                  (yield out n)
                  (recur (dec n)))))))
Yielding 10
Yielding 9
Yielding 8
Yielding 7
Yielding 6
Yielding 5
#'user/*test-sequence*
user> (first *test-sequence*)
Yielding 4
10

If 'position' is given, use that value to identify this sequence
for 'record-blockage'.
"

  `(with-yielding* ~n
     (bound-fn*
      (^{:once true} fn* [~name]
       ~@body))
     ~(if (nil? position)
        `(file-position 1)
        position)))

(def *blockage-map* (atom nil))

(defn make-persistent [map]
  "remove the atoms from a blockage map"
  (iterate/iter {for [loc {block :block non-block :non-block}] in map}
                {assoc {:block @block :non-block @non-block} key loc}))

(defmacro record-blockage [& body]
  "Record where and how many times a with-yielding seq is blocked
  because the consumers are not consuming fast enough. Good for
  detecting bottlenecks"
  `(let [old# @*blockage-map*]
     (try
       (reset! *blockage-map* {})
       ~@body
       (make-persistent @*blockage-map*)
       (finally
        (reset! *blockage-map* old#)))))
         

(defn with-yielding* [n f pos]
  (let [blockage-rec (and @*blockage-map*
                          (swap! *blockage-map*
                                 (fn [map]
                                   (if (contains? map pos)
                                     map
                                     (assoc map pos {:block (atom 0) :non-block (atom 0)}))))
                          (@*blockage-map* pos))

        queue (atom (java.util.concurrent.LinkedBlockingQueue. (int n)))
        ft (future
            (try
             (f queue)
             (catch Exception e
               (.offer @queue *exception-marker* 10 java.util.concurrent.TimeUnit/DAYS)
               (.offer @queue e 10 java.util.concurrent.TimeUnit/DAYS))
             (finally (.offer @queue *end-marker* 10 java.util.concurrent.TimeUnit/DAYS))))
        get-ele (fn get-ele [guard]
                  (if blockage-rec
                    (if (= (.remainingCapacity ^java.util.concurrent.LinkedBlockingQueue @queue) 0)
                        (swap! (blockage-rec :block) inc)
                        (swap! (blockage-rec :non-block) inc)))
                  (let [ele (.take ^java.util.concurrent.LinkedBlockingQueue @queue) ]
                    (cond (= ele *end-marker*) ()

                          (= ele *exception-marker*) 
                          (throw (RuntimeException. 
                                  (.take @queue)))

                          true
                          (cons (if (= ele *nil-marker*) nil ele)
                                (lazy-seq (get-ele guard))))))]
    (let [guard (upon-gc queue
                         (try
                           (let [q @queue]
                             (reset! queue nil)
                             (.clear q))
                          (catch Exception e (.printStackTrace e))))]
      (lazy-seq (get-ele guard)))))

