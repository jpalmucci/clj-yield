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
;; generating thread.
(defrecord ExceptionContainer [exception])
;; end of the sequence
(defonce *end-marker* (Object.))

(defn- offer [queue item]
  (while (not (.offer ^java.util.concurrent.BlockingQueue @queue (if (nil? item) *nil-marker* item) 10 java.util.concurrent.TimeUnit/SECONDS))))

(defn yield [yseq x]
  "Append an element to the sequence 'yseq', created with
'with-yielding'. May block if there is no capacity in yseq.

If, while blocking, the output sequence is garbage collected, yield
will throw a java.lang.InterruptedException. The body of the
with-yielding should return."
  (try
    (offer yseq x)
    (catch NullPointerException e 
      (if (nil? @yseq)
        (throw (java.lang.InterruptedException. "Computing Garbage"))
        (throw e)))))

(defn yield-exception
  "Pass the consumer of this yield sequence the exception. Since
  nothing can be consumed after this exception is thrown, the
  exception will also be thrown in the current thread, at which point it should exit."
  [yseq exception]
  (offer yseq (ExceptionContainer. exception))
  (throw exception))

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

(defn with-yielding* [n f pos]
  (let [queue (atom 
               #_
               (com.adknife.util.BurstingBlockingQueue. (int n) (int (/ n 2)) false #_ (int n))
               (java.util.concurrent.ArrayBlockingQueue. (int n) false)
               #_
               (java.util.concurrent.SynchronousQueue. false #_ (int n)))
        ft (future
            (try
             (f queue)
             (catch Throwable e
               (offer queue (ExceptionContainer. e)))
             (finally (offer queue *end-marker*))))
        get-ele (fn get-ele [guard]
                  (let [ele (.take ^java.util.concurrent.BlockingQueue @queue) ]
                    (cond (= ele *end-marker*)
                          (let [q @queue]
                            ;; make it impossible for lingering
                            ;; threads to push anything else to the
                            ;; queue
                            (reset! queue nil)
                            ;; dislodge any lingering threads
                            (if (not (nil? q))
                              (.clear q))
                            ())

                            (= (class ele) ExceptionContainer)
                            (throw (RuntimeException.
                                    (.exception ele)))

                            true
                            (cons (if (= ele *nil-marker*) nil ele)
                                  (lazy-seq (get-ele guard))))))]
    (let [guard (upon-gc queue
                         (try
                           (let [q @queue]
                             ;; make it impossible for lingering
                             ;; threads to push anything else to the
                             ;; queue
                             (reset! queue nil)
                             ;; dislodge any lingering threads
                             (if (not (nil? q))
                               (.clear q))
                             (future-cancel ft))
                          (catch Exception e (.printStackTrace e))))]
      (lazy-seq (get-ele guard)))))

