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
  "Pass the consumer of this yield sequence the exception."
  [yseq exception]
  (offer yseq (ExceptionContainer. exception)))

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
  (let [queue (java.util.concurrent.ArrayBlockingQueue. (int n) false)
        queue-atom (atom queue)
        ft (future
            (try
              (f queue-atom)
              (catch Throwable e
                (offer queue-atom (ExceptionContainer. e)))
              (finally (offer queue-atom *end-marker*)
                       (reset! queue-atom nil))))
        get-ele (fn get-ele [guard]
                  (let [ele (.take ^java.util.concurrent.BlockingQueue @guard) ]
                    (cond (= ele *end-marker*)
                          (do
                            ;; make it impossible for lingering
                            ;; threads to push anything else to the
                            ;; queue
                            (reset! queue-atom nil)
                            ;; dislodge any lingering threads
                            (.clear queue))
                          
                          (= (class ele) ExceptionContainer)
                          (throw (RuntimeException.
                                  (.exception ele)))
                          
                          true
                          (cons (if (= ele *nil-marker*) nil ele)
                                (lazy-seq (get-ele guard))))))]
    (let [guard
          (garbage_monitor.
           queue
           (fn [queue]
             (try
               ;; make it impossible for lingering
               ;; threads to push anything else to the
               ;; queue
               (reset! queue-atom nil)
               ;; dislodge any lingering threads
               (.clear queue)
               (future-cancel ft)
               (catch Exception e (.printStackTrace e)))))]
      (lazy-seq (get-ele guard)))))
