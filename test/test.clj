(ns test
  (use yield)
  (use iterate)
  (use clojure.test))

(deftest with-yielding-test
  ;; check that the sequence is correctly generated
  (is (= (apply + (with-yielding [out 10]
                    (loop [x 1000]
                      (if (pos? x)
                        (do
                          (yield out x)
                          (recur (dec x)))))))
         500500))

  ;; Throw an exception, but don't read far enough to get it.
  ;; checks lazy semantics
  (is (= (first (with-yielding [out 10]
                  (loop [x 1000]
                    (if (pos? x)
                      (do
                        (yield out x)
                        (recur (dec x)))))
                  (throw (Exception. "exception"))))
         1000))

  ;; hit the exception to test the exception throwing mechanism
  (is (= (try
           (count 
            (with-yielding [out 10]
              (loop [x 1000]
                (if (pos? x)
                  (do
                    (yield out x)
                    (recur (dec x)))))
              (throw (Exception. "exception"))))
           (catch Exception e :exception))
         :exception))

  ;; make sure we get an exception if the sequence becomes garbage collectable
  (is (= (let [got-exception (atom false)]
           (with-yielding [out 10]
             (dotimes [x 1000]
               (try
                 (yield out x)
                 (catch java.lang.InterruptedException e
                   (reset! got-exception true)
                   (throw e)))))
           (System/gc)
           (System/gc)
           (Thread/sleep 500)
           @got-exception)
         true))

  )


