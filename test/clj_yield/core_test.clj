(ns clj-yield.core-test
  (:use [yield] :reload-all)
  (:use [clojure.test]))

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
         :exception)))


