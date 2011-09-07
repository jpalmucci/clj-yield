(defproject clj-yield/clj-yield "1.1-SNAPSHOT"
  :description "A function like python's yield statement that can push items to a lazy sequence from arbitrary (non-lazy) code."
    :dev-dependencies [[lein-multi "1.1.0-SNAPSHOT"]]
    :multi-deps {"1.3" [[org.clojure/clojure "1.3.0-beta3"]]
                 "1.2" [[org.clojure/clojure "1.2.1"]]} )

