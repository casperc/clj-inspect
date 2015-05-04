(ns clj-inspect.core-test
  (:require [clojure.test :refer :all]
            [clj-inspect.core :as i :refer :all]))

(defn do-test []
  ;; Create atom for tracking
  (def a (atom {}))

  ;; Defining a few functions
  (defn not-tracked []
    (Thread/sleep 100))
  (defn waiter []
    (Thread/sleep (rand 1000)))
  (defn testfn [i] i)
  (defn doubler [i]
    (not-tracked)
    (waiter)
    (* 2 i))
  
  ;; Track some function calls 
  (with-record-redefs [a [#'testfn #'doubler #'waiter]]
    (prn "record:")
    (prn "1: " (testfn 1))
    (prn "2: " (doubler 4))
    (prn "3: " (testfn 2))
    (prn "4: " (not-tracked)))

  (println "Tracking atom:")
  (prn a)

  (println "Timing report:")
  (print-timing-report @a)

  ;; Redef it to multiply by 3 instead of 2 to show that the replay uses the result from before, not the function  
  (defn doubler [i] (* 3 i)) 
  
  (with-replay-redef [@a [#'testfn #'doubler #'waiter]]
   (prn "replay:")
   (prn "1: " (testfn 1))
   (prn "2: " (doubler 4))
   (prn "3: " (testfn 2)))


)
