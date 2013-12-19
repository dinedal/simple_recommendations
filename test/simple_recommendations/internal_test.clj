(ns simple-recommendations.internal-test
  (:require [clojure.test :refer :all]
    [simple-recommendations.internal :refer :all])
  (:use midje.sweet clojure.test simple-recommendations.core))


(facts
  (fact "merge-common-with is merge-with"
    (merge-common-with + {:foo 5} {:foo 5}) => (merge-with + {:foo 5} {:foo 5}))
  (fact "but only with common keys"
    (merge-common-with + {:bar 5} {:foo 5}) => {}
    (merge-common-with + {:bar 5 :foo 5 :d 1} {:foo 5 :bar 5}) => {:foo 10 :bar 10}))
