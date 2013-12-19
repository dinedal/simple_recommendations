;; # Simple Recommendations
;; Takes (some) of the algorithms described in http://guidetodatamining.com/
;; and implements them in clojure. Not a direct port, as these versions are
;; designed to allow for querying against datasets which do not include
;; the user they are recommending for.
;;
;; This is ideal in cases where the data being used to source the
;; recommendations is partitioned and the query source may not have all the
;; data at a given time.
;;
;; Functions generally expect that take in partial data of the form:
;;     many_users_and_ratings => {:user {:item rating ...} ...}
;;       => {"Sally" {"Popsciles" 5 "Baseball" 1}
;;           "Billy" {"Popsciles" 5 "Baseball" 5 "Ice Cream" 3}}
;;     and
;;     one_user_and_ratings => {:user {:item rating ...}}
;;       => {"Jenny" {"Popsciles" 5 "Baseball" 3}}

(ns simple-recommendations.core
  (:require [simple-recommendations.internal :refer :all]
    [simple-recommendations.heap-sort :refer :all]
    [clojure.math.numeric-tower :as math]))


(defn minkowski
  [r user_ratings1 user_ratings2]
  (math/expt
    (reduce +
      (vals
        (merge-common-with
          (fn [a b] (math/expt (math/abs (- a b)) r))
          user_ratings1
          user_ratings2))) (/ 1 r)))

(def manhattan (partial minkowski 1))
(def euclidean (partial minkowski 2))


(defn pivot-compare
  "Returns a compare function that will compare,
  using the provided distance function, how close
  two different users are two a third"
  [distance_fn]
  (fn [pivot_user user_ratings1 user_ratings2]
    (compare
      (distance_fn pivot_user user_ratings1)
      (distance_fn pivot_user user_ratings2))))

(def manhattan-comp (pivot-compare manhattan))
(def euclidean-comp (pivot-compare euclidean))


(defn nearest-neighbor
  [users_and_ratings user_ratings]
  (let [user_name (-> user_ratings (keys) (first))
         ratings (user_ratings user_name)
         users_and_ratings_without_user
         (remove #(= user_name (first %1)) users_and_ratings)
         distance_comparator (partial manhattan-comp ratings)]
    (heap-sort-by
      (fn [user_rating_pair] (second user_rating_pair))
      distance_comparator
      users_and_ratings_without_user)))

(defn recommend
  [users_and_ratings user_ratings]
  (let [nearest_user_ratings
         (first (nearest-neighbor users_and_ratings user_ratings))
         nearest (first nearest_user_ratings)
         nearest_ratings (second nearest_user_ratings)
         user_name (-> user_ratings (keys) (first))
         not_yet_seen (filter
                        #(not (contains? (user_ratings user_name) %1))
                        (keys nearest_ratings))]
    (heap-sort-by last > (map #(find nearest_ratings %1) not_yet_seen))))
