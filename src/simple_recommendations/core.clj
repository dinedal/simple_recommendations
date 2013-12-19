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
    [simple-recommendations.heap-sort :refer :all]))


(defn manhattan
  [user_ratings1 user_ratings2]
  (reduce +
    (vals
      (merge-common-with
        (fn [a b] (Math/abs (- a b)))
        user_ratings1
        user_ratings2))))


(defn nearest-neighbor
  [users_and_ratings user_ratings]
  (let [user_name (-> user_ratings (keys) (first))
         ratings (user_ratings user_name)
         users_and_ratings_without_user
         (remove #(= user_name (first %1)) users_and_ratings)]
    (heap-sort-by
      (fn [user_rating_pair] (second user_rating_pair))
      (fn [a b] (compare (manhattan ratings a) (manhattan ratings b)))
      users_and_ratings_without_user)))

(defn recommend
  [users_and_ratings user_ratings]
  (let [nearest_user_ratings
         (first (nearest-neighbor-sort-nd users_and_ratings user_ratings))
         nearest (first nearest_user_ratings)
         nearest_ratings (second nearest_user_ratings)
         user_name (-> user_ratings (keys) (first))
         not_yet_seen (filter
                        #(not (contains? (user_ratings user_name) %1))
                        (keys nearest_ratings))]
    (heap-sort-by last > (map #(find nearest_ratings %1) not_yet_seen))))
