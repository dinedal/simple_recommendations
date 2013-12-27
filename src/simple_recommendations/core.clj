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


(defn pearson
  "This is an approximation of the pearson coefficent,
   a measure of how much two users agree on ratings"
  [user_ratings1 user_ratings2]
  (let [sums
         (reduce (fn [a b] {:xy  (+ (a :xy) (b :xy))
                             :x  (+ (a :x)  (b :x))
                             :y  (+ (a :y)  (b :y))
                             :x2 (+ (a :x2) (b :x2))
                             :y2 (+ (a :y2) (b :y2))
                             :n  (+ (a :n)  (b :n))
                             })
           (vals
             (merge-common-with
               (fn [a b]
                 {:xy (* a b)
                   :x a
                   :y b
                   :x2 (math/expt a 2)
                   :y2 (math/expt b 2)
                   :n 1
                   })
               user_ratings1
               user_ratings2)))
         denominator (*
                       (math/sqrt
                         (- (sums :x2) (/ (math/expt (sums :x) 2) (sums :n))))
                       (math/sqrt
                         (- (sums :y2) (/ (math/expt (sums :y) 2) (sums :n)))))]
    (if (= (double denominator) (double 0)) 0.0
      (/ (- (sums :xy) (/ (* (sums :x) (sums :y)) (sums :n))) denominator))))

(defn inverse-pearson
  "Pearson approximation but inverted for sorting by agreement
   instead of disagree-ment"
  [user_ratings1 user_ratings2]
  (* (pearson user_ratings1 user_ratings2) -1))


(defn cosine-similarity
  [user_ratings1 user_ratings2]
  (let [all_keys (reduce conj (keys user_ratings1) (keys user_ratings2))]
    (map
      (fn [key] (let [a (or (user_ratings1 key) 0)
                       b (or (user_ratings2 key) 0)]
                  {:x a
                    :y b
                    :x2 (math/expt a 2)
                    :y2 (math/expt b 2)
                    :xy (* a b)
                    }))
      all_keys)))

(defn cosine-similarity
  [user_ratings1 user_ratings2]
  (let [all_keys (distinct (reduce conj (keys user_ratings1) (keys user_ratings2)))
         data_with_placeholders
         (map
           (fn [key] (let [a (or (user_ratings1 key) 0)
                            b (or (user_ratings2 key) 0)]
                       {:x a
                         :y b
                         :x2 (math/expt a 2)
                         :y2 (math/expt b 2)
                         :xy (* a b)
                         }))
           all_keys)
         sums
         (reduce
           (fn [a b] {:x (+ (a :x) (b :x))
                       :y (+ (a :y) (b :y))
                       :x2 (+ (a :x2) (b :x2))
                       :y2 (+ (a :y2) (b :y2))
                       :xy (+ (a :xy) (b :xy))})
           data_with_placeholders)]
    (/ (sums :xy) (* (math/sqrt (sums :x2)) (math/sqrt (sums :y2))))))


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
(def pearson-comp   (pivot-compare pearson))
(def inverse-pearson-comp   (pivot-compare inverse-pearson))


(defn nearest-neighbor
  [comp-fn users_and_ratings user_ratings]
  (let [user_name (-> user_ratings (keys) (first))
         ratings (user_ratings user_name)
         users_and_ratings_without_user
         (remove #(= user_name (first %1)) users_and_ratings)
         distance_comparator (partial comp-fn ratings)]
    (heap-sort-by
      (fn [user_rating_pair] (second user_rating_pair))
      distance_comparator
      users_and_ratings_without_user)))

(defn recommend
  [k n comp-fn sort-fn users_and_ratings user_ratings]
  (let [nearests (take k (nearest-neighbor sort-fn users_and_ratings user_ratings))
         distances (map (fn [user_and_ratings]
                          {(first user_and_ratings)
                            (comp-fn
                              (second user_and_ratings)
                              (first (vals user_ratings)))}) nearests)
         total_distance (reduce + (map #(-> %1 vals first) distances))
         weights (reduce conj
                   (map (fn [users_and_ratings]
                          {(first (keys users_and_ratings))
                            (/ (first (vals users_and_ratings)) total_distance)})
                     distances))
         ratings (first (vals user_ratings))
         ]
    (->> (reduce #(merge-with + %1 %2)
          (map (fn [neighbor_ratings]
                 (let [neighbor (first neighbor_ratings)
                        compare_ratings (second neighbor_ratings)
                        to_reccomend
                        (remove (set (keys ratings)) (keys compare_ratings))
                        ]
                   (if (some identity to_reccomend)
                     (reduce conj
                       (map (fn [item]
                              {item
                                (* (compare_ratings item) (weights neighbor))}
                              ) to_reccomend))
                     {}))
                 ) nearests))
      (sort-by val)
      (reverse))
    ))
