(ns simple-recommendations.core-test
  (:require [clojure.test :refer :all]
    [simple-recommendations.core :refer :all]
    [simple-recommendations.internal :refer :all]
    [clojure-csv.core :as csv]
    [clojure.string :as string])
  (:use midje.sweet clojure.test simple-recommendations.core))


;; Loads CSV test data, parses it into a more usable form of {user {movie rating}}
(def users_and_ratings
  (let [raw_data (csv/parse-csv (slurp "./test/Movie_Ratings.csv"))
         header_row (first raw_data)
         movie_rows (rest raw_data)
         users_by_index (reduce conj (map-indexed hash-map (rest header_row)))]
    (reduce #(merge-with conj %1 %2)
      (map (fn [movie_row]
             (let [movie (first movie_row)
                    ratings (rest movie_row)]
               (reduce merge
                 (remove nil?
                   (map-indexed
                     (fn [idx rating]
                       (if (not (string/blank? rating))
                         {(users_by_index idx)
                           {movie (Integer/parseInt rating)}}
                         nil))
                     ratings))))) movie_rows))))

(fact "manhattan calculates manhattan distance between two users' ratings"
  (manhattan (users_and_ratings "Gary") (users_and_ratings "Gary")) => 0
  (manhattan (users_and_ratings "Gary") (users_and_ratings "Zak"))  => 19
  (manhattan (users_and_ratings "Gary") {})                         => 0)

(fact "nearest-neighbor uses manhattan distance to determine users most alike"
  (nearest-neighbor users_and_ratings "Gary") => '([5 "Matt"] [9 "Jessica"] [11 "Josh"] [15 "Heather"] [15 "greg"] [16 "Erin"] [16 "vanessa"] [18 "Chris"] [18 "Katherine"] [19 "Bryan"] [19 "Patrick C"] [19 "Patrick T"] [19 "Zak"] [20 "Amy"] [20 "Jonathan"] [21 "Zwe"] [21 "aaron"] [21 "ben"] [22 "Stephen"] [22 "Valerie"] [23 "Thomas"] [24 "Jeff"] [26 "brian"])
  (nearest-neighbor users_and_ratings "Zak")  => '([5 "Josh"] [6 "Matt"] [7 "Erin"] [8 "Jessica"] [9 "Heather"] [10 "Jonathan"] [11 "Amy"] [11 "aaron"] [11 "brian"] [12 "Katherine"] [13 "Bryan"] [13 "Stephen"] [14 "Zwe"] [15 "Jeff"] [15 "Patrick C"] [15 "ben"] [15 "greg"] [16 "Patrick T"] [18 "Chris"] [19 "Gary"] [19 "vanessa"] [22 "Valerie"] [24 "Thomas"]))
