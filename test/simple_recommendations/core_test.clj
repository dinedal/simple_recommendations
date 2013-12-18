(ns simple-recommendations.core-test
  (:require [clojure.test :refer :all]
    [simple-recommendations.core :refer :all]
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


(fact "FIXME, I fail."
  (= 0 1) => true)
