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

(def other_users_and_ratings {"Veronica" {"The Strokes" 3.0, "Blues Traveler" 3.0, "Phoenix" 4.0, "Norah Jones" 5.0, "Slightly Stoopid" 2.5}, "Angelica" {"Broken Bells" 2.0, "The Strokes" 2.5, "Blues Traveler" 3.5, "Phoenix" 5.0, "Norah Jones" 4.5, "Vampire Weekend" 2.0, "Slightly Stoopid" 1.5}, "Bill" {"Broken Bells" 3.5, "Blues Traveler" 2.0, "Phoenix" 2.0, "Vampire Weekend" 3.0, "Deadmau5" 4.0, "Slightly Stoopid" 3.5}, "Jordyn" {"Broken Bells" 4.5, "The Strokes" 4.0, "Phoenix" 5.0, "Norah Jones" 5.0, "Vampire Weekend" 4.0, "Deadmau5" 4.0, "Slightly Stoopid" 4.5}, "Hailey" {"Broken Bells" 4.0, "The Strokes" 4.0, "Norah Jones" 4.0, "Vampire Weekend" 1.0, "Deadmau5" 1.0}, "Dan" {"Broken Bells" 4.0, "The Strokes" 4.0, "Blues Traveler" 3.0, "Phoenix" 3.0, "Vampire Weekend" 2.0, "Deadmau5" 4.5, "Slightly Stoopid" 4.5}, "Chan" {"Broken Bells" 1.0, "Blues Traveler" 5.0, "Phoenix" 5, "Norah Jones" 3.0, "Deadmau5" 1.0, "Slightly Stoopid" 1.0}, "Sam" {"Broken Bells" 2.0, "The Strokes" 5.0, "Blues Traveler" 5.0, "Phoenix" 5.0, "Norah Jones" 3.0, "Slightly Stoopid" 4.0}})

(fact "manhattan calculates manhattan distance between two users' ratings"
  (manhattan (users_and_ratings "Gary") (users_and_ratings "Gary")) => 0
  (manhattan (users_and_ratings "Gary") (users_and_ratings "Zak"))  => 19
  (manhattan (users_and_ratings "Gary") {})                         => 0
  (manhattan (other_users_and_ratings "Hailey") (other_users_and_ratings "Veronica")) => 2.0
  (manhattan (other_users_and_ratings "Hailey") (other_users_and_ratings "Jordyn")) => 7.5)

(fact "manhattan-comp compares two user_ratings relative to a third user"
  (manhattan-comp
    (users_and_ratings "Gary")
    (users_and_ratings "Gary")
    (users_and_ratings "Gary")) => 0
  (manhattan-comp
    (users_and_ratings "Gary")
    (users_and_ratings "Zak")
    (users_and_ratings "Josh")) => 1
  )

(fact "nearest-neighbor uses manhattan distance to determine users most alike"
  (->
    (nearest-neighbor users_and_ratings {"Gary" (users_and_ratings "Gary")})
    (first)
    (first)) => "Matt"
  (->
    (nearest-neighbor users_and_ratings {"Zak" (users_and_ratings "Zak")})
    (first)
    (first)) => "Josh")
