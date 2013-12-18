(ns simple-recommendations.core)


(defn merge-common-with [f m1 m2]
  (let [[a b] (if (< (count m1) (count m2))
                [m1 m2]
                [m2 m1])]
    (persistent!
     (reduce-kv (fn [out k v]
                  (if (contains? b k)
                    (assoc! out k (f (get a k) (get b k)))
                    out))
                (transient {})
                a))))

(defn manhattan
  [user_ratings1 user_ratings2]
  (reduce +
    (vals
      (merge-common-with
        (fn [a b] (Math/abs (- a b)))
        user_ratings1
        user_ratings2))))


(defn nearest-neighbor
  [users_and_ratings user]
  (let [users (keys users_and_ratings)]
    (sort
      (remove nil?
        (map (fn [compare_user]
               (if (not= user compare_user)
                 [(manhattan
                    (users_and_ratings user)
                    (users_and_ratings compare_user))
                   compare_user]))
          users)))))


(defn recommend
  [users_and_ratings user]
  (let [nearest (second (first (nearest-neighbor users_and_ratings user)))
         nearest_ratings (users_and_ratings nearest)
         user_ratings (users_and_ratings user)
         not_yet_seen (filter
                        #(not (contains? user_ratings %1))
                        (keys nearest_ratings))]
    (sort-by last > (map #(find nearest_ratings %1) not_yet_seen))))
