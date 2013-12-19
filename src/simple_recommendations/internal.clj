;; Internal functions used by the public interface that's exposed in
;; simple-recommendations.core

(ns simple-recommendations.internal)

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
