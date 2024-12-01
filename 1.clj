(require '[clojure.string :as string])

(def INPUT "1.input")

(def split-lines string/split-lines)

(defn split-numbers [x]
  (string/split x #"\s+"))

(defn map-integer [xs]
  (map #(Integer. %) xs))

(defn transpose [xs]
  (apply map list xs))

(defn diff [[l r]]
  (abs (- l r)))

(defn silver []
  (->> INPUT
       (slurp)
       (split-lines)
       (map split-numbers)
       (map map-integer)
       (transpose)
       (map sort)
       (transpose)
       (map diff)
       (reduce +)))

(defn convert-second-to-map [[l r]]
  [l (->> r
          (group-by identity)
          (map (fn [[x xs]] [x (count xs)]))
          (into {}))])

(defn calculate-similarity [[xs mp]]
  (->> xs
       (map #(* % (get mp % 0)))
       (reduce +)))

(defn gold []
  (->> INPUT
       (slurp)
       (split-lines)
       (map split-numbers)
       (map map-integer)
       (transpose)
       (convert-second-to-map)
       (calculate-similarity)))

(prn "SILVER" (silver))
(prn "GOLD" (gold))