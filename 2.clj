(require '[clojure.string :as string])

(def INPUT "2.input")

(def split-lines string/split-lines)

(defn split-numbers [x]
  (string/split x #"\s+"))

(defn map-integer [xs]
  (map #(Integer. %) xs))

(defn is-safe? [xs]
  (->> xs
       (partition 2 1)
       (map #(apply - %))
       (map #(do [(<= 1 (abs %) 3)
                  (pos? %)]))
       (partition 2 1)
       (every? #(and (ffirst %) (apply = %)))))

(defn silver []
  (->> INPUT
       (slurp)
       (split-lines)
       (map split-numbers)
       (map map-integer)
       (filter is-safe?)
       (count)))

(defn butx [x xs]
  (let [cnt (count xs)
        lastx (- cnt x 1)]
    (concat (take x xs) (take-last lastx xs))))

(defn butx-all [xs]
  (map #(butx % xs) (range (count xs))))

(defn is-any-safe? [xs]
  (or (is-safe? xs)
      (some is-safe? (butx-all xs))))

(defn golden []
  (->> INPUT
       (slurp)
       (split-lines)
       (map split-numbers)
       (map map-integer)
       (filter is-any-safe?)
       (count)))

;; 252
(prn "SILVER" (silver))
(prn "GOLDEN" (golden))