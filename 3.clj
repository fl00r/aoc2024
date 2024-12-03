(require '[clojure.string :as string])

(def INPUT "3.input")

(defn map-integer [xs]
  (map #(Integer. %) xs))

(defn map-mult [xs]
  (apply * xs))

(defn silver []
  (->> INPUT
       (slurp)
       (re-seq #"mul\((\d+),(\d+)\)")
       (map rest)
       (map map-integer)
       (map map-mult)
       (reduce +)))

(defn split-by [re s]
  (string/split s re))


(defn golden []
  (->> INPUT
       (slurp)
       (str "do()")
       (split-by #"don't\(\)")
       (map (partial split-by #"do\(\)"))
       (mapcat rest)
       (string/join)
       (re-seq #"mul\((\d+),(\d+)\)")
       (map rest)
       (map map-integer)
       (map map-mult)
       (reduce +)))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))