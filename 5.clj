(require 'clojure.string)
(require 'clojure.set)

(def INPUT "5.input")

(defn split-bar [s]
  (clojure.string/split s #"\|"))

(defn map-integer [xs]
  (map #(Integer. %) xs))

(defn split-comma [s]
  (clojure.string/split s #","))

(defn ->identity-set [[k xs]]
  [k (set (map second xs))])

(defn is-valid-rule? [rules [head & tail]]
  (->> (get rules head #{})
       (clojure.set/difference (set tail))
       (empty?)))

(defn validate-rule [rules xs]
  (->> xs
       (partition-all (count xs) 1)
       (every? (partial is-valid-rule? rules))))

(defn middle [xs]
  (nth xs (quot (count xs) 2)))

(defn silver []
  (let [input (slurp INPUT)
        [input1 input2] (clojure.string/split input #"\n\n")
        rules (->> input1
                   (clojure.string/split-lines)
                   (map split-bar)
                   (map map-integer)
                   (group-by first)
                   (map ->identity-set)
                   (into {}))]
    (->> input2
         (clojure.string/split-lines)
         (map split-comma)
         (map map-integer)
         (filter (partial validate-rule rules))
         (map middle)
         (reduce +))))

(defn reorder [rules xs]
  (sort-by
   (fn [x] (-> (get rules x #{})
               (clojure.set/difference (set xs))
               (count)))
   xs))

(defn golden []
  (let [input (slurp INPUT)
        [input1 input2] (clojure.string/split input #"\n\n")
        rules (->> input1
                   (clojure.string/split-lines)
                   (map split-bar)
                   (map map-integer)
                   (group-by first)
                   (map ->identity-set)
                   (into {}))]
    (->> input2
         (clojure.string/split-lines)
         (map split-comma)
         (map map-integer)
         (remove (partial validate-rule rules))
         (map (partial reorder rules))
         (map middle)
         (reduce +))))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))
