(require 'clojure.string)

(def INPUT "7.input")

(defn split-numbers [s]
  (clojure.string/split s #":?\s+"))

(defn map-integer [xs]
  (map #(BigInteger. %) xs))

(defn combinations [ops [a b & rest]]
  (if b
    (->> (map #(combinations ops (cons (% a b) rest)) ops)
         (apply concat))
    [a]))

(defn is-operatable? [ops [val & numbers]]
  (->> numbers
       (combinations ops)
       (some (partial = val))))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map split-numbers)
       (map map-integer)
       (filter (partial is-operatable? [+ *]))
       (map first)
       (reduce +)))

(defn || [a b] (BigInteger. (str a b)))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map split-numbers)
       (map map-integer)
       (filter (partial is-operatable? [+ * ||]))
       (map first)
       (reduce +)))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))