(require 'clojure.string)

(def INPUT "10.input")

(defn map-integer [xs]
  (map #(Integer. (str %)) xs))

(defn climb [mtrx val x y]
  (let [pnt (some-> mtrx
                    (nth y nil)
                    (nth x nil))]
    (cond (= pnt val 9) [{:x x :y y}]
          (= pnt val) (concat (climb mtrx (inc val) (dec x) y)
                              (climb mtrx (inc val) (inc x) y)
                              (climb mtrx (inc val) x (dec y))
                              (climb mtrx (inc val) x (inc y)))
          :else [])))

(defn examine [mtrx]
  (for [[y row] (map-indexed vector mtrx)
        x (range (count row))]
    (climb mtrx 0 x y)))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map map-integer)
       (examine)
       (map set)
       (map count)
       (reduce +)))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map map-integer)
       (examine)
       (map count)
       (reduce +)))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))