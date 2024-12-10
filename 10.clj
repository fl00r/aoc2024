(require 'clojure.string)

(def INPUT "10.input")

(defn climb [mtrx val x y]
  (let [pnt (some-> mtrx
                    (nth y [])
                    (nth x nil)
                    (str)
                    (Integer.))]
    (cond (= pnt val 9) [{:x x :y y}]
          (= pnt val) (concat (climb mtrx (inc val) (dec x) y)
                              (climb mtrx (inc val) (inc x) y)
                              (climb mtrx (inc val) x (dec y))
                              (climb mtrx (inc val) x (inc y)))
          :else [])))

(defn examine [mtrx]
  (for [[y line] (map-indexed vector mtrx)
        x (range (count line))]
    (climb mtrx 0 x y)))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (examine)
       (map set)
       (map count)
       (reduce +)))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (examine)
       (map count)
       (reduce +)))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))