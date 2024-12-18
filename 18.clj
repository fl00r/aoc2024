(require 'clojure.string)

(def INPUT "18.input")

(def WALL "#")
(def VOID ".")

(defn ->integers [xs]
  (->> xs
       (re-seq #"\d+")
       (map #(Integer. %))))

(defn ->map [w h]
  (->> (mapv (constantly VOID) (range w))
       (repeat)
       (take h)
       (vec)))

(defn fall [map [x y]]
  (assoc-in map [y x] WALL))

(defn walk [queue distances map]
  (if (seq queue)
    (let [[head & tail] queue
          [[x y] distance] head
          current-distance (get distances [x y])
          point (-> map
                    (nth y [])
                    (nth x nil))]
      (if (and (= VOID point)
               (or (not current-distance)
                   (< distance current-distance)))
        (let [distances' (assoc distances [x y] distance)
              queue' (conj tail
                           [[(dec x) y] (inc distance)]
                           [[(inc x) y] (inc distance)]
                           [[x (dec y)] (inc distance)]
                           [[x (inc y)] (inc distance)])]
          (recur queue' distances' map))
        (recur tail distances map)))
    distances))

(defn ->result [x y mp]
  (get mp [x y]))

(defn silver [bytes]
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map ->integers)
       (take bytes)
       (reduce fall (->map 71 71))
       (walk '([[0, 0] 0]) {})
       (->result 70 70)))

(defn golden []
  (let [points (-> INPUT
                   (slurp)
                   (clojure.string/split-lines))]
    (loop [left 1024
           right (count points)]
      (let [bytes (+ left (quot (- right left) 2))
            first (some? (silver bytes))
            second (some? (silver (dec bytes)))]
        (cond (not= first second) (nth points (dec bytes))
              first (recur bytes right)
              :else (recur left bytes))))))

(prn "SILVER" (silver 1024))

(prn "GOLDEN" (golden))