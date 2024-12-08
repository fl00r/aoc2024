(require 'clojure.string)
(require 'clojure.set)

(def INPUT "8.input")

(def VOID \.)

(defn map-line [y xs]
  (->> xs
       (map-indexed (fn [x ch] [ch [x y]]))
       (remove (comp #{VOID} first))))

(defn assoc-last-to-first [acc [key xs]]
  (->> xs
       (map last)
       (assoc acc key)))

(defn all-pairs [[head & tail]]
  (when (seq tail)
    (concat (map #(do [head %]) tail)
            (all-pairs tail))))

(defn pair->antinodes [[[x1 y1] [x2 y2]]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    [[(+ x1 dx) (+ y1 dy)]
     [(- x2 dx) (- y2 dy)]]))

(defn is-inbound? [width height [x y]]
  (and (<= 0 x (dec width))
       (<= 0 y (dec height))))

(defn silver []
  (let [lines (->> INPUT
                   (slurp)
                   (clojure.string/split-lines))
        height (count lines)
        width (count (first lines))
        antennas (->> lines
                      (map-indexed map-line)
                      (apply concat)
                      (group-by first)
                      (reduce assoc-last-to-first {}))
        antinodes (->> antennas
                       (vals)
                       (mapcat all-pairs)
                       (mapcat pair->antinodes)
                       (filter (partial is-inbound? width height)))]
    (count (set antinodes))))

(defn pair->antinode [w h x y dx dy]
  (let [x' (+ x dx)
        y' (+ y dy)]
    (when (and (<= 0 x' w)
               (<= 0 y' h))
      (concat [[x' y']] (pair->antinode w h x' y' dx dy)))))

(defn pair->antinodes' [width height [[x1 y1] [x2 y2]]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (concat [[x1 y1] [x2 y2]]
            (pair->antinode (dec width)
                            (dec height)
                            x1 y1 dx dy)
            (pair->antinode (dec width)
                            (dec height)
                            x2 y2 (* -1 dx) (* -1 dy)))))

(defn golden []
  (let [lines (->> INPUT
                   (slurp)
                   (clojure.string/split-lines))
        height (count lines)
        width (count (first lines))
        antennas (->> lines
                      (map-indexed map-line)
                      (apply concat)
                      (group-by first)
                      (reduce assoc-last-to-first {}))
        antinodes (->> antennas
                       (vals)
                       (mapcat all-pairs)
                       (mapcat (partial pair->antinodes' width height)))]
    (count (set antinodes))))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))