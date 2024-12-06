(require 'clojure.string)

(def INPUT "6.input")

(def GUARD "^")

(def WALL \#)

(def TURN {[0 -1] [1 0]
           [1 0] [0 1]
           [0 1] [-1 0]
           [-1 0] [0 -1]})

(defn map->position [map]
  (loop [[head & tail] map
         y 0]
    (if-let [x (clojure.string/index-of head GUARD)]
      [x y]
      (recur tail (inc y)))))

(defn ->moves [map x y dx dy]
  (loop [x' x
         y' y
         dx' dx
         dy' dy
         moves #{[x y]}]
    (let [x'' (+ x' dx')
          y'' (+ y' dy')
          forward (nth (nth map y'' "") x'' nil)
          valid? (some? forward)]
      (if valid?
        (if (= forward WALL)
          (let [[dx'' dy''] (get TURN [dx' dy'])]
            (recur x' y' dx'' dy'' moves))
          (recur x'' y'' dx' dy' (conj moves [x'' y''])))
        moves))))

(defn silver []
  (let [map (->> INPUT
                 (slurp)
                 (clojure.string/split-lines))
        [x y] (map->position map)]
    (->> (->moves map x y 0 -1)
         (count))))

(defn has-cycle [map x y dx dy hole]
  (loop [x' x
         y' y
         dx' dx
         dy' dy
         moves #{[x y dx dy]}]
    (let [x'' (+ x' dx')
          y'' (+ y' dy')
          forward (nth (nth map y'' "") x'' nil)
          valid? (some? forward)]
      (if valid?
        (if (or (= [x'' y''] hole)
                (= forward WALL))
          (let [[dx'' dy''] (get TURN [dx' dy'])]
            (recur x' y' dx'' dy'' moves))
          (if (get moves [x'' y'' dx' dy'])
            true
            (recur x'' y'' dx' dy' (conj moves [x'' y'' dx' dy']))))
        false))))

(defn golden []
  (let [map (->> INPUT
                 (slurp)
                 (clojure.string/split-lines))
        [x y] (map->position map)
        moves (->moves map x y 0 -1)]
    (->> (disj moves [x y])
         (filter (partial has-cycle map x y 0 -1))
         (count))))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))