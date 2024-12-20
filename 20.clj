(require 'clojure.string)

(def INPUT "20.input")
(def START \S)
(def END \E)
(def VOID \.)

(defn ->xy [lines ch]
  (->> lines
       (map-indexed vector)
       (some (fn [[y row]] (when-let [x (clojure.string/index-of row ch)]
                             [x y])))))

(defn ->ch [lines [x y]]
  (-> lines
      (nth y [])
      (nth x nil)))

(defn ->path [lines]
  (let [start (->xy lines START)]
    (loop [path []
           current start]
      (let [[x y] current
            prev (last path)
            n [x (inc y)]
            w [(inc x) y]
            s [x (dec y)]
            e [(dec x) y]
            [next ch] (some (fn [xy]
                              (when (not= xy prev)
                                (let [ch (->ch lines xy)]
                                  (when (get #{VOID END} ch)
                                    [xy ch]))))
                            [n w s e])]
        (if (= ch END)
          (conj path current next)
          (recur (conj path current) next))))))

(defn mnhtn [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn ->subcheats [dist cutoff path]
  (let [start (first path)]
    (->> path
         (map-indexed vector)
         (filter (fn [[idx xy]]
                   (let [m (mnhtn start xy)]
                     (and
                      (>= dist m)
                      (>= (- idx m) cutoff))))))))

(defn ->cheats [dist cutoff path]
  (->> path
       (partition-all (count path) 1)
       (mapcat (partial ->subcheats dist cutoff))))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (->path)
       (->cheats 2 100)
       (count)))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (->path)
       (->cheats 20 100)
       (count)))

(prn "SILVER" (silver))

(prn "GOLDEN" (golden))