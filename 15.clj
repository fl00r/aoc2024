(require 'clojure.string)

(def INPUT "15.input")

(def WALL \#)
(def GOOD \O)
(def LGOOD \[)
(def RGOOD \])
(def VOID \.)
(def ROBOT \@)

(def UP \^)
(def RIGHT \>)
(def DOWN \v)
(def LEFT \<)

(defn parse-map [data]
  (->> data
       (clojure.string/split-lines)
       (mapv vec)))

(defn parse-moves [data]
  (->> data
       (clojure.string/split-lines)
       (mapcat vec)))

(defn map->robot [map]
  (->> map
       (map-indexed vector)
       (some (fn [[y row]]
               (let [x (.indexOf row ROBOT)]
                 (when (not= x -1)
                   [x y]))))))

(defn push [map x y dx dy xs]
  (when map
    (let [x' (+ x dx)
          y' (+ y dy)
          point (-> map
                    (nth y' [])
                    (nth x' nil))]
      (condp = point
        GOOD (push map x' y' dx dy xs)
        LGOOD (if (contains? xs (inc x'))
                (push map x' y' dx dy xs)
                (let [xs' (conj xs (inc x'))]
                  (push (push map x' y' dx dy xs') (inc x') y' dx dy xs')))
        RGOOD (if (contains? xs (dec x'))
                (push map x' y' dx dy xs)
                (let [xs' (conj xs (dec x'))]
                  (push (push map x' y' dx dy xs') (inc x') y' dx dy xs')))
        ROBOT (push map x' y' dx dy xs)
        nil))))

(defn move [map robot moves]
  (loop [map' map
         robot' robot
         [move & tail] moves]
    (let [[x y] robot
          [dx dy] (condp = move
                    UP [0 -1]
                    RIGHT [1 0]
                    DOWN [0 1]
                    LEFT [-1 0])
          [map'' robot''] (push map' x y dx dy #{x})]
      (if (seq tail)
        (recur (or map'' map')
               (or robot'' robot')
               tail)
        (or map'' map')))))

(defn calculate-gps [map]
  (->> (for [[y row] (map-indexed vector map)
             [x ch] (map-indexed vector row)]
         (when (= ch GOOD) (+ x (* 100 y))))
       (keep identity)
       (reduce +)))

(defn silver []
  (let [[map-data moves-data] (-> INPUT
                                  (slurp)
                                  (clojure.string/split #"\n\n"))
        map (parse-map map-data)
        moves (parse-moves moves-data)
        robot (map->robot map)
        map' (move map robot moves)
        result (calculate-gps map')]
    result))

(def GOLDEN-MAP
  {VOID [VOID VOID]
   WALL [WALL WALL]
   GOOD [LGOOD RGOOD]
   ROBOT [ROBOT VOID]})

(defn parse-golden-map [data]
  (->> data
       (clojure.string/split-lines)
       (mapv (partial mapcat GOLDEN-MAP))))

#_(defn print-map [xs]
    (print (str (char 27) "[2J\n"))
    (->> xs
         (map (partial clojure.string/join ""))
         (clojure.string/join "\n")
         (print))
    (flush))



(defn golden []
  (let [[map-data moves-data] (-> INPUT
                                  (slurp)
                                  (clojure.string/split #"\n\n"))
        map (parse-golden-map map-data)
        moves (parse-moves moves-data)
        robot (map->robot map)
        ;; map' (move-golden map robot moves)
        ;; result (calculate-gps map')
        ]
    ;; (print-map map)
    #_result))

(prn "SILVER" (silver))
;; 1456590
(prn "GOLDEN" (golden))