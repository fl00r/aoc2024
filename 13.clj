(require 'clojure.string)

(def INPUT "13.input")
(def A 3)
(def B 1)
(def MAX 100)

(defn int-seq [s]
  (->> s
       (re-seq #"\d+")
       (map #(Integer. %))))

(defn move-y [[x1 y1] [x2 y2]]
  [(/ y1 x1) (- y2 (* x2 (/ y1 x1)))])

(defn intersect [[a b] [x2 y2]]
  (/ b (- (/ y2 x2) a)))

(defn solve [eps [a b prize]]
  (let [[ax ay] (int-seq a)
        [bx by] (int-seq b)
        [px py] (map (partial + eps) (int-seq prize))
        N (-> [ax ay]
              (move-y [px py])
              (intersect [bx by])
              (/ bx))
        M (-> [bx by]
              (move-y [px py])
              (intersect [ax ay])
              (/ ax))]
    (when (and (integer? N) (integer? M))
      (bigint (+ (* M A) (* N B))))))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (partition-all 4)
       (map (partial solve 0))
       (keep identity)
       (reduce +)))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (partition-all 4)
       (map (partial solve 10000000000000))
       (keep identity)
       (reduce +)))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))