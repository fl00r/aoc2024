(require 'clojure.string)

(def INPUT "14.input")
(def WIDTH 101)
(def HEIGHT 103)

(defn ->robot [line]
  (let [[x y dx dy] (->> line
                         (re-seq #"\-?\d+")
                         (map #(Integer. %)))]
    {:x x :y y :dx dx :dy dy :v 0}))

(defn move [steps {:keys [dx dy] :as robot}]
  (-> robot
      (update :x (fn [x] (mod (+ x (* dx steps)) WIDTH)))
      (update :y (fn [y] (mod (+ y (* dy steps)) HEIGHT)))
      (update :v inc)))

(defn ->quadrant [{:keys [x y]}]
  (when (and (not= x (quot WIDTH 2))
             (not= y (quot HEIGHT 2)))
    [(< x (quot WIDTH 2)) (> x (quot WIDTH 2))
     (< y (quot HEIGHT 2)) (> y (quot HEIGHT 2))]))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map ->robot)
       (map (partial move 100))
       (group-by ->quadrant)
       (filter first)
       (vals)
       (map count)
       (concat [0 0 0 0])
       (take-last 4)
       (reduce *)))

;; (defn draw [robots]
;;   (let [robots' (map (partial move 1) robots)
;;         robots'' (group-by (juxt :x :y) robots')]
;;     (prn (:v (first robots')))
;;     (->> (for [y (range HEIGHT)]
;;            (->> (for [x (range WIDTH)]
;;                   (or (some-> robots''
;;                               (get [x y])
;;                               (count))
;;                       "."))
;;                 (clojure.string/join "")))
;;          (clojure.string/join "\n")
;;          (print))
;;     (prn "DONE")
;;     robots'))

(def TREE "1111111111111111111111111111111")

(defn is-christmas-tree? [robots]
  (let [groupped-robots (group-by (juxt :x :y) robots)
        str (->> (for [y (range HEIGHT)
                       x (range WIDTH)]
                   (if (get groupped-robots [x y]) "1" "."))
                 (clojure.string/join ""))]
    ;; (prn str)
    (clojure.string/includes? str TREE)))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map ->robot)
       (iterate #(map (partial move 1) %))
       (filter is-christmas-tree?)
       (ffirst)
       :v))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))