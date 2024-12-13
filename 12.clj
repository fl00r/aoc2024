(require 'clojure.string)

(def INPUT "12.input")

(defn split-chars [x]
  (clojure.string/split x #""))

(defn crawl [{:keys [visited matrix regions] :as coef} x y ch & [dir]]
  (let [ch' (-> matrix
                (nth y [])
                (nth x nil))
        last-region-n (dec (count regions))]
    (cond (not= ch ch') (update-in coef [:regions last-region-n] conj [:border dir x y])
          (contains? visited [x y]) coef
          :else (-> coef
                    (update-in [:regions last-region-n] conj [:region ch x y])
                    (update :visited conj [x y])
                    (crawl (dec x) y ch :left)
                    (crawl (inc x) y ch :right)
                    (crawl x (dec y) ch :top)
                    (crawl x (inc y) ch :bottom)))))

(defn get-region [{:keys [visited] :as coef} [x y ch]]
  (if (contains? visited [x y])
    coef
    (crawl (update coef :regions conj []) x y ch)))

(defn calculate-perimeter-by-area [xs]
  (let [perimeter (->> xs
                       (filter (comp #{:border} first))
                       (count))
        area (->> xs
                  (filter (comp #{:region} first))
                  (count))]
    (* perimeter area)))

(defn silver []
  (let [matrix (->> INPUT
                    (slurp)
                    (clojure.string/split-lines)
                    (map split-chars))
        points (for [[y row] (map-indexed vector matrix)
                     [x ch] (map-indexed vector row)]
                 [x y ch])
        coef {:matrix matrix
              :visited #{}
              :regions []}]
    (->> points
         (reduce get-region coef)
         :regions
         (map calculate-perimeter-by-area)
         (reduce +))))

(defn is-line? [[[x1 y1] [x2 y2]]]
  (and (= x1 x2) (= y1 (dec y2))))

(defn count-lines [xs]
  (->> xs
       (sort-by identity)
       (partition 2 1)
       (remove is-line?)
       (count)
       (inc)))

(defn get-lines [[dir xs]]
  (case dir
    :left (->> xs (map rest) (map vec) (count-lines))
    :right (->> xs (map rest) (map vec) (count-lines))
    :top (->> xs (map rest) (map reverse) (map vec) (count-lines))
    :bottom (->> xs (map rest) (map reverse) (map vec) (count-lines))))

(defn calculate-lines-by-area [xs]
  (let [lines (->> xs
                   (filter (comp #{:border} first))
                   (map rest)
                   (group-by first)
                   (map get-lines)
                   (reduce +))
        area (->> xs
                  (filter (comp #{:region} first))
                  (count))]
    (* lines area)))

(defn golden []
  (let [matrix (->> INPUT
                    (slurp)
                    (clojure.string/split-lines)
                    (map split-chars))
        points (for [[y row] (map-indexed vector matrix)
                     [x ch] (map-indexed vector row)]
                 [x y ch])
        coef {:matrix matrix
              :visited #{}
              :regions []}]
    (->> points
         (reduce get-region coef)
         :regions
         (map calculate-lines-by-area)
         (reduce +))))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))