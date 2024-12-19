(require 'clojure.string)

(def INPUT "19.input")

(defn parse-input [[towels-row _ & designs]]
  (let [towels (clojure.string/split towels-row #", ")]
    {:towels towels
     :designs designs}))

(defn try-towels [cache design towels]
  (if (contains? cache design)
    cache
    (let [[cache' res]
          (reduce
           (fn [[cache'' res'] towel]
             (if (clojure.string/starts-with? design towel)
               (let [suffix (subs design (count towel))
                     cache''' (try-towels cache'' suffix towels)]
                 [cache''' (+ res' (get cache''' suffix))])
               [cache'' res']))
           [cache 0]
           towels)]
      (assoc cache' design res))))

(defn ->combinations [designs towels]
  (reduce (fn [cache design]
            (try-towels cache design towels))
          {"" 1}
          designs))

(defn calculate-designs [{:keys [towels designs]}]
  (let [combinations (->combinations designs towels)]
    (->> designs
         (map combinations)
         (filter pos?)
         (count))))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (parse-input)
       (calculate-designs)))

(defn calculate-combinations [{:keys [towels designs]}]
  (let [combinations (->combinations designs towels)]
    (->> designs
         (map combinations)
         (reduce +))))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (parse-input)
       (calculate-combinations)))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))