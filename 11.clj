(require 'clojure.string)
(require 'clojure.math)

(def INPUT "11.input")

(defn split-integers [x]
  (->> (clojure.string/split x #" ")
       (map #(BigInteger. %))))

(defn ->map [acc num]
  (assoc acc num (bigint 1)))

(def CACHE (atom {}))

(defn logic [stone]
  (cond (= stone 0) [1]
        (even? (count (str stone)))
        (let [ord (->> stone
                       (str)
                       (count)
                       (* 0.5)
                       (clojure.math/pow 10)
                       (bigint))]
          [(quot stone ord)
           (rem stone ord)])
        :else [(* 2024 stone)]))

(defn magic [xs]
  (reduce (fn [acc [stone n]]
            (let [stones (or (get @CACHE stone)
                             (logic stone))]
              (reduce
               (fn [acc' new-stone]
                 (update acc' new-stone
                         (fn [n']
                           (+ (or n' 0)
                              n))))
               acc stones)))
          {}
          xs))


(defn blink [n xs]
  (-> (iterate magic xs)
      (nth n)))

(defn silver []
  (->> INPUT
       (slurp)
       (split-integers)
       (reduce ->map {})
       (blink 25)
       (vals)
       (reduce +)))

(defn golden []
  (->> INPUT
       (slurp)
       (split-integers)
       (reduce ->map {})
       (blink 75)
       (vals)
       (reduce +)))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))