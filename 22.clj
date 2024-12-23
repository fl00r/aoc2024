(require 'clojure.string)
(require 'clojure.math)

(def INPUT "22.input")

(defn mix [secret num]
  (.xor (biginteger secret) (biginteger num)))

(defn prune [secret]
  (mod secret 16777216))

(defn perform [secret op arg]
  (-> secret
      (op arg)
      (biginteger)
      (mix secret)
      (prune)))

(defn randomize [num]
  (-> num
      (perform * 64)
      (perform / 32)
      (perform * 2048)))

(defn silver [cnt]
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map #(Integer. %))
       (map #(iterate randomize %))
       (map #(nth % cnt))
       (reduce +)))

(defn ->sequence [acc xs]
  (let [seq (->> xs
                 (partition 2 1)
                 (map (partial apply -)))]
    (if (get acc seq) acc (assoc acc seq (last xs)))))

(defn ->sequences [acc xs]
  (->> xs
       (map #(rem % 10))
       (partition 5 1)
       (reduce ->sequence {})
       (merge-with + acc)))

(defn golden [cnt]
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map #(Integer. %))
       (map #(iterate randomize %))
       (map #(take cnt %))
       (reduce ->sequences {})
       (vals)
       (apply max)))

(prn "SILVER" (silver 2000))

(prn "GOLDEN" (golden 2000))