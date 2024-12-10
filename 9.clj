(require 'clojure.string)

(def INPUT "9.input")

(def SPACE ".")

(defn id-or-space [idx ch]
  (let [file? (zero? (rem idx 2))
        id (quot idx 2)
        amount (Integer. (str ch))]
    (if file?
      (repeat amount id)
      (repeat amount "."))))

(defn pack [unpacked left right]
  (if (> left right)
    (take left unpacked)
    (let [left-space? (= (nth unpacked left) SPACE)]
      (if left-space?
        (let [right-space? (= (nth unpacked right) SPACE)]
          (if right-space?
            (recur unpacked left (dec right))
            (recur (assoc unpacked left (nth unpacked right)) (inc left) (dec right))))
        (recur unpacked (inc left) right)))))

(defn silver []
  (let [unpacked (->> INPUT
                      (slurp)
                      (map-indexed id-or-space)
                      (flatten)
                      (vec))
        packed (pack unpacked 0 (dec (count unpacked)))]
    (->> packed
         (map-indexed vector)
         (map (partial apply *))
         (reduce +))))

(defn pack-blocks [str numbers]
  (if (empty? numbers)
    str
    (let [[number & tail] numbers
          number-n (count (clojure.string/split number #"\|"))
          dots (clojure.string/join "|" (repeat number-n "."))
          dots-index (clojure.string/index-of str dots)
          number-index (clojure.string/index-of str number)
          has-dots? (and dots-index (< dots-index number-index))]
      (if has-dots?
        (let [str' (-> str
                       (clojure.string/replace-first number dots)
                       (clojure.string/replace-first dots number))]
          (recur str' tail))
        (recur str tail)))))

(defn golden []
  (let [raw (->> INPUT
                 (slurp)
                 (map-indexed id-or-space))
        str (->> raw
                 (flatten)
                 (clojure.string/join "|"))
        numbers (->> raw
                     (partition-all 2)
                     (map first)
                     (map (partial clojure.string/join "|"))
                     (rest)
                     (reverse))
        packed (pack-blocks str numbers)]
    (->> (clojure.string/split packed #"\|")
         (map (fn [x] (if (= x ".") 0 (Integer. x))))
         (map-indexed vector)
         (map (partial apply *))
         (reduce +))))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))