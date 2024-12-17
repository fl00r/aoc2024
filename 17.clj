(require 'clojure.string)

(def INPUT "17.input")

(defn re-seq-int [s]
  (->> s
       (re-seq #"\d+")
       (map #(Integer. %))))

(defn parse-data [[r1 r2 r3 _ r5]]
  (let [[a] (re-seq-int r1)
        [b] (re-seq-int r2)
        [c] (re-seq-int r3)
        p (re-seq-int r5)]
    {:a a
     :b b
     :c c
     :programm (vec p)
     :pointer 0
     :out []}))

(defn operand->combo [{:keys [a b c]} operand]
  (nth [0 1 2 3 a b c] operand))

(defn forward! [cfx] (update cfx :pointer + 2))

(defn adv [{:keys [a] :as cfx} operand]
  (->> (operand->combo cfx operand)
       (Math/pow 2)
       (quot a)
       (assoc cfx :a)
       (forward!)))

(defn bxl [{:keys [b] :as cfx} operand]
  (->> (.xor (biginteger b) (biginteger operand))
       (assoc cfx :b)
       (forward!)))

(defn bst [cfx operand]
  (->> (mod (operand->combo cfx operand) 8)
       (assoc cfx :b)
       (forward!)))

(defn jnz [{:keys [a] :as cfx} operand]
  (if (zero? a)
    (forward! cfx)
    (assoc cfx :pointer operand)))

(defn bxc [{:keys [b c] :as cfx} _operand]
  (->> (.xor (biginteger b) (biginteger c))
       (assoc cfx :b)
       (forward!)))

(defn out [cfx operand]
  (->> (mod (operand->combo cfx operand) 8)
       (int)
       (update cfx :out conj)
       (forward!)))

(defn bdv [{:keys [a] :as cfx} operand]
  (->> (operand->combo cfx operand)
       (Math/pow 2)
       (quot a)
       (assoc cfx :b)
       (forward!)))

(defn cdv [{:keys [a] :as cfx} operand]
  (->> (operand->combo cfx operand)
       (Math/pow 2)
       (quot a)
       (assoc cfx :c)
       (forward!)))

(def OPCODES [adv ;; 0
              bxl ;; 1
              bst ;; 2
              jnz ;; 3
              bxc ;; 4
              out ;; 5
              bdv ;; 6
              cdv ;; 7
              ])

(defn perform [{:keys [programm pointer] :as cfx}]
  (if (< pointer (count programm))
    (let [opcode (nth programm pointer)
          operand (nth programm (inc pointer))
          op (nth OPCODES opcode)]
      (perform (op cfx operand)))
    cfx))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (parse-data)
       (perform)
       :out
       (clojure.string/join ",")))

(defn reverse-engineer [a n {:keys [programm] :as cfx}]
  (let [programm' (take-last n programm)]
    (mapcat
     (fn [i]
       (let [a' (+ (* a 8) i)
             cfx' (assoc cfx :a a')
             out (:out (perform cfx'))]
         (when (= out programm')
           (if (= out programm)
             [a']
             (reverse-engineer a' (inc n) cfx)))))
     (range 0 8))))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (parse-data)
       (reverse-engineer 0 1)
       (apply min)))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))