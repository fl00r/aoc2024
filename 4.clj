(require 'clojure.string)

(def INPUT "4.input")

(def XMAS #{"XMAS" "SAMX"})
(def X-MAS #{"MMASS" "SSAMM" "MSAMS" "SMASM"})

(defn split-chars [s]
  (clojure.string/split s #""))

(defn get-word [xs i j di dj]
  (str
   (nth (nth xs (+ i (* di 0)) []) (+ j (* dj 0)) "")
   (nth (nth xs (+ i (* di 1)) []) (+ j (* dj 1)) "")
   (nth (nth xs (+ i (* di 2)) []) (+ j (* dj 2)) "")
   (nth (nth xs (+ i (* di 3)) []) (+ j (* dj 3)) "")))

(defn get-words [xs]
  (let [line (count (first xs))]
    (for [i (range (count xs))]
      (for [j (range line)]
        [(get-word xs i j 0 1)
         (get-word xs i j 1 1)
         (get-word xs i j 1 0)
         (get-word xs i j 1 -1)]))))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map split-chars)
       (get-words)
       (flatten)
       (filter XMAS)
       (count)))

(defn get-x-word [xs i j]
  (str
   (nth (nth xs (+ i 0) []) (+ j 0) "")
   (nth (nth xs (+ i 2) []) (+ j 0) "")
   (nth (nth xs (+ i 1) []) (+ j 1) "")
   (nth (nth xs (+ i 0) []) (+ j 2) "")
   (nth (nth xs (+ i 2) []) (+ j 2) "")))

(defn get-x-words [xs]
  (let [line (count (first xs))]
    (for [i (range (count xs))]
      (for [j (range line)] (get-x-word xs i j)))))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map split-chars)
       (get-x-words)
       (flatten)
       (filter X-MAS)
       (count)))

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))