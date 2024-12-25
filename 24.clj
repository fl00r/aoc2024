(require 'clojure.string)
(require 'clojure.set)
(require 'clojure.pprint)

(def INPUT "24.input")

(defn ->system [acc line]
  (cond (empty? line) acc

        (clojure.string/includes? line ":")
        (let [[wire value] (clojure.string/split line #": ")]
          (assoc acc wire [:value (Integer. value)]))

        :else
        (let [[wire1 op wire2 wire3] (clojure.string/split line #" (-> )?")]
          (assoc acc wire3 [(keyword op) wire1 wire2]))))

(defmulti solve
  (fn [system k]
    (-> system
        (get k)
        (first))))

(defmethod solve :value [system _] system)

(defmethod solve :AND [system k]
  (let [[_ w1 w2] (get system k)
        system' (-> system
                    (solve w1)
                    (solve w2))]
    (->> (bit-and (-> system' (get w1) (last))
                  (-> system' (get w2) (last)))
         (conj [:value])
         (assoc system' k))))

(defmethod solve :OR [system k]
  (let [[_ w1 w2] (get system k)
        system' (-> system
                    (solve w1)
                    (solve w2))]
    (->> (bit-or (-> system' (get w1) (last))
                 (-> system' (get w2) (last)))
         (conj [:value])
         (assoc system' k))))

(defmethod solve :XOR [system k]
  (let [[_ w1 w2] (get system k)
        system' (-> system
                    (solve w1)
                    (solve w2))]
    (->> (bit-xor (-> system' (get w1) (last))
                  (-> system' (get w2) (last)))
         (conj [:value])
         (assoc system' k))))

(defn run [system]
  (reduce solve system (keys system)))

(defn x? [[k _]] (clojure.string/starts-with? k "x"))
(defn y? [[k _]] (clojure.string/starts-with? k "y"))
(defn z? [[k _]] (clojure.string/starts-with? k "z"))

(defn ->decimal [acc bit] (+ (* 2 acc) bit))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (reduce ->system {})
       (run)
       (filter z?)
       (sort)
       (map last)
       (map last)
       (reverse)
       (reduce ->decimal 0)))

(defn bit-sum [x y]
  (->> (map vector (reverse x) (reverse y))
       (reduce
        (fn [[h & rest] [x' y']]
          (let [z' (+ h x' y')
                n (quot z' 2)
                c (rem z' 2)]
            (conj rest c n)))
        (list 0))))

(defn first-bit-off [z1 z2]
  (or (->> (map vector (reverse z1) (reverse z2))
           (map-indexed vector)
           (some (fn [[idx [l r]]] (when (not= l r) idx))))
      10000))

(defn ->wires [system k]
  (let [[op w1 w2] (get system k)]
    (if (= op :value) #{}
        (clojure.set/union #{k} (->wires system w1) (->wires system w2)))))

(defn nested [system k]
  (let [[op w1 w2] (get system k)]
    (if (= op :value)
      k
      [op (nested system w1) (nested system w2)])))

(defn ->nested [system]
  (->> system
       (keys)
       (filter (fn [k] (clojure.string/starts-with? k "z")))
       (sort)
       (map (partial nested system))))

(defn ->reversed-system [acc line]
  (cond (empty? line) acc

        (clojure.string/includes? line ":")
        acc

        :else
        (let [[wire1 op wire2 wire3] (clojure.string/split line #" (-> )?")]
          (assoc acc [(keyword op) wire1 wire2] wire3))))

(defn ->instruction [system op]
  [op (get system op)])

(defn assemble-adder [bits system]
  (let [z0 [:XOR #{"x00" "y00"}]
        c0 [:AND #{"x00" "y00"}]]
    (loop [bit 1
           instructions [(->instruction system z0)
                         (->instruction system c0)]
           carry (get system c0)]
      (if (> bit bits)
        instructions
        (let [x (format "x%02d" bit)
              y (format "y%02d" bit)
              xy-xor (->instruction system [:XOR #{x y}])
              z (->instruction system [:XOR #{(last xy-xor) carry}])
              xy-and (->instruction system [:AND #{x y}])
              carry-or (->instruction system [:AND #{(last xy-xor)
                                                     carry}])
              carry' (->instruction system [:OR #{(last xy-and)
                                                  (last carry-or)}])]
          (recur (inc bit)
                 (conj instructions xy-xor z xy-and carry-or carry')
                 (last carry')))))))

(defn ->reverse-engineer [acc [instruction name]]
  (let [[op w1 w2] instruction]
    (assoc acc name
           (cond (and (= op :XOR)
                      (re-matches #"^[xy].*" w1))
                 [:xy-xor instruction]
                 (and (= op :AND)
                      (re-matches #"^[xy].*" w1))
                 [:xy-and instruction]
                 (= op :XOR)
                 [:z instruction]
                 (= op :AND)
                 [:carry-cond instruction]
                 (= op :OR)
                 [:carry instruction]))))

(defn valid? [system [name [type [op w1 w2]]]]
  (case type
    :xy-xor (and (nil? (get system w1))
                 (nil? (get system w2))
                 (= (re-find #"\d+" w1) (re-find #"\d+" w2)))
    :xy-and (and (nil? (get system w1))
                 (nil? (get system w2))
                 (= (re-find #"\d+" w1) (re-find #"\d+" w2)))
    :z (and (= #{:carry :xy-xor} #{(get-in system [w1 0])
                                   (get-in system [w2 0])})
            (some? (re-find #"\d+" name)))
    :carry-cond (and (= #{:carry :xy-xor} #{(get-in system [w1 0])
                                            (get-in system [w2 0])})
                     (not (some? (re-find #"\d+" name))))
    :carry (and (= #{:carry-cond :xy-and} #{(get-in system [w1 0])
                                            (get-in system [w2 0])})
                (not (some? (re-find #"\d+" name))))))

(defn enrich [system [name [type [op w1 w2]]]]
  (let [data-w1 (get system w1)
        data-w2 (get system w2)]
    (case type
      :z [(when-not (some? (re-find #"\d+" name)) [["$$$" name w1 data-w1 w2 data-w2]])
          (when-not (contains? #{:carry :xy-xor} (first data-w1)) [w1 data-w1])
          (when-not (contains? #{:carry :xy-xor} (first data-w2)) [w2 data-w2])]
      :carry-cond [(when (some? (re-find #"\d+" name)) [["~~" name w1 data-w1 w2 data-w2]])
                   (when-not (contains? #{:carry :xy-xor} (first data-w1)) [w1 data-w1])
                   (when-not (contains? #{:carry :xy-xor} (first data-w2)) [w2 data-w2])]
      :carry [(when (some? (re-find #"\d+" name)) [["!!"  name w1 data-w1 w2 data-w2]])
              (when-not (contains? #{:carry-cond :xy-and} (first data-w1)) [w1 data-w1])
              (when-not (contains? #{:carry-cond :xy-and} (first data-w2)) [w2 data-w2])]
      [["!!!!!!!!!!" [data-w1]]
       ["!!!!!!!!!!" [data-w2]]])))

(defn golden []
  (let [system (->> INPUT
                    (slurp)
                    (clojure.string/split-lines)
                    (reduce ->reversed-system {})
                    (reduce ->reverse-engineer {}))]
    (->> (remove (partial valid? system) system)
         (mapcat (partial enrich system))
         (keep identity)
         (map first)
         (distinct)
        ;;  (map first)
        ;;  (sort)
        ;;  (clojure.string/join ",")
         (clojure.pprint/pprint))))

{}

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))
;; sgj,tks,vss,z14,z22,z31,z32,z35
;; cgt,ghr,hjf,rqf,vss,wdr,z31,z35
;; bbc,tks,vss,z14,z22,z31,z32,z35
;; bbc,kpp,tks,vss,z14,z22,z31,z35
;; bbc,ghr,tks,vss,z14,z22,z31,z35
;; bbc,ghr,tks,vss,z14,z22,z31,z35
;; bbc,ghr,sbg,vss,z14,z22,z31,z35
;; bbc,ghr,hjf,vss,wdr,z14,z31,z35
;; bbc,cgt,ghr,gjn,hjf,vss,z31,z35
;; bjb,ctt,djn,ghr,rqf,sbg,z01,z15,z22
;; hjf,kdh,kdh,kpp,qtf,qtf,sgj,vss
;; hjf,kdh,kpp,qtf,sgj,vss,z14,z31