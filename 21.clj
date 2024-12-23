(require 'clojure.string)

(def INPUT "21.input")

(def NUMERIC-KEYPAD
  {\7 [0 0]
   \8 [1 0]
   \9 [2 0]
   \4 [0 1]
   \5 [1 1]
   \6 [2 1]
   \1 [0 2]
   \2 [1 2]
   \3 [2 2]
   \0 [1 3]
   \A [2 3]})

(def R (->> NUMERIC-KEYPAD
            (map reverse)
            (map vec)
            (into {})))

(def DIRECTIONAL-KEYPAD
  {:PRESS [2 0]
   :UP [1 0]
   :LEFT [0 1]
   :DOWN [1 1]
   :RIGHT [2 1]})

(def RR (->> DIRECTIONAL-KEYPAD
             (map reverse)
             (map vec)
             (into {})))


(defn ->direction [hole [[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        hgap [x2 y1]
        vgap [x1 y2]]
    (->> (cond-> []
           (neg? dx) (concat (repeat (abs dx) [(if (= hgap hole) 5 1) :LEFT]))
           (pos? dy) (concat (repeat (abs dy) [(if (= vgap hole) 6 2) :DOWN]))
           (neg? dy) (concat (repeat (abs dy) [(if (= vgap hole) 6 3) :UP]))
           (pos? dx) (concat (repeat dx [(if (= hgap hole) 5 4) :RIGHT]))
           :always (concat [[7 :PRESS]]))
         (sort)
         (map last))))

(defn use-keypad [keypad init hole buttons]
  (->> buttons
       (map keypad)
       (concat [(get keypad init)])
       (partition 2 1)
       (mapcat (partial ->direction hole))))

(defn press [cnt num]
  {:num num
   :directions
   (nth (->> (use-keypad NUMERIC-KEYPAD \A [0 3] num)
             (iterate (partial use-keypad DIRECTIONAL-KEYPAD :PRESS [0 0])))
        (dec cnt))})

(defn ->complexity [{:keys [num directions]}]
  (* (Integer. (re-find #"\d+" num))
     (count directions)))

(defn silver [cnt]
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map (partial press cnt))
       (map ->complexity)
       (reduce +)))

;; 
;; 
;; 

(defn use-keypad2 [keypad hole from buttons]
  (->> buttons
       (concat [from])
       (map keypad)
       (partition 2 1)
       (mapcat (partial ->direction hole))
       (concat [:PRESS])
       (partition 2 1)
       (group-by identity)
       (reduce (fn [acc [k xs]]
                 (assoc acc k (count xs))) {})))

(defn use-keypad3 [keypad hole from buttons]
  (->> buttons
       (map keypad)
       (partition 2 1)
       (mapcat (partial ->direction hole))
       (concat [from])
       (partition 2 1)
       (group-by identity)
       (reduce (fn [acc [k xs]]
                 (assoc acc k (count xs))) {})))

(def CACHE (atom {}))

(defn number->buttons [result]
  (reduce (fn [result' [b cnt]]
            (let [res (or (get @CACHE b)
                          (use-keypad2 DIRECTIONAL-KEYPAD [0 0] (first b) [(last b)]))
                  res' (reduce-kv (fn [acc k v]
                                    (assoc acc k (* cnt v)))
                                  {}
                                  res)]
              (swap! CACHE assoc b res)
              (merge-with + result' res')))
          {}
          result))

(defn ->buttons [cnt number]
  (let [init-buttons (use-keypad3 NUMERIC-KEYPAD [0 3] :PRESS (concat [\A] number))]
    (let [data (-> (iterate number->buttons init-buttons)
                   (nth cnt))]
      (* (Integer. (re-find #"\d+" number))
         (->> (vals data)
              (reduce +))))))

(defn golden [cnt]
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map (partial ->buttons cnt))
       (reduce +)))

(prn "SILVER" (silver 3))

(prn "GOLDEN" (golden 25))