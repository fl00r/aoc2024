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

(def DIRECTIONAL-KEYPAD
  {:PRESS [2 0]
   :UP [1 0]
   :LEFT [0 1]
   :DOWN [1 1]
   :RIGHT [2 1]})

(defn mnhtn [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn ->direction [hole [[x1 y1] [x2 y2]]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (cond-> []
      (and (not= [x1 y2] hole) (pos? dy)) (concat (repeat dy :DOWN))
      (pos? dx) (concat (repeat dx :RIGHT))
      (and (= [x1 y2] hole) (pos? dy)) (concat (repeat dy :DOWN))
      (and (not= [x2 y1] hole) (neg? dx)) (concat (repeat (abs dx) :LEFT))
      (neg? dy) (concat (repeat (abs dy) :UP))
      (and (= [x2 y1] hole) (neg? dx)) (concat (repeat (abs dx) :LEFT))
      :always (concat [:PRESS]))))

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

(prn "SILVER" (silver 3))

(prn "GOLDEN" (silver 26))