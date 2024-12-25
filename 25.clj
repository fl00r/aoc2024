(require 'clojure.string)

(def INPUT "25.input")

(def FILLED \#)

(defn split-2n [s]
  (clojure.string/split s #"\n\n"))

(defn ->type [s]
  (if (= (first s) FILLED) :locks :keys))

(defn fit? [s1 s2]
  (->> (map vector s1 s2)
       (some #{[FILLED FILLED]})
       (not)))

(defn try-locks [{:keys [locks keys]}]
  (for [lock locks
        key keys
        :when (fit? lock key)]
    [lock key]))

(defn silver []
  (->> INPUT
       (slurp)
       (split-2n)
       (group-by ->type)
       (try-locks)
       (count)))

(defn golden [])

(prn "SILVER" (silver))
(prn "GOLDEN" (golden))