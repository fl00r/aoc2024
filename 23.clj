(require 'clojure.string)
(require 'clojure.set)

(def INPUT "23.input")

(defn ->kvs [acc [k v]]
  (-> acc
      (update k clojure.set/union #{v})
      (update v clojure.set/union #{k})))

(defn split-by-dash [s]
  (clojure.string/split s #"-"))

(defn ->components-of-3 [mp]
  (->> mp
       (mapcat (fn [[k ks]]
                 (mapcat (fn [k']
                           (some->> (get mp k')
                                    (filter (fn [k''] (contains? ks k'')))
                                    (map (fn [k''] #{k k' k''}))))
                         ks)))
       (distinct)))

(defn with-chief? [xs]
  (some #(clojure.string/starts-with? % "t") xs))

(defn silver []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map split-by-dash)
       (reduce ->kvs {})
       (->components-of-3)
       (filter with-chief?)
       (count)))


(defn vertex->clique [graph v]
  (let [adjecent (get graph v)]
    (loop [cliques [#{v}]]
      (let [cliques' (->> (for [clique cliques
                                v' adjecent
                                :when (not (contains? clique v'))
                                :when (empty? (clojure.set/difference clique (get graph v')))]
                            (conj clique v'))
                          (distinct))]
        (if (empty? cliques')
          cliques
          (recur cliques'))))))

(defn graph->cliques [graph]
  (->> (keys graph)
       (filter #(clojure.string/starts-with? % "t"))
       (mapcat (partial vertex->clique graph))))

(defn golden []
  (->> INPUT
       (slurp)
       (clojure.string/split-lines)
       (map split-by-dash)
       (reduce ->kvs {})
       (graph->cliques)
       (sort-by count)
       (last)
       (sort)
       (clojure.string/join ",")))

(prn "SILVER" (silver))
;; 2109 high
(prn "GOLDEN" (golden))