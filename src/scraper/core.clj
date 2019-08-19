(ns scraper.core
  (:use hickory.core)
  (:require [clj-http.client :as client]
            [hickory.select :as s]
            [clojure.string :as str]))

(def BASE-URL "https://afltables.com/afl/")

(defn read-url [url]
  (-> (client/get url)
      :body
      parse
      as-hickory))

(def site-tree
  (-> (client/get "https://afltables.com/afl/seas/2018.html#lad")
      :body
      parse
      as-hickory))

(def test-data
  (map :content
       (s/select
        (s/child (s/tag :table)
                 (s/el-not (s/has-descendant (s/tag :table))))
        site-tree)))

(defn parse-round-header [data]
  "Returns the round number as a string: Round N"
  (->> data
       first :content
       first :content
       first :content
       first))

(defn parse-side [side]
  (let [[team scores total metadata] (:content side)]
    {:team
     (->> team
          :content first
          :content first)
     :scores
     (-> scores
         :content first
         :content first
         (str/replace "\u00A0" " ")
         str/trim
         (str/split #"\s+"))
     :total
     (->> total
          :content first
          str/trim)}))

(defn get-stats-url [away]
  (->> away
       last last last
       :content
       (remove #(= "]" %))
       last
       :attrs :href))

(defn full-path [base rel]
  (str base
       (last (str/split rel #"\.\./"))))

(defn parse-metadata [home away]
  (let [[_ _ _ metadata] (:content home)]
    {:time
     (->> metadata
          :content first str/trim)
     :venue
     (-> metadata
         :content last
         :content first
         str/trim)
     :stats
     (full-path BASE-URL (get-stats-url away))}))

(defn parse-match-header [data]
  (let [[home away] (remove #(= "\n" %) data)]
    {:home (parse-side home)
     :away (parse-side away)
     :metadata (parse-metadata home away)}))

(defn bye-get-team [data]
  (->> data
       first :content
       first :content
       first :content
       first))

(defn parse-bye-header [data]
  (let [team (bye-get-team data)]
    [:bye team]))

(defn parse-ladder-header [ladder]
  (->> ladder
       rest                  ; don't worry about heading
       (remove #(= "\n" %))
       (map :content)        ; extract content
       (map                  ; flatten data
        (partial
         map
         (comp read-string str/trim first :content)))
       (mapv                 ; convert first item to kw
        #(vec
          (cons
           (keyword (first %))
           (rest %))))))

(defn round-header? [data]
  (= 1 (count data)))

(defn match-header? [data]
  (= 4 (count data)))

(defn ladder-header? [data]
  (> (count data) 4))

(defn bye-header? [data]
  (= 2 (count data)))

(defn end-header?
  "The end of home 'n' away season ladder."
  [data]
  (= 3 (count data)))

(defn partition-rounds [data]
  (->> data
       (take-while #(not (end-header? %)))
       (partition-by #(round-header? %))
       (partition 2)
       (map #(apply concat %))))

(defn into-round [round]
  (let [name-data (first round)
        ladder-data (last round)
        matches-data ((comp rest butlast) round)]
    {:round name-data
     :ladder ladder-data
     :matches (vec matches-data)}))

(defn parse-round [round]
  (into-round
   (map
    (fn [row]
      (cond
        (round-header? row) (parse-round-header row)
        (match-header? row) (parse-match-header row)
        (bye-header? row) (parse-bye-header row)
        (ladder-header? row) (parse-ladder-header row)
        :else {:unknown (count row)}))
    round)))

(defn parse-season [data]
  (mapv parse-round (partition-rounds data)))

(defn parse-finals [data]
  (->> (drop-while #(not (end-header? %)) data)
       (map (fn [row]
              (when (match-header? row)
                (parse-match-header row))))
       (remove nil?)
       vec))

(defn afl-data [year]
  (let [tree (read-url (str BASE-URL "seas/" year ".html#lad"))
        data (map :content
                  (s/select
                   (s/child (s/tag :table)
                            (s/el-not (s/has-descendant (s/tag :table))))
                   tree))]
    {:homenaway (parse-season data)
     :finals (parse-finals data)}))

(defn match-stats [match]
  (let [stats-url (get-in match [:metadata :stats])]
    stats-url))

; TODO Remove this once above is written
(def stats-ex
  (get-in (first (:matches (first (:homenaway (afl-data 2018)))))
          [:metadata :stats]))

(defn match-stats [url]
  (s/select (s/child (s/tag :tr))
            (read-url url)))

(defn parse-match-stats [data]
  (let [ump (nth data 5)
        key (nth data 6)
        home-heading (nth data 7)
        home-header (nth data 8)
        ;; ... then the players
        ]
    ump))

; (s/select (s/child (s/tag :th) (s/attr {:colspan "25"})) stats-ex)

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
