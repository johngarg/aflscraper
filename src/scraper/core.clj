(ns scraper.core
  (:use hickory.core)
  (:require [clj-http.client :as client]
            [clojure.data.json :as json]
            [hickory.select :as s]
            [clojure.string :as str]))

(def BASE-URL "https://afltables.com/afl/")
(def MISSING :unknown)

(defn read-url [url]
  (-> (client/get url)
      :body
      parse
      as-hickory))

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
       ;; (remove #(= "]" %))
       (remove string?)
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
     (full-path BASE-URL
                (get-stats-url away))}))

;; matchstats
(defn match-stats [url]
  (s/select (s/child (s/tag :tr))
            (read-url url)))

(defn parse-ump [data] nil)

(defn parse-team-name [data]
  (-> data
      first
      :content
      first
      (str/split #"\s+")
      first))

(defn read-val [data]
  (cond
    (= "\u00A0" data) MISSING
    (= " " data) MISSING
    (= "" data) MISSING
    (= "1%" data) :OP
    (= "%P" data) :PP
    :else (read-string data)))

(defn parse-player-number [data]
  (-> data first :content first str/trim read-string))

(defn parse-player-name [data]
  (-> data first :content first :content first))

(defn parse-player-stat [[v k]]
  {(keyword (read-val (first (:content k))))
   (read-val (first (:content v)))})

(defn parse-player-stats [data]
  (map parse-player-stat data))

(defn parse-player [player cols]
  (let [data (map vector player cols)
        num (first data)
        name (second data)
        stats (drop 2 data)]
    {:number (parse-player-number num)
     :name (parse-player-name name)
     :stats (apply merge (parse-player-stats stats))}))

(defn parse-team-match-stats [data]
  ;; example match
  ;; (take 25 (drop 7 (match-stats stats-ex)))
  (let [d (map :content data)
        name (first d)
        ;; rushed (last d)
        cols (second d)
        players (drop 2 d)]
    {:name (parse-team-name name)
     :players (map #(parse-player % cols) players)}))

(defn match-stats-end-header? [x]
  (= ["Totals"] (-> x :content first :content first :content)))

(defn rushed? [x]
  (= ["Rushed"] (:content (first (:content x)))))

(defn team-start-header? [x]
  (= {:width "93%"} (-> x :content first :attrs)))

(defn get-team-data [data]
  (->> data
       (partition-by team-start-header?)
       last
       (partition-by rushed?)
       first))

(defn partition-teams [data]
  ;; t1pre = data containing team 1 and preable and possibly rushed
  ;; r1 = totals for team 1
  ;; t2tot = data containing team 2 and possibly rushed
  ;; r2 = totals for team 2
  ;; r = rest of data
  (let [[t1pre r1 t2tot r2 & r] (partition-by match-stats-end-header? data)
        ;; pre (take 7 t1pre) ;; for now don't do anything with this
        t1 (get-team-data t1pre)
        ;; t1 (drop 7 (first (partition-by rushed? t1pre)))
        ;; t2 (drop 2 (first (partition-by rushed? t2tot)))
        t2 (get-team-data t2tot)
        ]
    [t1 t2]))

(defn parse-match-stats [data]
  (let [ump (nth data 5)
        [home away] (partition-teams data)]
    {:home (parse-team-match-stats home)
     :away (parse-team-match-stats away)}))

(defn parse-match-header [data]
  (let [[home away] (remove #(= "\n" %) data)
        meta (parse-metadata home away)]
    {:home (parse-side home)
     :away (parse-side away)
     :matchstats (parse-match-stats (match-stats (:stats meta)))
     :metadata meta}))

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
        :else {MISSING (count row)}))
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



;; test data
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

(def stats-ex
  (get-in (first (:matches (first (:homenaway (afl-data 2018)))))
          [:metadata :stats]))



(defn -main
  "I don't do a whole lot."
  [year]
  (println
   (json/write-str (afl-data year))))
