(ns scraper.analysis
  (:require [scraper.core :as scr]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.charts :refer :all]
            [com.rpl.specter :refer [transform select ALL MAP-VALS]]
            [clojure.string :as str]))

(defn match-data
  "data: input data
   team: one of :home or :away
   query: e.g. :total"
  [data team query]
  (concat
   (select [:homenaway ALL :matches ALL team query] data) ; home n away
   (select [:finals ALL team query] data)))               ; finals

(defn polish-data [data]
  (->> data
       (remove nil?)
       (mapv read-string)))

(defn scores
  "Returns a map of vectors {:goals goals, :behinds points}.
   data: (match-data afl2018 :home :scores)"
  [data]
  (let [score-data (->> (select [ALL 3] data)
                        (remove nil?)
                        (map #(str/split % #"\.")))]
    {:goals (polish-data (select [ALL 0] score-data))
     :behinds (polish-data (select [ALL 1] score-data))}))



;; quick look at the data
(def DATA
  (->> (range 1900 2019)
       (map scr/afl-data)))

(defn score-data [team]
  (->> DATA
       (map #(match-data % team :scores))
       (map scores)
       (apply (partial merge-with concat))))

(defn total-data [team]
  (->> DATA
       (map #(match-data % team :total))
       (apply concat)
       (polish-data)))

(view (histogram (total-data :home)))
(view (histogram (total-data :away)))

(sum (:goals (score-data :home)))
(sum (:goals (score-data :away)))
(sum (:behinds (score-data :home)))
(sum (:behinds (score-data :away)))

(view (histogram (:goals (score-data :home))))
(view (histogram (:goals (score-data :away))))
(view (histogram (:behinds (score-data :home))))
(view (histogram (:behinds (score-data :away))))
