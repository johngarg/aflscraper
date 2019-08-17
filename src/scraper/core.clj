(ns scraper.core
  (:use hickory.core)
  (:require [net.cgrand.enlive-html :as html]
            [clj-http.client :as client]
            [hickory.select :as s]
            [clojure.string :as str]))

(def site-tree
  (-> (client/get "https://afltables.com/afl/seas/2019.html#lad")
      :body
      parse
      as-hickory))

;; first contains round info
;; second ... contains match info
;; last contains ladder info
;; goes on to next round
(def raw-data
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

(defn parse-metadata [side]
  (let [[_ _ _ metadata] (:content side)]
     {:time
      (->> metadata
           :content first str/trim)
      :venue
      (-> metadata
          :content (nth 5)
          :content first
          str/trim)}))

(defn parse-match-header [data]
  (let [[home _ away _] data]
    {:home (parse-side home)
     :away (parse-side away)
     :metadata (parse-metadata home)}))

;; (nth raw-data 10) is the ladder
(defn parse-ladder [ladder]
  nil)

(defn -main
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
