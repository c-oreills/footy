(ns footy.core
  (:use [clojure.string :only [split trim capitalize]]
        [footy.players :only [PLAYERS]])
  (:require-macros [hiccups.core :as hiccups])
  (:require [hiccups.runtime :as hiccupsrt]))

(defn main []
  (doall
    (map fetch-results LEAGUES)))

(defn all-processed []
  (do
    (let [cols ["player" "total" "high" "low" "week"]
          table (hiccups/html [:table
                               [:tr (for [col cols] [:th (capitalize col)])]
                               (for [player-points (map get-player-points (keys PLAYERS))]
                                 [:tr (for [col cols] [:td ((keyword col) player-points)])])
                               ])]
      (. (jq "#player-scores") (append table))
    )))

(def LEAGUES
  #{"premier-league"
    "championship"
    "league-one"
    "league-two"
    "conference"
    "scottish-premier"
    "scottish-first-division"
    "scottish-second-division"
    "scottish-third-division"})

(def unprocessed (atom (disj LEAGUES)))

(defn set-processed [league]
  (do
    (swap! unprocessed #(disj % league)))
    (if (empty? @unprocessed)
      (all-processed)))

(def not-neg? (comp not neg?))

(defn get-week [date]
  (let [diff (- date START-DATE)]
    (if (not-neg? diff)
      (quot diff (* 7 24 60 60 1000))
      -1)))

(def START-DATE (new js/Date 2012 7 17))

(def CURRENT-WEEK (get-week (new js/Date)))

(def team-matches (atom {}))

(def player-points (atom {}))

(defn get-player-points [player]
  (let [week-points
        (map #(get-player-week-points player %) (range (inc CURRENT-WEEK)))]
    {:player player :weekly week-points :total (apply + week-points)
     :high (apply max week-points) :low (apply min week-points)
     :week (nth week-points CURRENT-WEEK)}))

(defn get-player-week-points [player week]
  (let
    [teams (PLAYERS player)
     get-team-points (fn [team]
                       (if-not (contains? @team-matches team)
                         (log (str "Unknown team: " team) (filter #(= (first %) (first team)) (keys @team-matches))))
                       (get-in @team-matches [team week :points team]))]
    (reduce + (map get-team-points teams))))

(defn results-url [league]
  (str "http://www.bbc.co.uk/sport/football/" league "/results"))

(defn yql-url [url]
  (str
    "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%3D%22"
    (js/encodeURIComponent url)
    "%22&format=xml&callback=?"))

(defn fetch-results [league]
  (let [url (results-url league)
        url-callback (fn [data]
                       (process-result-response (aget data "results" 0) league))]
    (js/$.getJSON (yql-url url) url-callback)))

(defn process-result-response [html league]
  (let [headers (get-table-headers html)]
    (do
      (jq-each headers #(header-to-matches % league))
      (set-processed league))))

(defn get-table-headers [html]
  (.find (jq html) ".table-header"))

(defn header-to-matches [header league]
  (let [header (jq header)
        date (js/Date.parse (.text header))
        table (.next header)
        details (.find table ".match-details")
        process-details (fn [detail]
                          (let [match (detail-to-match detail date)]
                            (if match (store-match match league))))]
    (jq-each details process-details)))

(defn detail-to-match [detail date]
  (let
    [detail (jq detail)
     first-text (fn [query]
                  (trim (.text (.first (.find detail query)))))
     home-team (first-text ".team-home")
     away-team (first-text ".team-away")
     score (first-text ".score")
     [home-score away-score] (map int (split score #"-"))
     finished (= (trim (.text (.next detail))) "Full time")
     week (get-week date)
     weekday (.getDay date)]
    (if (and
          (not-any? empty? [home-team away-team])
          (not-neg? week)
          (or (>= 1 weekday) (<= 5 weekday))
          finished)
      {:home {:name home-team :score home-score}
       :away {:name away-team :score away-score}
       :points {home-team (points home-score away-score)
                away-team (points away-score home-score)}
       :date date :week week})))

(defn store-match [match league]
  (do
    (swap! team-matches store-team-match (get-in match [:home :name]) match)
    (swap! team-matches store-team-match (get-in match [:away :name]) match)))

(defn store-team-match [current-matches-val team match]
  (let
    [week (match :week)
     current-week-val (get-in current-matches-val [team week])]
    (if (or (not current-week-val)
            (> (current-week-val :date) (match :date)))
      (assoc-in current-matches-val [team week] match)
      current-matches-val)))

(defn points [score-a score-b]
  (+ score-a
     (cond
       (> score-a score-b) 3
       (= score-a score-b) 1
       :else 0)))

(defn jq-each [jq-obj fn]
  (. jq-obj (each #(fn %2))))

(def jq js/$)

(def log js/console.log)
