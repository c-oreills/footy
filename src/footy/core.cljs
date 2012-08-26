(ns footy.core
  (:use [clojure.string :only [split trim]]))

(defn main []
  (doall
    (map fetch_results LEAGUES)))

(defn all_processed []
  (do
    (log (get-in @team_matches ["Ebbsfleet"]))))

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

(defn set_processed [league]
  (do
    (swap! unprocessed #(disj % league)))
    (if (empty? @unprocessed)
      (all_processed)))

(def START_DATE (new js/Date 2012 7 17))

(def team_matches (atom {}))

(defn results_url [league]
  (str "http://www.bbc.co.uk/sport/football/" league "/results"))

(defn yql_url [url]
  (str
    "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%3D%22"
    (js/encodeURIComponent url)
    "%22&format=xml&callback=?"))

(defn fetch_results [league]
  (let [url (results_url league)
        url_callback (fn [data]
                       (process_result_response (aget data "results" 0) league))]
    (js/$.getJSON (yql_url url) url_callback)))

(defn process_result_response [html league]
  (let [headers (get_table_headers html)]
    (do
      (jq_each headers #(header_to_matches % league))
      (set_processed league))))

(defn get_table_headers [html]
  (.find (jq html) ".table-header"))

(defn header_to_matches [header league]
  (let [header (jq header)
        date (js/Date.parse (.text header))
        table (.next header)
        details (.find table ".match-details")
        process_details (fn [detail]
                          (let [match (detail_to_match detail date)]
                            (if match (store_match match league))))]
    (jq_each details process_details)))

(defn detail_to_match [table date]
  (let
    [table (jq table)
     first_text (fn [query]
                  (trim (.text (.first (.find table query)))))
     home_team (first_text ".team-home")
     away_team (first_text ".team-away")
     score (first_text ".score")
     [home_score away_score] (map int (split score #"-"))
     finished (= (first_text ".time") "Full time")
     week (get_week date)
     weekday (.getDay date)]
    (if (and
          (not-any? empty? [home_team away_team])
          (<= 0 week)
          (or (>= 1 weekday) (<= 5 weekday)))
      {:home {:name home_team :score home_score :points (points home_score away_score)}
       :away {:name away_team :score away_score :points (points away_score home_score)}
       :date date :week week :finished finished})))

(defn get_week [date]
  (let [diff (- date START_DATE)]
    (if (not (neg? diff))
      (quot diff (* 7 24 60 60 1000))
      -1)))

(defn store_match [match league]
  (do
    (swap! team_matches store_team_match (get-in match [:home :name]) match)
    (swap! team_matches store_team_match (get-in match [:away :name]) match)))

(defn store_team_match [current_matches_val team match]
  (let
    [week (match :week)
     current_team_val (get current_matches_val team {})
     current_week_val (get current_team_val week nil)]
    (assoc current_matches_val team
           (if (and current_week_val (< (current_week_val :date) (match :date)))
             current_team_val
             (assoc current_team_val week match)))))

(defn points [score_a score_b]
  (+ score_a
     (cond
       (> score_a score_b) 3
       (= score_a score_b) 1
       :else 0)))

(defn jq_each [jq_obj fn]
  (. jq_obj (each #(fn %2))))

(def jq js/$)

(def log js/console.log)
