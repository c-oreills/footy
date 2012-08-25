(ns footy.core
  (:use [clojure.string :only [split trim]]))

(defn main []
  (load_and_parse_url "http://www.bbc.co.uk/sport/football/championship/results"))

(defn yql_url [url]
  (str
    "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%3D%22"
    (js/encodeURIComponent url)
    "%22&format=xml&callback=?"))

(defn load_and_parse_url [url]
  (let [url_callback (fn [data]
                       (parse_tables (aget data "results" 0)))]
    (js/$.getJSON (yql_url url) url_callback)))

(defn parse_tables [html]
  (let [headers (get_table_headers html)]
    (jq_each headers header_to_matches)))

(defn get_table_headers [html]
  (.find (jq html) ".table-header"))

(defn header_to_matches [header]
  (let [header (jq header)
        date (.text header)
        table (.next header)
        details (.find table ".match-details")]
      (jq_each details #(log (aget (detail_to_match % date) "strobj"))
      )))

(defn detail_to_match [table date]
  (let
    [table (jq table)
     home_team (get_first_text table ".team-home")
     away_team (get_first_text table ".team-away")
     score (get_first_text table ".score")
     [home_score away_score] (map int (split score #"-"))
     finished (= (get_first_text table ".time") "Full time")
     ]
    {:home {:name home_team :score home_score :points (points home_score away_score)}
     :away {:name away_team :score away_score :points (points away_score home_score)}
     :date date :finished finished}))

(defn get_first_text [jq_obj query]
  (trim (.text (.first (.find jq_obj query)))))

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
