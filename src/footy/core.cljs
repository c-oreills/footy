(ns footy.core
  (:use [clojure.string :only [split trim capitalize replace join]]
        [footy.players :only [PLAYERS]])
  (:require-macros [hiccups.core :as hiccups])
  (:require [hiccups.runtime :as hiccupsrt]))

(defn main []
  (doall
    (map fetch-results LEAGUES)))

(defn all-processed []
  (do
    (draw-score-table)
    (draw-player-detail)))

(defn link-safe [s] (replace s #" " "-"))

(defn link-safe-revert [s] (replace s #"-" " "))

(defn player-detail-link [player-name]
  (let [link-safe-name (link-safe player-name)]
    (hiccups/html [:a {:href (str "#" link-safe-name)
                       :onClick (str "footy.core.draw_player_detail('" link-safe-name "');")}
                   player-name])))

(defn draw-score-table []
  (let [cols ["player" "total" "high" "low" "week"]
        name-render (fn [o val] (player-detail-link val))
        aa-data (for [player-points (map get-player-points (keys PLAYERS))]
                  (for [col cols] ((keyword col) player-points)))
        headers (for [col cols] {"sTitle" (capitalize col)
                                 "fnRender" (if (= col "player") name-render)
                                 "asSorting" [(if (= col "player") "asc" "desc")]
                                 })]
    (. (jq "#score-table")
       (dataTable (clj->js
                    {"aaData" aa-data "aoColumns" headers "bPaginate" false "aaSorting" [[1 "desc"]]})))))

(defn draw-player-detail [safe-player]
  (let [player (link-safe-revert (or
                                   safe-player
                                   (-> js/window (aget "location") (split #"#") (second))))]
    (if player
      (. (jq "#player-detail")
         (html
           (hiccups/html
             [:p player]
             (for [week (-> CURRENT-WEEK (inc) (range) (reverse))]
               [:p
                (format "Week %d - %d pts" (inc week) (get-player-week-points player week))
                [:ul
                 (for [match (get-player-week-matches player week)]
                   [:li (pretty-match match)])]])))))))

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
  (reduce + (get-player-week-matches player week :points-only true)))

(defn get-player-week-matches
  [player week & {:keys [points-only] :or {points-only false}}]
  (let
    [teams (PLAYERS player)
     get-team-match (fn [team]
                      (if-not (contains? @team-matches team)
                        (log
                          (str "Unknown team: " team)
                          (filter
                            #(= (first %) (first team))
                            (keys @team-matches))))
                      (let [match (get-in @team-matches [team week])
                            side (first (filter #(= team (get-in match [% :name]))
                                                [:home :away]))]
                        (cond
                          points-only (get-in match [side :points])
                          match (assoc match :side side)
                          :else match)))]
    (map get-team-match teams)))

(defn results-url [league]
  (str "http://www.bbc.co.uk/sport/football/" league "/results"))

(defn yql-url [url]
  (str
    "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%3D%22"
    (js/encodeURIComponent url)
    "%22%20and%20xpath%3D'%2F%2Fdiv%5B%40id%3D%22results-data%22%5D'&format=xml&callback=?"))

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
                          (let [match (detail-to-match detail date league)]
                            (if match (store-match match league))))]
    (jq-each details process-details)))

(defn detail-to-match [detail date league]
  (let
    [detail (jq detail)
     first-text (fn [query]
                  (trim (.text (.first (.find detail query)))))
     home-team (first-text ".team-home")
     away-team (first-text ".team-away")
     score (first-text ".score")
     comment (first-text ".comment")
     [home-score away-score] (map int (split score #"-"))
     finished (or
                (= (trim (.text (.next detail))) "Full time")
                (re-find #"^Abandoned" comment))
     week (get-week date)
     weekday (.getDay date)
     home-points (points home-score away-score)
     away-points (points away-score home-score)]
    (if (and
          (not-any? empty? [home-team away-team])
          (not-neg? week)
          (or (>= 1 weekday) (<= 5 weekday))
          finished)
      {:home {:name home-team :score home-score :points home-points}
       :away {:name away-team :score away-score :points away-points}
       :date date :week week :league league})))

(defn pretty-match [match]
  (let [side-score (fn [side]
                     (let [rev (if (= side :away) identity reverse)
                           d (fn [f k] (format f (get-in match [side k])))
                           style (if (= (get match :side) side)
                                   #(hiccups/html [:b {:style "color: blue;"} %])
                                   identity)]
                       (style (join " " (rev [(d "(%d pts)" :points) (d "%s" :name) (d "%d" :score)])))))]
    (if match
      (str (side-score :home) " - " (side-score :away))
      "---")))

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

;; borrowed from ibdknox/jayq
(defn map->js [m]
  (let [out (js-obj)]
    (doseq [[k v] m]
      (aset out (name k) v))
    out))

(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
other ClojureScript colls into JavaScript arrays, and ClojureScript
keywords into JavaScript strings."
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.-strobj (reduce (fn [m [k v]]
                                 (assoc m (clj->js k) (clj->js v))) {} x))
    (coll? x) (apply array (map clj->js x))
    :else x))
