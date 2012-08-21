(ns footy.core)

(defn main []
  (load_and_parse_url "http://www.bbc.co.uk/sport/football/championship/results"))

(defn yql_url [url]
  (str
    "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%3D%22"
    (js/encodeURIComponent url)
    "%22&format=xml&callback=?"))

(defn load_and_parse_url [url]
  (let [url_callback (fn [data]
                       (parse_tables (clean_html (aget data "results" 0))))]
    (js/$.getJSON (yql_url url) url_callback)))

(defn clean_html [ext_html]
  (let [jq_ext_html (js/$ ext_html)]
    (do 
      (. (. jq_ext_html (find "script")) (remove))
      jq_ext_html)))

(defn parse_tables [html]
  (js/console.log (. (. html (find ".table-header")) (next))))
