(ns footy.core)

(defn test []
  (load_url_to_container "http://www.bbc.co.uk/sport/football/championship/results" "#cont"))

(defn load_url_to_container [url container]
  (let [yql_url (str "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%3D%22" (js/encodeURIComponent url) "%22&format=xml&callback=?")
        url_callback (fn [data]
                       (.html (js/$ container) (aget data "results" 0))
                       (js/console.log "test")
                       (comment js/console.log (js/$ ".table-header")))]
    (js/$.getJSON yql_url url_callback)))
