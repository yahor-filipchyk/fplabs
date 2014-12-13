(ns org.yahor.clojure.fplabs.crawling.crawler
  (:gen-class)
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(import '(org.jsoup Jsoup))
(import '(java.util.concurrent Executors))

(defn abs-link
  "Concats domain with relative link if needed"
  [url link]
  (if (.startsWith link "/")
  (let [domain (let [with-slash (if (.endsWith url "/") url (str url "/"))]
                 (subs with-slash 0
                       (let [protocol (subs with-slash 0 (+ (.indexOf with-slash "/") 2))
                             without-protocol (subs with-slash (.length protocol))]
                         (+ (.indexOf without-protocol "/") (.length protocol)))))]
    (str domain link))
  link))

(defn link-filter
  [link]
  (or (.startsWith link "http://") (.startsWith link "https://") (.startsWith link "/")))

(defn links
  "Extracts links from html"
  [url html]
  (let [doc (Jsoup/parse html)
        hrefs (.select doc "a[href]")]
    (map #(abs-link url %)
         (set (filter link-filter (map #(.attr % "href") hrefs))))))

(defn html
  "Gets html from url"
  [url]
  (try
    (client/get url {:accept :html :ignore-unknown-host true})
     (catch Exception e
       :bad)))

(defn urls
  "Reads links from file"
  [fname]
  (with-open [file (io/reader fname)]
    (doall (filter #(and (not (str/blank? %)) (not (.startsWith % ";"))) (line-seq file)))))

(defn crawl-the-url
  "Gets html from the url, exracts links and handles redirections"
  [url]
  (let [page (html url)]
    (if (or (nil? page) (= page :bad))
      :bad
      (let [redirected-to (last (.get page :trace-redirects))
            redirected (not (= url redirected-to))]
        (if redirected
          {:links (links redirected-to (.get page :body)) :redirected redirected-to}
          {:links (links url (.get page :body)) :redirected false})))))

(defn print-messages
  [messages]
  (doall (map #(do
                (println (first %))
                (print-messages (peek %))) @messages)))

(defn dispatch-crawling
  [urls deepness max-deepness thread-pool messages]
  (if (and (not (empty? urls)) (< deepness max-deepness))
    (let [tasks (map #(
                       ; callable function
                       fn []
                         (let [url %
                               result (crawl-the-url url)
                               links-count (count (.get result :links))
                               redirected (.get result :redirected)
                               indentation (apply str (repeat deepness "  "))
                               message (if (= result :bad)
                                         (str indentation url " bad")
                                         (str indentation url " " links-count (if redirected
                                                                                          (str " redirect " redirected)
                                                                                          " links")))
                               child-messages (atom [])]
                           ; storing a message along with referance to child messages
                           (swap! messages conj [message child-messages])
                           (if-not (= result :bad)
                             (dispatch-crawling (.get result :links) (inc deepness) max-deepness thread-pool child-messages))
                           )) urls)]
      (.invokeAll thread-pool tasks))))

(defn crawl
  [params]
  (let [file-name (first params)
        deepness (Integer/parseInt (first (rest params)))
        threads-count (Integer/parseInt (first (rest (rest params))))
        urls-list (urls file-name)
        pool (Executors/newFixedThreadPool threads-count)
        messages (atom [])]
    (dispatch-crawling urls-list 0 deepness pool messages)
    (.shutdown pool)
    (print-messages messages)))