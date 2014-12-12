(ns org.yahor.clojure.fplabs.crawling.crawler
  (:gen-class)
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(import '(org.jsoup Jsoup))
(import '(java.util.concurrent Executors))

(defn domain
  [url]
  (let [with-slash (if (.endsWith url "/") url (str url "/"))]
    (subs with-slash 0
          (let [protocol (subs with-slash 0 (+ (.indexOf with-slash "/") 2))
                without-protocol (subs with-slash (.length protocol))]
            (+ (.indexOf without-protocol "/") (.length protocol))))))

(defn abs-link
  [url link]
  (if (.startsWith link "/")
    (str (domain url) link)
    link))

(defn link-filter
  [link]
  (or (.startsWith link "http://") (.startsWith link "https://") (.startsWith link "/")))

(defn links
  [url html]
  (let [doc (Jsoup/parse html)
        hrefs (.select doc "a[href]")]
    (map (fn [link] (abs-link url link))
         (set (filter link-filter (map #(.attr % "href") hrefs))))))

(defn html
  [url]
  (try
    (client/get url {:accept :html :ignore-unknown-host true})
     (catch Exception e
       "bad")))

(defn urls
  "Reads links from file"
  [fname]
  (with-open [file (io/reader fname)]
    (doall (filter #(and (not (str/blank? %)) (not (.startsWith % ";"))) (line-seq file)))))

(defn crawl-the-url
  [url]
  (let [page (html url)]
    (if (or (nil? page) (= page "bad"))
      "bad"
      (let [redirected-to (last (.get page :trace-redirects))
            redirected (not (= url redirected-to))]
        (if redirected
          {:links (links redirected-to (.get page :body)) :redirected redirected-to}
          {:links (links url (.get page :body)) :redirected false})))))

(defn dispatch-crawling
  [urls deepness max-deepness thread-pool messages]
  (if (and (not (empty? urls)) (< deepness max-deepness))
    (let [tasks (map (fn [url]
                       (fn []
                         (let [result (crawl-the-url url)
                               links-count (count (.get result :links))
                               redirected (.get result :redirected)
                               indentation (apply str (repeat deepness "  "))
                               message (if (= result "bad")
                                         (str indentation url " bad")
                                         (str indentation url " " links-count (if redirected
                                                                                          (str " redirect " redirected)
                                                                                          " links")))
                               childs-ref (ref [])]
                           ;(println message)
                           (dosync
                             (alter messages conj [message childs-ref]))
                           (if-not (= result "bad")
                             (dispatch-crawling
                               (.get result :links)
                               (inc deepness)
                               max-deepness
                               thread-pool
                               childs-ref))
                           ))) urls)]
      (.invokeAll thread-pool tasks))))

(defn print-messages
  [messages]
  (let [derefed @messages]
    (if-not (and (empty? derefed) (nil? derefed))
      (doall (map #(do
                    (println (first %))
                    (print-messages (peek %))) derefed)))))

(defn crawl
  [params]
  (let [file-name (first params)
        deepness (Integer/parseInt (first (rest params)))
        threads-count (Integer/parseInt (first (rest (rest params))))
        urls-list (urls file-name)
        pool (Executors/newFixedThreadPool threads-count)
        messages (ref [])
        crawling-result (dispatch-crawling urls-list 0 deepness pool messages)]
    (.shutdown pool)
    ;(println messages)
    (print-messages messages)))