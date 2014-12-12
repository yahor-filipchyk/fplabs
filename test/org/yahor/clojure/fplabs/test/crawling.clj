(ns org.yahor.clojure.fplabs.test.crawling
  (:use [org.yahor.clojure.fplabs.crawling.crawler]
        [clojure.test]))

(deftest no-response
  (testing "No responce from server"
    (is (nil? (html "http://non-existing.url")))))

(deftest bad-page-not-found
  (testing "Bad page is returned"
    (is (= (crawl-the-url "http://clojure.org/data_structures/123")
           "bad"))))

(deftest bad-page-no-response
  (testing "Bad page is returned"
    (is (= (crawl-the-url "http://non-existing.url")
           "bad"))))

(deftest extracting-domain
  (testing "Domain is exctracted from link"
    (is (= (domain "http://clojure.org/docs")
           "http://clojure.org"))))

(deftest count-links
  (testing "HTML contains 2 links"
    (is (= (count (links "http://localhost"
                         "<html>
                         <head><title>Test page</title></head>
                         <body>
                          <a href=\"/home\">Home</a>
                          <a href=\"/about\">About</a>
                         </body>
                         </html>"))
           2))))