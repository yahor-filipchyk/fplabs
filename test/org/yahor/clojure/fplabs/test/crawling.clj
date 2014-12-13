(ns org.yahor.clojure.fplabs.test.crawling
  (:use [org.yahor.clojure.fplabs.crawling.crawler]
        [clojure.test]))

(deftest count-links
  (testing "HTML contains 2 links"
    (is (= (count (links "http://localhost"
                         "<html>
                         <body>
                            <a href=\"/link1\">Link one</a> <a href=\"/link2\">Link two</a>
                         </body>
                         </html>"))
           2))))

(deftest bad-page-not-found
  (testing "Bad page is returned"
    (is (= (crawl-the-url "http://clojure.org/data_structures/123")
           :bad))))

(deftest bad-page-no-response
  (testing "Bad page is returned"
    (is (= (crawl-the-url "http://non-existing.url")
           :bad))))