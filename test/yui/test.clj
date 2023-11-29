(ns yui.test
  (:require [clojure.test :refer [deftest is]]
            [java-time.api :as jt]
            [yui.core :refer :all]))

;; Core functions and macros - at the top of src/yui/core.clj

(deftest core-functions
  (is (belongs 1 [1 2 3]))
  (is (= ["a" "b"] (tokenize "a b")))
  (is (= "a b" (detokenize ["a" "b"])))
  (is (cond-action "yui" config
                   :names
                   true)))

;; Reminders functions - in src/yui/functions.clj

(defn time-eq [a b]
  (and (jt/not-before? a b)
       (jt/not-after? a b)
       true))

(deftest reminders
  (is (time-eq (jt/truncate-to (parse-duration "2d") :days)
               (jt/truncate-to (jt/plus (jt/local-date-time)
                                        (jt/days 2)) :days)))
  (is (time-eq (parse-date "1/1/2025-18:30:30")
               (jt/local-date-time 2025 1 1 18 30 30))))

