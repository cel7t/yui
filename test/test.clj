(ns yui.test
  (:require [clojure.test :refer [deftest is]]
            [yui.core]))

;; Core functions and macros - at the top of src/yui/core.clj

(deftest core-functions
  (is (belongs 1 [1 2 3]))
  (is (= ["a" "b"] (tokenize "a b")))
  (is (= "a b" (detokenize ["a" "b"])))
  (is (cond-action "yui" config
                   :names
                   true)))

;; Reminders functions - in src/yui/functions.clj

(deftest reminders
  (is (= (parse-duration "2d")
         (jt/plus (jt/local-date-time)
                  (jt/days 2))))
  (is (= (parse-date "1/1/2025-18:30:30")
         (jt/local-date-time 2023 1 1 18 30 30)))


