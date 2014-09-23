;; bob
(ns bob)
(:require 'clojure.string)

;; constants
(def yelling-min-freq 0.60)
(def log-debug-statements false)

(defn debug [msg & rest]
  (if log-debug-statements (println msg rest)))

(defn remove-whitespace [msg]
  (clojure.string/replace msg #"\s" ""))

(defn clean-string [msg]
  (clojure.string/replace msg #"[^a-z0-9]" ""))

(defn find-uppercase-freq [msg]
  (let [whitespace-less-msg (remove-whitespace msg)
        msg-size (count whitespace-less-msg)
        uppercase-count (- msg-size (count (clean-string msg)))]
    (debug msg (clean-string msg))
    (debug uppercase-count msg-size)
    (if (.isEmpty whitespace-less-msg)
      0
      (/ uppercase-count msg-size))))

(defn is-question [msg]
  (.endsWith (.trim msg) "?"))

(defn is-nonsense [msg]
  (.isEmpty (.trim msg)))

(defn response-for [msg]
  (let [uppercase-freq (find-uppercase-freq msg)
        is-yelling (> uppercase-freq yelling-min-freq)]
    (debug is-yelling)
    (cond
     (is-nonsense msg) "Fine. Be that way!"
     is-yelling "Woah, chill out!"
     (is-question msg) "Sure."
     :else "Whatever.")))
