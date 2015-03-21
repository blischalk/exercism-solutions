(ns bob)

;; If we end with a question mark we are asking a question...  Unless
;; we are yelling...

;; We are yelling if our input is the same as uppercasing the input
;; and we have more than 2 consecutive capital letters

;; We have silence if we receive a blank string or all spaces

;; In all other scenarios we say Whatever.

(defn response-for [speech]
  (letfn [(question? [speech] (= (last speech) \?))
          (yell? [speech] (and (= speech (clojure.string/upper-case speech))
                               (not (nil? (re-find #"[A-Z]{2,}" speech)))))
          (silence? [speech]
            (or (= "" speech)
                (not (nil? (re-find #"^\s+$" speech)))))]

    (cond (and (question? speech) (not (yell? speech))) "Sure."
          (yell? speech) "Whoa, chill out!"
          (silence? speech) "Fine. Be that way!"
          :otherwise "Whatever.")))