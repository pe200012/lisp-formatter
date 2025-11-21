; lisp-format skip
(fn badly-formatted   [a b c]
  (do   (x y)))

; This should be formatted normally
(fn normal [a b c]
  (do (x y)))

;lisp-format skip
(defn another-skipped    [ x   y ]
    (let [z    (+ x y)]
      z))

; Continuation after skip
(defn formatted-function [x]
  (if (> x 10)
    (println "large")
    (println "small")))
