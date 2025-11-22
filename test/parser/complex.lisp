; Test complex parsing combinations
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; Mixed with comments and strings
(defun hello (name)
  ; This is a greeting function
  (print (string-append "Hello, " name "!")))

; Nested structures with different delimiters
(let [x 1
      y 2]
  {result (+ x y)
   message "calculation done"})

; Skip directive test
; lisp-format skip
(define raw (+ 1 2 3))

; Complex quoting
`(define (map ,f ,@lists)
   (if (null? lists)
       '()
       (cons (apply f (map car lists))
             (map f (map cdr lists)))))