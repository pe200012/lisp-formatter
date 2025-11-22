; Test parsing quoted expressions
'foo
'(a b c)
'((x y) z)
`(a b)
`(a ,b)
`(a ,@rest)
`(a ,b ,@rest)
(quasiquote (a ,b))
(unquote x)
(unquote-splicing rest)