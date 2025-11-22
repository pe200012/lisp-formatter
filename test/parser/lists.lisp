; Test parsing lists with different delimiters
()
(foo)
(foo bar)
(foo bar baz)
(a (b c))
(nested (lists (are (fun))))

; Square brackets
[]
[foo]
[foo bar]
[a [b c]]

; Curly braces
{}
{foo}
{foo bar}
{a {b c}}

; Mixed delimiters
(foo [bar] {baz})
(a (b [c {d}]))