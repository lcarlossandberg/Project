||This is a test string
testvalue = "(L x . x) y"
testv = "(help (me) ) please"


||this is the lexer
lexeme::= Bra|Ket|Fun|Dot|Var [char]

lex::[char]->[lexeme]
lex []             = []
lex ('(':xs)       = Bra:(lex xs)
lex (')':xs)       = Ket:(lex xs)
lex ('L':(' ':xs)) = Fun:(lex xs)
lex ('.':xs)       = Dot:(lex xs)
lex (' ':xs)       = lex xs
lex (x:xs)         = (Var a):(lex b)
                     where
                     (a,b)=f (x:xs) []
f []       a = (a,[])
f (' ':xs) a = (a,xs)
f (')':xs) a = (a,(')':xs))
f (x:xs)   a = f xs (a++[x])


||This is the parse tree
parse_tree ::= Empty  
               | Variable [char]
               | App (parse_tree, parse_tree)
               | Function ([char], parse_tree) 
               | Brackets parse_tree

||This is the parser
parser::[lexeme]->parse_tree
parser []           = Empty
parser [Var any]    = Variable any
parser (Bra:xs)     = App(parser a, parser b)   
                      where (a,b) = f_bracket xs []
                      f_bracket [] a       = (a, [])
                      f_bracket (Bra:xs) a = f_bracket c (a++d)
                                              where (c, d)      = in_bra xs [Bra] 
                                              in_bra (Ket:ys) d = (ys, (d++[Ket])) 
                                              in_bra (Bra:ys) d = in_bra ys (d++[Bra])
                                              in_bra (y:ys)   d = in_bra ys (d++[y])  
                      f_bracket (Ket:xs) a = (a, xs)
                      f_bracket (x:xs)   a = f_bracket xs (a++[x])
parser (Fun:xs)     = Function (a, parser b)
                      where (a,b) = f_dot xs 
                      f_dot (Var any:(x:xs)) = (any, xs)
parser (Var any:xs) = App(Variable any, parser xs) 
parser any          = error ("Bad input format: "++(show any))



