
lexeme::= Idcons [char]|Opequal|LBra|LKet|Bra|Ket|Opplus|Opminus|Opmult|Opdivide|Opgreater|Opless|Funhead|Funtail|Opcons|Concomma|Conwhere|Idfunc [char] [char]|Idintvar [char]|Idvar [char]|Idnum num|Expr|Opnotequal|Oplessequ|Opgreaterequ|Idexrun|Main|Stateif|Idcomment [char]|WBra|WKet

lex::[char]->[lexeme]
lex []             = []
lex ('>':'=':xs)   = Opgreaterequ:(lex xs)
lex ('<':'=':xs)   = Oplessequ:(lex xs)
lex ('>':xs)       = Opgreater:(lex xs)
lex ('<':xs)       = Opless:(lex xs)
lex ('~':'=':xs)   = Opnotequal:(lex xs)
lex ('=':xs)       = Opequal:(lex xs)
lex ('|':'|':xs)   = (Idcomment (takewhile (~= '\n') xs)) : (lex (tl (dropwhile (~= '\n') xs)))
lex ('[':xs)       = LBra:(lex xs)
lex (']':xs)       = LKet:(lex xs)
lex ('{':xs)       = WBra:(lex xs)
lex ('}':xs)       = WKet:(lex xs)
lex ('(':xs)       = Bra:(lex xs)
lex (')':xs)       = Ket:(lex xs)
lex ('+':xs)       = Opplus:(lex xs)
lex ('-':xs)       = Opminus:(lex xs)
lex ('*':xs)       = Opmult:(lex xs)
lex ('/':xs)       = Opdivide:(lex xs)
lex (' ':xs)       = lex xs
lex (':':xs)       = Opcons:(lex xs)
lex (',':xs)       = Concomma:(lex xs)
lex (x:xs)         = (Idnum (numval a)): (lex b), if (isnumber a)
                   = Funhead:(lex b), if a=['h','d']
                   = Funtail:(lex b), if a=['t','l']
                   = Conwhere:(lex b), if a=['w','h','e','r','e']
                   = Expr:(lex b), if a=['m', 'a', 'i', 'n']
                   = Stateif:(lex b), if a=['m','y','i','f']
                   = (lex b), if a=['t','h','e','n']
                   = (lex b), if a=['e','l','s','e']
                   = (Idcons a):(lex b), if (istype a)
                   = (Idvar a):(lex b), if (isvar a)
                   = (Idexrun):(lex b), if (isrun a)
                   = (returnfunc a []):(lex b), if (isfunc a)
                   = (Idintvar a):(lex b), otherwise
                     where
                     (a,b)=f (x:xs) []
                     f []       a = (a,[])
                     f (' ':xs) a = (a,xs)
                     f (')':xs) a = (a,(')':xs))
                     f (',':xs) a = (a,(',':xs))
                     f ('+':xs) a = (a,('+':xs))
                     f ('-':xs) a = (a,('-':xs))
                     f ('*':xs) a = (a,('*':xs))
                     f ('/':xs) a = (a,('/':xs))
                     f ('<':xs) a = (a,('<':xs))
                     f ('>':xs) a = (a,('>':xs))
                     f ('=':xs) a = (a,('=':xs))
                     f (']':xs) a = (a,(']':xs))
                     f (':':xs) a = (a,(':':xs))
                     f (x:xs)   a = f xs (a++[x])


isnumber x = (removeall "0123456789." (mkset x)) = []

removeall xs []     = []
removeall xs (y:ys) = removeall xs ys, if member xs y
                    = y:(removeall xs ys), otherwise

istype x = beforescore x [] = ['c']

isvar x = beforescore x [] = ['v','a','r']

isrun x = beforescore x [] = ['r', 'u', 'n']

isfunc [] = False
isfunc ('_':xs) = True
isfunc (x:xs) = isfunc xs

beforescore []       a = []
beforescore ('_':xs) a = a
beforescore (x:xs)   a = beforescore xs (a++[x])

returnfunc []       a = Idfunc a []
returnfunc ('_':xs) a = Idfunc a xs
returnfunc (x:xs)   a = returnfunc xs (a++[x])






program ::= Program [definition] experiment

definition ::= Name [char] expression | Function [char] [char] [argument] expression



argument::= Argument expression



experiment::= Experiment [globalvariables] experimentbody experimentrun

globalvariables::= Globalvariables [char] [argument] expression

experimentbody::= Emptybody|Expbody expression

experimentrun::= Emptyrun|Exprun expression




expression::= Emptyexpression
              |Ifelse expression expression expression
              |Brackets expression
              |List [argument]
              |Operation expression op expression
              |Funint [char] [argument]
              |Funext [char] [char] [argument]
              |Varint [char]
              |Varex [char]
              |Specialfunc specfunc expression
              |Number num
              |Where expression [definition]

op::= Plus
      |Minus
      |Multiply
      |Divide
      |Lessthan
      |Greaterthan
      |Equals
      |Notequals
      |Lessequ
      |Greaterequ

specfunc::= Listhead|Listtail|Listadd



t2 = "t0_bid t = t0_order c_Bid 0 0 0"
t3 = "1+3) 3"
t1= "(1+2)+3 3 4"

test = lex t1

r = get_expression test Emptyexpression
r2 = get_bracket (lex t3) []





get_expression (Bra:xs) a = get_expression new_xs new_a
                            where
                            (in_bra, new_xs) = get_bracket xs []
                            (exp_inbra, rest) = get_expression in_bra Emptyexpression
                            new_a = (Brackets (exp_inbra))
get_expression (x:xs) Emptyexpression = get_expression xs (match_exp x)
get_expression x      a               = (a, x), if end_test x
                                      = get_expression new_x new_a, otherwise
                                        where
                                        (new_x, new_a) = do_op x a





match_exp (Idnum x) = Number x
match_exp (Idintvar x) = Varint x
match_exp (Idvar x) = Varex x
||match_exp (Bra:xs) = (br_exp, rest)
||                     where
||                     (br_exp, rest) = get_bracket xs []


get_bracket (Bra:xs) a = get_bracket new_xs new_a
                         where
                         (new_a, new_xs) = get_innerbracket xs (a++[Bra])
get_bracket (Ket:xs) a = (a, xs) ||((Brackets bra_internal), xs)
                         ||where
                         ||(bra_internal, rest) = get_expression a Emptyexpression
get_bracket (x:xs)   a = get_bracket xs (a++[x])


get_innerbracket (Bra:xs) a = get_innerbracket new_xs new_a
                              where
                              (new_a, new_xs) = get_innerbracket xs (a++[Bra])
get_innerbracket (Ket:xs) a = ((a++[Ket]), xs)
get_innerbracket (x:xs)   a = get_innerbracket xs (a++[x])




match_op Opplus = Plus
match_op Opminus = Minus
match_op Opmult = Multiply
match_op Opdivide = Divide
match_op Opless = Lessthan
match_op Opgreater = Greaterthan
match_op Opequal = Equals
match_op Opnotequal = Notequals
match_op Oplessequ = Lessequ
match_op Opgreaterequ = Greaterequ


end_test [] = True
end_test (Opplus:xs) = False
end_test x = True


do_op (x:xs) a = (rest, (Operation a (match_op x) rest_a))
                 where
                 (rest_a, rest) = get_expression xs Emptyexpression























