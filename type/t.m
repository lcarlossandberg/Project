
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
              |Brackets expression ||-
              |List [argument]
              |Operation expression op expression ||-
              |Funint [char] [argument]
              |Funext [char] [char] [argument]
              |Varint [char] ||-
              |Varex [char] ||-
              |Specialfunc specfunc expression
              |Number num ||-
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



t2 = "t0_order c_Bid 0 0 0 t0_bid t = 4"
t3 = "c_Bid 1 2 3 t0_bid t 4"
t4 = "c_buy 1 2 3 c_bid 4 = 5"
t1= "(1+2)+3 3 4"

test = lex t1

r = get_expression test Emptyexpression
r2 = get_function (lex t2) []
r3 = get_argsfun (lex t3) []




get_expression (Bra:xs) a = get_expression new_xs new_a
                            where
                            (in_bra, new_xs) = get_bracket xs []
                            (exp_inbra, rest) = get_expression in_bra Emptyexpression
                            new_a = (Brackets (exp_inbra))
get_expression ((Idfunc x y):xs) a = get_expression new_xs new_a
                                     where
                                     (arg_list, new_xs) = get_function xs []
                                     ex_arglist = 
                                     new_a = (Funext x y ex_arglist)

get_expression (x:xs) Emptyexpression = get_expression xs (match_exp x) ||look at this again
get_expression x      a = (a, x), if end_test x
                        = get_expression new_x new_a, otherwise
                          where
                          (new_x, new_a) = do_op x a





match_exp (Idnum x) = Number x
match_exp (Idintvar x) = Varint x
match_exp (Idvar x) = Varex x



get_bracket (Bra:xs) a = get_bracket new_xs new_a
                         where
                         (new_a, new_xs) = get_innerbracket xs (a++[Bra])
get_bracket (Ket:xs) a = (a, xs) ||((Brackets bra_internal), xs)
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
end_test (Opminus:xs) = False
end_test (Opmult:xs) = False
end_test (Opdivide:xs) = False
end_test (Opless:xs) = False
end_test (Opgreater:xs) = False
end_test (Opequal:xs) = False
end_test (Opnotequal:xs) = False
end_test (Oplessequ:xs) = False
end_test (Opgreaterequ:xs) = False
end_test x = True


do_op (x:xs) a = (rest, (Operation a (match_op x) rest_a))
                 where
                 (rest_a, rest) = get_expression xs Emptyexpression



get_function [] a = (a, [])
get_function (Opequal:xs) a = (new_a, new_xs)
                              where
                              (mid_xs, new_a) = get_argsfun (reverse_list a []) []
                              new_xs = mid_xs++[Opequal]++xs
get_function (x:xs) a = get_function xs (a++[x])


get_argsfun ((Idfunc x y):xs) a = ((Idfunc x y):a, (reverse_list xs []))
get_argsfun ((Idcons x):xs)   a = ((Idcons x):a, (reverse_list xs []))
get_argsfun (x:xs)            a = get_argsfun xs (a++[x])

reverse_list []     a = a
reverse_list (x:xs) a = reverse_list xs (x:a)















