
t = "run_main = [main 0, main 1] var_selltime e = 10 var_t1startinv e = 2000 var_t2startinv e = 2000 var_nu e = 0 var_ul e = 3000 var_ll e = (-3000) var_startprice e = 2000 main exp_n = myif (exp_n = 1) then ([t1_inv 0, t1_inv 1, t1_inv 2]) else ([t2_inv 0, t2_inv 1, t2_inv 3]) where c_Buy = 0 c_Sell = 1 c_Bid = 2 c_Ask = 3 t0_sell t = myif (t < (var_selltime exp_n) ) then (t0_order c_Sell 1000 0 0) else (t0_order c_Sell 0 0 0) t0_order a b c d = [a, b, c, d] t0_bid t = t0_order c_Bid 0 0 0 t0_ask t = t0_order c_Ask 0 0 0 t0_buy t = t0_order c_Buy 0 0 0 t1_inv t = myif (t = 0) then ((var_t1startinv exp_n) ) else ((t1_inv (t-1)) + (t1_psi (t1_xbids(t-1))) + (t1_psi (t1_xbuys(t-1))) - (t1_psi (t1_xasks(t-1))) - (t1_psi (t1_xsells(t-1)))) t1_psi x = myif (x = []) then (0) else (myif ((t1_frh (hd x)) = 1) then ((t1_snd (hd x))+(t1_psi (tl x))) else (t1_psi (tl x))) t1_xbids  t = t1_snd (e1_exchoutput1 t) t1_xbuys  t = t1_thd (e1_exchoutput2 t) t1_xasks  t = t1_snd (e1_exchoutput2 t) t1_xsells t = t1_thd (e1_exchoutput1 t)"




t2 = "{ y_x1 { x2 { x3 } x4 }"

t3 = "a b c where { c_buy = 1 { y_x1 { x2 { x3 } x4 } }"








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



tester









find_exprr1 []           = []
find_exprr1 (Idexrun:xs) = find_exprr2 xs []
find_exprr1 (x:xs)       = find_exprr1 xs

find_exprr2 []             b = b
find_exprr2 ((Idvar a):xs) b = b
find_exprr2 ((Expr):xs)    b = b, if (find_exprr3 xs)
                             = find_exprr2 xs (b++[Expr]), otherwise
find_exprr2 (x:xs)         b = find_exprr2 xs (b++[x])

find_exprr3 []        = True
find_exprr3 (Expr:xs) = False
find_exprr3 (x:xs)    = find_exprr3 xs


find_code1 []                              = []
find_code1 (Conwhere:WBra:(Idfunc a b):xs) = find_code2 ((Idfunc a b):xs) []
find_code1 (Conwhere:WBra:(Idcons a):xs)   = find_code2 ((Idcons a):xs) []
find_code1 (x:xs)                          = find_code1 xs

find_code2 (WBra:xs) b = find_code2 x y
                         where
                         (x, y) = find_code3 (xs) (b++[WBra])
find_code2 (WKet:xs) b = b
find_code2 (x:xs)    b = find_code2 xs (b++[x])


find_code3 (WBra:xs) b = find_code3 xs (b++[WBra])
find_code3 (WKet:xs) b = (xs, (b++[WKet]))
find_code3 (x:xs)    b = find_code3 xs (b++[x])








||r = find_def1 test


||find_def1 ((Idcons a):Opequal:xs) = Name a (p_expression xs)
||find_def1 ((Idfunc a b):xs) = Function a b (find_argl1 c []) (p_expression d)
||                              where
||                              (c, d) = find_argex1 xs []


||p_expression [] = Emptyexpression
||p_expression ((Idnum a):xs) = find_op1 xs (Number a)
||p_expression ((Idvar a):xs) = find_op1 xs (Varex a)
||p_expression ((Idintvar a):xs) = find_op1 xs (Varint a)
||p_expression (Bra:xs) = find_op1 rest (Brackets (p_expression inner))
||                        where
||                        (inner, rest) = find_ket1 xs []


||find_op1 []     a = a
||find_op1 (x:xs) a = Operation a b (p_expression xs), if tf
||                  = a, otherwise
||                    where
||                    (tf, b) = find_op2 x

||find_op2 Opplus     = (True, Plus)
||find_op2 Opminus    = (True, Minus)
||find_op2 Opmult     = (True, Multiply)
||find_op2 Opdivide   = (True, Divide)
||find_op2 Opless     = (True, Lessthan)
||find_op2 Opgreater  = (True, Greaterthan)
||find_op2 Opequal    = (True, Equals)
||find_op2 Opnotequal = (True, Notequals)
||find_op2 Oplessequ  = (True, Lessequ)
||find_op2 Opgreaterequ = (True, Greaterequ)
||find_op2 x = (False, Plus)

||find_ket1 (Ket:xs) b = (b, xs)
||find_ket1 (Bra:xs) b = find_ket1 rest (b++mid)
||                       where
||                       (mid, rest) = find_ket2 xs []
||find_ket1 (x:xs) b = find_ket1 xs (b++[x])

||find_ket2 (Ket:xs) b = ((b++[Ket]), xs)
||find_ket2 (x:xs) b = find_ket2 xs (b++[x])


||find_argex1 (Opequal:xs) b = (b, xs)
||find_argex1 (x:xs) b = find_argex1 xs (b++[x])

||find_argl1 []     b = b
||find_argl1 (x:xs) b = find_argl1 xs (b++[Argument (p_expression [x])])


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
              |Funext [char] [argument]
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





t4= "1 2 3"
t5= "t0_bid t = t0_order c_Bid 0 0 0"
t6= "t0_bid t c = (1+2)+3"
t7=  []

test = lex t4
r = get_expression test Emptyexpression

||r = p_definations test []

||[lexme]->[definition]

||[lexeme]->expression+[lexeme] (one expression and the rest of the list)


||p_definations::[lexeme]->[definition]->[definition]
||p_definations []     deflist = deflist
||p_definations ((Idcons a):xs) deflist = p_definations rest new_deflist
||                                        where
||                                        new_deflist = newdef:deflist
||                                        newdef = Name a body
||                                        (body, rest) = get_expression xs

||get_expression (x:xs) = ((Number 1), [Idcons "e"])


||get_expression::[lexeme]->(expression, [lexeme])
||get_expression (x:rest) a = (a, rest), if end_test x
||                          = get_expression rest aa
||                            where
||                            aa = add_op x aa


get_expression (x:xs) a = ((match_exp x), xs), if end_test xs
                        = get_expression xs aa, otherwise
                          where
                          aa = match_exp (Idnum x)




f_enda x Emptyexpression = match_exp x

match_exp (Idnum x) = Number x

||f_oper (x:xs:xss) Emptyexpression = (Operation (match_exp x) (match_op xs) (rest_a), rest_l)
||                                    where
||                                    (rest_a, rest_l) = get_expression xs Emptyexpression



||get_expression (x:xs) a = (new_a, xs), if end_test xs
||                        = get_expression rest aa, otherwise
||                          where
||                          new_a = fnew_a a x
||                          (aa, rest) = faa (x:xs) a

end_test [] = True
end_test x = hd x ~= Opplus
||end_test x = hd x ~= Opminus
||end_test x = hd x ~= Opmult
||end_test x = hd x ~= Opdivide
||end_test x = hd x ~= Opless
||end_test x = hd x ~= Opgreater
||end_test x = hd x ~= Opequal
||end_test x = hd x ~= Opnotequal
||end_test x = hd x ~= Oplessequ
||end_test x = hd x ~= Opgreaterequ
||end_test x = True



||fnew_a Emptyexpression x = match_lexex x
||fnew_a a x = Number 3



||faa (x:xs) a = ((Operation a (find_op x) next_a), rest)
||               where
||               (next_a, rest) = get_expression xs Emptyexpression


match_op Opplus     = Plus
match_op Opminus    = Minus
match_op Opmult     = Multiply
match_op Opdivide   = Divide
match_op Opless     = Lessthan
match_op Opgreater  = Greaterthan
match_op Opequal    = Equals
match_op Opnotequal = Notequals
match_op Oplessequ  =  Lessequ
match_op Opgreaterequ = Greaterequ

||x =Idintvar 1


















||find_def1 ((Idcons a):Opequal:xs) = Name a (p_expression xs)
||find_def1 ((Idfunc a b):xs) = Function a b (find_argl1 c []) (p_expression d)
||                              where
||                              (c, d) = find_argex1 xs []


||p_expression [] = Emptyexpression
||p_expression ((Idnum a):xs) = find_op1 xs (Number a)
||p_expression ((Idvar a):xs) = find_op1 xs (Varex a)
||p_expression ((Idintvar a):xs) = find_op1 xs (Varint a)
||p_expression (Bra:xs) = find_op1 rest (Brackets (p_expression inner))
||                        where
||                        (inner, rest) = find_ket1 xs []


||find_op1 []     a = a
||find_op1 (x:xs) a = Operation a b (p_expression xs), if tf
||                  = a, otherwise
||                    where
||                    (tf, b) = find_op2 x

||find_op2 Opplus     = (True, Plus)
||find_op2 Opminus    = (True, Minus)
||find_op2 Opmult     = (True, Multiply)
||find_op2 Opdivide   = (True, Divide)
||find_op2 Opless     = (True, Lessthan)
||find_op2 Opgreater  = (True, Greaterthan)
||find_op2 Opequal    = (True, Equals)
||find_op2 Opnotequal = (True, Notequals)
||find_op2 Oplessequ  = (True, Lessequ)
||find_op2 Opgreaterequ = (True, Greaterequ)
||find_op2 x = (False, Plus)

||find_ket1 (Ket:xs) b = (b, xs)
||find_ket1 (Bra:xs) b = find_ket1 rest (b++mid)
||                       where
||                       (mid, rest) = find_ket2 xs []
||find_ket1 (x:xs) b = find_ket1 xs (b++[x])

||find_ket2 (Ket:xs) b = ((b++[Ket]), xs)
||find_ket2 (x:xs) b = find_ket2 xs (b++[x])





















