
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
                   = (Idcons a):(lex b), if (iscons a)
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
                     f ('{':xs) a = (a,('{':xs))
                     f ('}':xs) a = (a,('}':xs))
                     f (x:xs)   a = f xs (a++[x])


isnumber x = (removeall "0123456789." (mkset x)) = []

removeall xs []     = []
removeall xs (y:ys) = removeall xs ys, if member xs y
                    = y:(removeall xs ys), otherwise

iscons x = beforescore x [] = ['c']

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

definition ::= Name [char] expression | Function [char] [char] [argument] expression | InterVariable [char] expression



argument::= Argument expression



experiment::= Experiment [globalvariables] experimentbody experimentrun

globalvariables::= Globalvariables [char] [argument] expression

experimentbody::= Emptybody|Expbody expression

experimentrun::= Emptyrun|Exprun expression




expression::= Emptyexpression
              |Ifelse expression expression expression
              |Brackets expression
              |List [expression]
              |Operation expression op expression
              |Funint [char] [argument]
              |Funext [char] [char] [argument]
              |Varint [char]
              |Varex [char] [argument]
              |Constantvar [char]
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
      |Listadd

specfunc::= Listhead|Listtail


t = "c_Buy = 0 t0_sell t = myif (t < (var_selltime exp_n) ) then (t0_order 1000 0 0) else (t0_order c_Sell 0 0 0) t0_buy t = t0_order c_Buy 0 0 0 t1_xsells t = t1_thd (e1_exchoutput1 t) t1_bidprice bestbid bestask inv = t1_max 0 ((midprice-1)-alpha) where { midprice = ((bestbid+bestask)/2) alpha    = zeta*(1-(((var_ul exp_n)-1-inv)/((var_ul exp_n)-(var_ll exp_n)-2))) zeta     = 6}"

t2 = "t0_sell t = myif (t < (var_selltime runnumber) ) then (0) else (0)"

t3="t0_order a = [hd a]:[tl b] ||hi! \n"

test = lex t3

r = get_definationslist test []


get_definationslist [] defs = defs
get_definationslist ((Idcons x):Opequal:xs) defs = get_definationslist new_xs new_defs
                                                   where
                                                   (exp, new_xs) = get_expression xs Emptyexpression
                                                   def = Name x exp
                                                   new_defs = defs ++ [def]
get_definationslist ((Idfunc x y):xs) defs = get_definationslist new_xs new_defs
                                             where
                                             (arglist1, rest) = get_inputargs xs []
                                             arglist2 = f arglist1 []
                                             f [] a = a
                                             f x a = f new_x new_a
                                                     where
                                                     (mid_a, new_x) = get_expression x Emptyexpression
                                                     new_a = a++[Argument mid_a]
                                             (exp, new_xs) = get_expression rest Emptyexpression
                                             def = Function x y arglist2 exp
                                             new_defs = defs ++ [def]
get_definationslist ((Idintvar x):Opequal:xs) defs = get_definationslist new_xs new_defs
                                                     where
                                                     (exp, new_xs) = get_expression xs Emptyexpression
                                                     def = InterVariable x exp
                                                     new_defs = defs ++ [def]





get_expression x Emptyexpression = (new_a, new_xs), if istherewhere x
                                   where
                                   (bwhere, awhere) = wheresplitter x []
                                   (inter_where, new_xs) = get_wherestate awhere []
                                   def_list = get_definationslist inter_where []
                                   (mid_a, none) = get_expression bwhere Emptyexpression
                                   new_a = Where mid_a def_list
get_expression ((Idcomment x):xs) a = get_expression xs a
get_expression (Funhead:xs) Emptyexpression = (new_a, new_xs)
                                              where
                                              (mid_a, new_xs) = get_expression xs Emptyexpression
                                              new_a = Specialfunc Listhead mid_a
get_expression (Funtail:xs) Emptyexpression = (new_a, new_xs)
                                              where
                                              (mid_a, new_xs) = get_expression xs Emptyexpression
                                              new_a = Specialfunc Listtail mid_a
get_expression (LBra:xs) Emptyexpression = get_expression new_xs new_a
                                           where
                                           (listeditems, new_xs) = f xs []
                                           f (LKet:xs) a = (a, xs)
                                           f (Concomma:xs) a = f xs a
                                           f x a = f nn_xs nnn_a
                                                   where
                                                   (mid_a, nn_xs) = get_expression x Emptyexpression
                                                   nnn_a = a++[mid_a]
                                           new_a = List listeditems
get_expression (Stateif:xs) Emptyexpression = get_expression new_xs new_a
                                              where
                                              (if_con, if_yes, if_no, new_xs) = get_ifstatement xs
                                              (true_con, rest1) = get_expression if_con Emptyexpression
                                              (true_yes, rest2) = get_expression if_yes Emptyexpression
                                              (true_no, rest3) = get_expression if_no Emptyexpression
                                              new_a = (Ifelse true_con true_yes true_no)
get_expression (Bra:xs) Emptyexpression = get_expression new_xs new_a
                                          where
                                          (in_bra, new_xs) = get_bracket xs []
                                          (exp_inbra, rest) = get_expression in_bra Emptyexpression
                                          new_a = (Brackets (exp_inbra))
get_expression ((Idfunc x y):xs) Emptyexpression = get_expression new_xs new_a
                                                   where
                                                   (arg_list, new_xs) = get_function xs []
                                                   ex_arglist = f arg_list []
                                                   f [] m = m
                                                   f n m = f nn nnm
                                                           where
                                                           (nm, nn) = get_expression n Emptyexpression
                                                           nnm = m++[Argument nm]
                                                   new_a = (Funext x y ex_arglist)
get_expression ((Idnum x):xs) Emptyexpression = get_expression xs (Number x)
get_expression ((Idintvar x):xs) Emptyexpression = get_expression xs (Varint x)
get_expression ((Idvar x):xs) Emptyexpression = get_expression new_xs new_a
                                                where
                                                (arg_list, new_xs) = get_function xs []
                                                ex_arglist = f arg_list []
                                                f [] m = m
                                                f n m = f nn nnm
                                                        where
                                                        (nm, nn) = get_expression n Emptyexpression
                                                        nnm = m++[Argument nm]
                                                new_a = (Varex x ex_arglist)
get_expression ((Idcons x):xs) Emptyexpression = get_expression xs (Constantvar x)
get_expression x      a = (a, x), if end_test x
                        = get_expression new_x new_a, otherwise
                          where
                          (rest_a, new_x) = get_expression (tl x) Emptyexpression
                          new_a = (Operation a (match_op (hd x)) rest_a)



get_listeditems (LKet:xs) a = (a, xs)
get_listeditems (Concomma:xs) a = get_listeditems xs a
get_listeditems x a = get_listeditems new_xs nn
                      where
                      (new_a, new_xs) = get_expression x Emptyexpression
                      nn = a++[new_a]






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
match_op Opcons = Listadd


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
end_test (Opcons:xs) = False
end_test x = True

istherewhere []           = False
istherewhere (Stateif:xs) = istherewhere new_xs
                            where
                            (con, yes, no, new_xs) = get_ifstatement xs
istherewhere (Opequal:xs) = False
istherewhere (Conwhere:xs) = True
istherewhere (x:xs) = istherewhere xs

wheresplitter (Conwhere:WBra:xs) a = (a, xs)
wheresplitter (x:xs) a = wheresplitter xs (a++[x])

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

get_wherestate (WKet:xs) a = (a, xs)
get_wherestate (x:xs) a = get_wherestate xs (a++[x])

get_inputargs (Opequal:xs) a = (a, xs)
get_inputargs (x:xs)       a = get_inputargs xs (a++[x])

get_ifstatement x = (con, yes, no, rest)
                    where
                    (con, rest1) = get_bracket (tl x) []
                    (yes, rest2) = get_bracket (tl rest1) []
                    (no, rest)   = get_bracket (tl rest2) []







































