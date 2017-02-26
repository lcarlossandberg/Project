
t = "run_main = [main 0, main 1] var_selltime e = 10 var_t1startinv e = 2000 var_t2startinv e = 2000 var_nu e = 0 var_ul e = 3000 var_ll e = (-3000) var_startprice e = 2000 main exp_n = myif (exp_n = 1) then ([t1_inv 0, t1_inv 1, t1_inv 2]) else ([t2_inv 0, t2_inv 1, t2_inv 3]) where c_Buy = 0 c_Sell = 1 c_Bid = 2 c_Ask = 3 t0_sell t = myif (t < (var_selltime exp_n) ) then (t0_order c_Sell 1000 0 0) else (t0_order c_Sell 0 0 0) t0_order a b c d = [a, b, c, d] t0_bid t = t0_order c_Bid 0 0 0 t0_ask t = t0_order c_Ask 0 0 0 t0_buy t = t0_order c_Buy 0 0 0 t1_inv t = myif (t = 0) then ((var_t1startinv exp_n) ) else ((t1_inv (t-1)) + (t1_psi (t1_xbids(t-1))) + (t1_psi (t1_xbuys(t-1))) - (t1_psi (t1_xasks(t-1))) - (t1_psi (t1_xsells(t-1)))) t1_psi x = myif (x = []) then (0) else (myif ((t1_frh (hd x)) = 1) then ((t1_snd (hd x))+(t1_psi (tl x))) else (t1_psi (tl x))) t1_xbids  t = t1_snd (e1_exchoutput1 t) t1_xbuys  t = t1_thd (e1_exchoutput2 t) t1_xasks  t = t1_snd (e1_exchoutput2 t) t1_xsells t = t1_thd (e1_exchoutput1 t)"




t2 = "{ y_x1 { x2 { x3 } x4 }"

t3 = "a b c where { c_buy = 1 { y_x1 { x2 { x3 } x4 } }"

test = lex t






lexeme::= Idtype [char]|Opequal|LBra|LKet|Bra|Ket|Opplus|Opminus|Opmult|Opdivide|Opgreater|Opless|Funhead|Funtail|Opcons|Concomma|Conwhere|Idfunc [char] [char]|Idintvar [char]|Idvar [char]|Idnum num|Expr|Opnotequal|Oplessequ|Opgreaterequ|Idexrun|Main|Stateif|Idcomment [char]|WBra|WKet

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
                   = (Idtype a):(lex b), if (istype a)
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

beforescore []       a = a
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
find_code1 (Conwhere:WBra:(Idtype a):xs)   = find_code2 ((Idtype a):xs) []
find_code1 (x:xs)                          = find_code1 xs

find_code2 (WBra:xs) b = find_code2 x y
                         where
                         (x, y) = find_code3 (xs) (b++[WBra])
find_code2 (WKet:xs) b = b
find_code2 (x:xs)    b = find_code2 xs (b++[x])


find_code3 (WBra:xs) b = find_code3 xs (b++[WBra])
find_code3 (WKet:xs) b = (xs, (b++[WKet]))
find_code3 (x:xs)    b = find_code3 xs (b++[x])










