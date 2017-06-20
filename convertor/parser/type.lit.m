> ||This is literate script

This program will take an input file of a miranda type that fits the limitations placed on it by us.
An example file would be input.m

This files contents are then saved in the type format to be proccessed into Interdyne






Here are the first couple of lines of the sugested input file, just for quick testing of the code



>simple_example = read "inv_code.m"

>long_example = read "Input_frompaper.m"

"run_main = [main 0, main 1] var_init runnumber = myif (runnumber=0) then (10) else (20) main runnumber = [i_f1 1, i_f1 2, i_f1 3] where { i_f1 t = (i_f2 t) + (j_f1 t 1) i_f2 t = myif (t<12) then (10) else (20) j_f1 t a = myif (a<2) then (9) else (j_f2 t) j_f2 t = fun t 3 where{ fun t a = (k_f1 a)+t} k_f1 t = t + (var_init runnumber) }"

>pt = parser (lex simple_example)

>long_test = parser (lex long_example)



Lexer
Reads in the file and converts it to tokens

The lexeme contains all the tokens that will be used
LBra
LKet
Bra
Ket
WBra
WKet
Opequal
Opplus
Opminus
Opmult
Opdivide
Opgreater
Opless
Opbang
Opcons
Opnotequal
Oplessequ
Opgreaterequ
Funhead
Funtail
Concomma
Conwhere
Idcons [char]    c_name
Idfunc [char] [char] agent_name
Idintvar [char] any varible eg a b c d etc
Idvar [char] var_name
Idnum num 1,2,3,4..
Idexrun run_
Idintfunc [char] _name (an internal function where it is defined within a where statement and used in the corresponding function)
Idcomment [char]
Expr
Main
Stateif



>lexeme::= Idcons [char]|Opequal|LBra|LKet|Bra|Ket|Opplus|Opminus|Opmult|Opdivide|Opgreater|Opless|Opbang|Funhead|Funtail|Opcons|Concomma|Conwhere|Idfunc [char] [char]|Idintvar [char]|Idvar [char]|Idnum num|Expr|Opnotequal|Oplessequ|Opgreaterequ|Idexrun|Main|Stateif|Idcomment [char]|WBra|WKet|Idintfunc [char]

this shows the relations between the input langauge and the intermiate lexemes

>lex::[char]->[lexeme]
>lex []             = []
>lex ('>':'=':xs)   = Opgreaterequ:(lex xs)
>lex ('<':'=':xs)   = Oplessequ:(lex xs)
>lex ('>':xs)       = Opgreater:(lex xs)
>lex ('<':xs)       = Opless:(lex xs)
>lex ('~':'=':xs)   = Opnotequal:(lex xs)
>lex ('=':xs)       = Opequal:(lex xs)
>lex ('|':'|':xs)   = (Idcomment (takewhile (~= '\n') xs)) : (lex (tl (dropwhile (~= '\n') xs)))
>lex ('[':xs)       = LBra:(lex xs)
>lex (']':xs)       = LKet:(lex xs)
>lex ('{':xs)       = WBra:(lex xs)
>lex ('}':xs)       = WKet:(lex xs)
>lex ('(':xs)       = Bra:(lex xs)
>lex (')':xs)       = Ket:(lex xs)
>lex ('+':xs)       = Opplus:(lex xs)
>lex ('-':xs)       = Opminus:(lex xs)
>lex ('*':xs)       = Opmult:(lex xs)
>lex ('/':xs)       = Opdivide:(lex xs)
>lex ('!':xs)       = Opbang:(lex xs)
>lex (' ':xs)       = lex xs
>lex ('\n':xs)      = lex xs
>lex (':':xs)       = Opcons:(lex xs)
>lex (',':xs)       = Concomma:(lex xs)
>lex (x:xs)         = (Idnum (numval a)): (lex b), if (isnumber a)
>                   = (Idintfunc (tl a)):(lex b), if (hd a) = '_'
>                   = Funhead:(lex b), if a=['h','d']
>                   = Funtail:(lex b), if a=['t','l']
>                   = Conwhere:(lex b), if a=['w','h','e','r','e']
>                   = Expr:(lex b), if a=['m', 'a', 'i', 'n']
>                   = Stateif:(lex b), if a=['m','y','i','f']
>                   = (lex b), if a=['t','h','e','n']
>                   = (lex b), if a=['e','l','s','e']
>                   = (Idcons a):(lex b), if (iscons a)
>                   = (Idvar a):(lex b), if (isvar a)
>                   = (Idexrun):(lex b), if (isrun a)
>                   = (returnfunc a []):(lex b), if (isfunc a)
>                   = (Idintvar a):(lex b), otherwise
>                     where
>                     (a,b)=f (x:xs) []
>                     f []       a = (a,[])
>                     f (' ':xs) a = (a,xs)
>                     f (')':xs) a = (a,(')':xs))
>                     f (',':xs) a = (a,(',':xs))
>                     f ('+':xs) a = (a,('+':xs))
>                     f ('-':xs) a = (a,('-':xs))
>                     f ('*':xs) a = (a,('*':xs))
>                     f ('/':xs) a = (a,('/':xs))
>                     f ('<':xs) a = (a,('<':xs))
>                     f ('>':xs) a = (a,('>':xs))
>                     f ('=':xs) a = (a,('=':xs))
>                     f (']':xs) a = (a,(']':xs))
>                     f (':':xs) a = (a,(':':xs))
>                     f ('{':xs) a = (a,('{':xs))
>                     f ('}':xs) a = (a,('}':xs))
>                     f ('\n':xs) a = (a,xs)
>                     f (x:xs)   a = f xs (a++[x])
>

>isnumber x = (removeall "0123456789." (mkset x)) = []

>removeall xs []     = []
>removeall xs (y:ys) = removeall xs ys, if member xs y
>                    = y:(removeall xs ys), otherwise

>iscons x = beforescore x [] = ['c']

>isvar x = beforescore x [] = ['v','a','r']

>isrun x = beforescore x [] = ['r', 'u', 'n']

>isfunc [] = False
>isfunc ('_':xs) = True
>isfunc (x:xs) = isfunc xs

>beforescore []       a = a
>beforescore ('_':xs) a = a
>beforescore (x:xs)   a = beforescore xs (a++[x])

>returnfunc []       a = Idfunc a []
>returnfunc ('_':xs) a = Idfunc a xs
>returnfunc (x:xs)   a = returnfunc xs (a++[x])













Parse tree
The parse tree is componsed of type strutures that will be used to store the read in file

program is the parent type containing trees for the entire read in file


>program ::= Program [definition] experiment

definations organises the code, with the main functions either being a function or a type constant or internal varbile placed inside a where block

>definition ::= Name [char] expression | Function [char] [char] [argument] expression | InterVariable [char] expression | IntFunction [char] [argument] expression

here the arguments are the arguments to a fucntion call

>argument::= Argument expression


the experiment contains the globalvariables which are Variables that
can be changed for differnt runs of the experiment. then it has
the experiment code, detailing the function main and the code
used for the experiment. Then it has the experiment call meaning the
final experiment numbers to run

>experiment::= Experiment [globalvariables] experimentbody experimentrun

>globalvariables::= Globalvariables [char] [argument] expression

>experimentbody::= Emptybody|Expbody [argument] expression

>experimentrun::= Emptyrun|Exprun expression


expression defines the occurance of actual code in a recurance format
this should be able to represent any function

>expression::= Emptyexpression ||primarly used to intiate loops and should not ever exist in the final out put
>              |Ifelse expression expression expression ||this is the if statement taking: if condition, true code, else code
>              |Brackets expression
>              |List [expression] ||for []
>              |Operation expression op expression
>              |Funint [char] [argument] ||for internal functions
>              |Funext [char] [char] [argument] ||externally defined functions
>              |Varint [char]
>              |Varex [char] [argument]
>              |Constantvar [char]
>              |Specialfunc specfunc expression
>              |Number num
>              |Where expression [definition]
>              |Mainfunc [argument] ||this is the actual program itself

>op::= Plus
>      |Minus
>      |Multiply
>      |Divide
>      |Lessthan
>      |Greaterthan
>      |Equals
>      |Notequals
>      |Lessequ
>      |Greaterequ
>      |Listadd
>      |Bang


>specfunc::= Listhead|Listtail














Parser
This turns the tokens into the parse tree type

this section of code takes the input file and breaks it into two main sections, the body and the experemnt
the experemnt is broken down further into the global varibles the experemnt body and the experemnt run

>parser x = Program (get_definationslist a []) (get_experment b c d)
>           where
>           a = find_code1 x ||retuns lexeme between the {} in the master where
>           b = find_evar1 x ||returns lexemes whose defination states with var_
>           c = find_exprb1 x ||returns lexemes after main till where eg main a = a+2 where -> a=a+2
>           d = find_exprr1 x

find_code takes the input lexemes and returns just the body of the definations, the code inside the
experements where block.

>find_code1 []                              = []
>find_code1 (Conwhere:WBra:(Idfunc a b):xs) = find_code2 ((Idfunc a b):xs) []
>find_code1 (Conwhere:WBra:(Idcons a):xs)   = find_code2 ((Idcons a):xs) []
>find_code1 (x:xs)                          = find_code1 xs
             
>find_code2 (WBra:xs) b = find_code2 x y
>                         where
>                         (x, y) = find_code3 (xs) (b++[WBra])
>find_code2 (WKet:xs) b = b
>find_code2 (x:xs)    b = find_code2 xs (b++[x])

>find_code3 (WBra:xs) b = find_code3 xs (b++[WBra])
>find_code3 (WKet:xs) b = (xs, (b++[WKet]))
>find_code3 (x:xs)    b = find_code3 xs (b++[x])

find_evar returns a list of the global varibals Var_

>find_evar1 []             = []
>find_evar1 ((Idvar a):xs) = find_evar2 ((Idvar a):xs) []
>find_evar1 (x:xs)         = find_evar1 xs

>find_evar2 []           b = b
>find_evar2 (Expr:xs)    b = b
>find_evar2 (Idexrun:xs) b = b
>find_evar2 (x:xs) b = find_evar2 xs (b++[x])

find_exprb returns the body of what the expemetn is main ..

>find_exprb1 []        = []
>find_exprb1 (Expr:xs) = find_exprb3 (find_exprb2 xs []) []
>find_exprb1 (x:xs)    = find_exprb1 xs

>find_exprb2 []        b = b
>find_exprb2 (Expr:xs) b = find_exprb2 xs []
>find_exprb2 (x:xs)    b = find_exprb2 xs (b++[x])

>find_exprb3 []            b = b
>find_exprb3 (Conwhere:xs) b = b
>find_exprb3 (x:xs)        b = find_exprb3 xs (b++[x])

returns the expemrent run expression run_main

>find_exprr1 []           = []
>find_exprr1 (Idexrun:Opequal:xs) = find_exprr2 xs []
>find_exprr1 (x:xs)       = find_exprr1 xs

>find_exprr2 []             b = b
>find_exprr2 ((Idvar a):xs) b = b
>find_exprr2 ((Expr):xs)    b = b, if (find_exprr3 xs)
>                             = find_exprr2 xs (b++[Expr]), otherwise
>find_exprr2 (x:xs)         b = find_exprr2 xs (b++[x])

>find_exprr3 []        = True
>find_exprr3 (Expr:xs) = False
>find_exprr3 (x:xs)    = find_exprr3 xs







this section breaks the code into a list of definations with their associated expressions, this takes a list of lexemes that would represent this list of deifntions and returns a list of definations

>get_definationslist::[lexeme]->[definition]->[definition]
>get_definationslist [] defs = defs
>get_definationslist ((Idcons x):Opequal:xs)   defs = get_definationslist new_xs new_defs
>                                                     where
>                                                     (exp, new_xs) = get_expression xs Emptyexpression
>                                                     def = Name x exp
>                                                     new_defs = defs ++ [def]
>get_definationslist ((Idfunc x y):xs)         defs = get_definationslist new_xs new_defs
>                                                     where
>                                                     (arglist1, rest) = get_inputargs xs []
>                                                     arglist2 = f arglist1 []
>                                                     f [] a = a
>                                                     f x a = f new_x new_a
>                                                             where
>                                                             (mid_a, new_x) = get_expression x Emptyexpression
>                                                             new_a = a++[Argument mid_a]
>                                                     (exp, new_xs) = get_expression rest Emptyexpression
>                                                     def = Function x y arglist2 exp
>                                                     new_defs = defs ++ [def]
>get_definationslist ((Idintfunc x ):xs)       defs = get_definationslist new_xs new_defs
>                                                     where
>                                                     (arglist1, rest) = get_inputargs xs []
>                                                     arglist2 = f arglist1 []
>                                                     f [] a = a
>                                                     f x a = f new_x new_a
>                                                             where
>                                                             (mid_a, new_x) = get_expression x Emptyexpression
>                                                             new_a = a++[Argument mid_a]
>                                                     (exp, new_xs) = get_expression rest Emptyexpression
>                                                     def = IntFunction x arglist2 exp
>                                                     new_defs = defs ++ [def]
>get_definationslist ((Idintvar x):Opequal:xs) defs = get_definationslist new_xs new_defs  ||this one is for definations inside where statemnts like where{ a=1}
>                                                     where
>                                                     (exp, new_xs) = get_expression xs Emptyexpression
>                                                     def = InterVariable x exp
>                                                     new_defs = defs ++ [def]




Here the code expressions are turned into the expression type, this is for things after an =, so if you had a = 4+3 this would deal with the 4+3
this takes in the liast of lexemes and a pplace holder expression, normally Emptyexpression, which is used to look at the previos iteraction when self calling.

>get_expression::[lexeme]->expression->(expression, [lexeme])
>get_expression x                 Emptyexpression = (new_a, new_xs), if istherewhere x
>                                                   where
>                                                   (bwhere, awhere) = wheresplitter x []
>                                                   (inter_where, new_xs) = get_wherestate awhere []
>                                                   def_list = get_definationslist inter_where []
>                                                   (mid_a, none) = get_expression bwhere Emptyexpression
>                                                   new_a = Where mid_a def_list
>get_expression ((Idcomment x):xs)              a = get_expression xs a
>get_expression (Funhead:xs)      Emptyexpression = (new_a, new_xs)
>                                                   where
>                                                   (mid_a, new_xs) = get_expression xs Emptyexpression
>                                                   new_a = Specialfunc Listhead mid_a
>get_expression (Funtail:xs)      Emptyexpression = (new_a, new_xs)
>                                                   where
>                                                   (mid_a, new_xs) = get_expression xs Emptyexpression
>                                                   new_a = Specialfunc Listtail mid_a
>get_expression (LBra:xs)         Emptyexpression = get_expression new_xs new_a
>                                                   where
>                                                   (listeditems, new_xs) = f xs []
>                                                   f (LKet:xs) a = (a, xs)
>                                                   f (Concomma:xs) a = f xs a
>                                                   f x a = f nn_xs nnn_a
>                                                           where
>                                                           (mid_a, nn_xs) = get_expression x Emptyexpression
>                                                           nnn_a = a++[mid_a]
>                                                   new_a = List listeditems
>get_expression (Stateif:xs)      Emptyexpression = get_expression new_xs new_a
>                                                   where
>                                                   (if_con, if_yes, if_no, new_xs) = get_ifstatement xs
>                                                   (true_con, rest1) = get_expression if_con Emptyexpression
>                                                   (true_yes, rest2) = get_expression if_yes Emptyexpression
>                                                   (true_no, rest3) = get_expression if_no Emptyexpression
>                                                   new_a = (Ifelse true_con true_yes true_no)
>get_expression (Bra:xs)          Emptyexpression = get_expression new_xs new_a
>                                                   where
>                                                   (in_bra, new_xs) = get_bracket xs []
>                                                   (exp_inbra, rest) = get_expression in_bra Emptyexpression
>                                                   new_a = (Brackets (exp_inbra))
>get_expression ((Idfunc x y):xs) Emptyexpression = get_expression new_xs new_a
>                                                   where
>                                                   (arg_list, new_xs) = get_function xs []
>                                                   ex_arglist = f arg_list []
>                                                   f [] m = m
>                                                   f n m = f nn nnm
>                                                           where
>                                                           (nm, nn) = get_expression n Emptyexpression
>                                                           nnm = m++[Argument nm]
>                                                   new_a = (Funext x y ex_arglist)
>get_expression ((Idintfunc x ):xs) Emptyexpression = get_expression new_xs new_a                             ||shortcut
>                                                     where
>                                                     (arg_list, new_xs) = get_function xs []
>                                                     ex_arglist = f arg_list []
>                                                     f [] m = m
>                                                     f n m = f nn nnm
>                                                             where
>                                                             (nm, nn) = get_expression n Emptyexpression
>                                                             nnm = m++[Argument nm]
>                                                     new_a = (Funint x ex_arglist)
>get_expression (Expr:xs)         Emptyexpression = get_expression new_xs new_a
>                                                   where
>                                                   (arg_list, new_xs) = get_function xs []
>                                                   ex_arglist = f arg_list []
>                                                   f [] m = m
>                                                   f n m = f nn nnm
>                                                           where
>                                                           (nm, nn) = get_expression n Emptyexpression
>                                                           nnm = m++[Argument nm]
>                                                   new_a = (Mainfunc ex_arglist)
>get_expression ((Idnum x):xs)    Emptyexpression = get_expression xs (Number x)
>get_expression ((Idintvar x):xs) Emptyexpression = get_expression xs (Varint x)
>get_expression ((Idvar x):xs)    Emptyexpression = get_expression new_xs new_a
>                                                   where
>                                                   (arg_list, new_xs) = get_function xs []
>                                                   ex_arglist = f arg_list []
>                                                   f [] m = m
>                                                   f n m = f nn nnm
>                                                           where
>                                                           (nm, nn) = get_expression n Emptyexpression
>                                                           nnm = m++[Argument nm]
>                                                   new_a = (Varex x ex_arglist)
>get_expression ((Idcons x):xs)   Emptyexpression = get_expression xs (Constantvar x)
>get_expression x                 a               = (a, x), if end_test x
>                                                 = get_expression new_x new_a, otherwise
>                                                   where
>                                                   (rest_a, new_x) = get_expression (tl x) Emptyexpression
>                                                   new_a = (Operation a (match_op (hd x)) rest_a)



this part deals purely with brackets and can deal with nested breakes by calling the function get_innerbracket to deal wiht them

>get_bracket::[lexeme]->[lexeme]->([lexeme], [lexeme])
>get_bracket (Bra:xs) a = get_bracket new_xs new_a
>                         where
>                         (new_a, new_xs) = get_innerbracket xs (a++[Bra])
>get_bracket (Ket:xs) a = (a, xs)
>get_bracket (x:xs)   a = get_bracket xs (a++[x])


>get_innerbracket::[lexeme]->[lexeme]->([lexeme], [lexeme])
>get_innerbracket (Bra:xs) a = get_innerbracket new_xs new_a
>                              where
>                              (new_a, new_xs) = get_innerbracket xs (a++[Bra])
>get_innerbracket (Ket:xs) a = ((a++[Ket]), xs)
>get_innerbracket (x:xs)   a = get_innerbracket xs (a++[x])


returns the new type constrastures sperific for the operactiosn from the lexeme

>match_op::lexeme->op
>match_op Opplus       = Plus
>match_op Opminus      = Minus
>match_op Opmult       = Multiply
>match_op Opdivide     = Divide
>match_op Opless       = Lessthan
>match_op Opgreater    = Greaterthan
>match_op Opequal      = Equals
>match_op Opnotequal   = Notequals
>match_op Oplessequ    = Lessequ
>match_op Opgreaterequ = Greaterequ
>match_op Opcons       = Listadd
>match_op Opbang       = Bang


checks if the expression has eneded, the only times it has not ended is is this expression is connected to another through a operactior

>end_test::[lexeme]->bool
>end_test []                = True
>end_test (Opplus:xs)       = False
>end_test (Opminus:xs)      = False
>end_test (Opmult:xs)       = False
>end_test (Opdivide:xs)     = False
>end_test (Opless:xs)       = False
>end_test (Opgreater:xs)    = False
>end_test (Opequal:xs)      = False
>end_test (Opnotequal:xs)   = False
>end_test (Oplessequ:xs)    = False
>end_test (Opgreaterequ:xs) = False
>end_test (Opcons:xs)       = False
>end_test (Opbang:xs)       = False
>end_test x                 = True



goes through the function to check if it contains a where statement at the end

>istherewhere::[lexeme]->bool
>istherewhere []           = False
>istherewhere (Stateif:xs) = istherewhere new_xs
>                            where
>                            (con, yes, no, new_xs) = get_ifstatement xs
>istherewhere (Opequal:xs)  = False
>istherewhere (Conwhere:xs) = True
>istherewhere (x:xs)        = istherewhere xs


breaks the code into the code before the the where statement (the function) and the code after the where, which will contain the where statemtn and all other definations

>wheresplitter::[lexeme]->[lexeme]->([lexeme],[lexeme])
>wheresplitter (Conwhere:WBra:xs) a = (a, xs)
>wheresplitter (x:xs)             a = wheresplitter xs (a++[x])

returns the aguments the function and the rest of the code

>get_function::[lexeme]->[lexeme]->([lexeme],[lexeme])
>get_function []            a = (a, [])
>get_function (Concomma:xs) a = (a, xs) ||newnewnew
>get_function (LKet:xs) a = (a, (LKet:xs)) ||newnewnew
>get_function (Opequal:xs)  a = (new_a, new_xs)
>                               where
>                               (mid_xs, new_a) = get_argsfun (reverse_list a []) []
>                               new_xs = mid_xs++[Opequal]++xs
>get_function (x:xs)        a = get_function xs (a++[x])


>get_argsfun::[lexeme]->[lexeme]->([lexeme],[lexeme])
>get_argsfun ((Idfunc x y):xs) a = ((Idfunc x y):a, (reverse_list xs []))
>get_argsfun ((Idcons x):xs)   a = ((Idcons x):a, (reverse_list xs []))
>get_argsfun (x:xs)            a = get_argsfun xs (a++[x])

>reverse_list []     a = a
>reverse_list (x:xs) a = reverse_list xs (x:a)

>get_wherestate::[lexeme]->[lexeme]->([lexeme],[lexeme])
>get_wherestate (WKet:xs) a = (a, xs)
>get_wherestate (x:xs)    a = get_wherestate xs (a++[x])

>get_inputargs::[lexeme]->[lexeme]->([lexeme],[lexeme])
>get_inputargs (Opequal:xs) a = (a, xs)
>get_inputargs (x:xs)       a = get_inputargs xs (a++[x])

breaks and if statement into its section if, true, false and rest 

>get_ifstatement::[lexeme]->([lexeme],[lexeme],[lexeme],[lexeme])
>get_ifstatement x = (con, yes, no, rest)
>                    where
>                    (con, rest1) = get_bracket (tl x) []
>                    (yes, rest2) = get_bracket (tl rest1) []
>                    (no, rest)   = get_bracket (tl rest2) []









experiment::= Experiment [globalvariables] experimentbody experimentrun

globalvariables::= Globalvariables [char] [argument] expression

experimentbody::= Emptybody|Expbody [argument] expression

experimentrun::= Emptyrun|Exprun expression

a=var
b=body
c=run

>get_experment a b c = Experiment globvar expermentBody expermentRun
>                      where
>                      globvar = get_globalvarlist a []
>                      expermentBody = get_experimentbody b []
>                      expermentRun = f c
>                      f [] = Emptyrun
>                      f x  = Exprun runcode
>                             where
>                             (runcode, none) = get_expression c Emptyexpression

This turns the list of lexemes containing the global varibles into a list of the global varibles which are deifined in globalvariables

>get_globalvarlist::[lexeme]->[globalvariables]->[globalvariables]
>get_globalvarlist [] defs = defs
>get_globalvarlist ((Idvar x):xs)              defs = get_globalvarlist new_xs new_defs ||this is used to get the global varible list
>                                                     where
>                                                     (arglist1, rest) = get_inputargs xs []
>                                                     arglist2 = f arglist1 []
>                                                     f [] a = a
>                                                     f x a = f new_x new_a
>                                                             where
>                                                             (mid_a, new_x) = get_expression x Emptyexpression
>                                                             new_a = a++[Argument mid_a]
>                                                     (exp, new_xs) = get_expression rest Emptyexpression
>                                                     def = Globalvariables x arglist2 exp
>                                                     new_defs = defs ++ [def]



>get_experimentbody []           []    = Emptybody
>get_experimentbody (Opequal:xs) argsl = Expbody args body
>                                        where
>                                        (body, none) = get_expression xs Emptyexpression
>                                        args = f argsl []
>                                        f [] a = a
>                                        f x  a = f n_x n_a
>                                                 where
>                                                 (mid_a, n_x) = get_expression x Emptyexpression
>                                                 n_a = a++[Argument mid_a]
>get_experimentbody (x:xs)       argsl = get_experimentbody xs (argsl++[x])















Convertor
This will turn the now parsed program into one comparable with Interdyne














Infinite List output
Here the program will be transformed so that all functions that depend on time will be written with an infinite list output and no time parameter

j_f1 t a = myif (a<2) then (9) else (j_f2 t)

will become

j_f1 a = [sub_j_f1 t a | t<-[0..]]
         where
         sub_j_f1 t a = myif (a<2) then (9) else (j_f2 t)

which can be written as a loop in the form

j_f1_chris a = myloop 0 a
               where
               createlist t a = (sub_j_f1 t a) : (createlist list (t+1) a)
               sub_j_f1 t a = myif (a<2) then (9) else (j_f2 t)


This creates a sort of parent eqution that outputs the child (orginal) equations output in the correct form

All time dependent functions will be turned into this form


ilo takes the program type and then returns a new program type which reperesents the modified program

>ilo::program->program
>ilo (Program a b) = Program c b
>                    where c = ilo_ndl a []


ilo_ndl (new defination list), takes the defination list and returns the edited defination list


>ilo_ndl []     list = list
>ilo_ndl (x:xs) list = ilo_ndl xs (list++[edit_x])
>                      where
>                      edit_x = ilo_ndi x


ilo_ndi (new difintion item), takes an item from the defination list and then either returns that item as is if it is time idepenent or returns the edited list version if it is time dependent.
A time dependent function will have the Variable t as an input parameter


>ilo_ndi (Name a b)          = Name a b
>ilo_ndi (InterVariable a b) = InterVariable a b
>ilo_ndi (Function a b c d)  = edited_function
>                              where
>                              edited_function = ilo_nlf (Function a b c d), if (ilo_ct c)
>                                              = (Function a b c d), otherwise


ilo_ct (contains time) takes the arguments of the function and checks to see if time (t) is present, if it is this returns true and the function will hence be changed accordingly

>ilo_ct []                           = False
>ilo_ct ((Argument (Varint "t")):xs) = True
>ilo_ct (x:xs)                       = ilo_ct xs



ilo_nlf (new list function) takes a function that contains time and turns it into a fucntion that does not take time as an input and as an infite list output


>ilo_nlf (Function a b c d) = Function a b na_nt ne_il
>                             where
>                             na_nt = ilo_na c []           ||(new arguments no time)
>                             ne_il = ilo_ne c d            ||(new expression infinite list)


ilo_na (new arguments) takes the current argument list and returns it after removing t


>ilo_na []                           new_c = new_c
>ilo_na ((Argument (Varint "t")):xs) new_c = ilo_na xs new_c
>ilo_na (x:xs)                       new_c = ilo_na xs (new_c++[x])


ilo_ne (new expression) takes the current expression and rerutns the infinite list loop expression in the form shown Here

j_f1_chris a = _createlist 0 a
               where
               _createlist t a = (_sub_j_f1 t a) : (_createlist list (t+1) a)
               _sub_j_f1 t a = myif (a<2) then (9) else (j_f2 t)



>ilo_ne c d = Where (ilo_le c) (ilo_df c d)


ilo_le (list expression) returns the new expression
_createlist 0 ++ arguments


>ilo_le c = Funint "createlist" (ilo_nc c [])


ilo_nc (new c) edits the arguments so that t now equals 0

>ilo_nc []                           list = list
>ilo_nc ((Argument (Varint "t")):xs) list = ilo_nc xs (list++[(Argument (Number 0))])
>ilo_nc (x:xs)                       list = ilo_nc xs (list++[x])


ilo_df (definition of function) creates a definition list containing two definition, one for createlist and the other containing the logic of the unedited function
               
>ilo_df c d = [(IntFunction "createlist" c (ilo_cll c)), (IntFunction "sublogic" c d)]


ilo_cll (create list logic) this returns the logic of the create list

>ilo_cll c = Operation (Funint "sublogic" c) (Listadd) (Funint "createlist" (ilo_tpo c []))


ilo_tpo (t plus one) takes the input variables and returns them but has t as t+1



>ilo_tpo []                           list = list
>ilo_tpo ((Argument (Varint "t")):xs) list = ilo_tpo xs (list++[(Argument (Operation (Varint "t") (Plus) (Number 1)))])
>ilo_tpo (x:xs)                       list = ilo_tpo xs (list++[x])





>newt = (ilo (parser (lex simple_example))) ||=(parser (lex simple_example))




Here the requests for a function that now outputs a list has to be edited



















This is not working code
I will use this code to help make a print statement when I have time

printprog :: prog -> [char]
printprog (Prog a b) = "{\n" ++ (printdefs a) ++ "\n}\nIN\n" ++ (printexpr b)

printdefs :: defs -> [char]
printdefs (Emptydefs) = []
printdefs (Defse a)   = printfundef a
printdefs (Defs a rest) = (printfundef a)++"\n"++(printdefs rest)

printfundef :: fundef -> [char]
printfundef (Fundef name as e) = name++" "++(printargs as)++" = "++(printexpr e)++",\n"

printargs :: args -> [char]
printargs Emptyargs   = []
printargs (Argse a)   = (printarg a)
printargs (Args x xs) = (printarg x)++" "++(printargs xs)

printarg :: arg -> [char]
printarg (Arg name) = name

printexpr :: expr -> [char]
printexpr (Variable name)  = name
printexpr (Opexpr e1 o e2) = (printexpr e1)++" "++(printop o)++" "++(printexpr e2)
printexpr (App e1 e2)      = (printexpr e1)++" "++(printexpr e2)
printexpr (Brackets e)     = "("++(printexpr e)++")"
printexpr (Numcon x)       = (show x)

printop :: op -> [char]
printop Pluss   = "+"
printop Mults   = "*"
printop Minuss  = "-"
printop Divides = "/"

