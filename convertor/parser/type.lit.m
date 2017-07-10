> ||This is literate script

This program will take an input file of a miranda type that fits the limitations placed on it by us.
An example file would be input.m

This files contents are then saved in the type format to be proccessed into Interdyne






Here are the first couple of lines of the sugested input file, just for quick testing of the code



>simple_example = read "inv_code.m"

>long_example = read "Input_frompaper.m"

"run_main = [main 0, main 1] var_init runnumber = myif (runnumber=0) then (10) else (20) main runnumber = [i_f1 1, i_f1 2, i_f1 3] where { i_f1 t = (i_f2 t) + (j_f1 t 1) i_f2 t = myif (t<12) then (10) else (20) j_f1 t a = myif (a<2) then (9) else (j_f2 t) j_f2 t = fun t 3 where{ fun t a = (k_f1 a)+t} k_f1 t = t + (var_init runnumber) }"





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
>              |Tuple expression expression expression


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
This is called con_itl (Convertor infinite time lists)


>con_itl a = ili (ilo a)











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


ilo (infinite list output) takes the program type and then returns a new program type which reperesents the modified program

>ilo::program->program
>ilo (Program a b) = Program c b
>                    where c = ilo_ndl a []


ilo_ndl (new defination list), takes the defination list and returns the edited defination list


>ilo_ndl []     list = list
>ilo_ndl (x:xs) list = ilo_ndl xs (list++[edit_x])
>                      where
>                      edit_x = ilo_ndi x


ilo_ndi (new definition item), takes each item in the definition list, if its a c_ or a variable it returns it as is, if its a agent_ returns a list output version. This is done for both time dependent and time indepent agent_
A time dependent function will have the Variable t as an input parameter


>ilo_ndi (Name a b)          = Name a b
>ilo_ndi (InterVariable a b) = InterVariable a b
>ilo_ndi (Function a b c d)  = edited_function
>                              where
>                              edited_function = ilo_nlf (Function a b c d), if (ilo_ct c)    ||deals with time dependent Functions
>                                              = ilo_nlfnt (Function a b c d), otherwise      ||deals with time indepent functions



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



ilo_nlfnt (new list Function no time) takes a time indepent fucntion and returns a fucntion that lists that functions output through out time (will be the same at every time step)

>ilo_nlfnt(Function a b c d) = Function a b c ne_il  ||there are no new arguments as time is already not present
>                              where
>                              ne_il = ilo_nent c d




ilo_nent (new expression no time)  takes the current expression and rerutns the infinite list loop expression
j_f1_chris a = _createlist 0 a
               where
               _createlist t a = (_sub_j_f1 t a) : (_createlist list (t+1) a)
               _sub_j_f1 t a = myif (a<2) then (9) else (20)

t is added to the expression so that indexing can be done latter



>ilo_nent c d = Where (ilo_wrap) (ilo_df ((Argument (Varint "t")):c) d) ||t is added to the arguments list, this is unused at the moment
>               where
>               ilo_wrap = Funint "createlist" ((Argument (Number 0)):c)









Here the requests for a function that now outputs a list has to be edited
a function would previesly call another by used f 1 this no longer will work, instead since the output of this function is now for all time starting at t=0, a call for t=1 will be f!1
However most functions will call dependening upon their own time step t (this may not be currently avable to every function but will be once a wrapper function is written and starts recving its time step) therefor for fucntions that are not time dependent the call will be f!t



ili (infinite list input) this takes the type program and returns a new value of type program, in this new value only the call to  functions has been altered

>ili (Program a b) = Program (ili_ed a []) (ili_eeb b)


ili_ed (edited definitions) this returns a list of the edited definitions

>ili_ed [] new_dl = new_dl
>ili_ed (x:xs) new_dl = ili_ed xs (new_dl++[new_x])
>                       where
>                       new_x = ili_cfc x


ili_cfc (change function calls) looks at each fucntions expression and changes any calls to fucntions


>ili_cfc (Name a b)          = Name a b
>ili_cfc (InterVariable a b) = InterVariable a b
>ili_cfc (Function a b c d)  = Function a b c new_d
>                              where
>                              new_d = ili_cfd d



ili_cfd (change function definition) this changes a fucntins expression so that it now calls infinite lists correctly
All functions are now in where blocks
the function body is in the list of definitions

>ili_cfd (Where a b) = Where a (ili_cfwb b)

ili_cfwb (change function where block), this changes the expression in the where block
It is known that this list will only containt two items the createlist function and the sublogic function
it is the sublogic function that needs to be edited

>ili_cfwb ((IntFunction "createlist" a b):(IntFunction "sublogic" c d):xs) = [(IntFunction "createlist" a b), (IntFunction "sublogic" c new_d)]
>                                                                            where
>                                                                            new_d = ili_ce d


ili_ce (change expression) changes the actual expression


>ili_ce (Emptyexpression) = Emptyexpression
>ili_ce (Ifelse a b c)    = Ifelse (ili_ce a) (ili_ce b) (ili_ce c)
>ili_ce (Brackets d)      = Brackets (ili_ce d)
>ili_ce (List e)          = List (ili_la e [])
>ili_ce (Operation f g h) = Operation (ili_ce f) g (ili_ce h)
>ili_ce (Funint i j)      = Funint i (ili_aa j [])
>ili_ce (Varint n)        = Varint n
>ili_ce (Varex o p)       = Varex o (ili_aa p [])
>ili_ce (Constantvar q)   = Constantvar q
>ili_ce (Specialfunc r s) = Specialfunc r (ili_ce s)
>ili_ce (Number t)        = Number t
>ili_ce (Where u v)       = Where (ili_ce u) (ili_da v [])
>ili_ce (Funext k l m)    = ili_nif k l m



ili_la (list apply), applies ili_ce to every element in a list and then retuns the new list

>ili_la []     new_list = new_list
>ili_la (x:xs) new_list = ili_la xs (new_list++[(ili_ce x)])


ili_aa (argument apply), applies ili_ce to every expression in a list of arguments

>ili_aa []                new_list = new_list
>ili_aa ((Argument x):xs) new_list = ili_aa xs (new_list++[new_x])
>                                    where
>                                    new_x = Argument (ili_ce x)


ili_da (definitions apply), applies ili_ce to every expression of every definition in the list of definitions

>ili_da []     new_list = new_list
>ili_da (x:xs) new_list = ili_da xs (new_list++[new_x])
>                         where
>                         new_x = ili_dsa x

ili_dsa (definition sub apply), takes the definition and returns the definition with an edited expression

>ili_dsa (Name a b)          = Name a (ili_ce b)
>ili_dsa (Function a b c d)  = Function a b (ili_aa c []) (ili_ce d)
>ili_dsa (InterVariable a b) = InterVariable a (ili_ce b)
>ili_dsa (IntFunction a b c) = IntFunction a (ili_aa b []) (ili_ce c)



ili_nif (new idexed function) this changes how functions are called so that they are idex as lists with time, this is done by noting that the first argument of any function is time, therefore can remove this first argument and use it to idex

Funext [char] [char] [argument]

goes to

Operation (Funext [char] [char] [new_arguments]) Bang (Varint (t what ever is in the old argument t)) or (Varint 't')

>ili_nif k l m = Operation (Funext k l (ili_new_m m)) (Bang) (ili_indexer m)


ili_new_m retuns the arument list Minus the time argument, this is done knowing that the first argument is time

>ili_new_m (x:xs) = xs

ili_indexer, this retuns the  expression of the first agrument from the argument list, this is time and hence will be used for indexing

>ili_indexer ((Argument x):xs) = x



ili_eeb (edited experemnt body)

>ili_eeb (Experiment a b c) = Experiment a new_b c
>                             where
>                             new_b = ili_ceb b

ili_ceb (change Experiment body) changes the expression in the Experiment body

>ili_ceb (Emptybody)   = Emptybody
>ili_ceb (Expbody a b) = Expbody a (ili_ce b)














Con_ag (convert agents), this adds agents

>con_ag a = awc_cc (aw_cw a)




Here the wrapper function is created which groups together a agent and produces a infite list contain a finite list of the values at each time step of the internal functions, the functions also have to be changed

agent_Wrapper = _createlistw 0
                where
                _createlistw t = ([agent_f1!t, agent_f2!t, ..]):(_createlistw (t+1))
                agent_f1
                agent_f2
                agent_f3





here the agent_Wrapper will be added using aw_cw (agent wrapper create wrapper )

>aw_cw (Program a b) = Program new_a b
>                      where
>                      new_a = aw_gf a []


aw_gf (group functions) changes the defination list to have wrappers

>aw_gf []                      new_defs = new_defs
>aw_gf ((Function a b c d):xs) new_defs = aw_gf new_xs new_new_defs
>                                         where
>                                         new_new_defs = new_defs++[new_agent]
>                                         new_agent = aw_raf ((Function a b c d):xs) a
>                                         new_xs = aw_rol xs [] a
>aw_gf (x:xs)                  new_defs = aw_gf xs (new_defs++[x])



aw_rol (rest of list), retuns a list of all functions and names that are not the agent being looked for

>aw_rol [] list id = list
>aw_rol ((Function a b c d):xs) list id = aw_rol xs new_list id
>                                         where
>                                         new_list = list, if a = id
>                                                  = list++[(Function a b c d)], otherwise
>aw_rol (x:xs) list id = aw_rol xs (list++[x]) id



aw_raf (return agent function) retuns a function which is the wrapper with the agent inside


>aw_raf x id = agent
>              where
>              agent = aw_cta agentlist id
>              agentlist = aw_apol x [] id


aw_apol (agent part of list), this returns a list of only the functions associated with the agent this is important as it can be used to additfy the list and order of said list for each agent

>aw_apol []                      list id = list
>aw_apol ((Function a b c d):xs) list id = aw_apol xs new_list id
>                                          where
>                                          new_list = list++[(Function a b c d)], if a = id
>                                                   = list, otherwise
>aw_apol (x:xs)                  list id = aw_apol xs list id


aw_cta (create the agent) creates the agent with the list of what functions are in the agent

>aw_cta x id = Function agent wrap args expr
>              where
>              agent = id
>              wrap  = "wrapper"
>              args  = []
>              expr  = aw_rwe x


aw_rwe (return wrapper expression), returns the where statement and functionality of the wrapper function

agent_Wrapper = _createlistw 0
                where
                _createlistw t = ([agent_f1!t, agent_f2!t, ..]):(_createlistw (t+1))
                agent_f1
                agent_f2
                agent_f3


>aw_rwe x = Where wrapcall wrapfun
>           where
>           wrapcall = Funint "createlistw" [Argument (Number 0)]
>           wrapfun  = wrapint:edited_x
>           edited_x = aw_cftif x []
>           wrapint  = IntFunction "createlistw" [Argument (Varint "t")] wrapexp
>           wrapexp  = Operation (wrapfunlist) (Listadd) (Funint "createlistw" argtp)
>           argtp    = [Argument (Operation (Varint "t") (Plus) (Number 1))]
>           wrapfunlist = List (aw_globc x [])


aw_cftif (convert functions to internal functions), takes the list of type Function and returns one of type IntFucntion


>aw_cftif [] list = list
>aw_cftif ((Function a b c d):xs) list = aw_cftif xs (list++[IntFunction b c d])


aw_globc (get list of bang calls) this rerutns a list of expression of calls to all functions in the agent bang with t


>aw_globc []                      list = list
>aw_globc ((Function a b c d):xs) list = aw_globc xs new_list
>                                        where
>                                        new_list = list++[Operation (Funint b c) (Bang) (Varint "t")]











Here the new call for external functions is added. Any Function not in the same agent now has to be called as an output of its parent agents wrapper function.

the wraper function outputs a infinite list with each element relating to each time step.
each of these elements them selfs are lists which contain that times output for each fucntion within the agent.

therefore to access a function the wrapper has to be called as so ((wrapper!t)!func)
t starts at 0 and func is which function in the list it is, this list starts at 0 and is order the same way as it is defined in the input file.

This is done in two parts, one that idenfies what calls need to be changed and changes them, the second is needed for this and can identify the number in the output list that relates to the function


awc_cc (agent wrapper call change call), this function returns the the experemnt but with new calls to external functions

>awc_cc (Program def_list exp) = Program new_def_list new_exp
>                                where
>                                new_def_list = awc_cdl def_list [] def_list ||the second def_list is needed later on to find what number in a agent a function is
>                                new_exp = awc_ce exp def_list "na"          ||need to make na or something as an unusable agent name


awc_cdl (change def list) this returns the def list with the new call

>awc_cdl []                        def_list df = def_list
>awc_cdl ((Function agt b c d):xs) def_list df = awc_cdl xs (def_list++[new_wrapfun]) df
>                                                where
>                                                new_wrapfun = Function agt b c new_where
>                                                new_where = awc_nwsa d df agt             ||agt is the agent identifier
>awc_cdl (x:xs)                    def_list df = awc_cdl xs (def_list++[x]) df


awc_nwsa (new where statement agent) this changes the where statement in a agents function to have the write calls

>awc_nwsa (Where expr def_list) df agt = Where expr new_def_list
>                                        where
>                                        new_def_list = awc_cdla def_list [] df agt

awc_cdla (chang def list agent), this changes the def list in the agent
this def list contains names and the actual functions inside there list time wrappers
the first function will be the agent out defination called



>awc_cdla []                       edit_defs df agt = edit_defs
>awc_cdla ((IntFunction a b c):xs) edit_defs df agt = awc_cdla xs (edit_defs++[new_def]) df agt
>                                                     where
>                                                     new_def = (IntFunction a b c), if a = "createlistw"
>                                                             = (IntFunction a b new_sub), otherwise
>                                                     new_sub = awc_csdc c df agt
>awc_cdla ((Name a b):xs)          edit_defs df agt = awc_cdla xs (edit_defs++[(Name a b)]) df agt


||there shouldnt be an functions or InterVariable defiontions here, if there are a error will be thrown



awc_csdc (change sublogic defination calls) changes the calls in the sublogic to be correct

>awc_csdc (Where exp defs) df agt = Where exp new_defs
>                                   where
>                                   new_defs = awc_cds defs [] df agt


awc_cds (change definations) changes the definations to have the correct calls
(list should only contain IntFunction at this level)

>awc_cds []                       new_defs df agt = new_defs
>awc_cds ((IntFunction a b c):xs) new_defs df agt = awc_cds xs (new_defs++[new_def]) df agt
>                                                   where
>                                                   new_def = (IntFunction a b c), if a = "createlist"
>                                                           = (IntFunction a b new_exp), otherwise  ||this fucntion should be called "sublogic"
>                                                   new_exp = awc_ccie c df agt


awc_ccie (change calls in expression), changes the expression (inside _sublogic) to make the write calls
all external functions are already being called with a ! relating to the time they are being called from. Thereofore a function my look like
a_f!t
this needs to change to
(a!f)!t
therefore external functions (Funext) will only be encounted in a Operation with a bang


>awc_ccie (Ifelse a b c)    df agt = Ifelse (awc_ccie a df agt) (awc_ccie b df agt) (awc_ccie c df agt)
>awc_ccie (Brackets a)      df agt = Brackets (awc_ccie a df agt)
>awc_ccie (List a)          df agt = List (awc_cciel a [] df agt)
>awc_ccie (Operation a b c) df agt = awc_teftif (Operation a b c) df agt, if awc_tfsbif a agt
>                                  = awc_nfcb (Operation a b c) df agt, if awc_tfief a agt
>                                  = Operation (awc_ccie a df agt) b (awc_ccie c df agt), otherwise
>awc_ccie (Funint a b)      df agt = Funint a (awc_ccal b [] df agt)
>awc_ccie (Varint a)        df agt = Varint a
>awc_ccie (Varex a b)       df agt = Varex a (awc_ccal b [] df agt)
>awc_ccie (Constantvar a)   df agt = Constantvar a
>awc_ccie (Specialfunc a e) df agt = Specialfunc a (awc_ccie e df agt)
>awc_ccie (Number a)        df agt = Number a
>awc_ccie (Where e defs)    df agt = Where (awc_ccie e df agt) (awc_ccdl defs [] df agt)



awc_teftif (turn external function to internal function), this turns external fucntion calls, of functions that are of the same agetn into internal function calls

>awc_teftif (Operation a b c) df agt = Operation new_a b c
>                                      where
>                                      new_a = awc_tfc a df agt

awc_tfc (transform function call), tuns an external call into an internal call

>awc_tfc (Funext a b c) df agt = Funint b (awc_ccal c [] df agt)


awc_tfsbif (true false should be internal function), checks if this should be a interla function, eg does it have the same agent name

>awc_tfsbif (Funext a b c) agt = True, if a = agt
>                              = False, otherwise
>awc_tfsbif x              agt = False




awc_nfcb (new function call bang) this creates the new Operation which bangs the function


>awc_nfcb (Operation (Funext ag fc args) bg time) df agt = Operation funcb Bang time
>                                                          where
>                                                          funcb   = Operation agent Bang funnum
>                                                          agent   = Funext ag "wrapper" []
>                                                          funnum  = Number (awc_ftfn ag fc df)


awc_ftfn (find the function number), this function finds what number function output it is within the wrapper function
the first function will be 0



>awc_ftfn agent function df = funcnum
>                             where
>                             funcnum = awc_rfn defs function 0
>                             defs    = awc_rdfs wrapper
>                             wrapper = awc_faw df agent



awc_faw (find agent wrapper), returns the expression from the agent wrapper that relates to the agent identifier
df is the full definition list from program [definition] experiment
this is a list of Names and function wrappers

>awc_faw ((Function agent name args exp):xs) id = exp, if agent = id
>                                               = awc_faw xs id, otherwise


awc_rdfs (return defiontion functions ) returns a list just of the function defiontions

>awc_rdfs (Where exp defs) = new_defs
>                            where
>                            new_defs = awc_rmcl defs []

awc_rmcl (remove _createlistw), removes the _createlistw function from the list and returns just the list of function defiontions

>awc_rmcl []                       deflis = deflis
>awc_rmcl ((IntFunction a b c):xs) deflis = awc_rmcl xs deflis, if a = "createlistw"
>                                         = awc_rmcl xs (deflis++[IntFunction a b c]), otherwise
>awc_rmcl (x:xs)                   deflis = awc_rmcl xs (deflis++[x])


awc_rfn (return function number), this returns what number in the list a function is

>awc_rfn defs id b = b, if check
>                  = awc_rfn defs id (b+1), otherwise
>                    where
>                    check = awc_gcd def id
>                    def   = defs!b




awc_gcd (get conformation defiontions), returns true if the defiontions is the b'th item


>awc_gcd (IntFunction a b c) id = True, if a = id
>                               = False, otherwise





awc_tfief (true false is external function)

>awc_tfief (Funext a b c) agt = True, if a ~= agt
>                             = False, otherwise
>awc_tfief x              agt = False




awc_cciel (change call in expression list), retuns list of expressions, with calls changed

>awc_cciel []     lise df agt = lise
>awc_cciel (x:xs) lise df agt = awc_cciel xs (lise++[new_x]) df agt
>                               where
>                               new_x = awc_ccie x df agt


awc_ccal (change call argument list), changes the argument list to be correct

>awc_ccal []                arglist df agt = arglist
>awc_ccal ((Argument x):xs) arglist df agt = awc_ccal xs (arglist++[nex_argx]) df agt
>                                            where
>                                            nex_argx = Argument new_x
>                                            new_x    = (awc_ccie x df agt)


awc_ccdl (change call defionition list)

>awc_ccdl []     deflist df agt = deflist
>awc_ccdl (x:xs) deflist df agt = awc_ccdl xs (deflist++[new_x]) df agt
>                                 where
>                                 new_x = aw_cdc x df agt

aw_cdc (change defintion call), changes a defintion to have the correct

>aw_cdc (Name a ex)             df agt = Name a (awc_ccie ex df agt)
>aw_cdc (Function a b args ex)  df agt = Function a b (awc_ccal args [] df agt) (awc_ccie ex df agt)
>aw_cdc (InterVariable a ex)    df agt = InterVariable a (awc_ccie ex df agt)
>aw_cdc (IntFunction a args ex) df agt = IntFunction a (awc_ccal args [] df agt) (awc_ccie ex df agt)




awc_ce (change expression)


>awc_ce::experiment->[definition]->[char]->experiment
>awc_ce (Experiment gv eb er) df agt = Experiment gv new_eb er
>                                      where
>                                      new_eb = awc_neb eb df agt


awc_neb (new Experiment body )

>awc_neb (Expbody args ex) df agt = Expbody args new_ex
>                                   where
>                                   new_ex = awc_ccie ex df agt














con_harn (convert harness) adds the harness to the program

>con_harn a = phc_cc (ph_ch a)

here the harness is added
the harness creates an finite list with each element being the output list of one wrapper function

The harness is added as just another function, though it will be the first definition in the full list of definitions

program_harness = [i_wrapper, j_wrapper, k_wrapper, ..]


ph_ch (program harnes create harnes), creates the program harness

>ph_ch (Program a b) = Program new_a b
>                      where
>                      new_a = ph_cdl a ||add def of harness

ph_cdl (change definition list), adds the definition of the harness to the front of the  definitiont list

>ph_cdl defs = harnness_def:defs
>              where
>              harnness_def = ph_chd defs

ph_chd (create harness definition), returns the harness definition

>ph_chd defs = harness_def
>              where
>              harness_def = Function "program" "harness" [] expr
>              expr         = ph_che defs

ph_che (create harness expression) returns the expression for the harness

>ph_che defs = harness_expr
>              where
>              harness_expr = List expr_list
>              expr_list    = ph_cel defs []

ph_cel (create expression list), returns a list of the calls to the wrapper fucntions for their outputs

>ph_cel::[definition]->[expression]->[expression]
>ph_cel []                      expr_list = expr_list
>ph_cel ((Name a b):xs)         expr_list = ph_cel xs expr_list
>ph_cel ((Function a b c d):xs) expr_list = ph_cel xs new_expr_list
>                                           where
>                                           new_expr_list = expr_list++[Funext a b c]






phc_cc (program harness calls change calls), changes the calls to external functions to call the harness

>phc_cc (Program a b) = Program new_a new_b
>                       where
>                       new_a = phc_cdl a [] ol
>                       new_b = phc_cpe b ol
>                       ol    = phc_rol a ||returns a ordered list of the wrapper functions


phc_rol (return ordered list), retuns a orded list of the wrapper functions

>phc_rol ((Function a b c d):xs) = wrap_list, if a = "program" & b = "harness"
>                                = phc_rol xs, otherwise
>                                  where
>                                  wrap_list = phc_fol d
>phc_rol ((Name a b):xs)         = phc_rol xs


phc_fol (find order list), reutns the list of wrapper expressions from inside the expression for the program harness

>phc_fol (List exp_list) = exp_list



phc_cdl (change definition list)

>phc_cdl []                      new_defs ol = new_defs
>phc_cdl ((Name a b):xs)         new_defs ol = phc_cdl xs (new_defs++[Name a b]) ol
>phc_cdl ((Function a b c d):xs) new_defs ol = phc_cdl xs (new_defs++[Function a b c d]) ol, if a = "program" & b = "harness"
>                                            = phc_cdl xs (new_defs++[Function a b c new_d]) ol, otherwise
>                                              where
>                                              new_d = phc_cwe d ol


phc_cwe (change wrapper expression)

>phc_cwe (Where exp defs) ol = Where exp new_defs  ||this is a wrapper function
>                              where
>                              new_defs = phc_cdliw defs [] ol

phc_cdliw (change def list in wrapper), this changes the definition list in the wrapper to have the correct calls

>phc_cdliw []                       ndl ol = ndl
>phc_cdliw ((IntFunction a b c):xs) ndl ol = phc_cdliw xs (ndl++[IntFunction a b c]) ol, if a = "createlistw"
>                                          = phc_cdliw xs (ndl++[IntFunction a b new_c]) ol, otherwise
>                                            where
>                                            new_c = phc_cfw c ol

phc_cfw (change function where), changes the functions where statement to have the correct calls

>phc_cfw (Where exp defs) ol = Where exp new_defs
>                              where
>                              new_defs = phc_cfd defs [] ol

phc_cfd (change function definitions), changes that definition list of the function

>phc_cfd []                       defs ol = defs
>phc_cfd ((IntFunction a b c):xs) defs ol = phc_cfd xs (defs++[IntFunction a b c]) ol, if a = "createlist"
>                                         = phc_cfd xs (defs++[IntFunction a b new_c]) ol, otherwise
>                                           where
>                                           new_c = phc_cfe c ol

phc_cfe (change function expression), changes the function expression to have the correct calls to external functions

the call to external functions has now changed a call used to be

(j_wrapper!agent)!t

where agent is the number the agent is in the wrapper now a call will be

(((program_harness!wrapper)!agent)!t)

where wrapper is the wrapper number in the output for program_harness (in this case j would be 1, as i is 0)




>phc_cfe (Ifelse a b c)     ol = Ifelse (phc_cfe a ol) (phc_cfe b ol) (phc_cfe c ol)
>phc_cfe (Brackets a)       ol = Brackets (phc_cfe a ol)
>phc_cfe (List a)           ol = List (phc_cciel a [] ol)
>phc_cfe (Operation a b c)  ol = phc_nfcb (Operation a b c) ol, if phc_iefc a b c
>                              = Operation (phc_cfe a ol) b (phc_cfe c ol), otherwise
>phc_cfe (Funint a b)       ol = Funint a (phc_ccal b [] ol)
>phc_cfe (Varint a)         ol = Varint a
>phc_cfe (Varex a b)        ol = Varex a (phc_ccal b [] ol)
>phc_cfe (Constantvar a)    ol = Constantvar a
>phc_cfe (Specialfunc a e)  ol = Specialfunc a (phc_cfe e ol)
>phc_cfe (Number a)         ol = Number a
>phc_cfe (Where e defs)     ol = Where (phc_cfe e ol) (phc_ccdl defs [] ol)





phc_cciel (change call in expression list), retuns list of expressions, with calls changed

>phc_cciel []     lise ol = lise
>phc_cciel (x:xs) lise ol = phc_cciel xs (lise++[new_x]) ol
>                           where
>                           new_x = phc_cfe x ol



phc_nfcb (new function call bang), changes the external function call to call in the right order to the program harnes

>phc_nfcb (Operation func bg time) ol = Operation new_func bg time
>                                       where
>                                       new_func = phc_nfcb2 func ol

>phc_nfcb2 (Operation wrap bg agent) ol = Operation new_wrap bg agent
>                                         where
>                                         new_wrap = phc_nfcb3 wrap ol



>phc_nfcb3 wrap ol = Operation (Funext "program" "harness" []) Bang (wrapnum)
>                    where
>                    wrapnum = Number (phc_fwn wrap ol 0)


phc_fwn (find wrapper number) returns the number of the wrapper in the wrapper in the program haness list

>phc_fwn wrap ol num = num, if check
>                    = phc_fwn wrap ol (num+1), otherwise
>                      where
>                      check = phc_fwn2 wrap wrapfound
>                      wrapfound = ol!num

>phc_fwn2 wrap wrapfound = True, if wrap = wrapfound
>                        = False, otherwise






phc_iefc (is external function call), returns true if this is a call to an external function

>phc_iefc (Operation (Funext q w e) j k) b c = True, if b = Bang & j = Bang
>                                            = False, otherwise
>phc_iefc x                              y z = False








phc_ccal (change call argument list), changes the argument list to be correct

>phc_ccal []                arglist ol = arglist
>phc_ccal ((Argument x):xs) arglist ol = phc_ccal xs (arglist++[nex_argx]) ol
>                                        where
>                                        nex_argx = Argument new_x
>                                        new_x    = (phc_cfe x ol)






phc_ccdl (change call defionition list)

>phc_ccdl []     deflist ol = deflist
>phc_ccdl (x:xs) deflist ol = phc_ccdl xs (deflist++[new_x]) ol
>                             where
>                             new_x = phc_cdc x ol

phc_cdc (change defintion call), changes a defintion to have the correct

>phc_cdc (Name a ex)             ol = Name a (phc_cfe ex ol)
>phc_cdc (Function a b args ex)  ol = Function a b (phc_ccal args [] ol) (phc_cfe ex ol)
>phc_cdc (InterVariable a ex)    ol = InterVariable a (phc_cfe ex ol)
>phc_cdc (IntFunction a args ex) ol = IntFunction a (phc_ccal args [] ol) (phc_cfe ex ol)






phc_cpe (change program expression)

>phc_cpe (Experiment gv eb er) ol = Experiment gv new_eb er
>                                   where
>                                   new_eb = phc_eeb eb ol

>phc_eeb (Expbody args exp) ol = Expbody args new_exp
>                                where
>                                new_exp = phc_cfe exp ol 



















This section adds message passing, by adding agruments to the program harness and the wrapper functions

con_mp (convert message passing)

>con_mp a = mpa a

Here arguments are added to the program_harness which takes the output of each wrapper as a argument
arguments added to each wrapper which takes the program_harness as the argument

mpa (convert message passing arguments), adds the message arguments and correct arguments calls

>mpa a = mpav a


mpav (message passing arguments varibles), adds arguments to program_harness and wrappers

>mpav (Program a b) = Program new_a b
>                     where
>                     new_a = mpav_cwl a [] a

mpav_cwl (change wrapper list), changes the list of wrappers and program_harness to have the correct arguments

>mpav_cwl []                         new_defs defs = new_defs
>mpav_cwl ((Function a b args d):xs) new_defs defs = mpav_cwl xs (new_defs++[new_def]) defs
>                                                    where
>                                                    new_def = Function a b prog_args d, if a="program" & b="harness"
>                                                            = Function a b wrap_args d, otherwise
>                                                    prog_args = mpav_afp defs []
>                                                    wrap_args = mpav_afw
>mpav_cwl (x:xs)                     new_defs defs = mpav_cwl xs (new_defs++[x]) defs


mpav_afp (arguments for program)

>mpav_afp []                      args = args
>mpav_afp ((Function a b c d):xs) args = mpav_afp xs new_args
>                                        where
>                                        new_args = args ++ [Argument exp], if b = "wrapper"
>                                                 = args, otherwise
>                                        exp = Funext a b c
>mpav_afp (x:xs)                  args = mpav_afp xs args


mpav_afw (arguments for wrappers)

>mpav_afw = [arg]
>           where
>           arg = Argument progharn
>           progharn = Funext "program" "harness" []


















This section add directed messages with a to from componet

added to from messages which are sent to the harness
(from, to, value) The value is function!time
There will be a new message for every different value a wrapper my require
These will be output as a list [messages]
This will be passed to the harness, the harness will receive one of these lists from each wrapper at each time step
The harness will then output a list of lists (this will obvioulsy be within an infite time list), where each item in the main list is for a different wrapper, and then within that item is a list of each value the wrapper will recive
[i_wrapper, j_wrapper, k_wrapper]
where i_wrapper will be
[j_f1!t]
These new input values will be linked to there usage in the wrapper via a variable name



>con_dm a = dmi (dmh (dmo a))

dmo (direct messagging output), creates the output messages from the wrappers

>dmo (Program a b) = Program new_a b
>                    where
>                    new_a = dmo_ndl a [] cl a
>                    cl    = dmo_ccl a

dmo_ccl (create call list), this returns a [[[]]], the top list contains the data for each wrapper, the first wrapper will index !0, getting back a [[]]. list has an element for each other wrapper and contains a list what values are required for each wrapper
[i_wrapper, j_wrapper, k_wrapper] ! (which wrapper you are in) eg i
[j_wrapper, k_wrapper] ! (both eventually, but which one you are sending to), eg j
[f1] this is the list of values that j wants from i

>dmo_ccl defs = call_list
>               where
>               call_list = dmo_ccls wrappers [] wrappers
>               wrappers = dmo_groh defs []

dmo_groh (get rid of harness), retuns the def list with only wrappers, getting rid of the harness and any constants

>dmo_groh []                      wraplist = wraplist
>dmo_groh ((Function a b c d):xs) wraplist = dmo_groh xs (wraplist++[(Function a b c d)]), if b= "wrapper"
>                                          = dmo_groh xs (wraplist), otherwise
>dmo_groh (x:xs)                  wraplist = dmo_groh xs wraplist


dmo_ccls (create call list sub), actually returns the call list

>dmo_ccls []     clist wraps = clist
>dmo_ccls (x:xs) clist wraps = dmo_ccls xs (clist++[calls]) wraps
>                              where
>                              calls = dmo_ccfx notx [] x wraps
>                              notx = dmo_nx wraps [] x


dmo_nx (not x), retuns the wrapper list, without the wrapper relating to x

>dmo_nx []     nxwraps x = nxwraps
>dmo_nx (y:ys) nxwraps x = dmo_nx ys (nxwraps++[y]) x, if y~=x
>                        = dmo_nx ys nxwraps x, otherwise



dmo_ccfx (creat calls for x), returns a list of the wrappers calling for values from x

>dmo_ccfx []     xlist x wraps = xlist
>dmo_ccfx (y:ys) xlist x wraps = dmo_ccfx ys (xlist++[xcalls]) x wraps
>                                where
>                                xcalls = dmo_cfxciy y xnum
>                                xnum   = dmo_fxn x wraps 0

dmo_fxn (find x number), retuns the number in the list of wrappers that x is

>dmo_fxn x wraps b = b, if check
>                  = dmo_fxn x wraps (b+1), otherwise
>                    where
>                    check = True, if (wraps!b) = x
>                    check = False, otherwise

dmo_cfxciy (create function x calls in y), returns a list of calls to x from y
takes y_wrapper = ... looks inside the functions, and returns a list of what calls this wrapper makes to x

>dmo_cfxciy (Function a b c exp) xnum = intflist
>                                       where
>                                       intflist = dmo_cfl flist [] xnum
>                                       flist    = dmo_dcl fflist []
>                                       fflist   = dmo_fif exp

dmo_dcl (delete create list), reutns the list of definations with out the createlist defination

>dmo_dcl []                       flist = flist
>dmo_dcl ((IntFunction a b c):xs) flist = dmo_dcl xs flist, if a = "createlistw"
>                                       = dmo_dcl xs (flist++[(IntFunction a b c)]), otherwise


dmo_fif (find internal functions), retuns the list of definations wihtin the where statemnet of a wrapper

>dmo_fif (Where a defs) = defs

dmo_cfl (create Function list), retuns the list of all fucntions being called by this wrapper that are from wrapper x

>dmo_cfl []                         iflist xnum = iflist
>dmo_cfl ((IntFunction a b exp):xs) iflist xnum = dmo_cfl xs (iflist++fcalls) xnum
>                                                 where
>                                                 fcalls  = dmo_fefc subloge [] xnum
>                                                 subloge = dmo_rsle sublog
>                                                 sublog  = dmo_rslf inexp
>                                                 inexp   = dmo_iwe exp

dmo_rsle (return sublogic expression), retuns the expression from the sublogic of a function

>dmo_rsle (IntFunction a b exp) = exp

dmo_rslf (retuns sublogic function), takes the definations list and removes the creatlist function just retuning the sublogic

>dmo_rslf ((IntFunction a b c):xs) = (IntFunction a b c), if a = "sublogic"
>                                  = dmo_rslf xs, otherwise

dmo_iwe (in where expression), retuns the expression in the where statement for a function

>dmo_iwe (Where a defs) = defs

dmo_fefc (find external functton calls), retuns a list of what functions the expression uses from wrapper x, this is in the form of a list of num, [num]

>dmo_fefc::expression->[num]->num->[num]
>dmo_fefc (Ifelse a b c)    flist xnum = nflist
>                                        where
>                                        nflist = flist1++flist2++flist3
>                                        flist1 = dmo_fefc a [] xnum
>                                        flist2 = dmo_fefc b [] xnum
>                                        flist3 = dmo_fefc c [] xnum
>dmo_fefc (Brackets a)      flist xnum = nflist
>                                        where
>                                        nflist = dmo_fefc a [] xnum
>dmo_fefc (List a)          flist xnum = nflist
>                                        where
>                                        nflist = dmo_fefinl a [] xnum
>dmo_fefc (Operation a b c) flist xnum = nflist
>                                        where
>                                        nflist = dmo_effec a, if dmo_itefc a b c xnum
>                                               = dmo_efnx c xnum, if dmo_nxefc a b c xnum
>                                               = (dmo_fefc a [] xnum)++(dmo_fefc c [] xnum), otherwise
>dmo_fefc (Funint a b)      flist xnum = nflist
>                                        where
>                                        nflist = dmo_fefina b [] xnum
>dmo_fefc (Varint a)        flist xnum = flist
>dmo_fefc (Varex a b)       flist xnum = nflist
>                                        where
>                                        nflist = dmo_fefina b [] xnum
>dmo_fefc (Constantvar a)   flist xnum = flist
>dmo_fefc (Specialfunc a e) flist xnum = nflist
>                                        where
>                                        nflist = dmo_fefc e [] xnum
>dmo_fefc (Number a)        flist xnum = flist
>dmo_fefc (Where e defs)    flist xnum = nflist
>                                        where
>                                        nflist = flist1 ++ flist2
>                                        flist1 = dmo_fefc e [] xnum
>                                        flist2 = dmo_fefid defs [] xnum


dmo_fefinl (find external fucnition in list), returns a list of the external functions from x being called in the list

>dmo_fefinl []     flist xnum = flist
>dmo_fefinl (x:xs) flist xnum = dmo_fefinl xs nflist xnum
>                               where
>                               nflist = flist++flist1
>                               flist1 = dmo_fefc x [] xnum


dmo_effec (etract function for external function call), returns a list containing the number of the function that this is being called from x

>dmo_effec (Operation harnbf bng (Number func)) = [func]


dmo_itefc (is this external function call), returns true if this is a Operation relating to an external function call to the wrapper xnum

>dmo_itefc (Operation (Operation n m v) y z) b c xnum = True, if check
>                                                     = False, otherwise
>                                                       where
>                                                       check = (b = Bang & y = Bang & m = Bang & nv = xnum & n = (Funext "program" "harness" []))
>                                                       nv = getnum v
>                                                            where
>                                                            getnum (Number a) = a
>dmo_itefc a                                 b c xnum = False
           
           
dmo_nxefc (not x external function call), returns true if the Operation is a call to an external function but not from wrapper x

>dmo_nxefc (Operation (Operation n m v) y z) b c xnum = True, if check
>                                                     = False, otherwise
>                                                       where
>                                                       check = (b = Bang & y = Bang & m = Bang & nv ~= xnum & n = (Funext "program" "harness" []))
>                                                       nv = getnum v
>                                                            where
>                                                            getnum (Number a) = a
>dmo_nxefc a                                 b c xnum = False
           

dmo_efnx (external function not x), retuns any function calls in a call to an extrenal funtion that is not x

>dmo_efnx c xnum = flist
>                  where
>                  flist = dmo_fefc c [] xnum
           
           
dmo_fefina (for external function in arguments), retuns a list of the external functions in the argument list

>dmo_fefina::[argument]->[num]->num->[num]
>dmo_fefina [] flist xnum = flist
>dmo_fefina ((Argument ex):xs) flist xnum = dmo_fefina xs newflist xnum
>                                           where
>                                           newflist = flist ++(dmo_fefc ex [] xnum)
           
           
dmo_fefid (find external functions in defintions), retuns a list of defintion calls in a defintions list
           
>dmo_fefid []                           flist xnum = flist
>dmo_fefid ((Name a ex):xs)             flist xnum = dmo_fefid xs nflist xnum
>                                                    where
>                                                    nflist = flist ++ (dmo_fefc ex [] xnum)
>dmo_fefid ((Function a b args ex):xs)  flist xnum = dmo_fefid xs nflist xnum
>                                                    where
>                                                    nflist = flist ++ flist1 ++ flist2
>                                                    flist1 = (dmo_fefc ex [] xnum)
>                                                    flist2 = (dmo_fefina args [] xnum)
>dmo_fefid ((InterVariable a ex):xs)    flist xnum = dmo_fefid xs nflist xnum
>                                                    where
>                                                    nflist = flist ++ (dmo_fefc ex [] xnum)
>dmo_fefid ((IntFunction a args ex):xs) flist xnum = dmo_fefid xs nflist xnum
>                                                    where
>                                                    nflist = flist ++ flist1 ++ flist2
>                                                    flist1 = (dmo_fefc ex [] xnum)
>                                                    flist2 = (dmo_fefina args [] xnum)

           
           
           
           
           
           
           



dmo_ndl (new defination list),returns a new list of definations
cl = call list [[[]]]
reutns list of definations editing the wrapper function

>dmo_ndl []                      ndefs cl pds = ndefs
>dmo_ndl ((Name a b):xs)         ndefs cl pds = dmo_ndl xs (ndefs++[(Name a b)]) cl pds
>dmo_ndl ((Function a b c d):xs) ndefs cl pds = dmo_ndl xs (ndefs++[(Function a b c d)]) cl pds, if a = "program" & b = "harness"
>                                             = dmo_ndl xs nndefs cl pds, otherwise ||wrappers
>                                               where
>                                               nndefs = ndefs++[ndef]
>                                               ndef   = (Function a b c nd)
>                                               nd     = dmo_cclop d a cl wraps
>                                               wraps  = dmo_rwf pds []

dmo_rwf (return wrap functions), retuns the wrap functions

>dmo_rwf []                      wrps = wrps
>dmo_rwf ((Function a b c d):xs) wrps = dmo_rwf xs (wrps++[Function a b c d]), if b = "wrapper"
>                                     = dmo_rwf xs wrps, otherwise
>dmo_rwf (x:xs)                  wrps = dmo_rwf xs wrps

dmo_cclop (change create list out put), changes the output list in _createlistw, so that this now outputs messages at each time step to the othere wrappers that need them

inputs: wrapper expression, wrapper agent, call list

>dmo_cclop (Where a defs) wa cl wps = Where a ndefs
>                                     where
>                                     ndefs = dmo_ecl defs [] na cl
>                                     na    = dmo_fan wps wa 0           ||number of agent


dmo_fan (find agent number), retuns the number in the order of agents relating to the agent being looked at
wps is the list of wrappers [defintion]
wa is the name of the wrapper Function a b c d [char]

>dmo_fan::[definition]->[char]->num->num
>dmo_fan wps wa b = b, if check
>                 = dmo_fan wps wa (b+1), otherwise
>                   where
>                   wpap  = wps!b
>                   cwa   = dmo_gwn wpap
>                   check = True, if cwa = wa
>                         = False, otherwise

dmo_gwn (get wrapper name)

>dmo_gwn (Function a b c d) = a


dmo_ecl (edit create list), edits the _createlistw to output the new messages

>dmo_ecl []                       ndefs na cl = ndefs
>dmo_ecl ((IntFunction a b c):xs) ndefs na cl = dmo_ecl xs nndefs na cl
>                                               where
>                                               nndefs = ndefs ++ [nx]
>                                               nx     = (IntFunction a b nc), if a = "createlistw"
>                                                      = (IntFunction a b c), otherwise
>                                               nc     = dmo_ccle c na cl


dmo_ccle (change create list expression)
na is the number of the agent in question
cl is the list saying which agents want what functions from other agents

>dmo_ccle::expression->num->[[[num]]]->expression
>dmo_ccle (Operation ls b c) na cl = Operation nls b c
>                                    where
>                                    nls = dmo_rnle na cl

dmo_rnle (return new list expression), returns what the new list out put at a time step t should be
This will be a list of tuples, each of the internal lists will be a message that is being passed.
(from, to, value)
to and from will be num, so the first wrapper is 0 and then 1 ..
the value will be the functions return at time step t
therefore a value will be something similar to f!t
therefore an output could be

[(0,1,_f1!t), (0,1,_f2!t), (0,2,_f2!t)]

[i_wrapper, j_wrapper, k_wrapper] ! (which wrapper you are in) eg i
[j_wrapper, k_wrapper] ! (both eventually, but which one you are sending to), eg j
[f1] this is the list of values that j wants from i


>dmo_rnle na cl = nopl
>                 where
>                 nopl = List mesl
>                 mesl = dmo_lom ema na [] 0
>                 ema  = dmo_ema agms na [] 0    ||adds a empty list for this agent to the list
>                 agms = cl!na

dmo_ema (empty message added), adds a empty list into the list of lists, in the place where this agent appears in the order, to make recurance easier in later functions


>dmo_ema []     na nl li = nl++[[]], if na = li
>                        = nl, otherwise
>dmo_ema (x:xs) na nl li = dmo_ema xs na (nl++[[]]++[x]) (li+1), if na = li
>                        = dmo_ema xs na (nl++[x]) (li+1), otherwise


dmo_lom (list of messages)
(from, to, value)

>dmo_lom::[[num]]->num->[expression]->num->[expression]
>dmo_lom []      na mesl san = mesl
>dmo_lom ([]:xs) na mesl san = dmo_lom xs na mesl (san+1)
>dmo_lom (x:xs)  na mesl san = dmo_lom xs na nmesl (san+1)
>                              where
>                              nmesl = [Tuple (Number na) (Number san) (value)]
>                              value = Operation func Bang (Varint "t")
>                              func  = Number 1    ||||||do this!!!!





||||||||||||||||||||||||||||||||||

[Tuple (Number 1) (Number 1) (Number 1)]


>ttt (Program a b) = cl ||Program new_a b
>                    where
>                    cl    = dmo_ccl a




>tttt = ttt convo







dmh (direct message harness), creates the output message for the harness

>dmh a = a

dmi (direct message input), changes the inputs on the wrappers

>dmi a = a













This section is used for testing purposes while creating the program

current version


>convo = (con_mp (con_harn (con_ag (con_itl (parser (lex simple_example))))))

>convn = con_dm (con_mp (con_harn (con_ag (con_itl (parser (lex simple_example))))))

new testing, tests the working version against the current version, should return true until functionality is changed then should retun false

>newt = convn = convo


prints the current version

>pt = print convn

>ppt = print convo









This is a print program that will print any code inside the program type
so far it will only print the defination list


>print (Program defs prog) = pnt defs

>pnt = printdefs "\n" ""

>printdefs pdefs space []     = pdefs
>printdefs pdefs space (x:xs) = printdefs (pdefs++space++(printdef x)++"\n") space xs




>printdef (Name n e)             = "c_"++n++" = "++(printexpr e)
>printdef (Function a n args e)  = a++"_"++n++" "++(printargs args "")++" = "++(printexpr e)
>printdef (InterVariable n e)    = n++" = "++(printexpr e)
>printdef (IntFunction n args e) = "_"++n++" "++(printargs args "")++" = "++(printexpr e)



>printargs []     pargs = pargs
>printargs (x:xs) pargs = printargs xs (pargs++(printarg x))

>printarg (Argument e) = printexpr e



>printexpr (Emptyexpression)  = " "
>printexpr (Ifelse a b c)     = "if "++(printexpr a)++" do "++(printexpr b)++" else "++(printexpr c)
>printexpr (Brackets a)       = "("++(printexpr a)++")"
>printexpr (List a)           = "["++(printelist a "")++"]"
>printexpr (Operation a b c)  = (printexpr a)++" "++(printop b)++" "++(printexpr c)
>printexpr (Funint n args)    = "_"++n++" "++(printargs args "")
>printexpr (Funext a n args)  = a++"_"++n++(printargs args "")
>printexpr (Varint a)         = a
>printexpr (Varex n args)     = "Var_"++n++" "++(printargs args "")  ||dont think i need this
>printexpr (Constantvar n)    = "c_"++n
>printexpr (Specialfunc sf e) = (printspecfunc sf)++" "++(printexpr e)
>printexpr (Number n)         = (printnumber n)
>printexpr (Where e dl)       = (printexpr e)++"\n    where\n"++(printdefs "" "    " dl)
>printexpr (Mainfunc args)    = "this should not be printed yet (Mainfunc)"
>printexpr (Tuple a b c)      = "("++(printexpr a)++", "++(printexpr b)++", "++(printexpr c)++")"


>printelist []     list = list
>printelist (x:xs) list = printelist xs (list++(printexpr x)++", ")        ||need to change this so it doesnt add , to the end of the list


>printop Plus        = "+"
>printop Minus       = "-"
>printop Multiply    = "*"
>printop Divide      = "/"
>printop Lessthan    = "<"
>printop Greaterthan = ">"
>printop Equals      = "="
>printop Notequals   = "!="
>printop Lessequ     = "<="
>printop Greaterequ  = ">="
>printop Listadd     = ":"
>printop Bang        = "!"


>printspecfunc Listhead = "hd "
>printspecfunc Listtail = "tl "

>printnumber::num->[char]
>printnumber x = (show x)




