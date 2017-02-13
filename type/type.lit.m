> ||This is literate script

This program will take an input file of a miranda type that fits the limitations placed on it by us.
An example file would be input.m

This files contents are then saved in the type format to be proccessed into Interdyne






Here are the first couple of lines of the sugested input file, just for quick testing of the code

>tester = "c_Buy = 0 c_Sell = 1 c_Bid = 2 c_Ask = 3 t0_sell t = t0_order c_Sell 1000 0 0, if t<var_selltime = t0_order c_Sell 0 0 0, otherwise t0_order a b c d = [a, b, c, d] t0_bid t = t0_order c_Bid 0 0 0 t0_ask t = t0_order c_Ask 0 0 0 t0_buy t = t0_order c_Buy 0 0 0"





Lexer
Reads in the file and converts it to tokens

The lexeme contains all the tokens that will be used

>lexeme::= Idtype [char]|Opequal|LBra|LKet|Bra|Ket|Opplus|Opminus|Opmult|Opdivide|Opgreater|Opless|Funhead|Funtail|Opcons|Concomma|Conif|Conotherwise|Conwhere|Idfunc [char] [char]|Idintvar [char]|Idvar [char]|Idnum num|Expr|Opnotequal|Oplessequ|Opgreaterequ|Idexrun|Main|Stateif

>lex::[char]->[lexeme]
>lex []             = []
>lex ('>':'=':xs)   = Opgreaterequ:(lex xs)
>lex ('<':'=':xs)   = Oplessequ:(lex xs)
>lex ('>':xs)       = Opgreater:(lex xs)
>lex ('<':xs)       = Opless:(lex xs)
>lex ('~':'=':xs)   = Opnotequal:(lex xs)
>lex ('=':xs)       = Opequal:(lex xs)
>lex ('[':xs)       = LBra:(lex xs)
>lex (']':xs)       = LKet:(lex xs)
>lex ('(':xs)       = Bra:(lex xs)
>lex (')':xs)       = Ket:(lex xs)
>lex ('+':xs)       = Opplus:(lex xs)
>lex ('-':xs)       = Opminus:(lex xs)
>lex ('*':xs)       = Opmult:(lex xs)
>lex ('/':xs)       = Opdivide:(lex xs)
>lex (' ':xs)       = lex xs
>lex (':':xs)       = Opcons:(lex xs)
>lex (',':xs)       = Concomma:(lex xs)
>lex (x:xs)         = (Idnum (numval a)): (lex b), if (isnumber a)
>                   = Conif:(lex b), if a=['i','f']
>                   = Conotherwise:(lex b), if a=['o','t','h','e','r','w','i','s','e']
>                   = Funhead:(lex b), if a=['h','d']
>                   = Funtail:(lex b), if a=['t','l']
>                   = Conwhere:(lex b), if a=['w','h','e','r','e']
>                   = Expr:(lex b), if a=['m', 'a', 'i', 'n']
>                   = Stateif:(lex b), if a=['m','y','i','f']
>                   = (lex b), if a=['t','h','e','n']
>                   = (lex b), if a=['e','l','s','e']
>                   = (Idtype a):(lex b), if (istype a)
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
>                     f (x:xs)   a = f xs (a++[x])
>

>isnumber x = (removeall "0123456789." (mkset x)) = []

>removeall xs []     = []
>removeall xs (y:ys) = removeall xs ys, if member xs y
>                    = y:(removeall xs ys), otherwise

>istype x = beforescore x [] = ['c']

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

>program::= Program types functions experiment

types contains all of the user type definations for the program
it can look something like (Types deftype Emptytypes) for a single type

>types::= Emptytypes|Types deftype types

deftype contains the name of the type, the arguments and then an expression of what the type equals (normally a number)

>deftype::= Type [char] arguments expression

here the arguments are the arguments to a fucntion call

>arguments::= Emptyargs|Args arg arguments

>arg::= Arg expression

the fucntion body used to expression all the functions
functions lists all the fucntions
function has the name of the function its code and its where statemnts

>functions::= Emptyfunctions|Functions function functions

>function::= Function [char] arguments expression whereblock

>whereblock::= Emptywhere|Whereblock expression whereblock

the experiment contains the globalvariables which are Variables that
can be changed for differnt runs of the experiment. then it has
the experiment code, detailing the function main and the code
used for the experiment. Then it has the experiment call meaning the
final experiment numbers to run

>experiment::= Experiment globalvariables experimentbody experimentrun

>globalvariables::= Emptygvar|Globalvariables defgvar globalvariables

>defgvar::= Defgvar [char] arguments expression

>experimentbody::= Emptebody|Expbody expression

>experimentrun::= Empterun|Exprun expression


expression defines the occurance of actual code in a recurance format
this should be able to represent any function

>expression::= Emptyexpression
>              |Ifelse expression expression expression ||this is the if statement taking: if condition, true code, else code
>              |Brackets expression
>              |List arguments ||for []
>              |Operaction expression op expression
>              |Funint [char] arguments ||internally defined functions
>              |Funext [char] arguments ||externally defined functions
>              |Varint [char]
>              |Varex [char]
>              |Specialfunc specfunc expression
>              |Number num

>internarg::= Emptyinarg|Internarg expression internarg

>op::= Pluss
>      |Minus
>      |Multiply
>      |Divide
>      |Lessthan
>      |Greaterthan
>      |Equals
>      |Notequals
>      |Lessequ
>      |Greaterequ

>specfunc::= Listhead|Listtail|Listadd















Parser
This turns the tokens into the parse tree type

>parser x = Program (p_types a) (p_agents b) (p_experiment c)
>           where
>           a = find_types1 x
>           b = find_code1 x

||find_types isolates the code defining global types "var_" and retuns this

>find_types1 []             = []
>find_types1 ((Idvar a):xs) = find_types2 ((Idvar a):xs) []
>find_types1 (x:xs)         = find_types1 xs

>find_types2 []           b = b ||if var is put at end of file
>find_types2 (Expr:xs)    b = b ||if var is put before the main
>find_types2 (Idexrun:xs) b = b ||if var is put before run_main
>find_types2 (x:xs)       b = find_types xs (b++[x])


||find_code isolates the main body of the code and returns this

>find_code1 []        = []
>find_code1 (Expr:xs) = find_code2 xs
>find_code1 (x:xs)    = find_code1 xs

>find_code2 []            = []
>find_code2 (Conwhere:xs) = find_code3 xs
>find_code2 (x:xs)        = find_code2 xs

>find_code3 []                = []
>find_code3 ((Idfunc a b):xs) = find_code4 ((Idfunc a b):xs) []
>find_code3 (x:xs)            = find_code3 xs

>find_code4 []             b = b
>find_code4 (x:xs)         b = find_code4 xs (b++[x])







parser::[lexeme]->prog
parser (Letrec:xs) = Prog (p_defs a) (p_expr b)    ||splits the program into letrec and in
where (a, b)      = f_split xs []
f_split []      a = (a, [])
f_split (In:xs) a = (a, xs)
f_split (x:xs)  a = f_split xs (a++[x])
parser any         = error ("Bad input format (parser): "++(show any))

||this separates the list of tokens into a list of functions each containing a number of tokens
p_defs::[lexeme]->defs
p_defs []           = Emptydefs
p_defs (Def any:xs) = Defse (p_fundef a), if b=[] ||this is called when there is only a single function
= Defs (p_fundef a) (p_defs b), otherwise
where (a, b)        = f_split (Def any:xs) []
f_split []        a = (a, [])                     || Chris comment - don't forget this case!
f_split (Lend:xs) a = (a, xs)
f_split (x:xs)    a = f_split xs (a++[x])
p_defs any          = error ("Bad input format (p_defs): "++(show any))

||this takes a function and breaks it into its name, arguments and expressions
p_fundef::[lexeme]->fundef
p_fundef (Def any:xs) = Fundef any (p_args a) (p_expr b)
where (a, b)       = f_split xs []
f_split []       a = (a, [])
f_split (Equ:xs) a = (a, xs)
f_split (x:xs)   a = f_split xs (a++[x])
p_fundef any          = error ("Bad input format (p_fundef): "++(show any))


||puts the arguments into the parse tree
p_args::[lexeme]->args
p_args []           = Emptyargs
p_args [Var any]    = Argse (Arg any)
p_args (Var any:xs) = Args (Arg any) (p_args xs)
p_args any          = error ("Bad input format (p_args): "++(show any))


||puts the expressions into the parse tree
p_expr::[lexeme]->expr
p_expr [Var any]     = Variable any
p_expr (Bra:xs)      = f_bracket xs [] ||calls a special function to deal with the number of cases for brackets
p_expr (Numb any:xs) = Numcon any
p_expr any           = error ("Bad input format (p_expr): "++(show any))


||This finds what is inside a bracket
f_bracket::[lexeme]->[lexeme]->expr
f_bracket (Bra:xs) a = f_bracket c (a++d) ||deals with brackets inside brackets (5+10(60))->5+10(60)) returns with the ket
where (c, d)      = in_bra xs [Bra]
in_bra (Ket:ys) d = (ys, (d++[Ket]))
in_bra (Bra:ys) d = in_bra ys (d++[Bra])
in_bra (y:ys)   d = in_bra ys (d++[y])
f_bracket (Ket:xs) a = f_decider xs a ||calls another function to add the contents of the bracket to the parse tree
f_bracket (x:xs)   a = f_bracket xs (a++[x])
f_bracket any        = error ("Bad input format (f_bracket): "++(show any))


||Adds brackets to the parse tree
||looks at the first item following the ket of the bracket expression
f_decider::[lexeme]->[lexeme]->expr
f_decider (Bra:xs)    a = App (p_expr a) (p_expr xs) ||if its another bracket it must be a application
f_decider (Plus:xs)   a = Opexpr (p_expr a) Pluss (p_expr xs)
f_decider (Minus:xs)  a = Opexpr (p_expr a) Minuss (p_expr xs)
f_decider (Divide:xs) a = Opexpr (p_expr a) Divides (p_expr xs)
f_decider (Mult:xs)   a = Opexpr (p_expr a) Mults (p_expr xs)
f_decider any         a = Brackets (p_expr a) ||if anything else follows it has to just be a singler bracketed expression



















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

