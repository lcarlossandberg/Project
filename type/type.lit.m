> ||This is literate script

This program will take an input file of a miranda type that fits the limitations placed on it by us.
An example file would be input.m

This files contents are then saved in the type format to be proccessed into Interdyne






Here are the first couple of lines of the sugested input file, just for quick testing of the code

>tester = "c_Buy = 0 c_Sell = 1 c_Bid = 2 c_Ask = 3 t0_sell t = t0_order c_Sell 1000 0 0, if t<var_selltime = t0_order c_Sell 0 0 0, otherwise t0_order a b c d = [a, b, c, d] t0_bid t = t0_order c_Bid 0 0 0 t0_ask t = t0_order c_Ask 0 0 0 t0_buy t = t0_order c_Buy 0 0 0"





Lexer
Reads in the file and converts it to tokens

The lexeme contains all the tokens that will be used

>lexeme::= Idtype [char]|Opequal|LBra|LKet|Bra|Ket|Opplus|Opminus|Opmult|Opdivide|Opgreater|Opless|Funhead|Funtail|Opcons|Concomma|Conif|Conotherwise|Conwhere|Idfunc [char] [char]|Idintvar [char]|Idvar [char]|Idnum num|Expr|Opnotequal|Oplessequ|Opgreaterequ

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
>                   = Expr:(lex b), if a=['E','x','p','r']
>                   = (Idtype a):(lex b), if (istype a)
>                   = (Idvar a):(lex b), if (isvar a)
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

>program::= Program types agents experiment

types contains all of the user type definations for the program
it can look something like (Types deftype Emptytypes) for a single type

>types::= Emptytypes|Types deftype types

deftype contains the name of the type and then an expression of what the type equals (normally a number)

>deftype::= Type [char] expression

agents contains the body of the file, with each function stored here
each agent contains its global name,such as t1, and all associated fucntions

>agents::= Emptyagents|Agents agent agents

>agent::= Agent [char] functions

>functions::= Emptyfunctions|Functions deffunction functions

deffunction contains the name of the function its inputs and its function body

>deffunction::= Function [char] arguments bodyfunction

>arguments::= Emptyarguments|Argument arg arguments

>arg::= Arg [char]

bodyfunction stores the expression for each line of a function

>bodyfunction::= Emptyline|Line expression bodyfunction

The experment its self will contain initial values to use as well an expression for the experment

>experiment::= Experiment intialvalues expression

>intialvalues::= Emptyvalue|Intialvalue defvalue intialvalues

>defvalue::= Defvalue [char] num

expression is the meat of the tree allowing the code to expressed
it expresses each single element in the line in a kinda list
where an element is a singler expressible statement for instance
something contained within brackets

>expression::= Emptyexpression|Expression element expression

>element::= Emptyelement
>           |Brackets expression
>           |List expression ||for []
>           |Operaction expression op expression
>           |Functioncall [char] callinputs ||any function
>           |Intvariable [char]
>           |Exvariable [char]
>           |Where
>           |Specialfunc specfunc callinputs
>           |Otherwise
>           |Number num
>           |Comma
>           |If

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


>callinputs::= Emptycall|Callinputs element callinputs

>specfunc::= Listhead|Listtail|Listadd















Parser
This turns the tokens into the parse tree type

>parser x = Program (p_types a) (p_agents b) (p_experiment c)
>           where
>           (a, b, c) = program_splitter x





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

