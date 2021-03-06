||This is a convert
||It will convert a text file into a custom miranda type which will hold the meaning of the data
||
||Testing Values
||An input would have a form similar to 
||{
||_name1 arg1 arg2 arg3 = (expression) + (expression),
||_name2 arg1           = expression,
||etc
||}
||expression

|| Chris comment - in the above example each definition is ended by a comma AND a newline - do we need both?
||               - maybe, if we have definitions spreading across two or more lines.

testvalue = "{_name1 x y = (1) + (a), _name2 q = j,} (name1)+(name2)"
testvalue2 = "{_name1 x y = (12) + (a), _name2 q = j,} (name1)+(name2)"
testv = "{_name x y = z,} (x)+(z)5"



||This is the lexer 
||This reads in the text file and converts it to tokens


lexeme::= Bra|Ket|Fun|Dot|Var [char]|Numb num|Plus|Minus|Divide|Mult|Letrec|In|Def [char]|Equ|Lend

lex::[char]->[lexeme]
lex []             = []
lex ('{':xs)       = Letrec:(lex xs) 
lex ('}':xs)       = In:(lex xs)  
lex ('(':xs)       = Bra:(lex xs)
lex (')':xs)       = Ket:(lex xs)
lex ('_':xs)       = (Def a):(lex b) ||an underscore is used to represent the beginning of a function
                     where (a,b)=f xs [] ||the underscore is then lost and the associated name is saved
                     f []       a = (a,[])
                     f (' ':xs) a = (a,xs)
                     f (x:xs)   a = f xs (a++[x])
lex ('=':xs)       = Equ:(lex xs)
lex (',':xs)       = Lend:(lex xs) ||this signifies the end of a function    
lex ('L':(' ':xs)) = Fun:(lex xs)
lex ('.':xs)       = Dot:(lex xs)
lex ('+':xs)       = Plus:(lex xs)
lex ('-':xs)       = Minus:(lex xs) 
lex ('*':xs)       = Mult:(lex xs)
lex ('/':xs)       = Divide:(lex xs)
||lex ('1':xs)       = Numb 1:(lex xs)   ||How can i generalise this to all numbers?
lex (' ':xs)       = lex xs
lex (x:xs)         = (Numb (numval a)): (lex b), if (isnumber a)
                   = (Var a):(lex b), otherwise
                     where 
                     (a,b)=f (x:xs) []
                     f []       a = (a,[])
                     f (' ':xs) a = (a,xs)
                     f (')':xs) a = (a,(')':xs))
                     f (',':xs) a = (a,(',':xs))   || Chris comment - don't forget this case, now we have commas
                     f (x:xs)   a = f xs (a++[x])
         isnumber x = (removeall "0123456789." (mkset x)) = []
         removeall xs []     = []
         removeall xs (y:ys) = removeall xs ys, if member xs y
                             = y:(removeall xs ys), otherwise


||This is the parse tree

prog   ::= Prog defs expr ||as LETREC defs IN exp, holds the over all structure  

defs   ::= Emptydefs|Defse fundef|Defs fundef defs ||Holds the function structure, Defse is needed as an end case to stop Emptydefs being used  
|| Chris comment - you may still need Emptydefs if you really don't have any definitions at all
|| Chris comment - Defse isn't strictly necessary - it's a design option.  A single def could have the value (Defs fundef Emptdefs)

fundef ::= Fundef [char] args expr ||Holds the actual function 
                                   || Chris comment - presumably [char] gives the name of the function?
                                   || Chris comment - presumably the Equ lexeme is no longer needed?
args   ::= Emptyargs|Argse arg|Args arg args ||Argse is used as an end case
arg    ::= Arg [char]

|| Chris comment - presumably the lexeme (Var x) must be changed into (Arg x) if it appears before an Equ ??

    
    
    
expr   ::= Variable [char]
           |Opexpr expr op expr
           |App expr expr
           |Brackets expr
           |Numcon num

|| Chris comment - it won't be long before you will need non-numeric constants (eg booleans) and associated operators

op     ::= Pluss|Mults|Minuss|Divides




||This is the parser

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
