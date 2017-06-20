

>program ::= Program [definition] experiment




>definition ::= Name [char] expression | Function [char] [char] [argument] expression | InterVariable [char] expression |  IntFunction [char] [argument] expression



>argument::= Argument expression




>experiment::= Experiment [globalvariables] experimentbody experimentrun



>globalvariables::= Globalvariables [char] [argument] expression



>experimentbody::= Emptybody|Expbody [argument] expression



>experimentrun::= Emptyrun|Exprun expression




>expression::= Emptyexpression ||primarly used to intiate loops and should not ever exist in the final out put
>              |Ifelse expression expression expression ||this is the if statement taking: if condition, true code, else code
>              |Brackets expression
>              |List [expression] ||for []
>              |Operation expression op expression
>              |Funint [char] [argument] ||internal function
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

