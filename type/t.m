test="if"

lexeme::= Bra [char] [char]

||lex ('h':'e':'l':'p':xs) = Bra xs

lex x         = (Bra x), if x=['i','f']


isnumber x = (removeall "0123456789." (mkset x)) = []

removeall xs []     = []
removeall xs (y:ys) = removeall xs ys, if member xs y
                    = y:(removeall xs ys), otherwise



t = "c_yay"

istype x = beforescore x [] = ['c']

beforescore []       a = a
beforescore ('_':xs) a = a
beforescore (x:xs)   a = beforescore xs (a++[x])

returnfunc []       a = Bra a []
returnfunc ('_':xs) a = Bra a xs
returnfunc (x:xs)   a = returnfunc xs (a++[x])


isfunc [] = False
isfunc ('_':xs) = True
isfunc (x:xs) = isfunc xs
