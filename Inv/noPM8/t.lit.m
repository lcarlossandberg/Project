> ||This is literate script

get rid of pattern matching only use conditions





t0_psi []               = 0
t0_psi ([a, b, c, d]:r) = b+(t0_psi r), if d=0
                        = (t0_psi r), otherwise





>test x = 1+y, if x=1
>       = 4+y, if x =2
>         where
>         y = 5+z
>         z = 2








