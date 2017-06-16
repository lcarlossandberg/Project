run_main = [main 0, main 1]



var_init runnumber = myif (runnumber=0) then (10) else (20)




main runnumber =

[i_f1 1, i_f1 2, i_f1 3]

where
{
    
    
    
i_f1 t = (i_f2 t) + (j_f1 t 1)
    
i_f2 t = myif (t<12) then (10) else (20)
    
    
    
    
j_f1 t a = myif (a<2) then (9) else (j_f2 t)
    
j_f2 t = fun t 3
         where{
         fun t a = (k_f1 a)+t}
    
    
    
    
k_f1 t = t + (var_init runnumber)
    
    
}
