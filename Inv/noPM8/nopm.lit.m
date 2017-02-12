> ||This is literate script

get rid of pattern matching only use conditions




>c_Buy = 0
>c_Sell = 1
>c_Bid = 2
>c_Ask = 3







>t0_sell t = t0_order c_Sell 1000 0 0, if t<var_selltime
>          = t0_order c_Sell 0 0 0, otherwise

>t0_order a b c d = [a, b, c, d]

>t0_bid t = t0_order c_Bid 0 0 0

>t0_ask t = t0_order c_Ask 0 0 0

>t0_buy t = t0_order c_Buy 0 0 0













>t1_inv t = var_t1startinv, if t=0
>         = (t1_inv (t-1)) + (t1_psi (t1_xbids(t-1))) + (t1_psi (t1_xbuys(t-1))) - (t1_psi (t1_xasks(t-1))) - (t1_psi (t1_xsells(t-1))), otherwise

>t1_psi x = 0, if x=[]
>         = (t1_snd (hd x))+(t1_psi (tl x)), if (t1_frh (hd x))=1
>         = (t1_psi (tl x)), otherwise

>t1_xbids  t = t1_snd (e1_exchoutput1 t)

>t1_xbuys  t = t1_thd (e1_exchoutput2 t)

>t1_xasks  t = t1_snd (e1_exchoutput2 t)

>t1_xsells t = t1_thd (e1_exchoutput1 t)

>t1_snd x = hd (tl x)

>t1_thd x = hd (tl (tl x))

>t1_frh x = hd (tl (tl (tl x)))

>t1_sell t = t1_order c_Sell  0 0 1, if t=0
>          = t1_order c_Sell  (t1_sellsize (t1_inv t)) var_nu 1, otherwise

>t1_order a b c d = [a, b, c, d]

>t1_sellsize x = 0, if x<var_ul
>              = var_ul, otherwise


>t1_bid t = t1_order c_Bid (t1_bidsize (t1_inv 0)) (t1_bidprice (var_startprice-1) (var_startprice+1) (t1_inv 0)) 1, if t=0
>         = t1_order c_Bid (t1_bidsize (t1_inv t)) (t1_bidprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t1_inv t)) 1, otherwise

>t1_bidsize x = 0, if x>=var_ul
>             = t1_bidsize_(x), otherwise

>t1_bidsize_ x = t1_max 0 (var_ul-1-x)

>t1_bidprice bestbid bestask inv = t1_max 0 ((midprice-1)-alpha)
>                                  where
>                                  midprice = ((bestbid+bestask)/2)
>                                  alpha    = zeta*(1-((var_ul-1-inv)/(var_ul-var_ll-2)))
>                                  zeta     = 6

>t1_max x y = x, if x>y
>           = y, otherwise


>t1_ask t = t1_order c_Ask (t1_asksize (t1_inv t)) (var_startprice+1) 1, if t<2
>         = t1_order c_Ask (t1_asksize (t1_inv t)) (t1_askprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t1_inv t)) 1, otherwise

>t1_asksize x = 0, if x<=var_ll
>             = t1_asksize_(x), otherwise


>t1_asksize_ x = max[0, (x-(var_ll+1))]

>t1_askprice bestbid bestask inv = t1_max 0 ((midprice+1)+alpha)
>                                  where
>                                  midprice = ((bestbid+bestask)/2)
>                                  alpha = zeta*((var_ul-1-inv)/(var_ul-var_ll-2))
>                                  zeta = 6

>t1_buy t = t1_order c_Buy 0 0 1, if t=0
>         = t1_order c_Buy (t1_buysize (t1_inv t)) var_nu 1, otherwise

>t1_buysize x = 0, if x>var_ll
>             = -var_ll, otherwise











>t2_inv t = var_t2startinv, if t=0
>         = (t2_inv (t-1)) + (t2_psi (t2_xbids(t-1))) + (t2_psi (t2_xbuys(t-1))) - (t2_psi (t2_xasks(t-1))) - (t2_psi (t2_xsells(t-1))), otherwise

>t2_psi x = 0, if x=[]
>         = (t2_snd (hd x))+(t2_psi (tl x)), if (t2_frh (hd x))=2
>         = (t2_psi (tl x)), otherwise

>t2_xbids  t = t2_snd (e1_exchoutput1 t)

>t2_xbuys  t = t2_thd (e1_exchoutput2 t)

>t2_xasks  t = t2_snd (e1_exchoutput2 t)

>t2_xsells t = t2_thd (e1_exchoutput1 t)

>t2_snd x = hd (tl x)

>t2_thd x = hd (tl (tl x))

>t2_frh x = hd (tl (tl (tl x)))

>t2_sell t = t2_order c_Sell  0 0 2, if t=0
>          = t2_order c_Sell  (t2_sellsize (t2_inv t)) var_nu 2, otherwise

>t2_order a b c d = [a, b, c, d]

>t2_sellsize x = 0, if x<var_ul
>              = var_ul, otherwise


>t2_bid t = t2_order c_Bid (t2_bidsize (t2_inv 0)) (t2_bidprice (var_startprice-1) (var_startprice+1) (t2_inv 0)) 2, if t=0
>         = t2_order c_Bid (t2_bidsize (t2_inv t)) (t2_bidprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t2_inv t)) 2, otherwise

>t2_bidsize x = 0, if x>=var_ul
>             = t2_bidsize_(x), otherwise

>t2_bidsize_ x = t2_max 0 (var_ul-1-x)

>t2_bidprice bestbid bestask inv = t2_max 0 ((midprice-1)-alpha)
>                                  where
>                                  midprice = ((bestbid+bestask)/2)
>                                  alpha    = zeta*(1-((var_ul-1-inv)/(var_ul-var_ll-2)))
>                                  zeta     = 6

>t2_max x y = x, if x>y
>           = y, otherwise


>t2_ask t = t2_order c_Ask (t2_asksize (t2_inv t)) (var_startprice+1) 2, if t<2
>         = t2_order c_Ask (t2_asksize (t2_inv t)) (t2_askprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t2_inv t)) 2, otherwise

>t2_asksize x = 0, if x<=var_ll
>             = t2_asksize_(x), otherwise


>t2_asksize_ x = max[0, (x-(var_ll+1))]

>t2_askprice bestbid bestask inv = t2_max 0 ((midprice+1)+alpha)
>                                  where
>                                  midprice = ((bestbid+bestask)/2)
>                                  alpha = zeta*((var_ul-1-inv)/(var_ul-var_ll-2))
>                                  zeta = 6

>t2_buy t = t2_order c_Buy 0 0 2, if t=0
>         = t2_order c_Buy (t2_buysize (t2_inv t)) var_nu 2, otherwise

>t2_buysize x = 0, if x>var_ll
>             = -var_ll, otherwise


















>e1_exchoutput1 t = e1_match (e1_bidbook_ t) (e1_sells t)


>e1_match x y = [[],[],[]], if x=[]
>             = [x, [],[]], if y=[]
>             = [i1, ([a,f,c,d]:j1), ([e,f,g,h]:k1)], if f < b
>             = [i2, ([a,f,c,d]:j2), ([e,f,g,h]:k2)], if f > b
>             = [i3, ([a,f,c,d]:j3), ([e,f,g,h]:k3)], if f = b
>               where
>               a = hd (hd x)
>               b = e1_snd (hd x)
>               c = e1_thd (hd x)
>               d = e1_frh (hd x)
>               q = tl x
>               e = hd (hd y)
>               f = e1_snd (hd y)
>               g = e1_thd (hd y)
>               h = e1_frh (hd y)
>               z = tl y
>               ns1 = e1_match q ([e,(b-f),g,h]:z)
>               ns2 = e1_match q ([e,(f-b),g,h]:z)
>               ns3 = e1_match q z
>               i1 = hd ns1
>               j1 = e1_snd ns1
>               k1 = e1_thd ns1
>               i2 = hd ns2
>               j2 = e1_snd ns2
>               k2 = e1_thd ns2
>               i3 = hd ns3
>               j3 = e1_snd ns3
>               k3 = e1_thd ns3



>e1_bidbook_ t = e1_insertbid [] (e1_bids t)


>e1_insertbid x y = x, if y=[]
>                 = e1_insertbid [hd y] (tl y), if x=[]
>                 = e1_insertbid ([t,s,p,i]:l) a, if p>f
>                 = [d,e,f,g]:(e1_insertbid q z), otherwise
>                   where
>                   d = hd (hd x)
>                   e = e1_snd (hd x)
>                   f = e1_thd (hd x)
>                   g = e1_frh (hd x)
>                   q = tl x
>                   t = hd (hd y)
>                   s = e1_snd (hd y)
>                   p = e1_thd (hd y)
>                   i = e1_frh (hd y)
>                   a = tl y
>                   l = ([d,e,f,g]:q)
>                   z = ([t,s,p,i]:a)


>e1_bids t = [(t0_bid t), (t1_bid t), (t2_bid t)]

>e1_sells t = [(t0_sell t), (t1_sell t), (t2_sell t)]

>e1_exchoutput2 t = e1_match (e1_askbook_ t) (e1_buys  t)

>e1_askbook_ t = e1_insertask [] (e1_asks t)


>e1_insertask x y = x, if y=[]
>                 = e1_insertask [hd y] (tl y), if x=[]
>                 = e1_insertask ([t,s,p,i]:l) a, if p<f
>                 = [d,e,f,g]:(e1_insertask q z), otherwise
>                   where
>                   d = hd (hd x)
>                   e = e1_snd (hd x)
>                   f = e1_thd (hd x)
>                   g = e1_frh (hd x)
>                   q = tl x
>                   t = hd (hd y)
>                   s = e1_snd (hd y)
>                   p = e1_thd (hd y)
>                   i = e1_frh (hd y)
>                   a = tl y
>                   l = ([d,e,f,g]:q)
>                   z = ([t,s,p,i]:a)


>e1_asks t = [(t0_ask t), (t1_ask t), (t2_ask t)]

>e1_buys t = [(t0_buy t), (t1_buy t), (t2_buy t)]

>e1_bestbid t = e1_fstls (e1_bidbook t), if (e1_bidbook t) ~= []
>             = (var_startprice-1), otherwise

>e1_bidbook t = hd (e1_exchoutput1 t)

>e1_bestask t = e1_fstls (e1_askbook t), if (e1_askbook t) ~= []
>             = (var_startprice+1), otherwise

>e1_askbook t = hd (e1_exchoutput2 t)

>e1_snd x = hd (tl x)

>e1_thd x = hd (tl (tl x))

>e1_frh x = hd (tl (tl (tl x)))

>e1_fstls x = e1_thd (hd x)





>var_selltime = 10
>var_t1startinv = 2000
>var_t2startinv = 2000
>var_nu = 0
>var_ul = 3000
>var_ll = (-3000)
>var_startprice = 2000
















