run_main = [main 0, main 1]



var_selltime e = 10
var_t1startinv e = 2000
var_t2startinv e = 2000
var_nu e = 0
var_ul e = 3000
var_ll e = (-3000)
var_startprice e = 2000





main runnumber =

myif (runnumber = 1) then ([t1_inv 0, t1_inv 1, t1_inv 2])
else
([t2_inv 0, t2_inv 1, t2_inv 3])

where
{
c_Buy = 0
c_Sell = 1
c_Bid = 2
c_Ask = 3




t0_sell t = myif (t < (var_selltime runnumber) ) then (t0_order c_Sell 1000 0 0)
            else
            (t0_order c_Sell 0 0 0)

t0_order a b c d = [a, b, c, d]

t0_bid t = t0_order c_Bid 0 0 0

t0_ask t = t0_order c_Ask 0 0 0

t0_buy t = t0_order c_Buy 0 0 0






t1_inv t = myif (t = 0) then ((var_t1startinv runnumber) )
           else
           ((t1_inv (t-1)) + (t1_psi (t1_xbids(t-1))) + (t1_psi (t1_xbuys(t-1))) - (t1_psi (t1_xasks(t-1))) - (t1_psi (t1_xsells(t-1))))

t1_psi x = myif (x = []) then (0)
           else
           (myif ((t1_frh (hd x)) = 1) then ((t1_snd (hd x))+(t1_psi (tl x)))
           else
           (t1_psi (tl x)))

t1_xbids  t = t1_snd (e1_exchoutput1 t)

t1_xbuys  t = t1_thd (e1_exchoutput2 t)

t1_xasks  t = t1_snd (e1_exchoutput2 t)

t1_xsells t = t1_thd (e1_exchoutput1 t)

t1_snd x = hd (tl x)

t1_thd x = hd (tl (tl x))

t1_frh x = hd (tl (tl (tl x)))

t1_sell t = myif (t = 0) then (t1_order c_Sell  0 0 1)
            else
            (t1_order c_Sell  (t1_sellsize (t1_inv t)) (var_nu runnumber)  1)

t1_order a b c d = [a, b, c, d]

t1_sellsize x = myif (x < (var_ul runnumber)) then (0)
                else
                ((var_ul runnumber))

t1_bid t = myif (t = 0) then (t1_order c_Bid (t1_bidsize (t1_inv 0)) (t1_bidprice ((var_startprice runnumber)-1) ((var_startprice runnumber)+1) (t1_inv 0)) 1)
           else
           (t1_order c_Bid (t1_bidsize (t1_inv t)) (t1_bidprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t1_inv t)) 1)

t1_bidsize x = myif (x = (var_ul runnumber)) then (0)
               else
               (t1_bidsize_(x))

t1_bidsize_ x = t1_max 0 ((var_ul runnumber)-1-x)

t1_bidprice bestbid bestask inv = t1_max 0 ((midprice-1)-alpha)
                                  where
                                  {
                                  midprice = ((bestbid+bestask)/2)
                                  alpha    = zeta*(1-(((var_ul runnumber)-1-inv)/((var_ul runnumber)-(var_ll runnumber)-2)))
                                  zeta     = 6}

t1_max x y = myif (x > y) then (x)
             else
             (y)

t1_ask t = myif (t < 2) then (t1_order c_Ask (t1_asksize (t1_inv t)) ((var_startprice runnumber)+1) 1)
           else
           (t1_order c_Ask (t1_asksize (t1_inv t)) (t1_askprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t1_inv t)) 1)

t1_asksize x = myif (x <= (var_ll runnumber)) then (0)
               else
               (t1_asksize_(x))

t1_asksize_ x = t1_max 0 (x-((var_ll runnumber)+1))

t1_askprice bestbid bestask inv = t1_max 0 ((midprice+1)+alpha)
                                  where
                                  {
                                  midprice = ((bestbid+bestask)/2)
                                  alpha = zeta*(((var_ul runnumber)-1-inv)/((var_ul runnumber)-(var_ll runnumber)-2))
                                  zeta = 6}

t1_buy t = myif (t = 0) then (t1_order c_Buy 0 0 1)
           else
           (t1_order c_Buy (t1_buysize (t1_inv t)) (var_nu runnumber)  1)

t1_buysize x = myif (x > (var_ll runnumber)) then (0)
               else
               (-(var_ll runnumber))





t2_inv t = myif (t = 0) then ((var_t2startinv runnumber) )
           else
           ((t2_inv (t-1)) + (t2_psi (t2_xbids(t-1))) + (t2_psi (t2_xbuys(t-1))) - (t2_psi (t2_xasks(t-1))) - (t2_psi (t2_xsells(t-1))))

t2_psi x = myif (x = []) then (0)
           else
           (myif ((t2_frh (hd x)) = 2) then ((t2_snd (hd x))+(t2_psi (tl x)))
           else
           (t2_psi (tl x)))

t2_xbids  t = t2_snd (e1_exchoutput1 t)

t2_xbuys  t = t2_thd (e1_exchoutput2 t)

t2_xasks  t = t2_snd (e1_exchoutput2 t)

t2_xsells t = t2_thd (e1_exchoutput1 t)

t2_snd x = hd (tl x)

t2_thd x = hd (tl (tl x))

t2_frh x = hd (tl (tl (tl x)))

t2_sell t = myif (t = 0) then (t2_order c_Sell  0 0 2)
            else
            (t2_order c_Sell  (t2_sellsize (t2_inv t)) (var_nu runnumber)  2)

t2_order a b c d = [a, b, c, d]

t2_sellsize x = myif (x < (var_ul runnumber)) then (0)
                else
                ((var_ul runnumber))

t2_bid t = myif (t = 0) then (t2_order c_Bid (t2_bidsize (t2_inv 0)) (t2_bidprice ((var_startprice runnumber)-1) ((var_startprice runnumber)+1) (t2_inv 0)) 2)
           else
           (t2_order c_Bid (t2_bidsize (t2_inv t)) (t2_bidprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t2_inv t)) 2)

t2_bidsize x = myif (x = (var_ul runnumber)) then (0)
               else
               (t2_bidsize_(x))

t2_bidsize_ x = t2_max 0 ((var_ul runnumber)-1-x)

t2_bidprice bestbid bestask inv = t2_max 0 ((midprice-1)-alpha)
                                  where
                                  {midprice = ((bestbid+bestask)/2)
                                  alpha    = zeta*(1-(((var_ul runnumber)-1-inv)/((var_ul runnumber)-(var_ll runnumber)-2)))
                                  zeta     = 6}

t2_max x y = myif (x > y) then (x)
             else
             (y)

t2_ask t = myif (t < 2) then (t2_order c_Ask (t2_asksize (t2_inv t)) ((var_startprice runnumber)+1) 2)
           else
           (t2_order c_Ask (t2_asksize (t2_inv t)) (t2_askprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t2_inv t)) 2)

t2_asksize x = myif (x <= (var_ll runnumber)) then (0)
               else
               (t2_asksize_(x))

t2_asksize_ x = t2_max 0 (x-((var_ll runnumber)+1))

t2_askprice bestbid bestask inv = t2_max 0 ((midprice+1)+alpha)
                                  where
                                  {midprice = ((bestbid+bestask)/2)
                                  alpha = zeta*(((var_ul runnumber)-1-inv)/((var_ul runnumber)-(var_ll runnumber)-2))
                                  zeta = 6}

t2_buy t = myif (t = 0) then (t2_order c_Buy 0 0 2)
           else
           (t2_order c_Buy (t2_buysize (t2_inv t)) (var_nu runnumber)  2)

t2_buysize x = myif (x > (var_ll runnumber)) then (0)
               else
               (-(var_ll runnumber))






e1_exchoutput1 t = e1_match (e1_bidbook_ t) (e1_sells t)

e1_match x y = myif (x = []) then ([[],[],[]])
               else
               (myif (y=[]) then ([x, [],[]])
               else
               (myif (f < b) then ([i1, ([a,f,c,d]:j1), ([e,f,g,h]:k1)])
               else
               (myif (f > b) then ([i2, ([a,f,c,d]:j2), ([e,f,g,h]:k2)])
               else
               ([i3, ([a,f,c,d]:j3), ([e,f,g,h]:k3)]))))
               where
               {
               a = hd (hd x)
               b = e1_snd (hd x)
               c = e1_thd (hd x)
               d = e1_frh (hd x)
               q = tl x
               e = hd (hd y)
               f = e1_snd (hd y)
               g = e1_thd (hd y)
               h = e1_frh (hd y)
               z = tl y
               ns1 = e1_match q ([e,(b-f),g,h]:z)
               ns2 = e1_match q ([e,(f-b),g,h]:z)
               ns3 = e1_match q z
               i1 = hd ns1
               j1 = e1_snd ns1
               k1 = e1_thd ns1
               i2 = hd ns2
               j2 = e1_snd ns2
               k2 = e1_thd ns2
               i3 = hd ns3
               j3 = e1_snd ns3
               k3 = e1_thd ns3}

e1_bidbook_ t = e1_insertbid [] (e1_bids t)

e1_insertbid x y = myif (y = []) then (x)
                   else
                   (myif (x = []) then (e1_insertbid [hd y] (tl y))
                   else
                   (myif (p > f) then (e1_insertbid ([t,s,p,i]:l) a)
                   else
                   ([d,e,f,g]:(e1_insertbid q z))))
                   where
                   {
                   d = hd (hd x)
                   e = e1_snd (hd x)
                   f = e1_thd (hd x)
                   g = e1_frh (hd x)
                   q = tl x
                   t = hd (hd y)
                   s = e1_snd (hd y)
                   p = e1_thd (hd y)
                   i = e1_frh (hd y)
                   a = tl y
                   l = ([d,e,f,g]:q)
                   z = ([t,s,p,i]:a)}

e1_bids t = [(t0_bid t), (t1_bid t), (t2_bid t)]

e1_sells t = [(t0_sell t), (t1_sell t), (t2_sell t)]

e1_exchoutput2 t = e1_match (e1_askbook_ t) (e1_buys  t)

e1_askbook_ t = e1_insertask [] (e1_asks t)

e1_insertask x y = myif (y = []) then (x)
                   else
                   (myif (x = []) then (e1_insertask [hd y] (tl y))
                   else
                   (myif (p<f) then (e1_insertask ([t,s,p,i]:l) a)
                   else
                   ([d,e,f,g]:(e1_insertask q z))))
                   where
                   {
                   d = hd (hd x)
                   e = e1_snd (hd x)
                   f = e1_thd (hd x)
                   g = e1_frh (hd x)
                   q = tl x
                   t = hd (hd y)
                   s = e1_snd (hd y)
                   p = e1_thd (hd y)
                   i = e1_frh (hd y)
                   a = tl y
                   l = ([d,e,f,g]:q)
                   z = ([t,s,p,i]:a)}

e1_asks t = [(t0_ask t), (t1_ask t), (t2_ask t)]

e1_buys t = [(t0_buy t), (t1_buy t), (t2_buy t)]

e1_bestbid t = myif ((e1_bidbook t) ~= []) then (e1_fstls (e1_bidbook t))
               else
               ((var_startprice runnumber)-1)

e1_bidbook t = hd (e1_exchoutput1 t)

e1_bestask t = myif ((e1_askbook t) ~= []) then (e1_fstls (e1_askbook t))
               else
               ((var_startprice runnumber)+1)

e1_askbook t = hd (e1_exchoutput2 t)

e1_snd x = hd (tl x)

e1_thd x = hd (tl (tl x))

e1_frh x = hd (tl (tl (tl x)))

e1_fstls x = e1_thd (hd x)
}
