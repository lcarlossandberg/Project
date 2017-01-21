> ||This is literate script

Here the comments were removed and the naming convention of
t1, t2 for trader one and two and e1 for exchange one were introduced.

changes:
sells:

sells::num->[(otype, num, num, num)]
sells t = [(sell t i) | i <- [0..maxi]]
          where
          maxi = 2

e1-sells::num->[(otype, num, num, num)]
e1_sells t = [(t1_sell t 1), (t2_sell t 2)]


bids::num->[(otype, num, num, num)]
bids t = [(bid t i) | i <- [0..maxi]]
         where
         maxi = 2

e1_bids::num->[(otype, num, num, num)]
e1_bids t = [(t1_bid t 1), (t1_bid t 2)]


asks::num->[(otype, num, num, num)]
asks t = [(ask t i) | i <- [0..maxi]]
         where
         maxi = 2

e1_asks::num->[(otype, num, num, num)]
e1_asks t = [(t1_ask t 1), (t2_ask t 2)]


buys::num->[(otype, num, num, num)]
buys t = [(buy t i) | i <- [0..maxi]]
         where
         maxi = 2

e1_buys::num->[(otype, num, num, num)]
e1_buys t = [(t1_buy t 1), (t2_buy t 2)]









>t1_inv::num->num->num
>t1_inv 0 0 = 0 ||some intial condtion, say 0 inventory at time = 0
>t1_inv 1 0 = 2000 ||initial inventory for trader 1
>t1_inv 2 0 = 2000 ||intial inventory for trader 2
>t1_inv x 0 = error "only two traders present"
>t1_inv i t = (t1_inv i (t-1)) + (t1_psi (t1_xbids(t-1)) i) + (t1_psi (t1_xbuys(t-1)) i) - (t1_psi (t1_xasks(t-1)) i) - (t1_psi (t1_xsells(t-1)) i)

>otype::=Buy|Sell|Bid|Ask

>t1_psi::[(otype, num, num, num)]->num->num
>t1_psi []               i = 0
>t1_psi ((a, b, c, d):r) i = b+(t1_psi r i), if d=i
>                          = (t1_psi r i), otherwise

>t1_xbids::num->[(otype,num,num,num)]
>t1_xbids   t = t1_snd3 (e1_exchoutput1 t)

>t1_xbuys::num->[(otype,num,num,num)]
>t1_xbuys   t = t1_thd3 (e1_exchoutput2 t)

>t1_xasks::num->[(otype,num,num,num)]
>t1_xasks   t = t1_snd3 (e1_exchoutput2 t)

>t1_xsells::num->[(otype,num,num,num)]
>t1_xsells  t = t1_thd3 (e1_exchoutput1 t)

>t1_snd3::([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])->[(otype,num,num,num)]
>t1_snd3 (a,b,c) = b

>t1_thd3::([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])->[(otype,num,num,num)]
>t1_thd3 (a,b,c) = c

>t1_sell::num->num->(otype, num, num, num)
>t1_sell t i = t1_order Sell (t1_sellsize (t1_inv i t)) nu i
>              where
>              nu = 0

>t1_order::otype->num->num->num->(otype, num, num, num)
>t1_order a b c d = (a, b, c, d)

>t1_sellsize::num->num
>t1_sellsize x = 0, if x<ul
>              = ul, otherwise
>                where
>                ul = 3000

>t1_bid::num->num->(otype, num, num, num)
>t1_bid t i = t1_order Bid (t1_bidsize (t1_inv i t)) (t1_bidprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t1_inv i t)) i

>t1_bidsize::num->num
>t1_bidsize x = 0, if x>=ul
>             = t1_bidsize_(x), otherwise
>               where
>               ul = 3000

>t1_bidsize_::num->num
>t1_bidsize_ x = max[0, (ul-1-x)]
>                where
>                ul = 3000

>t1_bidprice::num->num->num->num
>t1_bidprice bestbid bestask inv = max[0,((midprice-1)-alpha)]
>                                  where
>                                  midprice = ((bestbid+bestask)/2)
>                                  alpha    = zeta*(1-((ul-1-inv)/(ul-ll-2)))
>                                  zeta     = 6
>                                  ul       = 3000
>                                  ll       = (-3000)

>t1_ask::num->num->(otype, num, num, num)
>t1_ask t i = t1_order Ask (t1_asksize (t1_inv i t)) (t1_askprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t1_inv i t)) i

>t1_asksize::num->num
>t1_asksize x = 0, if x<=ll
>             = t1_asksize_(x), otherwise
>               where
>               ll = (-3000)


>t1_asksize_::num->num
>t1_asksize_ x = max[0, (x-(ll+1))]
>                where
>                ll = (-3000)

>t1_askprice::num->num->num->num
>t1_askprice bestbid bestask inv = max[0,((midprice+1)+alpha)]
>                                  where
>                                  midprice = ((bestbid+bestask)/2)
>                                  alpha = zeta*((ul-1-inv)/(ul-ll-2))
>                                  zeta = 6
>                                  ul = 3000
>                                  ll = (-3000)

>t1_buy::num->num->(otype, num, num, num)
>t1_buy t i = t1_order Buy (t1_buysize (t1_inv i t)) nu i
>             where
>             nu = 0

>t1_buysize::num->num
>t1_buysize x = 0, if x>ll
>             = -ll, otherwise
>               where
>               ll = (-3000)














>t2_inv::num->num->num
>t2_inv 0 0 = 0 ||some intial condtion, say 0 inventory at time = 0
>t2_inv 1 0 = 2000 ||initial inventory for trader 1
>t2_inv 2 0 = 2000 ||intial inventory for trader 2
>t2_inv x 0 = error "only two traders present"
>t2_inv i t = (t2_inv i (t-1)) + (t2_psi (t2_xbids(t-1)) i) + (t2_psi (t2_xbuys(t-1)) i) - (t2_psi (t2_xasks(t-1)) i) - (t2_psi (t2_xsells(t-1)) i)

otype::=Buy|Sell|Bid|Ask

>t2_psi::[(otype, num, num, num)]->num->num
>t2_psi []               i = 0
>t2_psi ((a, b, c, d):r) i = b+(t2_psi r i), if d=i
>                          = (t2_psi r i), otherwise

>t2_xbids::num->[(otype,num,num,num)]
>t2_xbids   t = t2_snd3 (e1_exchoutput2 t)

>t2_xbuys::num->[(otype,num,num,num)]
>t2_xbuys   t = t2_thd3 (e1_exchoutput2 t)

>t2_xasks::num->[(otype,num,num,num)]
>t2_xasks   t = t2_snd3 (e1_exchoutput2 t)

>t2_xsells::num->[(otype,num,num,num)]
>t2_xsells  t = t2_thd3 (e1_exchoutput2 t)

>t2_snd3::([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])->[(otype,num,num,num)]
>t2_snd3 (a,b,c) = b

>t2_thd3::([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])->[(otype,num,num,num)]
>t2_thd3 (a,b,c) = c

>t2_sell::num->num->(otype, num, num, num)
>t2_sell t i = t2_order Sell (t2_sellsize (t2_inv i t)) nu i
>              where
>              nu = 0

>t2_order::otype->num->num->num->(otype, num, num, num)
>t2_order a b c d = (a, b, c, d)

>t2_sellsize::num->num
>t2_sellsize x = 0, if x<ul
>              = ul, otherwise
>                where
>                ul = 3000

>t2_bid::num->num->(otype, num, num, num)
>t2_bid t i = t2_order Bid (t2_bidsize (t2_inv i t)) (t2_bidprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t2_inv i t)) i

>t2_bidsize::num->num
>t2_bidsize x = 0, if x>=ul
>             = t2_bidsize_(x), otherwise
>               where
>               ul = 3000

>t2_bidsize_::num->num
>t2_bidsize_ x = max[0, (ul-1-x)]
>                where
>                ul = 3000

>t2_bidprice::num->num->num->num
>t2_bidprice bestbid bestask inv = max[0,((midprice-1)-alpha)]
>                                  where
>                                  midprice = ((bestbid+bestask)/2)
>                                  alpha    = zeta*(1-((ul-1-inv)/(ul-ll-2)))
>                                  zeta     = 6
>                                  ul       = 3000
>                                  ll       = (-3000)

>t2_ask::num->num->(otype, num, num, num)
>t2_ask t i = t2_order Ask (t2_asksize (t2_inv i t)) (t2_askprice (e1_bestbid(t-1)) (e1_bestask(t-1)) (t2_inv i t)) i

>t2_asksize::num->num
>t2_asksize x = 0, if x<=ll
>             = t2_asksize_(x), otherwise
>               where
>               ll = (-3000)


>t2_asksize_::num->num
>t2_asksize_ x = max[0, (x-(ll+1))]
>                where
>                ll = (-3000)

>t2_askprice::num->num->num->num
>t2_askprice bestbid bestask inv = max[0,((midprice+1)+alpha)]
>                                  where
>                                  midprice = ((bestbid+bestask)/2)
>                                  alpha = zeta*((ul-1-inv)/(ul-ll-2))
>                                  zeta = 6
>                                  ul = 3000
>                                  ll = (-3000)

>t2_buy::num->num->(otype, num, num, num)
>t2_buy t i = t2_order Buy (t2_buysize (t2_inv i t)) nu i
>             where
>             nu = 0

>t2_buysize::num->num
>t2_buysize x = 0, if x>ll
>             = -ll, otherwise
>               where
>               ll = (-3000)

















>e1_exchoutput1::num->([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])
>e1_exchoutput1 t = e1_match (e1_bidbook_ t) (e1_sells t)

>e1_match :: [(otype,num,num,num)] -> [(otype,num,num,num)]-> ([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])
>e1_match []            m             = ([],[],[])
>e1_match l             []            = (l, [],[])
>e1_match ((a,b,c,d):q) ((e,f,g,h):z) = (i, ((a,f,c,d):j), ((e,f,g,h):k)), if f < b
>                                       where
>                                       (i,j,k) = e1_match q ((e,(b-f),g,h):z)
>e1_match ((a,b,c,d):q) ((e,f,g,h):z) = (i, ((a,f,c,d):j), ((e,f,g,h):k)), if f > b
>                                       where
>                                       (i,j,k) = e1_match q ((e,(f-b),g,h):z)
>e1_match ((a,b,c,d):q) ((e,f,g,h):z) = (i, ((a,f,c,d):j), ((e,f,g,h):k)), if f = b
>                                       where
>                                       (i,j,k) = e1_match q z

>e1_bidbook_::num->[(otype, num, num, num)]
>e1_bidbook_ t = e1_insertbid [] (e1_bids t)

>e1_insertbid::[(otype, num, num, num)]->[(otype, num, num, num)]->[(otype, num, num, num)]
>e1_insertbid x             []            = x
>e1_insertbid []            (a:y)         = e1_insertbid [a] y
>e1_insertbid ((d,e,f,g):q) ((t,s,p,i):y) = e1_insertbid ((t,s,p,i):x) y, if p>f
>                                         = (d,e,f,g):(e1_insertbid q z), otherwise
>                                           where
>                                           x = ((d,e,f,g):q)
>                                           z = ((t,s,p,i):y)

>e1_bids::num->[(otype, num, num, num)]
>e1_bids t = [(t1_bid t 1), (t2_bid t 2)]

>e1_sells::num->[(otype, num, num, num)]
>e1_sells t = [(t1_sell t 1), (t2_sell t 2)]

>e1_exchoutput2::num->([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])
>e1_exchoutput2 t = e1_match (e1_askbook_ t) (e1_buys  t)

>e1_askbook_::num->[(otype, num, num, num)]
>e1_askbook_ t = e1_insertask [] (e1_asks t)

>e1_insertask::[(otype, num, num, num)]->[(otype, num, num, num)]->[(otype, num, num, num)]
>e1_insertask x             []            = x
>e1_insertask []            (a:y)         = (e1_insertask [a] y)
>e1_insertask ((d,e,f,g):q) ((t,s,p,i):y) = e1_insertask ((t,s,p,i):x) y, if p<f
>                                         = (d,e,f,g):(e1_insertask q z), otherwise
>                                           where
>                                           x = ((d,e,f,g):q)
>                                           z = ((t,s,p,i):y)

>e1_asks::num->[(otype, num, num, num)]
>e1_asks t = [(t1_ask t 1), (t2_ask t 2)]

>e1_buys::num->[(otype, num, num, num)]
>e1_buys t = [(t1_buy t 1), (t2_buy t 2)]

>e1_bestbid::num->num
>e1_bestbid t = e1_fstls (e1_bidbook t)

>e1_bidbook::num->[(otype,num,num,num)]
>e1_bidbook t = e1_fst3 (e1_exchoutput1 t)

>e1_bestask::num->num
>e1_bestask t = e1_fstls (e1_askbook t)

>e1_askbook::num->[(otype,num,num,num)]
>e1_askbook t = e1_fst3 (e1_exchoutput2 t)

>e1_fst3::([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])->[(otype,num,num,num)]
>e1_fst3 (a,b,c) = a

>e1_fstls::[(otype, num, num, num)]->num
>e1_fstls ((a, b, c, d):r) = c








