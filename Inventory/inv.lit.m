> ||This is literate script

Here the equations for inventory in thepaper Dynamic
Coupling and Market Instabilites, will be expressed.


The inventory at any one time is realated to the inventory at the previous time step with the adjustments
of four other terms, bids, buys, asks and sells. This has a form like,
inv(i,t) = inv(i,t-1) + p(i,xbids(t-1)) + p(i,xbuys(t-1)) - p(i,xasks(t-1)) - p(i,xsells(t-1)),
where p is the selecter function and xbids is the excuted bids (similar for xasks xsells xbuys)
at that time step. i is the ID of the Market-maker, t is discrete time inv is intentory.

>inv::num->num->num
>inv i 0 = 0 ||some intial condtion, say 0 inventory at time = 0
>inv i t = (inv (t-1) i) + (psi (xbids(t-1)) i) + (psi (xbuys(t-1)) i) - (psi (xasks(t-1)) i) - (psi (xsells(t-1)) i)


psi() a function that takes a list of orders with each order having four elements the type of order,
size of the order, the size of a limit order and the trader identifier: a, b, c and d.
a: type of order
b: size of order
c: price of limit order
d: trader identifier (i)
It returns the sum of the size of all orders for the selected trader

>otype::=Buy|Sell|Bid|Ask  ||define the order type


>psi::[(otype, num, num, num)]->num->num
>psi []               i = 0
>psi ((a, b, c, d):r) i = b+(psi r i), if d=i
>                       = (psi r i), otherwise



bids creates a list of each bid for each trader at the same time step
toti is the total number of traders in the simulation

>bids::num->[(otype, num, num, num)]
>bids t = [(bid t i) | i <- [0..maxi]]
>         where
>         maxi = 2


bid is a tuple type output containing the bid information for a single bid

>bid::num->num->(otype, num, num, num)
>bid t i = order Bid (bidsize (inv i t)) (bidprice (bestbid(t-1)) (bestask(t-1)) (inv i t)) i


Buys follows a similar logic

>buys::num->[(otype, num, num, num)]
>buys t = [(buy t i) | i <- [0..maxi]]
>         where
>         maxi = 2


>buy::num->num->(otype, num, num, num)
>buy t i = order Buy (buysize (inv i t)) nu i
>          where
>          nu = 0


Aks follows a similar logic

>asks::num->[(otype, num, num, num)]
>asks t = [(ask t i) | i <- [0..maxi]]
>         where
>         maxi = 2


>ask::num->num->(otype, num, num, num)
>ask t i = order Ask (asksize (inv i t)) (askprice (bestbid(t-1)) (bestask(t-1)) (inv i t)) i


Sells follows a similar logic

>sells::num->[(otype, num, num, num)]
>sells t = [(sell t i) | i <- [0..maxi]]
>          where
>          maxi = 2


>sell::num->num->(otype, num, num, num)
>sell t i = order Sell (sellsize (inv i t)) nu i
>           where
>           nu = 0


Order turns the indervidual inputs into a tuple type object

>order::otype->num->num->num->(otype, num, num, num)
>order a b c d = (a, b, c, d)


bidsize calculates how big a bid should be

>bidsize::num->num
>bidsize x = 0, if x>=ul
>          = bidsize_(x), otherwise
>            where
>            ul = 100


>bidsize_::num->num
>bidsize_ x = max[0, (ul-1-x)]
>             where
>             ul = 100


>buysize::num->num
>buysize x = 0, if x>ll
>          = -ll, otherwise
>            where
>            ll = -100


>asksize::num->num
>asksize x = 0, if x<=ll
>          = asksize_(x), otherwise
>            where
>            ll = -100


>asksize_::num->num
>asksize_ x = max[0, (x-(ll+1))]
>             where
>             ll = -100


>sellsize::num->num
>sellsize x = 0, if x<ul
>           = ul, otherwise
>             where
>             ul = 100


>bidprice::num->num->num->num
>bidprice bestbid bestask inv = max[0,((midprice-1)-alpha)]
>                               where
>                               midprice = ((bestbid+bestask)/2)
>                               alpha    = zeta*(1-((ul-1-inv)/(ul-ll-2)))
>                               zeta     = 6
>                               ul       = 100
>                               ll       = (-100)
                                

>askprice::num->num->num->num
>askprice bestbid bestask inv = max[0,((midprice+1)+alpha)]
>                               where
>                               midprice = ((bestbid+bestask)/2)
>                               alpha = zeta*((ul-1-inv)/(ul-ll-2))
>                               zeta = 6
>                               ul = 100
>                               ll = (-100)
                                              
                                              
                                              
the bestask and bestbid are taken from the front of the bid and ask books
                                              
>bestbid::num->num
>bestbid t = fstls (bidbook t)
   
                                              
>bestask::num->num
>bestask t = fstls (askbook t)
       
                                              
fstls returns the price from the order first in the list of the bid/ask book
                                              
>fstls::[(otype, num, num, num)]->num
>fstls ((a, b, c, d):r) = c


askbook_ and bidbook_ are a list of all the asks and bids
                                              
>askbook_::num->[(otype, num, num, num)]
>askbook_ t = insertask [] (asks t)


>bidbook_::num->[(otype, num, num, num)]
>bidbook_ t = insertbid [] (bids t)
                                              
insertbid and insertask creat ordered list of the bids and asks at a set [time
                                              
>insertask::[(otype, num, num, num)]->[(otype, num, num, num)]->[(otype, num, num, num)]
>insertask x             []            = x
>insertask []            (a:y)         = (insertask [a] y)
>insertask ((d,e,f,g):q) ((t,s,p,i):y) = insertask ((t,s,p,i):x) y, if p<f
>                                      = (d,e,f,g):(insertask q z), otherwise
>                                        where
>                                        x = ((d,e,f,g):q)
>                                        z = ((t,s,p,i):y)
                                                                          
  
>insertbid::[(otype, num, num, num)]->[(otype, num, num, num)]->[(otype, num, num, num)]
>insertbid x             []            = x
>insertbid []            (a:y)         = insertbid [a] y
>insertbid ((d,e,f,g):q) ((t,s,p,i):y) = insertbid ((t,s,p,i):x) y, if p>f
>                                      = (d,e,f,g):(insertbid q z), otherwise
>                                        where
>                                        x = ((d,e,f,g):q)
>                                        z = ((t,s,p,i):y)



match takes the bidbook_ and sells or the askbook_ and the buys and returns the
bidbook, xbids and xsells or the askbook, xasks and xbuys

>match :: [(otype,num,num,num)] -> [(otype,num,num,num)]-> ([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])
>match []            m             = ([],[],[])
>match l             []            = (l, [],[])
>match ((a,b,c,d):q) ((e,f,g,h):z) = (i, ((a,f,c,d):j), ((e,f,g,h):k)), if f < b
>                                    where
>                                    (i,j,k) = match q ((e,(b-f),g,h):z)
>match ((a,b,c,d):q) ((e,f,g,h):z) = (i, ((a,f,c,d):j), ((e,f,g,h):k)), if f > b
>                                    where
>                                    (i,j,k) = match q ((e,(f-b),g,h):z)
>match ((a,b,c,d):q) ((e,f,g,h):z) = (i, ((a,f,c,d):j), ((e,f,g,h):k)), if f = b
>                                    where
>                                    (i,j,k) = match q z
                                                                          
                                                                          
exchoutput is the excahnges out put which is the return of the match function, exchoutput1 for bids and sells
exchoutput2 for asks and buys

>exchoutput1::num->([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])
>exchoutput1 t = match (bidbook_ t) (sells t)
                                                                          
>exchoutput2::num->([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])
>exchoutput2 t = match (askbook_ t) (buys  t)
                                                                          
                                                                          
fst3, snd3 and thd3 return the first sendond and third elements of a three element input respectively
used to accesses the return of match

>fst3::([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])->[(otype,num,num,num)]
>fst3 (a,b,c) = a
                                                                          
>snd3::([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])->[(otype,num,num,num)]
>snd3 (a,b,c) = b
                                                                          
>thd3::([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])->[(otype,num,num,num)]
>thd3 (a,b,c) = c
                                                                          

bidbook/askbook, xbids/xasks and xsells/xbuys are now defined as the returns from match
        
>bidbook::num->[(otype,num,num,num)]
>bidbook t = fst3 (exchoutput1 t)

                                                                          
>askbook::num->[(otype,num,num,num)]
>askbook t = fst3 (exchoutput2 t)
                                                                          
                                                                          
>xbids::num->[(otype,num,num,num)]
>xbids   t = snd3 (exchoutput1 t)
                                                                          
                                                                          
>xasks::num->[(otype,num,num,num)]
>xasks   t = snd3 (exchoutput2 t)
 
                                                                          
>xsells::num->[(otype,num,num,num)]
>xsells  t = thd3 (exchoutput1 t)

                                                                          
>xbuys::num->[(otype,num,num,num)]
>xbuys   t = thd3 (exchoutput2 t)
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              
                                              
