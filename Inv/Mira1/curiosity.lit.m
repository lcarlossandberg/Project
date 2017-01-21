> ||This is literate script

Out of idle curiosity, what if we made simplesim even more like the equations and even less like the simulator?

i.e. instead of grouping recurrence relations into agents, and instead of modelling as recursive functions from list to list we model each recurrence relation separately

NB - although the code below shows that we CAN translate directly from recurrence relations to Miranda code, the performance data directly below shows that it is extremely (exponentially?) inefficient:

Miranda inv 1 0
2000
||reductions = 4, cells claimed = 13, no of gc's = 0, cpu = 0.00
Miranda inv 1 1
7999
||reductions = 5417, cells claimed = 8486, no of gc's = 0, cpu = 0.00
Miranda inv 1 2
23994
||reductions = 126326, cells claimed = 197789, no of gc's = 0, cpu = 0.00
Miranda inv 1 3
65982
||reductions = 3389179, cells claimed = 5306558, no of gc's = 0, cpu = 0.18
Miranda inv 1 4
175951
||reductions = 91002205, cells claimed = 142485548, no of gc's = 1, cpu = 4.92


\[
inv_{i,t} = inv_{i,(t-1)} + \psi(i,xbuys_(t-1)) - \psi(i,xsells_(t-1)) 
                          + \psi(i,xbids_(t-1)) - \psi(i,xasks_(t-1))
\]

>inv 0 0 = 0
>inv 1 0 = 2000
>inv 2 0 = 2000
>inv x 0 = error "only two traders"
>inv i t = (inv i (t-1)) + (psi i (xbuys (t-1))) - (psi i (xsells (t-1))) 
>                        + (psi i (xbids (t-1))) - (psi i (xasks (t-1)))

 invs i = 2000:[(invs i)!(t-1)+(psi i (xbuys (t-1)))-(psi i (xsells (t-1)))
                              +(psi i (xbids (t-1)))-(psi i (xasks (t-1))) | t<-[1..]]
 
>psi :: num -> [(otype,num,num,num)] -> num
>psi i [] = 0
>psi i ((a,b,c,d):r) = b + (psi i r), if d=i
>                    = psi i r, otherwise

>otype ::= Buy | Sell | Bid | Ask
>
>sell::num->num->(otype,num,num,num)
>sell 0 t = order Sell omega                0      0, if t < timelimit || fundamental seller
>         = order Sell 0                    0      0, otherwise
>           where
>           omega = 1000
>sell i 0 = order Sell 0                    0      i 
>sell i t = order Sell (sellsize inv i t) mosize i

>buy::num->num->(otype,num,num,num)
>buy  0 t = order Buy  0                    0      0                   || fundamental seller
>buy  i 0 = order Buy  0                    0      i 
>buy  i t = order Buy  (buysize  inv i t)   mosize i

>bid::num->num->(otype,num,num,num)
>bid  0 t = order Bid  0                    0      0                   || fundamental seller
>bid  i t = order Bid  (bidsize  inv i t) (bidprice mybb myba (inv i t)) i
>           where
>           bb = bestbid (t-1)
>           ba = bestask (t-1)
>           mybb = hd bb, if bb ~= []
>                = startprice-1, otherwise
>           myba = hd ba, if ba ~= []
>                = startprice+1, otherwise

>ask::num->num->(otype,num,num,num)
>ask  0 t = order Ask  0                    0      0                   || fundamental seller
>ask  i 0 = order Ask  (asksize  inv i 0)   (startprice+1)      i 
>ask  i 1 = order Ask  (asksize  inv i 1)   (startprice+1)      i 
>ask  i t = order Ask  (asksize  inv i t) (askprice mybb myba (inv i t)) i
>           where
>           bb = bestbid (t-1)
>           ba = bestask (t-1)
>           mybb = hd bb, if bb ~= []
>                = startprice-1, otherwise
>           myba = hd ba, if ba ~= []
>                = startprice+1, otherwise

>bestbid (-1) = []
>bestbid t = [thd4 (hd (bidbook t))], if (bidbook t) ~= []
>          = [], otherwise
>bestask (-1) = []
>bestask t = [thd4 (hd (askbook t))], if (askbook t) ~= []
>          = [], otherwise

>thd4 (a,b,c,d) = c

>mosize = 0
>maxi = 2
>bidprice bestbid bestask inv = max[0,(((bestbid+bestask)/2)-1)-zeta*(1-((ul-1-inv)/(ul-ll-2)))]
>askprice bestbid bestask inv = max[0,(((bestbid+bestask)/2)+1)+zeta*(((ul-1-inv)/(ul-ll-2)))] 
>startprice = 2000
>zeta=6
>ul=3000
>ll=(-3000)

>order ty sz pr id = (ty,sz,pr,id)

>
>buysize  inv i t = 0, if (inv i t > ll)
>                 = -ll, otherwise
>sellsize inv i t = 0, if (inv i t < ul)
>                 = ul, otherwise
>bidsize  inv i t = 0, if (inv i t >= ul)
>                 = bidsize1 inv i t, otherwise
>asksize  inv i t = 0, if (inv i t <= ll)
>                 = asksize1 inv i t, otherwise

>bidsize1 inv i t = max [0, ul - 1 - (inv i t)]
>asksize1 inv i t = max [0, (inv i t) - (ll + 1)]



>insertask x             []            = x
>insertask []            (a:y)         = insertask [a] y
>insertask ((d,e,f,g):q) ((t,s,p,i):y) = insertask ((t,s,p,i):x) y, if p<f
>                                      = (d,e,f,g):(insertask q z), otherwise
>                                        where
>                                        x = ((d,e,f,g):q)
>                                        z = ((t,s,p,i):y)
>       
>insertbid x             []            = x
>insertbid []            (a:y)         = insertbid [a] y
>insertbid ((d,e,f,g):q) ((t,s,p,i):y) = insertbid ((t,s,p,i):x) y, if p>f
>                                      = (d,e,f,g):(insertbid q z), otherwise
>                                        where
>                                        x = ((d,e,f,g):q)
>                                        z = ((t,s,p,i):y)
>       
>match :: [(otype,num,num,num)] -> [(otype,num,num,num)] 
>                               -> ([(otype,num,num,num)],[(otype,num,num,num)],[(otype,num,num,num)])
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
>
>bidbook1 t = insertbid [] (bids t)
>askbook1 t = insertask [] (asks t)
>bids  t = [bid i t  | i <- [0..maxi]]
>asks  t = [ask i t  | i <- [0..maxi]]
>buys  t = [buy i t  | i <- [0..maxi]]
>sells t = [sell i t | i <- [0..maxi]]
>
>bidbook t = fst3 (exchoutput1 t)
>askbook t = fst3 (exchoutput2 t)
>xbids   t = snd3 (exchoutput1 t)
>xasks   t = snd3 (exchoutput2 t)
>xsells  t = thd3 (exchoutput1 t)
>xbuys   t = thd3 (exchoutput2 t)
>exchoutput1 t = match (bidbook1 t) (sells t)
>exchoutput2 t = match (askbook1 t) (buys  t)
>fst3 (a,b,c) = a
>snd3 (a,b,c) = b
>thd3 (a,b,c) = c





>main = (map (inv 1) [0..4],
>        map (inv 2) [0..4])


>timelimit = 10 || time limit for fundamental seller
