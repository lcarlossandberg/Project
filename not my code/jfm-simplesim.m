>  || literate script   

This is a very simple numerical simulation of the dynamic interaction model in
the paper submitted to JFM.

To produce graphs, we want to know (i) inventories (ii) prices (iii) sell pressure (iv) buy pressure


An order is given by the type order_t

>order_t ::= Order ordertype_t ordersize_t orderprice_t orderid_t ordertime_t
>ordertype_t ::= Bid | Ask | Buy | Sell
>ordersize_t == num
>orderprice_t == num
>orderid_t == num
>ordertime_t == num

And here are some helper functions:

>getorderid         (Order ty sz p i t)       = i
>getordersize       (Order ty sz p i t)       = sz
>getorderprice      (Order ty sz p i t)       = p
>getordertime       (Order ty sz p i t)       = t
>setordertime       (Order ty sz p i t) newt  = Order ty sz p i newt
>setordersize       (Order ty sz p i t) newsz = Order ty newsz p i t
>setorderprice      (Order ty sz p i t) newp  = Order ty sz newp i t
>subtractordersize  (Order ty sz p i t) subsz = Order ty (sz-subsz) p i t


Market makers output orders, but we also make them output their own inventories so we can plot this on a graph.
The graph is plotted as the output from the exchange, so the market makers pass their inventories to the exchange.
The market makers take as input parameters: xbids, xasks, xsells, xbuys, bestbid and bestask etc from the exchange, 
and time and oldinv etc are state parameters


>mm xq ((bids,asks,sells,buys,xbids, xasks, xsells, xbuys, bestbid, bestask, ltp, pp, sellp, buyp, invs):rest) oldinv mosize time id 
>    = (bid, ask, sell, buy, newinv) : (mm nxq rest newinv mosize (time+1) id)
>      where
>      (xbidsq, xasksq, xbuysq, xsellsq) =  xq
>      (xbidsq1, xasksq1, xbuysq1, xsellsq1) 
>                = makedelay delta1 (xbidsq++[xbids], xasksq++[xasks], xbuysq++[xbuys], xsellsq++[xsells]), if time=delaytime1
>                = makedelay delta2 (xbidsq++[xbids], xasksq++[xasks], xbuysq++[xbuys], xsellsq++[xsells]), if time=delaytime2
>                = makedelay delta3 (xbidsq++[xbids], xasksq++[xasks], xbuysq++[xbuys], xsellsq++[xsells]), if time=delaytime3
>                = (xbidsq++[xbids], xasksq++[xasks], xbuysq++[xbuys], xsellsq++[xsells]), otherwise
>      makedelay n (a,b,c,d) = (delay n ([]:a), delay n ([]:b), delay n ([]:c), delay n ([]:d))
>      (xbids1, xasks1, xbuys1, xsells1) = (hd xbidsq1, hd xasksq1, hd xbuysq1, hd xsellsq1)
>      nxq = (tl xbidsq1, tl xasksq1, tl xbuysq1, tl xsellsq1)
>      bid  = Order Bid  bidsize  (bidprice bestbid1 bestask1 newinv) id time, if ((time mod 2) = 0)&(newinv<ul)&(newinv>ll)
>           = Order Bid  0        0                                   id time, otherwise
>      ask  = Order Ask  asksize  (askprice bestbid1 bestask1 newinv) id time, if ((time mod 2) = 0)&(newinv<ul)&(newinv>ll)
>           = Order Ask  0        0                                   id time, otherwise
>      buy  = Order Buy  buysize  0                                   id time, if ((time mod 2) = 0)&(newinv<=ll)
>           = Order Buy  0        0                                   id time, otherwise
>      sell = Order Sell sellsize 0                                   id time, if ((time mod 2) = 0)&(newinv>=ul)
>           = Order Sell 0        0                                   id time, otherwise
>      newinv = oldinv + (psi id xbids1) + (psi id xbuys1) - (psi id xasks1) - (psi id xsells1)
>      psi i os = foldr (+) 0 (map getordersize (filter ((=i).getorderid) os))
>      bidsize  = max[0,ul-1-newinv]
>      asksize  = max[0,newinv-(ll+1)]
>      buysize  = 0,   if (newinv > ll)
>               = mosize, otherwise || -ll, otherwise
>      sellsize = 0,   if (newinv < ul)
>               = mosize, otherwise || ul,  otherwise
>      bestbid1 = bestbid, if bestbid > 0
>               = max [0,ltp-10], otherwise
>      bestask1 = bestask, if bestask > 0
>               = ltp+10, otherwise
>      bidprice bb ba ni = max [0,bb - abs(ba-bb)*(1-(ul-1-ni)/(ul-ll-2))]
>      askprice bb ba ni = max [0,ba + abs(ba-bb)*(ul-1-ni)/(ul-ll-2)]


The fundamental seller is similar in form to the market maker, but only issues sell orders up to a given time.
The fundamental seller has id 3.


>fs xq ((bids,asks,sells,buys,xbids, xasks, xsells, xbuys, bestbid, bestask, ltp, pp, sellp, buyp, invs):rest) oldinv stoptime time id
>    = (bid, ask, sell, buy, newinv) : (fs xq rest newinv stoptime (time+1) id)
>      where
>      newinv = oldinv + (psi id xbids) + (psi id xbuys) - (psi id xasks) - (psi id xsells)
>      psi i os = foldr (+) 0 (map getordersize (filter ((=i).getorderid) os))
>      bid  = Order Bid  0    0       id time
>      ask  = Order Ask  0    0       id time
>      buy  = Order Buy  0    0       id time
>      sell = Order Sell fsellsize  0 id time, if ((time mod 2)=0) & (time < stoptime)
>           = Order Sell 0    0       id time, otherwise
>
>fb xq ((bids,asks,sells,buys,xbids, xasks, xsells, xbuys, bestbid, bestask, ltp, pp, sellp, buyp, invs):rest) oldinv stoptime time id
>    = (bid, ask, sell, buy, newinv) : (fb xq rest newinv stoptime (time+1) id)
>      where
>      newinv = oldinv + (psi id xbids) + (psi id xbuys) - (psi id xasks) - (psi id xsells)
>      psi i os = foldr (+) 0 (map getordersize (filter ((=i).getorderid) os))
>      bid  = Order Bid  0    0       id time
>      ask  = Order Ask  0    0       id time
>      sell = Order Sell  0    0       id time
>      buy  = Order Buy (fsellsize/2)  0 id time, if ((time mod 2)=0) & (time < stoptime) & ((time mod 30) = 0)
>           = Order Buy 0    0       id time, otherwise
>



The exchange takes in lists of orders from the traders and produces the bestbid and bestask plus
lists of confirmations (and price, inventories, sellp and buyp for the graphs).  
The exchange also takes in and outputs its own bidbook and askbook.  The exchange has id 0.

>exch:: num -> [([order_t],[order_t],[order_t],[order_t],[num])]->num->[order_t]->[order_t] -> num
>           -> [([order_t],[order_t],[order_t],[order_t],[order_t],[order_t],[order_t],[order_t],num,num,num,[num],num,num,[num])]
>exch id ((bids,asks,sells,buys,invs):rest) time bbook abook ltp
>    = (bids,asks,sells,buys,xbids, xasks, xsells, xbuys, bb, ba, newltp, pp, sellp, buyp, invs): (exch id rest (time+1) newbbook newabook newltp)
>      where
>      bbook1 = insb bids [], if allfillandkill
>             = insb bids bbook, otherwise
>               where
>               insb []     []     = []
>               insb []     ys     = ys
>               insb (x:xs) []     = insb xs [], if (getordersize x) = 0
>                                  = insb xs [x], otherwise
>               insb (x:xs) (y:ys) = insb xs (y:ys), if (getordersize x) = 0
>                                  = insb xs (x:(y:ys)), if (getorderprice x) > (getorderprice y)
>                                  = y:(insb (x:xs) ys), otherwise
>      abook1 = insa asks [], if allfillandkill
>             = insa asks abook, otherwise
>               where
>               insa []     []     = []
>               insa []     ys     = ys
>               insa (x:xs) []     = insa xs [], if (getordersize x) = 0
>                                  = insa xs [x], otherwise
>               insa (x:xs) (y:ys) = insa xs (y:ys), if (getordersize x) = 0
>                                  = insa xs (x:(y:ys)), if (getorderprice x) < (getorderprice y)
>                                  = y:(insa (x:xs) ys), otherwise
>      (xbids1, xasks1, bbook2, abook2) 
>             = ([],[],bbook1,abook1)
>             ||= uncross bbook1 abook1
>             ||  where
>             ||  uncross [] [] = ([],[],[],[])
>             ||  uncross [] ys = ([],[],[],ys)
>             ||  uncross xs [] = ([],[],xs,[])
>             ||  uncross (x:xs) (y:ys) = (xb:bs, xa:as, nbb, nab), if (getorderprice x) >= (getorderprice y)
>             ||                        = ([],[],(x:xs),(y:ys)), otherwise
>             ||                          where
>             ||                          (bs, as, nbb, nab) = uncross xs1 ys1
>             ||                          exsize  = min [getordersize x, getordersize y]
>             ||                          exprice = getorderprice x, if (getordertime x <= getordertime y)
>             ||                                  = getorderprice y, otherwise
>             ||                          xb = setordersize (setorderprice x exprice) exsize
>             ||                          xa = setordersize (setorderprice y exprice) exsize
>             ||                          xs1 = xs, if (getordersize y) >= (getordersize x)
>             ||                              = runt:xs, otherwise
>             ||                                where
>             ||                                runt = subtractordersize x ((getordersize x) - exsize)
>             ||                          ys1 = ys, if (getordersize x) >= (getordersize y)
>             ||                              = runt:ys, otherwise
>             ||                                where
>             ||                                runt = subtractordersize y ((getordersize y) - exsize)
>      (xbids2, xsells, newbbook) = match bbook2 sells
>      (xasks2, xbuys,  newabook) = match abook2 buys
>      (xbids, xasks) = (xbids1++xbids2, xasks1++xasks2)
>      newltp = min (map getorderprice xbids), if xbids ~= []
>             = max (map getorderprice xasks), if xasks ~= []
>             = ltp, otherwise
>      sellp = (foldr (+) 0 (map getordersize sells)) /(1+ (foldr (+) 0 (map getordersize bbook1)))
>      buyp = (foldr (+) 0 (map getordersize buys)) / (1+ (foldr (+) 0 (map getordersize abook1)))
>      bb = 0, if (newbbook = [])
>         = getorderprice (hd newbbook), otherwise
>      ba = 0, if (newabook = [])
>         = getorderprice (hd newabook), otherwise
>      pp = interleave xbuys xsells || assume interleaved selling and buying
>           where
>           interleave []     ys     = map getorderprice ys
>           interleave xs     []     = map getorderprice xs
>           interleave (x:xs) (y:ys) = (getorderprice x):((getorderprice y):(interleave xs ys))
>      match ls     []     = ([],[],ls)  || xlims, xmos, newbook
>      match []     ms     = ([],[],[])
>      match (l:ls) (m:ms) = match (l:ls) ms, if (getordersize m)=0
>                          = match ls (m:ms), if (getordersize l)=0
>                          = (l:a1,xmark1:b1,c1),  if (getordersize l) < (getordersize m)
>                          = (xlim2:a2, m2:b2,c2), if (getordersize l) > (getordersize m)
>                          = (l:a3, m2:b3,c3), otherwise
>                            where
>                            (a1,b1,c1) = match ls (m1:ms)
>                            (a2,b2,c2) = match (l1:ls) ms
>                            (a3,b3,c3) = match ls ms:]q!

>                            lsize = getordersize l
>                            lprice = getorderprice l
>                            msize = getordersize m
>                            xmark1 = setorderprice (setordersize m lsize) lprice
>                            m1 = subtractordersize m lsize
>                            m2 = setorderprice m lprice
>                            xlim2 = setordersize l msize
>                            l1 = subtractordersize l msize


We will need a delay component as follows:

>delay n []     = []
>delay n (x:xs) = (rep n x)++xs


Now we need to run a numerical simulation which uses the above definitions.

>sim steps = take steps allexchstates
>            where
>            allexchstates = initexchstate: (exch 0 allmessages 1 [] [] startprice)
>            initexchstate = ([],[],[],[],[],[],[],[],startprice-(startspread/2),startprice+(startspread/2),startprice,[],0,0,[]) 
>                            || bids,asks,sells,buys,xbids,xasks,xsells,xbuys,bb,ba,ltp,pp,sellp,buyp,invs
>            allmessages    = mergedmessages
>            tradermessages = map g (zip2 (agents allexchstates) [1..])
>                             where
>                             g (f,id) = f 0 id
>            mergedmessages = f tradermessages
>                             where
>                             f []         = []
>                             f ([]:rest)  = []   || if any trader has stopped issuing messages, we halt the simulation
>                             f any        = (g (map hd any) ([],   [],   [],    [],   []  )) : (f (map tl any))
>                                                         || (bids, asks, sells, buys, invs) 
>                                                         || All collected together for each time step
>                                                         || NB this does not depend on the number of traders - so it should work fine!
>                                                         || Justin - ignore my previous comment here!!!!!
>                                                         ||          (I confused simplesim with big sim!)
>                             g []                        (a,b,c,d,e) = (a,b,c,d,e)
>                             g ((a1,a2,a3,a4,a5):rest  ) (a,b,c,d,e) = g rest (a1:a, a2:b, a3:c,  a4:d, a5:e)
>                              ||
>                              ||(agent1 output ):others)                      (bids, asks, sells, buys, invs)
>                              ||

And now finally we need to output the sim results to file

>runtest steps = [Tofile "simplesim12mm.csv" (hdrs ++ (g startprice 0 (sim steps))), Closefile "simplesim2mm.csv"]
>                where
>                ||hdrs = "Price,SellPressure,BuyPressure,Inv5,Inv4,Inv3,Inv2,Inv1"
>                hdrs = "Time,Bids,Asks,Buys,Sells,Bidprice1,Askprice1,AskSize1,Buys1,Sells1,Price,SellPressure,BuyPressure,Net Pressure,BestBid,BestAsk,Invs\n"
>                g p t []     = []
>                g p t (x:xs) = output ++ (g ltp (t+1) xs)
>                               where
>                               (output,ltp) = h p t x
>                h p t (bids,asks,sells,buys,xbids,xasks,xsells,xbuys,bb,ba,ltp,pp,sellp,buyp,invs)
>                         = k 
>                             (foldr (+) 0 (map getordersize bids))  || ((getordersize.safehd) (filter ((=1).getorderid) bids)) 
>                             (foldr (+) 0 (map getordersize asks))  || ((getordersize.safehd) (filter ((=2).getorderid) bids)) 
>                             (foldr (+) 0 (map getordersize buys))  || ((getordersize.safehd) (filter ((=1).getorderid) asks)) 
>                             (foldr (+) 0 (map getordersize sells))  || ((getordersize.safehd) (filter ((=2).getorderid) asks)) 
>                             ((getorderprice.safehd) (filter ((=1).getorderid) bids)) 
>                             ((getorderprice.safehd) (filter ((=1).getorderid) asks)) 
>                             ((getordersize.safehd) (filter ((=1).getorderid) asks)) 
>                             ((getordersize.safehd) (filter ((=1).getorderid) buys)) 
>                             ((getordersize.safehd) (filter ((=1).getorderid) sells)) 
>                             || ((getordersize.safehd) (filter ((=1).getorderid) xbids)) 
>                             || ((getordersize.safehd) (filter ((=2).getorderid) xbids)) 
>                             sellp buyp bb ba invs pp ltp
>                           where
>                           safehd [] = Order Bid 0 0 0 0
>                           safehd (x:xs) = x
>                           showwithcommas []     = ""
>                           showwithcommas [x]    = (shownum x)
>                           showwithcommas (x:xs) = (shownum x)++","++(showwithcommas xs)
>                           k  bi as bu se bi1 as1 asize1 bu1 se1 sp bp bb ba is [] ltp
>                              = ((showwithcommas ([t,bi,as,bu,se,bi1,as1,asize1,bu1,se1,ltp,sp,bp,(sp-bp),bb,ba]++is))++"\n",ltp)
>                           k  bi as bu se bi1 as1 asize1 bu1 se1 sp bp bb ba is xs ltp 
>                              = (kk bi as bu se bi1 as1 asize1 bu1 se1 sp bp bb ba is xs, ltp)
>                           kk bi as bu se bi1 as1 asize1 bu1 se1 sp bp bb ba is [] = error "kk applied to []"
>                           kk bi as bu se bi1 as1 asize1 bu1 se1 sp bp bb ba is [x]    
>                              = (showwithcommas ([t,bi,as,bu,se,bi1,as1,asize1,bu1,se1,x,sp,bp,(sp-bp),bb,ba]++is))++"\n"
>                           kk bi as bu se bi1 as1 asize1 bu1 se1 sp bp bb ba is (x:xs) 
>                              = ((showwithcommas ([t,bi,as,bu,se,bi1,as1,asize1,bu1,se1,x,sp,bp,(sp-bp),bb,ba]++is))++"\n")
>                                ++(kk bi as bu se bi1 as1 asize1 bu1 se1 sp bp bb ba is xs)


For each experiment we need to specify some agents as follows. This is a list of partial applications of the function "mm"; 
the function "sim" will apply each of these to its start time and to its unique agent id 
(NB I changed the order of the arguments to mm and fs so this would work):

>agents agentinput = [
>                     mm initxq agentinput (mmsizes!0)  market_order_size,
>                     mm initxq agentinput (mmsizes!1)  market_order_size,
>                     mm initxq agentinput (mmsizes!2)  market_order_size,
>                     mm initxq agentinput (mmsizes!3)  market_order_size,
>                     mm initxq agentinput (mmsizes!4)  market_order_size,
>                     mm initxq agentinput (mmsizes!5)  market_order_size,
>                     mm initxq agentinput (mmsizes!6)  market_order_size,
>                     mm initxq agentinput (mmsizes!7)  market_order_size,
>                     mm initxq agentinput (mmsizes!8)  market_order_size,
>                     mm initxq agentinput (mmsizes!9)  market_order_size,
>                     mm initxq agentinput (mmsizes!10) market_order_size,
>                     mm initxq agentinput (mmsizes!11) market_order_size,
>                     fs initxq agentinput 0 fstoptime
>                     ||fb initxq agentinput 0 (fstoptime-50)
>                    ]
>                    where
>                    initxq = ([],[],[],[])


Other parameters for this experiment:

>ul = 3000
>ll = (-ul)
>allfillandkill = False
>delta1 = 0 || 2
>delaytime1 = 90
>delta2 = 3 || 3
>delaytime2 = 130
>delta3 = 6 || 4
>delaytime3 = 170
>startprice=1113
>startspread=20
>fsellsize=500 ||2500 || 4000
>fstoptime= 180
>market_order_size= 177
>mmsizes = [1650,1650,20,-20,40,-40,60,-60,80,-80,100,-100] || [3300,20,-20,40,-40,60,-60,0,0,800,-800,0]

