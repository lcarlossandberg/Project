> ||This is literate script

Here the equations for inventory in the paper Dynamic Coupling and Market Instabilites, will be expressed.
The inventory at any one time is realated to the inventory at the previous time step with the adjustments
of four other terms, bids, buys, asks and sells. This has a form like,
inv(i,t) = inv(i,t-1) + p(i,xbids(t-1)) + p(i,xbuys(t-1)) - p(i,xasks(t-1)) - p(i,xsells(t-1)),
where p is the selecter function and xbids is the excuted bids (similar for xasks xsells xbuys)
at that time step. i is the ID of the Market-maker, t is discrete time inv is intentory.
This could be coded as follows,

>inv::num->num->num
>inv i 0 = 0 ||some intial condtion say a 0 inventory at time = 0
>inv i t = (inv (t-1) i) + p(xbids(t-1) i) + p(xbuys(t-1) i) - p(xasks(t-1) i) - p(xsells(t-1) i)

p() a function that takes a list of orders with each order having four inputs the type of order,
size of the order, the size of a limit order and the trader identifier: a, b, c and d.
Which are inputed as a list for a, b and c and a single input for d: [a, b, c] d
a: type of order
b: size of order
c: size of limit order
d: trader identifier (i)

The output is a price  that is the sum of all the others for the same trader.

>p::[[num]]->num->num
>p []                i = 0
>p ([a, b, c, i]:xs) i = b+p(xs i)
>p (x:xs)            i = p(xs i)

The xbids etc are lists of bids with each bid containing four elements. An exapmle would be 
xbids = [[a,b,c,d], [e, f, g, h] ...] 
xbids contains the bids for all i traders
xbids takes the current time step which it is is calculaitng for as well as the identifier
of a trader to iterate over and a list of lists which is what it adds to to create an output
the itial list of lists should be empty 

>xbids::num->num->[[num]]->[[num]]
>xbids t 2 b = b++[bid(t 2)] ||end condtion when i = 2 (number of traders) returns the list of all orders
>xbids t i b = (xbids t (i+1) (b++[bid(t i)]))

Here bid calulcates the bid to be made in the [a, b, c, d].

>bid::num->num->[num]
>bid t i = order(bid, bidsize((inv i t)), bidprice(bestbid(t-1), bestask(t-1), (inv i t)), i)

>order::num->num->num->num->[num]
>order a b c i = [a, b, c, i]

>bid = 24 ||is this a function or just a number???

>bidsize::num->num
>bidsize x = 0, if x>=UL
           = bidsize_(x), otherwise ||bidsize_ is unknown at thee moment

>bidprice::num->num->num->num
>bidprice a b c = max [0, (midprice-1)-alpha*c]







  
