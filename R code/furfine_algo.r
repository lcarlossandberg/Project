#runs the furfine algorithm
assets <- read.csv("externalAssets.csv",head=FALSE, sep=",")

numNodes = 145

network <- read.csv("interbank.csv",head=FALSE, sep=",")



#stores if a bank is bust (0) or still functioning (1)
bust <- rep(1, numNodes)

bust[1] <- 0 #set first bank to fail
bust[2] <- 0
bust[3] <- 0
bust[4] <- 0
bust[5] <- 0
bust[6] <- 0

#function used to find the equity
new.equity <- function(i) {
    
    Owed = 0
    for(k in 1:numNodes) {
        Owed = Owed + (network[k,i]*bust[k])
    }
    
    #lost = 0
    #for(k in 1:numNodes) {
    #    lost = lost + (network[k,i]*(1-bust[k]))
    #}
    
    Recovered = 0#rRate*lost
    
    Owes = 0
    for(k in 1:numNodes) {
        Owes = Owes + network[i,k]
    }
    

#cat("i:", i,"owed",Owed,", ","owes",Owes,", ","asset",assets[i]) #for debugging
    
    assets[i] + Owed + Recovered - Owes
}





repeat{
    check = 0
    for(i in 1:numNodes){
        
        if(bust[i]==1){
            cEq = new.equity(i)
           
            if(cEq<0){
                
                 bust[i] <- 0
                 check = 1
            }
            
        }
        
    }
    bust
    if(check == 0){
        break
    }
}

bust









