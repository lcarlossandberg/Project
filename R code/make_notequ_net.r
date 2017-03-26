#creates a csv file containg a new network, this network is created using
#a adaptation of the Erdos-Renyi method where a network


#the number of nodes to be used in the new network
numNodes = 145

#reads in the list of equities relating to each bank
equities <- read.csv("equities.csv",head=FALSE, sep=",")


#calculates the total value of all equities, needed to calcualte the fraction probability
#totEquity = 0
#for(i in 1:numNodes) {
#    totEquity = totEquity + equities[i]
#}
#totEquity

#this is the fraction probability, multiplyng this by the equity of
#a bank gives the probability of drawing a link to this bank
fracProb = 7.08e-8 



#no weight, probability distrbuted random network
nwp_net = matrix(numeric(numNodes*numNodes), nrow=numNodes, ncol=numNodes)

#here links represnted by 1 are assigned with a probability of
#fracProb*the equity of the bank being connected to and
#are not assigned along the diagonal, no links directly to self
for(i in 1:numNodes) {
    for(j in 1:numNodes) {
        ranNum <- runif(1, 0, 1)
        probCon = fracProb*equities[j]
        
        if ((i != j) & (ranNum<probCon)) {
            nwp_net[i, j] <- 1
        }
    }
}

#counts the number of links in the network for testing perporpuses
sum(nwp_net)


#writes the new network directly to a file
write.table(nwp_net, file="testFile.csv", row.names=FALSE, col.names=FALSE, sep=",")



