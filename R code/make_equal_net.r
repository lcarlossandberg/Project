#creates a csv file containg a new network, this network is created using the Erdos-Renyi method


#the number of nodes to be used in the new network
numNodes = 145

#no weight, equal distrbuted random network
nwe_net = matrix(numeric(numNodes*numNodes), nrow=numNodes, ncol=numNodes)

#here links represnted by 1 are assigned with a probability 0.297 and
#are not assigned along the diagonal, not links directly to self
for(i in 1:numNodes) {
    for(j in 1:numNodes) {
        ranNum <- runif(1, 0, 1)
        
        if ((i != j) & (ranNum<0.297)) {
            nwe_net[i, j] <- 1
        }
    }
}


counter = 0
for(i in 1:numNodes) {
    for(j in 1:numNodes) {
        k = nwe_net[i,j]
        if (k != 0) {
            
            counter = counter + 1
            
        }
    }
}
counter

#writes the new network directly to a file
write.table(nwe_net, file="testFile.csv", row.names=FALSE, col.names=FALSE, sep=",")



