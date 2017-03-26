#creates a csv file containg a weighted version of the normal Erdos-Renyi network




#this finds the total value of all interbank assets
#oldnetwork <- read.csv("interbank.csv",head=FALSE, sep=",")

#sum(oldnetwork) #=2425843593

totAssets = 2425843593


network <- read.csv("equ_random_net.csv",head=FALSE, sep=",")

#the number of nodes to be used in the  network
numNodes = 145

#calculates the number of links
#counter = 0
#for(i in 1:numNodes) {
#    for(j in 1:numNodes) {
#        k = network[i,j]
#        if (k != 0) {
#
#            counter = counter + 1
#
#        }
#    }
#}
#counter

numLinks = 6165

#the amount of assets that will be assigned to each link
#linkAssets = totAssets/numLinks = 393486.4....

linkAssets = 393486 #rounded to a whole number


#here the links that exist are multiplied by the weight
for(i in 1:numNodes) {
    for(j in 1:numNodes) {

        k = network[i,j]
        if (k != 0) {
            network[i, j] <- linkAssets
        }
    }
}



#writes the new network directly to a file
write.table(network, file="testFile.csv", row.names=FALSE, col.names=FALSE, sep=",")

















