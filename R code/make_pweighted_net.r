#creates a csv file containg a probability weighted version of the probability Erdos-Renyi network

#here the normalisation factor for the weights is found
#fracA = totAssets/(sum_j(E_j*(sum_i(network[i,j])))), where l_i is the number of links to a node and E_i is the equity of that node

#the network before weights are added
network <- read.csv("prob_random_net.csv",head=FALSE, sep=",")

#reads in the list of equities relating to each bank
equities <- read.csv("equities.csv",head=FALSE, sep=",")

#the number of nodes to be used in the  network
numNodes = 145


#test <- read.csv("mynode.csv",head=FALSE, sep=",")
#sum(network)


#caluclates the facrtion to mutilply the equity by to get the wieght of each node
#Equsum = 0
#for(j in 1:numNodes) {
#    Equsum = Equsum + sum(network[,j])*equities[j]
#}
#Equsum

#totAssets = 2425843593

#fracA = totAssets/Equsum
fracA = 0.03326993






#here the links that exist are multiplied by the weight
for(i in 1:numNodes) {
    for(j in 1:numNodes) {

        k = network[i,j]
        if (k != 0) {
            network[i, j] <- (fracA*equities[j])
        }
    }
}

sum(network)

#writes the new network directly to a file
write.table(network, file="testFile.csv", row.names=FALSE, col.names=FALSE, sep=",")

















