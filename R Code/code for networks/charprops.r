#calculates the chariteristic properties of the interbank network
loans <- read.csv("interbank.csv", header=F, sep=",")

#how many nodes there are
numNodes = length(loans$V1)
#numNodes

#number of links in the network
counter = 0
for(i in 1:numNodes) {
    for(j in 1:numNodes) {
        k = loans[i,j]
        if (k != 0) {
            
            counter = counter + 1
            
        }
    }
}
#counter

#number of loops
#length 2
numLoops = 0

for (i in 1:numNodes) {
    numLoops = numLoops + loans[3,i]*loans[i,3]
}
numLoops







