
linkst <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

nodest <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)




#this code is to convert a i j matrix containing weigths into a
#matrix of to from weight and a list of all links (nodes and links matrix)

#this reads in the intial i j input file
links1 <- read.csv("interbank.csv",head=FALSE, sep=",")
#links1 <- read.csv("mynode.csv",head=FALSE, sep=",")


#this works out how many nodes there are
numNodes = length(links1$V1)
numNodes

#this puts the nodes in matrix that can be used
nodes1 <- 1:numNodes

row_n.names <- 1:numNodes
column_n.names <- c("id")

nodes <- matrix(nodes1, ncol = 1, dimnames = list(row_n.names, column_n.names))


#this section works out home many non-zero links there are
counter = 0
for(i in 1:numNodes) {
    for(j in 1:numNodes) {
        k = links1[i,j]
        if (k != 0) {
            
            counter = counter + 1
            
        }
    }
}
counter

#here the vector to hold these links is made, counter*3 is because for each like there is a
#to, from and weight component
links2 <- numeric((counter*3)) #length(a)

#here the links2 vector is filled with the to from and weight values for non-zero cases
counter2 =1
for(i in 1:numNodes) {
    for(j in 1:numNodes) {
        k = links1[i,j]
        if (k != 0) {
            links2[counter2] <- i
            links2[counter2+1] <- j
            links2[counter2+2] <- k

            counter2 = counter2+3
        }
    }
}


numLinks1 = length(links2)
numLinks= numLinks1/3

row_l.names <- 1:numLinks
column_l.names <- c("from","to","weight")

links <- matrix(links2, byrow = TRUE, ncol = 3, dimnames = list(row_l.names, column_l.names))



library('igraph')
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)

deg <- degree(net, mode="all")
V(net)$size <- deg/100           #this changes the node size depending on the numeber of links

#E(net)$width <- E(net)$weight #this changes the link thickness depending on the weight

#plot(net, edge.arrow.size=.4)


#deletes all links with weight less then the mean
links_x <- as.data.frame(links)
cut.off <- mean(links_x$weight)
net.sp <- delete_edges(net, E(net)[weight<cut.off])


#this deletes all nodes with connects less then the mean
#net.sp <- delete_vertices(net, V(net)[deg<mean(deg)])
#plot(net.sp, layout = layout_with_mds)





plot(net.sp, edge.arrow.size=.4,vertex.label=NA, layout = layout_with_fr)














