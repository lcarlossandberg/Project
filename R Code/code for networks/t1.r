nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

#links <- aggregate(links[,3], links[,-3], sum)
#links <- links[order(links$from, links$to),]
#colnames(links)[4] <- "weight"
#rownames(links) <- NULL



library('igraph')
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)

deg <- degree(net, mode="all")
V(net)$size <- deg*10           #this changes the node size depending on the numeber of links

E(net)$width <- E(net)$weight*2  #this changes the link thickness depending on the weight

plot(net, edge.arrow.size=.4)
