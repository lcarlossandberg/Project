#plots historgram
#equities <- read.csv("equities.csv", header=F, sep=",")

#df_e <- data.frame(equities)
#equities_vec  = as.numeric(df_e[1,])

#hist(equities_vec,main="Equities of Banks", xlab="Equity", ylab="Number of Banks"
#,breaks=1000)
#,breaks=1000,xlim=c(0,4e+07))


#equities <- read.csv("externalAssets.csv", header=F, sep=",")

#df_e <- data.frame(equities)
#equities_vec  = as.numeric(df_e[1,])

#hist(equities_vec,main="External Assets of Banks", xlab="External Assets", ylab="Number of Banks"
#,breaks=1000)
#,breaks=1000,xlim=c(0,4e+07))




loans <- read.csv("interbank.csv", header=F, sep=",")


numNodes = length(loans$V1)
numNodes

loans2 <- numeric(numNodes)

counter =1
for(i in 1:numNodes) {
    loans2[counter] <- sum(loans[i])
    counter = counter+1
}

hist(loans2,main="The Amount Owed to Banks", xlab="Amount Owed to Bank", ylab="Number of Banks"
,breaks=1000)




#loans <- read.csv("interbank.csv", header=F, sep=",")



#numNodes = length(loans$V1)


#loans2 <- numeric(numNodes)

#counter =1
#for(i in 1:numNodes) {
#    loans2[counter] <- sum(loans[i,])
#    counter = counter+1
#}


#hist(loans2,main="The Amount Owed by Banks", xlab="Amount Owed by Bank", ylab="Number of Banks"
#,breaks=1000)
#,breaks=1000,xlim=c(0,4e+07))














