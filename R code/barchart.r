#plots bar charts
#equities <- read.csv("equities.csv", header=F, sep=",")

#df_e <- data.frame(equities)
#equities_vec  = as.numeric(df_e[1,])

#barplot(equities_vec, main="Equities of Banks",
#xlab="Banks", ylab="Equity")


#equities <- read.csv("externalAssets.csv", header=F, sep=",")

#df_e <- data.frame(equities)
#equities_vec  = as.numeric(df_e[1,])

#barplot(equities_vec, main="External Assets of Banks",
#xlab="Banks", ylab="External Assets")



#loans <- read.csv("interbank.csv", header=F, sep=",")


#numNodes = length(loans$V1)
#numNodes

#loans2 <- numeric(numNodes)

#counter =1
#for(i in 1:numNodes) {
#    loans2[counter] <- sum(loans[i])
#    counter = counter+1
#}


#barplot(loans2, main="The Amount Owed to Banks", xlab="Banks", ylab="Amount Owed to Bank")





loans <- read.csv("interbank.csv", header=F, sep=",")



numNodes = length(loans$V1)
numNodes

loans2 <- numeric(numNodes)

counter =1
for(i in 1:numNodes) {
    loans2[counter] <- sum(loans[i,])
    counter = counter+1
}


#barplot(loans2, main="The Amount Owed by Banks", xlab="Banks", ylab="Amount Owed by Bank")




















