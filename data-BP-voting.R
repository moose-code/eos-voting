# Jonathan Clark
# CLRJON005
# Voting analytics for EOS


path <- "/Users/johanthan/Code/eos-voting/"

# reading file in
filename<-paste(path,"voters2019-02-06_04-30.csv", sep="")
df<-read.csv(filename, header=TRUE,sep=",")

# Converting to chracter for later opertaion
df[,3] <- as.character(df[,3])

# This is used to find all account that have acutally voted.
df_voters <- df[-which(df$producers == ""), ]

# All who have staked but not voted.
df_nonvoters <- df[which(df$producers == ""), ]


# USed to calculate the fraction of the network controlled by just a few individuals
sum(head(df_voters$staked,10)) / sum(df_voters$staked)



sum(head(df_voters$staked,1))

# vote power distribution plot 

plot((head(df_voters$staked,500)/1000000), type = "b",cex = .5, ylab = "Number of votes (millions)" , ylim = c(0,12), xlab = "Top 500 accounts with the greatest voting power", main = "Voting power distribution amongst accounts")
#proxy voters
df_voters_proxy <- df_voters[-which(df_voters$proxy == ""), ]

#direct voters
df_voters_direct <- df_voters[which(df_voters$proxy == ""), ]

sum(df_voters_proxy$staked) / sum(df_voters$staked)
sum(df_voters_direct$staked) / sum(df_voters$staked)

sum(df_voters$staked)
# Percentage of voters who have voted via proxy. 
sum(df_voters_proxy$staked) / (sum(df_voters_direct$staked) + sum(df_voters_proxy$staked))


head(df_nonvoters,10)

str(df2)
str(df)
head(df)
df[,2]
plot(df[,2])
barplot(df[,2], xlim = 200)
df[2,3]
count.fields(df[2,3],sep = ",")

producers <- as.data.frame.list(df[,3] )

#count.fields(textConnection(df[6,3]), sep = ",")

distribution <- sapply(df_voters[,3], function(x) count.fields(textConnection(x), sep = ",") )

sapply(df_voters[1,3], function(x) count.fields(textConnection(x), sep = ",") )

distribution2 <-sapply(strsplit(df_voters[,3],","),FUN=function(x){length(x)})


df_voters$distribution <- distribution2

str(df_voters)

df_voters

tot<- aggregate(df_voters$staked ~ df_voters$distribution, df_voters, sum)

rep.int(1,nrow(df_voters))


df_voters$temp <- rep.int(1,nrow(df_voters))

tot2<- aggregate(df_voters$temp ~ df_voters$distribution, df_voters, sum)
tot2[,2]

names(tot2) <- c("bp_number", "votes")
names(tot) <- c("bp_number", "votes")
tot


tot[,2]

myvec <- tot[,2]
sum(myvec[20:30])
# vote distribution
# tot - eos vote data
library(ggplot2)
p<-ggplot(data=tot, aes(x=bp_number, y=votes)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + xlab("Number of block producers voted for")+ ylab("Total eos Votes") + ggtitle("Vote distribution - Actual EOS Votes")

p


# account distribution
# tot2 - account data
p<-ggplot(data=tot2, aes(x=bp_number, y=votes)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + xlab("Number of block producers voted for")+ ylab("Total accounts") + ggtitle("Vote distribution - Number of accounts")
p


tot2$votes[30]/ sum(tot2$votes)




tot2$votes[1]  / sum(tot2$votes)
sum(tot2$votes)

tot$votes[30]  / sum(tot$votes)

sum(myvec[20:30]) / sum(tot$votes)

sum(myvec[20:30]) / sum(myvec)

sum(myvec[20:30])


