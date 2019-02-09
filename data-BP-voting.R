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

#proxy voters
df_voters_proxy <- df_voters[-which(df_voters$proxy == ""), ]

#direct voters
df_voters_direct <- df_voters[which(df_voters$proxy == ""), ]

sum(df_voters_proxy$staked)
sum(df_voters_direct$staked)

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

count.fields(textConnection(df[6,3]), sep = ",")


producers
head(producers)


df[,3] <- as.character(df[,3])

df[,3]
str(df)
df[1,3]
