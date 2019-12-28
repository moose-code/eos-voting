# Required imports
library(data.table)
library(mltools)
library(factoextra) 
library(cluster)
library(gridExtra)
library(data.table)


# Getting required data in
filename<-paste(path,"eos_voting_data.csv", sep="")
mydf<-read.csv(filename, header=TRUE,sep=",")
mydf[,3] <- as.character(mydf[,3])

# Analzing those account which vote
df_voters <- mydf[-which(mydf$producers == ""), ]
df_voters_relevant <- head(df_voters,200) # This chooses the number of voters in the dataset

#Checks percentage of votes covered in data set
sum(df_voters_relevant$staked) / sum(df_voters$staked)

mylist<- strsplit(df_voters_relevant[,3],",")
bps<- c("alohaeosprod", "atticlabeosb")

# generate a list of relevant block producers.
for (i in mylist){
  for (j in i){
    if(is.element(j,bps)){
    }else{
      bps <- c(bps, j)
    }
  }
}


##########################
##########################
# Creates a new dataframe df_mod which is suitable for clustering
df_mod <- df_voters_relevant[-c(2,3,4)]
df_mod[bps]<- NA
for (i in bps){
  df_mod[i] <- sapply(strsplit(df_voters_relevant[,3],","),FUN=function(x){is.element(i,x)})
}
rownames(df_mod) <- df_mod$voter_name
df_mod <- df_mod[-c(1)] # get rid of voter names column


##########################
##########################
# Creats a inverted data frame where bps are items and votes are features
mydf <- data.frame(t(sapply(strsplit(df_voters_relevant[,3],","),FUN=function(x){is.element('atticlabeosb',x)})))
for (i in bps){
  mydf<- rbind(mydf, data.frame(t(sapply(strsplit(df_voters_relevant[,3],","),FUN=function(x){is.element(i,x)}))))
}
mydf<- mydf[-c(1),]
rownames(mydf) <- colnames(df_mod)
colnames(mydf) <- rownames(df_mod)

# EXAMPLE
#                X1    X2    X3   X4    X5    X6    X7    X8    X9   X10   X11 .... -> number of voters (from strongest)
#alohaeosprod  TRUE FALSE FALSE TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE 
#atticlabeosb FALSE  TRUE  TRUE TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  
#argentinaeos  TRUE FALSE FALSE TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE 



# Creating a dissimiliarity matrix in preperation for clustering. 
dd <- daisy(mydf)
fviz_dist(dd)

fviz_dist(dd, show_labels = FALSE)+ labs(title = "clustering data")


km.res <- kmeans(mydf, 2, nstart = 50)

fviz_cluster(km.res, data = mydf,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","green","brown"), 
             ellipse.type = "euclid", 
             #labelsize = 0.3,
             #pointsize = 0.1,
             #choose.vars = c("GDP", "UrbanPopulation"),
             geom="point",
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             #labelsize = 0.3,
             ggtheme = theme_minimal()
)

pca.out<-prcomp(mydf)
pca.out
biplot(pca.out,scale = 0, cex=0.80)

plot1 <- fviz_contrib(pca.out, choice="var", axes = 1, top = 19)
plot2 <- fviz_contrib(pca.out, choice="var", axes = 2, top = 19, color = "lightgrey")
grid.arrange(plot1, plot2, ncol=2)


res.hc1 <- hclust(d = dd, method = "ward.D2")

cor(dd, cophenetic(res.hc1))

fviz_dend(res.hc1, k = 2, # Cut in four groups
          cex = 0.5, # label size
          #k_colors = "rickandmorty",
          #type = "circular",
          horiz = TRUE,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


