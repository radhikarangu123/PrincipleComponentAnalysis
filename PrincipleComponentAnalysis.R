##################PCA Example###############################
winedata <- read.csv(file.choose())
View(winedata)
help("princomp")
winedata_new<-winedata[-1]
View(winedata_new)
attach(winedata_new)
cor(winedata_new)
#model Building
pcawineobj<-princomp(winedata_new,cor = TRUE,scores = TRUE,covmat = NULL)
summary(pcawineobj)
str(pcawineobj)
plot(pcawineobj)
# Comp.1 having highest importance (highest variance)
biplot(pcawineobj)
#pcawineObj$loadings
pcawineobj$scores[1:3]# Top 3 PCA Scores which represents the whole data

#considering top 3 pca's and bind them with mywinedata

winedata <- cbind(winedata,pcawineobj$scores[,1:3])
View(winedata)

#considering only pca values
clus_data<-winedata[,15:17]
View(clus_data)

#normalized the data
norm_clus<-scale(clus_data)# scale function used to normalized the data

View(norm_clus)
#finding diastance
dist1<-dist(norm_clus,method = "euclidean")
dist1

fit1<-hclust(dist1,method = "complete")
fit1
plot(fit1)#displaying dandrogram
groups<-cutree(fit1,5)#cutting dandrogram for 5 clusters

membership_1<-as.matrix(groups)
View(membership_1)

final1<-cbind(membership_1,winedata)
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean))
setwd("D://ExcelR Data//Assignments//PCA")
write.csv(final1,file="wine_clustering.csv",row.names = F,col.names = F)
getwd()
