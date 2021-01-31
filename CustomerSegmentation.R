library(ranger)
library(caret)
library(data.table)
library(dplyr)
setwd("/Users/Soorya Suresh/Desktop/Data Analysis")
df<-read.csv("/Users/Soorya Suresh/Desktop/Data Analysis/customer-segmentation-dataset/customer-segmentation-dataset/Mall_Customers.csv")
#explore data
head(df,10)
names(df)
tail(df,10)
dim(df)
glimpse(df)
range(df$Age)
range(df$Gender)
range(df$Annual.Income..k..)
range(df$Spending.Score..1.100.)
summary(df)
summary(df$Age)
summary(df$Annual.Income..k..)
summary(df$Spending.Score..1.100.)
sd(df$Age)
sd(df$Annual.Income..k..)
sd(df$Spending.Score..1.100.)
table(df$Age)
table(df$Annual.Income..k..)
table(df$Spending.Score..1.100.)
#create a barplot to show gender distribution
counts<-table(df$Gender)
par("mar")
par(mar=c(1,1,1,1))
par(mar = c(5,5,3,1))
barplot(counts,main="Gender Distribution",xlab="Gender",ylab="Count",legend=rownames(counts),col=c("darkblue","red"))
#create a piechart to show gender distribution
library(plotrix)
pct<-round(counts/sum(counts)*100)
lbls<-paste(names(counts),pct,"%",sep=" ")
pie3D(counts,labels=lbls,explode=0.1,main="Gender Distribution")
#histogram to see frequency of customer ages
hist(df$Age,main="Histogram of Ages",xlab="Age",border="blue",col="green",labels = TRUE)
#create boxplot
boxplot(df$Age,data=df,main="Boxplot of Ages")
#create histogram for annual income
hist(df$Annual.Income..k..,main="Histogram of Annual Income",xlab="Annual Income",border="blue",col="green",labels=TRUE)
#density plot for annual income
d<-density(df$Annual.Income..k..)
plot(d,main="Density of Annual Income")
polygon(d,col="red",border="blue")
#boxplot and historgram of spending score
boxplot(df$Spending.Score..1.100.,data=df, horizontal=TRUE,main="Boxplot of Spending Score")
hist(df$Spending.Score..1.100.,main="Histogram of Spending Score",xlab="Spending Score",border="blue",col="green",labels=TRUE)
#use elbow method to find cluster
library(purrr)
set.seed(123)
wss<-function(k) {
  kmeans(df[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd")$tot.withinss
}
k.values<-1:10
wss_values<-map_dbl(k.values,wss)

plot(k.values,wss_values,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squeares",main="Elbow Method")
#4 clusters
#visualize the clustering
set.seed(123)
res.km<-kmeans(scale(df[,3:5]),4,nstart=100)
res.km$cluster
library(factoextra)
fviz_cluster(res.km,data=df[,3:5],palette="npg",geom="point",ellipse.type="convex",ggtheme=theme_bw())
#manipulate the visualization
res.pca<-prcomp(df[,3:5],scale=FALSE)
summary(res.pca)
ind.coord<-as.data.frame(get_pca_ind(res.pca)$coord)
ind.coord$cluster<-factor(res.km$cluster)
eigenvalue<-round(get_eigenvalue(res.pca),1)
variance.percent<-eigenvalue$variance.percent
library(ggpubr)
ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)
