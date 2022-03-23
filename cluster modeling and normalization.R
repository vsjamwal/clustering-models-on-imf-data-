corrplot
corrplot
library(readxl)
CountryData <- read_excel("CountryData.xlsx")
View(CountryData)
install.packages("gdata")
request(gdata)
reqire(gdata)
require(gdata)
# getting the excel data  in


df = read.xls(("CountryData.xlsx"), sheet = 1 , header = TRUE)
df<-df[c(1:21)]
df
df
head(df,6)
# Data analysis and cleansing
# get the insight of our data
str(df)



# it mean 192 observations with 21 variables
# box plot of the data

par(cex.axis=0.52)

boxplot(scale(df[c(-1,-2)]),las = 2)


#deal with null valued

require(caret)
install.packages("caret")
require(caret)

#we impute missing values with random forecast

imputational_model = preProcess(x = df [, -c(1,2)],method = "bagImpute")
imputated_data = predict(object = imputational_model,newdata = df[,-c(1,2)])


#adding countries name to the rows

rownames(imputated_data)<-df[,2]

#checking out this fresh imputed data and find all null values 

head(imputated_data)
str(imputated_data)
imputated_data = predict(object = imputational_model,newdata = df[,-c(1,2)])
apply(imputated_data,2,function(x) sum(is.na(x)))

# ploting the corrleation matrix 




# correlation 



#principle component analysis 

pca.out<- prcomp(imputated_data,scale = TRUE)
pca.out
biplot(pca.out, scale = 0 , cex = .75)
vexplained <- as.data.frame(pca.out$sdev^2/sum(pca.out$sdev^2))
vexplained <- cbind(c(1:19), vexplained, cumsum(vexplained[,1]))
colnames(vexplained) <- c("No_of_Principle_Cpmponents", "Individual_Variance_expalined", "Cumulative Variance")
vexplained



# Scatter plot
imputated_data
str(imputated_data)
a <-apply(imputated_data,2,mean)
s <-apply(imputated_data,2,sd)
z = imputated_data
z = scale(z,m,s)


##calculate the Euclidean Distance
distance  = dist(z)
z
distance 
print(distance, digits = 3)


# cluster Dendogram
plot(hc.c, hang = -1)

# cluster with average linkage 
hc.a = hclust(distance, method = "average" )
plot(hc.a, hang = -1)

# cluster Membership 
member.c = cutree(hc.c,5)
member.a = cutree(hc.a,5)
table(member.c, member.a)

# cluster means 
 x = aggregate(z,list(member.c),mean)

 
#silhoutte plot 
 
 library(cluster)
 plot(silhouette(cutree(hc.c,3),distance))
 
 #k mean clustering 
 kc = kmeans(z,50)
 kc
 
 # so kmean divides it into 3 cluster of sizes 58,78,56
 #cluster vector shows which country belong to which cluster 
 
 plot(BX.KLT.DINV.WD.GD.ZS~EG.ELC.ACCS.ZS, z, col = kc$cluster)
 