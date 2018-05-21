#Setting Path
path<-'C:/Users/Sankalp/Desktop/DataScience Trinity/Statistical Modelling/'
setwd(path)

#Libraries Used
library("jsonlite")
library("ggplot2")
library("readr")
library("MCMCpack")
library("magrittr")
library("dplyr")

#Combined dataset with reviews and business
breview<- readRDS('final.rds')
str(breview)

# Business dataset
businessq2 <- stream_in(file("businessq2.json"))
businessq2 <- flatten(businessq2)

businessq <- subset(businessq2,city=='Toronto')
businessq1 <- businessq[,c(1,16,22:24,28,32,34:36)]
data1 <- breview[,c(-3,-5,-6,-13:-17,-20)]
data<- merge(data1,businessq1, by.x = "business_id", by.y = "business_id")
dim(data)

#Converting into Factors
#Converting variables into factors
columns_factor<- c(8,12,86:94)
data %<>%
  mutate_each_(funs(factor(.)),columns_factor)
str(data)
saveRDS(updated,'brq2.rds')


#Near to Zero variance check
library(caret)
x <-nearZeroVar(data,saveMetrics=TRUE)
x


#Missing Values Check
library(VIM)
e1 <-aggr(data)
summary(e1)

# we can see still Coatcheck and Smoking attributes have 83% of the data missing. Hence it 
# will be removed.

data <- data[,c(-93,-94)]
str(data)


#Imputing Missing Values
data$attributes.BusinessAcceptsCreditCards<-as.numeric(data$attributes.BusinessAcceptsCreditCards)
data$attributes.HasTV<-as.numeric(data$attributes.HasTV)
data$attributes.NoiseLevel<-as.numeric(data$attributes.NoiseLevel)
data$attributes.RestaurantsAttire<-as.numeric(data$attributes.RestaurantsAttire)
data$attributes.RestaurantsReservations<-as.numeric(data$attributes.RestaurantsReservations)
data$attributes.RestaurantsTableService<-as.numeric(data$attributes.RestaurantsTableService)
data$attributes.RestaurantsDelivery <- as.numeric(data$attributes.RestaurantsDelivery)
str(data)

y1 <-data$attributes.BusinessAcceptsCreditCards
data <- transform(data, y1 = ifelse(is.na(y1), median(y1, na.rm=TRUE), y1))
data$attributes.BusinessAcceptsCreditCards<- factor (data$y1)


y2 <- data$attributes.HasTV
data <- transform(data, y2 = ifelse(is.na(y2), median(y2, na.rm=TRUE), y2))
data$attributes.HasTV<- factor (data$y2)

y3 <- data$attributes.NoiseLevel
data <- transform(data, y3 = ifelse(is.na(y3), median(y3, na.rm=TRUE), y3))
data$attributes.NoiseLevel<- factor (data$y3)

y4 <- data$attributes.RestaurantsAttire
data <- transform(data, y4 = ifelse(is.na(y4), median(y4, na.rm=TRUE), y4))
data$attributes.RestaurantsAttire<- factor (data$y4)

data <- data[,c(-93,-94,-95,-96)]

y5 <- data$attributes.RestaurantsReservations
data <- transform(data, y5 = ifelse(is.na(y5), median(y5, na.rm=TRUE), y5))
data$attributes.RestaurantsReservations<- factor (data$y5)

y6 <- data$attributes.RestaurantsTableService
data <- transform(data, y6 = ifelse(is.na(y6), median(y6, na.rm=TRUE), y6))
data$attributes.RestaurantsTableService<- factor (data$y6)

y7 <- data$attributes.RestaurantsDelivery 
data <- transform(data, y7 = ifelse(is.na(y7), median(y7, na.rm=TRUE), y7))
data$attributes.RestaurantsDelivery <- factor (data$y7)


data <- data[,c(-93,-94,-95)]
str(data)


#Missing Data Check Again
e1 <-aggr(data)
saveRDS(data,'brdataq2.rds')



str(data)

#Load cleaned dataset
breview1<- readRDS('brdataq2.rds')
final <- breview1[,c(-1,-2,-7,-8,-9,-14:-18,-21:-23,-25:-32,-35:-46,-48:-57,-59:-70,-72:-85)]

final$attributes.RestaurantsPriceRange2 <- as.numeric(final$attributes.RestaurantsPriceRange2)
final$attributes.BusinessAcceptsCreditCards<-as.numeric(final$attributes.BusinessAcceptsCreditCards)
final$attributes.HasTV<-as.numeric(final$attributes.HasTV)
final$attributes.NoiseLevel<-as.numeric(final$attributes.NoiseLevel)
final$attributes.RestaurantsAttire<-as.numeric(final$attributes.RestaurantsAttire)
final$attributes.RestaurantsReservations<-as.numeric(final$attributes.RestaurantsReservations)
final$attributes.RestaurantsTableService<-as.numeric(final$attributes.RestaurantsTableService)
final$attributes.RestaurantsDelivery <- as.numeric(final$attributes.RestaurantsDelivery)
str(final)



#One hot encoding
final1 <- breview1[,c(-1,-2,-7,-8,-9,-13:-85)]
library(dummies)

final1 <- dummy.data.frame(final1, names=c("attributes.RestaurantsPriceRange2"), sep="_")
final1 <- dummy.data.frame(final1, names=c("attributes.BusinessAcceptsCreditCards"), sep="_")
final1 <- dummy.data.frame(final1, names=c("attributes.HasTV"), sep="_")
final1 <- dummy.data.frame(final1, names=c("attributes.NoiseLevel"), sep="_")
final1 <- dummy.data.frame(final1, names=c("attributes.RestaurantsAttire"), sep="_")
final1 <- dummy.data.frame(final1, names=c("attributes.RestaurantsReservations"), sep="_")
final1 <- dummy.data.frame(final1, names=c("attributes.RestaurantsTableService"), sep="_")
final1 <- dummy.data.frame(final1, names=c("attributes.RestaurantsDelivery"), sep="_")

names(final1)

#--------------------------Correlation between Numeric features------------------------#
#extra..

pdf(file="output4.pdf")
corr_matrix<-cor(final[,c(1:16)])
library(corrplot)
corrplot(corr_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
dev.off()


#-------------------------Model Building-----------------#

lm1 <- lm(stars.y~ ., data=final)
summary(lm1)

fit<- MCMCregress(final$stars.y~ final$stars.x + final$useful + final$cool + final$funny + final$review_count + final$attributes.RestaurantsPriceRange2 +final$attributes.BusinessAcceptsCreditCards + final$attributes.HasTV + final$attributes.NoiseLevel + final$attributes.RestaurantsAttire + final$attributes.RestaurantsReservations + final$attributes.RestaurantsTableService +final$attributes.RestaurantsDelivery +final$Food +final$Canadian..New. + final$Indian + final$Cafes + final$Italian +final$Korean + final$Vegan + final$Caribbean + final$Noodles)
summary(fit)
pdf(file="output5.pdf")
plot(fit)
dev.off()

pdf(file="output6.pdf")
cumuplot(fit)
dev.off()

beta_mean <- apply(fit, 2, mean)

df_dummy <- final[, -5]
df_dummy <- cbind(1, as.matrix(df_dummy))

dim(df_dummy)
beta_mean[-24]
pred_fit <- df_dummy %*% as.matrix(beta_mean[-24])

plot(pred_fit,final$stars.y)
RMSE(pred_fit,final$stars.y)
sum((pred_fit - final$stars.y)^2) 



#------------------------------------------------------------

fit1<- MCMCregress(final$stars.y~ final$stars.x + final$useful + final$cool + final$funny + final$review_count + final$attributes.RestaurantsPriceRange2 + final$attributes.NoiseLevel + final$Food +final$Canadian..New. + final$Indian + final$Cafes + final$Italian +final$Korean + final$Vegan + final$Caribbean + final$Noodles)
summary(fit1)
pdf(file="output5.pdf")
plot(fit)
dev.off()

pdf(file="output6.pdf")
cumuplot(fit)
dev.off()

beta_mean1 <- apply(fit1, 2, mean)
beta_mean1_df <- as.data.frame(beta_mean1)

df_dummy1 <- final[, c(-5,-17,-18,-20:-23)]
df_dummy1 <- cbind(1, as.matrix(df_dummy1))

dim(df_dummy1)

pred_fit1 <- df_dummy1 %*% as.matrix(beta_mean1[-18])

plot(pred_fit1,final$stars.y)

sum((pred_fit1 - final$stars.y)^2) 
