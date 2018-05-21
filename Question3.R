path<-'C:/Users/Sankalp/Desktop/DataScience Trinity/Statistical Modelling/'
setwd(path)

#Libraries Used
library("jsonlite")
library("ggplot2")
library("readr")

business <- stream_in(file("business_open_Toronto.json"))
business <- flatten(business)

# Separating categories from the restaurant types
bus_cat_tidy <- cbind(business[1, ]$business_id, business[1, ]$categories[[1]])
for(i in 2:nrow(business)) bus_cat_tidy <- rbind(bus_cat_tidy, cbind(business[i, ]$business_id, business[i, ]$categories[[1]]))

#Removing Restaurants type cataegory from Restaurant types
cat_names <- names(sort(table(bus_cat_tidy[, 2]), decreasing = TRUE))[2:10] 
cat_bus_ind_mat <- t(sapply(tapply(bus_cat_tidy[, 2], bus_cat_tidy[, 1], function(y) cat_names %in% y), function(x) x*1)) 
nrow(cat_bus_ind_mat)
# Gives the category Names back to The table
colnames(cat_bus_ind_mat) <- cat_names


df_cat <- data.frame(ind = rownames(cat_bus_ind_mat), cat_bus_ind_mat)
business_merge <- merge(business, df_cat, by.x = "business_id", by.y = "ind")






# this to get last 9 categories 
categories <- business_merge[,(ncol(business_merge)-8):ncol(business_merge)]

#categories$neighborhood <- as.numeric(as.factor(categories$neighborhood))


##################################
library("mclust")
library("BayesLCA")

bic_values <- list()

for(i in 1:length(cat_names)) {
  fit <- blca.em(categories, i)
  bic_values[[i]] <-fit$BIC
}

fit3 <- blca.em(categories, 3, restarts = 20)  
fit4 <- blca.em(categories, 4, restarts = 25) 
fit5 <- blca.em(categories, 5, restarts = 25) 
fit7 <- blca.em(categories, 7, restarts = 30) 
 

BIC3 <- fit3$BIC
BIC4 <- fit4$BIC
BIC5 <- fit5$BIC
BIC7 <- fit7$BIC



plot(fit4, which = 1)

# Convergence
plot(fit4, which = 5)
neighborhood <-as.factor(df_bus_TO$neighborhood) 

table(MAP(Zscore(categories,fit3))) 
# Find the class probalitlites for each data points

temp <- table(MAP(Zscore(categories, fit4)),business$neighborhood)
temp <- as.data.frame.array(temp)
# Assign each data point to a class based on the maximum posterio