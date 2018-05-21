
#Setting Path
path<-'C:/Users/Sankalp/Desktop/DataScience Trinity/Statistical Modelling/'
setwd(path)

#Libraries Used
library("jsonlite")
library("ggplot2")
library("readr")
library("MCMCpack")

#Combined dataset with reviews and business
breview<- readRDS('final.rds')
View(breview)

#Toronto Business dataset
business <- stream_in(file("business_open_Toronto.json"))
business <- flatten(business)

#Restaurants with No of reviews greater than 15
business_new <- subset(business,review_count>15)
names(business_new)

#Neighbourhood with Stars
nstar1<- business_new[, c("neighborhood","stars")]
nstar1 <- subset(nstar1,neighborhood!='Downsview')
nstar1 <- subset(nstar1,neighborhood!='West Don Lands')
dim(nstar1)

nstar1$neighborhood <- factor(nstar1$neighborhood)
nlevels(nstar1$neighborhood)


#Missing Values Check
library(VIM)
e1 <-aggr(nstar1)
summary(e1)

#Boxplot of Neighborhood and star
ggplot(nstar1) + geom_boxplot(aes(x = reorder(neighborhood, stars,median), stars, fill = reorder(neighborhood, stars,median)), show.legend=FALSE)



#Number of restaurants in a neighborhood
ggplot(nstar1, aes(x = reorder(neighborhood, neighborhood, length))) + stat_count()
as.data.frame(table(business_new$neighborhood))

#Number of reviews for each neighborhood
aggregate(business$review_count,by=list(business$neighborhood),FUN=sum)
aggregate(business_new$review_count,by=list(business_new$neighborhood),FUN=sum)

#Number of reviews for each restaurant
restreview <- business_new[, c("name","neighborhood","review_count","stars")]
unique(restreview$neighborhood)

#Restaurants with 15 or less than 15 reviews
business_15 <- subset(business,review_count<16)
NROW(business_15)
ggplot(business_15, aes(x=stars, y=review_count)) + geom_point()

#Number of reviews for each restaurant
restreview15 <- business_15[, c("name","neighborhood","review_count","stars")]



# Stars Distribution

ggplot(business, aes(stars)) + stat_bin()


ggplot(data.frame(size = tapply(nstar1$stars, nstar1$neighborhood, length), 
                  mean_score = tapply(nstar1$stars, nstar1$neighborhood, mean)), 
                  aes(size, mean_score)) + geom_point()

star_mean <-aggregate(business_new$stars, list(business_new$neighborhood), mean)



# Gibbs Sampling
compare_m_gibbs <- function(y, ind, maxiter = 5000)
{
  
  ### weakly informative priors
  a0 <- 2.5 ; b0 <- 3 ## tau_w hyperparameters
  eta0 <- 1/2 ; t0 <- 3 ## tau_b hyperparameters
  mu0<-3.5 ; gamma0 <- 1
  ###
  
  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  an <- a0 + sum(n_m)/2
  ###
  
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    bn <- b0 + ss/2
    tau_w <- rgamma(1, an, bn)
    
    #sample a new value of mu
    gammam <- m * tau_b + gamma0
    mum <- (mean(theta) * m * tau_b + mu0 * gamma0) / gammam
    mu <- rnorm(1, mum, 1/ sqrt(gammam)) 
    
    # sample a new value of tau_b
    etam <- eta0 + m/2
    tm <- t0 + sum((theta-mu)^2)/2
    tau_b <- rgamma(1, etam, tm)
    
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}

fit2 <- compare_m_gibbs(nstar1$stars, nstar1$neighborhood)
apply(fit2$params, 2, mean)
apply(fit2$params, 2, sd)
mean(1/sqrt(fit2$params[, 2]))
sd(1/sqrt(fit2$params[, 2]))

theta_hat <- apply(fit2$theta, 2, mean)
ggplot(data.frame(size = tapply(nstar1$stars, nstar1$neighborhood, length), theta_hat = theta_hat), aes(size, theta_hat)) + geom_point()

fit <- as.mcmc(fit2$params)
plot(fit)
acf(fit)
raftery.diag(fit)