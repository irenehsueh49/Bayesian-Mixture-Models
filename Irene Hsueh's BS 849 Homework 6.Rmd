---
title: "Irene Hsueh's BS 849 Homework 6"
author: "Irene Hsueh"
date: "3/14/2022"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(rjags)
library(coda)
library(formatR)
set.seed(1234)
```

# Longitudinal Trajectories Dataset 1 
```{r}
data1_beta0 <- c(2, 5)
data1_beta1 <- c(5, 7)
data1_alpha <- rnorm(500, 0, 1)
data1_distribution = 1 + rbinom(n=500, size=1, p=0.3)
table(data1_distribution)

data1_x <- c(1:6)
data1_y <- c()
data1_id <- c()

for(i in 1:500){
  data1_id <- rbind(data1_id, rep(i, 6))
  data1_y <- rbind(data1_y, c(data1_beta0[data1_distribution[i]] +
                                data1_beta1[data1_distribution[i]]*data1_x + 
                                data1_alpha[i] + rnorm(6,0,1) 
                              )
                   )
}

trajectory_data1 <- list(n_subjects=500, id=data1_id, x=data1_x, y=data1_y)
```



# Longitudinal Trajectories Dataset 2
```{r}
data2_beta0 <- c(2, 5, 6)
data2_beta1 <- c(5, 7, 2)
data2_alpha <- rnorm(500, 0, 1)

data2_generator = rmultinom(n=500, size=1, prob=c(0.5, 0.3, 0.2))
data2_distribution <- which(data2_generator==1, arr.ind=TRUE)[,1]
table(data2_distribution)

data2_x <- c(1:6)
data2_y <- c()
data2_id <- c()

for(i in 1:500){
  data2_id <- rbind(data2_id, rep(i, 6))
  data2_y <- rbind(data2_y, c(data2_beta0[data2_distribution[i]] +
                                data2_beta1[data2_distribution[i]]*data2_x + 
                                data2_alpha[i] + rnorm(6,0,1) 
                              )
                   )
}

trajectory_data2 <- list(n_subjects=500, id=data2_id, x=data2_x, y=data2_y)
```



# Plots of Trajectory Datasets
```{r}
#Plot of Trajectory Dataset 1
plot(data1_x, data1_y[1,], 
     ylim = c(0, 50), 
     type = "l",
     xlab = "Time", 
     ylab = "Outcome", 
     main = "Trajectory Dataset 1"
     )
for(i in 1:500){
  lines(data1_x, data1_y[i,],
        col = ifelse(data1_distribution[i]==1, "deepskyblue", "hotpink"))
}



#Plot of Trajectory Dataset 2
plot(data2_x, data2_y[1,], 
     ylim = c(0, 50), 
     type = "l",
     xlab = "Time", 
     ylab = "Outcome", 
     main = "Trajectory Dataset 2"
     )
for(i in 1:500){
  lines(data2_x, data2_y[i,],
        col = if (data2_distribution[i]==1) {"deepskyblue"}
              else if (data2_distribution[i]==2) {"hotpink"}
              else ("springgreen")
        )
}
```



# Longitudinal Trajectories Two Clusters 
```{r}
cluster2_bugs <- 
"model  
  {for(i in 1:n_subjects){
    epsilon[i] ~ dbin(theta, 1)
    w[i] <- 1 + epsilon[i]
      
    for(j in 1:6){
      y[i,j] ~ dnorm(mu[i,j], tau)
      mu[i,j] <- b0[w[i]] + b1[w[i]]*x[j]  
    }
  }

#Cluster Effects
for(i in 1:2){    
  b1[i] ~ dnorm(0,0.01)
}

#Fixed Effects  
b0[1] ~ dnorm(0,0.001)
b0[2] <- b0[1] + delta
delta ~ dgamma(1,1)

#Variance Components
theta ~ dbeta(1,1)
tau ~ dgamma(1,1)
}
"

#Dataset 1 
cluster2_model1 <- jags.model(textConnection(cluster2_bugs),
                             data=trajectory_data1, n.adapt=1000)
cluster2_gibbs1 <- update(cluster2_model1, n.iter=1000)
cluster2_test1 <- coda.samples(cluster2_model1, 
                               c("b0", "b1", "theta", "epsilon"), n.iter=1000)
summary(cluster2_test1[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")])


#Dataset 2
cluster2_model2 <- jags.model(textConnection(cluster2_bugs),
                             data=trajectory_data2, n.adapt=1000)
cluster2_gibbs2 <- update(cluster2_model2, n.iter=1000)
cluster2_test2 <- coda.samples(cluster2_model2, 
                               c("b0", "b1", "theta", "epsilon"), n.iter=1000)
summary(cluster2_test2[, c("b0[1]","b0[2]","b1[1]","b1[2]","theta")])
```



# Diagnostics of 2-Cluster Model on Dataset 1 
```{r}
#Misclassification Errors
cluster2_simulated1 <- as.data.frame(as.matrix(cluster2_test1))
cluster2_residuals1 <- 1 + cluster2_simulated1[, grep("epsilon",
                                                      names(cluster2_simulated1))]
cluster2_classification1 <- apply(cluster2_residuals1, 2, median)
table(data1_distribution, cluster2_classification1)



#Traceplots 
traceplot(cluster2_test1[, c("b0[1]", "b0[2]", "b1[1]", "b1[2]", "theta")], col="hotpink")



#Plot at Each Time Point
cluster2_data1_group1 <- which(apply(cluster2_residuals1, 2, median)==1)
cluster2_data1_group2 <- which(apply(cluster2_residuals1, 2, median)==2)
for(i in 1:6){
  plot(c(1:500)[cluster2_data1_group1],
       trajectory_data1$y[,i][cluster2_data1_group1],
       ylim=c(0, 50), 
       pch=19, col="deepskyblue", xlab="Index", ylab="Outcome")
  points(c(1:500)[cluster2_data1_group2],
         trajectory_data1$y[,i][cluster2_data1_group2],
         pch=19, col="hotpink")
}



#Plot of Cluster Labeling for Trajectory Dataset 1
plot(data1_x, data1_y[1,], 
     ylim = c(0, 50), 
     type = "l",
     xlab = "Time", 
     ylab = "Outcome", 
     main = "Two-Cluster Labeling for Trajectory Dataset 1"
     )
for(i in 1:500){
  lines(data1_x, data1_y[i,],
        col = ifelse(cluster2_classification1[i]==1, "deepskyblue", "hotpink"))
}
```



# Diagnostics of 2-Cluster Model on Dataset 2 
```{r}
#Misclassification Errors
cluster2_simulated2 <- as.data.frame(as.matrix(cluster2_test2))
cluster2_residuals2 <- 1 + cluster2_simulated2[, grep("epsilon",
                                                      names(cluster2_simulated2))]
cluster2_classification2 <- apply(cluster2_residuals2, 2, median)
table(data2_distribution, cluster2_classification2)



#Traceplots 
traceplot(cluster2_test2[, c("b0[1]", "b0[2]", "b1[1]", "b1[2]", "theta")], col="mediumpurple")



#Plot at Each Time Point
cluster2_data2_group1 <- which(apply(cluster2_residuals2, 2, median)==1)
cluster2_data2_group2 <- which(apply(cluster2_residuals2, 2, median)==2)
for(i in 1:6){
  plot(c(1:500)[cluster2_data2_group1],
       trajectory_data2$y[,i][cluster2_data2_group1],
       ylim=c(0, 50), 
       pch=19, col="deepskyblue", xlab="Index", ylab="Outcome")
  points(c(1:500)[cluster2_data2_group2],
         trajectory_data2$y[,i][cluster2_data2_group2],
         pch=19, col="hotpink")
}



#Plot of Cluster Labeling for Trajectory Dataset 2
plot(data2_x, data2_y[1,], 
     ylim = c(0, 50), 
     type = "l",
     xlab = "Time", 
     ylab = "Outcome", 
     main = "Two-Cluster Labeling for Trajectory Dataset 2"
     )
for(i in 1:500){
  lines(data2_x, data2_y[i,],
        col = ifelse(cluster2_classification2[i]==1, "deepskyblue", "hotpink"))
}
```



# Longitudinal Trajectories Three Clusters 
```{r}
cluster3_bugs <- 
"model  
  {for(i in 1:n_subjects){
    epsilon[i] ~ dcat(theta[])
    
    for(j in 1:6){
      y[i,j] ~ dnorm(mu[i,j], tau)
      mu[i,j] <- b0[epsilon[i]] + b1[epsilon[i]]*x[j]
    }
  }
  
#Prior Distribution
theta[1:3]~ddirch(gamma[])

#Cluster and Fixed Effects
  for(i in 1:3){    
    b0[i] ~ dnorm(0,0.01)
    b1[i] ~ dnorm(0,0.01)
    gamma[i] <-1
}

#Variance Component
tau ~ dgamma(1,1)
}
"


#Dataset 1 
cluster3_model1 <- jags.model(textConnection(cluster3_bugs),
                             data=trajectory_data1, n.adapt=1000)
cluster3_gibbs1 <- update(cluster3_model1, n.iter=1000)
cluster3_test1 <- coda.samples(cluster3_model1, 
                               c("b0", "b1", "theta", "epsilon"), n.iter=1000)
summary(cluster3_test1[, c("b0[1]", "b0[2]", "b0[3]", 
                           "b1[1]", "b1[2]", "b1[3]", 
                           "theta[1]", "theta[2]", "theta[3]")])


#Dataset 2 
cluster3_model2 <- jags.model(textConnection(cluster3_bugs),
                             data=trajectory_data2, n.adapt=1000)
cluster3_gibbs2 <- update(cluster3_model2, n.iter=1000)
cluster3_test2 <- coda.samples(cluster3_model2, 
                               c("b0", "b1", "theta", "epsilon"), n.iter=1000)
summary(cluster3_test2[, c("b0[1]", "b0[2]", "b0[3]", 
                           "b1[1]", "b1[2]", "b1[3]", 
                           "theta[1]", "theta[2]", "theta[3]")])
```



# Diagnostics of 3-Cluster Model on Dataset 1 
```{r}
#Misclassification Errors
cluster3_simulated1 <- as.data.frame(as.matrix(cluster3_test1))
cluster3_residuals1 <- cluster3_simulated1[, grep("epsilon",
                                                  names(cluster3_simulated1))]
cluster3_classification1 <- apply(cluster3_residuals1, 2, median)
table(data1_distribution, cluster3_classification1)



#Traceplots 
traceplot(cluster3_test1[, c("b0[1]", "b0[2]", "b0[3]", 
                             "b1[1]", "b1[2]", "b1[3]", 
                             "theta[1]", "theta[2]", "theta[3]")],
          col="deepskyblue")


#Plot of Trajectory Dataset 1
plot(data1_x, data1_y[1,], 
     ylim = c(0, 50), 
     type = "l",
     xlab = "Time", 
     ylab = "Outcome", 
     main = "Three-Cluster Labeling for Trajectory Dataset 1"
     )
for(i in 1:500){
  lines(data1_x, data1_y[i,],
        col = if (cluster3_classification1[i]==1) {"deepskyblue"}
              else if (cluster3_classification1[i]==2) {"hotpink"}
              else ("springgreen")
        )
}
```



# Diagnostics of 3-Cluster Model on Dataset 2 
```{r}
#Misclassification Errors
cluster3_simulated2 <- as.data.frame(as.matrix(cluster3_test2))
cluster3_residuals2 <- cluster3_simulated2[, grep("epsilon",
                                                  names(cluster3_simulated2))]
cluster3_classification2 <- apply(cluster3_residuals2, 2, median)
table(data2_distribution, cluster3_classification2)



#Traceplots 
traceplot(cluster3_test1[, c("b0[1]", "b0[2]", "b0[3]", 
                             "b1[1]", "b1[2]", "b1[3]", 
                             "theta[1]", "theta[2]", "theta[3]")],
          col="springgreen")



#Plot of Trajectory Dataset 2
plot(data2_x, data2_y[1,], 
     ylim = c(0, 50), 
     type = "l",
     xlab = "Time", 
     ylab = "Outcome", 
     main = "Three-Cluster Labeling for Trajectory Dataset 2"
     )
for(i in 1:500){
  lines(data2_x, data2_y[i,],
        col = if (cluster3_classification2[i]==1) {"deepskyblue"}
              else if (cluster3_classification2[i]==2) {"hotpink"}
              else ("springgreen")
        )
}
```












