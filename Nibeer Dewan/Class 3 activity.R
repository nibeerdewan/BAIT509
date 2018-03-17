library(tidyverse)
library(ISLR)
library(knitr)

set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

#finding the distance
dat2<-mutate(dat,d=abs(x-0))

#subet to the 1st k rows: kNN
dat3<-arrange(dat2,d)[1:5,] #picking 5 nearest neighbors

y_predictk<-mean(dat3$y) #preducting y mean from kNN

#fix a window, say 0.1: loess
dat3sub<-filter(dat2,d<=1)
y_predictl<-mean(dat3sub$y)  #predicting y mean from loess


#########################
#3.2
##########################


xgrid <- seq(-5, 4, length.out=1000)

kNN_estimates <- map_dbl(xgrid, function(x){
  dat2<-mutate(dat,d=abs(dat$x-x))
  dat3<-arrange(dat2,d)[1:5,]
  yhat<-mean(dat3$y)
  return(yhat)
  ## YOUR CODE HERE FOR kNN
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for kNN from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
})



loess_estimates <- map_dbl(xgrid, function(x){
  ## YOUR CODE HERE FOR LOESS
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for loess from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
  
  dat2<-mutate(dat,d=abs(dat$x-x))
  dat3sub<-filter(dat2,d<=1)
  yhat<-mean(dat3sub$y)
  return(yhat)
})
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y), colour=my_accent) +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()
