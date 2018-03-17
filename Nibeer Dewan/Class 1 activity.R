library(dplyr)
library(tidyverse)

#1
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

dat<-genreg(100)

#2
#mutate(dat,










gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x-0.2)))
  tibble(x=x, y=y)
}

dat2<-gencla(1000)

dat2<-mutate(dat2,
             yhat=sapply(x,function(x_)
               if(x_<0) "C" else "B"))

#X=1:
pB <- 0.8/(1+exp(-1))
pA<-0.2
pC<-1-pB-pA

#X=-2:
pB <- 0.8/(1+exp(2))
pA<-0.2
pC<-1-pB-pA

#X=0:
pB <- 0.8/(1+exp(0))
pA<-0.2
pC<-1-pB-pA

#if x<0, C
#if x>0, B
#if x=0, don't know because mode is not fixed


