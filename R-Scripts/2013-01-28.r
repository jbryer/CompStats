setwd("~/Dropbox/School/Teaching/EPSY887 Spring 2013 Computational Statistics/R-Scripts")

#Load packages. See Setup.r for installing packages.
require(psych)
require(pisa)

data(package='pisa')
data(pisa.catalog.student)
data(pisa.student)

names(pisa.catalog.student)
names(pisa.student)

View(cbind(names(pisa.catalog.student), pisa.catalog.student), 'Student Catalog')

pisa.student$math <- (pisa.student$PV1MATH + pisa.student$PV2MATH + pisa.student$PV3MATH +
	pisa.student$PV4MATH + pisa.student$PV5MATH)
pisa.student$math <- apply(pisa.student[,c('PV1MATH','PV2MATH','PV3MATH','PV4MATH','PV5MATH')], 1, mean)

#NOTE: In complex survey designs such as PISA, it is not correct to use the mean of scores.
#      We will discuss in a few weeks how to analyze this type of data using the survey package.
mean(pisa.student$math)
sd(pisa.student$math)
median(pisa.student$math)
summary(pisa.student$math)
fivenum(pisa.student$math)
quantile(pisa.student$math, seq(0,1,.2))
describe(pisa.student$math)
mathByCnt <- describeBy(pisa.student$math, group=pisa.student$CNT, mat=TRUE)
mathByCnt[order(mathByCnt$mean), c('group1','mean','sd','n','median')]


#### We Googled "Mixed effects in R" 
install.packages('nlme')
require(nlme)
search()
ls(2)
ls( "package:nlme")
help(package='nlme')

#NA vs NULL
myvar <- 'Hello'
myvar
length(myvar)
class(myvar)
myvar <- NA
myvar
myvar <- NULL
myvar
rm(myvar)

myvar <- 1/9999
myvar
myvar == 1/9999

myvar1 <- as.integer(2112)
myvar2 <- 3.14
class(myvar1)
class(myvar2)
myvar3 <- c(myvar1, myvar2)
myvar3
class(myvar3)
myvar4 <- c(myvar3, 'Hello')
myvar4
class(myvar4)
str(myvar4)
str(myvar3)

names(myvar4) <- c('a','b','c')
names(myvar4)
myvar4
myvar4[2]
myvar4['b']

myvar5 <- c(var1='Hello', var2='World')
myvar5
names(myvar5)

