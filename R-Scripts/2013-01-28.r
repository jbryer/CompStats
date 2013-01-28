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


