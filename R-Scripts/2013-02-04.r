
setwd("~/Dropbox/School/Teaching/EPSY887 Spring 2013 Computational Statistics")
getwd()


#The c function
var1 <- c(1,2,3,4)
class(var1)
var2 <- c(1,2,'a','b')
class(var2)
var3 <- c(TRUE, TRUE, FALSE)
class(var3)
var4 <- c(var3, 1)
class(var4)
var4

#SPSS files we'll use the foreign
require(foreign)
help(package='foreign')
?read.spss
filtered <- read.spss('FILTERED.sav', to.data.frame=TRUE)
class(filtered)
str(filtered)
names(filtered)
options(max.print=15000)
options(digits=2)

head(names(filtered), n=30)
mycols <- c('mex01001','grs01021','ags0131','raso1242','int04336','int04339')
filtered2 <- filtered[,mycols]
cbind(mycols, mycols %in% names(filtered) )
mycols %in% names(filtered)
mycols <- c('mex01001','grs01021','ags01031','ras01242','int04336','int04339')
mycols %in% names(filtered)
cbind(mycols, mycols %in% names(filtered) )
filtered2 <- filtered[,mycols]
names(filtered2)
head(filtered2)
names(filtered2) <- c('id','gender','age','ethnicity','wj1','wj2')
names(filtered2)
str(filtered2)
filtered2$id
filtered2$id <- as.integer(filtered2$id)
table(filtered2$gender, useNA='ifany')
table(filtered2$gender, useNA='always')
filtered2$gender <- as.factor(as.character(filtered2$gender))

save(filtered, filtered2, mycols, file='filtered.rda')
load(file='filtered.rda')

?write.csv
write.csv(filtered2, file='filtered-edited.csv')
write.foreign(filtered2, 'filtered-edited.dat', 'filtered-edited.sps', package='SPSS')

require(gdata)
?read.xls
help(package='gdata')


#Let's use a smaller dataset
data(mtcars)
help(mtcars)
str(mtcars)
nrow(mtcars)
ncol(mtcars)
names(mtcars)
row.names(mtcars)

#Subsetting
head(mtcars)
head(mtcars, n=10)
tail(mtcars)
mtcars[1,]
mtcars[,1]
mtcars$mpg
mtcars[,c('mpg','cyl')]

mtcars[1:5,]
?sample
sample(1:nrow(mtcars), 5)
mtcars[sample(1:nrow(mtcars), 5), ]
set.seed(2112); mtcars[sample(1:nrow(mtcars), 5), ]

mtcars$cyl
table(mtcars$cyl, useNA='ifany')
table(mtcars$cyl, mtcars$gear, useNA='ifany')
table(mtcars$cyl, mtcars$gear, mtcars$am, useNA='ifany')

mtcars$am2 <- factor(mtcars$am, levels=c(0,1), labels=c('Automatic','Manual'))
table(mtcars$am2, useNA='ifany')
table(mtcars$cyl, mtcars$gear, mtcars$am2, useNA='ifany')

mtcars$am2 == 'Automatic'
mtcars.auto <- mtcars[ mtcars$am2 == 'Automatic',  ]
mtcars.manu <- mtcars[ mtcars$am2 != 'Automatic',  ]
nrow(mtcars.auto)
nrow(mtcars)

filtered2.1314 <- filtered2[filtered2$age >= 13, ]
head(filtered2.1314)

#Logical operators: ==, !=, >, <, >=, <=

table(mtcars$carb, useNA='ifany')
#Let's say we have a data error, the 8 should be a 6
mtcars[mtcars$carb == 8, ]
mtcars[mtcars$carb == 8, ]$carb
mtcars[mtcars$carb == 8, c('carb')]
mtcars[mtcars$carb == 8, c('carb','am2')]
#Change the value
mtcars[mtcars$carb == 8, ]$carb <- NA

is.na(mtcars$car)
mtcars[is.na(mtcars$carb), ]
mtcars[is.na(mtcars$carb), ]$carb <- 8


#### Side note on factors
myfactor <- factor(1:26, labels=letters)
str(myfactor)
levels(myfactor)
labels(myfactor)

agefactor <- factor(c(15, 18 , 20, 18))
table(agefactor)
class(agefactor)
as.integer(agefactor)
as.character(agefactor)
levels(agefactor) #used by as.character
labels(agefactor) #used by as.integer
as.integer(as.character(agefactor))

mygenderfactor <- factor(c(0,0,1,1,0), levels=c(0,1), labels=c('Female','Male'))
mygenderfactor
table(mygenderfactor)
as.integer(mygenderfactor) - 1
length(mygenderfactor)
mygenderfactor[1]
mygenderfactor[4]
1:3
mygenderfactor[1:3]
class(5:15)
mygenderfactor[c(1,3,5)]
?seq
seq(1, 20, by=2)
seq(2, 20, by=2)


filtered2[filtered2$wj1 == -99, ]$wj1 <- NA
filtered2[filtered2$wj2 == -99, ]$wj2 <- NA

table(filtered2$wj1, useNA='ifany')
table(filtered2$wj2, useNA='ifany')

recodecols <- c('wj1','wj2')
for(i in recodecols) {
	filtered2[ filtered2[,i] == -99, i] <- NA
}

table(is.na(filtered2$wj1))
prop.table(table(is.na(filtered2$wj1)))
prop.table(table(is.na(filtered2$wj1))) * 100


#Back to filtered2 dataset
#Descriptives
str(filtered2)
mean(filtered2$wj1, na.rm=TRUE)
median(filtered2$wj1, na.rm=TRUE)
sd(filtered2$wj1, na.rm=TRUE)
range(filtered2$wj1, na.rm=TRUE)
min(filtered2$wj1)
max(filtered2$wj1)
cor(filtered2[,c('wj1','wj2')], use='na.or.complete')
cor(filtered2$wj1, filtered2$wj2, use='na.or.complete')

table(filtered2$ethnicity, useNA='ifany')
filtered2$ethnicity <- as.factor(as.character(filtered2$ethnicity))

table(filtered2$gender, useNA='ifany')
filtered2$gender <- as.factor(as.character(filtered2$gender))

require(psych)
describe(filtered2$wj1)
?describe
?describeBy
describeBy(filtered2$wj1, group=filtered2$ethnicity)
describeBy(filtered2$wj1, group=filtered2$ethnicity, mat=TRUE)
describeBy(filtered2$wj1, group=list(filtered2$ethnicity, filtered2$gender), mat=TRUE)

mytable <- describeBy(filtered2$wj1, group=list(filtered2$ethnicity, filtered2$gender), mat=TRUE)
mytable <- mytable[, c('group1','group2','n','mean','sd')]
mytable
names(mytable) <- c('Ethnicity','Gender','n','mean','sd')
write.csv(mytable, 'results.csv')

summary(filtered2)
