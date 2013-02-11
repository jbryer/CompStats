setwd("~/Dropbox/School/Teaching/EPSY887 Spring 2013 Computational Statistics")
getwd()

require(psych)
require(ggplot2)

#### For Bruce
install.packages('~/Dropbox/Bryer Dissertation/fromDudek/bcdstats_1.0.tar.gz')
require(devtools)
install('~/Dropbox/Bryer Dissertation/fromDudek/bcdstats')
require(bcdstats)
?pgchisq
pgchisq(3.84,df=1)
?qgchisq
qgchisq(.05,df=1)

#NAs
test <- rnorm(100, mean=1, sd=.5)
hist(test)
mean(test)
test[10] <- NA
head(test, n=15)
mean(test)
mean(test, na.rm=TRUE)
?cor
set.seed(2)
test2 <- sample(c('Male','Female'), 100, replace=TRUE)
table(test2, useNA='ifany')
table(test2, useNA='always')
table(test2)
table(test2, useNA='ifany')
test2[10] <- NA
head(test2, n=15)
table(test2, useNA='always')
table(test2, useNA='ifany')
table(test2, useNA='no')

#### Sorting data
test <- c('s','t','a','t','i','s','t','i','c','s')
test
length(test)
order(test)
length(order(test))
test[order(test)]

mtcars.mpg <- mtcars[order(mtcars$mpg),]
head(mtcars.mpg)

order(mtcars$mpg, mtcars$gear)
mtcars.mpg <- mtcars[order(mtcars$mpg, mtcars$gear),]
head(mtcars.mpg)

mtcars.mpg.desc <- mtcars[order(mtcars$mpg, decreasing=TRUE),]
head(mtcars.mpg.desc)

myorder <- order(mtcars$mpg, decreasing=TRUE)
myorder
mtcars[myorder,]
rm(myorder)

order(mtcars[,c("mpg","gear")])

#### Merging Data
authors <- data.frame(
	surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
	nationality = c("US", "Australia", "US", "UK", "Australia"),
	deceased = c("yes", rep("no", 4)))
books <- data.frame(
	name = I(c("Tukey", "Venables", "Tierney",
			   "Ripley", "Ripley", "McNeil", "R Core")),
	title = c("Exploratory Data Analysis",
			  "Modern Applied Statistics ...",
			  "LISP-STAT",
			  "Spatial Statistics", "Stochastic Simulation",
			  "Interactive Data Analysis",
			  "An Introduction to R"),
	other.author = c(NA, "Ripley", NA, NA, NA, NA,
					 "Venables & Smith"))
authors
books
merge(authors, books, by.x = "surname", by.y = "name")
merge(books, authors, by.x = "name", by.y = "surname")
merge(authors, books, by.x = "surname", by.y = "name", all=TRUE)



require(pisa)
data(pisa.student)
data(pisa.school)
names(pisa.student)
names(pisa.school)
pisa.student <- pisa.student[,c("SCHOOLID","CNT","StIDStd","PV1MATH","PV2MATH",
								"PV3MATH","PV4MATH","PV5MATH")]
#SC02Q01 = Public or private
#SC15Q01 = Standardised Tests (1 Never; 2 1-2 times a year; 3 3-5 times a year;
#          4 Monthly; 5 More than once a month; 7 N/A; 8 Invalid; 9 Miss)
pisa.school <- pisa.school[,c("SCHOOLID","CNT","COUNTRY","SC02Q01","SC15Q01")]

table(pisa.school$SC15Q01, useNA='ifany')

str(pisa.student)
str(pisa.school)
pisa.student$SCHOOLID <- as.integer(pisa.student$SCHOOLID)
pisa.school$SCHOOLID <- as.integer(pisa.school$SCHOOLID)
head(pisa.student); nrow(pisa.student)
head(pisa.school); nrow(pisa.school)
pisa <- merge(pisa.student, pisa.school, by=c("SCHOOLID"), all.x=TRUE)
nrow(pisa.student); nrow(pisa)

pisa <- merge(pisa.student, pisa.school, by=c("CNT","SCHOOLID"), all.x=TRUE)

describeBy(pisa$PV1MATH, group=list(pisa$SC15Q01), mat=TRUE, skew=FALSE)

#Let's look at duplicated schools
nrow(pisa.school)
length(unique(pisa.school$SCHOOLID))
dupids <- pisa.school[duplicated(pisa.school$SCHOOLID),]$SCHOOLID
length(dupids)
dupids <- unique(dupids)
length(dupids)
tmp <- pisa.school[order(pisa.school$SCHOOLID),]
head(tmp[which(tmp$SCHOOLID %in% dupids),])

#### Reshaping
require(reshape2)
mydata <- data.frame(id=c(1,1,2,2), time=c(1,2,1,2), x1=c(5,3,6,2), x2=c(6,5,1,4))
mydata

mydata.melted <- melt(mydata, id=c("id","time"))
mydata.melted

dcast(mydata.melted, id ~ variable, mean)
dcast(mydata.melted, time ~ variable, mean)


#### xtable
require(xtable)
data(mtcars)
head(mtcars)
mtcars$car <- row.names(mtcars)
row.names(mtcars) <- 1:nrow(mtcars)
mtcars[,c('car', 'mpg', 'wt')]

x <- xtable(mtcars[,c('car','mpg','wt')])
print(x, include.rownames=FALSE)

#We want to the wt column to be in parentheses
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(-1)
addtorow$command <- c('\\hline Car & \\multicolumn{2}{c}{Miles Per Gallon} \\\\')
mtcars$wt2 <- paste0('(', mtcars$wt, ')')
x <- xtable(mtcars[,c('car','mpg','wt2')])
print(x, include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow,
	  hline.after=c(0,(nrow(x)-1), nrow(x)))


