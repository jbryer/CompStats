setwd("~/Dropbox/School/Teaching/EPSY887 Spring 2013 Computational Statistics")
getwd()

require(psych)
require(ggplot2)

#### For Bruce
install.packages('~/Dropbox/Bryer Dissertation/fromDudek/bcdstats_1.0.tar.gz',
				 type='source')
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

#pisa <- merge(pisa.student, pisa.school, by=c("SCHOOLID"), all.x=TRUE)
nrow(pisa.student); nrow(pisa)

pisa <- merge(pisa.student, pisa.school, by=c("CNT","SCHOOLID"), all.x=TRUE)
nrow(pisa.student); nrow(pisa)

names(pisa)
table(pisa.school$COUNTRY, useNA='ifany')
table(pisa$COUNTRY, useNA='ifany')
table(is.na(pisa$COUNTRY), useNA='ifany')

describeBy(pisa$PV1MATH, group=list(pisa$SC15Q01), mat=TRUE, skew=FALSE)
res <- describeBy(pisa$PV1MATH, group=list(pisa$CNT, pisa$SC15Q01), mat=TRUE, skew=FALSE)
View(res)
View(res[order(res$group1, res$group2),])

describe(mtcars$mpg)

#Let's look at duplicated schools
nrow(pisa.school)
length(unique(pisa.school$SCHOOLID))
dupids <- pisa.school[duplicated(pisa.school$SCHOOLID),]$SCHOOLID
length(dupids)
dupids <- unique(dupids)
length(dupids)
tmp <- pisa.school[order(pisa.school$SCHOOLID),]
head(tmp[which(tmp$SCHOOLID %in% dupids),])

dups2 <- duplicated(pisa.school[,c('SCHOOLID', 'CNT')])
table(dups2)

#With mtcars
duplicated(mtcars$cyl), mtcars$cyl)
cbind(duplicated(mtcars$cyl), mtcars$cyl)
data.frame(dup=duplicated(mtcars$cyl), cyl=mtcars$cyl)
data.frame(dup=duplicated(mtcars$cyl, fromLast=TRUE), cyl=mtcars$cyl)

#### Reshaping
require(reshape2)
mydata <- data.frame(id=c(1,1,2,2), time=c(1,2,1,2), x1=c(5,3,6,2), x2=c(6,5,1,4))
mydata

mydata.melted <- melt(mydata, id=c("id","time"))
mydata.melted

describeBy(mydata.melted$value, group=list(mydata.melted$variable), mat=TRUE, skew=FALSE)

describe(mydata$x1)
describe(mydata$x2)

dcast(mydata.melted, id + time ~ variable, mean)

dcast(mydata.melted, id ~ variable, mean)
dcast(mydata.melted, id ~ variable, sd)

dsum <- dcast(mydata.melted, id ~ variable, sum)
dlen <- dcast(mydata.melted, id ~ variable, length)
dsum[,2:3] / dlen[,2:3]

dcast(mydata.melted, time ~ variable, mean)
dcast(mydata.melted, time ~ variable, sd)

mydata.melted$id2 <- factor(mydata.melted$id, levels=c(1,2), labels=c('A','B'))
dcasted <- dcast(mydata.melted, id2 ~ variable, mean)
dcasted
dcasted[which(dcasted$id2 == 'A'),]

###ggplot2
p <- ggplot(diamonds, aes(x=carat,y=price,colour=cut)) + geom_point()

p + facet_wrap(~cut) +	ggtitle("First example")
p + facet_grid(~cut) +	ggtitle("First example")
p + facet_grid(cut ~ .) +	ggtitle("First example")

diamonds$logprice <- log(diamonds$price)
head(diamonds)

p + scale_y_log10()

ggplot(diamonds, aes(x=carat, y=price, colour=cut)) + geom_smooth()

ggplot(diamonds, aes(x=carat, y=price, colour=cut)) + geom_point(alpha=.1) + geom_smooth()

search()
ls('package:ggplot2')
ls('package:ggplot2')[grep('^geom_*', ls('package:ggplot2'))]


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


