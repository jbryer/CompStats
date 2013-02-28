setwd("~/Dropbox/School/Teaching/EPSY887 Spring 2013 Computational Statistics")
require(tree)
require(rpart)
require(party)
require(randomForest)
require(ggplot2)
require(mice)
require(ROCR)

# California house prices
calif <- read.table('./Data/cadata.dat', header=TRUE)

# Titanic data.
# http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic.html
load('./Data/titanic3.sav') 
#titanic3 <- read.csv('./Data/titanic3.csv')

titanic3$survived <- as.integer(titanic3$survived)
titanic3$pclass <- as.integer(titanic3$pclass)

names(titanic3)
str(titanic3)
nrow(titanic3)
table(titanic3$survived, useNA='ifany')
table(titanic3$pclass, useNA='ifany')
table(titanic3$sex, useNA='ifany')
table(titanic3$sibsp, useNA='ifany')
summary(titanic3$age)

par(mfrow = c(1,1), xpd = NA) #To prevent text from being clipped
par(bg="white", mai=c(1.2,1.5,1,1))

##### Classification Trees #####################################################

#Using rpart
titanic.rpart <- rpart(survived ~ pclass + sex + age + sibsp,
					   data=titanic3)
print(titanic.rpart)
plot(titanic.rpart); text(titanic.rpart, use.n=TRUE, cex=.8)

#Using tree
titanic.tree <- tree(survived ~ pclass + sex + age + sibsp,
					 data=titanic3)
print(titanic.tree)
plot(titanic.tree); text(titanic.tree, cex=.8)

#Using ctree
titanic.ctree <- ctree(survived ~ pclass + sex + age + sibsp,
					   data=titanic3)
print(titanic.ctree)
plot(titanic.ctree)

#Checing with ROCR
titanic.pred <- predict(titanic.ctree)
pred <- prediction(titanic.pred, as.integer(titanic3$survived))
perf <- performance(pred, measure="tpr", x.measure="fpr") 
plot(perf, colorize=TRUE, yaxis.at=c(0,0.5,0.8,0.9,1), yaxis.las=1)
lines(c(0,1), c(0,1), col='grey')


plot(perf, avg='vertical', spread.estimate='boxplot',
	 show.spread.at= seq(0.1, 0.9, by=0.1),
	 yaxis.at=c(0,0.5,0.8,0.9,1), yaxis.las=1)

perf <- performance(pred, "cal", window.size=50)
plot(perf)

perf <- performance(pred, "acc")
plot(perf, avg= "vertical", spread.estimate="boxplot", 
	 show.spread.at= seq(0.1, 0.9, by=0.1))

##### Health Sciences
require(TriMatch)
data(students)
students$Treat <- TRUE
students[students$TreatBy %in% c('Control'),]$Treat <- FALSE
rp = rpart(Treat ~ Age + Ethnicity + Military + Gender + Employment +
		   TransferCredits + NativeEnglish + EdLevelMother + EdLevelFather + Income,
		   data=students)
plot(rp); text(rp, cex=1, use.n=TRUE)

strata = factor(rp$where)
table(strata, students$Treat, useNA='ifany')

printcp(rp)
(cp4min <- rp$cptable[which.min(rp$cptable[,"xerror"]),"CP"])
rp2 = prune(rp, cp=cp4min - .001)
plot(rp2); text(rp2, use.n=TRUE, all=FALSE)
rp2

strata2 = factor(rp2$where)
table(strata2, students$Treat)


##### Logistic regression
titanic.mice <- mice(titanic3[,c("pclass","sex","age","sibsp")])
titanic.complete <- cbind(survived=titanic3$survived, complete(titanic.mice, 5))

titanic.glm <- glm(survived ~ pclass + sex + age + sibsp,
				   data=titanic.complete,
				   family=binomial(logit))
summary(titanic.glm)


titanic.glm2 <- glm(survived ~ pclass + sex + pclass:sex + age + sibsp,
				   data=titanic.complete,
				   family=binomial(logit))
summary(titanic.glm2)


titanic.complete$pclass <- factor(titanic.complete$pclass, 
								  levels=c(1,2,3),
								  labels=c('First','Second','Third'),
								  ordered=TRUE)
table(titanic.complete$pclass)

##### Random Forests ###########################################################
titanic.rf <- randomForest(survived ~ pclass + sex + age + sibsp, 
						   data=titanic.complete,
						   ntree=5000,
						   importance=TRUE)
print(titanic.rf)
importance(titanic.rf)


##### Regression Trees #########################################################
treefit <- tree(log(MedianHouseValue) ~ Longitude + Latitude, data=calif)
plot(treefit); text(treefit, cex=0.75)

price.deciles <- quantile(calif$MedianHouseValue, 0:9/9)
cut.prices <- cut(calif$MedianHouseValue, price.deciles, include.lowest=TRUE)

plot(calif$Longitude, calif$Latitude, col=grey(10:2/11)[cut.prices], pch=20, 
	 xlab="Longitude", ylab="Latitude")
partition.tree(treefit, ordvars=c("Longitude","Latitude"), add=TRUE)

summary(treefit)

treefit2 <- tree(log(MedianHouseValue) ~ Longitude + Latitude, 
				 data=calif,
				 mindev=.001)
plot(treefit2); text(treefit2, cex=0.75)
summary(treefit2)

plot(calif$Longitude, calif$Latitude, col=grey(10:2/11)[cut.prices], pch=20, 
	 xlab="Longitude", ylab="Latitude")
partition.tree(treefit2, ordvars=c("Longitude","Latitude"), add=TRUE)

treefit3 <- tree(log(MedianHouseValue) ~ ., data=calif)
plot(treefit3); text(treefit3, cex=0.75)
summary(treefit3)
print(treefit3)


##### Check missingness
missing.col <- apply(students, 2, FUN=function(x) { length(which(is.na(x))) / length(x) })
missing.row <- apply(students, 1, FUN=function(x) { length(which(is.na(x))) / length(x) })
length(missing.row)
hist(missing.row)
summary(missing.row)
View(students[which(missing.row > .1),])

