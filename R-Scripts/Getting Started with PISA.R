#Load libraries
require(psych)
require(ggplot2)
require(multilevelPSA)
require(reshape)

#Know what is loaded
library() #List of installed packages
search() #List of loaded packages
ls() #List of objects (e.g. data frames, vectors, functions, etc.) in the global environment
ls('package:psych') #List of objects in the psych package

#Getting help
?describe #Put a question mark in front of any function to get its help documentation
help.search("mean") #Search the help system (of installed packages) for a keyword(s)
str(mean)

#Vignettes are PDF documents that provide an overview of a package.
vignette() #List of all available vignettes within installed packages.
vignette('overview', package='psych')

#Demos
demo() #Returns a list of all available vignettes within installed packages.

#Data
data()
data(pisana)
names(pisana)
str(pisana)
class(pisana)

nrow(pisana)
ncol(pisana)

head(pisana)
tail(pisana)
View(pisana)

pisana[1,]
pisana[1:10,1]
pisana[1:10,'AGE']
pisana[1:10,]$AGE
pisana[1:10,c('CNT','AGE')]

#Factors
class(pisana$CNT)
levels(pisana$CNT)
table(pisana$CNT, useNA='ifany')
table(pisana$TESTLANG, pisana$CNT, useNA='ifany')
summary(pisana$CNT)

#Converting a factor to a string
pisana$TESTLANG = as.character(pisana$TESTLANG)
class(pisana$TESTLANG)
table(pisana$TESTLANG, pisana$CNT, useNA='ifany')

#Numeric
class(pisana$AGE)
summary(pisana$AGE)
fivenum(pisana$AGE)
mean(pisana$AGE, na.rm=TRUE)
sd(pisana$AGE, na.rm=TRUE)
weighted.mean(pisana$AGE, pisana$W_FSTUWT)
#The psych package has some useful functions for summarizing data
describe(pisana$AGE)
describe.by(pisana$AGE, pisana$CNT, mat=TRUE)

table(pisana$PARED, useNA='ifany')
#Calculate the correlation between parents education and the first math score
cor(pisana$PARED, pisana$PV1MATH, use='pairwise.complete.obs')

#Now we'll subset out USA students using the which function
pisa.usa = pisana[which(pisana$CNT == 'USA'),]
nrow(pisa.usa)


ggplot(pisa.usa, aes(x=PV1READ, y=PV1MATH)) + geom_point(alpha=.1) + geom_smooth()

ggplot(pisana, aes(CNT, PV1MATH)) + geom_boxplot() + coord_flip()

cor(pisana$PV1MATH, pisana$PV1READ, use='pairwise.complete.obs')


pairs(pisana[,c('PV1MATH', 'PV1SCIE', 'PV1READ')])
#A better pairs plot. First we need to define two functions (these come from the
#pairs help page).
panel.hist <- function(x, ...) {
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 1.5) )
	h <- hist(x, plot = FALSE)
	breaks <- h$breaks; nB <- length(breaks)
	y <- h$counts; y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
	usr <- par("usr"); on.exit(par(usr))
	par(usr = c(0, 1, 0, 1))
	r <- abs(cor(x, y))
	txt <- format(c(r, 0.123456789), digits=digits)[1]
	txt <- paste(prefix, txt, sep="")
	if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(pisana[,c('PV1MATH', 'PV1SCIE', 'PV1READ')], 
	  lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist)

pisa.math = pisa.usa[,c('StIDStd','PV1MATH','PV2MATH','PV3MATH','PV4MATH','PV5MATH')]
pisa.math.melted = melt(pisa.math, id.vars='StIDStd')
names(pisa.math.melted)
head(pisa.math.melted)
ggplot(pisa.math.melted, aes(x=variable, y=value)) + geom_boxplot() + coord_flip()	   
pisa.sample = pisa.math[sample(nrow(pisa.math), 100),'StIDStd']
ggplot(pisa.math.melted[which(pisa.math.melted$StIDStd %in% pisa.sample),], 
	   aes(x=variable, y=value, group=StIDStd)) + geom_line(alpha=.2)
