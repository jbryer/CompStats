require(foreign)
require(ggplot2)
require(psych)

clust <- read.spss('Stephanie/clusterCleaneddataALLvar.sav', to.data.frame=TRUE)
names(clust)
nrow(clust)
head(clust)
table(clust$Cluster, useNA='ifany')

clust$Cluster <- as.factor(clust$Cluster)

ggplot(clust, aes(x=Cluster)) + geom_histogram()
ggplot(clust[!is.na(clust$Cluster),], aes(x=Cluster)) + geom_histogram()

ggplot(clust, aes_string(x='Cluster')) + geom_histogram()

for(i in 2:4) {
	p <- ggplot(clust[!is.na(clust[,i]),], aes_string(x=names(clust)[i])) +
		geom_histogram() + ggtitle(paste('Histogram for', names(clust)[i]))
	print(p)
	#ggsave
}

#TODO: impute missingness; run cluster analysis

table(is.na(clust$Cluster))

clust2 <- clust[!is.na(clust$Cluster),]
table(is.na(clust2$Cluster))

cols <- names(clust2)[2:8]

for(i in cols) {
	# (x - mean) / SD
	clust2[,i] <- (clust2[,i] - mean(clust2[,i])) / sd(clust2[,i])
}

d1 <- describeBy(clust2$monitor, clust2$Cluster, mat=TRUE)[,c('group1','mean')]
d1
d1$variable <- 'monitor'

d2 <- describeBy(clust2$ncs01733, clust2$Cluster, mat=TRUE)[,c('group1','mean')]
d2
d2$variable <- 'ncs01733'

dtotal <- rbind(d1, d2)
dtotal

dtotal <- data.frame()
for(i in cols) {
	d <- describeBy(clust2[,i], clust2$Cluster, mat=TRUE)[,c('group1','mean')]
	d$variable <- i
	dtotal <- rbind(dtotal, d)
}
dtotal

ggplot(dtotal, aes( y=mean, x=variable)) + geom_bar(stat='identity')

ggplot(dtotal, aes( y=mean, x=variable)) + 
	geom_bar(stat='identity') + facet_grid(~ group1)

ggplot(dtotal, aes( y=mean, x=variable)) + 
	geom_bar(stat='identity') + facet_grid(~ group1) + coord_flip()

ggplot(dtotal, aes( y=mean, x=variable)) + 
	geom_bar(stat='identity') + facet_grid(~ group1) + coord_flip() +
	ylim(c(-1,1)) + xlab('') + ylab('Standardized Mean')

#Different version
ggplot(dtotal, aes(y=mean, x=variable, fill=group1)) + 
	geom_bar(stat='identity', position='dodge') +
	ylim(c(-1,1)) + xlab('') + ylab('Standardized Mean')

ggplot(dtotal, aes(y=mean, x=variable, fill=group1)) + 
	geom_bar(stat='identity', position='dodge') +
	ylim(c(-1,1)) + xlab('') + ylab('Standardized Mean') +
	facet_grid(~ variable, scales='free_x')

ggplot(dtotal, aes(y=mean, x=variable, fill=group1)) + 
	geom_bar(stat='identity', position='dodge') +
	ylim(c(-1,1)) + xlab('') + ylab('Standardized Mean') +
	facet_grid(~ variable, scales='free_x') +
	theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
		  panel.grid=element_line(size=3, color='black', linetype=1))

ggplot(dtotal, aes(y=mean, x=variable, fill=group1)) + 
	geom_bar(stat='identity', position='dodge') +
	ylim(c(-1,1)) + xlab('') + ylab('Standardized Mean') +
	facet_grid(~ variable, scales='free_x') +
	theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
		  panel.grid=element_line(size=3, color='black', linetype=1)) +
	scale_fill_hue('Cluster')

ggplot(dtotal, aes(y=mean, x=variable, fill=group1)) + 
	geom_bar(stat='identity', position='dodge') +
	ylim(c(-1,1)) + xlab('') + ylab('Standardized Mean') +
	facet_grid(~ variable, scales='free_x') +
	theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
		  panel.grid=element_line(size=3, color='black', linetype=1),
		  legend.position='none')

#### Boxplot
head(clust)
str(clust)
summary(clust$monitor)
plot(clust$monitor)
hist(clust$monitor)
plot(clust$monitor, clust$ncs01733)
pairs(clust[,2:8])
#These functions are from the help file for pairs (see ?pairs)
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
	r <- abs(cor(x, y, use='pairwise.complete.obs'))
	txt <- format(c(r, 0.123456789), digits=digits)[1]
	txt <- paste(prefix, txt, sep="")
	if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
	text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(clust[,2:8], diag.panel=panel.hist, upper.panel=panel.cor, 
	  lower.panel=panel.smooth, cex.cor=5)


######
arrests <- read.csv('Data/ArrestsPotency.csv')
head(arrests)
nrow(arrests)
str(arrests)
View(arrests)

p <- ggplot(arrests, aes(x=Total.Marijuana.Arrests, y=Average.Potency)) +
	geom_point() + 
	geom_text(aes(label=paste(Year, Year.1, sep=' to ')), size=4, vjust=-1)
print(p)

arrests$label <- paste(arrests$Year, arrests$Year.1, sep=' to ')

ggplot(arrests, aes(x=Total.Marijuana.Arrests, y=Average.Potency, label=label)) +
	geom_point() + 
	geom_text(size=4, vjust=-1, position=position_jitter(width=5, height=0))

range(arrests$Total.Marijuana.Arrests)

ggplot(arrests, aes(x=Total.Marijuana.Arrests, y=Average.Potency, label=label)) +
	geom_point() + 
	geom_text(size=4, vjust=-1, position=position_jitter(width=5, height=0)) +
	xlim(c(630000, 900000))


ggplot(arrests, aes(x=Total.Marijuana.Arrests, y=Average.Potency, label=label)) +
	geom_point() + 
	geom_text(size=4, vjust=-1, position=position_jitter(width=5, height=0)) +
	xlim(c(630000, 900000)) + geom_smooth()

ggplot(arrests, aes(x=Year, y=Total.Marijuana.Arrests)) + geom_point() + 
	geom_smooth()

#TODO: is there another way preventing overlap
require(directlabels)
p <- ggplot(arrests, aes(x=Total.Marijuana.Arrests, y=Average.Potency,
						 color=label)) +
	geom_point()
direct.label(p) #Looks kind of bad

####
data(movies)
head(movies)
require(reshape2)

ggplot(movies, aes(x=year)) + geom_histogram()
describeBy(movies$length, movies$year, mat=TRUE, skew=FALSE)

movies2 <- movies[,c('year','length','Action','Animation','Comedy','Drama',
					 'Documentary','Romance')]
movies2.melt <- melt(movies2, id.vars=c('year','length'), variable.name='Genre')
head(movies2.melt)
#movies2.melt <- movies2.melt[which(movies2.melt$value != 0), ]

movies2.melt$length <- movies2.melt$length * movies2.melt$value

movsum <- describeBy(movies2.melt$length, 
					 group=list(movies2.melt$year, movies2.melt$Genre), 
		             mat=TRUE, skew=FALSE)
head(movsum)
str(movsum)

movsum$group1 <- as.integer(as.character(movsum$group1))

p <- ggplot(movsum, aes(x=group1, y=mean, group=group2, color=group2)) + 
	geom_line()
print(p)

direct.label(p)
direct.label(p, method='first.qp')

p + geom_point()
p + geom_point(aes(size=n))
p + geom_point(aes(size=n)) + scale_size_continuous(range=c(1,20))
p + geom_line(aes(size=n))
p + geom_line(aes(size=n)) + scale_size_continuous(range=c(1,20))

#### Side note on the which function
test <- c(1,1,1,0,0,1,1,1,1,0,1)
test != 0
which(test != 0)
test[which(test != 0)]

