require(ggplot2)
require(psych)
require(likert)
require(pisa)

data(pisaitems)

items28 = pisaitems[,substr(names(pisaitems), 1,5) == 'ST24Q']
head(items28); ncol(items28)

items28 <- rename(items28, c(
	ST24Q01="I read only if I have to.",
	ST24Q02="Reading is one of my favorite hobbies.",
	ST24Q03="I like talking about books with other people.",
	ST24Q04="I find it hard to finish books.",
	ST24Q05="I feel happy if I receive a book as a present.",
	ST24Q06="For me, reading is a waste of time.",
	ST24Q07="I enjoy going to a bookstore or a library.",
	ST24Q08="I read only to get information that I need.",
	ST24Q09="I cannot sit still and read for more than a few minutes.",
	ST24Q10="I like to express my opinions about books I have read.",
	ST24Q11="I like to exchange books with my friends"))
str(items28)


items28.2 <- items28
for(i in 1:ncol(items28.2)) {
	items28.2[,i] <- as.integer(items28.2[,i]) - 1
}
str(items28.2)

table(items28[,1])
table(items28.2[,1])

items28.3 <- items28.2
for(i in 1:ncol(items28.3)) {
	items28.3[,i] <- factor(items28.3[,i], levels=c(0,1,2,3), 
					labels=c('Strongly disagree','Disagree','Agree','Strongly Agree'),
							ordered=TRUE)
}
str(items28.3)

table(items28.3[,1])

items28.4 <- items28.2
for(i in 1:ncol(items28.4)) {
	items28.4[,i] <- factor(items28.4[,i], levels=3:0, 
					labels=c('Strongly Agree','Agree','Disagree','Strongly disagree'),
					ordered=TRUE)
}
str(items28.4)

table(items28[,1], useNA='ifany')

df1 <- data.frame(Country=pisa.student$CNT, Reading=items28[,1], stringsAsFactors=FALSE)
df1 <- df1[df1$Country %in% c('United States','Canada','Mexico'),]
df1 <- df1[!is.na(df1$Reading),]
df1$Reading2 <- as.integer(df1$Reading)

ggplot(df1, aes(x=Reading)) + geom_bar()
ggplot(df1, aes(x=Country, fill=Reading)) + geom_bar(position='stack')
ggplot(df1, aes(x=Country, fill=Reading)) + geom_bar(position='dodge')

ggplot(df1, aes(x=Reading)) + geom_bar() + facet_grid(~ Country)
ggplot(df1, aes(x=Reading)) + geom_bar() + facet_grid(Country ~ .)

ggplot(df1, aes(x=Reading)) + geom_bar() + facet_wrap(~ Country)
ggplot(df1, aes(x=Reading)) + geom_bar() + facet_wrap(~ Country, ncol=2)
ggplot(df1, aes(x=Reading)) + geom_bar() + facet_wrap(~ Country, ncol=1)

ggplot(df1, aes(x=Country, y=Reading2)) + geom_boxplot()

ggplot(df1, aes(x=Country, y=Reading2)) + geom_boxplot() + geom_point(alpha=.2)
ggplot(df1, aes(x=Country, y=Reading2)) + geom_boxplot() + geom_jitter(alpha=.02)
ggplot(df1, aes(x=Country, y=Reading2)) + geom_jitter(alpha=.02) + geom_boxplot()
ggplot(df1, aes(x=Country, y=Reading2)) + geom_jitter(alpha=.02, aes(color=Reading)) + geom_boxplot()



##### likert package
l28 = likert(items28, grouping=pisa.student$Country)
print(l28)
summary(l28)

plot(l28)
plot(l28, centered=TRUE, low.color='#FF9900', high.color='#660066')

plot(l28, type='heat')


#Group by country
l28g = likert(items28, grouping = pisa.student$CNT)
print(l28g)
summary(l28g)

plot(l28g, low.color='maroon', high.color='burlywood4')
plot(l28g, low.color='maroon', high.color='burlywood4', centered=TRUE)

#How often do you read these materials because you want to?
items29 = pisaitems[,substr(names(pisa.student), 1,5) == 'ST25Q']
head(items29); ncol(items29)
names(items29) = c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")
for(i in 1:ncol(items29)) {
	items29[,i] = factor(items29[,i], levels=1:5, 
						 labels=c('Never or almost never', 'A few times a year', 
						 		 'About once a month', 'Several times a month', 
						 		 'Several times a week'), ordered=TRUE)
}

l29 = likert(items29)
print(l29)
summary(l29)

plot(l29, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")

plot(l29, type='heat') + opts(title="How often do you read these materials because you want to?")

l29g = likert(items29, grouping=pisa.student$CNT)
summary(l29g)

plot(l29g, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")

plot(l29g, centered=TRUE, low.color='maroon', high.color='burlywood4') + 
	opts(title="How often do you read these materials because you want to?")



################################################################################
likert <- likert(items28)
low.color='blue'
high.color='red'
neutral.color='white'
text.size=3
text.color='black'
centered=FALSE
ordered=TRUE

lowrange = 1:ceiling(likert$nlevels / 2 - likert$nlevels %% 2)
highrange = ceiling(likert$nlevels / 2 + 1 ) : likert$nlevels
ramp = colorRamp(c(low.color, neutral.color))
ramp = rgb( ramp(seq(0, 1, length=((likert$nlevels+1)/2) )), max=255)
bamp = colorRamp(c(neutral.color, high.color))
bamp = rgb( bamp(seq(0, 1, length=((likert$nlevels+1)/2) )), max=255)
cols = NULL
if(likert$nlevels %% 2 != 0) {
	cols = c(ramp[1:(length(ramp)-1)], neutral.color, bamp[2:length(bamp)])
} else {
	cols = c(ramp[1:(length(ramp)-1)], bamp[2:length(bamp)])
}
results = melt(likert$results, id.vars='Item')
if(ordered) {
	order = likert$summary[order(likert$summary$high),'Item']
	results$Item = factor(results$Item, levels=order, ordered=TRUE)
}
p = ggplot(results, aes(y=value, x=Item, group=Item))
ymin = 0
p = p + geom_bar(stat='identity', aes(fill=variable)) + ylim(c(-5,105))
p = p + scale_fill_manual('Response', values=cols, 
						  breaks=levels(results$variable), 
						  labels=levels(results$variable))
p = p + 
	geom_text(data=likert$summary, y=ymin, aes(x=Item, 
											   label=paste(round(low), '%', sep='')), 
			  size=text.size, hjust=1) +
	geom_text(data=likert$summary, y=100, aes(x=Item,
											  label=paste(round(high), '%', sep='')), 
			  size=text.size, hjust=-.2) +
	coord_flip() + ylab('Percentage') + xlab('') + 
	theme(axis.ticks=element_blank())



theme_update(panel.background=element_blank(), 
			 panel.grid.major=element_blank(), 
			 panel.border=element_blank())

