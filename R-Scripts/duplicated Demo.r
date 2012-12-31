#Demonstration of duplicated function

df = data.frame(rbind(
	c('1','A'),
	c('2','A'),
	c('1','B'),
	c('2','B'),
	c('1','C'),
	c('2','C'),
	c('1','A')))
df

df = cbind(df, 'FromStart'=duplicated(df$X1, fromLast=FALSE))
df = cbind(df, 'FromLast' =duplicated(df$X1, fromLast=TRUE))
df = cbind(df, 'NotFromStart'=!duplicated(df$X1, fromLast=FALSE))
df = cbind(df, 'NotFromLast' =!duplicated(df$X1, fromLast=TRUE))
df

df = cbind(df, 'TwoCols' = duplicated(df[,c('X1','X2')]))
df

df[duplicated(df$X1, fromLast=FALSE),]
df[duplicated(df$X1, fromLast=TRUE),]


v1 = c(1:5)
v2 = letters[1:5]
cbind(v1, v2, deparse.level=1) #The default
cbind(v1, v2, deparse.level=0)
cbind('Variable1'=v1, 'Variable2'=v2, deparse.level=2)
rbind(v1, v2)

df2 = as.data.frame(cbind(v1,v2))
class(df2)
names(df2)
row.names(df2)

cbind(v1, letters[1:6])
cbind(v1, letters[1:10])
length(letters[1:10]) %% length(v1)

require(ipeds)
data(surveys)
View(surveys)
ls('package:ipeds')
ipedsHelp('EFFY', 2008)
en09 = getIPEDSSurvey('EFFY', 2009)
en08 = getIPEDSSurvey('EFFY', 2008)


