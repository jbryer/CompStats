require(ggplot2)

#Create a dummy data frame with three variables, id, x, and y. The x and y will
#have random normal distributions with means of 2 and 5, respectively.
df = data.frame(id=1:20, x=rnorm(20, mean=2, sd=.5), y=rnorm(20, mean=5, sd=2))

ggplot(df, aes(x=x, y=y)) + geom_point()

mylm = lm(y ~ x, data=df)
summary(mylm)
names(mylm) #This lists all the elements in the lm class object
mylm$coefficients #We want the coefficients

#Same as above but we will add geom_abline
ggplot(df, aes(x=x, y=y)) + geom_point() + geom_abline(intercept=mylm$coefficients[1], slope=mylm$coefficients[2])

#I tend to like Loess regression lines better
ggplot(df, aes(x=x, y=y)) + geom_point() + stat_smooth()

#Nice thing about ggplot2 is that we can layer on all the features
ggplot(df, aes(x=x, y=y)) + geom_point() + geom_abline(intercept=mylm$coefficients[1], slope=mylm$coefficients[2]) + stat_smooth()
