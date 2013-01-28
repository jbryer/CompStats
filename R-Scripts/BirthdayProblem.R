## See http://en.wikipedia.org/wiki/Birthday_problem for an explanation  of the problem
require(ggplot2)
require(reshape)

theme_update(panel.background=theme_blank(), 
			 panel.grid.major=theme_blank(), 
			 panel.border=theme_blank())

birthday <- function(n) { 
	1 - exp( - n^2 / (2 * 365) )
}

myBirthday <- function(n) {
	1 - ( (365 - 1) / 365 ) ^ n
}

d = 200
df = data.frame(n=1:d, AnyTwoSame=birthday(1:d), SameAsMine=myBirthday(1:d))
df = melt(df, id.vars='n')

ggplot(df, aes(x=n, y=value, colour=variable)) + geom_line() + scale_colour_hue('') +
	xlab('Number of People in Group') + ylab('Probability')

