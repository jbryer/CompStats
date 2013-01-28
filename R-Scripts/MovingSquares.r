## By Markus Gesmann
## http://lamages.blogspot.com/2012/07/bridget-riley-exhibition-in-london.html
## Inspired by Birdget Riley's Moving Squares
x <- c(0, 70, 140, 208, 268, 324, 370, 404, 430, 450, 468,
	   482, 496, 506,516, 523, 528, 533, 536, 542, 549, 558, 
	   568, 581, 595, 613, 633, 659, 688, 722, 764, 810)
y <- seq(from=0, to=840, by=70)
m <- length(y)
n <- length(x)
z <- t(matrix(rep(c(0,1), m*n/2), nrow=m))
image(x[-n], y[-m], z[-n,-m], col=c("black", "white"), 
	  axes=FALSE, xlab="", ylab="")


optical.art <- function(P, N, colors = brewer.pal(P, "Greys")) {
	## This function creates an Op art figure as can be found on
	## http://r-de-jeu.blogspot.com/
	## http://r-de-jeu.blogspot.com/2012/07/optical-art-with-r.html
	##
	## Inputs:
	##    - P: (integer) number of starting points
	##    - N: (integer) number of semi-lines starting from each point.
	##                   For best results, N should be a multiple of P 
	##    - colors:      vector of colors for filling polygons.
	##                   For best results, you should use P colors
	
	if(!require(sp) | !require(rgeos) | !require(RColorBrewer)) {
		#stop('Missing required packages.')
		install.packages(c('sp','rgeos','RColorBrewer'), repos='http://cran.r-project.org')
	}
	
	# plot area
	plot(c(0, 1), c(0, 1), "n", axes=FALSE, xlab="", ylab="")
	
	create.polygon <- function(x, y) {
		pol <- Polygon(cbind(c(x, head(x, 1)), c(y, head(y, 1))))
		set <- Polygons(list(pol), "pol")
		spP <- SpatialPolygons(list(set))
	}
	
	plot.area <- create.polygon(x = c(0,1,1,0), y = c(0,0,1,1))
	
	polygons <- vector("list", length = P)
	
	for (p in 1:P) {
		
		# randomly select the coordinates of a point and the
		# angles of N semi-lines starting from that point
		center.x <- runif(1)
		center.y <- runif(1)
		angles   <- 2*pi/N*(0:(N-1) + 2*pi*runif(1))
		
		# the semi-lines and frame will define N polygons
		polygons[[p]] <- vector("list", length = N)
		for (i in 1:N) {
			
			i1 <- i
			i2 <- ifelse((i + 1) > N, 1, i + 1)
			a1 <- angles[i1]
			a2 <- angles[i2]
			
			polygons[[p]][[i]] <-
				create.polygon(x = center.x + c(0, 2*cos(a1), 2*cos(a2), 0),
							   y = center.y + c(0, 2*sin(a1), 2*sin(a2), 0))
		}
	}
	
	all.comb <- expand.grid(data.frame(replicate(P, 1:N)))
	for (k in 1:nrow(all.comb)) {
		
		# compute the intersection of the polygons
		poly <- plot.area
		for (i in 1:P) {
			poly <- gIntersection(poly, polygons[[i]][[all.comb[k,i]]])
			if (gIsEmpty(poly)) break
		}
		if (gIsEmpty(poly)) next
		
		# plot the polygon and fill it with the appropriate color
		coords <- poly@polygons[[1]]@Polygons[[1]]@coords
		polygon(coords[, 1], coords[, 2], border = NA,
				col = colors[1 + (sum(all.comb[k,]) %% length(colors))])
	}
}

optical.art(P=2, N=30, colors=c('black','white'))
optical.art(P=3, N=12, colors=brewer.pal(3, 'Reds'))
optical.art(P=3, N=15, colors=brewer.pal(3, 'Purples'))
optical.art(P=3, N=15, colors=brewer.pal(3, 'Blues'))
optical.art(P=3, N=9, colors=brewer.pal(3, 'Blues'))
optical.art(P=3, N=9, colors=brewer.pal(3, 'Greens'))

