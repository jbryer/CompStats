mandelbrot <- function(x = c(-3.0, 1.0), # x coordinates 
					   y = c(-1.8, 1.8), # y coordinates 
					   b = .05, # by 'steps' 
					   iter = 20) # maximum number of iterations 
{ 
	m = NULL # will store the results, this is the 'image' matrix 
	
	for(i in seq(x[1], x[2], by = b)) { 
		r = NULL # stores part of the iteration results 
		for(j in seq(y[1], y[2], by = b)) { 
			it = iter # will hold iteration at which point (i, j) breaks off 
			c = c(i, j) # initial point 
			z = c(i, j) # i: real part; j: imaginary part 
			for(k in 1:iter) { 
				# the Mandelbrot iteration formulae: z -> z*z + c 
				z = c(z[1]^2 - z[2]^2, 2 * z[1]*z[2]) + c 
				# tests if point breaks off 
				if((z[1] + z[2])^2 > 4) { it = k; break } 
			} 
			r = c(r, it) # stores iteration results 
		} 
		# constructs the 'image' matrix 
		m = rbind(m, r) 
	} 
	# the output fractal object 
	fractal = list(x = seq(x[1], x[2], by = b), # x coordinates 
				   y = seq(y[1], y[2], by = b), # y coordinates 
				   z = m) # it matrix 
}

if(FALSE) {
	image(mandelbrot(b=.01))
	
	Limits=c(-2,0.8)
	MaxIter=25
	cl=colours()
	Step=seq(Limits[1],Limits[2],by=0.005)
	S=floor(length(cl)/MaxIter)
	Dist=0
	PointsMatrix=array(0,dim=c(length(Step)*length(Step),3))
	t=0
	
	
	for(a in Step)
	{
		for(b in Step+0.6)
		{
			x=0;y=0;n=0;Dist=0
			while(n<MaxIter & Dist<4)
			{
				n=n+1
				newx=a+x^2-y^2
				newy=b+2*x*y
				Dist=newx^2+newy^2
				x=newx;y=newy
			}
			if(Dist<4) colour=24 # black colour
			else colour=n*S
			t=t+1
			PointsMatrix[t,]=c(a,b,colour)
		}
	}
	
	X11()
	
	plot(PointsMatrix[,1], PointsMatrix[,2], xlim=Limits, ylim=Limits+0.6, col=cl[PointsMatrix[,3]], pch=".")

}
