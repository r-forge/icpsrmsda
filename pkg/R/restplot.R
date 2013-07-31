restplotfun <- function(x,y,jitter,...){
	plot(x, jitter(y, jitter), xlab = "Rest Scale", ...)
	lines(lowess(x,y,f=.25), col="red", lwd=2)
}
restplot <- function(X, jitter=0){
	nc <- ncol(X)+1
	nr <- ceiling(nc/3)
	par(mfrow=c(nr, 3))
	apdat <- lapply(1:(nc-1), function(i)cbind(rowSums(X[,-i]), X[,i]))
	for(i in 1:length(apdat)){
		colnames(apdat[[i]]) <- c("restscore", colnames(X)[i])
	}
	sapply(apdat, function(x)restplotfun(x=x[,1], y=x[,2], jitter=jitter, ylab = colnames(x)[2]))
	los <- lapply(apdat, function(Z)lowess(Z[,1], Z[,2], f=.25))
	xl <- range(c(sapply(los, function(x)x$x)))
	yl <- range(c(sapply(los, function(x)x$y)))
	plot(c(mean(xl),mean(yl)), xlim=xl,ylim=yl, type="n", xlab="Rest Scale", ylab="Individual Variables")
	lapply(1:length(los), function(i)lines(los[[i]]$x, los[[i]]$y, lty=i))
}
