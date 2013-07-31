restplot <- function(X, jitter=0,...){
	require(lattice)
	nc <- ncol(X)
	apdat <- lapply(1:nc, function(i)cbind(rowSums(X[,-i]), X[,i]))
	plot.data <- as.data.frame(do.call(rbind, apdat))
	names(plot.data) <- c("restscore", "xj")
	plot.data$var <- rep(1:length(apdat), sapply(apdat, nrow))
	plot.data$var <- factor(plot.data$var, labels=colnames(X))
	xyplot(xj ~ restscore | var, data=plot.data, ..., 
		panel = function(x,y,subscripts){
			panel.points(x,jitter(y, jitter),col="gray75")
      		panel.loess(x, y, span =	 .7, col = "black",
      		        		family = "symmetric", degree = 2)		
	})

}
