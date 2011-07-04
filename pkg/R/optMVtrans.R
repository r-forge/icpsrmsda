optMVtrans <- function(dat, maxit=25, rescale=TRUE, diff=.01){
require(car)
if(rescale){
	dat <- sweep(dat, 2, apply(dat, 2, min)-1)
}
mat <- as.matrix(dat)
R2 <- cor(dat)^2
tmp.corsq <- sum(R2[lower.tri(R2)])
bench.corsq <- 0
k <- 1
while(tmp.corsq - bench.corsq > diff & k <= maxit){
bench.corsq <- tmp.corsq
out <- sapply(1:ncol(mat), function(x){
	tmp.y <- mat[,x]
	tmp.x <- mat[,-x]
	tmp.mod <- lm(tmp.y ~ tmp.x + boxCoxVariable(tmp.y))
	tmp.trans.param <- 1-tmp.mod$coef[grep("box", names(tmp.mod$coef))]
	if(tmp.trans.param > -.05 & tmp.trans.param < .05){
		new.y <- log(tmp.y)
	} 
	else{
		new.y <- ((tmp.y^tmp.trans.param)-1)/tmp.trans.param
	}
new.y
})
mat <- sweep(out, 2, 
	apply(out, 2, min)-1)
tmp.R2 <- cor(out)^2
tmp.corsq <- sum(tmp.R2[lower.tri(tmp.R2)])
if(k == 1 & tmp.corsq < bench.corsq){
	out <- dat
	cat("Correlations Already Optimal, No Transformations Done\n")
}
k <- k+1
}
colnames(out) <- colnames(dat)
return(out)
}
