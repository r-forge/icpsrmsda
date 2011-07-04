\encoding{UTF-8}
\name{optMVtrans}
\alias{optMVtrans}
\title{Optimal Transformations to linearize Multivariate Data
}
\description{
optMVtrans creates a new dataset where the columns of that dataset have been subjected to a non-linear transformation that maximizes the sum of squared lower-triangle correlations in the correlation matrix among the variables. 
}
\usage{
optMVtrans(dat, maxit = 25, rescale=TRUE, diff=.01)
}
\arguments{
  \item{dat}{A data frame containing only numeric variables. 
}
  \item{maxit}{The maximum number of transformations to compute before stopping. 
}
  \item{rescale}{Logical indicating whether the data should be rescaled such that all variables have minimum value=1.
}
  \item{diff}{The difference between the sum of squared lower-triangle correlations of the current and previous iterations, below which the algorithm will stop trying to find better transformations
}
}
\details{
The function starts by estimating the Box-Cox transformation on each variable and returns a new matrix where each variable was optimally, non-linearly transformed against all of the other original variables in the provided data matrix.  Then, if that set of transformations resulted in a greater sum of squared lower-triangle correlations among the transformed variables (with respect to the original data), the algorithm estimates another set of Box-Cox transformations on the data transformed in the previous step.  The algorithm continues until it reaches the maximum number of iteration or the sum of squared lower-triangle correlations converges. 
}
\value{
\item{out}{A matrix of transformed variables of the same dimension as the original input data
}
}
\references{
Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. \emph{JRSS B} \bold{26}: 211–246.
}
\author{
Dave Armstrong
}
\seealso{
\code{\link{boxCoxVariable}}}
\examples{
data(banks76)
num.dat <- banks76[,-(1:3)]
R_old <- cor(num.dat)
banks.trans <- optMVtrans(num.dat)
R_new <- cor(banks.trans)
R_new-R_old
}