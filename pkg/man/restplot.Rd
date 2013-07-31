\encoding{UTF-8}
\name{restplot}
\alias{restplot}
\alias{restplotfun}
\title{Plot Item Response Functions for a Summated Rating Scale
}
\description{
This function plots the rest scale (i.e., the scale excluding item j) against the variable Xj.  
}
\usage{
restplot(X, jitter = 0)
restplotfun(x,y,jitter,...)
}
\arguments{
  \item{X}{A matrix of numeric variables that are being subjected to an item analysis in a summated rating scale. 
}
  \item{jitter}{The factor by which the y-values will be jittered
}
  \item{x}{In \code{restplotfun}, it is the variable on the x-axis of the plot}
  \item{y}{In \code{restplotfun}, it is the variable on the y-axis of the plot}
  \item{...}{Other arguments to be passed to \code{plot}}
}
\details{
This function plots the rest scale (i.e., the scale excluding item j) against the variable Xj.  This amounts to a visual investigation of the monotone homogeneity assumption in the summated rating model
}
\value{
A plot of each variable against its rest score (i.e., the item response function) and a plot of all item response functions in a single pane. 
}
\author{
Dave Armstrong
}
