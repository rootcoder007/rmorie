% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qda}
\alias{Qda}
\title{Eq (10.73) with the mean and covariance estimated per class by}
\usage{
Qda(X, y, query, priors = NULL)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{A vector; its length is taken.}
\item{query}{See Usage.}
\item{priors}{Defaults to \code{NULL}.}
}
\description{
eqs (10.68)-(10.69).  Each class keeps its OWN covariance, so the boundaries are quadrics; a single pooled covariance would make this LDA.  A class needs more samples than features or its covariance is singular -- QDA estimates p(p+1)/2 covariance parameters PER CLASS, so it is the first thing to break on small samples.
}
\value{
A list with \code{g}, \code{assigned}, \code{assigned_index}, \code{classes}, \code{means}, \code{covariances}, \code{priors}, \code{reduces_to_lda_when_covariances_are_equal}, \code{parameters_per_class}, \code{method}.
}
\keyword{internal}
