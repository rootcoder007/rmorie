% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PvcBayes}
\alias{PvcBayes}
\title{Section 10.11.2 with the normal-pattern Bayes classifier of 10.6.2}
\usage{
PvcBayes(features, labels, priors = NULL, query = NULL) 
}
\arguments{
\item{features}{Passed to \code{.morie_bx_mat}.}
\item{labels}{Passed to \code{.morie_bx_vec}.}
\item{priors}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_vec}.}
\item{query}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_mat}.}
}
\description{
The linear rule of 10.11.1 commits to a hard boundary; the Bayes rule states the posterior odds and lets the prevalence of ectopy enter, which matters because a PVC prior is far below one half.
}
\value{
A list with \code{predictions}, \code{queryclass}, \code{posterior}, \code{means}, \code{covariances}, \code{scale}, \code{confusion}, \code{accuracy}, \code{sensitivity}, \code{specificity}, \code{priors}, \code{classes}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(5)
f <- rbind(matrix(rnorm(40, 0), 20, 2), matrix(rnorm(40, 2), 20, 2))
lab <- rep(c(0L, 1L), each = 20)
PvcBayes(f, lab)
}
