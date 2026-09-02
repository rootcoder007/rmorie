% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Xlearn}
\alias{Xlearn}
\title{X-learner heterogeneous treatment effect (Kunzel et al. 2019)}
\usage{
Xlearn(tau1, tau0, g) 
}
\description{
X-learner heterogeneous treatment effect (Kunzel et al. 2019)
}
\arguments{
\item{tau1}{Argument \code{tau1}; see Usage.}
\item{tau0}{Argument \code{tau0}; see Usage.}
\item{g}{Argument \code{g}; see Usage.}
}
\value{
A list with \code{tau}, \code{ate}, \code{se}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(3)
rmorie:::Xlearn(tau1 = rnorm(20, 1), tau0 = rnorm(20, 0.8), g = runif(20, 0.3, 0.7))
}
