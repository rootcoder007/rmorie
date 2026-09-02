% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cv_risk}
\alias{cv_risk}
\title{Cross-validated risk per column of Z ---------------------------}
\usage{
cv_risk(y, Z, loss = "l2") 
}
\arguments{
\item{y}{Numeric; combined arithmetically in the body.}
\item{Z}{A matrix; indexed by row and column.}
\item{loss}{One of \code{"l2"}, \code{"nll"}. Defaults to \code{"l2"}.}
}
\description{
A step of the flxipt_native implementation. Called by \code{morie_tmlcic_adaptive_prespecification}, \code{super_learner}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cv_risk(V, V)
}
