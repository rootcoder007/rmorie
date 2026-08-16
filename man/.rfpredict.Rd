% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfpredict}
\alias{.rfpredict}
\title{Yhat_i = (1/B) sum_b T_b(x_i), the p. 640 aggregation}
\usage{
.rfpredict(trees, Xnew, q)
}
\arguments{
\item{trees}{A vector; its length is taken.}
\item{Xnew}{A matrix; indexed by row and column.}
\item{q}{A count; the body uses it as \code{numeric(...)}.}
}
\description{
A step of the mvsml_rf_shared implementation. Called by \code{Rfmlt}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
