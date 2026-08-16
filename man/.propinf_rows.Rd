% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_rows}
\alias{.propinf_rows}
\title{.propinf_rows}
\usage{
.propinf_rows(X)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
}
\description{
A step of the propinf_native implementation. Called by \code{morie_propinf_fcnn_predict}, \code{morie_propinf_train_fcnn}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.propinf_rows(V)
}
