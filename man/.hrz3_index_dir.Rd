% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_index_dir}
\alias{.hrz3_index_dir}
\title{Index direction with the scale normalisation |beta_1| = 1}
\usage{
.hrz3_index_dir(X, y, h)
}
\arguments{
\item{X}{Passed to \code{.hrz3_ade}.}
\item{y}{Passed to \code{.hrz3_ade}.}
\item{h}{Passed to \code{.hrz3_ade}.}
}
\description{
(Horowitz assumption HT2(a), p. 219).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.hrz3_index_dir(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5)
}
