% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{central_path_dual}
\alias{central_path_dual}
\title{Central-path dual}
\usage{
central_path_dual(fvals, t)
}
\arguments{
\item{fvals}{Numeric vector of \eqn{f_i(x)} values.}
\item{t}{Positive scalar.}
}
\description{
Boyd equation 11.10: \code{lambda_i*(t) = -1 / (t f_i(x))}.
}
\value{
Numeric vector of dual multipliers.
}
\keyword{internal}
