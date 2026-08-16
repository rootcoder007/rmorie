% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FDiff}
\alias{FDiff}
\title{Eq (3.123).  The 1/T is not cosmetic: the book is explicit that it}
\usage{
FDiff(x, T = 1, n = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{T}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
"is required in order to obtain the rate of change of the signal with respect to the true time".  Highpass, and it amplifies noise -- eq (3.128) is the book\'s remedy.
}
\value{
A list with \code{y}, \code{value}, \code{index}, \code{T}, \code{scale_factor_gives_true_time_rate}, \code{highpass}, \code{amplifies_noise}, \code{removes_dc}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
FDiff(V)
}
