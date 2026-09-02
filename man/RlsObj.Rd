% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RlsObj}
\alias{RlsObj}
\title{Eq. (3.206): xi = sum lambda^(n-i) e^2(i).  lambda < 1 discounts old}
\usage{
RlsObj(errors, lam) 
}
\arguments{
\item{errors}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lam}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
errors, giving an effective memory of 1/(1-lambda) samples; lambda = 1 is the growing window, which never forgets and so cannot track.
}
\value{
A list with \code{xi}, \code{weights}, \code{lam}, \code{n}, \code{memory}, \code{growing_window}, \code{method}.
}
\keyword{internal}
\examples{
RlsObj(c(1, 1, 1), 0.5)
}
