% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LmsDescent}
\alias{LmsDescent}
\title{Eqs. (3.201)-(3.202): the instantaneous gradient is -2 e r, so}
\usage{
LmsDescent(w, e, rvec, mu)
}
\arguments{
\item{w}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{e}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rvec}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{mu}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
steepest descent gives w - mu grad, which is exactly Widrow-Hoff.
}
\value{
A list with \code{gradient}, \code{w_next}, \code{mu}, \code{e}, \code{order}, \code{equals_widrow_hoff}, \code{method}.
}
\keyword{internal}
\examples{
LmsDescent(w = c(1, 2, 3, 4, 5, 6, 7, 8), e = c(1, 2, 3, 4, 5, 6, 7, 8), rvec = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8))
}
