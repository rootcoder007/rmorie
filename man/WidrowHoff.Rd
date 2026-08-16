% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WidrowHoff}
\alias{WidrowHoff}
\title{Eq. (3.203): w(n+1) = w(n) + 2 mu e(n) r(n).  The factor of two is in}
\usage{
WidrowHoff(w, e, rvec, mu)
}
\arguments{
\item{w}{See Usage.}
\item{e}{See Usage.}
\item{rvec}{See Usage.}
\item{mu}{See Usage.}
}
\description{
the book\\'s equation and is kept; folding it into mu silently halves every step size a reader transcribes from the text.  The stability bound 0 < mu < 1/lambda_max is reported against the input power.
}
\value{
A list with \code{w_next}, \code{mu}, \code{e}, \code{order}, \code{factor_of_two_is_in_the_equation}, \code{stable_bound}, \code{within_bound}, \code{method}.
}
\keyword{internal}
\examples{
WidrowHoff(w = c(1, 2, 3, 4, 5, 6, 7, 8), e = c(1, 2, 3, 4, 5, 6, 7, 8), rvec = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8))
}
