% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsWSqDrv}
\alias{QrsWSqDrv}
\title{Eq (4.4) (Murthy and Rangaraj): g1(n) = sum_i |x(n-i+1) - x(n-i)|^2}
\usage{
QrsWSqDrv(x, nwin = 8) 
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{nwin}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{8}.}
}
\description{
(N - i + 1).  Squaring rewards the large QRS differences; the linearly falling weight smooths the result without a separate filter.
}
\value{
A list with \code{g1}, \code{nwin}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(6)
fs <- 200
n <- fs * 3
x <- rep(0, n)
for (b in seq(0.4, 2.8, by = 0.8)) {
  i <- round(b * fs); x[i:(i + 4)] <- c(0.3, 1.2, -0.4, 0.1, 0)
}
x <- x + 0.01 * rnorm(n)
r <- QrsWSqDrv(x)
str(r, max.level = 1)
}
