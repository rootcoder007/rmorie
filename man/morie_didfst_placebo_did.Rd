% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_didfst_placebo_did}
\alias{morie_didfst_placebo_did}
\title{Pre-period placebo}
\usage{
morie_didfst_placebo_did(Y, D, event_time, split = NULL)
}
\arguments{
\item{Y}{Numeric panel (n by T).}
\item{D}{Numeric vector of 0/1 adoption indicators.}
\item{event_time}{Integer H.}
\item{split}{Optional integer, period at which the pre-period is   split.}
}
\description{
Split the pre-period in two and run the whole estimator inside it, where the true effect is zero by non-anticipation (Assumption 13.1).
}
\value{
A list with \code{estimate} and the change components.
}
\keyword{internal}
\examples{
set.seed(3)
Y <- matrix(rnorm(48, 10), 8, 6)
D <- rep(c(1, 0), 4)
r <- morie_didfst_placebo_did(Y, D, event_time = 4)
str(r, max.level = 1)
}
