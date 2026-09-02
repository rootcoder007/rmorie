% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kscrit}
\alias{Kscrit}
\title{KS critical value (Table F) by exact bisection -- p. 565}
\usage{
Kscrit(n, alpha = 0.05, exact = TRUE) 
}
\description{
KS critical value (Table F) by exact bisection -- p. 565
}
\arguments{
\item{n}{Argument \code{n}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{exact}{Argument \code{exact}; see Usage.}
}
\value{
A list with \code{dcrit}, \code{dcrit_asymp}, \code{k_alpha}, \code{n}, \code{alpha}.
}
\keyword{internal}
\examples{
Kscrit(n = 5L)
}
