% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CtftF}
\alias{CtftF}
\title{Eq (3.76), the Hz spelling of eq (3.75); one implementation so the}
\usage{
CtftF(x, f, t = NULL, dt = NULL)
}
\arguments{
\item{x}{Passed to \code{Ctft}.}
\item{f}{Passed to \code{Ctft}.}
\item{t}{Passed to \code{Ctft}.}
\item{dt}{Passed to \code{Ctft}.}
}
\description{
two can never drift apart.
}
\value{
The value of \code{Ctft}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CtftF(V, V)
}
