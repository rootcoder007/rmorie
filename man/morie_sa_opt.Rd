% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sa_opt}
\alias{morie_sa_opt}
\title{morie_sa_opt}
\usage{
morie_sa_opt(fun, x0, step = 1, T0 = 1, n_iter = 1000, schedule = "geometric", alpha = 0.99, lower = NULL, upper = NULL, seed = 0)
}
\arguments{
\item{fun}{See Usage.}
\item{x0}{See Usage.}
\item{step}{Defaults to \code{1}.}
\item{T0}{Defaults to \code{1}.}
\item{n_iter}{Defaults to \code{1000}.}
\item{schedule}{Defaults to \code{"geometric"}.}
\item{alpha}{Defaults to \code{0.99}.}
\item{lower}{Defaults to \code{NULL}.}
\item{upper}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the sa_opt_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
