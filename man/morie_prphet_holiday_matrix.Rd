% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prphet_holiday_matrix}
\alias{morie_prphet_holiday_matrix}
\title{morie_prphet_holiday_matrix}
\usage{
morie_prphet_holiday_matrix(t, holidays, lower = 0, upper = 0)
}
\arguments{
\item{t}{See Usage.}
\item{holidays}{See Usage.}
\item{lower}{Defaults to \code{0}.}
\item{upper}{Defaults to \code{0}.}
}
\description{
Part of the prphet_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{rows}, \code{names}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prphet_holiday_matrix(V, V)
}
