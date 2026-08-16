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
\item{t}{A vector; its length is taken and its elements indexed.}
\item{holidays}{A vector; indexed elementwise.}
\item{lower}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{upper}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the prphet_native implementation. Called by \code{morie_prphet_design}, \code{prophe_additive_components}. See the file header for the source the module follows.
}
\value{
A list with \code{rows}, \code{names}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prphet_holiday_matrix(V, V)
}
