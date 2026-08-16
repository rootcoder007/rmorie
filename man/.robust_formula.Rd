% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.robust_formula}
\alias{.robust_formula}
\title{Internal helper: Robust Formula}
\usage{
.robust_formula(response, terms, data)
}
\description{
Internal helper: Robust Formula
}
\value{
The value of \code{stats::reformulate}.
}
\keyword{internal}
\examples{
.robust_formula(response = c(1, 2, 3, 4, 5, 6, 7, 8), terms = c("a", "b", "c"), data = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)))
}
