% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bayes_design}
\alias{.bayes_design}
\title{Internal helper: Bayes Design}
\usage{
.bayes_design(formula, data)
}
\description{
Internal helper: Bayes Design
}
\value{
A list with \code{y}, \code{X}, \code{p}.
}
\keyword{internal}
\examples{
.bayes_design(formula = y ~ x, data = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)))
}
