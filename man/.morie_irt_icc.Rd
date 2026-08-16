% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_irt_icc}
\alias{.morie_irt_icc}
\title{Two-parameter logistic ICC with the 1.7 normal-metric scaling}
\usage{
.morie_irt_icc(a, b, theta)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{Passed to \code{outer}.}
\item{theta}{A vector; its length is taken.}
}
\description{
A step of the survey_psych_native implementation. Called by \code{.morie_irt_link}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_irt_icc(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = c(1, 2, 3, 4, 5, 6, 7, 8), theta = 0.5)
}
