% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gammainc_q}
\alias{.morie_gammainc_q}
\title{Regularized UPPER incomplete gamma Q(a, x), computed directly so the}
\usage{
.morie_gammainc_q(a, x)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{x}{Numeric; passed to \code{log}.}
}
\description{
far tail never passes through 1 - P and lose its digits.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
