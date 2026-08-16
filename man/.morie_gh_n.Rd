% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gh_n}
\alias{.morie_gh_n}
\title{N is kept as a DOUBLE, not coerced with as.integer(): these rate}
\usage{
.morie_gh_n(n)
}
\arguments{
\item{n}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
formulas are routinely evaluated at hypothetical sample sizes like 1e10, which overflow R\\'s 32-bit integer and silently become NA. Python\\'s int has no such limit, so coercing here would break parity exactly where the asymptotics become interesting.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
\examples{
.morie_gh_n(n = 5L)
}
