% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SinCosTest}
\alias{SinCosTest}
\title{Bsasig}
\usage{
SinCosTest(n = NULL, f1 = 5, f2 = 20, a1 = 1, a2 = 1, fs = 100, duration = 1)
}
\arguments{
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{f1}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.}
\item{f2}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{20}.}
\item{a1}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{a2}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{100}.}
\item{duration}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_ccf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{x}, \code{t}, \code{n}, \code{fs}, \code{f1}, \code{f2}, \code{a1}, \code{a2}, \code{components_are_known_by_construction}, \code{method}.
}
\keyword{internal}
\examples{
SinCosTest()
}
