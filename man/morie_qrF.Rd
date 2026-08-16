% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_qrF}
\alias{morie_qrF}
\title{morie_qrF}
\usage{
morie_qrF(y_true, y_pred, theta = 0.5)
}
\arguments{
\item{y_true}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y_pred}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{theta}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
}
\description{
A step of the qrF_mixedcase_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{total}, \code{losses}, \code{theta}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_qrF(V, V)
}
