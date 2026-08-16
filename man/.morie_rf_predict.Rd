% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rf_predict}
\alias{.morie_rf_predict}
\title{Internal helper: predict from a native random forest on new data}
\usage{
.morie_rf_predict(fit, X)
}
\description{
Internal helper: predict from a native random forest on new data
}
\value{
The value of \code{factor}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_rf_predict(D, V)
}
