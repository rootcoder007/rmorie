% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svm_gamma}
\alias{.svm_gamma}
\title{Internal helper: resolve the gamma argument}
\usage{
.svm_gamma(gamma, x)
}
\description{
`"scale"` is 1 / (p * var(x)) and `"auto"` is 1 / p, matching the convention these functions have always used. LIBSVM\'s own default is 1 / p, i.e. `"auto"`.
}
\value{
A vector.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.svm_gamma(V, V)
}
