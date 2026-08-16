% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svm_fit_binary}
\alias{.svm_fit_binary}
\title{Internal helper: fit one binary C-SVC and keep only its support vectors}
\usage{
.svm_fit_binary(X, y_pm1, C, ktype, gamma, coef0, degree, tol = 0.001, max_iter = 1e+06)
}
\description{
Internal helper: fit one binary C-SVC and keep only its support vectors
}
\value{
A list with \code{SV}, \code{coef}, \code{rho}, \code{n_sv}, \code{iterations}.
}
\keyword{internal}
