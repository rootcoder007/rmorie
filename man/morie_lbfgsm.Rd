% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_lbfgsm}
\alias{morie_lbfgsm}
\title{morie_lbfgsm}
\usage{
morie_lbfgsm(fun, x0, grad, m = 10, max_iter = 200, tol = 1e-08, c1 = 1e-04, c2 = 0.9, max_ls = 60)
}
\arguments{
\item{fun}{Accepted by the signature and not used anywhere in the body.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{grad}{Accepted by the signature and not used anywhere in the body.}
\item{m}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{tol}{Passed to \code{<=}. Defaults to \code{1e-08}.}
\item{c1}{Numeric; combined arithmetically in the body. Defaults to \code{1e-04}.}
\item{c2}{Numeric; combined arithmetically in the body. Defaults to \code{0.9}.}
\item{max_ls}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60}.}
}
\description{
A step of the lbfgsm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{x}, \code{fun}, \code{grad}, \code{grad_norm}, \code{iterations}, \code{n_fun}, \code{memory}, \code{converged}, \code{history}, \code{method}.
}
\keyword{internal}
