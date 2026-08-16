% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_safrl}
\alias{morie_safrl}
\title{morie_safrl}
\usage{
morie_safrl(g, H, B = NULL, c = NULL, delta = 0.01, tol = 1e-12, max_iter = 5000)
}
\arguments{
\item{g}{Passed to \code{.safrl_vec}.}
\item{H}{Passed to \code{.safrl_mat}.}
\item{B}{Optional; may be \code{NULL}. Passed to \code{.safrl_mat}.}
\item{c}{Optional; may be \code{NULL}. Passed to \code{.safrl_vec}.}
\item{delta}{Numeric; combined arithmetically in the body. Defaults to \code{0.01}.}
\item{tol}{Passed to \code{.safrl_dual}. Defaults to \code{1e-12}.}
\item{max_iter}{Passed to \code{.safrl_dual}. Defaults to \code{5000}.}
}
\description{
A step of the safrl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.safrl_finish}.
}
\keyword{internal}
