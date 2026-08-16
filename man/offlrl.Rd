% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{offlrl}
\alias{offlrl}
\title{offlrl}
\usage{
offlrl(dataset, states = NULL, actions = NULL, alpha = 1, gamma = 0.99, variant = "H", backup = "max", policy = NULL, mu = NULL, lr = 0.5, iters = 2000, tol = 1e-12)
}
\arguments{
\item{dataset}{See Usage.}
\item{states}{Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.}
\item{actions}{Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{0.99}.}
\item{variant}{One of \code{"H"}, \code{"mu"}, \code{"rho"}. Defaults to \code{"H"}.}
\item{backup}{One of \code{"max"}, \code{"pi"}. Defaults to \code{"max"}.}
\item{policy}{Passed to \code{.offlrl_as_dist}.}
\item{mu}{Passed to \code{.offlrl_as_dist}.}
\item{lr}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000}.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
A step of the offlrl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{value}, \code{greedy}, \code{behavior}, \code{counts}, \code{penalty}, \code{bellman_error}, \code{objective}, \code{alpha}, \code{variant}, \code{backup}, \code{n_transitions}, \code{method}.
}
\keyword{internal}
