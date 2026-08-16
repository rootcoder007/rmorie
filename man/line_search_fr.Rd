% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{line_search_fr}
\alias{line_search_fr}
\title{line_search_fr}
\usage{
line_search_fr(f, grad, x, p, f0, g0, est = NULL, max_double = 60L, max_cubic = 40L, tol = 1e-12)
}
\arguments{
\item{f}{See Usage.}
\item{grad}{See Usage.}
\item{x}{A vector; its length is taken.}
\item{p}{Numeric; combined arithmetically in the body.}
\item{f0}{Numeric; combined arithmetically in the body.}
\item{g0}{Passed to \code{.cgnonl_dot}.}
\item{est}{Defaults to \code{NULL}.}
\item{max_double}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.}
\item{max_cubic}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{40L}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-12}.}
}
\description{
A step of the cgnonl_native implementation. Called by \code{nonlinear_cg}. See the file header for the source the module follows.
}
\value{
A list with \code{t}, \code{x_new}, \code{f_new}, \code{g_new}, \code{n_eval}.
}
\keyword{internal}
