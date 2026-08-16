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
\item{x}{See Usage.}
\item{p}{See Usage.}
\item{f0}{See Usage.}
\item{g0}{See Usage.}
\item{est}{Defaults to \code{NULL}.}
\item{max_double}{Defaults to \code{60L}.}
\item{max_cubic}{Defaults to \code{40L}.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
Part of the cgnonl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{t}, \code{x_new}, \code{f_new}, \code{g_new}, \code{n_eval}.
}
\keyword{internal}
