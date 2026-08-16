% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Glr}
\alias{Glr}
\title{Eqs (8.30)-(8.31), after Appel and v. Brandt.  The reference window}
\usage{
Glr(x, m, n = NULL, order = 4)
}
\arguments{
\item{x}{See Usage.}
\item{m}{See Usage.}
\item{n}{Defaults to \code{NULL}.}
\item{order}{Defaults to \code{4}.}
}
\description{
GROWS from the start of the segment -- that is what separates GLR from the SEM and ACF methods.
}
\value{
A list with \code{d}, \code{h_pooled}, \code{h_reference}, \code{h_test}, \code{m}, \code{n}, \code{order}, \code{n_reference}, \code{n_test}, \code{reference_window_grows}, \code{near_zero_when_one_model_explains_both}, \code{method}.
}
\keyword{internal}
