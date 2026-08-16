% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_irt_link}
\alias{.morie_irt_link}
\title{Shared linking machinery for Haebara and Stocking-Lord. The two}
\usage{
.morie_irt_link(a_ref, b_ref, a_focal, b_focal, n_quad, theta_range, aggregate_first)
}
\arguments{
\item{a_ref}{See Usage.}
\item{b_ref}{See Usage.}
\item{a_focal}{See Usage.}
\item{b_focal}{See Usage.}
\item{n_quad}{See Usage.}
\item{theta_range}{Numeric; combined arithmetically in the body.}
\item{aggregate_first}{A flag; the body branches on it.}
}
\description{
differ ONLY in whether the curve differences are summed before or after squaring, which is exactly the difference between matching every item and matching the test as a whole.
}
\value{
A list with \code{A}, \code{B}, \code{criterion}, \code{a_transformed}, \code{b_transformed}, \code{n_items}, \code{converged}, \code{warnings}.
}
\keyword{internal}
