% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{build_tree}
\alias{build_tree}
\title{The recursion of Algorithm 3, returning the paper\'s nine values:}
\usage{
build_tree(theta, r, logu, v, j, eps, logp, grad, rnd, joint0)
}
\arguments{
\item{theta}{Passed to \code{leapfrog}.}
\item{r}{Passed to \code{leapfrog}.}
\item{logu}{Numeric; combined arithmetically in the body.}
\item{v}{Numeric; combined arithmetically in the body.}
\item{j}{Numeric; combined arithmetically in the body.}
\item{eps}{Numeric; combined arithmetically in the body.}
\item{logp}{Passed to \code{.joint}.}
\item{grad}{Passed to \code{leapfrog}.}
\item{rnd}{Passed to \code{build_tree}.}
\item{joint0}{Numeric; combined arithmetically in the body.}
}
\description{
the new trajectory ends (theta_minus, r_minus, theta_plus, r_plus), the candidate sample t_p, and the tree\'s bookkeeping (count, indicator, summed alpha, alpha normaliser).
}
\value{
A list, whose contents depend on the branch taken; across the branches its names are \code{tm}, \code{rm}, \code{tp}, \code{rp}, \code{t_p}, \code{n}, \code{s}, \code{alpha}, \code{na}.
}
\keyword{internal}
