% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nashq_is_saddle}
\alias{.nashq_is_saddle}
\title{Definition 13: each agent gains when the OTHER deviates}
\usage{
.nashq_is_saddle(A, B, p, q, tol)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{B}{Passed to \code{.nashq_payoff}.}
\item{p}{Passed to \code{.nashq_payoff}.}
\item{q}{Passed to \code{.nashq_payoff}.}
\item{tol}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the nashq_native implementation. Called by \code{.nashq_select}, \code{stage_game_type}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
