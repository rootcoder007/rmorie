% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_hungarian}
\alias{.s4_hungarian}
\title{# Optimal assignment, Kuhn-Munkres shortest-augmenting-path form.}
\usage{
.s4_hungarian(cost)
}
\arguments{
\item{cost}{See Usage.}
}
\description{
Exact, # O(n^3). Ties broken by the first strictly improving index so both arms # walk the same path. Returns 0-based column for each row.
}
\value{
The value of \code{ans}, as built in the body.
}
\keyword{internal}
