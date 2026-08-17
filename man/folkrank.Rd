% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{folkrank}
\alias{folkrank}
\title{folkrank}
\usage{
folkrank(triples, focus, d = 0.7, weight = 0.9, iters = 200)
}
\arguments{
\item{triples}{Passed to \code{tripartite_graph}.}
\item{focus}{Passed to \code{preference_vector}.}
\item{d}{Passed to \code{adapted_pagerank}. Defaults to \code{0.7}.}
\item{weight}{Passed to \code{preference_vector}. Defaults to \code{0.9}.}
\item{iters}{Passed to \code{adapted_pagerank}. Defaults to \code{200}.}
}
\description{
A step of the tagRC_native implementation. Called by \code{morie_tagRC}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{ranking}, \code{difference}, \code{with_preference}, \code{without_preference}, \code{undifferenced_ranking}, \code{baseline_ranking}, \code{focus}, \code{n_nodes}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
triples <- list(c("u1", "t1", "r1"), c("u1", "t2", "r2"),
                c("u2", "t1", "r1"), c("u2", "t3", "r3"))
folkrank(triples, focus = "t:t1")
}
