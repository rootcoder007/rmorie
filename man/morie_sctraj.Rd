% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj}
\alias{morie_sctraj}
\title{morie_sctraj}
\usage{
morie_sctraj(X, labels, root, ends = NULL, cov = "full", max_iter = 15, shrink = TRUE, weight_arg = "shifted", span = 0.4, n_points = 100)
}
\arguments{
\item{X}{See Usage.}
\item{labels}{See Usage.}
\item{root}{See Usage.}
\item{ends}{Defaults to \code{NULL}.}
\item{cov}{Defaults to \code{"full"}.}
\item{max_iter}{Defaults to \code{15}.}
\item{shrink}{Defaults to \code{TRUE}.}
\item{weight_arg}{Defaults to \code{"shifted"}.}
\item{span}{Defaults to \code{0.4}.}
\item{n_points}{Defaults to \code{100}.}
}
\description{
Part of the sctraj_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{pseudotime}, \code{weights}, \code{lineages}, \code{curves}, \code{tree}, \code{distance}, \code{clusters}, \code{centers}, \code{n_lineages}, \code{root}, \code{cov}, \code{shrink}, \code{method}, \code{note}.
}
\keyword{internal}
