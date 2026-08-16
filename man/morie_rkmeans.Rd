% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rkmeans}
\alias{morie_rkmeans}
\title{morie_rkmeans}
\usage{
morie_rkmeans(X, k = 2, alpha = 0.1, penalty = "square", n_start = 20, max_iter = 100, huber_c = 1.345, seed = 0, centers = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{k}{Defaults to \code{2}.}
\item{alpha}{Defaults to \code{0.1}.}
\item{penalty}{Defaults to \code{"square"}.}
\item{n_start}{Defaults to \code{20}.}
\item{max_iter}{Defaults to \code{100}.}
\item{huber_c}{Defaults to \code{1.345}.}
\item{seed}{Defaults to \code{0}.}
\item{centers}{Defaults to \code{NULL}.}
}
\description{
Part of the rkmeans_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{centers}, \code{labels}, \code{kept}, \code{outliers}, \code{criterion}, \code{distances}, \code{sizes}, \code{n_trimmed}, \code{n_kept}, \code{alpha}, \code{k}, \code{penalty}, \code{method}.
}
\keyword{internal}
