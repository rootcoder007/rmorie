% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_tsne}
\alias{.morie_tsne}
\title{Exact t-SNE core. Returns list(Y, kl)}
\usage{
.morie_tsne(x, dims = 2L, perplexity = 30, n_iter = 1000L, eta = "auto")
}
\arguments{
\item{x}{See Usage.}
\item{dims}{Defaults to \code{2L}.}
\item{perplexity}{Defaults to \code{30}.}
\item{n_iter}{Defaults to \code{1000L}.}
\item{eta}{Defaults to \code{"auto"}.}
}
\description{
Part of the tsnrd implementation; see the file header for the source it follows.
}
\value{
A list with \code{Y}, \code{kl}.
}
\keyword{internal}
