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
\item{x}{A matrix; passed to \code{nrow}.}
\item{dims}{A count; the body uses it as \code{matrix(...)}. Defaults to \code{2L}.}
\item{perplexity}{Numeric; passed to \code{log}. Defaults to \code{30}.}
\item{n_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1000L}.}
\item{eta}{Passed to \code{.morie_tsne_descent_cpp}. Defaults to \code{"auto"}.}
}
\description{
A step of the tsnrd implementation. Called by \code{morie_tsne_reduction}. See the file header for the source the module follows.
}
\value{
A list with \code{Y}, \code{kl}.
}
\keyword{internal}
