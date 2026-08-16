% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tsne_pcond}
\alias{.tsne_pcond}
\title{.tsne_pcond}
\usage{
.tsne_pcond(D2, perp, tol = 1e-05, max_iter = 60)
}
\arguments{
\item{D2}{A matrix; indexed by row and column.}
\item{perp}{Numeric; passed to \code{log}.}
\item{tol}{Defaults to \code{1e-05}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60}.}
}
\description{
A step of the sctsne_native implementation. Called by \code{morie_sctsne}. See the file header for the source the module follows.
}
\value{
The value of \code{P}, as built in the body.
}
\keyword{internal}
