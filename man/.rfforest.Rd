% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfforest}
\alias{.rfforest}
\title{.rfforest}
\usage{
.rfforest(X, Y, n_trees, nodesize, mtry, q)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{Y}{Passed to \code{.rfgrow}.}
\item{n_trees}{A count; the body uses it as \code{seq_len(...)}.}
\item{nodesize}{Passed to \code{.rfgrow}.}
\item{mtry}{Passed to \code{.rfgrow}.}
\item{q}{Passed to \code{.rfgrow}.}
}
\description{
A step of the mvsml_rf_shared implementation. Called by \code{Rfmdi}, \code{Rfmlt}, \code{Rfpmi}. See the file header for the source the module follows.
}
\value{
A list with \code{trees}, \code{oob}.
}
\keyword{internal}
