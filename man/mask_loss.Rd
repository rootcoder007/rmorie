% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mask_loss}
\alias{mask_loss}
\title{mask_loss}
\usage{
mask_loss(logits, target, decoupled = TRUE)
}
\arguments{
\item{logits}{Passed to \code{.masrcn_mat}.}
\item{target}{Passed to \code{.masrcn_mat}.}
\item{decoupled}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the masrcn_native implementation. Called by \code{morie_masrcn}. See the file header for the source the module follows.
}
\value{
A list with \code{loss}, \code{kind}, \code{caveat}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
mask_loss(V, V)
}
