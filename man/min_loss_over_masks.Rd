% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{min_loss_over_masks}
\alias{min_loss_over_masks}
\title{min_loss_over_masks}
\usage{
min_loss_over_masks(predictions, target, loss_fn)
}
\arguments{
\item{predictions}{A vector; its length is taken.}
\item{target}{See Usage.}
\item{loss_fn}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the sammkr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{loss}, \code{index}, \code{losses}, \code{mean_loss}, \code{gap}, \code{note}.
}
\keyword{internal}
