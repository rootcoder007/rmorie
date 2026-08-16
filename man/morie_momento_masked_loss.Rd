% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_momento_masked_loss}
\alias{morie_momento_masked_loss}
\title{Masked-position MSE}
\usage{
morie_momento_masked_loss(truth, reconstruction, mask)
}
\arguments{
\item{truth,reconstruction}{Patch lists.}
\item{mask}{Boolean vector of length n_patches.}
}
\description{
Masked-position MSE
}
\value{
A list with mse, n_scored, scored.
}
\keyword{internal}
\examples{
morie_momento_masked_loss(truth = c(1, 2, 3, 4, 5, 6, 7, 8), reconstruction = c(1, 2, 3, 4, 5, 6, 7, 8), mask = c(1, 2, 3, 4, 5, 6, 7, 8))
}
