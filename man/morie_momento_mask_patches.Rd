% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_momento_mask_patches}
\alias{morie_momento_mask_patches}
\title{Mask named patches with a fill value}
\usage{
morie_momento_mask_patches(patches, mask_idx, fill = 0)
}
\arguments{
\item{patches}{A list of numeric patches.}
\item{mask_idx}{Integer indices of patches to mask.}
\item{fill}{Fill value.}
}
\description{
Mask named patches with a fill value
}
\value{
A list with masked, mask, mask_idx, mask_rate, n_patches.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_momento_mask_patches(V, M)
}
