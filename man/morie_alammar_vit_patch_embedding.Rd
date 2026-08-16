% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_vit_patch_embedding}
\alias{morie_alammar_vit_patch_embedding}
\title{ViT patch embedding (Dosovitskiy et al. 2021)}
\usage{
morie_alammar_vit_patch_embedding(image, patch_size, E, cls_token = NULL, E_pos = NULL)
}
\arguments{
\item{image}{Numeric matrix; must tile exactly.}
\item{patch_size}{P.}
\item{E}{(P^2 x d) projection.}
\item{cls_token,E_pos}{Optional class token and positional table.}
}
\description{
ViT patch embedding (Dosovitskiy et al. 2021)
}
\value{
A list with \code{sequence}, \code{n_patches}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
img <- matrix(0:15, 4, 4, byrow = TRUE)
morie_alammar_vit_patch_embedding(img, 2, diag(4))
}
