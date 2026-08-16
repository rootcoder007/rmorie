% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_momento_reconstruction_curve}
\alias{morie_momento_reconstruction_curve}
\title{Reconstruction error vs mask rate}
\usage{
morie_momento_reconstruction_curve(patches, reconstructor, rates, seed = 0)
}
\arguments{
\item{patches}{Patch list.}
\item{reconstructor}{Function (masked, mask) -> reconstruction.}
\item{rates}{Sequence of mask rates in (0, 1).}
\item{seed}{Seed for the shared generator.}
}
\description{
Reconstruction error vs mask rate
}
\value{
A list with curve, n_patches, rates, mse.
}
\keyword{internal}
