% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{patch_grid}
\alias{patch_grid}
\title{Patchify a latent and count tokens.}
\usage{
patch_grid(latent_size, patch)
}
\arguments{
\item{latent_size}{integer, side length I of the latent.}
\item{patch}{     integer, patch side p; p must divide I.}
}
\description{
Tokens: T = (I/p)^2. Halving p quadruples T at essentially constant parameter count -- the axis a parameter count cannot see.
}
\value{
list with tokens, grid, patch, latent_size, note.
}
\keyword{internal}
