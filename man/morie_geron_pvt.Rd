% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_pvt}
\alias{morie_geron_pvt}
\title{Pyramid Vision Transformer: hierarchical multi-scale patch-embedding stages (Geron Ch 16, hmpvt)}
\usage{
morie_geron_pvt(image, stage_cfgs, seed = 0) 
}
\arguments{
\item{image}{Array (H, W, C) or (H, W).}
\item{stage_cfgs}{List of stage specs.}
\item{seed}{LCG seed for default projections.}
}
\description{
Pyramid Vision Transformer: hierarchical multi-scale patch-embedding stages (Geron Ch 16, hmpvt)
}
\value{
A list with \code{tokens}, \code{stages}, \code{output_shape}, \code{n_parameters}, \code{attention_cost}, \code{full_attention_cost}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
img <- array(0:47, dim = c(3, 4, 4)); img <- aperm(img, c(3, 2, 1))  # numpy arange(48).reshape(4,4,3), row-major
Wm <- matrix(1 / 12, 12, 1)
r <- morie_geron_pvt(img, list(list(patch_size = 2, dim = 1, W = Wm)))
r$output_shape
r$tokens[1, 1, 1]
}
