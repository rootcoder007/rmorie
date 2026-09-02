% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{adaptive_density_control}
\alias{adaptive_density_control}
\title{Adaptive density control: clone, split or prune each Gaussian}
\usage{
adaptive_density_control(gradients, scales, opacities, grad_threshold = 2e-04, scale_threshold = 0.01, 
    opacity_threshold = 0.005) 
}
\arguments{
\item{gradients}{Per-Gaussian positional gradient.}
\item{scales}{Per-Gaussian scale (max component).}
\item{opacities}{Per-Gaussian opacity.}
\item{grad_threshold}{Gradient above which a Gaussian is updated.}
\item{scale_threshold}{Scale above which large-gradient Gaussians   are split instead of cloned.}
\item{opacity_threshold}{Below which a Gaussian is pruned.}
}
\description{
Adaptive density control: clone, split or prune each Gaussian
}
\value{
List with clone, split, prune, n_before, n_after, method,   note.
}
\keyword{internal}
\examples{
adaptive_density_control(gradients = c(1, 2, 3, 4, 5, 6, 7, 8), scales = c(1, 2, 3, 4, 5, 6, 7, 8), opacities = c(1, 2, 3, 4, 5, 6, 7, 8))
}
