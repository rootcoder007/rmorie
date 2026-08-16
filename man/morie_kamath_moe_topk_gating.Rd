% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_moe_topk_gating}
\alias{morie_kamath_moe_topk_gating}
\title{morie_kamath_moe_topk_gating}
\usage{
morie_kamath_moe_topk_gating(x, W_g, k = 2)
}
\arguments{
\item{W_g}{Gate projection, d x n.}
\item{k}{Experts kept.}
}
\description{
morie_kamath_moe_topk_gating
}
\value{
A list with \code{weights}, \code{selected_experts}, \code{n_active}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_moe_topk_gating(1, rbind(c(3, 1, 2, 0.5)), k = 2)
}
