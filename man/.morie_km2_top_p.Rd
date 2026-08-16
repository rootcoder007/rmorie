% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_top_p}
\alias{.morie_km2_top_p}
\title{Toppd\\\'s core: nucleus truncation of temperature-scaled logits}
\usage{
.morie_km2_top_p(z, p, T = 1)
}
\arguments{
\item{z}{See Usage.}
\item{p}{See Usage.}
\item{T}{Defaults to \code{1}.}
}
\description{
Part of the kamath_llm2_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{tensor}, \code{keep_mask}, \code{n_kept}.
}
\keyword{internal}
