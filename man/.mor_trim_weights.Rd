% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_trim_weights}
\alias{.mor_trim_weights}
\title{Cap the IPW WEIGHTS at percentile cutpoints -- the operation Lee,}
\usage{
.mor_trim_weights(w, weight_trim = NULL, side = "upper")
}
\arguments{
\item{w}{See Usage.}
\item{weight_trim}{Defaults to \code{NULL}.}
\item{side}{One of \code{"both"}, \code{"upper"}. Defaults to \code{"upper"}.}
}
\description{
B. K., Lessler, J. and Stuart, E. A. (2011), "Weight Trimming and Propensity Score Weighting", PLoS ONE 6(3), e18174, actually perform: "we trimmed high weights downwards, with cutpoints ranging from the 99th to the 50th percentiles ... all weights with value above the [cutpoint] were set equal to the [cutpoint]".  Note that they cap the HIGH side only, which is why side = "upper" is the default here. Contrast .mor_trim_ps, which caps the SCORES. See also Cole, S. R. and Hernan, M. A. (2008), American Journal of Epidemiology 168(6), 656-664.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
