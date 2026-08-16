% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_pick_key}
\alias{.vepan_pick_key}
\title{Table 7\\'s order: canonical, then protein coding, then severity}
\usage{
.vepan_pick_key(r)
}
\arguments{
\item{r}{A list; the body reads \code{$biotype}, \code{$canonical}, \code{$most_severe}, \code{$transcript} from it.}
}
\description{
A step of the vepan_native implementation. Called by \code{morie_vepan_pick}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
