% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_hgvs_p}
\alias{.vepan_hgvs_p}
\title{.vepan_hgvs_p}
\usage{
.vepan_hgvs_p(info, terms)
}
\arguments{
\item{info}{A list; the body reads \code{$alt_aa}, \code{$protein_position}, \code{$ref_aa} from it.}
\item{terms}{One of \code{"frameshift_variant"}, \code{"start_retained_variant"}, \code{"stop_retained_variant"}, \code{"synonymous_variant"}.}
}
\description{
A step of the vepan_native implementation. Called by \code{morie_vepan_annotate}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
