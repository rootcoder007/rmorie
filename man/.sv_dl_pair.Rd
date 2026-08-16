% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sv_dl_pair}
\alias{.sv_dl_pair}
\title{Normalise one read pair; mate 1 is the left-most alignment}
\usage{
.sv_dl_pair(p)
}
\arguments{
\item{p}{A list; the body reads \code{$chrom1}, \code{$chrom2}, \code{$id}, \code{$len1}, \code{$len2}, \code{$pos1}, \code{$pos2}, \code{$read_length}, \code{$seq}, \code{$strand1}, \code{$strand2} from it.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_classify_pair}. See the file header for the source the module follows.
}
\value{
A list with \code{chrom1}, \code{pos1}, \code{strand1}, \code{len1}, \code{chrom2}, \code{pos2}, \code{strand2}, \code{len2}, \code{seq}, \code{id}.
}
\keyword{internal}
