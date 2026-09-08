% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_classify_pair}
\alias{morie_sv_dl_classify_pair}
\title{morie_sv_dl_classify_pair}
\usage{
morie_sv_dl_classify_pair(p, median, sd, orientation = c("+", "-"), n_sd = 3) 
}
\arguments{
\item{p}{A list; the body reads \code{$chrom1}, \code{$chrom2}, \code{$strand1}, \code{$strand2} from it.}
\item{median}{Numeric; combined arithmetically in the body.}
\item{sd}{Numeric; combined arithmetically in the body.}
\item{orientation}{A vector; indexed elementwise. Defaults to \code{c("+", "-")}.}
\item{n_sd}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_paired_end_calls}. See the file header for the source the module follows.
}
\value{
Nothing; the function is called for its effect.
}
\keyword{internal}
\examples{
p <- list(chrom1 = "chr1", pos1 = 1000, strand1 = "+",
          chrom2 = "chr1", pos2 = 3000, strand2 = "-")
morie_sv_dl_classify_pair(p, median = 300, sd = 15)
}
