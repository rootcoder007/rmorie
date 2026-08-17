% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_varcal}
\alias{morie_varcal}
\title{morie_varcal}
\usage{
morie_varcal(reads, reference, scorer = NULL, min_quality = 10, ...)
}
\arguments{
\item{reads}{Passed to \code{.varcal_norm_reads}.}
\item{reference}{Passed to \code{.varcal_chars}.}
\item{scorer}{Passed to \code{varcal_genotype_posterior}.}
\item{min_quality}{Passed to \code{>=}. Defaults to \code{10}.}
\item{...}{Passed through.}
}
\description{
A step of the varcal_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{candidates}, \code{n_candidates}, \code{calls}, \code{n_called}, \code{method}.
}
\references{
----------
Poplin, R., Newburger, D., Dijamco, J., Nguyen, N., Loy, D., Gross,
S. S., McLean, C. Y. & DePristo, M. A. (2016) "Creating a universal
SNP and small indel variant caller with deep neural networks",
bioRxiv 092890, doi:10.1101/092890 (published as Poplin et al. (2018)
*Nature Biotechnology* 36(10), 983-987,
doi:10.1038/nbt.4235). The high-sensitivity/low-specificity candidate
stage, the pileup image of reference and read data around each
candidate, the Inception-v2 classifier emitting the three diploid
genotype probabilities, the calibration argument for why an image of
all reads captures inter-read dependence, and the printed accounting:
candidate PPV 8.1\% raised to 99.7\% on Ion Torrent, a mean loss of 2.3\%
of candidate sensitivity, and the SOLiD figures of 13.9\% PPV at 96.2\%
sensitivity.

Li, H. (2011) "A statistical framework for SNP calling, mutation
discovery, association mapping and population genetical parameter
estimation from sequencing data", *Bioinformatics* 27(21), 2987-2993,
doi:10.1093/bioinformatics/btr509, for the conventional genotype
likelihood this replaces.
}
\keyword{internal}
\examples{
reads <- c(
  lapply(1:3, function(i) list(pos = 0L, seq = "ACGTTCGT")),
  lapply(1:3, function(i) list(pos = 0L, seq = "ACGTACGT")))
r <- morie_varcal(reads, reference = "ACGTACGTAC")
str(r, max.level = 1)
}
