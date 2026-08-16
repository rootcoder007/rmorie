% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.varcal_norm_reads}
\alias{.varcal_norm_reads}
\title{.varcal_norm_reads}
\usage{
.varcal_norm_reads(reads)
}
\arguments{
\item{reads}{See Usage.}
}
\description{
Part of the varcal_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{lapply}.
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
