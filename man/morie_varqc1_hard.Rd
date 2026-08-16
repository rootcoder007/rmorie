% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_varqc1_hard}
\alias{morie_varqc1_hard}
\title{Apply each hard threshold independently}
\usage{
morie_varqc1_hard(records, fields, thresholds)
}
\arguments{
\item{records}{Annotation matrix, one row per variant.}
\item{fields}{Column names.}
\item{thresholds}{A list of annotation, direction, cutoff triples.}
}
\description{
A missing annotation does NOT fail its filter. A caller omits an annotation when it could not be computed, and treating "not measured" as "measured and bad" would discard exactly the variants with unusual read support -- which is where real novel variation lives.
}
\value{
A list with one FILTER string per record and the counts.
}
\references{
  Broad Institute, GATK Best Practices: "Hard-filtering germline short
    variants," the source of the threshold values above.
  DePristo, M.A. et al. (2011) "A framework for variation discovery
    and genotyping using next-generation DNA sequencing data." Nature
    Genetics 43(5), 491-498.
  Van der Auwera, G.A. et al. (2013) "From FastQ data to
    high-confidence variant calls." Current Protocols in
    Bioinformatics 43, 11.10.1-11.10.33.
  Dempster, A.P., Laird, N.M. and Rubin, D.B. (1977) JRSS B 39(1),
    1-38.
}
\keyword{internal}
\examples{
morie_varqc1_hard(records = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), fields = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), thresholds = c(1, 2, 3, 4, 5, 6, 7, 8))
}
