% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlseqsl_sequential_super_learner}
\alias{morie_tlseqsl_sequential_super_learner}
\title{morie_tlseqsl_sequential_super_learner}
\usage{
morie_tlseqsl_sequential_super_learner(histories, outcomes, library, T, V = 5, seed = 0) 
}
\arguments{
\item{histories}{A vector; its length is taken and its elements indexed.}
\item{outcomes}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{library}{Passed to \code{morie_tlseqsl_ensemble_super_learner}.}
\item{T}{Numeric; combined arithmetically in the body.}
\item{V}{Passed to \code{morie_tlseqsl_ensemble_super_learner}. Defaults to \code{5}.}
\item{seed}{Passed to \code{morie_tlseqsl_ensemble_super_learner}. Defaults to \code{0}.}
}
\description{
A step of the tlseqsl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{mean}, \code{sequential_fits}, \code{T}, \code{method}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 3.

van der Laan, M. J., Polley, E. C. & Hubbard, A. E. (2007) "Super
Learner", Statistical Applications in Genetics and Molecular Biology
6(1), Article 25, doi:10.2202/1544-6115.1309.

Bang, H. & Robins, J. M. (2005) "Doubly robust estimation in missing
data and causal inference models", Biometrics 61(4), 962-973,
doi:10.1111/j.1541-0420.2005.00377.x.
}
\keyword{internal}
