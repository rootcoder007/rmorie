% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlseqsl_loss}
\alias{.tlseqsl_loss}
\title{.tlseqsl_loss}
\usage{
.tlseqsl_loss(kind, y, p)
}
\arguments{
\item{kind}{Compared against \code{"squared"}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{p}{Numeric; passed to \code{max}.}
}
\description{
A step of the tlseqsl_native implementation. Called by \code{morie_tlseqsl_cv_risk}, \code{morie_tlseqsl_ensemble_super_learner}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
