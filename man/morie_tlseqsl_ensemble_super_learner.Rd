% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlseqsl_ensemble_super_learner}
\alias{morie_tlseqsl_ensemble_super_learner}
\title{morie_tlseqsl_ensemble_super_learner}
\usage{
morie_tlseqsl_ensemble_super_learner(X, y, library, V = 10, loss = "squared", seed = 0, grid = 21)
}
\arguments{
\item{X}{Passed to \code{morie_tlseqsl_discrete_super_learner}.}
\item{y}{Passed to \code{morie_tlseqsl_discrete_super_learner}.}
\item{library}{Passed to \code{morie_tlseqsl_discrete_super_learner}.}
\item{V}{Passed to \code{morie_tlseqsl_discrete_super_learner}. Defaults to \code{10}.}
\item{loss}{Passed to \code{morie_tlseqsl_discrete_super_learner}. Defaults to \code{"squared"}.}
\item{seed}{Passed to \code{morie_tlseqsl_discrete_super_learner}. Defaults to \code{0}.}
\item{grid}{Numeric; combined arithmetically in the body. Defaults to \code{21}.}
}
\description{
A step of the tlseqsl_native implementation. Called by \code{morie_tlseqsl_sequential_super_learner}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{weights}, \code{cv_risk}, \code{discrete_risks}, \code{discrete_choice}, \code{best_single}, \code{method}, \code{note}.
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
\examples{
morie_tlseqsl_ensemble_super_learner(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), library = c(1, 2, 3, 4, 5, 6, 7, 8))
}
