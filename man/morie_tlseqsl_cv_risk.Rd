% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlseqsl_cv_risk}
\alias{morie_tlseqsl_cv_risk}
\title{morie_tlseqsl_cv_risk}
\usage{
morie_tlseqsl_cv_risk(X, y, algorithm, V = 10, loss = "squared", seed = 0)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{algorithm}{Accepted by the signature and not used anywhere in the body.}
\item{V}{Passed to \code{morie_tlseqsl_cv_folds}. Defaults to \code{10}.}
\item{loss}{Passed to \code{.tlseqsl_loss}. Defaults to \code{"squared"}.}
\item{seed}{Passed to \code{morie_tlseqsl_cv_folds}. Defaults to \code{0}.}
}
\description{
A step of the tlseqsl_native implementation. Called by \code{morie_tlseqsl_discrete_super_learner}. See the file header for the source the module follows.
}
\value{
A list with \code{risk}, \code{cv_predictions}, \code{V}, \code{loss}.
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
set.seed(1)
X <- matrix(rnorm(100), 50, 2)
y <- X \%*\% c(1, -1) + rnorm(50)
algo <- function(Xtr, ytr) {
  b <- qr.solve(cbind(1, Xtr), ytr)
  function(xrow) sum(c(1, xrow) * b)
}
morie_tlseqsl_cv_risk(X, y, algo, V = 5, seed = 1)$risk
}
