% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlonsl_loss}
\alias{.tlonsl_loss}
\title{.tlonsl_loss}
\usage{
.tlonsl_loss(kind, y, p)
}
\arguments{
\item{kind}{Compared against \code{"squared"}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{p}{Numeric; passed to \code{max}.}
}
\description{
A step of the tlonsl_native implementation. Called by \code{morie_tlonsl_online_super_learner}, \code{morie_tlonsl_sequential_risk}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 18 (van der
Laan & Benkeser).

Benkeser, D., Ju, C., Lendle, S. & van der Laan, M. J. (2018)
"Online cross-validation-based ensemble learning", Statistics in
Medicine 37(2), 249-260, doi:10.1002/sim.7320.

van der Laan, M. J., Polley, E. C. & Hubbard, A. E. (2007) "Super
Learner", Statistical Applications in Genetics and Molecular
Biology 6(1), Article 25, doi:10.2202/1544-6115.1309.
}
\keyword{internal}
\examples{
.tlonsl_loss(kind = 5L, y = c(1, 2, 3, 4, 5, 6, 7, 8), p = 0.5)
}
