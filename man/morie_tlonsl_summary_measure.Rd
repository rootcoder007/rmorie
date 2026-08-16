% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlonsl_summary_measure}
\alias{morie_tlonsl_summary_measure}
\title{A fixed-dimensional summary Z of the past. lags=0 gives the empty}
\usage{
morie_tlonsl_summary_measure(history, lags = 1)
}
\arguments{
\item{history}{See Usage.}
\item{lags}{Defaults to \code{1}.}
}
\description{
summary, which is exactly the i.i.d. case.
}
\value{
The value of \code{z}, as built in the body.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_tlonsl_summary_measure(V)
}
