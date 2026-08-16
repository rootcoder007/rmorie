% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlonsl_online_super_learner}
\alias{morie_tlonsl_online_super_learner}
\title{morie_tlonsl_online_super_learner}
\usage{
morie_tlonsl_online_super_learner(y, library, loss = "squared", burn_in = 5, lags = 1, eta = 1)
}
\arguments{
\item{y}{See Usage.}
\item{library}{A vector; its length is taken and its elements indexed.}
\item{loss}{Passed to \code{morie_tlonsl_sequential_risk}. Defaults to \code{"squared"}.}
\item{burn_in}{Passed to \code{morie_tlonsl_sequential_risk}. Defaults to \code{5}.}
\item{lags}{Passed to \code{morie_tlonsl_sequential_risk}. Defaults to \code{1}.}
\item{eta}{Passed to \code{morie_tlonsl_update_weights}. Defaults to \code{1}.}
}
\description{
A step of the tlonsl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{weights}, \code{risk}, \code{member_risks}, \code{best_single}, \code{best_member}, \code{weight_path}, \code{n_scored}, \code{method}, \code{note}.
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
