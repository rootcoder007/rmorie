% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dynamical_fit}
\alias{morie_dynamical_fit}
\title{EM over the rates and the latent variables. E step is the closest}
\usage{
morie_dynamical_fit(u, s, alpha0 = NULL, beta0 = 1, gamma0 = 0.5, t_switch0 = NULL, n_iter = 25, grid = 120)
}
\arguments{
\item{u}{A vector; its length is taken and its elements indexed.}
\item{s}{A vector; its length is taken and its elements indexed.}
\item{alpha0}{Defaults to \code{NULL}.}
\item{beta0}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{gamma0}{Defaults to \code{0.5}.}
\item{t_switch0}{Defaults to \code{NULL}.}
\item{n_iter}{Defaults to \code{25}.}
\item{grid}{Passed to \code{.scvelo_residual}. Defaults to \code{120}.}
}
\description{
point on the trajectory; M step is a one-at-a-time multiplicative grid search on alpha, beta, gamma, t_switch. The loop stops as soon as no candidate improves the residual, so the rss_history is the visible monotone sequence.
}
\value{
A list with \code{estimate}, \code{alpha}, \code{beta}, \code{gamma}, \code{t_switch}, \code{rss}, \code{rss_history}, \code{latent}, \code{velocity}, \code{steady_on}, \code{method}.
}
\references{
----------
Bergen, V., Lange, M., Peidli, S., Wolf, F. A. & Theis, F. J. (2019)
"Generalizing RNA velocity to transient cell states through dynamical
modeling", bioRxiv 820936, doi:10.1101/820936; published as Bergen
et al. (2020) Nature Biotechnology 38(12), 1408-1414,
doi:10.1038/s41587-020-0591-3. The splicing ODEs reproduced above,
the two assumptions the steady-state model needs (full dynamics
observed per gene, one shared splicing rate) and why transient or
heterogeneous populations violate them, the explicit solution of
the kinetics, the latent variables (a discrete state ki and
continuous time ti per cell), the EM scheme assigning ti by minimum
distance to the phase trajectory and ki by segment likelihood, and
velocity as the derivative of spliced abundance.

La Manno, G., Soldatov, R., Zeisel, A., Braun, E., Hochgerner, H.,
Petukhov, V., Lidschreiber, K., Kastriti, M. E., Lönnerberg, P.,
Furlan, A., Fan, J., Borm, L. E., Liu, Z., van Bruggen, D., Guo, J.,
He, X., Barker, R., Sundström, E., Castelo-Branco, G., Cramer, P.,
Adameyko, I., Linnarsson, S. & Kharchenko, P. V. (2018) "RNA
velocity of single cells", Nature 560(7719), 494-498,
doi:10.1038/s41586-018-0414-6, for the steady-state model this
generalises.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_dynamical_fit(V, V)
}
