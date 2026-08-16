% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_steady_state_velocity}
\alias{morie_steady_state_velocity}
\title{The baseline model: regress gamma/beta through the origin on the}
\usage{
morie_steady_state_velocity(u, s, quantile = 0.95)
}
\arguments{
\item{u}{See Usage.}
\item{s}{See Usage.}
\item{quantile}{Defaults to \code{0.95}.}
}
\description{
cells assumed to be at steady state, and call the residual the velocity. It needs the steady states to be present in the data, which is the assumption the dynamical model removes.
}
\value{
A list with \code{gamma_over_beta}, \code{velocity}, \code{n_fitted}, \code{assumptions}, \code{method}.
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
