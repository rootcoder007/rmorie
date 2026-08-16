% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_solve_kinetics}
\alias{morie_solve_kinetics}
\title{The explicit solution of the splicing ODEs at time tau after a}
\usage{
morie_solve_kinetics(tau, alpha, beta, gamma, u0 = 0, s0 = 0)
}
\arguments{
\item{tau}{See Usage.}
\item{alpha}{Numeric; combined arithmetically in the body.}
\item{beta}{Numeric; combined arithmetically in the body.}
\item{gamma}{Numeric; combined arithmetically in the body.}
\item{u0}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{s0}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
phase switch. The gamma = beta branch is the limit of the (e^-gt - e^-bt)/(g - b) term, which is t*e^-bt at g = b, not a genuine singularity.
}
\value{
A list with \code{u}, \code{s}, \code{tau}.
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
morie_solve_kinetics(tau = 0.5, alpha = 0.5, beta = 0.5, gamma = 0.5)
}
