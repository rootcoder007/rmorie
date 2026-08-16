% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_assign_latent_time}
\alias{morie_assign_latent_time}
\title{E step: each observation is assigned the time on the trajectory}
\usage{
morie_assign_latent_time(u, s, alpha, beta, gamma, t_switch, grid = 200, t_max = NULL)
}
\arguments{
\item{u}{See Usage.}
\item{s}{See Usage.}
\item{alpha}{See Usage.}
\item{beta}{See Usage.}
\item{gamma}{See Usage.}
\item{t_switch}{See Usage.}
\item{grid}{Defaults to \code{200}.}
\item{t_max}{Defaults to \code{NULL}.}
}
\description{
closest to it, in Euclidean (u, s) distance, and the transcriptional state of the segment it lands on.
}
\value{
The value of \code{out}, as built in the body.
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
