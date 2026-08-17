% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_latent_time}
\alias{morie_latent_time}
\title{A gene-shared clock: the per-cell median of the gene times. Each}
\usage{
morie_latent_time(fits)
}
\arguments{
\item{fits}{A vector; its length is taken and its elements indexed.}
}
\description{
gene has its own fit and its own rate, so without this aggregation the per-cell times are not comparable across genes.
}
\value{
A list with \code{latent_time}, \code{n_genes}, \code{n_cells}, \code{note}.
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
mk <- function(v) lapply(v, function(x) list(t = x))
fits <- list(list(latent = mk(c(0.1, 0.5, 0.9, 0.3))),
             list(latent = mk(c(0.2, 0.4, 0.8, 0.35))))
lt <- morie_latent_time(fits)
is.list(lt) || is.numeric(lt)
}
