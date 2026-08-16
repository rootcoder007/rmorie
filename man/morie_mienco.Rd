% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mienco}
\alias{morie_mienco}
\title{morie_mienco}
\usage{
morie_mienco(summary, patches, other_patches, critic, estimator = "jsd")
}
\arguments{
\item{summary}{See Usage.}
\item{patches}{A vector; its length is taken.}
\item{other_patches}{A vector; its length is taken.}
\item{critic}{Accepted by the signature and not used anywhere in the body.}
\item{estimator}{Compared against \code{"jsd"}. Defaults to \code{"jsd"}.}
}
\description{
A step of the mienco_native implementation. Called by \code{.mienco_global_objective}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{mi_lower_bound}, \code{estimator}, \code{n_patches}, \code{n_negative_patches}, \code{method}, \code{note}.
}
\references{
----------
Hjelm, R. D., Fedorov, A., Lavoie-Marchildon, S., Grewal, K.,
Bachman, P., Trischler, A. & Bengio, Y. (2019) "Learning deep
representations by mutual information estimation and maximization",
International Conference on Learning Representations (ICLR 2019),
arXiv:1808.06670. The abstract and Sec. 1-3: maximising mutual
information between the input and the output of a deep encoder;
structure matters, and maximising the AVERAGE MI between the global
representation and LOCAL patches greatly improves representation
quality for downstream tasks compared with the global objective; the
Donsker-Varadhan and Jensen-Shannon estimators built from a
discriminator over paired versus unpaired samples; and matching the
representation to a prior to control its characteristics.

Belghazi, M. I., Baratin, A., Rajeswar, S., Ozair, S., Bengio, Y.,
Courville, A. & Hjelm, R. D. (2018) "Mutual Information Neural
Estimation", ICML 2018, PMLR 80, 531-540, arXiv:1801.04062. The
Donsker-Varadhan estimator.

Zhu, Y., Xu, Y., Yu, F., Liu, Q., Wu, S. & Wang, L. (2020) "Deep
Graph Contrastive Representation Learning", arXiv:2006.04131. The
graph-domain descendant; implemented in :mod:`grace`.
}
\keyword{internal}
