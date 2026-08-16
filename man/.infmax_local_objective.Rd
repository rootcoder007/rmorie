% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.infmax_local_objective}
\alias{.infmax_local_objective}
\title{.infmax_local_objective}
\usage{
.infmax_local_objective(global_features, feature_maps, critic, estimator = "jsd")
}
\arguments{
\item{global_features}{Iterated over elementwise, with \code{lapply}.}
\item{feature_maps}{Iterated over elementwise, with \code{lapply}.}
\item{critic}{Accepted by the signature and not used anywhere in the body.}
\item{estimator}{Carried through into a list the body builds. Defaults to \code{"jsd"}.}
}
\description{
A step of the infmax_native implementation. Called by \code{morie_infmax}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{objective}, \code{estimator}, \code{n_locations}, \code{n_positive}, \code{n_negative}, \code{method}, \code{note}.
}
\references{
----------
Hjelm, R. D., Fedorov, A., Lavoie-Marchildon, S., Grewal, K.,
Bachman, P., Trischler, A. & Bengio, Y. (2019) "Learning deep
representations by mutual information estimation and maximization",
International Conference on Learning Representations (ICLR 2019),
arXiv:1808.06670. Sec. 2-3: that maximising MI between the input and
output of an encoder can be done with an MI estimator; that a
Jensen-Shannon-divergence-based alternative to the Donsker-Varadhan
KL estimator is more stable and provides better results; that
structure-aware objectives -- maximising MI between the global feature
and LOCAL patches of the feature map -- improve the suitability of the
representation for classification; and the comparison with CPC, which
processes local features sequentially to build summary features and
predicts specific local features autoregressively with separate
estimators, whereas DIM uses a single global summary feature that
predicts all local features simultaneously in one step with one
estimator.

van den Oord, A., Li, Y. & Vinyals, O. (2018) "Representation
Learning with Contrastive Predictive Coding", arXiv:1807.03748. CPC,
the ordered-autoregression alternative.

Belghazi, M. I., Baratin, A., Rajeswar, S., Ozair, S., Bengio, Y.,
Courville, A. & Hjelm, R. D. (2018) "Mutual Information Neural
Estimation", ICML 2018, PMLR 80, 531-540, arXiv:1801.04062. The
Donsker-Varadhan estimator being replaced.
}
\keyword{internal}
