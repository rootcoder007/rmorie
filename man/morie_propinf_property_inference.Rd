% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_propinf_property_inference}
\alias{morie_propinf_property_inference}
\title{Property inference on fully connected networks}
\usage{
morie_propinf_property_inference(shadow_models, shadow_labels, target_models = NULL, target_labels = NULL, 
    representation = "set", meta_hidden = c(16L), phi_hidden = c(8L), repr_dim = 4L, 
    rho_hidden = c(8L), context = "paired", epochs = 30L, lr = 0.05, seed = 0L) 
}
\arguments{
\item{shadow_models}{List of shadow networks.}
\item{shadow_labels}{Numeric 0/1 vector, one per shadow network.}
\item{target_models}{Optional list of target networks. Defaults to   the shadow models.}
\item{target_labels}{Optional numeric 0/1 vector, one per target   network.}
\item{representation}{One of \code{"baseline"}, \code{"sorting"},   \code{"set"}.}
\item{meta_hidden}{Hidden layer sizes for the vector meta-classifier.}
\item{phi_hidden}{Hidden layer sizes for the per-layer phi nets.}
\item{repr_dim}{Node representation dimension.}
\item{rho_hidden}{Hidden layer sizes for the rho net.}
\item{context}{One of \code{"paired"}, \code{"as_printed"},   \code{"none"}; only used for \code{representation="set"}.}
\item{epochs}{Number of SGD passes.}
\item{lr}{Positive learning rate.}
\item{seed}{Seed for the shared generator.}
}
\description{
Train a meta-classifier on shadow model features labelled by the training-set property, then apply it to the target models. The representation determines the meta-classifier: \code{"baseline"} and \code{"sorting"} feed a flat vector MLP; \code{"set"} feeds the DeepSets form of Algorithm 2.
}
\value{
A list with \code{estimate}, \code{accuracy},   \code{train_accuracy}, \code{prediction}, \code{score},   \code{representation}, \code{context}, \code{n_shadow},   \code{n_target}, \code{architecture}, \code{meta_classifier},   \code{method}, \code{note}.
}
\keyword{internal}
