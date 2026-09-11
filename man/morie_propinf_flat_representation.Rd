% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_propinf_flat_representation}
\alias{morie_propinf_flat_representation}
\title{Baseline feature vector: every weight and bias in one vector}
\usage{
morie_propinf_flat_representation(net) 
}
\arguments{
\item{net}{Network as returned by \code{morie_propinf_train_fcnn}.}
}
\description{
Flattening all the weights and biases into one vector. Kept because it is the thing the other two have to beat, not because it works.
}
\value{
Numeric vector.
}
\keyword{internal}
\examples{
# a small fully connected net: three hidden units over two inputs,
# then a single logistic output
net <- list(
  list(W = matrix(c(2, 3,
                    1, 0,
                    4, 5), nrow = 3, byrow = TRUE), b = c(10, 20, 30)),
  list(W = matrix(c(0.5, -0.5, 0.25), nrow = 1), b = 7)
)
# every row of W followed by that row's bias, layer by layer
morie_propinf_flat_representation(net)
# not invariant to relabelling, which is the weakness Ganju et al. report
perm <- morie_propinf_permute_hidden_layer(net, 0L, c(2L, 0L, 1L))
identical(morie_propinf_flat_representation(net),
          morie_propinf_flat_representation(perm))
}
