% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_fda_env_interaction}
\alias{morie_fda_env_interaction}
\title{Written out for all I environments the blocks sum column by column to}
\usage{
morie_fda_env_interaction(X, env, reference = TRUE) 
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{env}{A vector; indexed elementwise.}
\item{reference}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
X exactly, so the joint design carrying both X and X_EF is rank deficient and beta and beta_EF are not separately identified by least squares; the book fits (14.14) in BGLR, where the prior on each block resolves that.  The default reference = TRUE drops the first environment block, the same reference coding the book applies to the environment design itself on p.607, where its code reads X_E = model.matrix(~0+Env, data = dat_F)[, -1].  Pass reference = FALSE for the redundant parameterization as printed.
}
\value{
A list with \code{X_EF}, \code{levels}, \code{kept_levels}, \code{reference}, \code{n_columns}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_fda_env_interaction(V, V)
}
