% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{edgrn_moderate_dispersion}
\alias{edgrn_moderate_dispersion}
\title{edgrn_moderate_dispersion}
\usage{
edgrn_moderate_dispersion(gene_dispersions, common = NULL, prior_df = 10, df_residual = 1)
}
\arguments{
\item{gene_dispersions}{See Usage.}
\item{common}{Defaults to \code{NULL}.}
\item{prior_df}{Defaults to \code{10}.}
\item{df_residual}{Defaults to \code{1}.}
}
\description{
Part of the edgrn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{dispersion}, \code{common}, \code{shrinkage}, \code{prior_df}, \code{df_residual}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
edgrn_moderate_dispersion(V)
}
