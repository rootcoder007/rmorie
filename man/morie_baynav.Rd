% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_baynav}
\alias{morie_baynav}
\title{Variational inference: normalizing flows and automatic}
\usage{
morie_baynav(u, w, b, value, support, eps, z0, log_q0, layers, log_joint, 
    log_q, samples) 
}
\arguments{
\item{u,w}{Numeric vectors of equal length.}
\item{b}{Numeric scalar.}
\item{value}{Numeric scalar for \code{transform_to_real}.}
\item{support}{One of \code{"positive"}, \code{"unit"},   \code{"real"}.}
\item{eps}{Positive tolerance; default \code{1e-10}. Not a tuning   knob for the R arm.}
\item{z0}{Numeric vector; the base sample.}
\item{log_q0}{Numeric scalar; the base log-density at \code{z0}.}
\item{layers}{List of \code{list(u, w, b)} triples, one per   planar-flow layer.}
\item{log_joint,log_q}{Functions of a sample, each returning a   numeric scalar.}
\item{samples}{List of samples to evaluate the bound on.}
}
\description{
differentiation VI Two routes to the same bound. \code{planar_flow}, \code{enforce_invertibility} and \code{flow_log_density} are the normalizing-flow arm of Rezende and Mohamed (2015): a simple base density is pushed through invertible maps, the change of variables subtracts the log-determinants, and a planar flow\'s rank-one Jacobian is computable in O(d) by the matrix determinant lemma. \code{transform_to_real} is the ADVI arm of Kucukelbir et al. (2017): constrained latents are mapped to \code{R^K} by a bijection, a Gaussian is fitted there, and the density is corrected by the log-Jacobian. \code{elbo} is the common yardstick (Blei et al. 2017) -- both approaches optimise it, so a flow of depth 0 must equal plain mean-field.
}
\value{
A named list whose element names match the Python payload   keys. \code{enforce_invertibility} returns   \code{u, adjusted, u_dot_w, u_dot_w_after, note};   \code{planar_flow} returns   \code{z, log_det, det, invertibility_adjusted, note};   \code{flow_log_density} returns   \code{estimate, log_q, z, log_dets, depth, method, note};   \code{transform_to_real} returns   \code{real, log_jacobian, inverse};   \code{elbo} returns \code{elbo, se, n_samples, note}.
}
\keyword{internal}
