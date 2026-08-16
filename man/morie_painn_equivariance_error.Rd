% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_painn_equivariance_error}
\alias{morie_painn_equivariance_error}
\title{PaiNN: equivariant message passing for tensorial properties}
\usage{
morie_painn_equivariance_error(model, s, v, R, Q, tol = 1e-09)
}
\description{
PaiNN: equivariant message passing for tensorial properties. "Equivariant message passing for the prediction of tensorial properties and molecular spectra", ICML 2021, PMLR 139, 9377-9388, arXiv:2102.03150. The data-efficiency gap of invariant message passing; equivariance of vector features; the polarizable atom interaction neural network improving on common molecule benchmarks while REDUCING model size and inference time; tensorial properties read off directly with 4-5 orders-of-magnitude speed-ups over the "SchNet", NeurIPS 2017, arXiv:1706.08566, the invariant predecessor. Graph Neural Networks", ICML 2021, arXiv:2102.09844. Base R only, faithful translation of painn_python_reference.py.
}
\value{
A list with components \code{scalar_error}, \code{vector_error}, \code{scalars_invariant}, \code{vectors_equivariant}, \code{note}.
}
\references{
Sources: Schutt, K. T., Unke, O. T. & Gastegger, M. (2021)
electronic-structure reference. Schutt, K. T. et al. (2017)
Satorras, V. G., Hoogeboom, E. & Welling, M. (2021) "E(n) Equivariant
}
\keyword{internal}
