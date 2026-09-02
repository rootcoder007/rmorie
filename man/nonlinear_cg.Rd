% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{nonlinear_cg}
\alias{nonlinear_cg}
\title{nonlinear_cg}
\usage{
nonlinear_cg(f, grad, x0, beta = "fletcher-reeves", restart = NULL, max_iter = NULL, 
    tol = 1e-10, est = NULL, line_search = "fletcher-reeves", hess_vec = NULL, 
    keep_path = FALSE) 
}
\arguments{
\item{f}{Passed to \code{line_search_fr}.}
\item{grad}{Passed to \code{line_search_fr}.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{beta}{Passed to \code{.cgnonl_beta}. Defaults to \code{"fletcher-reeves"}.}
\item{restart}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{max_iter}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
\item{est}{Passed to \code{line_search_fr}.}
\item{line_search}{Compared against \code{"exact-quadratic"}. Defaults to \code{"fletcher-reeves"}.}
\item{hess_vec}{Optional; may be \code{NULL}. Passed to \code{.cgnonl_exact_quadratic_step}.}
\item{keep_path}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the cgnonl_native implementation. Called by \code{morie_cgnonl}. See the file header for the source the module follows.
}
\value{
A list with \code{x}, \code{fun}, \code{grad}, \code{gnorm}, \code{n_iter}, \code{n_restart}, \code{n_feval}, \code{converged}, \code{betas}, \code{path}, \code{beta_rule}, \code{line_search}, \code{restart_every}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
f <- function(x) sum((x - c(1, 2))^2) + 0.5 * x[1]^2 * x[2]^2
g <- function(x) c(2 * (x[1] - 1) + x[1] * x[2]^2,
                   2 * (x[2] - 2) + x[1]^2 * x[2])
r <- nonlinear_cg(f, g, x0 = c(0, 0))
str(r, max.level = 1)
}
