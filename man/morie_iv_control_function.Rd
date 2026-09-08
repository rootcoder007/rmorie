% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_iv_control_function}
\alias{morie_iv_control_function}
\title{Control-function (residual augmentation) IV}
\usage{
morie_iv_control_function(data, outcome, endogenous, instruments, exogenous = NULL, robust = TRUE, 
    alpha = 0.05, dist = c("normal", "t")) 
}
\description{
Control-function (residual augmentation) IV
}
\arguments{
\item{data}{Argument \code{data}; see Usage.}
\item{outcome}{Argument \code{outcome}; see Usage.}
\item{endogenous}{Argument \code{endogenous}; see Usage.}
\item{instruments}{Argument \code{instruments}; see Usage.}
\item{exogenous}{Argument \code{exogenous}; see Usage.}
\item{robust}{Argument \code{robust}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{dist}{Argument \code{dist}; see Usage.}
}
\value{
A named list with elements \code{coefficients}, \code{std_errors}, \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper}, \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
}
\keyword{internal}
