% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tree_fit_cpp}
\alias{morie_tree_fit_cpp}
\title{Grow one regression / second-order tree (compiled)}
\usage{
morie_tree_fit_cpp(X, g, h, max_depth, min_node, mtry, lambda, alpha, gamma_pen) 
}
\description{
Grow one regression / second-order tree (compiled)
}
\arguments{
\item{X}{Argument \code{X}; see Usage.}
\item{g}{Argument \code{g}; see Usage.}
\item{h}{Argument \code{h}; see Usage.}
\item{max_depth}{Argument \code{max_depth}; see Usage.}
\item{min_node}{Argument \code{min_node}; see Usage.}
\item{mtry}{Argument \code{mtry}; see Usage.}
\item{lambda}{Argument \code{lambda}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
\item{gamma_pen}{Argument \code{gamma_pen}; see Usage.}
}
\value{
The value of \code{.Call}.
}
\keyword{internal}
