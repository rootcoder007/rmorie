% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_adapter_core}
\alias{.morie_km2_adapter_core}
\title{.morie_km2_adapter_core}
\usage{
.morie_km2_adapter_core(H_o, H_in, W_down, W_up, f)
}
\arguments{
\item{H_o}{A matrix; passed to \code{nrow}.}
\item{H_in}{A matrix; passed to \code{nrow}.}
\item{W_down}{A matrix; passed to \code{as.matrix}.}
\item{W_up}{A matrix; passed to \code{as.matrix}.}
\item{f}{A function; the body checks with \code{is.function}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_ch4_parallel_adapter}, \code{morie_kamath_ch4_series_adapter}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
