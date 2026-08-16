% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_load_chain}
\alias{.morie_load_chain}
\title{Resolve a dataset loader through the live -> included -> synthetic chain.}
\usage{
.morie_load_chain(source = c("auto", "live", "bundled", "synthetic", "empty"), live_fn = NULL, bundled_name = NULL, synth_fn = NULL, columns = NULL)
}
\arguments{
\item{source}{One of \code{"auto"}, \code{"live"}, \code{"included"},   \code{"synthetic"}, \code{"empty"}.}
\item{live_fn}{A function that returns the live frame. May error or   return \code{NULL}; either is treated as a miss in \code{"auto"}.   Pass \code{NULL} to mean "no live path".}
\item{bundled_name}{Character; the included fixture stem (without   \code{.csv}). The file is looked up via   \code{system.file("extdata", paste0(bundled_name, ".csv"), package = "rmorie")}.   Pass \code{NULL} or \code{NA_character_} to mean "no included   fixture".}
\item{synth_fn}{A function that returns the synthetic frame. Pass   \code{NULL} to mean "no synthetic generator".}
\item{columns}{Character vector of the documented column schema for   the typed-empty fallback. Pass \code{NULL} to mean "no schema".}
}
\description{
Internal dispatch used by the unified \code{morie_datasets_*} loaders that accept a \code{source = c("auto", "live", "included", "synthetic", "empty")} argument. The chain is: \describe{   \item{auto}{Try \code{live_fn()} first (network). If it errors or     returns \code{NULL}, fall back to the included fixture, then to     \code{synth_fn(...)}, then to a typed-empty 0-row frame built     from \code{columns}. Whichever step succeeds first is returned.}   \item{live}{Run \code{live_fn()} only; error if it fails.}   \item{included}{Read the included \code{inst/extdata/<fixture>.csv}     only; error if missing.}   \item{synthetic}{Run \code{synth_fn()} only; error if no generator     is wired up for this dataset.}   \item{empty}{Return a 0-row data.frame with the documented     \code{columns}; error if no schema is wired up.} }
}
\value{
A \code{data.frame}.
}
\keyword{internal}
