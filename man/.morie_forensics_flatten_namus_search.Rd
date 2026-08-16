% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_forensics_flatten_namus_search}
\alias{.morie_forensics_flatten_namus_search}
\title{Internal: flatten one flat Search-projection record (the shape the}
\usage{
.morie_forensics_flatten_namus_search(rec)
}
\arguments{
\item{rec}{See Usage.}
}
\description{
NamUs Search endpoint actually returns; verified live 2026-07). Height/weight are not projectable via Search -- NA by contract.
}
\value{
A list with \code{case_number}, \code{state}, \code{county}, \code{dlc_date}, \code{sex}, \code{race}, \code{age_min}, \code{age_max}, \code{height_cm_min}, \code{height_cm_max}, \code{weight_kg_min}, \code{weight_kg_max}, \code{first_name}, \code{last_name}, \code{city}, \code{circumstances}.
}
\keyword{internal}
