% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funcal}
\alias{morie_funcal}
\title{morie_funcal}
\usage{
morie_funcal(hits, groups, annotations, taxa = NULL, target_taxa = NULL, target_types = NULL, sources = NULL, evalue = 0.001, score = 60, query_cov = 0.2, target_cov = 0.2, min_support = 1, searcher = "diamond")
}
\arguments{
\item{hits}{See Usage.}
\item{groups}{See Usage.}
\item{annotations}{See Usage.}
\item{taxa}{Defaults to \code{NULL}.}
\item{target_taxa}{Defaults to \code{NULL}.}
\item{target_types}{Defaults to \code{NULL}.}
\item{sources}{Defaults to \code{NULL}.}
\item{evalue}{Defaults to \code{0.001}.}
\item{score}{Defaults to \code{60}.}
\item{query_cov}{Defaults to \code{0.2}.}
\item{target_cov}{Defaults to \code{0.2}.}
\item{min_support}{Defaults to \code{1}.}
\item{searcher}{Defaults to \code{"diamond"}.}
}
\description{
Part of the funcal_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{annotations}, \code{seeds}, \code{orthologs}, \code{n_queries}, \code{n_with_seed}, \code{n_annotated}, \code{searcher}, \code{target_taxa}, \code{target_types}, \code{min_support}, \code{method}, \code{note}.
}
\keyword{internal}
