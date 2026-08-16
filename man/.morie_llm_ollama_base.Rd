% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_llm_ollama_base}
\alias{.morie_llm_ollama_base}
\title{Internal helper: Morie Llm Ollama Base}
\usage{
.morie_llm_ollama_base()
}
\description{
The Ollama endpoint. Point \code{OLLAMA_HOST} (or \code{OLLAMA_BASE_URL}) at ANY reachable Ollama-compatible server: a local daemon (\code{http://localhost:11434}), a box on your network, or a remote / Cloudflare-tunnelled gateway (e.g. a shared team server). No model is bundled or assumed -- you bring your own.
}
\value{
The value of \code{sub}.
}
\keyword{internal}
