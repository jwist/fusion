% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readSpectrum.R
\name{readSpectrum}
\alias{readSpectrum}
\title{read a spectrum (processed) from a Bruker expno folder}
\usage{
readSpectrum(path, procs = TRUE, options = list())
}
\arguments{
\item{path}{\itemize{
\item the path to the expNo folder
}}

\item{procs}{\itemize{
\item the name of the folder with experiments
}}

\item{options}{\itemize{
\item options
}}
}
\value{
a vector with spectra (real part and x axis)
}
\description{
read a spectrum (processed) from a Bruker expno folder
}
