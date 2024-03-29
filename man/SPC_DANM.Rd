% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPC_DANM.R
\name{SPC_DANM}
\alias{SPC_DANM}
\title{SPC_DANM}
\usage{
SPC_DANM(specdat, withBC = TRUE)
}
\arguments{
\item{specdat}{A complex number vector of observed frequency domain data}

\item{withBC}{A logical parameter that enables/disables baseline correction after baseline correction}
}
\value{
A numeric vector of phase corrected absorption spectrum
}
\description{
A single linear model with Minimization of difference between absolute area and net area
}
\details{
This function is to process phase error correction through a single linear model with minimization of difference between absolute area and net area,
followed by polynomial baseline correction if necessary
}
\examples{
data("fdat")
spc_danm_phased1 <- SPC_DANM(fdat$frequency_domain)
}
\references{
Liland KH, Almøy T, Mevik B (2010), Optimal Choice of Baseline
Correction for Multivariate Calibration of Spectra, Applied Spectroscopy 64, pp. 1007-1016.
}
\author{
Aixiang Jiang
}
\concept{phase correction}
