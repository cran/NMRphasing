% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MPC_DANM.R
\name{MPC_DANM}
\alias{MPC_DANM}
\title{MPC_DANM}
\usage{
MPC_DANM(specdat, withBC = TRUE)
}
\arguments{
\item{specdat}{A complex number vector of observed frequency domain data}

\item{withBC}{A logical parameter that enables/disables baseline correction after baseline correction.}
}
\value{
A numeric vector of phase corrected absorption spectrum
}
\description{
Multiple linear models that minimize the difference between absolute area and net area.
}
\details{
This function processes phase error correction through multiple linear models that minimize the difference between absolute area and net area, followed by polynomial baseline correction when necessary.
}
\examples{
data("fdat")
mpc_danm_phased1 <- MPC_DANM(fdat$frequency_domain)
}
\references{
Liland KH, Almøy T, Mevik B (2010), Optimal Choice of Baseline
Correction for Multivariate Calibration of Spectra, Applied Spectroscopy 64, pp. 1007-1016.
}
\author{
Aixiang Jiang
}
\concept{phase correction}
