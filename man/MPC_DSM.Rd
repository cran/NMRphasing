% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MPC_DSM.R
\name{MPC_DSM}
\alias{MPC_DSM}
\title{MPC_DSM}
\usage{
MPC_DSM(specdat, withBC = TRUE)
}
\arguments{
\item{specdat}{A complex number vector of observed frequency domain data.}

\item{withBC}{A logical parameter that enables/disables baseline correction after baseline correction}
}
\value{
A numeric vector of phase corrected absorption spectrum
}
\description{
Multiple single linear models that minimize the total dispersion.
}
\details{
This function is used to process phase error correction through multiple single linear models that minimize the total dispersion, followed by polynomial baseline correction when necessary.
}
\examples{
data("fdat")
mpc_dsm_phased1 <- MPC_DSM(fdat$frequency_domain)
}
\references{
Chen, L., Weng, Z., Goh, L., & Garland, M. (2002). An efficient algorithm for automatic phase correction of NMR spectra based on
entropy minimization. Journal of Magnetic Resonance, 158, 1-2.

Ernst, R. R. (1969). Numerical Hilbert transform and automatic phase correction in magnetic resonance spectroscopy.
Journal of Magnetic Resonance, 1, 7-26

Liland KH, Almøy T, Mevik B (2010), Optimal Choice of Baseline
Correction for Multivariate Calibration of Spectra, Applied Spectroscopy 64, pp. 1007-1016.
}
\author{
Aixiang Jiang
}
\concept{phase correction}
