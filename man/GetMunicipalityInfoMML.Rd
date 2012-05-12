\name{GetMunicipalityInfoMML}
\alias{GetMunicipalityInfoMML}
\title{Get information of Finnish municipalities from Land Survey Finland 2012.
(C) Maanmittauslaitos MML 2012. For details of MML data, see
help(GetShapeMML).}
\usage{
  GetMunicipalityInfoMML(MML)
}
\arguments{
  \item{MML}{MML data, obtain with 'data(MML)'}
}
\value{
  A data frame with municipality data
}
\description{
  Get information of Finnish municipalities from Land
  Survey Finland 2012. (C) Maanmittauslaitos MML 2012. For
  details of MML data, see help(GetShapeMML).
}
\examples{
# data(MML); tab <- GetMunicipalityInfoMML(MML)
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

