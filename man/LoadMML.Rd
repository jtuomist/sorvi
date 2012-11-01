\name{LoadMML}
\alias{LoadMML}
\title{LoadMML}
\usage{
  LoadMML(data.id, resolution, verbose = TRUE)
}
\arguments{
  \item{data.id}{data ID. See details.}

  \item{resolution}{Map resolution. Options:
  "4_5_milj_shape_etrs-tm35fin"; "1_milj_Shape_etrs_shape"}

  \item{verbose}{verbose

  Return:}
}
\value{
  url connection
}
\description{
  Arguments:
}
\details{
  See help(MML). To browse for RData options, see
  LouhosStoragePath()/mml/ eg. resolution =
  "4_5_milj_shape_etrs-tm35fin"; data.id = "maaku4_p".
}
\examples{
# sp <- LoadMML(data.id = "kunta4_p", resolution = "4_5_milj_shape_etrs-tm35fin")
}
\author{
  Leo Lahti \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\keyword{utilities}

