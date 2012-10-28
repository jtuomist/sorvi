\name{GetShapeMML}
\alias{GetShapeMML}
\title{Shows how the MML Shape files have been converted into the Rdata files included in sorvi. For detailed example, see https://github.com/louhos/sorvi/wiki/Maanmittauslaitos The various Finland shape data files obtained from http://www.maanmittauslaitos.fi/aineistot-palvelut/digitaaliset-tuotteet/ilmaiset-aineistot/hankinta have been preprocessed using this script.}
\usage{
  GetShapeMML(input.data.dir = "./", verbose = TRUE)
}
\arguments{
  \item{input.data.dir}{Directory path where the original
  data can be accessed.}

  \item{verbose}{Print intermediate processing information

  Returns:}
}
\value{
  Returns a list of preprocessed shape files containing the
  public MML data sets.
}
\description{
  Arguments:
}
\examples{
# MML <- GetShapeMML(data.dir = "./")
# save(MML, file = "MML.rda")
# require(tools)
# res <- resaveRdaFiles(MML.rda)
}
\author{
  Leo Lahti \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\keyword{internal}

