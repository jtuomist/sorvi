\name{GetEdustajaData}
\alias{GetEdustajaData}
\title{Hakee tietyn äänestyksen tulokset edustajaittain}
\usage{
  GetEdustajaData(aanestys)
}
\arguments{
  \item{aanestys}{äänestyksen tunniste.}
}
\value{
  data.frame jossa valinta, puolue ja nimi
}
\description{
  Hakee tietyn äänestyksen tulokset edustajaittain
}
\examples{
# edustajat <- GetEdustajaData('a3_80-2011')
}
\author{
  Juuso Haapanen
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{eduskunta}
