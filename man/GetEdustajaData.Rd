\name{GetEdustajaData}
\alias{GetEdustajaData}
\title{Hakee tietyn aanestyksen tulokset edustajaittain}
\usage{
  GetEdustajaData(aanestys)
}
\arguments{
  \item{aanestys}{aanestyksen ID}
}
\value{
  data.frame jossa valinta, puolue ja nimi
}
\description{
  Hakee tietyn aanestyksen tulokset edustajaittain
}
\examples{
# edustajat <- GetEdustajaData('a3_80-2011')
}
\author{
  Juuso Haapanen \email{louhos@googlegroups.com}
}
\references{
  See citation("sorvi")
}
\keyword{eduskunta}

