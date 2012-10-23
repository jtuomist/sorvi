\name{GetEdustajanAanestykset}
\alias{GetEdustajanAanestykset}
\title{Hakee tietyn kansanedustajan äänestykset}
\usage{
  GetEdustajanAanestykset(edustaja)
}
\arguments{
  \item{edustajan}{nimi muodossa Sukunimi Etunimi}
}
\value{
  data.frame
}
\description{
  Hakee tietyn kansanedustajan äänestykset
}
\examples{
# paavo <- GetEdustajanAanestykset('Lipponen Paavo')
}
\author{
  Juuso Haapanen
  \email{sorvi-commits@lists.r-forge.r-project.org}
}
\references{
  See citation("sorvi")
}
\keyword{eduskunta}

