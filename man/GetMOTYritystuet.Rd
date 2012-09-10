\name{GetMOTYritystuet}
\alias{GetMOTYritystuet}
\title{Load Finnish company funding information from MOT website}
\usage{
  GetMOTYritystuet(url = "http://yle.fi/fst/mot-2012-yritystuet/data/mot_yritystuet_data.csv")
}
\arguments{
  \item{url}{URL}
}
\value{
  data frame
}
\description{
  Source: (C) Yle MOT-toimitus; published under CC-BY-SA
  3.0 For data documentation, see
  http://ohjelmat.yle.fi/mot/10_9
}
\examples{
# tuet <- GetMOTYritystuet()
}
\author{
  Leo Lahti
  \email{sorvi-commits@lists.r-forge.r-project.org}
}

