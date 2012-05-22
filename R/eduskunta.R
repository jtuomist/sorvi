#Eduskuntadatan hakemiseen kirjoitettu R-sovellus, jolla saa helposti R:ään data frameksi XML-muotoista dataa. 
#
#

GetAllAanestykset <- function() {
  #hakee kaikki äänestykset
  require(XML)
  url <- "http://www.biomi.org/eduskunta/"
  kaikki.tree <- xmlParse(url)
  tunnisteet <- getNodeSet(kaikki.tree, path='//luettelo/aanestys/tunniste')
  out <- xmlToDataFrame(tunnisteet)
  out <- as.character(out$text)
  return(out)
  
}


#' @param aanestys äänestyksen tunniste.
#' @return data.frame jossa valinta, puolue ja nimi
#' @author Juuso Haapanen
#' 
GetEdustajaData <- function(aanestys)
{
  require(XML)
  baseurl <- "http://www.biomi.org/eduskunta/?haku=aanestys&id="
  if(is.na(aanestys)) {
     stop('Error ')
  }
  else {
    search_url <- paste(baseurl,aanestys,sep="")
    ekdat.tree <- xmlParse(search_url)
    ekdat.edustajat <- getNodeSet(ekdat.tree, path="//edustajat/edustaja")
    if(length(ekdat.edustajat) == 0) {
      stop('Virheellinen Äänestys-id')
    }
    df <- xmlToDataFrame(ekdat.edustajat)
    df$valinta <- as.factor(df$valinta)
    df$puolue <- as.factor(df$puolue)
    df$nimi <- as.character(df$nimi)
  }
  return(df)
}


GetEdustajanAanestykset <- function(edustaja) {
  if(!require(XML)) {
    install.packages('XML')
  }
  #options(encoding='UTF-8')
  #edustaja <- URLencode(edustaja)
  url <- "http://www.biomi.org/eduskunta/?haku=edustaja&id"
  url.haku <- paste(url, edustaja, sep="=")
  edustaja.puu <- xmlParse(url.haku, encoding="latin1")
  aanestykset <- getNodeSet(edustaja.puu, path='//edustaja/aanestys/tiedot')
  df <- xmlToDataFrame(aanestykset)
  
  return(df)
}


#' @param hakusana string
#' @return data.frame
haeHakuSanalla <- function(hakusana) {

  require(XML)
  
  hakusana <- URLencode(hakusana)
  url <- "http://www.biomi.org/eduskunta/?haku=sanahaku&id"
  url.haku <- paste(url, hakusana, sep="=")
  aanestykset.puu <- xmlParse(url.haku)
  aanestykset <- getNodeSet(aanestykset.puu, path="//aanestykset/aanestys/tiedot")
  df <- xmlToDataFrame(aanestykset)
  return(df)
}


