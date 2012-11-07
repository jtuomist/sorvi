# Copyright (C) Juuso Haapanen 2012, <juuso(at)haapanen.biz> All rights reserved
# This program is open source software; you can redistribute it and/or
# modify it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# This software has been published as part of louhos project (louhos.github.com) 



# Versio 0.1


#' Hakee kaikki aanestykset eduskuntarajapinnasta
#' @param no params
#' @return list
#' @author Juuso Haapanen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples
#' # aanestykset <- GetAllAanestykset()
#' @keywords eduskunta
#' @references citation("sorvi") 
#' @export
GetAllAanestykset <- function() {
  if(!require(XML)) {
    install.packages('XML')
  }
  url <- "http://www.biomi.org/eduskunta/"
  kaikki.tree <- XML::xmlParse(url)
  tunnisteet <- XML::getNodeSet(kaikki.tree, path='//luettelo/aanestys/tunniste')
  out <- XML::xmlToDataFrame(tunnisteet)
  out <- as.character(out$text)
  return(out)
  
}

#' Hakee tietyn aanestyksen tulokset edustajaittain
#' 
#' @param aanestys 
#' @return data.frame jossa valinta, puolue ja nimi
#' @author Juuso Haapanen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples 
#' # edustajat <- GetEdustajaData('a3_80-2011')
#' @references See citation("sorvi") 
#' @keywords eduskunta
#' @export
GetEdustajaData <- function(aanestys)
{
  if(!require(XML)) {
    install.packages('XML')
  }
  baseurl <- "http://www.biomi.org/eduskunta/?haku=aanestys&id="
  if(is.na(aanestys)) {
     stop('Param aanestys not defined ')
  }
  else {
    search_url <- paste(baseurl,aanestys,sep="")
    ekdat.tree <- XML::xmlParse(search_url)
    ekdat.edustajat <- XML::getNodeSet(ekdat.tree, path="//edustajat/edustaja")
    if(length(ekdat.edustajat) == 0) {
      stop('Virheellinen Äänestys-id')
    }
    df <- XML::xmlToDataFrame(ekdat.edustajat)
    df$valinta <- as.factor(df$valinta)
    df$puolue <- as.factor(df$puolue)
    df$nimi <- as.character(df$nimi)
  }
  return(df)
}

#' Hakee tietyn kansanedustajan aanestykset
#' @param edustajan nimi muodossa Sukunimi Etunimi
#'
#' @return data.frame
#'
#' @author Juuso Haapanen \email{sorvi-commits@@lists.r-forge.r-project.org}
#'
#' @examples
#' # paavo <- GetEdustajanAanestykset('Lipponen Paavo')
#'
#' @references See citation("sorvi") 
#' @keywords eduskunta
#' @export
GetEdustajanAanestykset <- function(edustaja) {
  if(!require(XML)) {
    install.packages('XML')
  }
  
  edustaja <- URLencode(edustaja)
  url <- "http://www.biomi.org/eduskunta/?haku=edustaja&id"
  url.haku <- paste(url, edustaja, sep="=")
  edustaja.puu <- XML::xmlParse(url.haku)
  aanestykset <- XML::getNodeSet(edustaja.puu, path='//edustaja/aanestys/tiedot')
  df <- XML::xmlToDataFrame(aanestykset)
  
  return(df)
}

#' Hakee hakusanalla aanestyksiä eduskuntarajapinnasta
#' @param hakusana string
#' @return data.frame
#' @author Juuso Haapanen \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples
#' # luonto <- haeHakusanalla('luonto')
#' @export
#' @references See citation("sorvi") 
#' @keywords eduskunta 

haeHakusanalla <- function(hakusana) {

  require(XML)
  
  hakusana <- URLencode(hakusana)
  url <- "http://www.biomi.org/eduskunta/?haku=sanahaku&id"
  url.haku <- paste(url, hakusana, sep="=")
  aanestykset.puu <- XML::xmlParse(url.haku)
  aanestykset <- XML::getNodeSet(aanestykset.puu, path="//aanestykset/aanestys/tiedot")
  df <- XML::xmlToDataFrame(aanestykset)
  return(df)
}


