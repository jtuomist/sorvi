
# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


#' Get information of Finnish provinces.
#' @aliases get.province.info
#' @param url URL of the Wikipedia source 
#' @return A data frame. With the following entries: Maakunta: province; Pinta-ala: area; Vakiluku: population; Vaestotiheys: population density
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # tab <- GetProvinceInfo()
#' @keywords utilities

GetProvinceInfo <- function (url = "http://fi.wikipedia.org/wiki/V%C3%A4est%C3%B6tiheys") {

  .InstallMarginal("XML")

  # Read tables from the website
  tables <- XML::readHTMLTable(url)

  # Population density in regions (maakunnat)
  tab <- tables[[1]]		

  # tab$Maakunta <- iconv(tab$Maakunta, "latin1", "UTF-8")

  names(tab) <- c("Maakunta", "Pinta.ala", "Vakiluku", "Vaestotiheys")
  tabPinta.ala <- as.numeric(as.character(tab$Pinta.ala))
  tab$Vakiluku <- as.numeric(as.character(tab$Vakiluku))
  tab$Vaestotiheys <- as.numeric(gsub(",", ".", tab$Vaestotiheys))

  tab

}


#' Convert municipality names into standard versions
#'
#' @param municipality.names municipality names to convert
#' @return standardized municipality names
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # tmp <- ConvertMunicipalityNames(municipality.names)
#' @keywords utilities
ConvertMunicipalityNames <- function (municipality.names) {
			 
  municipality.names <- gsub("Hämeenkyrö-Tavastkyro", "Hämeenkyrö", municipality.names)
  municipality.names <- gsub("Mänttä", "Mänttä-Vilppula", municipality.names)
  municipality.names <- gsub("Mänttä-Vilppula-Vilppula", "Mänttä-Vilppula", municipality.names)
  municipality.names <- gsub("Pedersören kunta", "Pedersöre", municipality.names)
  municipality.names <- gsub("Maarianhamina - Mariehamn", "Maarianhamina", municipality.names)
  municipality.names <- gsub("Länsi-Turunmaa", "Parainen", municipality.names)
  municipality.names <- gsub("Koski Tl", "Koski.Tl", municipality.names)

  municipality.names <- gsub("Loimaan kunta", "Loimaan.kunta", municipality.names)
  municipality.names <- gsub("Pieksämäen mlk", "Pieksämäen.mlk", municipality.names)
  municipality.names <- gsub("Jyväskylän mlk", "Jyväskylän.mlk", municipality.names)
  municipality.names <- gsub("Rovaniemen mlk", "Rovaniemen.mlk", municipality.names)
  # municipality.names <- gsub("", "", municipality.names)

  municipality.names

}

#' Get information of Finnish municipalities from Statistics Finland 2012 
#  (C) Tilastokeskus 2012 http://www.stat.fi/tup/atilastotietokannat/index.html
#' and Maanmittauslaitos (C) MML 2011. For details of MML data, see 
#' help(GetShapeMML).
#' 
#' @aliases get.municipality.info
#' @param url URL for Tilastokeskus municipality information 
#' @param MML MML data, obtain with 'data(MML)'
#' @return A data frame with municipality data
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # data(MML); tmp <- GetMunicipalityInfo(MML = MML)
#' @keywords utilities

GetMunicipalityInfo <- function (url = "http://pxweb2.stat.fi/Database/Kuntien%20perustiedot/Kuntien%20perustiedot/Kuntaportaali.px", MML) {

  mml <- sorvi::GetMunicipalityInfoMML(MML)    # (C) MML 2012
  statfi <- sorvi::GetMunicipalityInfoStatFi() # (C) Tilastokeskus 2012

  # Combine municipality information from Tilastokeskus and Maanmittauslaitos
  municipalities <- rownames(statfi)
  municipality.table <- cbind(statfi[municipalities, ], mml[municipalities, ])

  # FIXME: merge GetPopulationRegister function in here

  municipality.table

}


#' Get information of Finnish municipalities from Statistics Finland 2012 
#  (C) Tilastokeskus 2012 http://www.stat.fi/tup/atilastotietokannat/index.html
#' 
#' @param url URL for Tilastokeskus municipality information 
#' @return A data frame with municipality data
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # tmp <- GetMunicipalityInfo()
#' @keywords utilities

GetMunicipalityInfoStatFi <- function (url = "http://pxweb2.stat.fi/Database/Kuntien%20perustiedot/Kuntien%20perustiedot/Kuntaportaali.px") {

  .InstallMarginal("reshape2")

  # FIXME: merge GetPopulationRegister function in here

  # Get municipality information from Tilastokeskus
  municipality.info <- sorvi::GetPXTilastokeskus(url)

  # Clean up municipality names
  # FIXME: scandinavic characters cause error in Windows systems, find solution

  municipality.info$Alue <- sapply(strsplit(as.character(municipality.info$Alueluokitus.2012), " - "), function (x) {x[[1]]})

  municipality.info$value <- municipality.info$dat
  
  # Convert to wide format
  municipality.info <- reshape2::cast(municipality.info[, c("Alue", "Tunnusluku", "value")], Alue ~ Tunnusluku) 

  kuntanimi.statfin <- as.character(municipality.info$Alue)

  municipality.info[, "Alue"] <- sorvi::ConvertMunicipalityNames(municipality.info[, "Alue"])
  
  municipality.info$Alue <- factor(municipality.info$Alue)

  municipality.info[["Kunta.Tilastokeskus"]] <- kuntanimi.statfin
  municipality.info$Kunta <- factor(municipality.info$Alue)
  rownames(municipality.info) <- as.character(municipality.info[["Alue"]])

  # FIXME: Kunta is factor but Maakunta is character and 
  # UTF-8 does not seem to be working with Maakunta field
  
  municipality.info

}


#' Get information of Finnish municipalities from Land Survey Finland 2012.
#' (C) Maanmittauslaitos MML 2012. For details of MML data, see 
#' help(GetShapeMML).
#' 
#' @param MML MML data, obtain with 'data(MML)'
#' @return A data frame with municipality data
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # data(MML); tab <- GetMunicipalityInfoMML(MML)
#' @keywords utilities

GetMunicipalityInfoMML <- function (MML) {

  .InstallMarginal("reshape2")

  # Municipality information table from Maanmittauslaitos
  mml.table <- MML[["1_milj_Shape_etrs_shape"]][["kunta1_p"]]
  mml.table$Kunta.MML <- mml.table$Kunta.FI
  mml.table <- mml.table[c("AVI.FI", "Kieli.FI", "Suuralue.FI", "Maakunta.FI", "Seutukunta", "Kunta.FI", "Kunta.MML")]
  names(mml.table) <- c("AVI", "Kieli", "Suuralue", "Maakunta", "Seutukunta", "Kunta", "Kunta.MML")
  # Hammarland appears in the table twice but only SHAPE_Leng and SHAPE_Area
  # differ, otherwise the two are identical -> Remove the duplicate 
  rmind <- which(mml.table$Kunta == "Hammarland")[[2]]
  mml.table <- as.data.frame(mml.table)[-rmind,]

  # Use MML municipality names except Parainen:
  # Lansi-Turunmaa changed its name to Parainen since 2012
  kuntanimi <- as.character(mml.table$Kunta)
  kuntanimi[kuntanimi == "Länsi-Turunmaa"] <- "Parainen"
  rownames(mml.table) <- kuntanimi
  # Drop off Kunta field as redundant
  mml.table <- mml.table[, -which(colnames(mml.table) == "Kunta")]

  # FIXME: Kunta is factor but Maakunta is character and 
  # UTF-8 does not seem to be working with Maakunta field
  
  mml.table

}


#' List province for each municipality in Finland.
#' @aliases municipality2province
#' @param municipalities NULL 
#' @param municipality.info NULL 
#' @return Mapping vector listing the province for each municipality in Finland.
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples 
#' # Info table for municipalities:
#' # municipality.info <- GetMunicipalityInfo()
#' # List all municipalities: 
#' # all.municipalities <- as.character(municipality.info$Kunta) 
#' # Pick province for given municipalities:
#' # m2p <- FindProvince(c("Helsinki", "Tampere", "Turku")) # mapping between municipalities (kunta) and provinces (maakunta)
#' # Speed up by providing predefined table of municipality info:
#' # m2p <- FindProvince(c("Helsinki", "Tampere", "Turku"), municipality.info)
#' @keywords utilities

FindProvince <- function (municipalities = NULL, municipality.info = NULL) {

  if (is.null(municipality.info)) { 
    municipality.info <- sorvi::GetMunicipalityInfo()
  }

  m2p <- as.character(municipality.info$Maakunta)
  names(m2p) <- as.character(municipality.info$Kunta)

  if (!is.null(municipalities)) {
    m2p <- m2p[municipalities]
  }

  m2p

}

