# This file is a part of the soRvi program (http://louhos.github.com/sorvi/)

# Copyright (C) 2010-2012 Louhos <louhos.github.com>. All rights reserved.

# This program is open source software; you can redistribute it and/or modify 
# it under the terms of the FreeBSD License (keep this notice): 
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  
#' GetMunicipalElectionData2000
#'
#' Get municipal election data from Statistics Finland (C) 2012
#' http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/kvaa_2000_fi.asp
#' 
#' @param which Indicate which of the available Statistics Finland data sets to parse. Options: election.statistics, candidates, selected.candidates.by.region, selected.candidates.all, parties, all.municipality.level.data
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2000 <- function (which = "election.statistics") {


  .InstallMarginal("plyr")
  .InstallMarginal("reshape2")

  
  if (which == "election.statistics") {

    #Kunnallisvaalit 2000, äänestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/010_kvaa_2000_2008-10-17_tau_101_fi.px"
    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Alue ~ Äänestystiedot")
    tab <- reshape2::cast(df, kaava, value="dat")
    rownames(tab) <- as.character(tab$Alue)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(as.character(tab$Alue), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available

    # Parse municipality codes and names

    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "candidates") {

    #Ehdokkaat puolueittain vaalipiirin ja kunnan mukaan kunnallisvaaleissa 2000
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/020_kvaa_2000_2008-10-17_tau_102_fi.px"
    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    tmp <- reshape2::cast(df, Alue ~ Puolue ~ Ehdokastiedot, value="dat")

    tab1 <- tmp[,,"Ehdokkaiden lkm"]
    tab2 <- tmp[,,"Ehdokkaiden osuus (%)"]
    tab3 <- tmp[,,"Naisehdokkaiden lkm"]
    tab4 <- tmp[,,"Naisten osuus ehdokkaista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Ehdokkaiden lkm"]), "(Ehdokkaiden lkm)")
    colnames(tab2) <- paste(colnames(tmp[,,"Ehdokkaiden osuus (%)"]), "(Ehdokkaiden osuus)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden lkm"]), "(Naisehdokkaiden lkm)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus ehdokkaista (%)"]), "(Naisten osuus ehdokkaista)")
    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "selected.candidates.by.region") {

    #Valitut puolueittain vaalipiirin ja kunnan mukaan kunnallisvaaleissa 2000
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/030_kvaa_2000_2008-10-17_tau_103_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    tmp <- reshape2::cast(df, Alue ~ Puolue ~ Valittujen.tiedot, value="dat")

    tab1 <- tmp[,,"Valittujen lkm"]
    tab2 <- tmp[,,"Valittujen osuus (%)"]
    tab3 <- tmp[,,"Valittujen naisten lkm"]
    tab4 <- tmp[,,"Naisten osuus valituista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valittujen lkm"]), "(Valittujen lkm)")
    colnames(tab2) <- paste(colnames(tmp[,,"Valittujen osuus (%)"]), "(Valittujen osuus)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen naisten lkm"]), "(Naisehdokkaiden lkm)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisten osuus valituista (%)"]), "(Naisten osuus valituista)")
    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "parties") {

    #Kunnallisvaalit 2000, puolueiden kannatus
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/040_kvaa_2000_2008-10-17_tau_104_fi.px"
    px <- pxR::read.px(url)
    df <- as.data.frame(px)

    tmp <- reshape2::cast(df, Alue ~ Puolue ~ Kannatustiedot, value="dat")

    tab1 <- tmp[,,"Ääniä yhtensä"]
    tab2 <- tmp[,,"Ennakkoäänet"]
    tab3 <- tmp[,,"Naisehdokkaiden äänimäärä"]
    tab4 <- tmp[,,"Naisehdokkaiden osuus äänistä (%)"]
    tab5 <- tmp[,,"Osuus äänistä (%)"]
    tab6 <- tmp[,,"Osuus ennakkoäänistä (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Ääniä yhtensä"]), "(Ääniä yhtensä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Ennakkoäänet"]), "(Ennakkoäänet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naisehdokkaiden äänimäärä"]), "(Naisehdokkaiden äänimäärä)")
    colnames(tab4) <- paste(colnames(tmp[,,"Naisehdokkaiden osuus äänistä (%)"]), "(Naisehdokkaiden osuus äänistä (%))")
    colnames(tab5) <- paste(colnames(tmp[,,"Osuus äänistä (%)"]), "(Osuus äänistä (%))")
    colnames(tab6) <- paste(colnames(tmp[,,"Osuus ennakkoäänistä (%)"]), "(Osuus ennakkoäänistä (%))")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2000", colnames(tab))

  } else if (which == "selected.candidates.all") {

    #Kunnallisvaalit 2000, valitut ehdokkaat
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa_2000/050_kvaa_2000_2008-10-17_tau_105_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    tab <- reshape2::cast(df, Ehdokas ~ Ehdokastiedot, value="dat")

  } else if (which == "all.municipality.level.data") {

    tab1 <- sorvi::GetMunicipalElectionData2000("election.statistics")
    tab2 <- sorvi::GetMunicipalElectionData2000("candidates")
    tab3 <- sorvi::GetMunicipalElectionData2000("selected.candidates.by.region")
    tab4 <- sorvi::GetMunicipalElectionData2000("parties")

    municipalities <- sort(rownames(tab1))
    tab <- cbind(tab1[municipalities, ],
             tab2[municipalities, ],
      	     tab3[municipalities, ],
      	     tab4[municipalities, ])
  }

  tab

}

  
#' GetMunicipalElectionData2004
#'
#' Get municipal election data from Statistics Finland 2012
#' 
#' Taulukot tilastossa: 5. Kunnallisvaalit 2004 - vaalitulos, aanestaminen
#' http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/2004_05_fi.asp
#'
#' @param which Indicate which of the available Statistics Finland data sets to parse. 
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2004 <- function (which = "election.statistics") {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape2")

  if (which == "election.statistics") {

    #Kunnallisvaalit 2004, äänestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/010_KVAA_2004_2008-07-23_TAU_101_FI.px"
    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Alue~Äänestystiedot~Sukupuoli")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Sukupuolet yhteensä"]
    tab2 <- tmp[,,"Miehet"]
    tab3 <- tmp[,,"Naiset"]

    colnames(tab1) <- paste(colnames(tmp[,,"Sukupuolet yhteensä"]), "(Sukupuolet yhteensä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Miehet"]), "(Miehet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naiset"]), "(Naiset)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep(" 00", rnams)]
    rnams <- rnams[-grep(" 01", rnams)]
    rnams <- rnams[-grep(" 02", rnams)]
    rnams <- rnams[-grep(" 03", rnams)]
    rnams <- rnams[-grep(" 04", rnams)]
    rnams <- rnams[-grep(" 05", rnams)]
    rnams <- rnams[-grep(" 06", rnams)]
    rnams <- rnams[-grep(" 07", rnams)]
    rnams <- rnams[-grep(" 08", rnams)]
    rnams <- rnams[-grep(" 09", rnams)]
    rnams <- rnams[-grep(" 1", rnams)]
    rnams <- rnams[-grep(" 2", rnams)]
    rnams <- rnams[-grep(" 3", rnams)]
    rnams <- rnams[-grep(" 4", rnams)]
    rnams <- rnams[-grep(" 5", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    #rnams <- rnams[-grep(" 6", rnams)]
    #rnams <- rnams[-grep(" 7", rnams)]
    #rnams <- rnams[-grep(" 8", rnams)]
    #rnams <- rnams[-grep(" 9", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: coarse election region (vaalipiiri) information also available but discarded
    # NOTE: detailed election region information also available (below municipality level) but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 äänestystiedot", colnames(tab))

  } else if (which == "selected.candidates.by.election.region") {

    warning("Vaalipiiri level information; TODO")

    #Valittujen lukumäärä ja prosenttiosuudet puolueittain ja vaalipiireittäin kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-28_tau_107_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Puolue~Vaalipiiri~Lukumäärätiedot")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Valtuutettujen lukumäärä"]
    tab2 <- tmp[,,"Puolueen osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valtuutettujen lukumäärä"]), "(Valtuutettujen lukumäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Puolueen osuus"]), "(Puolueen osuus)")

    tab <- cbind(tab1, tab2)
    
    tab <- NULL

  } else if (which == "selected.candidates.count") {

    # Kunnallisvaalit 2004, valittujen lukumäärä
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/010_kvaa_2004_2008-08-28_tau_103_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Alue~Puolue~Sukupuoli~Valittujen.lukumäärä")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Kaikki ehdokkaat", "Valittujen lukumäärä"]
    colnames(tab1) <- paste("Kaikki ehdokkaat", "Valittujen lukumäärä", colnames(tab1))

    tab2 <- tmp[,,"Miesehdokkaat", "Valittujen lukumäärä"]
    colnames(tab2) <- paste("Miesehdokkaat", "Valittujen lukumäärä", colnames(tab2))

    tab3 <- tmp[,,"Naisehdokkaat", "Valittujen lukumäärä"]
    colnames(tab3) <- paste("Naisehdokkaat", "Valittujen lukumäärä", colnames(tab3))

    tab4 <- tmp[,,"Kaikki ehdokkaat", "Osuus valituista %"]
    colnames(tab4) <- paste("Kaikki ehdokkaat", "Osuus valituista %", colnames(tab4))

    tab5 <- tmp[,,"Miesehdokkaat", "Osuus valituista %"]
    colnames(tab5) <- paste("Miesehdokkaat", "Osuus valituista %", colnames(tab5))

    tab6 <- tmp[,,"Naisehdokkaat", "Osuus valituista %"]
    colnames(tab6) <- paste("Naisehdokkaat", "Osuus valituista %", colnames(tab6))

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,], 
    	         tab4[regs,], tab5[regs,], tab6[regs,])

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    
    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 valittujen lukumäärä", colnames(tab))
    
  } else if (which == "selected.candidates.by.party") {  

    # Valittujen lukumäärä ja prosenttiosuudet puolueittain ja 
    # vaalipiireittäin kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/650_kvaa_2004_2009-11-02_tau_141_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- pxR::read.px(url)
    #df <- as.data.frame(px)
    #tmp <- reshape2::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumäärä)

  } else if (which == "selected.candidates.count") {

    warning("Puoluetason tietoa, ei kuntia. TODO.")
    tab <- NULL

    #Valitut ikäryhmittäin sukupuolen ja puolueen mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-07-15_tau_110_fi.px"

  } else if (which == "selected.candidates.count") {

    #Valitut ikäryhmittäin sukupuolen mukaan vaalipiireittäin kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/670_kvaa_2004_2009-11-02_tau_143_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- pxR::read.px(url)
    #df <- as.data.frame(px)
    #tmp <- reshape2::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumäärä)

  } else if (which == "parties") {

    #Kunnallisvaalit 2004, puolueiden kannatus
    #url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/010_KVAA_2004_2008-08-28_TAU_102_FI.px"
    #df <- as.data.frame(pxR::read.px(url))
    # -> Segmentation fault
    warning("Segmentation fault at Kunnallisvaalit 2004, puolueiden kannatus, ignoring.")
    tab <- NULL

  } else if (which == "parties.per.region") {

    # Puolueiden äänimäärät ja prosenttiosuudet sekä äänestysprosentit 
    # vaalipiireittäin kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-27_tau_114_fi.px"
    warning("Vaalipiiri level, TODO")
    tab <- NULL

  } else if (which == "parties.change") {

    # Puolueiden äänimäärät ja äänestysprosentti sekä valittujen lukumäärä 
    # kunnittain kunnallisvaaleissa 2004 ja muutos edellisiin vaaleihin 
    # verrattuna
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-27_tau_111_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Vaalipiiri.ja.kunta~Puolue~Lukumäärätiedot")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Äänimäärä"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Muutos edelliseen vaaliin verrattuna"]
    tab4 <- tmp[,,"Valittujen lukumäärä"]

    colnames(tab1) <- paste(colnames(tmp[,,"Äänimäärä"]), "(Äänimäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Muutos edelliseen vaaliin verrattuna"]), "(Muutos edelliseen vaaliin verrattuna)")
    colnames(tab4) <- paste(colnames(tmp[,,"Valittujen lukumäärä"]), "(Valittujen lukumäärä)")

    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 puolueiden äänimäärät: ", colnames(tab))

  } else if (which == "party.votes") {

    #Puolueiden äänimäärät ja valittujen lukumäärä kunnittain (pienet puolueet), hylätyt liput sekä ennakkoäänestäneet kunnallisvaaleissa 2004
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-27_tau_114_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Vaalipiiri.ja.kunta~Äänestystiedot.ja.puolueiden.kannatus~Lukumäärätiedot")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Äänimäärä"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Valittujen lukumäärä"]

    colnames(tab1) <- paste(colnames(tmp[,,"Äänimäärä"]), "(Äänimäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen lukumäärä"]), "(Valittujen lukumäärä)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 puolueiden kannatus: ", colnames(tab))

  } else if (which == "voting.stats") {

    # Aanioikeutetut ja aanestaneet sukupuolen mukaan, hyvaksytyt aanestysliput, valtuutetuiksi valitut ja ennakkoaanet puolueittain sekä hylattyjen 
    # aanestyslippujen lukumaara kunnittain kunnallisvaaleissa 2004
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/060_kvaa_2004_2008-08-28_tau_116_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Vaalipiiri.ja.kunta~Äänestystiedot.ja.puolueiden.kannatus~Lukumäärätiedot")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Lukumäärä / Äänimäärä"]
    tab2 <- tmp[,,"Osuus äänistä"]
    tab3 <- tmp[,,"Valitut"]
    tab4 <- tmp[,,"Osuus valituista"]
    tab5 <- tmp[,,"Ennakkoäänet"]
    tab6 <- tmp[,,"Ennakkoäänten osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Lukumäärä / Äänimäärä"]), "(Lukumäärä / Äänimäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus äänistä"]), "(Osuus äänistä)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valitut"]), "(Valitut)")
    colnames(tab4) <- paste(colnames(tmp[,,"Osuus valituista"]), "(Osuus valituista)")
    colnames(tab5) <- paste(colnames(tmp[,,"Ennakkoäänet"]), "(Ennakkoäänet)")
    colnames(tab6) <- paste(colnames(tmp[,,"Ennakkoäänten osuus"]), "(Ennakkoäänten osuus)")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2004 muuta: ", colnames(tab))

  } else if (which == "previous.experience") {

    # Valituiksi tulleiden aikaisempi kokemus valtuustossa kuntatyypin, sukupuolen ja puolueen mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/730_kvaa_2004_2009-12-30_tau_149_fi.px"
    #px <- pxR::read.px(url)
    #df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "rejected") {

    #Hylätyt äänestysliput hylkäysperusteen ja vaalipiirin mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/740_kvaa_2004_2009-12-30_tau_150_fi.px"
    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "results") {

    #Kunnallisvaalit 2004, tulosanalyysi
    #http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_06/2004_06_fi.asp
    
    warning("No municipality level data available. TODO.")
    # NOTE: vaalipiiri level available
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_06/810_kvaa_2004_2004-10-27_tau_150_fi.px"
    #kvaa <- as.data.frame(pxR::read.px(url))
    tab <- NULL

  } else if (which == "pre") {

    #Ennakkoon äänestäneet äänestyspaikan ja vaalipiirin mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/750_kvaa_2004_2009-12-30_tau_151_fi.px"
    #kvaa <- as.data.frame(pxR::read.px(url))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad") {

    # Suomen ulkomaan edustustoissa ja laivoissa aanestaneet sukupuolen mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/760_kvaa_2004_2009-12-30_tau_152_fi.px"
    #kvaa <- as.data.frame(pxR::read.px(url))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad2") {

    #Äänioikeutetut ja äänestäneet ulkomaalaiset vaalipiirin mukaan kunnallisvaaleissa 2004
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2004_05/770_kvaa_2004_2009-12-30_tau_153_fi.px"
    kvaa <- as.data.frame(pxR::read.px(url))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "all.municipal") {

    tab1 <- sorvi::GetMunicipalElectionData2004("voting.stats")
    tab2 <- sorvi::GetMunicipalElectionData2004("party.votes")
    tab3 <- sorvi::GetMunicipalElectionData2004("parties.change")
    tab4 <- sorvi::GetMunicipalElectionData2004("selected.candidates.count")
    tab6 <- sorvi::GetMunicipalElectionData2004("election.statistics")

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,],
    	         tab4[regs,], tab6[regs,])

  }

  tab

}


  
#' GetMunicipalElectionData2008
#'
#' Get municipal election data 
#' 
#' @param which Indicate which of the available Statistics Finland data sets to parse. Options: 
#' @return data.frame
#' @export 
#' @references
#' See citation("sorvi") 
#' @author Leo Lahti \email{sorvi-commits@@lists.r-forge.r-project.org}
#' @examples # 
#' @keywords utilities
GetMunicipalElectionData2008 <- function (which = "election.statistics") {

  .InstallMarginal("plyr")
  .InstallMarginal("reshape2")

  # Taulukot tilastossa: 5. Kunnallisvaalit 2008 - vaalitulos, aanestaminen
  # http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/2008_05_fi.asp

  if (which == "election.statistics") {

    #Kunnallisvaalit 2008, aanestystiedot
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/610_kvaa_2008_2009-10-30_tau_137_fi.px"
    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Alue~Äänestystiedot~Sukupuoli")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Sukupuolet yhteensä"]
    tab2 <- tmp[,,"Miehet"]
    tab3 <- tmp[,,"Naiset"]

    colnames(tab1) <- paste(colnames(tmp[,,"Sukupuolet yhteensä"]), "(Sukupuolet yhteensä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Miehet"]), "(Miehet)")
    colnames(tab3) <- paste(colnames(tmp[,,"Naiset"]), "(Naiset)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep(" 00", rnams)]
    rnams <- rnams[-grep(" 01", rnams)]
    rnams <- rnams[-grep(" 02", rnams)]
    rnams <- rnams[-grep(" 03", rnams)]
    rnams <- rnams[-grep(" 04", rnams)]
    rnams <- rnams[-grep(" 05", rnams)]
    rnams <- rnams[-grep(" 06", rnams)]
    rnams <- rnams[-grep(" 07", rnams)]
    rnams <- rnams[-grep(" 08", rnams)]
    rnams <- rnams[-grep(" 09", rnams)]
    rnams <- rnams[-grep(" 1", rnams)]
    rnams <- rnams[-grep(" 2", rnams)]
    rnams <- rnams[-grep(" 3", rnams)]
    rnams <- rnams[-grep(" 4", rnams)]
    rnams <- rnams[-grep(" 5", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    #rnams <- rnams[-grep(" 6", rnams)]
    #rnams <- rnams[-grep(" 7", rnams)]
    rnams <- rnams[-grep(" 8", rnams)]
    #rnams <- rnams[-grep(" 9", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: coarse election region (vaalipiiri) information also available but discarded
    # NOTE: detailed election region information also available (below municipality level) but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 äänestystiedot", colnames(tab))

  } else if (which == "woman.candidates") {

    #Naisehdokkaitten vaalitiedot puolueen ja kunnan mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/720_kvaa_2008_2009-12-30_tau_148_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    tmp <- reshape2::cast(df, Kunta~Puolue~Naisehdokastiedot)

    tab1 <- tmp[,,"Äänimäärä"]
    tab2 <- tmp[,,"Osuus äänistä (%)"]
    tab3 <- tmp[,,"Ehdokkaat"]
    tab4 <- tmp[,,"Osuus ehdokkaista (%)"]
    tab5 <- tmp[,,"Valitut"]
    tab6 <- tmp[,,"Osuus valituista (%)"]

    colnames(tab1) <- paste(colnames(tmp[,,"Äänimäärä"]), "(Äänimäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus äänistä (%)"]), "(Osuus äänistä (%))")
    colnames(tab3) <- paste(colnames(tmp[,,"Ehdokkaat"]), "(Ehdokkaat)")
    colnames(tab4) <- paste(colnames(tmp[,,"Osuus ehdokkaista (%)"]), "(Osuus ehdokkaista (%))")
    colnames(tab5) <- paste(colnames(tmp[,,"Valitut"]), "(Valitut)")
    colnames(tab6) <- paste(colnames(tmp[,,"Osuus valituista (%)"]), "(Osuus valituista (%))")

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,], tab4[regs,], tab5[regs,], tab6[regs,])

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 naisehdokkaat", colnames(tab))

  } else if (which == "selected.candidates.by.election.region") {

    warning("Vaalipiiri level information; TODO")

    #Valittujen lukumäärä ja prosenttiosuudet puolueittain ja vaalipiireittäin kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/690_kvaa_2008_2009-11-02_tau_145_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Puolue~Vaalipiiri~Lukumäärätiedot")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Valtuutettujen lukumäärä"]
    tab2 <- tmp[,,"Puolueen osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Valtuutettujen lukumäärä"]), "(Valtuutettujen lukumäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Puolueen osuus"]), "(Puolueen osuus)")

    tab <- cbind(tab1, tab2)
    
    tab <- NULL

  } else if (which == "selected.candidates.count") {

    # Kunnallisvaalit 2008, valittujen lukumaara
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/630_kvaa_2008_2009-10-30_tau_139_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Alue~Puolue~Sukupuoli~Valittujen.lukumäärä")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Kaikki ehdokkaat", "Valittujen lukumäärä"]
    colnames(tab1) <- paste("Kaikki ehdokkaat", "Valittujen lukumäärä", colnames(tab1))

    tab2 <- tmp[,,"Miesehdokkaat", "Valittujen lukumäärä"]
    colnames(tab2) <- paste("Miesehdokkaat", "Valittujen lukumäärä", colnames(tab2))

    tab3 <- tmp[,,"Naisehdokkaat", "Valittujen lukumäärä"]
    colnames(tab3) <- paste("Naisehdokkaat", "Valittujen lukumäärä", colnames(tab3))

    tab4 <- tmp[,,"Kaikki ehdokkaat", "Osuus valituista %"]
    colnames(tab4) <- paste("Kaikki ehdokkaat", "Osuus valituista %", colnames(tab4))

    tab5 <- tmp[,,"Miesehdokkaat", "Osuus valituista %"]
    colnames(tab5) <- paste("Miesehdokkaat", "Osuus valituista %", colnames(tab5))

    tab6 <- tmp[,,"Naisehdokkaat", "Osuus valituista %"]
    colnames(tab6) <- paste("Naisehdokkaat", "Osuus valituista %", colnames(tab6))

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,], 
    	         tab4[regs,], tab5[regs,], tab6[regs,])

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    rnams <- rnams[-grep("Manner-Suomi", rnams)]
    tab <- as.data.frame(tab[rnams, ])
    
    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 valittujen lukumäärä", colnames(tab))
    
  } else if (which == "selected.candidates.by.party") {  

    # Valittujen lukumäärä ja prosenttiosuudet puolueittain ja 
    # vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/650_kvaa_2008_2009-11-02_tau_141_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- pxR::read.px(url)
    #df <- as.data.frame(px)
    #tmp <- reshape2::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumäärä)

  } else if (which == "selected.candidates.count") {

    warning("Puoluetason tietoa, ei kuntia. TODO.")
    tab <- NULL

    # Valitut ikaryhmittain sukupuolen ja puolueen mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/660_kvaa_2008_2009-11-02_tau_142_fi.px"

  } else if (which == "selected.candidates.count") {

    #Valitut ikaryhmittain sukupuolen mukaan vaalipiireittain kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/670_kvaa_2008_2009-11-02_tau_143_fi.px"

    warning("Vaalipiiritason tietoa. TODO.")
    tab <- NULL

    #px <- pxR::read.px(url)
    #df <- as.data.frame(px)
    #tmp <- reshape2::cast(df, Alue~Puolue~Sukupuoli~Valittujen.lukumäärä)

  } else if (which == "parties") {

    #Kunnallisvaalit 2008, puolueiden kannatus
    #url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/620_kvaa_2008_2009-10-30_tau_138_fi.px"
    #df <- as.data.frame(pxR::read.px(url))
    # -> Segmentation fault
    warning("Segmentation fault at Kunnallisvaalit 2008, puolueiden kannatus, ignoring.")
    tab <- NULL

  } else if (which == "parties.per.region") {

    # Puolueiden äänimäärät ja prosenttiosuudet sekä äänestysprosentit vaalipiireittäin kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/640_kvaa_2008_2009-11-02_tau_140_fi.px"

    warning("Vaalipiiri level, TODO")
    tab <- NULL

  } else if (which == "parties.change") {

    # Puolueiden aanimäärät ja aanestysprosentti sekä valittujen lukumaara
    # kunnittain kunnallisvaaleissa 2008 ja muutos edellisiin vaaleihin 
    # verrattuna
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/680_kvaa_2008_2009-11-02_tau_144_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Vaalipiiri.ja.kunta~Puolue~Lukumäärätiedot")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Äänimäärä"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Muutos edelliseen vaaliin verrattuna"]
    tab4 <- tmp[,,"Valittujen lukumäärä"]

    colnames(tab1) <- paste(colnames(tmp[,,"Äänimäärä"]), "(Äänimäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Muutos edelliseen vaaliin verrattuna"]), "(Muutos edelliseen vaaliin verrattuna)")
    colnames(tab4) <- paste(colnames(tmp[,,"Valittujen lukumäärä"]), "(Valittujen lukumäärä)")

    tab <- cbind(tab1, tab2, tab3, tab4)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 puolueiden äänimäärät: ", colnames(tab))

  } else if (which == "party.votes") {

    #Puolueiden äänimäärät ja valittujen lukumäärä kunnittain (pienet puolueet), hylätyt liput sekä ennakkoäänestäneet kunnallisvaaleissa 2008
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/700_kvaa_2008_2009-11-02_tau_146_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Vaalipiiri.ja.kunta~Äänestystiedot.ja.puolueiden.kannatus~Lukumäärätiedot")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Äänimäärä"]
    tab2 <- tmp[,,"Osuus %"]
    tab3 <- tmp[,,"Valittujen lukumäärä"]

    colnames(tab1) <- paste(colnames(tmp[,,"Äänimäärä"]), "(Äänimäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus %"]), "(Osuus %)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valittujen lukumäärä"]), "(Valittujen lukumäärä)")

    tab <- cbind(tab1, tab2, tab3)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 puolueiden kannatus: ", colnames(tab))

  } else if (which == "voting.stats") {

    # Aanioikeutetut ja aanestäneet sukupuolen mukaan, hyvaksytyt aanestysliput, valtuutetuiksi valitut ja ennakkoaanet puolueittain sekä 
    # hylattyjen aanestyslippujen lukumaara kunnittain kunnallisvaaleissa 2008
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/710_kvaa_2008_2009-11-02_tau_147_fi.px"

    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    kaava <- as.formula("Vaalipiiri.ja.kunta~Äänestystiedot.ja.puolueiden.kannatus~Lukumäärätiedot")
    tmp <- reshape2::cast(df, kaava, value="dat")

    tab1 <- tmp[,,"Lukumäärä / Äänimäärä"]
    tab2 <- tmp[,,"Osuus äänistä"]
    tab3 <- tmp[,,"Valitut"]
    tab4 <- tmp[,,"Osuus valituista"]
    tab5 <- tmp[,,"Ennakkoäänet"]
    tab6 <- tmp[,,"Ennakkoäänten osuus"]

    colnames(tab1) <- paste(colnames(tmp[,,"Lukumäärä / Äänimäärä"]), "(Lukumäärä / Äänimäärä)")
    colnames(tab2) <- paste(colnames(tmp[,,"Osuus äänistä"]), "(Osuus äänistä)")
    colnames(tab3) <- paste(colnames(tmp[,,"Valitut"]), "(Valitut)")
    colnames(tab4) <- paste(colnames(tmp[,,"Osuus valituista"]), "(Osuus valituista)")
    colnames(tab5) <- paste(colnames(tmp[,,"Ennakkoäänet"]), "(Ennakkoäänet)")
    colnames(tab6) <- paste(colnames(tmp[,,"Ennakkoäänten osuus"]), "(Ennakkoäänten osuus)")

    tab <- cbind(tab1, tab2, tab3, tab4, tab5, tab6)

    # Keep only municipality-level information, filter out others
    rnams <- setdiff(rownames(tab), c("Koko maa", "- Niistä Ruotsissa", "S Kaupunkimaiset kunnat", "S Maaseutumaiset kunnat", "S Taajaan asutut kunnat", "Suomessa asuvat Suomen kansalaiset", "Ulkomailla asuvat Suomen kansalaiset"))
    rnams <- rnams[-grep("Kaupunkimaiset kunnat", rnams)]
    rnams <- rnams[-grep("Taajaan asutut kunnat", rnams)]
    rnams <- rnams[-grep("Maaseutumaiset kunnat", rnams)]
    rnams <- rnams[-grep("vaalipiiri", rnams)]
    tab <- as.data.frame(tab[rnams, ])

    # NOTE: election region information also available but discarded

    # Parse municipality codes and names
    v <- plyr::ldply(strsplit(sorvi::ConvertMunicipalityNames(rownames(tab)), " "), function (x) {x})
    tab$Kuntakoodi <- v[,1]
    tab$Kunta <- v[,2]
    rownames(tab) <- as.character(tab$Kunta)
    colnames(tab) <- paste("Kunnallisvaalit 2008 muuta: ", colnames(tab))

  } else if (which == "previous.experience") {

    # Valituiksi tulleiden aikaisempi kokemus valtuustossa kuntatyypin, sukupuolen ja puolueen mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/730_kvaa_2008_2009-12-30_tau_149_fi.px"
    #px <- pxR::read.px(url)
    #df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "rejected") {

    # Hylatyt aanestysliput hylkaysperusteen ja vaalipiirin mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/740_kvaa_2008_2009-12-30_tau_150_fi.px"
    px <- pxR::read.px(url)
    df <- as.data.frame(px)
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "results") {

    #Kunnallisvaalit 2008, tulosanalyysi
    #http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_06/2008_06_fi.asp
    
    warning("No municipality level data available. TODO.")
    # NOTE: vaalipiiri level available
    url <- "http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_06/810_kvaa_2008_2008-10-27_tau_150_fi.px"
    #kvaa <- as.data.frame(pxR::read.px(url))
    tab <- NULL

  } else if (which == "pre") {

    #Ennakkoon aanestaneet aanestyspaikan ja vaalipiirin mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/750_kvaa_2008_2009-12-30_tau_151_fi.px"
    #kvaa <- as.data.frame(pxR::read.px(url))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad") {

    #Suomen ulkomaan edustustoissa ja laivoissa aanestaneet sukupuolen mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/760_kvaa_2008_2009-12-30_tau_152_fi.px"
    #kvaa <- as.data.frame(pxR::read.px(url))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "abroad2") {

    #Aanioikeutetut ja aanestaneet ulkomaalaiset vaalipiirin mukaan kunnallisvaaleissa 2008
    url<-"http://pxweb2.stat.fi/database/StatFin/vaa/kvaa/2008_05/770_kvaa_2008_2009-12-30_tau_153_fi.px"
    kvaa <- as.data.frame(pxR::read.px(url))
    warning("No municipality level data available. TODO.")
    tab <- NULL

  } else if (which == "all.municipal") {

    tab1 <- sorvi::GetMunicipalElectionData2008("voting.stats")
    tab2 <- sorvi::GetMunicipalElectionData2008("party.votes")
    tab3 <- sorvi::GetMunicipalElectionData2008("parties.change")
    tab4 <- sorvi::GetMunicipalElectionData2008("selected.candidates.count")
    tab5 <- sorvi::GetMunicipalElectionData2008("woman.candidates")
    tab6 <- sorvi::GetMunicipalElectionData2008("election.statistics")

    regs <- rownames(tab1)

    tab <- cbind(tab1[regs,], tab2[regs,], tab3[regs,],
    	         tab4[regs,], tab5[regs,], tab6[regs,])

  }

  tab

}

