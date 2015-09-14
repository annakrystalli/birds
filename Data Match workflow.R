rm(list=ls())

options(stringsAsFactors = F)
#____________________________________________________________________________
#.....Functions

#____________________________________________________________________________


source("/Users/Anna/Documents/workflows/Sex Roles in Birds/birds/functions1.R")

#__________________________________________________________________________________________________________________
# Settings

qcmnames = c("qc", "observer", "ref", "n")
var.omit <- c("andras.dat", "elliott.dat", "dm.dat", "ms.dat", "aus_ind_birds", "index")
taxo.var <- c("species", "order","family", "subspp", "parent.spp")
var.var <- c("var", "value", "data")
#__________________________________________________________________________________________________________________
#LOAD DATA
#__________________________________________________________________________________________________________________

setwd("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/")

dl <- NULL

#Open files
#...Bio vars....................... 
dl <- c(dl, list(D3 = processDat("BirdListBreedingSystem2George.csv", label = F, 
                                 taxo.dat, var.omit, 
                                 observer = "George Brooks", qc = NULL, ref = NULL, n = NULL)))

dl <- c(dl, list(D4 = processDat("Elliot New Data 7th November 2014.csv", label = F, 
                                 taxo.dat, var.omit,
                                 observer = "Elliot Capp", qc = NULL, ref = NULL, n = NULL)))
dl <- c(dl, list(D1 = processDat("M.System _ D.Mode Data, Species 4999 - 9993_extra.csv", 
                                 label = F, taxo.dat, var.omit,
                                 observer = "Emma Hughes", qc = NULL, ref = NULL, n = NULL)))
dl <- c(dl, list(D2 = processDat("M.System _ D.Mode Data, Species 4999 - 9993.csv", 
                                 label = F, taxo.dat, var.omit,
                                 observer = "Emma Hughes", qc = NULL, ref = NULL, n = NULL)))
dl <- c(dl, list(D5 = processDat("clutch size.csv", label = F, 
                                 taxo.dat, var.omit,
                                 observer = "unknown", qc = NULL, ref = NULL, n = NULL)))

refCode2Source(filename = "bird_ssd7")
refCode2Source(filename = "bird_ssd7_clutch.size.akif", sources = "bird_ssd7")

dl <- c(dl, list(D6 = processDat("bird_ssd7.csv", label = F, taxo.dat, var.omit,
                                 observer = "unknown", qc = NULL, ref = NULL)))
dl <- c(dl, list(D6a = processDat("bird_ssd7_clutch.size.akif.csv", label = F, 
                                  taxo.dat, var.omit,
                                  observer = "unknown", qc = NULL, ref = NULL)))
    dl[["D6a"]]$data$clutch.size[dl[["D6a"]]$data$species == "Zonotrichia_leucophrys"] <- 4.5

dl <- c(dl, list(D7 = processDat("ASR_mortality_to_Anna_Gavin.csv", label = F,
                 taxo.dat, var.omit, observer = "Andras")))

dl <- c(dl, list(D8 = processDat("life_history_to_Anna_Gavin.csv", label = F, 
                                 taxo.dat, var.omit, 
                                 observer = "Andras")))
dl <- c(dl, list(D8a = processDat("life_history_to_Anna_Gavin_ctm_all.csv", 
                                  label = F, taxo.dat, var.omit,
                                  observer = "Andras")))
dl <- c(dl, list(D8b = processDat("life_history_to_Anna_Gavin_ctm_rel.csv", 
                                  label = F, taxo.dat, var.omit,
                                  observer = "Andras")))


dl <- c(dl, list(D9 = processDat("breeding_system_to_Anna_Gavin.csv", label = F, 
                                 taxo.dat, var.omit,
                                 observer = "Andras")))
    dl[["D9"]]$data$mpg <- dl[["D9"]]$data$mpg/100  #change to prop for consistency
    dl[["D9"]]$data$fpg <- dl[["D9"]]$data$fpg/100  #change to prop for consistency

#source("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/scripts/Cockburn data process.R", local = T)
dl <- c(dl, list(D12 = processDat("Cockburn 2006_Appendix A resub.csv", label = F, taxo.dat, var.omit)))

processBirdFuncTxt()
dl <- c(dl, list(D13 = processDat("BirdFuncDat.csv", label = F, taxo.dat, var.omit,
                                  observer = NULL, qc = NULL, ref = NULL)))

#...Environmental vars.......................

dl <- c(dl, list(D14 = processDat("BioClim.csv", label = F, taxo.dat, var.omit,
                                  ref = "bioclim")))

#D14 dl <- c(dl, list(D7 = processDat("birdlife spp list.csv", label = F, taxo.dat, var.omit)))
#D14 <- D14[!(duplicated(D14$species) & D14$status != "R"),]

dl <- c(dl, list(D15 = processDat("global bird body masses - no subspp.csv", 
                                     label = F, taxo.dat, var.omit,
                                  ref = " Dunning, Avian Body Masses, 2nd Edition (2008, CRC Press)")))
dl <- c(dl, list(D16 = processDat("plumage.csv", label = F, taxo.dat, var.omit)))


#...Unresolved vars....................... 
#D16 <- read.table("standardised csv data/HBW spp list.csv", 
#                  header=TRUE, quote="\"", stringsAsFactors=FALSE)
#D16$status <- "R"




#__________________________________________________________________________________________________________________
#CREATE MASTER
#__________________________________________________________________________________________________________________

#Make master spreadsheet. Copy first three columns (index, species, family) straight across. 


spp.list <- data.frame(species = dl[["D3"]]$data$species)


# create master shell. fill with NAs
master <- data.frame(matrix(NA, ncol= length(c(taxo.var, var.var, "synonyms", "data.status",
                                               qcmnames)), nrow = 1))
names(master) <- c(taxo.var, var.var, "synonyms", "data.status", qcmnames)

#__________________________________________________________________________________________________________________
#MATCH MS
#__________________________________________________________________________________________________________________

# Read in match params for individual dataset matches
FUN.params <- c("data.match", "match")

for(FUN.param in FUN.params){
  assign(paste(FUN.param, "params", sep ="."), read.csv(paste("r data/params/",FUN.param," params.csv", sep = ""),
                                                        stringsAsFactors=FALSE))}

# Set folders
output.folder <- "/Users/Anna/Google Drive/Sex Roles in Birds Data Project/Outputs/"
input.folder <- "/Users/Anna/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/"


for(data.ID in names(dl)){

m <-  matchObj(data.ID, spp.list, data = dl[[data.ID]]$data, status = "unmatched", 
                       sub = data.match.params$sub[data.match.params$data.ID == data.ID],
                       match.params = match.params, qcref = dl[[data.ID]]$qcref)  
  
#Match Mating System datasheets (D1-D4)
 output <- matchMSToMaster(m, taxo.var = taxo.var, var.omit = var.omit, 
                                        input.folder,
                           output.folder = output.folder)
 
master <- rbind(master, output$mdat)
spp.list <- output$spp.list
}


#Correct error to spp name on master
master$species[master$species == "Nectarinia_neergardi"] <- "Nectarinia_neergaardi"
master$species[master$species == "Brachypteracias_squamiger"] <- "Brachypteracias_squamigera"



for(i in c(1, 6:(length(master.vnames)))){
  if(sum(!is.na(as.numeric(master[,master.vnames[i]])))==0){}else{
    master[,master.vnames[i]] <- as.numeric(master[,master.vnames[i]]) 
  }}




#__________________________________________________________________________________________________________________
#MATCH D5 - D13
#__________________________________________________________________________________________________________________

output <- list(master = master)

for(i in 1:(dim(data.match.params)[1])){
  
  # Add D5 to D13.....................................................................
  output <- dataSppMatch(data.list, data.ID = data.match.params$data.ID[i], 
                         master = output$master, sub = data.match.params$sub[i], match.params = match.params)
  
  output$master <- addVars(output$data, output$master)
}



# Make corrections.....................................................................

output$master$inc[which(output$master$inc == 15.5)] <- NA #obviously incorrect
output$master$postf.feed[which(output$master$postf.feed %in% c(31.5, 34  ,35 ,93))] <- NA #obviously incorrect 


for(i in which(sapply(output$master, FUN = class) == "character")){
  output$master[,i] <-  str_trim(output$master[,i], side = "both")}


write.csv(output$master, "~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/master Data Sheet.csv",
          row.names = F)





#________________________________________________________________________________________________
#.....MANUAL MATCH Birdlife CODE
________________________________________________________________________________________________
testSynonym <- function(spp, x, pm = data.match.params){
  #identify next species being matched and print
  mmatch <- read.csv(paste("r data/match data/",x$data.ID," mmatched.csv", sep = ""),
                     stringsAsFactors=FALSE)  
  
  sub <- pm$sub[pm$data.ID == x$data.ID] 
  
  
  if(sub == "master"){
    set <- "data"
    lookup <- "species"
    lookupin <- "synonyms"
  }
  if(sub == "data"){
    set <- "master"
    lookup <- "synonyms"
    lookupin <- "species"
  }   
  
  spp.m <- mmatch[[lookup]][min(which(mmatch[lookupin] == "" | is.na(mmatch[[lookupin]])))]
  print(paste("match", lookup, spp.m))
  
  #test potential synonym 
  if(spp %in% c("Extinct","New")){
    print(paste("match", lookupin, spp))
    mmatch[mmatch[lookup] == spp.m, lookupin] <- spp
    next.spp <-mmatch[[lookup]][min(which(mmatch[lookupin] == "" | is.na(mmatch[[lookupin]])))]
    
    print(paste("next", lookup, ":", next.spp))
    write.csv(mmatch, paste("r data/match data/",x$data.ID," mmatched.csv", sep = ""),
              row.names = F)
  }else{
    spp <- gsub(" ","_", spp)
    match <- any(spp %in% x[[set]]$species)  
    print(paste("match", lookupin, spp))
    print(match)
    
    if(match){
      
      mmatch[mmatch[lookup] == spp.m, lookupin] <- spp
      next.spp <-mmatch[[lookup]][min(which(mmatch[lookupin] == "" | is.na(mmatch[[lookupin]])))]
      
      print(paste("next", lookup, ":", next.spp))
      write.csv(mmatch, paste("r data/match data/",x$data.ID," mmatched.csv", sep = ""),
                row.names = F)
      
    }}
  
}
whichNext <- function(x = output, pm = data.match.params){
  #identify next species being matched and print
  mmatch <- read.csv(paste("r data/match data/",x$data.ID," mmatched.csv", sep = ""),
                     stringsAsFactors=FALSE)  
  
  sub <- pm$sub[pm$data.ID == x$data.ID] 
  
  
  if(sub == "master"){
    set <- "data"
    lookup <- "species"
    lookupin <- "synonyms"
  }
  if(sub == "data"){
    set <- "master"
    lookup <- "synonyms"
    lookupin <- "species"
  }   
  
  spp.m <- mmatch[[lookup]][min(which(mmatch[lookupin] == "" | is.na(mmatch[[lookupin]])))]
  print(paste("match", lookup, spp.m))
  
}


whichNext(x = output, pm = data.match.params)
testSynonym("Aratinga acuticaudata", x = output, pm = data.match.params)


any("Gallinula nesiotis" %in% output$data$species)



#________________________________________________________________________________________________
#.....MANUAL MATCH CODE
________________________________________________________________________________________________
any("Accipiter_pulchellus" %in% output$master$species)


#________________________________________________________________________________________________
#.....QC Variables
________________________________________________________________________________________________

nm <- gsub("_D6", "", names(output$master))

nm[duplicated(nm)]



