rm(list=ls())

options(stringsAsFactors = F)
#____________________________________________________________________________
#.....Functions

#____________________________________________________________________________


source("/Users/Anna/Documents/workflows/Sex Roles in Birds/birds/functions.R")

#__________________________________________________________________________________________________________________
# Settings

qcmnames = c("qc", "observer", "ref", "n", "notes")
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
                                 observer = "George Brooks", qc = NULL, ref = NULL, n = NULL, notes = NULL)))

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
dl <- c(dl, list(D12 = processDat("Cockburn 2006_Appendix A resub.csv", label = F, taxo.dat, var.omit,
                                  observer = "unknown")))

processBirdFuncTxt()
dl <- c(dl, list(D13 = processDat("BirdFuncDat.csv", label = F, taxo.dat, var.omit,
                                  observer = NULL, qc = NULL, ref = NULL)))

#...Environmental vars.......................

dl <- c(dl, list(D14 = processDat("BioClim.csv", label = F, taxo.dat, var.omit,
                                  ref = NULL)))

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
#MATCH MS

# Settings
#__________________________________________________________________________________________________________________

# Read in match params for individual dataset matches
FUN.params <- c("data.match", "match")

for(FUN.param in FUN.params){
  assign(paste(FUN.param, "params", sep ="."), read.csv(paste("r data/params/",FUN.param," params.csv", sep = ""),
                                                        stringsAsFactors=FALSE))}

# Set folders
output.folder <- "/Users/Anna/Google Drive/Sex Roles in Birds Data Project/Outputs/"
input.folder <- "/Users/Anna/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/"

#__________________________________________________________________________________________________________________
#CREATE MASTER
#__________________________________________________________________________________________________________________

# Assign spp.list from species in original dataset D3
spp.list <- data.frame(species = dl[["D3"]]$data$species)

# create master shell
master <- c()

# match and append processed data to master
for(data.ID in names(dl)[-16]){
  
  # Create match object
  m <-  matchObj(data.ID, spp.list, data = dl[[data.ID]]$data, status = "unmatched", 
                 sub = data.match.params$sub[data.match.params$data.ID == data.ID],
                 match.params = match.params, qcref = dl[[data.ID]]$qcref) 
  
  # Match data set to spp.list and process
  output <- matchMSToMaster(m, taxo.var = taxo.var, var.omit = var.omit, input.folder,
                           output.folder = output.folder, ignore.unmatched = F)
 
  master <- rbind(master, output$mdat)
  spp.list <- output$spp.list
}



#__________________________________________________________________________________________________________________

# Make corrections.....................................................................

master$species[master$species == "Nectarinia_neergardi"] <- "Nectarinia_neergaardi"
master$species[master$species == "Brachypteracias_squamiger"] <- "Brachypteracias_squamigera"


master$var[which(master$value == 15.5 & master$var == "inc")] <- "inc.dur" #obviously refering to duration
master$var[which(master$var == "postf.feed" & master$value %in% c(31.5, 34  ,35 ,93))] <- "post.dur" #obviously refering to duration 

master$value <- trimws(master$value)

master <- data.frame(ID = 1:nrow(master), master)
write.csv(master, "~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/master data sheet (long).csv",
          row.names = F)


### PREPARE APP DATA #####################################################
dat <- master[, c("species", "family", "var", "value", "data", "ID")]
  # Remove variables
  dat <- dat[!dat$var %in% c("m.pre.care",
                             "m.incub",
                             "m.post.care",
                             "f.pre.care",
                             "f.incub",
                             "f.post.care",
                             "m.mating.system",
                             "f.mating.system"),]

  write.csv(dat, "~/Documents/workflows/Sex Roles in Birds/birds/bird app/data/data.csv",
          row.names = F)

  
write.csv(output$spp.list, "~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/master species list.csv",
          row.names = F)


#________________________________________________________________________________________________
#.....MANUAL MATCH Birdlife CODE
#________________________________________________________________________________________________

whichNext(x = m, pm = data.match.params)
testSynonym("Aratinga acuticaudata", x = output, pm = data.match.params)






#________________________________________________________________________________________________
#.....Quick check

any("Accipiter_sylvestris" %in% output$spp.list$species)
any("Accipiter_sylvestris" %in% m$data$species)




