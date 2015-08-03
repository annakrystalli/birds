rm(list=ls())


#____________________________________________________________________________
#.....Functions

#____________________________________________________________________________


source("/Users/Anna/Documents/workflows/Sex Roles in Birds/birds/functions.R")
#__________________________________________________________________________________________________________________
#LOAD DATA
#__________________________________________________________________________________________________________________

setwd("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/")

#Open files
#...Bio vars....................... 
D3 <- as.data.frame(read.csv("standardised csv data/BirdListBreedingSystem2George.csv", stringsAsFactors=FALSE))
D4 <- read.csv("standardised csv data/Elliot New Data 7th November 2014.csv", stringsAsFactors=FALSE)
D1 <- read.csv("standardised csv data/M.System _ D.Mode Data, Species 4999 - 9993_extra.csv", stringsAsFactors=FALSE)
D2 <- read.csv("standardised csv data/M.System _ D.Mode Data, Species 4999 - 9993.csv", stringsAsFactors=FALSE)
D5 <- labelVars(read.csv("standardised csv data/clutch size.csv", stringsAsFactors=FALSE), "D5", label = T)

D6 <- labelVars(read.csv("standardised csv data/bird_ssd7.csv", stringsAsFactors=FALSE), "D6", label = T)
D7 <- read.csv("standardised csv data/ASR_mortality_to_Anna_Gavin.csv", stringsAsFactors=FALSE)
D8 <- read.csv("standardised csv data/life_history_to_Anna_Gavin.csv", stringsAsFactors=FALSE)
D9 <- read.csv("standardised csv data/breeding_system_to_Anna_Gavin.csv", stringsAsFactors=FALSE)
    D9$mpg <- D9$mpg/100  #change to prop for consistency
    D9$fpg <- D9$fpg/100  #change to prop for consistency

#source("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/scripts/Cockburn data process.R", local = T)
D12 <- labelVars(read.csv("standardised csv data/Cockburn 2006_Appendix A resub.csv", stringsAsFactors=FALSE), "D12", label = F)
D13 <- labelVars(read.csv("standardised csv data/BirdFuncDat.csv", stringsAsFactors=FALSE), "D13", label = F)

#...Environmental vars.......................

D14 <- read.csv("standardised csv data/BioClim.csv",stringsAsFactors=FALSE)

      #D14 <- read.csv("standardised csv data/birdlife spp list.csv", stringsAsFactors=FALSE)
      #D14 <- D14[!(duplicated(D14$species) & D14$status != "R"),]

D15 <- labelVars(read.csv("standardised csv data/global bird body masses - no subspp.csv", stringsAsFactors=FALSE), "D15", label = T)

D16 <- labelVars(read.csv("standardised csv data/plumage.csv", stringsAsFactors=FALSE), "D16", label = F)


#...Unresolved vars....................... 
#D16 <- read.table("standardised csv data/HBW spp list.csv", 
#                  header=TRUE, quote="\"", stringsAsFactors=FALSE)
#D16$status <- "R"

#Create data.list
data.list <- list(D1 = D1, D2 = D2, D3 = D3, D4 = D4, D5 = D5, D6 = D6, D7 = D7, 
                  D8 = D8, D9 = D9, D12 = D12, D13 = D13, D14 = D14, D15 = D15, D16 = D16)


#__________________________________________________________________________________________________________________
#CREATE MASTER
#__________________________________________________________________________________________________________________

#Make master spreadsheet. Copy first three columns (index, species, family) straight across. 
#Set species names as rownames

#BS.varnames <- as.vector(read.table("r data/BS varnames.csv", quote="\"")[,"V1"])
master.vnames <- unique(unlist(lapply(data.list[c(1:4, 7:9)], FUN=names)))
master.vnames <- c(master.vnames[1:2], 
                   "subspp", "parent.spp", 
                   master.vnames[3:length(master.vnames)])

master <- data.frame(matrix(NA, ncol= length(master.vnames), nrow = dim(D3)[1]))
names(master) <-master.vnames
master[,match(names(D3[1:3]), names(master))] <-D3[,1:3]
master$subspp <- FALSE

rownames(master) <- as.character(master$species)


#__________________________________________________________________________________________________________________
#MATCH MS
#__________________________________________________________________________________________________________________

# Set folders
output.folder <- "/Users/Anna/Google Drive/Sex Roles in Birds Data Project/Outputs/"

# Read in match params for individual dataset matches
FUN.params <- c("data.match", "match")

for(FUN.param in FUN.params){
assign(paste(FUN.param, "params", sep ="."), read.csv(paste("r data/params/",FUN.param," params.csv", sep = ""),
         stringsAsFactors=FALSE))}



#Match Mating System datasheets (D1-D4)
master <- matchMSToMaster(data.list, "D9" ,master, overwrite = F)
master <- matchMSToMaster(data.list, "D8" ,master, overwrite = T)
master <- matchMSToMaster(data.list, "D7" ,master, overwrite = T)

master <- matchMSToMaster(data.list, "D3" ,master, overwrite = T)
master <- matchMSToMaster(data.list, "D2" ,master, overwrite = T)
master <- matchMSToMaster(data.list, "D1" ,master, overwrite = T)
master <- matchMSToMaster(data.list, "D4" ,master, overwrite = T)

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

