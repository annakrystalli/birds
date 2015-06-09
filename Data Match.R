rm(list=ls())

#__________________________________________________________________________________________________________________
#CREATE MASTER
#__________________________________________________________________________________________________________________

setwd("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/")

D3 <- as.data.frame(read.csv("standardised csv data/BirdListBreedingSystem2George.csv", stringsAsFactors=FALSE))
BS.varnames <- as.vector(read.table("r data/BS varnames.csv", quote="\"")[,"V1"])


#Make master spreadsheet. Copy first three columns (index, species, family) straight across. 
#Set species names as rownames

master <- data.frame(matrix(NA, ncol= length(BS.varnames), nrow = dim(D3)[1]))
names(master) <-BS.varnames
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

#Open files
#...Bio vars....................... 
D4 <- read.csv("standardised csv data/Elliot New Data 7th November 2014.csv", stringsAsFactors=FALSE)
D1 <- read.csv("standardised csv data/M.System _ D.Mode Data, Species 4999 - 9993_extra.csv", stringsAsFactors=FALSE)
D2 <- read.csv("standardised csv data/M.System _ D.Mode Data, Species 4999 - 9993.csv", stringsAsFactors=FALSE)
D5 <- read.csv("standardised csv data/clutch size.csv", stringsAsFactors=FALSE)
D6 <- read.csv("standardised csv data/bird_ssd7.csv", stringsAsFactors=FALSE)
D7 <- read.csv("standardised csv data/ASR_mortality_to_Anna_Gavin.csv", stringsAsFactors=FALSE)
D8 <- read.csv("standardised csv data/life_history_to_Anna_Gavin.csv", stringsAsFactors=FALSE)
D9 <- read.csv("standardised csv data/breeding_system_to_Anna_Gavin.csv", stringsAsFactors=FALSE)
#source("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/scripts/Cockburn data process.R", local = T)
D12 <- read.csv("standardised csv data/Cockburn 2006_Appendix A resub.csv", stringsAsFactors=FALSE)
    names(D12)[which(names(D12) != "species")] <- paste(names(D12)[which(names(D12) != "species")], "_D12", sep = "")
D13 <- labelVars(read.csv("standardised csv data/BirdFuncDat.csv", stringsAsFactors=FALSE), "D13")

#...Environmental vars....................... 
D14 <- read.csv("standardised csv data/birdlife spp list.csv", stringsAsFactors=FALSE)
D14 <- D14[!(duplicated(D14$species) & D14$status != "R"),]




#...Unresolved vars....................... 
D15 <- read.table("standardised csv data/HBW spp list.csv", 
                  header=TRUE, quote="\"", stringsAsFactors=FALSE)
D15$status <- "R"

#Create data.list
data.list <- list(D1 = D1, D2 = D2, D3 = D3, D4 = D4, D5 = D5, D6 = D6, D7 = D7, 
                  D8 = D8, D9 = D9, D12 = D12, D13 = D13, D14 = D14)

#Match Mating System datasheets (D1-D4)
master <- matchMSToMaster(data.list, "D3" ,master, overwrite = F)
master <- matchMSToMaster(data.list, "D2" ,master, overwrite = T)
master <- matchMSToMaster(data.list, "D1" ,master, overwrite = T)
master <- matchMSToMaster(data.list, "D4" ,master, overwrite = T)

#Correct error to spp name on master
master$species[master$species == "Nectarinia_neergardi"] <- "Nectarinia_neergaardi"
master$species[master$species == "Brachypteracias_squamiger"] <- "Brachypteracias_squamigera"



for(i in c(1, 5:(length(BS.varnames)-1))){
  if(sum(!is.na(as.numeric(master[,BS.varnames[i]])))==0){}else{
    master[,BS.varnames[i]] <- as.numeric(master[,BS.varnames[i]]) 
  }}




#__________________________________________________________________________________________________________________
#MATCH D5 - D13
#__________________________________________________________________________________________________________________

output <- list(master = master)

for(i in 1:7){
  
# Add D5 to D13.....................................................................
output <- dataSppMatch(data.list, data.ID = data.match.params$data.ID[i], 
                       master = output$master, sub = data.match.params$sub[i], match.params = match.params)

output$master <- addVars(output$data, output$master)
}



# Make corrections.....................................................................
output$master$mpg_D9 <- output$master$mpg_D9/100  #change to prop for consistency
output$master$fpg_D9 <- output$master$fpg_D9/100  #change to prop for consistency
output$master$inc[which(output$master$inc == 15.5)] <- NA #obviously incorrect
output$master$postf.feed[which(output$master$postf.feed %in% c(31.5, 34  ,35 ,93))] <- NA #obviously incorrect 


for(i in which(sapply(output$master, FUN = class) == "character")){
    output$master[,i] <-  str_trim(output$master[,i], side = "both")}
   

write.csv(output$master, "~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/master Data Sheet.csv")





#________________________________________________________________________________________________
#.....MANUAL MATCH Birdlife CODE
________________________________________________________________________________________________
testSynonym <- function(spp, data, data.ID){
  #identify next species being matched and print
  mmatch <- read.csv(paste("r data/match data/",data.ID," mmatched.csv", sep = ""),
                     stringsAsFactors=FALSE)    
  
  spp.m <- mmatch$species[min(which(mmatch$synonyms == ""))]
  print(paste("match species:", spp.m))
  
  #test potential synonym 
  spp <- gsub(" ","_", spp)
  match <- any(spp %in% data$species)  
  print(paste("match to synonym:",spp))
  print(match)
  
  if(match){
    
    mmatch[mmatch$species == spp.m,"synonyms"] <- spp
    next.spp <- mmatch$species[min(which(mmatch$synonyms == ""))]
    
    print(paste("next species:", next.spp))
    write.csv(mmatch, paste("r data/match data/",data.ID," mmatched.csv", sep = ""),
              row.names = F)
    
  }
  
}
whichNext <- function(data.ID){
  
  mmatch <- read.csv(paste("r data/match data/",data.ID," mmatched.csv", sep = ""),
                     stringsAsFactors=FALSE)    
  
  spp.m <- mmatch$species[min(which(mmatch$synonyms == ""))]
  print(paste("match species:", spp.m))  
}


whichNext("D14")
testSynonym("Alario alario", data, "D15")


any("Stizorhina_finschi" %in% output$data$species)



#________________________________________________________________________________________________
#.....MANUAL MATCH CODE
________________________________________________________________________________________________
any("Geobiastes_squamiger" %in% master$species)


#________________________________________________________________________________________________
#.....QC Variables
________________________________________________________________________________________________

