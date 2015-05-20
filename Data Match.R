rm(list=ls())
#__________________________________________________________________________________________________________________
#FUNCTIONS
#__________________________________________________________________________________________________________________


matchMSToMaster <-  function(data.list,data.id, master, overwrite = F, add.var = NULL){
  
  match.dat <- data.list[[data.id]]
  
  
  if(!is.null(add.var)){
    add.cols <- matrix(NA, ncol = length(add.var), nrow = dim(master)[1])
    names(add.cols) <- add.var
    master <- cbind(master, add.cols)
  }
  
  #Make sure there are no empty cells and replace any with NA cells
  match.dat[which(match.dat == "", arr.ind = T)] <- NA
  
  #make vector of match data variables and check that they match the master
  match.vars <- names(match.dat[,4:length(match.dat)])
  if(any(is.na(match.vars %in% names(master)))){stop("variable name mismatch")}
  
  #for(i in 1:length(match.vars)){
  #if(sum(!is.na(as.numeric(match.dat[,match.vars[i]])))==0){}else{
  # match.dat[,match.vars[i]] <- as.numeric(match.dat[,match.vars[i]])  
  #}}
  
  
  #find non NA values in match data. Match arr.indices to spp and variable names (for QA)
  id <- which(!is.na(match.dat[,4:length(match.dat)]), arr.ind = T)
  
  spp <- as.character(match.dat[,"species"][id[, "row"]])
  var <- match.vars[id[, "col"]]
  
  if(any(!(spp %in% master$species))){stop("species name mismatch")}
  if(any(!(var %in% names(master)))){stop("variable name mismatch")}
  if(anyDuplicated(match.dat$species) > 0){stop("duplicate species name in match data")}
  
  match.id  <- cbind(row = match(spp, master$species), 
                     col = match(var, names(master)))
  #check to see if any data will be overwritten
  
  check <- sum(!is.na(master[match.id]))
  
  #stop function if overwrite = F
  if(!overwrite){
    if(check > 0){errors <- cbind(as.character(master$species)[match.id[,1]][which(!is.na(master[match.id]), arr.ind = T)],
                                  names(master)[match.id[,2]][which(!is.na(master[match.id]), arr.ind = T)])
                  print(errors)
                  write.csv(errors, file=paste("r data/Data overwrite error reports/", data.id,".csv", sep = ""), 
                            row.names = F)
                  print(unique(errors[,2]))
                  stop("Function terminated. Data overwrite danger")}}
  
  #Notify of any data overwriting
  print(paste( check, "datapoints overwritten"))
  
  #match data to master
  master[match.id] <- match.dat[, 4:length(match.dat)][id]
  #Update ref column  
  names(master)[which(names(master) == "ref")] <- paste("ref.", data.id, sep = "")
  master <- cbind(master, ref = NA)
  return(master)}

addVarsToMaster <-  function(match.dat, master, overwrite = F, add.var){
  
  
  add.cols <- matrix(NA, ncol = length(add.var), nrow = dim(master)[1])
  a <- length(master) + 1
  master <- cbind(master, add.cols)
  names(master)[a:length(master)] <- add.var
  
  
  #Make sure there are no empty cells and replace any with NA cells
  match.dat[which(match.dat == "", arr.ind = T)] <- NA
  
  #find non NA values in match data. Match arr.indices to spp and variable names (for QA)
  id <- which(!is.na(match.dat[,add.var]), arr.ind = T)
  
  if(!is.null(dim(id))){
    spp <- as.character(match.dat[,"species"][id[, "row"]])
    var <- match.vars[id[, "col"]]}else{
      spp <- as.character(match.dat[,"species"][id])
      var <- rep(add.var,l = length(spp))                    
    }
  
  
  if(any(!(spp %in% master$species))){stop("species name mismatch")}
  if(any(!(var %in% names(master)))){stop("variable name mismatch")}
  if(anyDuplicated(match.dat$species) > 0){stop("duplicate species name in match data")}
  
  match.id  <- cbind(row = match(spp, master$species), 
                     col = match(var, names(master)))
  
  #match data to master
  master[match.id] <- match.dat[, 4:length(match.dat)][id]
  #Update ref column  
  names(master)[which(names(master) == "ref")] <- paste("ref.", data.id, sep = "")
  master <- cbind(master, ref = NA)
  return(master)}


ITISlookUpData <- function(){
  aves.names <- read.csv("r data/match data/Aves synonym data (ITIS).csv", stringsAsFactors=FALSE)
  aves.codes <- read.csv("r data/match data/spp code matches.csv", stringsAsFactors=FALSE)
  
  main <- aves.names$species[match(aves.codes$Main, aves.names$code)]
  synonym <- aves.names$species[match(aves.codes$Synonym, aves.names$code)]
  
  itis.match <- data.frame(main, synonym)
  itis.match <- itis.match[complete.cases(itis.match),]
  
  return(itis.match)}


matchMetrics <- function(data, master, itis.match, unmatched, unmatched1, unmatched2){
  n <- min(nrow(master), nrow(data))
  print(paste("total data set:", length(data$species)))
  print(paste("total matched:", n - length(unmatched)))            
  print(paste("total unmatched:", length(unmatched)))                        
  print(paste("total ITIS matches:", 
              length(unmatched)-length(unmatched1)))
  print(paste("manual matches:", length(unmatched2)))}


dataSppMatch <- function(data.list, data.ID = "D5", master){
  
  data <- data.list[[data.ID]]  
  
  if(any(data$species == "")){
    data <- data[-which(data$species == ""),]}
  
  
  # ITIS serial no. match.....................................................................
  unmatched <- data$species[!(data$species %in% master$species)]
  
  itis.match <- ITISlookUpData()
  
  
  #synonyms of mismatched master spp, matched to lookup1
  lookup.match <- as.character(itis.match[,2][itis.match[,1] %in% unmatched])
  synonyms <- as.character(itis.match[,1][itis.match[,1] %in% unmatched])
  
  #lookup.match with matches in master 
  dd.match <- lookup.match[lookup.match %in% master$species]
  synonyms <- synonyms[lookup.match %in% master$species]
  
  data$species[match(synonyms, data$species)] <- dd.match
  
  
  unmatched0 <- data$species[!(data$species %in% master$species)]
  
  #synonyms of mismatched master spp, matched to lookup2
  syn2 <- as.character(itis.match[,1][itis.match[,2] %in% unmatched0])
  syn2.dat <- as.character(itis.match[,2][itis.match[,2] %in% unmatched0])
  
  #syn2 with matches in master 
  syn2.mas <- syn2[syn2 %in% master$species]
  syn2.dat <- syn2.dat[syn2 %in% master$species]
  
  data$species[match(syn2.dat, data$species)] <- syn2.mas
  
  #....Match from previous matches
  
  unmatched1 <- data$species[!(data$species %in% master$species)]
  
  if(length(unmatched1) >0){
    
    print("performing matching using prior cases")  
    
    match.master <- read.csv("r data/match data/match master.csv", stringsAsFactors=FALSE)
    match.temp <- match.master[which(match.master$unmatched %in% data$species),]
    match.temp <- match.temp[which(match.temp$unmatched %in% unmatched1),]  
    
    if(any(match.temp$synonyms %in% data$species)){
      
      match.add <- match.temp[which(match.temp$synonyms %in% data$species),]
      add <- data.frame(matrix(NA, ncol = ncol(master), nrow = nrow(match.add)))
      names(add) <- names(master)
      add$species <- match.add$unmatched
      add$subspp <- 1
      add$parent.spp <- match.add$synonyms
      add$family  <- master$family[match(add$parent.spp, master$species)]
      
      master <- rbind(master, add)
      
      match.temp <- match.temp[-which(match.temp$synonyms %in% data$species),]  
    }
    
    data$species[match(match.temp$unmatched, data$species)] <- match.temp$synonym 
    
    unmatched2 <- data$species[!(data$species %in% master$species)]
    
    matchMetrics(data, master, itis.match, unmatched, unmatched1, unmatched2)
    
    
    #....Check if manual matching required
    if(length(unmatched2) == 0){
      return(list(data = data, master = master, data.ID = data.ID))
      stop(paste(data.ID,"matching complete, no manual matching required"))}
    
    if(length(unmatched2) >0){
      
      #if required and the mmatch file exists, carry on matching 
      if(file.exists(paste("r data/match data/",data.ID," mmatched.csv", sep = ""))){
        
        mmatch <- read.csv(paste("r data/match data/",data.ID," mmatched.csv", sep = ""),
                           stringsAsFactors=FALSE)
        
        mmatch.temp <- mmatch[which(mmatch$unmatched %in% data$species),]
        mmatch.temp <- mmatch.temp[which(mmatch.temp$unmatched %in% unmatched2),]      
        
        if(any(mmatch.temp$synonyms %in% data$species)){
          
          match.add <- mmatch.temp[which(mmatch.temp$synonyms %in% data$species),]
          add <- data.frame(matrix(NA, ncol = ncol(master), nrow = nrow(match.add)))
          names(add) <- names(master)
          add$species <- match.add$unmatched
          add$subspp <- 1
          add$parent.spp <- match.add$synonyms
          add$family  <- master$family[match(add$parent.spp, master$species)]
          
          master <- rbind(master, add)
          
          mmatch.temp <- mmatch.temp[-which(mmatch.temp$synonyms %in% data$species),]  
        }
        
        
        data$species[match(mmatch.temp$unmatched, data$species)] <- mmatch.temp$synonym
        unmatched2 <- data$species[!(data$species %in% master$species)]
        return(list(data = data, master = master, data.ID = data.ID))
        
        #otherwise write spp that need matching and  
      }else{write.csv(unmatched2, paste("r data/match data/",data.ID," mmatch.csv", sep = ""))
            stop(paste("manual matching required",length(unmatched2),"datapoints unmatched"))}
      
    }
  }else{
    return(list(data = data, master = master, data.ID = data.ID))
  }}

sppMatch <- function(X, spp = unmatched, lookup, match){
  
  data <- X$data
  master <- X$master
  
  #synonyms of mismatched master spp, matched to lookup1
  synonyms <- as.character(match[lookup %in% spp]) #Synonyms of master spp
  lookup.match <- as.character(lookup[lookup %in% spp]) #master spp name
  
  #lookup.match with matches in data 
  spp.master <- lookup.match[synonyms %in% data$species] #Synonyms found in data
  spp.synonyms <- synonyms[synonyms %in% data$species] #master spp name in data
  
  match.data <- data.frame(synonyms = spp.synonyms, species = spp.master, stringsAsFactors = F)
  
  if(any(match.data$species %in% data$species)){
    
    master.add <- match.data[which(match.data$species %in% data$species),]
    add <- data.frame(matrix(NA, ncol = ncol(master), nrow = nrow(master.add)))
    names(add) <- names(master)
    add$species <- master.add$synonyms
    add$subspp <- 1
    add$parent.spp <- master.add$species
    add$family  <- master$family[match(add$parent.spp, master$species)]
    
    #add to master
    master <- rbind(master, add)
    #remove any species from match.data already added to master
    match.data <- match.data[-which(match.data$species %in% master.add$species),]} 
  
  
  if(any(match.data$synonyms %in% master$species)){
    
    #find synonyms in master species
    data.add <- match.data[match.data$synonyms %in% master$species,]
    #remove any species already added to data
    data.add <- data.add[!data.add$species %in% X$data.add$species,]
    
    if(nrow(data.add)>=1){
      #duplicate data row for species to be added to data
      add.dd <- data.frame(species = data.add$species, 
                           status = "add", 
                           synonyms = data[match(data.add$synonyms, 
                                                 data$species),]$species,
                           stringsAsFactors = F)
      
      #add to data
      data <- rbind(data, add.dd)}
    #update data.add on macthObj
    X$data.add <- rbind(X$data.add, data.add)
    
    match.data <- match.data[-which(match.data$species %in% add.dd$species),]
  }   
  
  
  #update species names in data$species
  data$species[match(match.data$synonyms, data$species)] <- match.data$species
  
  m <- matchObj(master, data, data.ID=X$data.ID, X$data.add)
  
  return(m)}



birdlifeSppMatch <- function(data.list, data.ID = "D13", master){
  
  data <- data.list[[data.ID]]  
  
  if(any(data$species == "")){
    data <- data[-which(data$species == ""),]}
  
  data <- data.frame(data, synonyms = data$species, stringsAsFactors = F)
  
  #Create match object
  m <- matchObj(master, data, data.ID, add = NULL)
  
  #unmatched
  unmatched <- master$species[!(master$species %in% data$species)]
  
  # ITIS serial no. match.....................................................................
  itis.match <- ITISlookUpData()
  m <- sppMatch(m, spp = unmatched, lookup = itis.match[,1], match = itis.match[,2])
  
  unmatched0 <- m$master$species[!(m$master$species %in% m$data$species)]
  m <- sppMatch(m, spp = unmatched0, lookup = itis.match[,2], match = itis.match[,1])
  
  #....Match from previous matches
  
  unmatched1 <- m$master$species[!(m$master$species %in% m$data$species)]
  
  if(length(unmatched1) >0){
    
    print("performing matching using prior cases")  
    
    bl.master <- read.csv("r data/match data/birdlife synonyms.csv", stringsAsFactors=FALSE)
    
    m <- sppMatch(m, spp = unmatched1, lookup = bl.master$species, match = bl.master$synonyms)  
    
    
    unmatched2 <- m$master$species[!(m$master$species %in% m$data$species)]    
    
    
    
    match.master <- read.csv("r data/match data/match master.csv", stringsAsFactors=FALSE)
    
    m <- sppMatch(m, spp = unmatched2, lookup = match.master$species,
                  match = match.master$synonyms)  
    
    unmatched3 <- m$master$species[!(m$master$species %in% m$data$species)]
    
    #....Check if manual matching required
    if(length(unmatched3) == 0){
      return(m)
      stop(paste(data.ID,"matching complete, no manual matching required"))}
    
    if(length(unmatched3) >0){
      
      if(!file.exists(paste("r data/match data/",data.ID," mmatch.csv", sep = ""))){
        write.csv(unmatched3, paste("r data/match data/",data.ID," mmatch.csv", sep = ""),
                  row.names = F)
        print(paste("manual matching required",length(unmatched3),"datapoints unmatched"))
        return(m)}else{
          
          #if required and the mmatch file exists, carry on matching 
          if(file.exists(paste("r data/match data/",data.ID," mmatched.csv", sep = ""))){
            
            mmatch <- read.csv(paste("r data/match data/",data.ID," mmatched.csv", sep = ""),
                               stringsAsFactors=FALSE)    
            
            m <- sppMatch(m, spp = unmatched3, lookup = mmatch$species,
                          match = mmatch$synonyms)
            
            unmatched4 <- m$master$species[!(m$master$species %in% m$data$species)]
            print(unmatched4)
            return(m)  
          }else{return(m)} }
      
    }
  }else{
    return(m)
  }}

addVars <- function(data, master){
  
  vars <- names(data)[names(data) != "species"]
  
  for(var in vars){
    if(is.character(data[,var])){data[,var][data[,var]==""] <- NA}
    add <- data.frame(spp = match(data$species, master$species), 
                      dat = data[,var], stringsAsFactors = F)
    add <- add[complete.cases(add),]
    #if(!any(is.na(as.numeric(add$dat)))){add$dat <- as.numeric(add$dat)}
    
    var.col <- data.frame(rep(NA, dim(master)[1]))
    var.col[add[,"spp"],] <- add[,"dat"]
    
    names(var.col)<- var
    
    master <- data.frame(master, var.col, stringsAsFactors = F)}
  
  return(master)}


matchObj <- function(master, data, data.ID, add = NULL){
  m <- list(master = master, data = data, data.ID = data.ID, data.add = add)
  return(m)
}


#__________________________________________________________________________________________________________________
#CREATE MASTER
#__________________________________________________________________________________________________________________

setwd("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/")

D3 <- as.data.frame(read.csv("standardised csv data/BirdListBreedingSystem2George.csv", stringsAsFactors=FALSE))
BS.varnames <- as.vector(read.table("r data/BS varnames.csv", quote="\"")[,"V1"])


#Make master spreadsheet. Copy first three columns (index, species, family) straight across. 
#Set species names as rownames

master <- data.frame(matrix(NA, ncol= length(BS.varnames), nrow = dim(D3)[1]))
master[,1:3] <-D3[,1:3]
names(master) <-BS.varnames
rownames(master) <- as.character(master$species)


#__________________________________________________________________________________________________________________
#MATCH MS
#__________________________________________________________________________________________________________________

#Open files

D4 <- read.csv("standardised csv data/Elliot New Data 7th November 2014.csv", stringsAsFactors=FALSE)
D1 <- read.csv("standardised csv data/M.System _ D.Mode Data, Species 4999 - 9993_extra.csv", stringsAsFactors=FALSE)
D2 <- read.csv("standardised csv data/M.System _ D.Mode Data, Species 4999 - 9993.csv", stringsAsFactors=FALSE)
D5 <- read.csv("standardised csv data/clutch size.csv", stringsAsFactors=FALSE)
D6 <- read.csv("standardised csv data/bird_ssd7.csv", stringsAsFactors=FALSE)
D7 <- read.csv("standardised csv data/ASR_mortality_to_Anna_Gavin.csv", stringsAsFactors=FALSE)
D8 <- read.csv("standardised csv data/life_history_to_Anna_Gavin.csv", stringsAsFactors=FALSE)
D9 <- read.csv("standardised csv data/breeding_system_to_Anna_Gavin.csv", stringsAsFactors=FALSE)
D12 <- read.csv("standardised csv data/Cockburn 2006_Appendix A resub.csv", stringsAsFactors=FALSE)
names(D12)[which(names(D12) != "species")] <- paste(names(D12)[which(names(D12) != "species")], "_D12", sep = "")
D13 <- read.csv("standardised csv data/birdlife spp list.csv", stringsAsFactors=FALSE)
D13 <- D13[!(duplicated(D13$species) & D13$status != "R"),]


D14 <- read.table("standardised csv data/HBW spp list.csv", 
                  header=TRUE, quote="\"", stringsAsFactors=FALSE)
D14$status <- "R"


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


#Add subspp columns
master$subspp <- 0
master$parent.spp <- NA



#__________________________________________________________________________________________________________________
#MATCH Clutch Size (D5)
#__________________________________________________________________________________________________________________


# Add D5.....................................................................
output <- dataSppMatch(data.list, data.ID = "D5", master)
data <- output$data
master <- output$master
master <- addVars(data, master)


# Add D6.....................................................................
output <- dataSppMatch(data.list, data.ID = "D6", master)
data <- output$data
master <- output$master
master <- addVars(data, master)

# Add D7.....................................................................
output <- dataSppMatch(data.list, data.ID = "D7", master)
data <- output$data
master <- output$master
master <- addVars(data, master)


# Add D8.....................................................................
output <- dataSppMatch(data.list, data.ID = "D8", master)
data <- output$data
master <- output$master
master <- addVars(data, master)


# Add D9.....................................................................
output <- dataSppMatch(data.list, data.ID = "D9", master)
data <- output$data
master <- output$master
master <- addVars(data, master)



# Add D12.....................................................................
source("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/scripts/Cockburn data process.R", local = T)
output <- dataSppMatch(data.list, data.ID = "D12", master)
data <- output$data
master <- output$master
master <- addVars(data, master)

# Add D13.....................................................................

output <- birdlifeSppMatch(data.list, data.ID = "D13", master)
data <- output$data
master <- output$master
master <- addVars(data, master)


# Add D14.....................................................................

output <- birdlifeSppMatch(data.list, data.ID = "D14", master)
data <- output$data
master <- output$master
master <- addVars(data, master)


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
testSynonym("Alario alario", data, "D14")


any("Sicalis_uropygialis" %in% data.list[[data.ID]]$species)


data[8005,]


master$mpg_D9 <- master$mpg_D9/100  #change to prop for consistency
master$fpg_D9 <- master$fpg_D9/100  #change to prop for consistency
master$inc[which(master$inc == 15.5)] <- NA #obviously incorrect
master$postf.feed[which(master$postf.feed %in% c(31.5, 34  ,35 ,93))] <- NA #obviously incorrect 


write.csv(master, "~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/Master Data Sheet.csv")

#________________________________________________________________________________________________
#.....MANUAL MATCH CODE
________________________________________________________________________________________________
any("Nectarinia_neergaardi" %in% master$species)


#________________________________________________________________________________________________
#.....QC Variables
________________________________________________________________________________________________

