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
