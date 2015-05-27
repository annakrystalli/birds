
#Look up unmantched vector of species. If vector contains unmatched data species, 
#Prepares data for matching of species to master species name. Creates synonyms column to link back to original data and 
# and data status, used to indicate data has bee prepared but also whether state of data row is original or has been added.

dataMatchPrep <- function(data){
  if("data.status" %in%names(data)){
    print("Data already prep-ed")
    return(data)
  }else{
    dt <- data.frame(data[,names(data) != "species"], stringsAsFactors = FALSE)
    names(dt) <- names(data)[names(data) != "species"]
    data <- data.frame(species = data$species, synonyms = data$species, data.status = "original", dt,
                       stringsAsFactors = FALSE)
    return(data)}
}


# Create match object
matchObj <- function(master, data, data.ID, add = NULL, status = "unmatched"){
  m <- list(master = master, data = data, data.ID = data.ID, data.add = add, status = status)
  return(m)
}




#match to master species to lookup dataset of known match pairs. If list is of unmatched data species,
# match to known synonyms.
sppMatch <- function(X, unmatched = unmatched, lookup.dat, sub = "master", status = status){
  
  data <- X$data
  master <- X$master
  
  if(sub == "master"){
    lookup <- lookup.dat$species
    lookupin <- lookup.dat$synonyms
    set <- "data"
  }
  if(sub == "data"){
    lookup <- lookup.dat$synonyms
    lookupin <- lookup.dat$species
    set <- "master"
  }
  
  
  #Find synonyms of unmatched species 
  lookupin.match <- as.character(lookupin[lookup %in% unmatched]) #find syns of unmatched species with matches in lookup
  lookup.match <- as.character(lookup[lookup %in% unmatched]) # find unmatched spp with matches in lookup
  
  # Compile match pairs for species whose synonyms match species names in lookup.data
  spp <- lookup.match[lookupin.match %in% X[[set]]$species] #find unmacthed species with synonym matches in set
  syns <- lookupin.match[lookupin.match %in% X[[set]]$species] #trim synonym matches
  
  #Compile match pairs into dataframe 
  if(sub == "data"){match.data <- data.frame(synonyms = spp, species = syns, stringsAsFactors = F)}
  if(sub == "master"){match.data <- data.frame(synonyms = syns, species = spp, stringsAsFactors = F)}
                                      
  #Check that data spp name change won't cause a sinle master species name to be associated with two
  # different data points. This would cause the original data point (which is a straught match to the dataset)
  # to be overwritten with most likely data for a subspecies. Identify such cases and add them to the master.
  # The data point can then form a straight match and is identified by the column subspp.
  
  if(any(match.data$species %in% data$species)){
    
    master.add <- match.data[which(match.data$species %in% data$species),]
    add <- data.frame(matrix(NA, ncol = ncol(master), nrow = nrow(master.add)))
    names(add) <- names(master)
    add$species <- master.add$synonyms
    add$subspp <- TRUE
    add$parent.spp <- master.add$species
    add$family  <- master$family[match(add$parent.spp, master$species)]
    
    #add to master
    master <- rbind(master, add)
    #remove any species from match.data already added to master
    match.data <- match.data[-which(match.data$species %in% master.add$species),]
  } 
  
  # Also check that any of the data species names about to be replaced do not already map to another species in the master.
  # Identify and duplicate any such datarows so that the orginal link to the master will remain. Species can be linked back
  # to data via the synonoys column in data. Additionally check against previously added species to avoid duplicate additions.
  if(any(match.data$synonyms %in% master$species)){
    
    #find synonyms in master species
    data.add <- match.data[match.data$synonyms %in% master$species,]
    #remove any species already added to data
    data.add <- data.add[!data.add$species %in% X$data.add$species,]
    
    if(nrow(data.add)>=1){
      #duplicate data row for species to be added to data
      add.dd <- data.frame(species = data.add$species, data[match(data.add$synonyms, 
                                                                  data$species),names(data) != "species"],
                           stringsAsFactors = F)
      add.dd$data.status <- "duplicate"
      
      #add to data
      data <- rbind(data, add.dd)}
    
    #update data.add on macthObj
    X$data.add <- rbind(X$data.add, data.add)
    
    match.data <- match.data[-which(match.data$species %in% add.dd$species),]
  }   
  
  
  #update species names in data$species
  data$species[match(match.data$synonyms, data$species)] <- match.data$species
  
  m <- matchObj(master, data, data.ID=X$data.ID, X$data.add, status = status)
  
  return(m)}


# match data set to master species list
dataSppMatch <- function(data.list, data.ID = "D13", master, sub = "data", match.params = match.params){
  
  data <- data.list[[data.ID]]  
  
  if(any(data$species == "")){
    data <- data[-which(data$species == ""),]}
  
  data <- dataMatchPrep(data)
  
  #Create match object
  m <- matchObj(master, data, data.ID, add = NULL, status = "unmatched")
  
  if(sub == "master"){
    set <- "data"
  }
  if(sub == "data"){
    set <- "master"
  }   
    
  #unmatched
  unmatched <- get(sub)$species[!(get(sub)$species %in% get(set)$species)]
  
  # Load match data.....................................................................
  itis1 <- ITISlookUpData(1)
  itis2 <- ITISlookUpData(2)
  birdlife  <- read.csv("r data/match data/birdlife synonyms.csv", stringsAsFactors=FALSE)
  match.master <- read.csv("r data/match data/match master.csv", stringsAsFactors=FALSE)
  
  
  for(id in match.params$name){

    match <- match.params$match[match.params$name == id]
    status <- match.params$status[match.params$name == id]
    
  m <- sppMatch(m, unmatched = get(match), lookup.dat = get(id), sub = sub,
                                   status = status)
  
  #generate next unmatched species list
  assign(status, m[[sub]]$species[!(m[[sub]]$species %in% m[[set]]$species)])
  
  # if no more species unmatched break loop
  if(length(get(status)) == 0){
    print(paste(data.ID, "match complete on", id))
    break}
  
  # if all match pair datasets checked and species remain unmatched write manual match spp list
  if(status == "unmatched3"){

    write.csv(data.frame(synonyms = if(sub == "master"){""}else{unmatched3},
                         species = if(sub == "master"){unmatched3}else{""}),
              paste("r data/match data/",data.ID," mmatch.csv", sep = ""),
              row.names = F)
    print(paste("manual matching required,",length(unmatched3),"datapoints unmatched"))
    break}
  
    
  }
  
  match.lengths <- sapply(match.params$match, FUN = function(x){if(exists(x)){length(get(x))}else{0}})
  
  matchMetrics(data, master, match.lengths)
  
  save(m, file = paste(output.folder, "data/match objects/", data.ID, " match object.RData", sep = ""))
       
  return(m)}



