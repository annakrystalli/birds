# Compile mating system data (D1-4 & D7-9)



#Label variable with dataset name
labelVars <- function(dat, data.ID, label = F){
  require(stringr)
  if(label){
  names(dat)[names(dat) != "species"] <- paste(names(dat), data.ID, sep = "_")[names(dat) != "species"]}
  dat$species <- gsub(" ", "_", dat$species)
  #dat <- rbind(dat, NA)
  #dat[dim(dat)[1], "species"] <- "NoData"
  return(dat)
}

#Match datasets D1-4. Returns updated master. Allows halting of function if duplicate 
#datapoints across datasets exist through the overwrite argument

matchMSToMaster <-  function(data.list, data.id, master, overwrite = F, add.var = NULL, 
                             taxo.var = taxo.var, var.omit = var.omit, input.folder){
  
  dat <- data.list[[data.id]]
  
  
  if(!is.null(add.var)){
    add.cols <- matrix(NA, ncol = length(add.var), nrow = dim(master)[1])
    names(add.cols) <- add.var
    master <- cbind(master, add.cols)
  }
  
  #Make sure there are no empty cells and replace any with NA cells
  dat[which(dat == "", arr.ind = T)] <- NA
  
  #make vector of match data variables and check that they match the master
  match.vars <- names(dat)[!names(dat) %in% c(taxo.var, var.omit)]
  match.dat <- dat[, match.vars]
  if(any(is.na(match.vars %in% names(master)))){stop("variable name mismatch")}
  
  #for(i in 1:length(match.vars)){
  #if(sum(!is.na(as.numeric(match.dat[,match.vars[i]])))==0){}else{
  # match.dat[,match.vars[i]] <- as.numeric(match.dat[,match.vars[i]])  
  #}}
  
  
  #find non NA values in match data. Match arr.indices to spp and variable names (for QA)
  id <- which(!is.na(match.dat), arr.ind = T)
  
  spp <- as.character(dat[,"species"][id[, "row"]])
  var <- match.vars[id[, "col"]]
  
  # Check that species names and variable names to be matched are consistent with master and
  # there are no species duplicates
  if(any(!(spp %in% master$species))){stop("species name mismatch")}
  if(any(!(var %in% names(master)))){stop("variable name mismatch")}
  if(anyDuplicated(match.dat$species) > 0){stop("duplicate species name in match data")}
  
  #Create match index for datapoints to be added
  match.id  <- cbind(row = match(spp, master$species), 
                     col = match(var, names(master)))
  
  #check to see if any data will be overwritten (ie duplicate datapoints across datasets)
  check <- sum(!is.na(master[match.id]))
  
  #stop function if overwrite = F
  
    if(check > 0){if(!overwrite){stop("Function terminated. Data overwrite danger")}else{
      errors <- cbind(as.character(master$species)[match.id[,1]][which(!is.na(master[match.id]), arr.ind = T)],
                                  names(master)[match.id[,2]][which(!is.na(master[match.id]), arr.ind = T)])
          
                  write.csv(errors, file=paste(input.folder, "r data/Data overwrite error reports/", data.id,".csv", sep = ""), 
                            row.names = F)
                  
                  #Notify of any data overwriting
                  print(paste(check, "datapoints overwritten"))
                  
                  
    }

  
  #match data to master
  master[match.id] <- match.dat[id]

  return(master)}

# Processes ITIS synonyms data into a species synonym dataset 
ITISlookUpData <- function(version=NULL){
  aves.names <- read.csv("r data/match data/Aves synonym data (ITIS).csv", stringsAsFactors=FALSE)
  aves.codes <- read.csv("r data/match data/spp code matches.csv", stringsAsFactors=FALSE)
  
  species <- aves.names$species[match(aves.codes$Main, aves.names$code)]
  synonyms <- aves.names$species[match(aves.codes$Synonym, aves.names$code)]
  
  itis.match <- data.frame(species, synonyms, stringsAsFactors = F)
  itis.match <- itis.match[complete.cases(itis.match),]
  
  if(version == 2){names(itis.match) <- c("synonyms", "species")}
  
  return(itis.match)}

# Outputs details of master species name match lookup
matchMetrics <- function(data, master, match.lengths){
  n <- min(nrow(master), nrow(data))
  print(paste("total data set:", length(data$species)))
  print(paste("total matched:", n - match.lengths["unmatched"]))            
  print(paste("total unmatched:", match.lengths["unmatched"]))                       
  print(paste("total ITIS matches:", 
              match.lengths["unmatched"] - match.lengths["unmatched1"]))
  print(paste("total Birdlife:", 
              match.lengths["unmatched1"] - match.lengths["unmatched2"]))
  print(paste("total master.match:", 
              match.lengths["unmatched2"] - match.lengths["unmatched3"]))
  print(paste("manual matches:", match.lengths["unmatched3"]))}




#Look up unmantched vector of species. If vector contains unmatched data species, 
#Prepares data for matching of species to master species name. Creates synonyms column to link back to original data and 
# and data status, used to indicate data has bee prepared but also whether state of data row is original or has been added.

dataMatchPrep <- function(data){
  if("data.status" %in% names(data)){
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
    add$order  <- master$order[match(add$parent.spp, master$species)]
    
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
  
  # Load match data.....................................................................
  itis1 <- ITISlookUpData(1)
  itis2 <- ITISlookUpData(2)
  birdlife  <- read.csv("r data/match data/birdlife synonyms.csv", stringsAsFactors=FALSE)
  match.master <- read.csv("r data/match data/match master.csv", stringsAsFactors=FALSE)
  
  
  if(sub == "master"){
    set <- "data"
  }
  if(sub == "data"){
    set <- "master"
  }   
    
  #unmatched
  unmatched <- get(sub)$species[!(get(sub)$species %in% get(set)$species)]
  
  if(sub == "data"){
    rm <- match.master$synonyms[match.master$synonyms %in% unmatched & match.master$species %in% c("Extinct", "New")]
    data <- data[!(data$species %in% rm),]
  }

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


addVars <- function(data, master){
  
  vars <- names(data)[!(names(data) %in% c("species", "synonyms", "data.status"))]
  
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
