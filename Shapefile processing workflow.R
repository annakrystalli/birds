rm(list=ls())


#____________________________________________________________________________
#.....Functions

#____________________________________________________________________________


source("/Users/Anna/Documents/workflows/Sex Roles in Birds/birds/Shapefile processing functions.R")

#____________________________________________________________________________
#.....SETTINGS

#____________________________________________________________________________

eres <- 10
input.folder <- "/Users/Anna/Documents/Range Match Data/"
bird.folder <- "BirdLife Data/Shapefiles/"
env.folder <-paste("BioCLIM Data/bil",eres,"/", sep="")
output.folder <- "Output/"

wd.bird <-paste(input.folder, bird.folder, sep = "")
wd.env <-paste(input.folder, env.folder, sep = "")
wd.output <- paste(input.folder, output.folder, sep = "")


bird.files <- list.files(wd.bird, pattern = ".shp")
env.files <- list.files(wd.env, pattern = ".bil")

bios <- c("alt",paste("bio", 1:19, sep=""))
#____________________________________________________________________________
#.....WORKFLOW

#____________________________________________________________________________

#...Process files

for(bird.file in bird.files[501:1000]){
  
  if(bird.file == bird.files[1]){
    bird.dat <- data.frame(getSppRow(bird.file, wd.bird, wd.env, wd.output, bios, 
                                     input.folder = input.folder))}else{
      bird.dat <- data.frame(rbind(bird.dat,
                                   getSppRow(bird.file, wd.bird, wd.env, wd.output, 
                                             bios, input.folder = input.folder)))
    }
  
  print(paste("SPECIES no.", which(bird.files == bird.file), "out of", length(bird.files)))
  if(which(bird.files == bird.file) %% 10 == 0){
    print(".............................# SAVING BIRD.DAT")
    save(bird.dat, file = paste(wd.output, "bird.dat.Rdata",sep = ""))    
  }
  
}  

#____________________________________________________________________________

#...Compile processed files
    
    #...Load initial processing bird.dat
    load(file = paste(wd.output, "bird.dat.Rdata",sep = ""))    
    
    #...Load linux processing bird.dat
    load(file = paste(wd.output, "Linux output/bird.dat1001-end.Rdata",sep = "")) 

    bird.dat <- rbind(bird.dat, bird.dat.parallel)
    bird.dat <- bird.dat[ !apply(bird.dat, 1, function(x) all(is.na(x))), ]


    #...Create vector of spps names to look up in bird.dat
    spps <- NULL
    for(i in 1:length(bird.files)){
      spps <- c(spps, sub("_2.*?$", "\\1", bird.files[i]))}

    comp.files <- bird.files[which(!(spps %in% bird.dat$spp))]


  for(bird.file in comp.files){
    
        bird.dat <- data.frame(rbind(bird.dat,
                                     getSppRow(bird.file, wd.bird, wd.env, wd.output, bios, input.folder = input.folder)))
      
    
    print(paste("SPECIES no.", which(bird.files == bird.file), "out of", length(bird.files)))
    if(which(bird.files == bird.file) %% 10 == 0){
      print(".............................# SAVING BIRD.DAT")
      #save(bird.dat, file = paste(wd.output, "bird.dat.Rdata",sep = ""))    
    }
    
  }  




bird.dat.m <- bird.dat

load(file = paste(wd.output, "Linux output/bird.dat1001-end.Rdata",sep = "")) 
bird.dat <- rbind(bird.dat, bird.dat.parallel)
bird.dat <- complete.cases(bird.dat)
bird.dat[duplicated(bird.dat),]
