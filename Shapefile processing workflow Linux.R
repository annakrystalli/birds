rm(list=ls())


#____________________________________________________________________________
#.....Functions

#____________________________________________________________________________


source("~/Documents/Anna/Range Match Data/Shapefile processing functions.R")

#____________________________________________________________________________
#.....SETTINGS

#____________________________________________________________________________
options(stringsAsFactors = F)
eres <- 10
input.folder <- "~/Documents/Anna/Range Match Data/"
bird.folder <- "BirdLife Data/Shapefiles/"
env.folder <-paste("BioCLIM Data/bil",eres,"/", sep="")
output.folder <- "Output/"
  
wd.bird <-paste(input.folder, bird.folder, sep = "")
wd.env <-paste(input.folder, env.folder, sep = "")
wd.output <- paste(input.folder, output.folder, sep = "")
dir.create(path = paste(wd.output, "range dat/", sep = ""), showWarnings = F)

  
bird.files <- list.files(wd.bird, pattern = ".shp")
env.files <- list.files(wd.env, pattern = ".bil")

bios <- c("alt",paste("bio", 1:19, sep=""))
#____________________________________________________________________________
#.....WORKFLOW

#____________________________________________________________________________


#...SET UP CLUSTER..............................................................................

cores <- detectCores()  

cl <- makeCluster(cores - 1)
registerDoParallel(cl)

#clusterExport(cl, varlist =c("wd.bird", "wd.env", "wd.output", "bios", "getSppRow", "fixholes",
                             #"getBioRow", "bioDataTable", "areaDataTable", "latWts"))




min <- 34
max <- 36

     
#...1001:end............
          
min <- 1
max <- length(bird.files)

bird.dat.parallel <- foreach(x = bird.files[min:max], .combine = rbind,
                             .inorder = F, .errorhandling = "remove") %dopar%{
                               require("rgeos")
                               require("rgdal")
                               require("sp")
                               require("maptools")
                               require("spatstat")
                               getSppRow(x, wd.bird, wd.env, wd.output, bios, 
                                         input.folder, overwrite = T)}

save(bird.dat.parallel, file = paste(wd.output, "bird.dat.Rdata",sep = ""))
