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

for(bird.file in bird.files[1:2]){
  
  if(bird.file == bird.files[1]){
    bird.dat <- data.frame(getSppRow(bird.file, wd.bird, wd.env, wd.output, bios, 
                                     input.folder = input.folder, overwrite = T))}else{
                                       
      bird.dat.row <- data.frame(getSppRow(bird.file, wd.bird, wd.env, wd.output, 
                                             bios, input.folder = input.folder,
                                           overwrite = T))
      if(is.null(bird.dat.row)){next}else{
      bird.dat <- data.frame(rbind(bird.dat,bird.dat.row))}
    }
  
  print(paste("SPECIES no.", which(bird.files == bird.file), "out of", length(bird.files)))
  if(which(bird.files == bird.file) %% 10 == 0){
    print(".............................# SAVING BIRD.DAT")
    save(bird.dat, file = paste(wd.output, "bird.dat.Rdata",sep = ""))    
  }
  
}  

#____________________________________________________________________________

#......Match data to synoym list 
#____________________________________________________________________________


setwd("/Users/Anna/Google Drive/Sex Roles in Birds Data Project/")
data.ID <- "D14"

#...Load initial bird.dat
load(file = paste(wd.output, "bird.dat.Rdata",sep = ""))  
bird.dat <- bird.dat.parallel
data <- read.csv("Inputs/Anna workflow/data in/r data/match data/matched BioClim.csv", 
                 stringsAsFactors = F)

bird.dat$spp <- as.character(bird.dat$spp)
bird.dat$spp <- gsub("_[0-9].*$", "", bird.dat$spp)


data.full <- data.frame(species = data$species , bird.dat[match(data$synonyms, bird.dat$spp),-1],
                   stringsAsFactors = F)

write.csv(data.full,"Inputs/Anna workflow/data in/standardised csv data/BioClim.csv", row.names = F)







#____________________________________________________________________________

#...Compile individual polygon region data
#____________________________________________________________________________

require("rgeos")
require("rgdal")
require("sp")
require("maptools")
require("spatstat")

bird.files <- list.files(paste(wd.output,"Matched Shapefiles/", sep=""))

#... Create df

  load(file=paste(input.folder, "bird.dat.colnames.Rdata", sep=""))

    bio.nm <- bird.dat.colnames[-(1:6)]
    season.nm <- paste("S", 1:4, sep = "")
    grid <- expand.grid(bio.nm, season.nm, 
                        stringsAsFactors = F)
    bio.vars <- paste(grid[,2], grid[,1], sep = "")
    header <- c(bird.dat.colnames[1:6], bio.vars)
    df <- data.frame(matrix(NA, ncol = length(header), nrow = 1))
    names(df) <- header
    df.blank <- df
    df <- df[-1,]


for(bird.file in bird.files){
  
  
   df <- rbind(df,  compileSeasonRangeRow(bird.file, df.blank, 
                                          wd.output, wd.bird, 
                                          bio.nm, bios))}
    

  

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


unp <- which(!gsub("_[0-9].*$", "",bird.files) %in% gsub(".Rdata", "",bird.files.m))
bird.file <- bird.files[unp[1]]

x<-fixholes(x)[-1,]

x@data$id = rownames(x@data)
x.points = fortify(x, region="id")
x.df = join(x.points, x@data, by="id")


ggplot(x.df) + 
  aes(long,lat,group=group,fill="id") + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_brewer(type = "seq", palette = 1)
