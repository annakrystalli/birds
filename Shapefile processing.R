rm(list=ls())

require("rgeos")
require("rgdal")
require("sp")
require("maptools")
require("spatstat")
require("data.table")
require(ggplot2)
library(scales)
library(compiler)
require(parallel)
library(plyr)
library(foreach)
require(doParallel)




#____________________________________________________________________________
#.....FUNCTIONS

#____________________________________________________________________________


fixholesF = function(sp.obj) {
  require(rgeos)
  require(stringr)
  if(!inherits(sp.obj, "SpatialPolygons")) stop("Input object must be of class SpatialPolygons")
  pls = slot(sp.obj, "polygons")
  pls1 = lapply(pls, checkPolygonsHoles)
  slot(sp.obj, "polygons") = pls1
  return(sp.obj)
}
fixholes <- cmpfun(fixholesF)

latWts <- function(lats, e){cell <- e@grid@cellsize[1]
                            
                            l <- cos((lats-cell/2)*pi/180)^2 *cos(cell*pi/180)
                            l0 <- cos(mean(lats-cell/2)*pi/180)^2 *cos(cell*pi/180)
                            l/l0}


areaDataTable<- function(xt){  
  data <- NULL
  a <- gArea(xt)/10^6
  data <- a
  
  for(seasonal in 1:4){
    
    xs <-try(xt[xt$SEASONAL == seasonal,], silent = T)
    if(class(xs) == "try-error"){ap <- NA}else{
      as <- gArea(xs)/10^6
      ap <- as/a}
    data <- cbind(data, ap)}
  data <- as.data.frame(data)
  colnames(data) <- c('area', paste("area.s", 1:4, sep = ""))
  return(data)}


bioDataTableF <- function(x, xt, bio, e, wd.env, calc.dat){
  
  
  # means calculated using lat corrected values
  e.dat <- lapply(calc.dat, FUN = function(x,e){x[,1] <- e@data[as.numeric(rownames(x)),1]}, e)
  coo.polys <- lapply(calc.dat, FUN = function(x,e){coordinates(e)[as.numeric(rownames(x)),2]}, e)
  coo.wts <- lapply(coo.polys, FUN =latWts, e)
  
  mean.regions <- data.frame(mapply(e.dat,coo.wts, FUN = function(x, coo.wts){weighted.mean(x, wt =coo.wts)}))
  names(mean.regions) <- paste(bio, "m", sep=".")
  
  # max, min & var calculated on absolute values
  max.regions <- data.frame(unlist(lapply(e.dat, FUN = function(x){max(x)})))
  names(max.regions) <- paste(bio, "max", sep=".")
  
  min.regions <- data.frame(unlist(lapply(e.dat, FUN = function(x){min(x)})))
  names(min.regions) <- paste(bio, "min", sep=".")
  
  var.regions <- data.frame(mapply(e.dat, coo.wts, FUN = function(x, coo.wts){weighted.var(x, coo.wts)}))
  names(var.regions) <- paste(bio, "var", sep=".")
  
  wts <- latWts(unlist(calc.dat), e)
  var.all <- data.frame(rep(weighted.var(unlist(calc.dat), wts),  dim(var.regions)[1]))
  names(var.all) <- paste(bio, "varall", sep=".")
  
  if(bio == "alt"){ area <- gArea(fixholes(xt), byid  = T)/10^6
                    bio.dat <- data.frame(area, mean.regions, max.regions, min.regions, var.regions, var.all)}else{
                      bio.dat <- data.frame(mean.regions, max.regions, min.regions, var.regions, var.all)}  
}
bioDataTable <- cmpfun(bioDataTableF)

getBioRowF <- function(x, bios){
  
  for(bio in bios){
    
    mean <- weighted.mean(x@data[,paste(bio, "m", sep=".")], wt = x@data[,"area"])
    max <- max(x@data[,paste(bio, "max", sep=".")])
    min <- min(x@data[,paste(bio, "min", sep=".")])
    var <- unique(x@data[,paste(bio, "varall", sep=".")])
    
    if(bio == "alt"){bio.dat<- data.frame(mean, max, min, var)
                     names(bio.dat) <- paste(bio, c(".m", ".max", ".min", ".var"), sep = "")}else{
                       dat<- data.frame(mean, max, min, var)
                       names(dat) <- paste(bio, c(".m", ".max", ".min", ".var"), sep = "")
                       bio.dat <- data.frame(bio.dat, dat)
                     }
  }
  return(bio.dat)}
getBioRow <- cmpfun(getBioRowF)

getSppRowF <- function(bird.file, wd.bird, wd.env, wd.output, bios, 
                       input.folder = input.folder){
  
  
  spp <- sub("_2.*?$", "\\1", bird.file)     
  dsn <- paste(wd.bird, bird.file, sep="")
  load(file=paste(input.folder, "bird.dat.colnames.Rdata", sep=""))
  
  # Load Shapefile
  #x <- readOGR(dsn, ogrListLayers(dsn))
  x <- readShapeSpatial(dsn, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  if(all(!x$PRESENCE %in% 1:2)){print(paste(spp, "....................NA..............no presence"))
                                end.dat <- data.frame(matrix(c(spp, rep(NA, 85)), nrow = 1), stringsAsFactors = F)
                                names(end.dat) <- bird.dat.colnames
                                return(end.dat)
  }
  
  range.ls <- list.files(paste(wd.output, "range dat/", sep = ""))
  if(paste(spp, ".Rdata", sep = "") %in% range.ls){
    load(paste(wd.output, "range dat/", spp, ".Rdata", sep = ""))}else{
      
      
      xt <- spTransform(x, CRS = CRS("+proj=laea +lon_0=0.001 +lat_0=0.001 +ellps=sphere"))
      
      # Calculate range attribute data
      range.dat <- areaDataTable(fixholes(xt))
      save(range.dat, file = paste(wd.output, "range dat/", spp, ".Rdata", sep = ""))}
  
  
  done.ls <- list.files(paste(wd.output, "Matched Shapefiles/", sep = ""))
  if(!paste(spp, ".Rdata", sep="") %in% done.ls){
    
    x<- x[x$PRESENCE %in% 1:2,]
    
    
    # Extract BIOClim data and update spdf
    
    t0 <- Sys.time()
    for(env.file in paste(bios, ".bil", sep="")){
      
      # import and set bio
      bio <- gsub(".bil", "", env.file)
      e <- readGDAL(paste(wd.env, env.file, sep = ""))
      if(bio %in% paste("bio", c(1:2, 4:11), sep = "")){e@data <- e@data/10}
      
      
      if(env.file == "alt.bil"){
        # Make polys
        polys <- SpatialPolygons(x@polygons, proj4string = CRS(summary(e)$proj4string))
        #OVERLAY - 
        calc.dat <- over(polys,e , returnList = T)
        if(length(unlist(calc.dat)) == 0 ){print(paste(spp, ".....................NA..............no polys"))
                                           end.dat <- data.frame(matrix(c(spp, rep(NA, 85)), nrow = 1), stringsAsFactors = F)
                                           names(end.dat) <- bird.dat.colnames
                                           return(end.dat)
        }
        
        
        check <- which(unlist(lapply(calc.dat, FUN = function(x){dim(x[1])[1] == 0})))
        while(length(check)>=1){
          x <- x[-check,]
          polys <- SpatialPolygons(x@polygons, proj4string = CRS(summary(e)$proj4string))
          #OVERLAY - 
          calc.dat <- over(polys,e , returnList = T)
          check <- which(unlist(lapply(calc.dat, FUN = function(x){dim(x[1])[1] == 0})))
        }}
      
      
      
      
      x@data <- data.frame(x@data, bioDataTable(x, xt, bio, e, wd.env, calc.dat))}
    
    
    # save spdf    
    save(x, file = paste(wd.output, "Matched Shapefiles/", spp,".Rdata", sep=""))
    
    t1 <- Sys.time()
    print(t1-t0)
  }else{load(file = paste(wd.output, "Matched Shapefiles/", spp,".Rdata", sep=""))}
  
  
  # calculate range bio data
  bio.dat <- getBioRow(x, bios)
  
  
  
  
  print(paste("COMPLETE...........................",spp))
  
  row.dat <- data.frame(spp, range.dat, bio.dat)
  return(row.dat)}
getSppRow <- cmpfun(getSppRowF)
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


for(bird.file in bird.files[501:1000]){
 
  if(bird.file == bird.files[1]){
    bird.dat <- data.frame(getSppRow(bird.file, wd.bird, wd.env, wd.output, bios))}else{
    bird.dat <- data.frame(rbind(bird.dat,
                                 getSppRow(bird.file, wd.bird, wd.env, wd.output, bios)))
 }

 print(paste("SPECIES no.", which(bird.files == bird.file), "out of", length(bird.files)))
 if(which(bird.files == bird.file) %% 10 == 0){
   print(".............................# SAVING BIRD.DAT")
   save(bird.dat, file = paste(wd.output, "bird.dat.Rdata",sep = ""))    
 }
 
}  
    
    
load(file = paste(wd.output, "bird.dat.Rdata",sep = ""))    
    

#...PARALLELISE............


clusterExport(cl, varlist =c("wd.bird", "wd.env", "wd.output", "bios", "getSppRow", "fixholes",
                             "getBioRow", "bioDataTable", "areaDataTable", "latWts"))

cores <- detectCores()  

#....Started parallel at 321 and atched to 170.
min <- 500
max <- 1000

cl <- makeCluster(cores - 1)
registerDoParallel(cl)

bird.files[min:max]

bird.dat.parallel <- foreach(x = spp.test[1:5],  .combine = rbind) %dopar%{
  require("rgeos")
  require("rgdal")
  require("sp")
  require("maptools")
  require("spatstat")
  getSppRow(x, wd.bird, wd.env, wd.output, bios, input.folder)}
 
data.frame(getSppRow(x, wd.bird, wd.env, wd.output, bios))}


spp.test <- c("Coeligena_violifer_22726750.shp", "Diglossa_carbonaria_22723681.shp",
              "Laniarius_amboimensis_22707540.shp", "Acrocephalus_rehsei_22714791.shp",
              "Acrocephalus_rimatarae_22714826.shp",
              "Metopothrix_aurantiaca_22702679.shp")




rbind(getSppRow(spp.test[1], wd.bird, wd.env, wd.output, bios),
      getSppRow(spp.test[4], wd.bird, wd.env, wd.output, bios))



rbind(getSppRow(spp.test[1], wd.bird, wd.env, wd.output, bios, input.folder),
      getSppRow(spp.test[4], wd.bird, wd.env, wd.output, bios, input.folder))



ab <- bird.files[min:max]
done <- unlist(read.table("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/Completed.csv", quote="\"", stringsAsFactors=FALSE),
               use.names = F)
#early <- list.files(paste(wd.output, "/Matched Shapefiles", sep=""))
#write.csv(early, file="~/Desktop/early.csv")
early <- unlist(read.table("~/Desktop/early.csv", quote="\"", stringsAsFactors=FALSE),
                use.names = F)

spp.ls<- unlist(read.table("~/Desktop/all.csv", quote="\"", stringsAsFactors=FALSE),
       use.names = F)


#write.csv(bird.files, file="~/Desktop/all.csv")


un <- setdiff(spp.ls , done)
un <- setdiff(un , early)

for(i in 1:100){
bird.file = bird.files[grep(un[i], bird.files)]  
test <- getSppRow(bird.file, wd.bird, wd.env, wd.output, bios, input.folder)}

length(un)

getSppRow(bird.file, wd.bird, wd.env, wd.output, bios, input.folder)

default.stringsAsFactors()
options(stringsAsFactors = F)



default.stringsAsFactors()
options(stringsAsFactors = F)
#____________________________________________________________________________
#.....PLOT

#____________________________________________________________________________
dsn <- paste(wd.bird, bird.file, sep="")
# Load Shapefile
#x <- readOGR(dsn, ogrListLayers(dsn))
x <- readShapeSpatial(dsn, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

world <- map_data("world")
pp <- ggplot(data = world, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey50") + 
  geom_polygon(data = x[x$SEASONAL == 2,], aes(x = long, y = lat, group = group), fill = alpha("lightsalmon1", 0.5)) +
  geom_polygon(data = x[x$SEASONAL == 3,], aes(x = long, y = lat, group = group), fill = alpha("darkseagreen1", 0.5)) +
  geom_polygon(data = x[x$SEASONAL == 1,], aes(x = long, y = lat, group = group), fill = alpha("cyan", 0.5)) +
  geom_polygon(data = x[x$SEASONAL == 4,], aes(x = long, y = lat, group = group), fill = alpha("blueviolet", 0.5)) +

  coord_equal()

print(pp)






world <- map_data("world")
pp <- ggplot(data = world, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey50") + 
  geom_polygon(data = x[x$SEASONAL == 4,], aes(x = long, y = lat, group = group), fill = alpha("blueviolet", 0.5)) +

  coord_equal()

print(pp)















#____________________________________________________________________________
#.....DEBUGGING AREA

#____________________________________________________________________________



bird.file = bird.files[grep("Corvus_corone", bird.files)]  
bird.file = bird.files[2]  

bird.dat1 <- bird.dat

t0 <- Sys.time()
data.frame(getSppRow(bird.file, wd.bird, wd.env, wd.output, bios))
t1 <- Sys.time()
t1-t0

for(bird.file in bird.files[231:400]){
  
  bird.dat1 <- data.frame(rbind(bird.dat1,
                                getSppRow(bird.file, wd.bird, wd.env, wd.output, bios)))
  
  print(paste("SPECIES no.", which(bird.files == bird.file), "out of", length(bird.files)))
  
}


aaply(seq(1,10000,100), function(x) rnorm(1, mean=x+(1:100), 
                                          .parallel=p.flag)
      
      cores <- detectCores()      
      
      min <- 271
      max <- 370
      
      registerDoParallel(cl, cores=cores-1)
      
      
      bird.dat.parallel <- parLapply(cl, bird.files[min:max],
                                     fun = function(x){data.frame(getSppRow(x, wd.bird, wd.env, wd.output, bios))})
      , 
      wd.bird, wd.env, wd.output, bios)

.margins = seq(min, max, length(min:max)/cores)


cl <- makeCluster(cores - 1, type = "PSOCK")
registerDoParallel(cl)

clusterExport(cl, varlist =c("wd.bird", "wd.env", "wd.output", "bios", "getSppRow", "fixholes",
                             "getBioRow", "bioDataTable", "areaDataTable", "latWts"))





