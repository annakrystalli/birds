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
  
  wts <- latWts(unlist(coo.polys), e)
  var.all <- data.frame(rep(weighted.var(unlist(e.dat), wts),  dim(var.regions)[1]))
  names(var.all) <- paste(bio, "varall", sep=".")
  
  if(bio == "alt"){ area <- gArea(fixholes(xt), byid  = T)/10^6
                    bio.dat <- data.frame(area, mean.regions, max.regions, min.regions, var.regions, var.all)}else{
                      bio.dat <- data.frame(mean.regions, max.regions, min.regions, var.regions, var.all)}
  return(bio.dat)
}
bioDataTable <- cmpfun(bioDataTableF)

getBioRowF <- function(x, bios){
  
  if(!paste(bios[1], "m", sep=".") %in% names(x@data)){
  bio.dat.nm <- paste(rep(bios, each = 4), c(".m", ".max", ".min", ".var"), sep = "")
  bio.dat <-data.frame(matrix(NA, ncol = length(bio.dat.nm), nrow=1))
  names(bio.dat) <- bio.dat.nm}else{
  
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
  }}
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
