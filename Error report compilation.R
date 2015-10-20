
#____________________________________________________________________________
#.....SETTINGS

options(stringsAsFactors = F)


eres <- "2.5"
input.folder <- "~/Documents/Range Match Data/"
bird.folder <- "BirdLife Data/Shapefiles/"
env.folder <-paste("BioCLIM Data/bil",eres,"/", sep="")
output.folder <- "Output.test/"

wd.bird <-paste(input.folder, bird.folder, sep = "")
wd.env <-paste(input.folder, env.folder, sep = "")
wd.output <- paste(input.folder, output.folder, sep = "")
dir.create(path = paste(wd.output, "Matched Shapefiles/", sep = ""), showWarnings = F)
dir.create(path = paste(wd.output, "range dat/", sep = ""), showWarnings = F)

bird.files <- list.files(wd.bird, pattern = ".shp")
env.files <- list.files(wd.env, pattern = ".bil")

bios <- c("alt",paste("bio", 1:19, sep=""))





# Compile error files...............................................................................................................................

error.files <- list.files(paste(wd.output, "error reports/", sep = ""))
for(error.file in error.files){
  
  error <- read.csv(paste(wd.output, "error reports/", error.file, sep = ""), 
           stringsAsFactors = F)
  
  if(error.file == error.files[1]){
    error.report <- error
  }else{error.report <- rbind(error.report,error)}
}

write.csv(error.report, paste(wd.output, "error report.csv", sep = ""))

# Testing.........

error.bird.files <- bird.files[grep(error.report$species, bird.files)]


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
write.csv(bird.dat.parallel, file = paste(wd.output, "bird.dat.csv", sep = ""))






























getSppRow(bird.file, wd.bird, wd.env, wd.output, bios, 
          input.folder, overwrite = T)
