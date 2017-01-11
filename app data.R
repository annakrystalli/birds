data <- read.csv("~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/master data sheet (wide).csv", 
                 stringsAsFactors=FALSE)

metadata <- read.csv("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/metadata/metadata.csv", 
                     stringsAsFactors=FALSE)[-(2:12),]


metadata <- metadata[!metadata$cat %in% c("ref", "qc") & metadata$dat.type != "character" &
                      !is.na(metadata$ms.vname),]



example.vars <- metadata$ms.vname[metadata$dat.type == "logical"]



spl <- metadata[metadata$dat.type %in% c("factor", "numeric"),]

set.seed(75)
example.vars <- c(example.vars, tapply(spl$ms.vname, 
                              INDEX = spl$dat.type, FUN = function(x){sample(x, 5)})$factor)


set.seed(18)
example.vars <- c(example.vars, tapply(spl$ms.vname, 
                                       INDEX = spl$dat.type, FUN = function(x){
                                         sample(x, 5, replace = F)})$numeric)

example.vars <- example.vars[example.vars != "pc.known"]


data <- cbind(data[,1:3], data[,example.vars])


data <- data[data$species %in% sample(data$species, length(data$species)/3, replace = FALSE),]

#randomise taxonomic data
rspp <- sample(nrow(data))

data[, c("species", "family")]<- data[rspp, c("species", "family")]

write.csv(data, "~/Documents/workflows/Sex Roles in Birds/birds/bird app/data/data.csv", 
          row.names = F)

metadata <- metadata[metadata$ms.vname %in% example.vars,]
write.csv(metadata, "~/Documents/workflows/Sex Roles in Birds/birds/bird app/data/metadata.csv", 
          row.names = F)
