rm(list=ls())

setwd("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/")

master <- read.csv("~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/master Data Sheet.csv")

vars <- read.csv("data collection files/Luciana/vars for collection.csv", stringsAsFactors=FALSE)$var


dd.master <- master[,vars[vars %in% names(master)]]
add.v <- vars[!vars %in% names(master)]
add <- data.frame(matrix(NA, ncol = length(add.v), nrow = nrow(master)))
names(add) <- add.v
dd.master <- data.frame(dd.master, add)


write.csv(dd.master, "data collection files/Luciana/master data sheet.csv", 
          row.names = F, na = "")

#___________________________________________________________________________________

#...Isolate added data..............................................................


origin <- read.csv("data collection files/Luciana/csvs/original data sheet.csv", 
                   stringsAsFactors = F, header = T)

updated <- read.csv("data collection files/Luciana/csvs/Luciana data sheet.csv", 
                    stringsAsFactors = F, header = T)


codes <- read.csv("data collection files/Luciana/csvs/ref_codes.csv", 
                    stringsAsFactors = F, header = T)

origin[is.na(origin)] <- ""
updated[is.na(updated)] <- ""

added <- updated
added[,3:dim(added)[2]] <- ""

added[updated != origin[, names(updated)]] <- updated[updated != origin[, names(updated)]]
    added[added == ""] <- NA
    added <- added[which(rowSums(is.na(added[,3:dim(added)[2]])) < 39),]

# trim whitespace
    added <- as.data.frame(apply(added, 2, FUN = trimws))
        
    added$ref <- as.numeric(gsub("vol.","", added$ref))
    added$ref <- codes$source[match(added$ref, codes$code)]
    
#for(num.var in names(added)[!names(added) %in% c("ref", "species")]){
 # added[,num.var] <- as.numeric(added[,num.var])
#}

added <- added[,-1]
    
write.csv(added, "standardised csv data/Luciana added data", 
              row.names = F, na = "")
    
