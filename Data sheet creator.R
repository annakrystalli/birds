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
