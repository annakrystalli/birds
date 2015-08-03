rm(list=ls())


dat <- read.csv("~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/master Data Sheet.csv",
          stringsAsFactors = F)


var.counts <- data.frame(var = names(dat), count = apply(dat, 2, FUN = function(x){sum(!is.na(x))}), row.names = NULL)

var.counts <- var.counts[order(var.counts$count, decreasing = T),]

write.csv(var.counts, "~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/shiny/data/var counts.csv",
          row.names = F)


