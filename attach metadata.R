
metadata <- read.csv("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/shiny/data/metadata.csv")

metadata[grep("_qc", metadata$ms.vname), 
         c("cat", "type", "scores", "levels")] <- data.frame(matrix(c("QC",	"bar","0,1,2","0,1,2"),
                                                         ncol = 4))


metadata[grep("RE", metadata$list.vname), 
         c("cat", "type", "scores", "levels")]<- data.frame(matrix(c("repro",	"bar","0,1,2,3,4","no,1-33%,34-66%,67-99%,100%"),
                                                                   ncol = 4))


metadata[grep("absent", metadata$list.vname), 
         c("cat", "type", "scores", "levels")]<- data.frame(matrix(c("repro",	"bar","0,1","FALSE,TRUE"),
                                                                   ncol = 4))



write.csv(metadata, "~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/shiny/data/metadata.csv",
          row.names = F)



lapply(strsplit(a, ","), FUN = as.numeric)