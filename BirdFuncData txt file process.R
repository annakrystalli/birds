
BirdFuncDat <- read.delim("~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/raw data/BirdFuncDat.txt")

BirdFuncDat <- BirdFuncDat[-which(BirdFuncDat$Scientific == ""),]

names(BirdFuncDat)[names(BirdFuncDat) == 'Scientific'] <- "species"
names(BirdFuncDat)[names(BirdFuncDat) == 'BodyMass.Value'] <- "unsexed.mass"
names(BirdFuncDat)[names(BirdFuncDat) == 'BodyMass.Source'] <- "unsexed.mass_ref"
names(BirdFuncDat)[names(BirdFuncDat) == "ForStrat.SpecLevel"] <- "ForStrat_qc"
names(BirdFuncDat)[names(BirdFuncDat) == "BodyMass.SpecLevel"] <- "unsexed.mass_qc"

names(BirdFuncDat) <- gsub(".Source", "_ref", names(BirdFuncDat))
names(BirdFuncDat) <- gsub(".Certainty", "_qc", names(BirdFuncDat))
names(BirdFuncDat) <- gsub(".EnteredBy", "_observer", names(BirdFuncDat))

BirdFuncDat <- BirdFuncDat[,-c(1:7,9,39:40)]

write.csv(BirdFuncDat, "~/Google Drive/Sex Roles in Birds Data Project/Inputs/Anna workflow/data in/standardised csv data/BirdFuncDat.csv", row.names = F)
