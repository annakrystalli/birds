
processCockburn <- function(){

dd <- read.csv("standardised csv data/Cockburn 2006_Appendix A resub process.csv", stringsAsFactors=FALSE)


#convert shortened genera names to full
rep <- grep("\\.$", dd$species1)
src <- which(!1:length(dd$species1) %in% rep)


for(i in rep){
  if(!substr(dd$species1[i],1,1) == substr(dd$species1[src[max(which(src < i))]],1,1)){
    print(paste("error! genera name mismatch", dd$species))
    break
  }else{
  dd$species1[i] <- dd$species1[src[max(which(src < i))]]}
}


#fill in references
rep1 <- grep("--", dd$pc.ref)
src1 <- which(!1:length(dd$species1) %in% rep1)

for(i in rep1){

    dd$pc.ref[i] <- dd$pc.ref[src1[max(which(src1 < i))]]
}

#replace species column & delete individual columns and change ref column name
dd$species <- paste(dd$species1, dd$species2, sep = "_")
dd <- dd[, -which(names(dd) %in% c("species1", "species2"))]
names(dd)[names(dd) == "pc.ref"] <- "ref"

dd <- dd[!dd$pc.known == "Subspp",]
dd <- dd[!dd$species == "Coua_delalandei",]#Extinct
dd <- dd[!dd$species == "Sceloglaux_albifacies",]#Extinct
dd <- dd[!dd$species == "Gallicolumba_salamonis",]#Extinct
dd <- dd[!dd$species == "Microgoura_meeki",]#Extinct
dd <- dd[!dd$species == "Podilymbus_gigas",]#Extinct
dd <- dd[!dd$species == "Nesillas_aldabrana",]#Extinct
dd <- dd[!dd$species == "Aplonis_mavornata",]#Extinct
dd <- dd[!dd$species == "Myadestes_myadestinus",]#Extinct
dd <- dd[!dd$species == "Viridonia_sagittirostris",]#Extinct
dd <- dd[!dd$species == "Paroreomyza_flammea",]#Extinct


dd$species[dd$species == "Ceuthmochares_australia"] <- "Ceuthmochares_australis" #Correct name
dd$species[dd$species == "Arundinicola_tricolor"] <- "Alectrurus_tricolor" #Correct name
dd$species[dd$species == "Arundinicola_risora"] <- "Alectrurus_risora" #Correct name
dd$species[dd$species == "Amytornis_merrotsyi"] <- "Amytornis_merrotsoyi" #Correct name
dd$species[dd$species == "Ambylramphus_holosericeus"] <- "Amblyramphus_holosericeus" #Correct name


write.csv(dd, "standardised csv data/Cockburn 2006_Appendix A resub.csv", row.names = F)}

processCockburn()

