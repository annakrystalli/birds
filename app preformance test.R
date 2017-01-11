#-----------------------------------------------------

vars <- as.list(unique(datl$var))
names(vars) <- metadata$list.vname[match(unique(dat$var), metadata$ms.vname)]
#names(vars) <- paste(metadata$cat[match(unique(dat$var), metadata$ms.vname)], 
#                    "-", metadata$list.vname[match(unique(dat$var), metadata$ms.vname)])
vars <- vars[order(names(vars))]

input <- NULL
input$varOut <- "all"


datw <- read.csv("~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/master data sheet (wide).csv",
                 stringsAsFactors = F)

datl <- read.csv("~/Google Drive/Sex Roles in Birds Data Project/Outputs/app/data.csv",
                 stringsAsFactors = F)

c <- 20
v <- length(vars)
input$varGroup <- NULL

# Run dowload tab datatable generation with each dataset and print time elapsed

for(dat.f in c("datw", "datl")){
  t0 <- Sys.time()
  
  dat <- get(dat.f)
  
  
  print(dat.f)

  
  input$varOut <- vars[1:v]
  
  
  
  if(is.null(c(input$varOut, input$varGroup))){choices <- list("NA")}else{
    if(any(input$varOut == "all")){choices <- as.list(c("all", sort(as.character(unique(
      dat$family)))))}else{
        vars.out <- unique(c(input$varOut, metadata$ms.vname[metadata$cat %in% input$varGroup]))
        choices <- as.list(c("all", sort(as.character(unique(
          dat$family[dat$var %in% vars.out])))))}}
  
  
  input$taxoOut <- choices[1:c]
  
  
  
  # Print selected families
  
  
  if(is.null(input$taxoOut)){}else{if(any(input$taxoOut == "all")){"all"}else{
    HTML(paste(input$taxoOut, '<br/>'))
  }}
  
  
  # Create vector of selected families
  if(is.null(c(input$varOut, input$varGroup))){}else{
    
    if(any(input$varOut == "all")){vars.out <- unlist(vars, use.names = F)}else{
      vars.out <- unique(c(input$varOut, metadata$ms.vname[metadata$ms.vname %in% vars & metadata$cat %in% input$varGroup]))}
    
    var.print <- sort(metadata$list.vname[match(vars.out, metadata$ms.vname)])
    
    # Create vector of selected families
    if(is.null(input$taxoOut)){fam <- unique(dat$family)}else{if(any(input$taxoOut == "all")){
      fam <- unique(dat$family)}else{fam <- input$taxoOut}}
    
    
    # calculated unique species data points for each variable for selected families
    
    if(dat.f == "datl"){
      n <- unlist(lapply(vars.out, FUN = function(x, dat){length(unique(dat$species[dat$var == x & dat$family %in% fam]))}, 
                         dat = dat))[order(metadata$list.vname[match(vars.out, metadata$ms.vname)])]}else{
                           n <- apply(dat[,unlist(vars.out)], 2, FUN = function(x){sum(!is.na(x))})
                         }
    n[n == 0] <- "-"
    
    
    
    dfprint <- data.frame(var = var.print, n = n, row.names = NULL)
  }
  
  t1 <- Sys.time()
  t <- t1 - t0
  print(t)
}
