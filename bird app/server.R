#source("plotly_passwords.R")

library(shiny)
library(dplyr)
library(tidyr)
library(psych)
library(shinyjs)
#library("devtools") 
#install_github("ropensci/plotly")
library(plotly)
library(prettyR)

#source("plotly_passwords.R")

dat <- read.csv("data/data.csv",
                stringsAsFactors = F)

metadata <- read.csv("data/metadata.csv", 
                     stringsAsFactors = FALSE)

vars <- as.list(unique(dat$var))
names(vars) <- metadata$list.vname[match(unique(dat$var), metadata$ms.vname)]
#names(vars) <- paste(metadata$cat[match(unique(dat$var), metadata$ms.vname)], 
#                    "-", metadata$list.vname[match(unique(dat$var), metadata$ms.vname)])
vars <- vars[order(names(vars))]

shinyServer(function(input, output) {
  
  ### SINGLE VARIABLE PANEL ##################################################
  
  # Subset by family ..............
  output$taxoDat <- renderUI({

    choices <- as.list(c("all", sort(as.character(unique(
      dat$family[dat$var == input$variable])))))

    selectInput("taxo", "select families", choices = choices, selected = "all",
                selectize = TRUE)
  })
  
  # Plot output ##############################################################
  output$plot1 <-renderPlotly({

    
    
    numerise <- function(x){if(is.numeric(t <- type.convert(x))) t else x}
  
    if(is.null(input$taxo)){select <- rep(TRUE, dim(dat)[1])}else{  
  if(input$taxo == "all"){
    select <- rep(TRUE, dim(dat)[1])}else{
      select <- dat$family %in% input$taxo}}
     select <- select & dat$var == input$variable
   
   titl <- metadata$descr[metadata$ms.vname == input$variable]
   
   
   
    
   if(metadata$plot.type[metadata$ms.vname == input$variable] == "bar"){
     
     # BAR...........................................................
         
     x <- unlist(strsplit(metadata$levels[metadata$ms.vname == input$variable], ","))
     tab <- table(numerise(dat$value[select]))
     
     if(any(x == "levels")){x <- names(tab)
     y <- as.vector(tab)}else{
       y <- rep(0, length(x))
       names(y) <- unlist(strsplit(metadata$scores[metadata$ms.vname == input$variable], ","))
       y[match(names(tab), names(y))] <- tab}
     names(y) <- NULL
     
     p1 <- plot_ly(x = x,  y = y, 
             type = "bar",
             marker = list(color = toRGB("aquamarine3"),
                           line = list(color = toRGB("aquamarine3")))) %>%
     
     layout(autosize = T, xaxis = list(title = titl), 
            yaxis = list(title = "counts"), 
            title = paste("n =", sum(select)), margin = list(b= 100))
     p1}else{
     
              # HISTOGRAM.....................................................
              
              if(input$log == T){x <- log(as.numeric(dat$value[select]))
              titl <- paste("log", titl)}else{
                x <- as.numeric(dat$value[select])
              }
              
              if(all(x >= 0 & 1 >= x)){xaxis <- list(title = titl, range = c(0,1), 
                   autorange = F, autotick = T, tick0 = 0)}else{
                xaxis <- list(title = titl, autorange = T,
                              autotick = T, tick0 = 0) 
              }
              
              minx <- min(x) - 1
              maxx <- max(x) + 1
              size <- (maxx - minx) / input$bins
    

    
            p1 <- plot_ly(data = dat, x = x, 
                      type = "histogram", autobinx = F, 
                      xbins = list(start = minx, 
                                   end = maxx, 
                                   size = size),
                      marker = list(color = toRGB("aquamarine3"),
                                    line = list(color = toRGB("aquamarine3")))) %>%
            
              
              layout(xaxis = xaxis,
                     title = paste("n =", sum(select)))  # dtick = size))
            p1
   }
    
    })
  
  # Summary output ##############################################################
  output$summary <- renderDataTable({
  
    
    numerise <- function(x){if(is.numeric(t <- type.convert(x))) t else x}
  
    if(input$taxo == "all"){
      select <- rep(TRUE, dim(dat)[1])}else{
        select <- dat$family %in% input$taxo}
    select <- select & dat$var == input$variable
    
    x <- dat$value[select]
    
    if(metadata$plot.type[metadata$ms.vname == input$variable] == "histogram"){
      x <- numerise(x)
      sum <- summary(x)
      extra <- as.data.frame(round(psych::describe(x),2))
      sumtab <- as.data.frame(c(sum, extra[,c("n","sd", "range", "skew", "kurtosis", "se")]))
      names(sumtab) <- gsub("X", "", names(sumtab))
      sumtab}else{
        sumtab <- round(prettyR::describe(x)[[2]]$x,2)
        sumtab <- cbind(descr = rownames(sumtab), sumtab)}
    
    
  }, options = list(searching = FALSE,
                    paging = FALSE))
  
  # Data output ##############################################################
  output$data <- renderDataTable({

    if(input$taxo == "all"){
      select <- rep(TRUE, dim(dat)[1])}else{
        select <- dat$family %in% input$taxo}
    select <- select & dat$var == input$variable
    
    rawdat <- dat[select, c("species", "family", "value", "data")]
    rawdat <- as.data.frame(lapply(rawdat,FUN = numerise))
  })
  
  
  ### CROSS VARIABLE PANEL ##################################################
  output$plot2 <-renderPlotly({

    #source("app_output_functions.R")
    
    df <- widenMaster(vars = c(input$var1, input$var2), 
                    species = unique(dat$species[dat$var %in% c(input$var1, input$var2)]), 
                    master = dat, 
                    metadata = metadata)
    df <- df[complete.cases(df),]
    
    validate(
      need(nrow(df) >0, "no species with overlapping data. Select different variable pair")
    )
   
    
    
    ### Both variables NUMERIC ##########################################################
    if(all(metadata$plot.type[metadata$ms.vname == input$var1] == "histogram",
           metadata$plot.type[metadata$ms.vname == input$var2] == "histogram")){
      type <- "scatter"
      x <- df[, which(names(df) == input$var1)]
      x.titl <- metadata$descr[metadata$ms.vname == input$var1]
      if(input$log1 == T){x <- log(x)
      x.titl <- paste("log", x.titl)}
      y <- df[, which(names(df) == input$var2)]
      y.titl <- metadata$descr[metadata$ms.vname == input$var2]
      if(input$log2 == T){y <- log(y)
      y.titl <- paste("log", y.titl)}
      
      
      z <- NULL
      xaxis <- list(title = x.titl)
      yaxis <- list(title = y.titl)
      text <- df$species}else{
        if(all(metadata$plot.type[metadata$ms.vname == input$var1] == "bar",
               metadata$plot.type[metadata$ms.vname == input$var2] == "bar")){
          
          type <- "heatmap"
          x1 <- df[, which(names(df) == input$var1)]
          x2 <- df[, which(names(df) == input$var2)]
          
          r.nm <- data.frame(s = unlist(strsplit(metadata$scores[metadata$ms.vname == input$var1], ",")),
                             n = unlist(strsplit(metadata$levels[metadata$ms.vname == input$var1], ",")))
          c.nm <- data.frame(s = unlist(strsplit(metadata$scores[metadata$ms.vname == input$var2], ",")),
                             n = unlist(strsplit(metadata$levels[metadata$ms.vname == input$var2], ",")))
          
          tab <- table(x1, x2)
          ids <- data.frame(v = as.vector(tab), 
                            r = rep(match(rownames(tab), r.nm$s),length(colnames(tab))),
                            c = rep(match(colnames(tab), c.nm$s), each = length(rownames(tab))))
          
          z <- matrix(0, nrow = nrow(r.nm), ncol = nrow(c.nm))
          z[as.matrix(ids[,c("r","c")])] <- ids[,"v"]
          z <- t(z)
          
          y <- as.character(c.nm[,"n"])
          x <- as.character(r.nm[,"n"])
          
          xaxis <- list(title = metadata$descr[metadata$ms.vname == input$var1])
          yaxis <- list(title = metadata$descr[metadata$ms.vname == input$var2])
          text <- ""
          
        }else{
          
          df <- df[,c("species", input$var1, input$var2)]
          
          vid <- metadata$plot.type[match(c(input$var1, input$var2), metadata$ms.vname )] == "bar"
          x <- df[,c(input$var1, input$var2)[vid]]
          y <- df[,c(input$var1, input$var2)[!vid]]
          y.titl <- metadata$descr[metadata$ms.vname == c(input$var1, input$var2)[!vid]]
          
          if(input[[c("log1", "log2")[!vid]]] == T){y <- log(y)
          y.titl <- paste("log", y.titl)}
          
          x.nm <- data.frame(s = unlist(strsplit(metadata$scores[metadata$ms.vname == c(input$var1, input$var2)[vid]], ",")),
                             n = unlist(strsplit(metadata$levels[metadata$ms.vname == c(input$var1, input$var2)[vid]], ",")))
          x <- x.nm$n[match(x, x.nm$s)]
          type <- "box"
          
          xaxis <- list(title = metadata$descr[metadata$ms.vname == c(input$var1, input$var2)[vid]])
          yaxis <- list(title = y.titl)
          z <- NULL
          text <- df$species
          
        }
        
      }
      
  
      
      p2 <- plot_ly(x = x, y = y, z = z, hoverinfo ="x+y+text", text = text,
              type = type, mode = "markers", colorscale = "Greens", reversescale = T,
              marker = list(color = toRGB("aquamarine2"), opacity = 0.5, size = 10,
                            line = list(color = toRGB("aquamarine4"), width = 2))) %>%
      
      layout(xaxis = xaxis, yaxis = yaxis,
             title = paste("n =", dim(df)[1])) 
      
      p2
  })
  
  ### DOWNLOAD PANEL ##################################################
  
  # Print selected variables
  output$var_out <- renderUI({
    
    if(is.null(c(input$varOut, input$varGroup))){}else{
    
    if(any(input$varOut == "all")){vars.out <- unlist(vars, use.names = F)}else{
      vars.out <- unique(c(input$varOut, metadata$ms.vname[metadata$ms.vname %in% vars & metadata$cat %in% input$varGroup]))}

      HTML(paste(sort(metadata$list.vname[match(vars.out, metadata$ms.vname)]), '<br/>'))
      }
  })
  
  # Print n of selected variables
  output$n_out <- renderUI({
    
    if(is.null(c(input$varOut, input$varGroup))){}else{
      
      if(any(input$varOut == "all")){vars.out <- unlist(vars, use.names = F)}else{
        vars.out <- unique(c(input$varOut, metadata$ms.vname[metadata$ms.vname %in% vars & 
                                                               metadata$cat %in% input$varGroup]))}
      
      if(is.null(input$taxoOut)){fam <- unique(dat$family)}else{if(any(input$taxoOut == "all")){
        fam <- unique(dat$family)}else{fam <- input$taxoOut}}
      
        n <- unlist(lapply(vars.out, FUN = function(x, dat){length(unique(dat$species[dat$var == x & dat$family %in% fam]))}, 
                    dat = dat))[order(metadata$list.vname[match(vars.out, metadata$ms.vname)])]
        n[n == 0] <- "-"
        
      HTML(paste(n, '<br/>'))
    }
  })
  
  # Subset by family - output panel
  output$taxoDat2 <- renderUI({
    
    if(is.null(c(input$varOut, input$varGroup))){choices <- list("NA")}else{
    if(any(input$varOut == "all")){choices <- as.list(c("all", sort(as.character(unique(
      dat$family)))))}else{
      vars.out <- unique(c(input$varOut, metadata$ms.vname[metadata$cat %in% input$varGroup]))
      choices <- as.list(c("all", sort(as.character(unique(
        dat$family[dat$var %in% vars.out])))))}}
    
    selectInput("taxoOut", "select families", choices = choices, selected = "all",
                selectize = TRUE, multiple = T)
  })
  
  # Print selected families
  output$fam_out <- renderUI({
    
    if(is.null(input$taxoOut)){}else{if(any(input$taxoOut == "all")){"all"}else{
      
      HTML(paste(input$taxoOut, '<br/>'))
    }}
  })
  

  observe({
    if (input$password == password) {
      Sys.sleep(0)
      # enable the download button
      shinyjs::enable("downloadData")}
  })
  
  output$downloadData <- downloadHandler(
    filename = paste("birdDat-",Sys.Date(),".zip", sep = ""),
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      #print(tempdir())
      
      if(any(input$varOut == "all")){vars.out <- unlist(vars, use.names = F)}else{
        vars.out <- unique(c(input$varOut, 
                             metadata$ms.vname[metadata$ms.vname %in% vars & 
                                                 metadata$cat %in% input$varGroup]))}
      
      if(any(input$taxoOut == "all")){
        spp <- unique(dat$species[dat$var %in% vars.out])}else{
          spp <- unique(dat$species[dat$family %in% input$taxoOut & dat$var %in% vars.out])}
      
      wide.out <- widenMaster(vars = vars.out, species = spp, master = dat, metadata = metadata)
      raw.out <- dat[dat$species %in% spp & dat$var %in% vars.out,]
      metadata.out <- metadata[metadata$ms.vname %in% vars.out,]
      
      fs <- c("raw.csv", "wide.csv", "metadata.csv")
      write.csv(raw.out, file = "raw.csv")
      write.csv(wide.out, file = "wide.csv")
      write.csv(metadata.out, file = "metadata.csv")
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip")
  
  # disable the downdload button on page load
  shinyjs::disable("downloadData")
  
  

  
})


