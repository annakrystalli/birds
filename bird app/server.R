library(shiny)




shinyServer(function(input, output) {
  
  # Partial example
  output$taxoDat <- renderUI({
    choices <- c("all", sort(as.character(unique(
      dat$family[!is.na(dat[,input$variable])]))))
    selectInput("taxo", "select families", choices = choices,
                selectize = TRUE)
  })
  
  output$plot1 <-renderPlotly({
  
    
  if(input$taxo == "all"){
    select <- rep(TRUE, length(dim(dat)[1]))}else{
      select <- dat$family %in% input$taxo}
   select <- select & !is.na(dat[,input$variable])
    
   if(class(dat[,input$variable]) == "factor"){
     plot_ly(x = levels(dat[select,input$variable]),
             y = as.numeric(table(dat[select,input$variable])), 
             type = "bar")
     
     layout(p, autosize = T, xaxis = list(title = input$variable), 
            yaxis = list(title = "counts"), 
            title = paste("n =", sum(select)), margin = list(b= 100))
     
   }else{
     
   
    
    minx <- min(dat[select,input$variable]) - 1
    maxx <- max(dat[select,input$variable]) + 1
    size <- (maxx - minx) / input$bins
    
    plot_ly(data = dat, x = dat[select,input$variable], 
            type = "histogram", autobinx = F, 
            xbins = list(start = minx, 
                         end = maxx, 
                         size = size)
            )
    
    layout(p, xaxis = list(title = input$variable, #range = c(minx, maxx), 
                           autorange = T,
                           autotick = T, tick0 = 0))  # dtick = size))
   }
    
    })
})