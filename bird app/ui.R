

library(shiny)
library(plotly)



dat <- read.csv("~/Google Drive/Sex Roles in Birds Data Project/Outputs/data/master Data Sheet.csv",
                stringsAsFactors = T)

vars <- as.list(names(dat))
names(vars) <- names(dat)

vars <- sort(names(dat))
  
h1("my title")

shinyUI(fluidPage(theme = "bootstrap.css",

  
  h1(strong("sex roles in birds data exploration app")),
  img(src = "bird_sil.png"),
  
  br(""),
  
  
  h3("Explore single variables"),
  br("Use this panel to exlore the distribution of individual variables. 
    You can subset the data by conditions on up to three covariates.
    Bins in histograms can also be altered using the slider"),
  br(""),
  
  sidebarLayout(
    sidebarPanel(h2(strong(em("Input select"))),
                 checkboxGroupInput("group", "select variable group", 
                                    choices = c("eco", "macro", "repro")),
                 selectInput("variable", "select variable", choices = sort(vars)),
                 
                 br(),
                 h3(strong(em("subset"))),
                 uiOutput("taxoDat")
          
                 ),
    
    mainPanel(plotlyOutput("plot1"),
              helpText("use slider to select number of histogram bins"),
              sliderInput("bins", label = "", min = 1, max = 100, value = 15))
  )
  # Copy the line below to make a select box 
  #selectInput("select", label = h4("Select input"), 
             # choices = vars, 
             # selected = 1),
  
))