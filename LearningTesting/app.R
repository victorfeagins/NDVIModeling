#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Libraries ----
library(shiny)
library(stringr)
library(dplyr)
library(coda)
library(GOESDiurnalNDVI)
library(leaflet)

#Parllelization ----
library(future)
library(future.apply)


diurnalplot <- function(MCMCFile, RawData, site, date){
  out.mat <- as.matrix(MCMCFile) #need coda loaded for this work properly
  rndNums <-  sample.int(nrow(out.mat), 10000, replace = T)
  
  xseq <- seq(0,24,.001)
  
  a <- out.mat[rndNums,1]
  c <- out.mat[rndNums,2]
  k <- out.mat[rndNums,3]
  
  
  values <- future_mapply(diurnalExp, a, c, k, MoreArgs = list(xseq = xseq)) #Each column is one possible NDVI time series
  
  
  
  ci <- future_apply(values,1,quantile,c(0.025,0.5, 0.975), na.rm= TRUE) %>% 
    t()
  
  
  plot(x=list(),y=list(),main=str_c(site,"_", date),ylim=c(0,1),xlim=c(0,24),ylab="NDVI",xlab="Hour",cex=2.5)
  ecoforecastR::ciEnvelope(xseq,ci[,1],ci[,3],col="lightBlue") 
  lines(xseq,ci[,2],col="black")

  points(RawData$x, RawData$y)
  
}




#Set up ----
set.seed(123)
numCores = 4 #has to be greater then 2 because workers have a size limit
options(future.globals.maxSize= 891289600)
plan(multisession, workers = numCores)

inputdirectory <- "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/" #This is the raw data points
modeldirectory <- "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2021/" #This is the mcmc output

sitecodebook <- "/projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/GOESdownloadSites.csv" #This is the site codebook




Sites <- read.csv(sitecodebook)

sitenames <- Sites$siteName

modelfiles <- list.files(modeldirectory)
inputfiles <- list.files(inputdirectory)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NDVI Model Examination Tool"),
  h3("Select a Site"),
  leafletOutput("mymap"),
  
  h3(textOutput("action")),
  conditionalPanel(
    condition = "input.mymap_marker_click !== undefined",
    uiOutput("daterange")),
  plotOutput("modelplot")
  
)
  


server <- function(input, output) {
  output$mymap <- renderLeaflet({
    leaflet(data = Sites) %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addMarkers(~Long, ~Lat, label = ~siteName, layerId = ~siteName)
  })
  
  output$action <- renderText({
    paste("Examining", input$mymap_marker_click$id)
  })
  
  datevector <- reactive({
    modelfiles %>%
    str_subset(input$mymap_marker_click$id)%>% #Sitename
    str_extract("_[\\d]+_[\\d]+_") %>%
    as.Date("_%Y_%j_")
  })
  output$daterange <- 
    renderUI({
      selectInput("date", "Select a Date", sort(datevector()))
      })
 
MCMCFile <- reactive({
  MCMCFile<- modelfiles %>% 
    str_subset(format(as.Date(input$date), str_c(input$mymap_marker_click$id,"_%Y_%j_"))) %>% #Picking the file from date and site
    file.path(modeldirectory, .) %>% 
    readRDS()
  
})

RawData <- reactive({
  inputfiles %>% 
  str_subset(format(as.Date(input$date), str_c(input$mymap_marker_click$id,"_%Y_%j_"))) %>% #Picking the file from date and site
  file.path(inputdirectory, .) %>% 
  read.csv()
})

output$modelplot <- renderPlot({
  diurnalplot(MCMCFile(), RawData(), site = input$mymap_marker_click$id, date = input$date)
})



  
}

# Run the application 
shinyApp(ui = ui, server = server)
