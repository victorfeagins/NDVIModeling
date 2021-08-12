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
library(coda)# Needed since as.matix behaves differently when this is loaded
library(GOESDiurnalNDVI)
library(leaflet)
library(ggplot2)
library(plotly)

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
numCores = 4 #has to be greater then 2 because workers have a size limit also
options(future.globals.maxSize= 891289600)
plan(multisession, workers = numCores)



summarydirectory <- "/projectnb/dietzelab/GOES_DataFTP/SummaryModel" #The summary files
inputdirectory <- "/projectnb/dietzelab/GOES_DataFTP/InputFilesNDVIModel/2021/" #This is the raw data points
modeldirectory <- "/projectnb/dietzelab/GOES_DataFTP/OutputFilesNDVIModel/2021/" #This is the mcmc output
sitecodebook <- "/projectnb/dietzelab/vfeagins/Programming/NDVI_Modeling/GOESdownloadSites.csv" #This is the site codebook




Sites <- read.csv(sitecodebook)
# Sites <- Sites %>% 
#   filter(siteName == "harvard" | siteName == "NEON.D01.HARV.DP1.00033")# These sites are very close together

sitenames <- Sites$siteName

modelfiles <- list.files(modeldirectory)
inputfiles <- list.files(inputdirectory)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NDVI Model Examination Tool"),
  h3("Site Map"),
  h5("Click on a site to see NDVI timeseries"),
  leafletOutput("mymap"),
  h3("Site Summary Time Series"),
  h5("Click on a point to see more details"),
  plotlyOutput("summarytimeseries"),
  h3(textOutput("action")),
  plotOutput("modelplot"),
  tableOutput("summarytable")
  
)
  


server <- function(input, output) {
  #Select Map ----
  output$mymap <- renderLeaflet({
    leaflet(data = Sites) %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>% #base map
      addMarkers(~Long, ~Lat, label = ~siteName, layerId = ~siteName)# layer id is what can be accessed as id by click events
  })
  #Summary csv file ----
 summaryfile <- reactive({
   summarydirectory %>% 
     list.files() %>% 
     str_subset(as.character(input$mymap_marker_click$id)) %>% 
     file.path(summarydirectory, .) %>% 
     read.csv()
 })
 #Summary Time series Plot ----
  output$summarytimeseries <- renderPlotly({
    summaryfile() %>%
      filter_all(all_vars(!is.na(.))) %>%  #error y does not like missing values.
      plot_ly(key = ~DaySiteID, source = "Date") %>% #Key is what is transferred to event data
      add_markers(x = ~as.Date(Date), y = ~c.50.,
                  error_y = ~list(symmetric = FALSE,
                                  arrayminus = c.50. - c.2.5., #error bars need the difference value
                                  array = c.97.5. - c.50.,
                                  color = '#000000')) %>% # Color of error bars
      layout(title = str_c(as.character(input$mymap_marker_click$id),"_", "NDVI Time Series"),
             xaxis = list(title = 'Date'),
             yaxis = list(title = 'NDVI (95% CI)')) %>% 
      event_register("plotly_click")
    
  })
  #Plotly Click ----

  
  #Event Data click something here
  plotly_event <- reactive({
    event_data(event = "plotly_click", source = "Date") %>% 
      select(x, key)
  })

  #Examining Text  ----
  output$action <- renderText({
    paste("Examining", input$mymap_marker_click$id)
  })
  
  # datevector <- reactive({
  #   modelfiles %>%
  #   str_subset(input$mymap_marker_click$id)%>% #Sitename
  #   str_extract("_[\\d]+_[\\d]+_") %>%
  #   as.Date("_%Y_%j_")
  # })
  # output$daterange <- 
  #   renderUI({
  #     selectInput("date", "Select a Date", sort(datevector()))
  #     })
 
MCMCFile <- reactive({
  MCMCFile<- modelfiles %>% 
    str_subset(str_c(as.character(plotly_event()["key"]),"_")) %>% #Picking the file from date and site
    file.path(modeldirectory, .) %>% 
    readRDS()
  
})
output$test <- renderText({
  as.character(plotly_event()["key"])
})
RawData <- reactive({
  inputfiles %>% 
  str_subset(str_c(as.character(plotly_event()["key"]),"_")) %>% #Picking the file from date and site
  file.path(inputdirectory, .) %>% 
  read.csv()
})

output$modelplot <- renderPlot({
  diurnalplot(MCMCFile(), RawData(), site = input$mymap_marker_click$id, date = plotly_event()["x"])#x is the date from the plotly
})

output$summarytable <- renderTable({
  summaryfile() %>% 
    filter(DaySiteID == as.character(plotly_event()["key"])) %>% 
    select(-DaySiteID, -Date, -Site)
    
})



  
}

# Run the application 
shinyApp(ui = ui, server = server)
