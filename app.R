library(shiny)
library(raster)
library(sp)
library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(broom)
idwrasters<-readRDS("idwrasters.Rdata")

sppnames<-readRDS("sppnames.Rdata")

cmpboundaries<-shapefile("CMPboundaries.shp")

cmpboundaries_fortified <- tidy(cmpboundaries)

sppcodetrans<-read.csv("IBP-AOS-LIST21.csv",header=T)

sppcodetrans$SPEC


sppcodetrans_overlap<-sppcodetrans[sppcodetrans$SPEC %in% sppnames,]

sppcodetrans_overlap_order<-sppcodetrans_overlap[order(sppcodetrans_overlap$SPEC),]


ui <- fluidPage(selectInput("species", "Select common name", choices = unique(sppcodetrans_overlap_order$COMMONNAME)),
                plotOutput(outputId = "raster"))

server <- function(input, output){
  datasetInput <- reactive({
    as.data.frame(idwrasters[[which(sppcodetrans_overlap_order$COMMONNAME == input$species)]],xy=TRUE) 
  }) 
  
  output$raster <- renderPlot({
    dataset <- datasetInput()
    ggplot()+geom_raster(data=dataset,aes(x=x,y=y,fill=var1.pred))+
      scale_fill_gradient(low="red",high="green")+
      geom_polygon(data=cmpboundaries_fortified,aes( x = long, y = lat,group=group),fill=NA, color="black")+
      theme_minimal()
  },height = 400, width = 600)
}
shinyApp(ui = ui, server = server)