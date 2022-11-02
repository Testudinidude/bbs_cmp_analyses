library(shiny)
library(raster)
library(sp)
library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(broom)
idwrasters_occ<-readRDS("idwrasters_fulllandveg.Rdata")

idwrasters_det<-readRDS("idwrasters_fulllandveg_detection.Rdata")

occdetlist<-list(idwrasters_occ,idwrasters_det)

names(occdetlist)[[1]]<-"Occupancy"
names(occdetlist)[[2]]<-"Detection"



sppnames<-readRDS("sppnames.Rdata")

cmpboundaries<-shapefile("CMPboundaries.shp")

cmpboundaries_fortified <- tidy(cmpboundaries)

sppcodetrans<-read.csv("IBP-AOS-LIST21.csv",header=T)

sppcodetrans$SPEC

coefs<-read.csv("outmsfulllandveg_90percent_rev_sig.csv",header=T)

#coefs_occ<-coefs[coefs$Process=="Occupancy",]
coefs_noint<-na.omit(coefs[coefs$Covariate!="Intercept",])

sppcodetrans_overlap<-sppcodetrans[sppcodetrans$SPEC %in% sppnames,]

sppcodetrans_overlap_order<-sppcodetrans_overlap[order(sppcodetrans_overlap$SPEC),]

ui <- shinyUI(
  fluidPage(
    titlePanel("Occupancy Models for Breeding Birds"),
    sidebarLayout(position = "left",
                  sidebarPanel("select your options",
                               selectInput("process","Select process",
                                           choices = c("Occupancy","Detection")),
                               selectInput("species", "Select common name", 
                                         choices = unique(sppcodetrans_overlap_order$COMMONNAME)),
                               selectInput("reservation", "Select reservation", 
                                         choices = unique(cmpboundaries$res)),width = 2),
                  mainPanel("plots",
                            fluidRow(
                              splitLayout(style="border: 1px solid silver:",cellwidths=800,
                                          plotOutput("raster"),
                                          plotOutput("reservation"))),
                            fluidRow(
                              plotOutput("effect",width = 1200)),width = 10))))


server <- shinyServer(function(input, output){
  datasetInput <- reactive({as.data.frame(occdetlist[[which(names(occdetlist)==input$process)]][[which(sppcodetrans_overlap_order$COMMONNAME == input$species)]],xy=TRUE)
  }) 
  datasetInput2<-reactive({
    as.data.frame(coefs_noint[which(coefs_noint$Process == input$process & coefs_noint$Species == input$species),])
  })
  datasetInput3<-reactive({
    cmpboundaries[cmpboundaries$res %in% input$reservation,]
  })
  output$raster <- renderPlot({
    dataset <- datasetInput()
    ggplot()+geom_raster(data=dataset,aes(x=x,y=y,fill=var1.pred))+
      scale_fill_gradient(low="red",high="green")+
      geom_polygon(data=cmpboundaries_fortified,aes( x = long, y = lat,group=group),fill=NA, color="black")+
      theme_minimal(base_size=16)+ggtitle("Interpolated estimates")
  })
  output$effect<-renderPlot({
    dataset2<-datasetInput2()
    ggplot(aes(x=Covariate,y=Mean),data=dataset2)+geom_point()+
      geom_errorbar(aes(x=Covariate,ymin=X5.,ymax=X95.))+
      theme_minimal(base_size=16)+ggtitle("Logit-scale coefficients (+/- 90% CI)")
  })
  output$reservation <- renderPlot({
    r<-occdetlist[[which(names(occdetlist)==input$process)]][[which(sppcodetrans_overlap_order$COMMONNAME == input$species)]]
    dataset3 <- datasetInput3()
    r2 <- as.data.frame(crop(r, extent(dataset3)),xy=TRUE)
    #r3 <- as.data.frame(mask(r2, dataset3),xy=TRUE)
    reservation_fortified<-tidy(dataset3)
    ggplot()+geom_raster(data=r2,aes(x=x,y=y,fill=var1.pred))+
      scale_fill_gradient(low="red",high="green")+
      geom_polygon(data=dataset3,aes( x = long, y = lat,group=group),fill=NA, color="black")+
      theme_minimal(base_size=16)+ggtitle(input$reservation)
  })
})

shinyApp(ui = ui, server = server)
