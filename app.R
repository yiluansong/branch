library(shiny)
library(leaflet)
library(raster)
library(colorRamps)
library(plotrix)
library(rgdal)

file <- c("TMP_World.nc")
TMP <- brick(file)
TMP <- brick(TMP[[81:114]])
allyears <- rep(1, nlayers(TMP))
mnTMP <- stackApply(TMP, indices = allyears, fun = mean)

file <- c("SOS_World_0.5deg_QA_Smooth.nc")
SOS <- brick(file)
allyears <- rep(1, nlayers(SOS))
mnSOS <- stackApply(SOS, indices = allyears, fun = mean)

Vc <- raster("Vc.nc")
Vp <- raster("Vp.nc")
projection <- raster("projection.nc")
mismatch <- raster("mismatch.nc")

####
TMP_df <- as.data.frame(TMP, xy = T)
SOS_df <- as.data.frame(SOS, xy = T)
time<-1981:2014
####
pal_tmpano <- colorNumeric(palette = matlab.like(10),  domain = c(-3,3), na.color = "transparent")
pal_sosano <- colorNumeric(palette = rev(matlab.like(10)),  domain = c(-30,30), na.color = "transparent")

qpal_Vc <- colorBin("Reds", domain = c(0,10000),bins = c( 0, 1, 5, 10, 20, 100, 1000, 10000),na.color = "transparent")

qpal_Vp <- colorBin("Blues", domain = c(0,10000),bins = c( 0, 1, 5, 10, 20, 100, 1000, 10000),na.color = "transparent")

qpal_projection <- colorBin(rev(matlab.like(10)), domain = c(-400,400),bins = c(-400,-100,-20,-10,-5,-1,0, 1, 5, 10, 20, 100,400),na.color = "transparent")

qpal_mismatch <- colorBin(matlab.like(10), domain = c(-400,400),bins = c(-400,-100,-20,-10,-5,-1,0, 1, 5, 10, 20, 100,400),na.color = "transparent")

############################
ui<-fluidPage(
  navbarPage("IMCP: An Interactive Map of Climate and Phenology", id="nav",
             
             tabPanel("Mean Annual Temperature vs Start of Season",
                      # div(class="outer",
                      selectInput("layer", "Layer",
                                  choices = c("TMP Anomaly", "SOS Anomaly","TMP Velocity","SOS Velocity", "Projection", "Mismatch")),
                      sliderInput("year", "Year",
                                  min=1981, max=2014, value=1, ticks=T),    
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                      leafletOutput("raster_map", height=300,width=650),
                          
                      
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 120, left = "auto", right = 360, bottom = "auto",width = 300, height = "auto",
                                        
                                        h4("Temporal patterns"),
                                        plotOutput("lineplot_TMP", height = 200),
                                        plotOutput("lineplot_SOS", height = 200)
                          ),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 120, left = "auto", right = 60, bottom = "auto",width = 300, height = "auto", 
                                        
                                        h4("Spatial patterns"),
                                        plotOutput("neighbours_TMP",height = 200),
                                        plotOutput("neighbours_SOS",height = 200)
                          ),
                          tags$div(id="cite",
                                   'Data compiled for ', tags$em('"An Interactive Map of Climate and Phenology"'), ' by Yiluan Song'
                          )
                      # )
             )
  )
)


server<-function(input, output){
  
  output$raster_map = renderLeaflet({leaflet(width = "100%") %>%
      addTiles()%>%
      setView(lng = -35, lat = 30, zoom = 1)})
  
  observe({
    if(input$layer=="TMP Anomaly") {
      reactiveRaster <- reactive({TMP[[input$year-1980]]-mnTMP})
      leafletProxy("raster_map") %>%
        clearImages() %>%
        clearControls() %>% 
        addRasterImage(reactiveRaster(),colors = pal_tmpano, opacity = 1, layerId = "TMP")%>%
        addLegend(pal = pal_tmpano, values = seq(-2,2,by=1),
                  position = "bottomleft",title = "MAT Anomaly (°C)",
                  layerId = "TMP")
    }
    if(input$layer=="SOS Anomaly") {
      reactiveRaster <- reactive({SOS[[input$year-1980]]-mnSOS})
      leafletProxy("raster_map") %>%
        clearImages()%>%
        clearControls() %>% 
        addRasterImage(reactiveRaster(),colors = pal_sosano, opacity = 1, layerId = "SOS")%>%
        addLegend(pal = pal_sosano, values = seq(-15,15,by=5),
                  position = "bottomleft",title = "SOS Anomaly (day)",
                  layerId = "SOS")
    }
    if(input$layer=="TMP Velocity") {
      leafletProxy("raster_map") %>%
        clearImages() %>%
        clearControls() %>% 
        addRasterImage(Vc, colors = qpal_Vc, opacity = 1,layerId = "TMP Velocity") %>%
        addLegend(pal = qpal_Vc, values = values(Vc),
                  position = "bottomleft",title = "TMP Velocity (km/year)",
                  layerId = "TMP Velocity")
    }
    if(input$layer=="SOS Velocity") {
      leafletProxy("raster_map") %>%
        clearImages() %>%
        clearControls() %>% 
        addRasterImage(mask(Vp,Vc), colors = qpal_Vp, opacity = 1,layerId = "SOS Velocity") %>%
        addLegend(pal = qpal_Vp, values = values(mask(Vp,Vc)),
                  position = "bottomleft",title = "SOS Velocity (km/year)",
                  layerId = "SOS Velocity")
    }
    if(input$layer=="Projection") {
      leafletProxy("raster_map") %>%
        clearImages() %>%
        clearControls() %>% 
        addRasterImage(projection, colors = qpal_projection, opacity = 1,layerId = "Projection") %>%
        addLegend(pal = qpal_projection, values = values(projection),
                  position = "bottomleft",title = "Projection (km/year)",
                  layerId = "Projection")
    }
    if(input$layer=="Mismatch") {
      leafletProxy("raster_map") %>%
        clearImages() %>%
        clearControls() %>% 
        addRasterImage(mismatch, colors = qpal_mismatch, opacity = 1,layerId = "Mismatch") %>%
        addLegend(pal = qpal_mismatch, values = values(mismatch),
                  position = "bottomleft",title = "Mismatch (km/year)",
                  layerId = "Mismatch")
    }
  })
  
  #Show popup on click
  observeEvent(input$raster_map_click, {
    click <- input$raster_map_click
    lat<-(90+click$lat)%%180-90
    lng<-(180+click$lng)%%360-180
    text_lat<-paste("Latitude: ", round(lat,2))
    text_lng<-paste("Longtitude: ", round(lng,2))
    text_mnTMP<-paste("Mean TMP: ", round(extract(mnTMP,data.frame(lng,lat)),2)," °C")
    text_mnSOS<-paste("Mean SOS: ", round(extract(mnSOS,data.frame(lng,lat)),2), " Day")
    text_Vc<-paste("TMP Velocity: ", round(extract(Vc,data.frame(lng,lat)),2)," km/year")
    text_Vp<-paste("SOS Velocity: ", round(extract(Vp,data.frame(lng,lat)),2)," km/year")
    text_projection<-paste("Projection: ", round(extract(projection,data.frame(lng,lat)),2)," km/year")
    text_mismatch<-paste("Mismatch: ", round(extract(mismatch,data.frame(lng,lat)),2)," km/year")
    
    content <- as.character(tagList(
      text_lat, tags$br(),
      text_lng, tags$br(),
      text_mnTMP, tags$br(),
      text_mnSOS, tags$br(),
      text_Vc, tags$br(),
      text_Vp, tags$br(),
      text_projection, tags$br(),
      text_mismatch
    ))
   
    leafletProxy("raster_map") %>%
      clearPopups() %>%
      addPopups(click$lng, click$lat, content)
  })
  
  #Lineplot
  observeEvent(input$raster_map_click, {
  click <- input$raster_map_click
  lat<-(90+click$lat)%%180-90
  lng<-(180+click$lng)%%360-180
  xcoor<-round((lng - 0.25) * 2) / 2 + 0.25
  ycoor<-round((lat - 0.25) * 2) / 2 + 0.25
  
  sp<-SpatialPoints(cbind(xcoor, ycoor))
  sp_top<-SpatialPoints(cbind(xcoor, ycoor+0.5))
  sp_bottom<-SpatialPoints(cbind(xcoor, ycoor-0.5))
  sp_right<-SpatialPoints(cbind(xcoor+0.5, ycoor))
  sp_left<-SpatialPoints(cbind(xcoor-0.5, ycoor))
  sp_top_left<-SpatialPoints(cbind(xcoor-0.5, ycoor+0.5))
  sp_bottom_left<-SpatialPoints(cbind(xcoor-0.5, ycoor-0.5))
  sp_top_right<-SpatialPoints(cbind(xcoor+0.5, ycoor+0.5))
  sp_bottom_right<-SpatialPoints(cbind(xcoor+0.5, ycoor-0.5))
  
  tmp_neighbours<-matrix(c(extract(mnTMP,sp_top_left),extract(mnTMP,sp_top),extract(mnTMP,sp_top_right),
                           extract(mnTMP,sp_left),extract(mnTMP,sp),extract(mnTMP,sp_right),
                           extract(mnTMP,sp_bottom_left),extract(mnTMP,sp_bottom),extract(mnTMP,sp_bottom_right))
                         , nrow=3, byrow=TRUE)
  
  sos_neighbours<-matrix(c(extract(mnSOS,sp_top_left),extract(mnSOS,sp_top),extract(mnSOS,sp_top_right),
                           extract(mnSOS,sp_left),extract(mnSOS,sp),extract(mnSOS,sp_right),
                           extract(mnSOS,sp_bottom_left),extract(mnSOS,sp_bottom),extract(mnSOS,sp_bottom_right))
                         , nrow=3, byrow=TRUE)
    output$neighbours_TMP <- renderPlot({
      color2D.matplot(tmp_neighbours,show.values = TRUE, main="Mean MAT (°C)", xlab="Latitude",ylab="Longitude",axes=FALSE,extremes = c("white","red"))
    })
    
    output$neighbours_SOS <- renderPlot({
      color2D.matplot(sos_neighbours,show.values = TRUE, main="Mean SOS (day)", xlab="Latitude",ylab="Longitude",axes=FALSE,extremes = c("dark green","white"))
    })
  })
  
  #Lineplot
  observeEvent(input$raster_map_click, {
    click <- input$raster_map_click
    lat<-(90+click$lat)%%180-90
    lng<-(180+click$lng)%%360-180
    xcoor<-round((lng - 0.25) * 2) / 2 + 0.25
    ycoor<-round((lat - 0.25) * 2) / 2 + 0.25
    
    
    ts_TMP<-as.numeric(TMP_df[TMP_df$x == xcoor & TMP_df$y ==ycoor,c(3:dim(TMP_df)[2])])
    
    ts_SOS<-as.numeric(SOS_df[SOS_df$x == xcoor & SOS_df$y ==ycoor,c(3:dim(SOS_df)[2])])
    
    # df<-data.frame(ts_TMP,ts_SOS,time)
    # df<-df[complete.cases(df),]
    
    output$lineplot_TMP <- renderPlot({
      plot(ts_TMP~time,type="l",lty=2,
           ylab="MAT (°C)",xlab="",
           col="red",  col.lab="red")
      abline(lm(ts_TMP~time),col="red")
    })
    
    output$lineplot_SOS <- renderPlot({
      plot(ts_SOS~time,type="l",lty=2,
           ylab="SOS (Day)",xlab="",
           col="dark green",  col.lab="dark green")
      abline(lm(ts_SOS~time),col="dark green")
    })
  })
}


shinyApp(ui, server)#, options = list(height=600,width=1200)
