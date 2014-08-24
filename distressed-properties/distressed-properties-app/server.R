library(shiny)
library(ggmap)
library(maptools)
library(RColorBrewer)
library(leaflet)

load('data/data.RData')

bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
  eventFunc <- exprToFunction(eventExpr, env, quoted)
  
  initialized <- FALSE
  invisible(observe({
    eventVal <- eventFunc()
    if (!initialized)
      initialized <<- TRUE
    else
      isolate(callback())
  }))
}

shinyServer(
  function(input, output, session) {

    makeReactiveBinding('selectedParcel')
    
    data <- reactive({
      switch(input$year,
             "2011" = data11,
             "2012" = data12,
             "2013" = data13)
    })
    
    parcelsInBounds <- reactive({
      bounds = input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      subset(data(),
             lat >= latRng[1] & lat <= latRng[2] &
               lon >= lngRng[1] & lon <= lngRng[2])
    })
    
    map <- createLeafletMap(session, 'map')
    
    observe({
      map$clearMarkers()
      data = head(parcelsInBounds(), input$npar)
      data = data[length(data$risk):1,]
      cols = brewer.pal(11,'RdYlGn')
        
      map$addCircleMarker(
        data$lat,
        data$lon,
        3,
        data$parcelid,
        list(
          weight=5,
          fill=TRUE,
          opacity=1,
          color=cols[round(11*(1-data$risk))])
      )
    })
    
    observe({
      event <- input$map_marker_click
      map$clearPopups()
      isolate({
        data = parcelsInBounds()
        pardat = data[(which(data$parcelid==event$id)),]
        content = paste(pardat$address, br(), 'Risk Score:', pardat$risk)
        map$showPopup(event$lat,event$lng,content,event$id)
      })
    })
    
    output$parstable <- renderDataTable({
      data = head(parcelsInBounds(),input$npar)
      data[,union('parcelid',input$cols)]
    })
        
  }
)
