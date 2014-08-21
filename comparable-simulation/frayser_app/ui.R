library(shiny)
library(leaflet)

shinyUI(fluidPage(
  titlePanel("Hypothetical Rehabilitations in Frayser"),
  
  sidebarLayout(
    sidebarPanel(
#       helpText("Control Map."),
      
      h4("Total change in appraised value attributable to selling this parcel at this value: "),
      textOutput('totchange'),
      br(),
      
      selectInput('parofint',
                  label = "Select a tax-delinquent property",
                  choices = as.list(frayparcelsplot$parceladd)),
      br(),
      
      p('Current appraised value: '), textOutput('currentappraisal'),
      br(),
      
      numericInput('amount',
                   label = 'Input hypothetical selling price',
                   value = 50000,min = 0,step = 5000),
      br()
       ),
  
    mainPanel(
      leafletMap(
        "map", "100%", 500,
        initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#         initialTileLayer = "//server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(
          center = c(35.218, -89.994),
          zoom = 13,
          maxBounds = list(list(17, -180), list(59, 180))
        )
      ),
            
      br(),
      dataTableOutput('parstable')      
    )
  )
))
