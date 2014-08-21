library(leaflet)

shinyUI(fluidPage(
  titlePanel("Distressed Properties in Memphis, TN"),
  
  sidebarLayout(
    sidebarPanel(
#       helpText("Control Map."),
      
      radioButtons("year", 
                   label = "Year:",
                   choices = c("2011", "2012", "2013"),
                   selected = "2013"),
      br(),
      sliderInput("npar", 
                  label = "Number of Parcels:",
                  min = 10, max = 10000, value = 1000, step = 25),
      br(),
      checkboxGroupInput("cols",
                         label = 'Parcel Information',
                         choices = list('Risk Score'='risk',
                                        'Address'='address',
                                        'Appraised Value'='rtotapr',
                                        'Land Use Code'='luc',
                                        'Year Built'='yrblt',
                                        'Land Area'='sfland',
                                        'Living Area'='sqft',
                                        'Number of Stories'='stories',
                                        'Number of Rooms'='rmtot',
                                        'Condition'='cdu',
                                        'Owner'='ownername',
                                        'Owner Address'='ownaddress'),
                         selected = c('risk','address')),
      br(),
      h4("What is a risk score?"),
      helpText("The risk score is an estimate of how likely a property is to be distressed.",
               "It is calculated by comparing properties in a given year to those in 2008,",
               "when CBANA organized a windshield survey of all properties in the city.",
               "The more similar a property is to those marked as distressed in 2008,",
               "the higher its risk score.",
               "Keep in mind that risk scores are merely estimates and do not represent",
               "an official evaluation of any specific property.",
               "Apply local knowledge before drawing conclusions.")
    ),
    
    mainPanel(
      leafletMap(
        "map", "100%", 500,
        initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#         initialTileLayer = "//server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(
          center = c(35.13,-89.93),
          zoom = 11,
          maxBounds = list(list(17, -180), list(59, 180))
        )
      ),
            
      br(),
      dataTableOutput('parstable')      
    )
  )
))
