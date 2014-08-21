library(shiny)
library(ggmap)
library(maptools)
library(RColorBrewer)
library(leaflet)
library(gdata)

# setwd('<directory>')

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
    
    paridofint = reactive({
      q = fray[which(fray$parceladd == input$parofint),'parcelid']
      return(q)
    })    
    
    pardata <- reactive({
           m = get(fray[which(fray$parceladd == input$parofint),'dfname'])
           m[m$matchedparid == paridofint(), 'price'] = input$amount
           
           m$newpred = (m$price)
           m$newpred = m$newpred + mod$coefficients['sqft'] * (m$sqft.y - m$sqft.x)
           m$newpred = m$newpred + mod$coefficients['rmtot'] * (m$rmtot_a - m$rmtot_s)
           m$newpred = m$newpred + mod$coefficients['rmbed'] * (m$rmbed_a - m$rmbed_s)
           m$newpred = m$newpred + mod$coefficients['fixbath'] * (m$fixbath_a - m$fixbath_s)
           m$newpred = m$newpred + mod$coefficients['firesTRUE'] * (m$fires_a - m$fires_s)
           m$newpred = m$newpred - mod$coefficients['heat3'] * (m$heat_s == 3 & (m$heat_a ==2 | m$heat_a == 1))
           m$newpred = m$newpred - mod$coefficients['heat4'] * (m$heat_s == 4 & (m$heat_a ==2 | m$heat_a == 1))
           m$newpred = m$newpred + mod$coefficients['heat3'] * (m$heat_a == 3 & (m$heat_s ==2 | m$heat_s == 1))
           m$newpred = m$newpred + mod$coefficients['heat4'] * (m$heat_a == 4 & (m$heat_s ==2 | m$heat_s == 1))
           m$newpred = m$newpred + (mod$coefficients['heat3'] -mod$coefficients['heat4']) * (m$heat_a == 3 & m$heat_s ==4)
           m$newpred = m$newpred + (mod$coefficients['heat4'] -mod$coefficients['heat3']) * (m$heat_a == 4 & m$heat_s ==3)
           m$newpred = m$newpred + (mod$coefficients['poolTRUE']) * (m$pool_a - m$pool_s)
           m$newpred = (m$newpred)  
           
           with_agg = aggregate(m[m$with,c('rtotapr13','newpred')], by=list(m[m$with,'parcelid']),FUN = mean)
           names(with_agg) = c('parid','rtotapr13','newpred')
         
           wo_agg = aggregate(m[m$without,c('rtotapr13','newpred')], by=list(m[m$without,'parcelid']),FUN = mean)
           names(wo_agg) = c('parid','rtotapr13','newpred')
           
           compare = merge(with_agg, wo_agg, by = 'parid',suffix = c('_w','_wo'))
           compare = merge(compare, propdat[,c('parid','lon','lat')], by = 'parid')
           compare$dif = compare$newpred_w - compare$newpred_wo
           names(compare) = c('parcel_id','appraised_val','predicted_withsale','rtotapr13_wo','predicted_withoutsale','lon','lat','difference')
#            compare = rename.vars(compare, from = c('parid','rtotapr13_w','newpred_w','newpred_wo','dif'), 
#                                  to = c('parcel_id','appraised_val','predicted_withsale','predicted_withoutsale','difference'))
           
          return(compare)
      })
    
  totalchange = reactive({
    sum(subset(pardata(),T,'difference'),na.rm = T)
  })
  
  output$totchange = renderText(paste("$",format(totalchange(),big.mark = ',',justify = 'right'),sep =''))

  curapp = reactive({
    subset(saledata(),T,'rtotapr13')
  })
  output$currentappraisal = renderText(paste("$",format(curapp(),big.mark = ',',justify = 'right'),sep =''))
    
    saledata = reactive({
      frayparcelsplot[which(frayparcelsplot$parceladd == input$parofint),]
      })

    maxd = reactive({
      pardata()
      max(abs(subset(pardata(),T,'difference')),na.rm = T)
    })
  
    parcelsInBounds <- reactive({
      pardata()
     bounds = input$map_bounds
     latRng <- range(bounds$north, bounds$south)
     lngRng <- range(bounds$east, bounds$west)
      subset(pardata(),
             lat >= latRng[1] & lat <= latRng[2] &
               lon >= lngRng[1] & lon <= lngRng[2] 
             )
    })

     allsalesdata = reactive({
       frayparcelsplot
     })
    map <- createLeafletMap(session, 'map')
    
    observe({
      map$clearShapes()
      data = parcelsInBounds()
      colors = brewer.pal(11,'RdYlGn')
        
      map$addCircle(
        data$lat,
        data$lon,
        10,
        data$parcel_id,
        list(
          weight=5,
          fill=TRUE,
          color=colors[round(5*data$difference/maxd()+6)])
      )
      
#       allsales = allsalesdata()
#       map$addCircle(
#         allsales$lat,
#         allsales$lon,
#         15,
#         allsales$parcelid,
#         list(
#           weight = 5,
#           opacity = 1,
#           fillOpacity = 1,
#           fill = T,
#           color = 'black'))
      
     compdata = saledata()
      map$addCircle(
        compdata$lat,
        compdata$lon,
        15,
        compdata$parcelid,
        list(
          weight=10,
          opacity = 1,
          fillOpacity=1,
          fill=TRUE,
          color='blue')
      )

        
    })
    
    observe({
      event <- input$map_shape_click
      masterid = ''
      map$clearPopups()
      
      isolate({
        data = parcelsInBounds()
        pardat = data[(which(data$parcel_id==event$id)),]
        content = paste("Parcel ID: ", pardat$parcel_id, br(),'Current appraised value: ',paste("$",format(pardat$appraised_val,big.mark = ','),sep = ''),
                        br(),'Predicted change in appraised value: ',paste("$",format(round(pardat$difference),big.mark=','),sep = ''),sep = '')
        map$showPopup(event$lat,event$lng,content,event$id)
        

      })
    })

   
    output$parstable <- renderDataTable({
      data = parcelsInBounds()
      data[,c('parcel_id','appraised_val','predicted_withsale','predicted_withoutsale','difference')]
     })



  }
)
