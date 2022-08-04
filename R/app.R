library(shiny)
library(leaflet)
library(DT)
library(tidyverse)
sfc <- readRDS("C:/Users/matthew.grainger/Documents/Projects_in_development/WaterLevelFlux/sfc.RDS")
shiny::shinyApp(
  ui = fluidPage(
    column(
      width = 9,
      fluidRow(
        column(
          width = 12,
          solidHeader = TRUE,
          leafletOutput(
            "my_leaflet"
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          solidHeader = TRUE,
          DTOutput(
            "my_datatable"
          )
        )
      )
    )
  ),
  
  server = function(session, input, output) {
    
    output$my_leaflet <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles(
          provider = providers$CartoDB.Positron,
          options = providerTileOptions(
            noWrap = FALSE
          )
        ) %>% 
        setView(
          lat = 63,
          lng = 9,
          zoom = 4
        ) %>% 
        addPolygons(data =st_zm(sfc),
                    layerId =~ vatnLnr, 
                    popup=paste("ID: ", sfc$vatnLnr, "<br>",
                                "navn: ", sfc$navn, "<br>"))

    })
    click_lake <- reactiveVal()
    
    observeEvent(input$my_leaflet_shape_click, {
      # Capture the info of the clicked polygon
      if(!is.null(click_lake()) && click_lake() == input$my_leaflet_shape_click$id)
        click_lake(NULL)     # Reset filter
      else
        click_lake(input$my_leaflet_shape_click$id)
    })
    
    # Parcels data table
    output$my_datatable <- DT::renderDataTable({
      DT::datatable( 
        if(is.null(click_lake())) 
          sfc    # Not filtered
        else 
          sfc %>% filter(vatnLnr==click_lake())
      )
    })
    
    }
  )
    
