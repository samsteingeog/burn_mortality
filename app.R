
# load packages
library(leaflet)
library(shiny)
library(shinythemes)
library(tidyverse)
library(raster)


### Run these files first
source("regression_model3.R", local = TRUE) # gives us dataframes: fires_weather, ave_data, dbr_data
source("CroppingTifs.R", local = TRUE) # gives us: fire_polygons, live_biomassList, dead_biomassList, veg_typeList, and tree_densityList

# Run these files to get data frames
# Maybe we should turn them into .R files instead?
# rmarkdown::render('./ClimateStations_RAWS.Rmd') # gives us weather dataframe ("fires_weather")
# rmarkdown::render('./FireDBR_calcs.Rmd') # gives us dbr dataframe ("dbr_data")
# rmarkdown::render('./Cropping_biomass&vegtype&treedens.Rmd') # gives us list of dead & live biomass rasters ("live_biomassList" & "dead_biomassList") and vegtype rasters ("veg_typeList"), and tree density rasters ("tree_densityList")
# rmarkdown::render('./regression_model2.Rmd') # gives us dataframes: fires_weather, ave_data, dbr_data


# data prep
# download all dbr tif files 
path = 'data/BurnSev_110Fires/'
list_files <- list.files(path, pattern = '.tif')

# create raster for each .tif file
rasterList <- list()
for (i in 1:length(list_files)) {
  rasterList[[i]] <- raster(paste0(path, list_files[i]))
}

# get objectIDs
OBJECTID <- as.numeric(substr(list_files, 1, 5))

# name rasters
names(rasterList) <- OBJECTID

# get centroid of fires
fires <- fire_polygons %>%
  mutate(centroids = st_centroid(geometry))
fires_points <- fires$centroids
fires_points <- st_transform(fires_points, 4326)
fires_points <- as.data.frame(st_coordinates(fires_points))
fires_points <- fires_points %>% mutate(OBJECTID = as.character(fires$OBJECTID))

ave_data2 <- ave_data %>%
  mutate(OBJECTID = as.character(OBJECTID)) %>%
  full_join(fires_points, by = 'OBJECTID') %>%
  mutate(startdate = format(as.Date(ALARM_DATE), format = "%B %d")) %>%
  mutate(enddate = format(as.Date(CONT_DATE), format = "%B %d")) %>%
  filter(!is.na(DEM_MEAN)) %>%
  filter(!is.na(dbr_means)) %>%
  filter(!is.na(wind_speed)) %>%
  filter(!is.na(wind_max)) %>%
  distinct(OBJECTID, .keep_all = TRUE)


# Make fire icon for leaflet map

fireIcon <-makeIcon(
  "./graphics/icons8-fire-48.png",
  iconWidth = 28, iconHeight = 28
)


# User Interface

ui <- fluidPage (
  navbarPage(id="DS421", "DS421 Project",
  #titlePanel("California Wildfires 2017"), #make better title later
  theme = shinythemes::shinytheme('simplex'),
  tabPanel("About",
           fluidRow(column(12, img(src = "WildFireImage.png", height = 500, width = 900))),
           fluidRow(column(12, h1("Exploring the relationship between environmental variables and burn intensity in California wildfires"))),
           fluidRow(column(12, p("There has been an increase in wildfires in California in the last decade.  This increase coincided with the death of 129 million trees due to drought and bark beetles. The factors that influence burn severity in wildfires are complex. The goal of this project is to further explore the drivers of burn severity in California wildfires, including presence of dead biomass. We sampled 102 wildfires that occurred in California in 2017."))),
           fluidRow(column(12, 
                           p("Our team worked with CalFire’s Fire Research and Assessment Program to review our methodology and identify data sources for environmental variables. For more information about land-owner assistance programs, visit:"))),
           fluidRow(column(12,
                           a("Cal Fire Land Owner Assistance Program", href = "https://www.fire.ca.gov/programs/resource-management/resource-protection-improvement/landowner-assistance"))),
           fluidRow(column(12,
                           a("USDA Disaster Recovery Assistance Program", href = "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/programs/?cid=NRCSEPRD1361073"))),
  #    fluidRow(column(4, h4("Summary of Fires"), p("We sampled 102 wildfires that occurred in California in 2017. Here is the average distribution of average burn severities of the fires we selected."), plotOutput('piechart')))
  ),
  
  tabPanel("Fire Data",
    fluidRow(
      column(6, h4("Burn Severity for Selected Fire"), leaflet::leafletOutput('dbr_map')),
      column(6, h4("California Wildfires in 2017"), leaflet::leafletOutput('map', width = '100%')) # add CA map
    ),
    fluidRow(
      column(3, h5("Live biomass"), leaflet::leafletOutput('livebiomass_map')), # live biomass map
      column(3, h5("Dead biomass"), leaflet::leafletOutput('deadbiomass_map')), # dead biomass map
      column(3, h5("Tree density"), leaflet::leafletOutput('treedensity_map')), # tree density map
      column(3, h5("Land cover type"), leaflet::leafletOutput('vegtype_map'))
    ),
    fluidRow(
      column(12, h5("Weather parameters"), tableOutput(outputId = "table")) # weather table
    )
  ),
    
 
  tabPanel("Model Results",
          sidebarLayout(
            sidebarPanel(
              verticalLayout(
                selectInput('models', 'Pick a Model to See', choices = c('Support Vector Machine', 'Random Forest')),
                h5("Order of importance of each variable to final model:"),
                verbatimTextOutput('model_list'))),
            mainPanel(plotOutput("modelplot"))
           ))
  
  )
)



# Server

server <- function(input, output, session) {
  
  # box to tell user to click on a fire
  observeEvent(input$DS421, {
    if (input$DS421 == "Fire Data") {
    showModal(modalDialog("Click on a fire in the map on the right to see its burn severity, live biomass, dead biomass, tree density, land cover type, and weather parameters. It may take a few seconds to load.", title = "Select a fire"))
    }
  })
 
  # create map of California with a marker for each fire
  output$map <- leaflet::renderLeaflet({
    leaflet() %>% 
      addTiles() %>% # Add default OpenStreetMap map tiles
      addProviderTiles("Esri.WorldImagery") %>%
      addMarkers(lat = ave_data2$Y, lng = ave_data2$X, 
                 layerId = ave_data2$OBJECTID, 
                 icon = fireIcon,
                 popup = paste(ave_data2$FIRE_NAME, "Fire", "<br>",
                               "Dates:", ave_data2$startdate, "-", ave_data2$enddate, "<br>",
                               "Fire Size:", round(ave_data2$GIS_ACRES), "acres", "<br>",
                               "Average Burn Severity:", ave_data2$mean_severityClass))
      })
  
  # pie chart output
  output$piechart <- renderPlot({
    ave_data2 %>%
    count(mean_severityClass) %>% 
    ggplot(aes(x = "", y = n, fill = mean_severityClass)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(values = c("darkgreen", "forestgreen", "limegreen", "yellow", "orange", "orangered", "purple")) +
    theme(legend.title = element_blank()) #+
    #labs(title = "Proportion of total fires by average burn severity")

  })
  
  # show image
    # fire_chosen <- reactive({input$fires})
    # output$dbr_plot <- renderPlot(rasterList$`fire_chosen`)
    # #renderImage()
    # output$dbr_map <- renderLeaflet({
    #   leaflet() %>% 
    #     addTiles %>% 
    #     addRasterImage(fire_chosen()) 
    # })
  
  # define legend inputs for DBR maps
  colorsDBR = c("darkgreen", "forestgreen", "limegreen", "yellow", "orange", "orangered", "purple")
  binsDBR = c(-1000, -251, -101, 99, 269, 439, 659, 2000)
  paletteDBR <- colorBin(palette = colorsDBR, bins = binsDBR, domain = binsDBR, na.color = "transparent")
  labelsDBR = c("Enhanced Regrowth, High", "Enhanced Regrowth, Low", "Unburned", "Low Severity", "Modereat-Low Severity", "Moderate-High Severity", "High Severity")

 
    observeEvent(input$map_marker_click, {
      # user clicks fire
      click <- input$map_marker_click
    
      # DBR map
      fire_chosen <- reactive({rasterList[[click$id]]})
      
      output$dbr_map <- renderLeaflet({
      leaflet() %>% 
        addTiles %>% 
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(fire_chosen(), colors = paletteDBR, opacity = 0.8) %>%
        addLegend(position = "bottomleft",
                  pal = paletteDBR,
                  values = binsDBR,
                  opacity = 0.7,
                  title = "dNBR Classes",
                  labFormat = function(type, cuts, p) {
                    paste0(labelsDBR)
                  }
        )
      })
      
      # Live Biomass Map
      # live biomass raster for selected fire
      livebiomass_fire <- reactive({live_biomassList[[click$id]]})
       
      # define legend inputs for live biomass maps
      domainLB <- cellStats(livebiomass_fire(), "range")
      paletteLB <- colorNumeric("Greens", domain = domainLB, na.color = "transparent")
  

      output$livebiomass_map <- renderLeaflet({
      leaflet() %>% 
        addTiles %>% 
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(livebiomass_fire(), colors = paletteLB, opacity = 1) %>%
        addLegend(position = "bottomleft",
                  pal = paletteLB,
                  values = domainLB, 
                  opacity = 0.7,
                  title = "live biomass (kg/ha)")
      })
      
      # Dead Biomass Map
      # dead biomass raster for selected fire
      deadbiomass_fire <- reactive({dead_biomassList[[click$id]]})

      # define legend inputs for live biomass maps
      domainDB <- cellStats(deadbiomass_fire(), "range")
      paletteDB <- colorNumeric("YlOrBr", domain = domainDB, na.color = "transparent")


      output$deadbiomass_map <- renderLeaflet({
      leaflet() %>%
        addTiles %>%
        addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(deadbiomass_fire(), colors = paletteDB, opacity = 1) %>%
        addLegend(position = "bottomleft",
                  pal = paletteDB,
                  values = domainDB,
                  opacity = 0.7,
                  title = "dead biomass (kg/ha)")
      })

      # Tree Density Map
      # tree density raster for selected fire
      treedensity_fire <- reactive({tree_densityList[[click$id]]})

      # define legend inputs for live biomass maps
      domainTD <- cellStats(treedensity_fire(), "range")
      #domainTD <- c(0, 11000)
      paletteTD <- colorNumeric("RdPu", domain = domainTD, na.color = "transparent")


      output$treedensity_map <- renderLeaflet({
      leaflet() %>%
      addTiles %>%
      addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(treedensity_fire(), colors = paletteTD, opacity = 1, maxBytes = Inf, project = FALSE) %>%
        addLegend(position = "bottomleft",
                  pal = paletteTD,
                  values = domainTD,
                  opacity = 0.7,
                  title = "tree density (#/ha)")
      })

      # Veg Type Map
      # veg type raster for selected fire
      vegtype_fire <- reactive({veg_typeList[[click$id]]})

      # define legend inputs for veg type maps
      colorsVT = c("darkgreen", "deeppink", "darkorchid", "gold", "red", "orange", "cyan", "green")
      levelsVT = c(1, 2, 3, 4, 5, 6, 7, 8)
      labelsVT = c("conifer", "shrub", "herbaceous", "barren-other", "urban", "hardwood", "water", "agricultural")
      paletteVT <- colorFactor(palette = colorsVT, levels = levelsVT, na.color = "transparent")
      
      output$vegtype_map <- renderLeaflet({
      leaflet() %>%
      addTiles %>%
      addProviderTiles("Esri.WorldImagery") %>%
        addRasterImage(vegtype_fire(), colors = paletteVT, opacity = 1, maxBytes = Inf, project = FALSE) %>%
        addLegend(position = "bottomleft",
                  pal = paletteVT,
                  values = levelsVT,
                  opacity = 0.7,
                  title = "land cover type",
                  labFormat = function(type, cuts, p) {
                    paste0(labelsVT)
                  }
        )
      })
      
      # Summary Table
      # create table for average weather data for each fire
        output$table <- renderTable({
          ave_data2 %>%
            filter(FIREID == click$id) %>%
            dplyr::select(month_temp, 
                          week_temp, 
                          month_precip, 
                          week_precip,
                          wind_speed, 
                          wind_max, 
                          month_humid, 
                          week_humid) %>%
            dplyr::rename(
              "Prior month avg temp (F)" = month_temp, 
              "Prior week mavg temp (F)" = week_temp, 
              "Prior month avg precip (in)" = month_precip, 
              "Prior week avg precip (in)" = week_precip, 
              "Avg wind speed during (mph)" = wind_speed, 
              "Avg max wind speed during (mph)" = wind_max, 
              "Prior month avg humidity (%)" = month_humid, 
              "Prior week avg humidity (%)" = week_humid)
              
          
        })

      })
    
    
    # Model plots
    output$modelplot <- renderPlot({
      if (input$models == 'Random Forest') {
      ggplot(all_df) + #doesn't seem to make much difference
        geom_point(aes(x = dbr, y = rf_dbr), alpha = 0.4) +
        geom_abline(slope = 1) +
        xlim(0,1000) +
        ylim(0,1000) +
        theme_minimal() +
        ggtitle("Random Forest - All Sample Points") +
        xlab("dBR") + 
        ylab("Predicted dBR")
      }
      
      #(input$models == 'Support Vector Machine')
      else  {
      ggplot(ave_df) +
        geom_point(aes(x = dbr_means, y = svm_dbr1_alt), alpha = 0.4) +
        geom_abline(slope = 1) +
        xlim(0,1000) +
        ylim(0,1000) +
        theme_minimal() +
        ggtitle("SVM Average Data (Biomass_Sum)") +
        xlab("Mean dBR") +
        ylab("Predicted Mean dBR")
      }
    })
    
    # Model order of importance texts
    output$model_list <- renderText({
      if (input$models == "Random Forest") {
"1. Elevation 
2. Slope 
3. Aspect 
4. Living biomass 
5. Temp (month prior) 
6. Humidity (month prior) 
7. Fire size 
8. Fire duration 
9. Humidity (week prior) 
10. Wind speed (average) 
11. Dead biomass 
12. Temp (week prior) 
13. Wind speed (max) 
14. Precip (month prior) 
15. Precip (week prior)"
      }
      else { 
         
"1. Temp (month prior) 
2. Humidity (week prior)
3. Wind speed (average)
4. Aspect
5. Slope
6. Conifer LC 
7. Humidity (month prior)
8. Living biomass
9. Fire duration
10. Dead biomass 
11. Precip (month prior) 
12. Wind speed (max) 
13. Elevation 
14. Urban LC 
15. Herbaceous LC 
16. Precip (week prior) 
17. Temp (week prior) 
18. Water LC 
19. Barren LC 
20. Tree Density 
21. Shrub LC 
22. Hardwood LC 
23. Agricultural LC 
24. Fire Size"
      }
        
      
    })
}




# Run Shiny

shinyApp(ui = ui, server = server)


