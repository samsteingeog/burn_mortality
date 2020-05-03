
# load packages
library(leaflet)
library(shiny)
library(shinythemes)
library(tidyverse)
library(raster)


### Run these files first
source("regression_model3.R", local = TRUE) # gives us dataframes: fires_weather, ave_data, dbr_data

#--------------------------------------------------------
# DATA PREP
# DBR data: download all dbr tif files 
path = 'data/BurnSev_110Fires/'
list_files <- list.files(path, pattern = '.tif')

# create DBR raster for each .tif file
rasterList <- list()
for (i in 1:length(list_files)) {
  rasterList[[i]] <- raster(paste0(path, list_files[i]))
}

# get objectIDs
OBJECTID <- as.numeric(substr(list_files, 1, 5))

# name rasters
names(rasterList) <- OBJECTID

#~~~~~~
# Dead Biomass data: download all tif files 
path = 'data/DeadBiomassTifs/'
list_files <- list.files(path, pattern = '.tif')

# create DBR raster for each .tif file
dead_biomassList <- list()
for (i in 1:length(list_files)) {
  dead_biomassList[[i]] <- raster(paste0(path, list_files[i]))
}

# get objectIDs
OBJECTID <- as.numeric(substr(list_files, 1, 5))

# name rasters
names(dead_biomassList) <- OBJECTID

#~~~~~~
# Live Biomass data: download all tif files 
path = 'data/LivingBiomassTifs/'
list_files <- list.files(path, pattern = '.tif')

# create DBR raster for each .tif file
live_biomassList <- list()
for (i in 1:length(list_files)) {
  live_biomassList[[i]] <- raster(paste0(path, list_files[i]))
}

# get objectIDs
OBJECTID <- as.numeric(substr(list_files, 1, 5))

# name rasters
names(live_biomassList) <- OBJECTID

#~~~~~~
# Tree Dens data: download all tif files 
path = 'data/TreeDensTifs/'
list_files <- list.files(path, pattern = '.tif')

# create DBR raster for each .tif file
tree_densityList <- list()
for (i in 1:length(list_files)) {
  tree_densityList[[i]] <- raster(paste0(path, list_files[i]))
}

# get objectIDs
OBJECTID <- as.numeric(substr(list_files, 1, 5))

# name rasters
names(tree_densityList) <- OBJECTID

#~~~~~~
# Veg Type data: download all tif files 
path = 'data/VegTypeTifs/'
list_files <- list.files(path, pattern = '.tif')

# create DBR raster for each .tif file
veg_typeList <- list()
for (i in 1:length(list_files)) {
  veg_typeList[[i]] <- raster(paste0(path, list_files[i]))
}

# get objectIDs
OBJECTID <- as.numeric(substr(list_files, 1, 5))

# name rasters
names(veg_typeList) <- OBJECTID

# get centroid of fires
# fires <- fire_polygons %>%
#   mutate(centroids = st_centroid(geometry))
# fires_points <- fires$centroids
# fires_points <- st_transform(fires_points, 4326)
# fires_points <- as.data.frame(st_coordinates(fires_points))
# fires_points <- fires_points %>% mutate(OBJECTID = as.character(fires$OBJECTID))

# read in centroids of fires, and lat&long
fires_points <- read_csv("data/fires_points.csv")
fires_points <- fires_points %>%
  mutate(OBJECTID = as.character(OBJECTID))

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

#--------------------------------------------------------
# User Interface

ui <- fluidPage (
  navbarPage(id="DS421", "DS421 Project",
  #titlePanel("California Wildfires 2017"), #make better title later
  theme = shinythemes::shinytheme('simplex'),
  tabPanel("About",
           fluidRow(column(12, img(src = "WildFireImage.png", height = 500, width = 900))),
           fluidRow(column(12, h1("Exploring the relationship between environmental variables and burn intensity in California wildfires"))),
           fluidRow(column(12, p("There has been an increase in wildfires in California over the past decade, coinciding with the death of 129 million trees due to drought and bark beetles. Wildfires cause varying degrees of damage to ecosystems, but the factors that influence burn severity in wildfires are complex."))),
           fluidRow(column(12, 
                           p("The goal of this project is to further explore the drivers of burn severity in California wildfires, including presence of dead biomass due to tree death. This study is based on a sample of 102 wildfires (>300 acres) that occurred in California in 2017. Our team worked with CalFire’s Fire Research and Assessment Program to review our methodology and identify data sources for environmental variables."))),
           fluidRow(column(12, 
                           p("For more information about land-owner assistance programs, visit:"))),
           fluidRow(column(12,
                           a("Cal Fire Land Owner Assistance Program", href = "https://www.fire.ca.gov/programs/resource-management/resource-protection-improvement/landowner-assistance"))),
           fluidRow(column(12,
                           a("USDA Disaster Recovery Assistance Program", href = "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/programs/?cid=NRCSEPRD1361073"))),
  
           fluidRow(column(12, 
                           p(" "))),
           fluidRow(column(12, 
                           p("This project was produced as part of the Data Science for the 21st Century program at UC Berkeley. Questions can be directed to Sam Stein at samstein (@) berkeley (.) edu. Our team consisted of Sam Hing, Sam Stein, Yiyi He, and Arfa Aijazi. Our team would like to thank Mark Rosenberg from CalFire for his invaluable assistance with this project."))),
           fluidRow(column(12,
                           a("Repository with reproducible code", href = "https://github.com/samsteingeog/burn_mortality"))), 
           fluidRow(column(12,
                           a("Downloads for tiff files", href = "https://datadryad.org/stash/dataset/doi:10.6078/D16988"))), 
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


#--------------------------------------------------------
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
        ggplot(all_df) + 
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
          geom_point(aes(x = dbr_means, y = svm_dbr1), alpha = 0.5, size = 3) +
          geom_abline(slope = 1) +
          xlim(0,750) +
          ylim(0,750) +
          theme_minimal() +
          ggtitle("SVM Average Data (mean Biomass)") +
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
12. Wind speed (max) 
13. Temp (week prior) 
14. Precip (month prior) 
15. Precip (week prior)"
      }
      else { 
         
"1. Elevation 
2. Fire duration
3. Conifer Land Cover
4. Aspect
5. Temp (week prior)
6. Living Biomass
7. Temp (month prior)
8. Tree Density
9. Humidity (month prior)
10. Wind speed (average)
11. Herbaceous Land Cover 
12. Fire size
13. Dead Biomass 
14. Humidity (week prior)
15. Barren Land Cover
16. Wind Speed (max) 
17. Precip (month prior) 
18. Slope
19. Shrub Land Cover
20. Precip (month prior)"
      }
        
      
    })
}



#--------------------------------------------------------
# Run Shiny

shinyApp(ui = ui, server = server)


