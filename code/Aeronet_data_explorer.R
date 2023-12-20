
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Full version available:  https://aeronet.gsfc.nasa.gov/new_web/aeronet_map_tool/
#
# Author: Rell Credle
# +++++++++++++++++++++++++++++++++++++++++++++++++++++


# Load the packages using pacman
if (!require(pacman)) install.packages("pacman")
pacman::p_load(here,
               readr,
               magrittr,
               leaflet,
               shiny,
               htmltools,
               htmlwidgets,
               ggplot2,
               ggthemes,
               bslib,
               dplyr,
               update = FALSE,)

source(here("functions", "download_ggplot.R"))

# Define the UI
ui <- 
  body <- fluidPage(
    # Title
    titlePanel("AERONET Data Explorer"),
    # Sidebar with input for date
    sidebarLayout(
      sidebarPanel(
        # Date input
        dateInput("date", "Select a date:", value = Sys.Date()),
        #download plots
        actionButton("download", "Download Plots"),
        fluidRow(
          
          
          #Output the selected marker ggplot
          fillCol(plotOutput("dailygraph")) 
        )
        
      ),
      # Main panel with output for map and table
      mainPanel(
        # Map output
        leafletOutput("map"),
        # Table output
        tableOutput("table"),
        fluidRow(
          # Output current Mean, SD, Active Obs. Summaries
          column(6, DT::dataTableOutput("summary_table")),
          # Output the distribution of AOD_500nm readings among all sites 
          column(6, plotOutput("allsitegraph")),
        )
        
      )
    )
  )
body <- append(
  body,
  bootstrapPage(
    div(
      class = "outer",
      tags$style(
        type = "text/css",
        ".outer {position: fixed; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"
      ),
      # Map output
      leafletOutput("map", width = "100%", height = "100%")
    )
  )
)


# Define server for shiny
server <- function(input, output) {
  # Set aeronet api url
  aeronet_api_url <- reactive({
    # Construct the API url with the date and other arguments
    paste0("https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_v3?year=", 
           format(input$date, "%Y"), 
           "&month=", 
           format(input$date, "%m"), 
           "&day=", 
           format(input$date, "%d"), 
           "&AOD15=1&AVG=20&if_no_html=1")
  })
  
  aeronet_api_url_30 <- reactive({
    new_date <-input$date -30
    
    # build api link for new date to set date
    # Construct the API url that grabs 30 day range of site with the date and other arguments
    # https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_v3?year=2020&month=6&day=1&year2=2023&month2=6&day2=14&AOD15=1&AVG=10&site=Tahiti_MF
    paste0("https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_v3?",
           "year=", 
           format(new_date, "%Y"), 
           "&month=", 
           format(new_date, "%m"), 
           "&day=", 
           format(new_date, "%d"),
           "&year2=",
           format(input$date, "%Y"), 
           "&month2=", 
           format(input$date, "%m"), 
           "&day2=", 
           format(input$date, "%d"),
           "&AOD15=1&AVG=20&if_no_html=1",
           "&site=",
           format(input$clicked_marker)) 
  })
  
  ## Set site list url
  
  site_list_url <- reactive({
    paste0("https://aeronet.gsfc.nasa.gov/Site_Lists_V3/aeronet_locations_v3_",
           format(input$date, "%Y"),
           "_lev15.txt")
  })
  
  
  # aeronet data table
  aeronet_data <- reactive({
    if(input$date)
    {
      #Fetch the data from the API using read.table function
      #Wrangling of data
      read.table(aeronet_api_url(), header = TRUE, sep = ",", na.strings = "-999", skip = 5) %>% 
        # clean columns
        dplyr::rename(date=Date.dd.mm.yyyy.,site=AERONET_Site,lng=Site_Longitude.Degrees.,lat=Site_Latitude.Degrees.) %>% 
        #remove unneeded headers
        dplyr::select(site,date,AOD_1640nm:AOD_709nm ) %>% 
        group_by(site) %>%
        #if move than one occurence is added due to GMT (12:00:00) then keep the lowest date
        filter(date == min(date)) %>%
        ungroup()
    }
  })
  
  # all site data table
  site_data <- reactive({
    # Fetch the data from the API using read.table function
    read.table(site_list_url(), header = TRUE, sep = ",", na.strings = "-999", skip = 1) %>% 
      dplyr::rename(site=Site_Name, long=Longitude.decimal_degrees., lat=Latitude.decimal_degrees.) %>% 
      dplyr::select(site:lat) %>% 
      left_join(aeronet_data(), by="site") %>%
      filter(AOD_500nm >= 0) %>%  
      dplyr::mutate(color = case_when(AOD_500nm < 0.2 ~ "blue",
                                      AOD_500nm < 0.4 ~ "green",
                                      AOD_500nm < 0.6 ~ "yellow",
                                      AOD_500nm < 0.8 ~ "orange",
                                      AOD_500nm < 1.0 ~ "red",
                                      AOD_500nm > 1.0 ~ "darkred",
                                      TRUE ~ 'grey')) %>% 
      mutate(color = factor(color, levels = c("blue", "green", "yellow", "orange", "red", "darkred"))) %>%
      arrange(color)
    
  })
  
  # summarized data table
  summarized.dataset <- reactive({
    site_data() %>% 
      # remove AOD values that are in the NA category
      filter(AOD_500nm >= 0) %>% 
      # Seperate into groups to be summarized
      group_by(color) %>%
      summarise(        
        sites_observed = n(), # all observations checked
        mean_500nm = round(mean(AOD_500nm, na.rm=TRUE),4), # Mean for aod within color range
        sd_500nm = round(sd(AOD_500nm, na.rm=TRUE),4) # standard deviation of mean
      )
  })
  
  ## Render data to output
  output$allsitegraph <- renderPlot(width =300, height=250, {
    ## Create graph of all sites
    site_data() %>% 
      # remove grey from batch (inactive sites)
      filter(AOD_500nm >= 0) %>%
      ggplot(
        aes(
          x=color,
        )
      ) +
      geom_bar(
        aes(fill = color),
        show.legend = FALSE,
        alpha=0.5
      ) +
      # Set custom headers given their group
      scale_x_discrete("Distribution of AOD Density",
                       label=c(
                         "< 0.2",
                         "< 0.4" ,
                         "< 0.6",
                         "< 0.8",
                         "< 1.0",
                         "> 1.0"
                       ))+
      # assign colors manually
      scale_fill_manual(values = c("blue", "green", "yellow", "orange", "red", "darkred"))+
      theme_classic()
    
  })  
  
  # Display the summary table using DT package
  output$summary_table <- DT::renderDataTable({
    # Create summary table for viewing information of daily chosen set
    DT::datatable(summarized.dataset(),
                  colnames = names(summarized.dataset()), # table column names to display
                  options = list(ordering = FALSE, # Disable sorting
                                 searching = FALSE, # Hide search box
                                 paging = FALSE, # Show all rows
                                 info = FALSE, # Hide info
                                 dom = "t")) # Show only table
  })
  
  # Listen even for when a marker is clicked 
  observeEvent(input$clicked_marker, {
    # get new date 30 days previous
    
    # 30 day average data for site clicked on
    aeronet_30_data <- read.table(aeronet_api_url_30(), header = TRUE, sep = ",", na.strings = "-999", skip = 5) %>% 
      # Remove duplicate rows based on name column
      ##unique(., by = "AERONET_Site_Name")
      dplyr::rename(date=Date.dd.mm.yyyy.,site=AERONET_Site,lng=Site_Longitude.Degrees.,lat=Site_Latitude.Degrees.) %>% 
      dplyr::select(site,date,AOD_1640nm:AOD_709nm ) %>% 
      ungroup()
    # Output to graph      
    aeronet.30.plot <-
      aeronet_30_data %>% 
      ## plot using x as the date (format date so it can later be readjusted for a cleaner look)
      ggplot(aes(x = as.Date(date, format = "%d:%m:%Y"), y= AOD_500nm)) +
      geom_line(
        show.legend = FALSE,
        alpha=0.5
      ) +
      xlab(paste0("30 Day Average for ", format(input$clicked_marker)))+
      scale_x_date(date_labels = "%m-%d")+
      scale_fill_manual(values = c("blue", "green", "yellow", "orange", "red", "darkred"))+
      theme_classic()
    output$dailygraph <- renderPlot({
      aeronet.30.plot
    })  
    
    
    
  })
  
  
  # recreates and download charts if download is selected
  observeEvent(input$download, {
    # regrab aeronet data
    aeronet_30_data <- read.table(aeronet_api_url_30(), header = TRUE, sep = ",", na.strings = "-999", skip = 5) %>% 
      # Remove duplicate rows based on name column
      ##unique(., by = "AERONET_Site_Name")
      dplyr::rename(date=Date.dd.mm.yyyy.,site=AERONET_Site,lng=Site_Longitude.Degrees.,lat=Site_Latitude.Degrees.) %>% 
      dplyr::select(site,date,AOD_1640nm:AOD_709nm ) %>% 
      ungroup()
    # assign 30 day avg plot
    aeronet.30.plot <-
      aeronet_30_data %>% 
      ## plot using x as the date (format date so it can later be readjusted for a cleaner look)
      ggplot(aes(x = as.Date(date, format = "%d:%m:%Y"), y= AOD_500nm)) +
      geom_line(
        show.legend = FALSE,
        alpha=0.5
      ) +
      xlab(paste0("30 Day Average for ", format(input$clicked_marker)))+
      scale_x_date(date_labels = "%m-%d")+
      scale_fill_manual(values = c("blue", "green", "yellow", "orange", "red", "darkred"))+
      theme_classic()
    # assign allsite plot
    allsite.mean.sd.data <-
      ## Create graph of all sites
      site_data() %>% 
      filter(AOD_500nm >= 0) %>%
      ggplot(
        aes(
          x=color,
        )
      ) +
      geom_bar(
        aes(fill = color),
        show.legend = FALSE,
        alpha=0.5
      ) +
      scale_x_discrete("Distribution of AOD Density",
                       label=c(
                         "< 0.2",
                         "< 0.4" ,
                         "< 0.6",
                         "< 0.8",
                         "< 1.0",
                         "> 1.0"
                       ))+
      scale_fill_manual(values = c("blue", "green", "yellow", "orange", "red", "darkred"))+
      
      theme_classic()
    
    # download to graph folder
    download_ggplot(aeronet.30.plot, "aeronet.30.plot")
    download_ggplot(allsite.mean.sd.data, "allsite.mean.sd.plot")
  })
  
  # Render the map output
  output$map <- renderLeaflet({
    # Create a leaflet map
    leaflet() %>%
      # Create legend for leaflet
      addLegend(position = "bottomright", title = "AOD_500nm",
                colors = c("darkred", "red","orange","yellow","green","blue","grey"),
                labels = c( "> 1","< 1", "< 0.8","< 0.6","< 0.4","< 0.2", "NA"),
                opacity = 0.8) %>% 
      
      
      # Add default OpenStreetMap map
      addTiles() %>%
      # Add markers for the aeronet sites
      addCircleMarkers(data = site_data(), color = ~color,
                       label = ~site,  lng = ~long, lat = ~lat, 
                       popup = ~paste0("AOD 500nm: ", as.character(AOD_500nm))) %>% 
      
      htmlwidgets::onRender("
        // Javascript
        async function clickMarker() {
          // Get the leaflet object
          var map = this;
            map.eachLayer(function(layer){
             if (layer instanceof L.CircleMarker) {
              // Add a click event listener to the marker
              layer.on('click', async function(e) {
                // Set label of the e object
                var label = e.target.options.label;

                // create graph when clicked_marker is sent to input
                Shiny.onInputChange('clicked_marker', label);
              });
            }
          }) 

        }

      ")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)