library(DBI); library(dbplyr); library(dplyr); library(DT); library(stringr); library(lubridate); library(plotly); library(purrr);
library(odbc); library(rlang); library(shinyBS); library(shiny); library(shinydashboard); library(data.table); library(mapsapi);
library(maps); library(leaflet); library(scales); library(sf); library(googleway); library(owmr); library(glue);

key1 = "your_key_here"
weat_KEY <- "your_key_here"
set_key(key = key1)
owmr_settings(weat_KEY)
search_strings <- data.frame(Start = character(), End = character(), Directions = character(), Mode = character(),stringsAsFactors = FALSE)
con <- dbConnect(odbc::odbc(), dsn = "mysql", Database = "mywaypoints")

## Backend Google Maps API/Google weather API
  ## Create mysql database
    ## Push new query to Search tbl in SQL DB
    ## Push users to Users tbl in SQL DB
    ## Push waather along round along with date, time, distance to Weather tbl in SQL DB

ui <- dashboardPage(
        dashboardHeader(title = "My Way Points"),
        dashboardSidebar(
          sidebarMenu(
            id = "sidebar_menu",
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(
              tabName = "dashboard",
              column(
                width = 3,
                textInput("start_point", label = "Enter Starting Address:", placeholder = "Example: 123 Main St, City, State, 12345")
              ),
              column(
                width = 3,
                textInput("end_point", label = "Enter Final Destination:", placeholder = "Example: 123 Main St, City, State, 12345")
              ),
              column(
                width = 3,
                selectizeInput("travel_mode", "Select Travel Mode", multiple = TRUE, options = list(placeholder = "Select option below", maxItems = 1), choices = list(
                  "Car" = "driving",
                  "Public Transportation" = "transit",
                  "Walking" = "walking",
                  "Bicycle" = "bicycling"
                ))
              ),
              column(
                width = 3,
                h5("Click for Directions"),
                uiOutput("go_button")
              ),
              box(
                title = "Map", collapsible = FALSE, status = "primary", width = 12, solidHeader = TRUE,
                google_mapOutput("mymap")
              )
          )
      )
    )
)

server <- function(input, output) {
  
  output$go_button <- renderUI({
    if ((input$start_point != "") && (input$end_point != "") && !(is.null(input$travel_mode)))  { actionButton("launch", "Get Directions") }
  })

  output$mymap <- renderGoogle_map({
    google_map(location = c(42.8864, -78.8784), zoom = 10, height = 7) %>%
      add_traffic()
  })

  observeEvent(input$launch, {
    browser()
    
    google_map_update(map_id = "mymap") %>% 
      clear_polylines() %>%
      clear_markers()
    
    temp_tbl <- tbl(con, "search_strings") %>% as.data.table()
    flag <- FALSE
    
    gf <- gsub('[0-9]+', '', input$start_point); gf <- gsub(' ', '', gf);
    gd <- gsub('[0-9]+', '', input$end_point); gd <- gsub(' ', '', gd);
    
    if (length(temp_tbl$Start >= 1)) {
      for (i in 1:length(temp_tbl$Start)) {
        if ( (gf == temp_tbl$Start[i]) && (gd == temp_tbl$End[i]) ) {
          df_routes <- data.frame(polyline = temp_tbl$Directions[i], stringsAsFactors = FALSE)
          flag <- TRUE
          break
        }
      }
    }
    
    if (flag == FALSE) {
      doc <- google_directions(
        origin = input$start_point,
        destination = input$end_point,
        alternatives = FALSE,
        mode = input$travel_mode,
        simplify = TRUE)
    
      polylinez <- direction_polyline(doc)
      df_routes <- data.frame(polyline = direction_polyline(doc), stringsAsFactors = FALSE)
      gg <- gsub('[0-9]+', '', input$start_point); gg <- gsub(' ', '', gg);
      gh <- gsub('[0-9]+', '', input$end_point); gh <- gsub(' ', '', gh);
    
      search_strings[nrow(search_strings) + 1, ] <<- list(gg, gh, polylinez, input$travel_mode)
      row.names(search_strings) <<- 1:nrow(search_strings)
      DBI::dbWriteTable(con, "search_strings", value = search_strings[nrow(search_strings),], append = TRUE)
      search_strings <<- search_strings[0, ]
    }
    
    jj <- df_routes$polyline[1]
    df_cord <- decode_pl(jj) %>% as.data.frame()
    rando2 <- c(1, nrow(df_cord))
    start_end <- df_cord[rando2,]
    row.names(start_end) <- 1:nrow(start_end)
    rando <- sample(1:length(df_cord$lat), 2, replace = F)
    df_cord <- df_cord[rando,]
    row.names(df_cord) <- 1:nrow(df_cord)
    
    blah1 <- get_current(lat = df_cord$lat[1], lon = df_cord$lon[1], cnt = 1, units = "imperial") %>% tidy_up_() %>% as.data.frame()
    blah2 <- get_current(lat = df_cord$lat[2], lon = df_cord$lon[2], cnt = 1, units = "imperial") %>% tidy_up_() %>% as.data.frame()
    
    start_end$info_win[1] <- glue("<b>Start:</b> {input$start_point}") 
    start_end$info_win[2] <- glue("<b>End:</b> {input$end_point}") 
    
    df_cord$info_win[1] <- glue("<b>Coordinate Pair:</b> {df_cord$lat[1]}, {df_cord$lon[1]}</br>",
                                "<b>Lowest Temp:</b> {blah1$main.temp_min[1]} °F</br>",
                                "<b>Highest Temp:</b> {blah1$main.temp_max[1]}°F</br>",
                                "<b>Current Temp:</b> {blah1$main.temp[1]} °F</br>",
                                "<b>Current Weather:</b> {blah1$weather.main[1]}")
    df_cord$info_win[2] <- glue("<b>Coordinate Pair:</b> {df_cord$lat[2]}, {df_cord$lon[2]}</br>",
                                "<b>Lowest Temp:</b> {blah2$main.temp_min[1]} °F</br>",
                                "<b>Highest Temp:</b> {blah2$main.temp_max[1]} °F</br>",
                                "<b>Current Temp:</b> {blah2$main.temp[1]} °F</br>",
                                "<b>Current Weather:</b> {blah2$weather.main[1]}")
    
    google_map_update(map_id = "mymap") %>% 
      add_polylines(data = df_routes, polyline = "polyline") %>%
      add_markers(data = start_end, info_window = "info_win") %>%
      add_markers(data = df_cord, info_window = "info_win") 
    
  }, ignoreInit = TRUE)
  
}

shinyApp(ui, server)
