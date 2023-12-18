## Please open via .Rproj

library(shiny)
library(shinydashboard)
library(leaflet)
library(htmlwidgets)
library(sf)


##mainPanel(tabsetPanel())
#location <- read.csv("https://risk.spc.int/geoserver/ows?service=WFS&version=1.0.0&request=GetFeature&typename=geonode%3Atl_bldexp_aggregates&outputFormat=csv&srs=EPSG%3A4326")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Top", tabName = "top", icon = icon("home")),
    menuItem("General Description", tabName = "overview", icon = icon("book"),
             startExpanded = FALSE,
             menuSubItem("Location & Key Facts", tabName = "map"),
             menuSubItem("Brief", tabName = "brief")),
    menuItem("Key Demographics", tabName = "demo", icon = icon("person")),
    menuItem("Regional Comparison", tabName = "comparison", icon = icon("signal")),
    menuItem("SWOT", tabName = "swot", icon = icon("play"), startExpanded = FALSE,
             menuSubItem("Strength", tabName = "stg"),
             menuSubItem("Weakness", tabName = "wkns"),
             menuSubItem("Opportunities", tabName = "oppr"),
             menuSubItem("Threats", tabName = "thrts")),
    menuItem("Citations & References", tabName = "ref", icon = icon("circle-info"))
  )
  #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    #label = "Search")
)

body <- dashboardBody(
  # Top Page
  tabItems(
    tabItem(tabName = "top",
            tags$div(
              style = "background-image: url('https://blog.gale.com/wp-content/uploads/2022/05/iStock-489482040.jpg');
            height: 400px; background-size: cover;",
              h1("top", align = "center", color = "grey90")
            )),
    # General Description: Location
    tabItem(tabName = "map", 
            fluidRow(
              tabBox(title = "Maps", id = "tabloc", height = "300px", width = 12, 
                     tabPanel("World Location", leafletOutput("WorldLoc", height = "300px")),
                     tabPanel("Island (Slow)", leafletOutput("Island", height = "300px"))
              )
            ),
            fluidRow(
              div(
                HTML("<strong>Capital:</strong> Dili<br>
                <strong>Area:</strong> 14,874 sq km<br>
                <strong>Population:</strong> 1.3 million<br>
                <strong>Languages:</strong> Portuguese, Tetum, plus others<br>
                <strong>Life expectancy:</strong> 66 years (men), 70 years (women)"),
                style = "text-align: center; font-size: 16px; margin-top: 20px;"
              )
            )
    ),
    tabItem(tabName = "brief",
            tags$div(
              style = "position: relative; text-align: center;",  
              tags$img(src = 'https://encrypted-tbn0.gstatic.com/licensed-image?q=tbn:ANd9GcT7qXiP2MXYMgtt_WqpHHVLobwnFcDh3Bz6xF2EV6nTqsfNyiODDfogtThij-_7zNbIkcbY7QcA76sP_PPDYjIl6pyUzcKRKJ7lZPJNcQ',
                       style = "height: 200px; width: 100%; object-fit: cover; display: block"),
              h1("Introduction", style = "position: absolute; top: 50%; left: 50%; font-family: Georgia, serif; 
                                          transform: translate(-50%, -50%); color: seashell;")),),
    
    tabItem(tabName = "demo", h1("Key Demographic Info")),
    
    # tabItem 3
    tabItem(tabName = "comparison", h1("Some graphs"),
            box(
              title = "Histogram", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("plot3", height = 250)),
            box(
              title = "Inputs", solidHeader = TRUE,
              "Box content here", br(), "More box content",
              sliderInput("slider", "Slider input:", 1, 100, 50),
              textInput("text", "Text input:")),
            fluidRow(
              tabBox(
                title = "First tabBox",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                tabPanel("Tab1", "First tab content"),
                tabPanel("Tab2", "Tab content 2")
              ),
              tabBox(
                side = "right", height = "250px",
                selected = "Tab3",
                tabPanel("Tab1", "Tab content 1"),
                tabPanel("Tab2", "Tab content 2"),
                tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
              )
            ),
            fluidRow(
              tabBox(
                # Title can include an icon
                title = tagList(shiny::icon("gear"), "tabBox status"),
                tabPanel("Tab1",
                         "Currently selected tab from first box:",
                         verbatimTextOutput("tabset1Selected")
                ),
                tabPanel("Tab2", "Tab content 2")
              )
            )),
    # tabItem 4
    tabItem(tabName = "swot"),
    tabItem(tabName = "stg",  h3("Strength"),
            fluidRow(
              # A static infoBox
              infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
              # Dynamic infoBoxes
              infoBoxOutput("progressBox"),
              infoBoxOutput("approvalBox")
            )),
    tabItem(tabName = "wkns", h3("Weakness")),
    tabItem(tabName = "oppr", h3("Opportunities")),
    tabItem(tabName = "thrts", h3("Threats")),
    tabItem(tabName = "ref", h3("Citations"),
            a("Timor-Leste (East Timor): Overview. University of Illinois LibGuides", 
              href = "https://guides.library.illinois.edu/timor-leste", target = "_blank"),
            br(),
            a("East Timor country profile. BBC News", href = "https://www.bbc.com/news/world-asia-pacific-14919009", target = "_blank"),
            
            br(),
            h3("References")
            )
)
)
# # Define UI for application that draws a histogram

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "MA615 Final"),
  sidebar,
  body
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  #input$searchText <- textInput("id", "Search")
  output$WorldLoc <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 125.727539, lat = -8.874217, zoom = 4)  # 以东帝汶为中心的地图
  })
  tls_nation <- st_read("TLS_adm0.shp")
  tls_provin <- st_read("TLS_adm1.shp")
  tls_adm2 <- st_read("TLS_adm2.shp")
  tls_adm3 <- st_read("TLS_adm3.shp")
  output$Island <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = tls_nation, fillColor = "blue", weight = 1, smoothFactor = 0.5, opacity = 0.8, fillOpacity = 0) %>%
      addPolygons(data = tls_provin, fillColor = "red", weight = 1, smoothFactor = 0.5, opacity = 0.8, fillOpacity = 0) %>%
      addPolygons(data = tls_adm2, fillColor = "yellow", weight = 1, smoothFactor = 0.5, opacity = 0.8, fillOpacity = 0.1) %>%
      addPolygons(data = tls_adm3, fillColor = "green", weight = 1, smoothFactor = 0.5, opacity = 0.8, fillOpacity = 0)
  })
  # output$progressBox <- renderInfoBox({
  #   infoBox(
  #     "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
  #     color = "purple"
  #   )
  # })
  # output$approvalBox <- renderInfoBox({
  #   infoBox(
  #     "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
  #     color = "yellow"
  #   )
  # })
}

# Run the application
shinyApp(ui = ui, server = server)


# #runApp("")


## app.R ##



