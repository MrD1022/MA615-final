## Please open via .Rproj

library(shiny)
library(shinydashboard)
library(leaflet)
library(htmlwidgets)
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

# Data Cleaning
demo_tls <- read.csv("70-23 long version.csv")
columns <- c("SEX.Sex", "INDICATOR.Indicator", "UNIT_MULTIPLIER.Unit.multiplier",
             "UNIT_MEASURE.Unit.of.measure")
demo_tls <- demo_tls |> 
  select(-DATAFLOW, -REF_AREA.Geographic.area, -AGE.Current.age) |>
  select(where(~ any(!is.na(.)))) 
demo_tls[columns] <- lapply(demo_tls[columns], 
                            function(x) gsub(".*: ", "", x))

com_wdbk <- read_xlsx("wdbk.xlsx")
colnames(com_wdbk) <- gsub("\\s*\\[.*?\\]\\s*", "", colnames(com_wdbk))
com_wdbk <- com_wdbk |> 
  filter(!is.na(`GDP per capita growth (annual %)`))


##mainPanel(tabsetPanel())
#location <- read.csv("https://risk.spc.int/geoserver/ows?service=WFS&version=1.0.0&request=GetFeature&typename=geonode%3Atl_bldexp_aggregates&outputFormat=csv&srs=EPSG%3A4326")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Top", tabName = "top", icon = icon("home")),
    menuItem("General Description", tabName = "overview", icon = icon("book"),
             startExpanded = FALSE,
             menuSubItem("Maps & Key Facts", tabName = "map"),
             menuSubItem("Brief", tabName = "brief")),
    menuItem("Key Demographics", tabName = "demo", icon = icon("person")),
    menuItem("Regional Comparison", tabName = "comparison", icon = icon("signal")),
    menuItem("SWOT", tabName = "swot", icon = icon("play"), startExpanded = FALSE),
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
              h1("East Timor (Timor-Leste)", style = "position: relative; top: 60%; left: 50%; transform: translate(-50%, -50%); color: white;"),
              h2("Mingrui Du, MSSP 2023", 
                 style = "position: relative; top: 60%; left: 50%; transform: translate(-50%, -50%); color: white;"),
              #h3("Mingrui Du, MSSP 2023", style = "")
            )),
    # (complete) General Description: Location
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
                <strong>Government Type:</strong> semi-presidential republic<br>
                <strong>Area:</strong> 14,874 sq km<br>
                <strong>Population:</strong> 1,476,042 (2023 est.)<br>
                <strong>Languages:</strong> Portuguese, Tetum, plus others<br>
                <strong>Natural Resources:</strong> gold, petroleum, natural gas, manganese, marble"),
                style = "text-align: center; font-size: 16px; margin-top: 36px;"
              )
            )
    ),
    # (complete) Brief 
    tabItem(tabName = "brief",
            tags$div(
              style = "position: relative; text-align: center;",  
              tags$img(src = 'https://encrypted-tbn0.gstatic.com/licensed-image?q=tbn:ANd9GcT7qXiP2MXYMgtt_WqpHHVLobwnFcDh3Bz6xF2EV6nTqsfNyiODDfogtThij-_7zNbIkcbY7QcA76sP_PPDYjIl6pyUzcKRKJ7lZPJNcQ',
                       style = "height: 200px; width: 100%; object-fit: cover; display: block"),
              h1("Introduction", style = "position: absolute; top: 50%; left: 50%; font-family: Georgia, serif; 
                                          transform: translate(-50%, -50%); color: seashell;")),
            tags$div(HTML("<strong>Timor-Leste (East Timor)</strong> is a country located in Southeast Asia. 
                           <br>Having gained full independence in 2002, Timor-Leste is one of the youngest countries in the world. 
                           <br>The country was a Portuguese colony from the 1500s to 1975 and then fell under Indonesian control from 1975 to about 1999, when citizens voted for independence. 
                           <br>Timor-Leste is a country rich in history, languages, and culture.
                           <br>The area has a dry tropical climate and moderate rainfall. Hilly areas are covered with sandalwood. 
                           <br>Scrub and grass grow in the lowlands, together with coconut palms and eucalyptus trees. 
                           <br>There are hot springs and numerous mountain streams. 
                           <br>Wildlife includes the cuscus (a species of marsupial), monkeys, deer, civet cats, snakes, and crocodiles."),
              style = "text-align: center; font-size: 18px;"
            )),
    # (complete, can improve) Demographic
    tabItem(tabName = "demo",
            fluidRow(h1("Key Demographics"),
              box("Indicator Selection", width = 3,
                  selectInput("indica", "Indicator: ", choices = c("Number of births", "Child dependency ratio", 
                                                                   "Total dependency ratio", "Total fertility rate",
                                                                   "Life expectancy", "Youth population from 15 to 24",
                                                                   "Adolescent population (10-19)", "Adolescent population as proportion of total population (%)",
                                                                   "Population annual growth rate", "Total population", "Population under age 18",
                                                                   "Population under age 5", "Share of urban population")),
                  radioButtons("gender", "Gender: ", choices = c("Total", "Male", "Female"))),
              box(uiOutput("tlsdemoui"),
                  #plotOutput("tlsdemoplot"),
                  width = 9, 
                  )
            )),
    
    # (complete, can improve) Regional Comparison
    tabItem(tabName = "comparison",
            fluidRow(
              box(title = "Statistics", solidHeader = TRUE, width = 5,
                  tableOutput("testab")),
              box(title = "Plot", width = 7,  
                  plotOutput("complot", height = 300))
              ),
            fluidRow(
              column(6, 
                     sliderInput("slider", "Year: ", 2008, 2022, 15, sep = "")
              ),
              column(6, 
                     selectInput("comindi", "Indicators: ", choices = c(
                       "GDP per capita growth (annual %)", "GDP per capita (current US$)", 
                       "GDP growth (annual %)", "GDP (current US$)",
                       "Population density (people per sq. km of land area)", 
                       "Access to electricity (% of population)",
                       "Suicide mortality rate (per 100,000 population)", 
                       "Agricultural raw materials exports (% of merchandise exports)",
                       "Fuel imports (% of merchandise imports)", 
                       "International tourism, expenditures (% of total imports)")))
              )
            ),
    # (ing) SWOT
    tabItem(tabName = "swot",
            fluidRow(box(width = 12, title = "Strength", collapsible = TRUE, background = "maroon", style = "border: 2px solid maroon;",
                         p("The agriculture sector employs a significant portion of East Timor's active population. 
                         As of 2009, about 67,000 households were involved in coffee growing, 
                         with coffee being one of the country's largest exports, generating approximately $10 million annually. 
                         East Timor is also a notable producer of cinnamon and cocoa, ranking 40th in coffee, 6th in cinnamon, and 50th in cocoa production worldwide."),
                          br(),
                         p("East Timor possesses considerable natural wealth, particularly in oil and natural gas. The Timor-Leste Petroleum Fund, established in 2005, 
                           was valued at US$8.7 billion by 2011. This fund is a significant source of income for the government, supporting a large portion of its annual budget. 
                           The country is labeled by the International Monetary Fund as the 'most oil-dependent economy in the world,' 
                           highlighting the importance of this sector to its overall economic health."),
                         br(),
                         p("Timor-Leste is well positioned for community-based ecotourism, 
                           which has been written into the nation’s tourism strategic plan. 
                           The Nino Konis National Park (situated in the eastern part of the country) 
                           is a well protected area and considered as some of the last surviving zones of 
                           tropical lowland rainforest in the world with rich coastal environment. 
                           The national park accommodates bird-watching, diving, trekking and pre-historic archeological sites."))),
            fluidRow(box(width = 12, title = "Weakness", collapsible = TRUE, background = "aqua",
                         p("Timor-Leste has been moving forward with the regeneration of its economy and rebuilding key infrastructure, including telecommunications networks, that were destroyed during the years of civil unrest; fixed-line and fixed broadband penetration in Timor-Leste remains extremely low, mainly due to the limited fixed-line infrastructure and the proliferation of mobile connectivity; in an effort to boost e-government services; the number of subscribers through to 2026 is expected to develop steadily, though from a low base."),
                         br(),
                         p("Even before the pandemic, there were urgent human capital challenges; some 47% of children are stunted, for example, and many students had poor learning outcomes due to low levels of education service delivery. These challenges were compounded by the COVID-19 pandemic which disrupted education services, leading to school closures and learning losses affecting 45% of school children."),
                         br(),
                         p("While Timor-Leste has succeeded in saving the proceeds of its natural resource endowment, key remaining challenges include how to increase the productivity and effectiveness of government spending, and how to ensure the environment is preserved as an important economic and social resource for future generations."))),
            fluidRow(box(width = 12, title = "Opportunities", collapsible = TRUE, background = "teal",
                     p("East Timor is part of the Timor Leste–Indonesia–Australia Growth Triangle (TIA-GT). The initiative aims to support economic, social, and cultural development primarily by attracting investment, developing manufacturing industries, enhancing human capital, and overall building a stronger cooperative relationship between the three countries involved. The initiative also aimed to accelerate the accession of Timor-Leste into the Association of Southeast Asian Nations (ASEAN) and to fulfill goals set by Timor-Leste's Strategic Development Plan, such as increasing the nation's economic prosperity and stability."))),
            fluidRow(box(width = 12, title = "Threats", collapsible = TRUE, background = "olive",
                         p('“The first half of 2021 brought devastating floods and an outbreak of COVID-19 – which, until recently, the country had been so successful in suppressing. These twin disasters threaten to set back hard-won gains in poverty reduction and economic stability,” said Bernard Harborne, Country Representative for Timor-Leste. “As a long-term partner, the World Bank stands with the people of Timor-Leste. We are ready to provide short-term support and develop long-term solutions to protect and invest in people during and after the pandemic. This report provides a useful snapshot of the current economic situation and key insights into building a stronger health system"')))
            ),
    
    tabItem(tabName = "ref", h3("Citations"),
            a("Timor-Leste (East Timor): Overview. University of Illinois LibGuides", 
              href = "https://guides.library.illinois.edu/timor-leste", target = "_blank"),
            br(),
            a("East Timor country profile. BBC News", href = "https://www.bbc.com/news/world-asia-pacific-14919009", target = "_blank"),
            br(),
            a("East Timor | History, Independence, Flag, & Facts. Britannica", href = "https://www.britannica.com/place/East-Timor", target = "_blank"),
            br(),
            a("Timor-Leste - The World Factbook. Central Intelligence Agency", href = "https://www.cia.gov/the-world-factbook/countries/timor-leste/", target = "_blank"),
            br(),
            a("East Timor. Wikitravel", href = "https://wikitravel.org/en/East_Timor", target = "_blank"),
            br(),
            a("Economy of East Timor. Wikipedia", href = "https://en.wikipedia.org/wiki/Economy_of_East_Timor", target = "_blank"),
            br(),
            a("Timor-Leste Economy Grapples with Dual Impacts of Pandemic and Cyclone Seroja. World Bank",
              href = "https://www.worldbank.org/en/news/press-release/2021/06/10/timor-leste-economy-grapples-with-dual-impacts-of-pandemic-and-cyclone-seroja",
              target = "_blank"),
            
            br(),
            h3("Data & References"),
            br(),
            a("UNICEF Data Warehouse. UNICEF", href = "https://data.unicef.org/resources/data_explorer/unicef_f/?ag=UNICEF&df=GLOBAL_DATAFLOW&ver=1.0&dq=TLS..&startPeriod=1970&endPeriod=2023", target = "_blank"),
            br(),
            a("Spatial Data Download. DIVA-GIS", href = "https://www.diva-gis.org/gdata", target = "_blank"),
            br(),
            a("World Development Indicators. DataBank", href = "https://databank.worldbank.org/home", target = "_blank"),
            br(),
            a("ChatGPT. OpenAI", href = "https://chat.openai.com/", target = "_blank"),
            br(),
            h3("Special Thanks"),
            br(),
            p("Yuchen Huang")
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
  # Locations
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
  # Demo Option
  demopt <- reactive({
    filter(demo_tls, SEX.Sex == input$gender, INDICATOR.Indicator == input$indica)
  })
  output$tlsdemoui <- renderUI({
    if(nrow(demopt()) == 0) {
      h3("Data Unavailable")
    } else {
      plotOutput("tlsdemoplot")
    }
  })
  output$tlsdemoplot <- renderPlot({
    data <- demopt()
    unit <- unique(data$UNIT_MULTIPLIER.Unit.multiplier)
    ggplot(data, aes(x = TIME_PERIOD.Time.period, y = OBS_VALUE.Observation.Value)) +
      geom_line(color = "blue", size = 2) +
      labs( x = "Time Period", y = input$indica,
            caption = paste0("Data Source: UNICEF; Unit multiplier: ", unit)) +
      theme(axis.text.x = element_text(size = 5),
            axis.text.y = element_text(size = 5)) +
      theme_minimal()
  })
  # Comparison
  compari <- reactive({
    filt <- filter(com_wdbk, Time == input$slider)
    sele <- select(filt, Time, `Country Name`, all_of(input$comindi))
    sele
  })
  output$testab <- renderTable({
    compari()
    })
  output$complot <- renderPlot({
    data <- compari()
    names(data)[3] <- "Value"
    ggplot(data, aes_string(x = "`Country Name`", y = "Value")) +
      geom_point(data = data %>% 
                   mutate(Color = ifelse(`Country Name` == "Timor-Leste", "red", "skyblue1")), 
                 aes(color = Color), size = 2) +
      labs(x = "Country", y = "Value", caption = "Data Source: World Bank") +
      theme(axis.text.x = element_text(size = 5, angle = 45, vjust = 1, hjust = 1),
            axis.text.y = element_text(size = 5),
            legend.position = "none") +
      theme_minimal() +
      scale_color_identity()
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



