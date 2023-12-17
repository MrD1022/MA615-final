library(shiny)
library(shinydashboard)
##mainPanel(tabsetPanel())

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview"),
    menuItem("Maps", tabName = "map"),
    menuItem("Graph", tabName = "graph"),
    menuItem("Statistics", tabName = "stat")
  )
)

body <- dashboardBody(
  # Boxes need to be put in a row (or column)
  tabItems(
    #tabItem 1
    tabItem(tabName = "overview",
            tags$div(
            style = "background-image: url('https://blog.gale.com/wp-content/uploads/2022/05/iStock-489482040.jpg');
            height: 400px; background-size: cover;",
            h1("dis is overview page", align = "center", color = "grey90")
            )),
    # tabItem 2
    tabItem(tabName = "map", h2("here to view maps")),
    # tabItem 3
    tabItem(tabName = "graph", h1("What are graphs")),
    # tabItem 4
    tabItem(tabName = "stat", h6("now see some stats"), align = "center")
  )
)
# # Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "MA615 Final"),
  sidebar,
  body
)




# Define server logic required to draw a histogram
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins +1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application
shinyApp(ui = ui, server = server)


# #runApp("")


## app.R ##



