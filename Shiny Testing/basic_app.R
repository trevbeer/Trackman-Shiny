#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)


game <- read.csv("allGames.csv")
game <- game %>% subset(TaggedPitchType != "Other")
game <- game %>% subset(TaggedPitchType != "Knuckleball")
# just ucla guys, not really necessary line
game <- game %>% subset(PitcherTeam == "UCLA")
# game$Pitcher <- gsub("(.*),.*", "\\1", game$Pitcher)
# game$Pitcher <- str_glue(str_split(game$Pitcher, ", ")[[1]][2], str_split(game$Pitcher, ", ")[[1]][1], .sep = " ")
for (i in 1:nrow(game)) {
  game$Pitcher[i] <- str_glue(strsplit(game$Pitcher[i], ', ')[[1]][2], strsplit(game$Pitcher[i], ', ')[[1]][1], .sep = ' ')
}
game$Date <- as.Date(game$Date, "%m/%d/%Y")
pitch_colors <- c('Fastball' = '#d22d49', 'Sinker' = '#fe9d00', 'Cutter' = '#933f2c', 'Curveball' = '#00d1ed',
                  'Slider' = '#c3bd0d', 'Sweeper' = '#CB9AF0', 'ChangeUp' = '#23be41', 'Splitter' = '#3bacac', 'Other' = '#Acafaf')

ui <- fluidPage(
  
  titlePanel("Simple Sabermetrics: Trackman Shiny Application"),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "PitcherInput", label = "Select Pitcher", choices = sort(unique(game$Pitcher))),
      dateRangeInput(inputId = "DateRangeInput", label = "Select Date Range", start = min(game$Date), end = max(game$Date)),
      img(src = "bruinlogo.png", style = "display: block; margin-left: auto; margin-right: auto;", height = 150, width = 150)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pitch Usage - Bar Chart", br(), plotOutput("barchart")),
        tabPanel("Pitch Velocity - Box Plot", br(), plotOutput("boxplot")),
        tabPanel("Pitch Velocity Trend - Line Plot", br(), plotOutput("lineplot"))
      )
    )
  )
)


server <- function(input, output) {
  
  output$barchart <- renderPlot({
    dataFilter <- reactive({
      game %>% 
        filter(Pitcher == input$PitcherInput,
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
        group_by(TaggedPitchType) %>%
        dplyr::summarize('count' = n())
    })
    ggplot(dataFilter(), aes(x = reorder(TaggedPitchType, -count), y = count, fill = TaggedPitchType)) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = pitch_colors) +
      labs(x = "Pitch Type", y = "Count", title = "Pitch Usage") +
      theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
      theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
  }, width = 850, height = 450)
  
  
  output$boxplot <- renderPlot({
    dataFilter <- reactive({
      game %>%
        filter(Pitcher == input$PitcherInput,
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2]))
    })
    ggplot(dataFilter(), aes(x = reorder(TaggedPitchType, -RelSpeed), y = RelSpeed, fill = TaggedPitchType)) +
      geom_boxplot(width = 0.5) + 
      scale_fill_manual(values = pitch_colors) +
      labs(x = "Pitch Type", y = "Velocity (MPH)", title = "Distribution of Pitch Velocity") +
      theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
      theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
  }, width = 850, height = 450)
  
  
  output$lineplot <- renderPlot({
    dataFilter <- reactive({
      game %>%
        filter(Pitcher == input$PitcherInput,
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2]),
               TaggedPitchType == "Fastball") %>%
        group_by(Pitcher, TaggedPitchType, Date) %>%
        dplyr::summarize('mean_release_speed' = mean(RelSpeed, na.rm = TRUE))
    })
    ggplot(dataFilter(), aes(x = Date, y = mean_release_speed, group = Pitcher, color = Pitcher)) + 
      geom_line(size = 1) + geom_point(size = 3) +
      labs(x = "Game Date", y = "Velocity (MPH)", title = "Average FB Velocity by Game", color = "") +
      theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
      theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
  }, width = 850, height = 450)
  
}

shinyApp(ui = ui, server = server)