library(shiny)
library(dplyr)
library(DT)
library(ggplot2)

game <- read.csv("cleanedGames.csv")
game$Date <- as.Date(game$Date)
game$TaggedPitchType <- factor(game$TaggedPitchType, levels = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter"))
# this line maybe needs work/analogous column created
# game$pitch_type <- factor(game$pitch_type, levels = c("FF", "SI", "FC", "CS", "CU", "KC", "SL", "CH", "FS"))

ui <- fluidPage(
  column(10, offset = 1,
         titlePanel("Trackman Shiny Application - Post-Game Report"),
         hr(style="border-color: black;"), 
         fluidRow(
           column(2, selectInput(inputId = "PitcherInput", label = "Select Pitcher", choices = sort(unique(game$Pitcher)))),
#           column(2, selectInput(inputId = "GameInput", label = "Select Game", choices = ""))
           column(2, dateRangeInput(inputId = "DateRangeInput", label = "Select Date Range", start = min(game$Date), end = max(game$Date)))
         ),
         hr(style="border-color: black;"),
         wellPanel(style = "background: white; border-color:black; border-width:2px",
                   fluidRow(
                     column(2, img(src = "bruinlogo.png", height = 150, width = 150), align = "center"), 
                     column(4, h2(strong(textOutput("selected_pitcher"))), hr(style="border-color: black;"), style = "padding-right:0px;"),
                     column(6, h2("Post-Game Report"), hr(style="border-color: black;"), h2(textOutput("selected_game")), align = "right", style = "padding-left:0px;")),
                   hr(style="border-color: black;"), 
                   fluidRow(
                     column(10, offset = 1, h3(strong("Pitcher Summary Table")), dataTableOutput("pitcher_summary_table"), align = "center")
                   ), br(), br(), 
                   fluidRow(
                     column(4, plotOutput("pitch_movement_plot"), align = "center"),
                     column(4, plotOutput("pitch_location_plot"), align = "center"),
                     column(4, plotOutput("pitch_velocity_plot"), align = "center")
                   ), br(), br(), br()
         ), br(),
         p(em("If the contents of this page appear distorted, please decrease your web browser zoom to 80% or 90%."), align = "center")
  )
)


server <- function(input, output, session) {
  
 observeEvent(input$PitcherInput, 
              updateSelectInput(session, inputId = "DateRangeInput", label = "Select Date Range", 
                                choices = sort(unique(game$Date[game$Pitcher == input$PitcherInput]))))
  
  output$selected_pitcher <- renderText({paste(input$PitcherInput)})
  
  output$selected_game <- renderText({paste(input$DateRangeInput)})
  
  output$pitcher_summary_table <- renderDataTable({
    table <- game %>%
      filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%# Date == input$GameInput) %>%
      group_by('Pitch' = TaggedPitchType) %>%
      dplyr::summarize('No.' = n(),
                'Max Velo (MPH)' = round(max(RelSpeed, na.rm = TRUE),1),
                'Avg. Velo (MPH)' = round(mean(RelSpeed, na.rm = TRUE),1),
                'Avg. Spin (RPM)' = round(mean(SpinRate, na.rm = TRUE),0),
                'Tilt' = AveTilt[1],
                'RelHeight' = round(mean(RelHeight, na.rm = TRUE), 2),
                'RelSide' = round(mean(RelSide, na.rm = TRUE), 2),
                'Extension' = round(mean(Extension, na.rm = TRUE), 2),
                'VB' = round(mean(InducedVertBreak, na.rm = TRUE), 2),
                'HB' = round(mean(HorzBreak, na.rm = TRUE), 2),
                'VAA' = round(mean(VertApprAngle, na.rm = TRUE), 2),
                'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2),
                'Strike %' = round(sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay"))/n(),3)*100,
                'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                    sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")),3)*100)
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,13,15), `border-right` = "solid 1px")
  })
  
  
  output$pitch_movement_plot <- renderPlot({
    dataFilter <- reactive({
      game %>%
        filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]))# %>%#Date == input$GameInput)
    })
    ggplot(data = dataFilter(), aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
      labs(x = "Horizontal Movement (in.)", y = "Vertical Movement (in.)", color = " ", title = "Pitch Movement") + 
      xlim(-25, 25) + ylim(-25, 25) +
      geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") + 
      geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
      geom_point(size = 2, na.rm = TRUE) +
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
  }, width = 450, height = 450)
  
  
  output$pitch_location_plot <- renderPlot({
    dataFilter <- reactive({
      game %>%
        filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]))# %>%#Date == input$GameInput)
    })
    ggplot(data = dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      xlim(-3,3) + ylim(0,5) + labs(color = "", title = "Pitch Location") +
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = 1, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 1, color = "black") + 
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") + 
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 1, color = "black") + 
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 1, color = "black") + 
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
      geom_point(size = 3, na.rm = TRUE) +
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
  }, width = 350, height = 450)
  
  
  output$pitch_velocity_plot <- renderPlot({
    dataFilter <- reactive({
      game %>%
        filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%#Date == input$GameInput) %>%
        group_by(TaggedPitchType) %>%
        dplyr::mutate(PitchNum = row_number())
    })
    ggplot(data = dataFilter()) + 
      geom_line(aes(y = RelSpeed, x = PitchNum, color = TaggedPitchType), size = 2) + 
      labs(x = "Pitch Count", y = "Pitch Velocity (MPH)", color = " ", title = "Pitch Velocity") + 
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 12)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
  }, width = 450, height = 450)
  
  
}

shinyApp(ui = ui, server = server)
