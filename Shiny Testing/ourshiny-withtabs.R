library(shiny)
library(readr)
library(tidyverse)
library(DT)
library(sportyR)

# csv read
game <- read.csv("cleanedGames.csv")
game$Date <- as.Date(game$Date)
game$TaggedPitchType <- factor(game$TaggedPitchType, levels = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter"))
pitch_colors <- c('Fastball' = '#d22d49', 'Sinker' = '#fe9d00', 'Cutter' = '#933f2c', 'Curveball' = '#00d1ed',
                  'Slider' = '#c3bd0d', 'Sweeper' = '#CB9AF0', 'ChangeUp' = '#23be41', 'Splitter' = '#3bacac', 'Other' = '#Acafaf')

# some useful functions
strike_zone <- c(-.9, .9, 1.55, 3.35)
is_in_zone <- function(height, side, zone = strike_zone) {
  zone <- 0
  for (i in seq_along(height)) {
    if (is.na(height[i]) | is.na(side[i])) {
      zone <- zone
    } else if (between(height[i], strike_zone[3], strike_zone[4]) &
               between(side[i], strike_zone[1], strike_zone[2])) {
      zone <- zone + 1
    }
  }
  return(zone)
}
is_swing <- function(pitch_call) {
  swings <- 0
  for (i in seq_along(pitch_call)) {
    if (pitch_call[i] %in% c('StrikeSwinging', 'FoulBall', 'InPlay')) {
      swings <- swings + 1
    }
  }
  return(swings)
}
is_o_swing <- function(height, side, pitch_call, zone = strike_zone) {
  o_swings <- 0
  for (i in seq_along(height)) {
    if ((!is_in_zone(height[i], side[i])) & is_swing(pitch_call[i])) {
      o_swings <- o_swings + 1
    }
  }
  return(o_swings)
}

ui <- fluidPage(
  
  titlePanel("UCLA Pitching Data"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "PitcherInput", label = "Select Pitcher", 
                  choices = c(All = "All", sort(unique(game$Pitcher)))),
      dateRangeInput(inputId = "DateRangeInput", label = "Select Date Range", 
                     start = min(game$Date), end = max(game$Date)),
      selectInput(inputId = "SplitInput", label = "Select Batter Hand", 
                  choices = c("Both", sort(unique(game$BatterSide)))),
      selectInput(inputId = "PitchInput", label = "Select Pitch", 
                  choices = c("All", "Primaries", "Breaking Balls", "OffSpeed", 
                              sort(levels(unique(game$TaggedPitchType)))), multiple = TRUE, selected = "All"),
      selectInput(inputId = "CountInput", label = "Select Count", 
                  choices = c("All", "Ahead", "Behind", "Even", 
                              sort(unique(game$Counts))), multiple = TRUE, selected = "All"),
      img(src = "bruinlogo.png", 
          style = "display: block; margin-left: auto; margin-right: auto;", height = 150, width = 150)),
    mainPanel(
      tabsetPanel(
        tabPanel("Metrics", br(), dataTableOutput("summary_table"),
                 dataTableOutput("pitcher_summary_table"),
                 dataTableOutput("pitcher_percent_table")),
        tabPanel("Metric Plots", br(),  
                 fluidRow(
          column(4, plotOutput("pitch_movement_plot"), align = "center"),
          column(4, plotOutput("pitch_location_plot"), align = "center"),
          column(4, plotOutput("pitch_tilt_plot"), align = "center")
        )),
        tabPanel("Batted Ball Results", br(), dataTableOutput("batted_ball_table")),
        tabPanel("Batted Ball Plots", br()),
        tabPanel("Heat Maps", br(), plotOutput("heat_map")),
        tabPanel("Trends Over Time", br(), plotOutput("pitch_usage_plot"), br(),
                 plotOutput("pitch_velocity_plot"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$PitcherInput,# input$SplitInput, input$PitchInput, input$CountInput,
               updateSelectInput(session, inputId = "DateRangeInput", label = "Select Date Range", 
                                 choices = sort(unique(game$Date[game$Pitcher == input$PitcherInput]))))
  
  output$selected_pitcher <- renderText({paste(input$PitcherInput)})
  
  output$selected_game <- renderText({paste(input$DateRangeInput)})
  
  output$selected_hand <- renderText({paste(input$SplitInput)})
  
  output$selected_pitch <- renderText({paste(input$PitchInput)})
  
  output$selected_count <- renderText({paste(input$CountInput)})

output$summary_table <- renderDataTable({
  table <- game
  
  if(input$PitcherInput != "All") {
    table <- table %>% filter(Pitcher %in% input$PitcherInput)
  }
    
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PlayResult != "Undefined") %>%
      dplyr::summarize('PA' = n(), 
                       BBE = sum(PitchCall == "InPlay", na.rm = TRUE),
                       H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE), 
                       `1B` = sum(PlayResult == "Single", na.rm = TRUE), 
                       `2B` = sum(PlayResult == "Double", na.rm = TRUE), 
                       `3B` = sum(PlayResult == "Triple", na.rm = TRUE), 
                       HR = sum(PlayResult == "HomeRun", na.rm = TRUE), 
                       SO = sum(PlayResult == "Strikeout", na.rm = TRUE), 
                       BB = sum(PlayResult == "Walk", na.rm = TRUE), 
                       HBP = sum(PlayResult == "HitByPitch", na.rm = TRUE))
    
    table[is.na(table)] <- "-"
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE))))
})

    
output$pitcher_summary_table <- renderDataTable({
  table <- game
    if(any(input$PitchInput == "All")){
      pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
    }
    else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
      input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")]
      input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
      input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
      if(length(input1) == 1){
        input1 <- c("Fastball", "Sinker")
      }
      if(length(input2) == 1){
        input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
      }
      if(length(input3) == 1){
        input3 <- c("ChangeUp", "Splitter")
      }
      pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
      pitchinput <- c(pitchinput, input1, input2, input3)
    }
    else{
      pitchinput = input$PitchInput
    }
    
    if(any(input$CountInput == "All")){
      countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
    }
    else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
      input4 <- input$CountInput[input$CountInput %in% c("Even")] 
      input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
      input6 <- input$CountInput[input$CountInput %in% c("Behind")]
      if(length(input4) == 1){
        input4 <- c("0-0", "1-1", "2-2")
      }
      if(length(input5) == 1){
        input5 <- c("0-1", "0-2", "1-2")
      }
      if(length(input6) == 1){
        input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
      }
      countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
      countinput <- c(countinput, input4, input5, input6)
    }
    else{
      countinput = input$CountInput
    }
    if(input$SplitInput == "Both"){
      splitinput = c("Right", "Left")
    }
    else{
      splitinput = input$SplitInput
    }
    if(input$PitcherInput != "All") {
      table <- table %>% filter(Pitcher %in% input$PitcherInput)
    }

      table <- table %>%
        filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide %in% splitinput, TaggedPitchType %in% pitchinput, Counts %in% countinput) %>%
        group_by('Pitch' = TaggedPitchType) %>%
        dplyr::summarize('No.' = n(),
                         'Max Velo (MPH)' = round(max(RelSpeed, na.rm = TRUE),1),
                           'Avg. Velo (MPH)' = round(mean(RelSpeed, na.rm = TRUE),1),
                           'Avg. Spin (RPM)' = round(mean(SpinRate, na.rm = TRUE),-1),
                           'Tilt' = AveTilt[1],
                           'RelHeight' = round(mean(RelHeight, na.rm = TRUE), 2),
                           'RelSide' = round(mean(RelSide, na.rm = TRUE), 2),
                           'Extension' = round(mean(Extension, na.rm = TRUE), 2),
                           'IVB' = round(mean(InducedVertBreak, na.rm = TRUE), 1),
                           'HB' = round(mean(HorzBreak, na.rm = TRUE), 1),
                           'VAA' = round(mean(VertApprAngle, na.rm = TRUE), 2),
                           'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2)) %>%
          ungroup() %>%
          mutate('Usage %' = round(prop.table(No.), 3)*100)
        
        # aux <- nrow(table) - 1
        # table$hiddenColumn <- 0
        # table$hiddenColumn[aux] <- 1
        tableFilter <- reactive({table})
        datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(visible = FALSE, targets = 0)))) # %>%
          # formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,13,15,19,20), `border-right` = "solid 1px") %>% 
          # formatStyle(1:ncol(table), valueColumns = "hiddenColumn", `border-bottom` = styleEqual(1, "solid 3px")) %>%
          # formatStyle('Extension',
          #             backgroundColor = styleInterval(c(5.5, 6.5), c('lightcoral', 'white', 'lightgreen')))

  })

output$pitcher_percent_table <- renderDataTable({
  table <- game
  if(any(input$PitchInput == "All")){
    pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
  }
  else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
    input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")]
    input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
    input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
    if(length(input1) == 1){
      input1 <- c("Fastball", "Sinker")
    }
    if(length(input2) == 1){
      input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
    }
    if(length(input3) == 1){
      input3 <- c("ChangeUp", "Splitter")
    }
    pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
    pitchinput <- c(pitchinput, input1, input2, input3)
  }
  else{
    pitchinput = input$PitchInput
  }
  
  if(any(input$CountInput == "All")){
    countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
  }
  else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
    input4 <- input$CountInput[input$CountInput %in% c("Even")] 
    input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
    input6 <- input$CountInput[input$CountInput %in% c("Behind")]
    if(length(input4) == 1){
      input4 <- c("0-0", "1-1", "2-2")
    }
    if(length(input5) == 1){
      input5 <- c("0-1", "0-2", "1-2")
    }
    if(length(input6) == 1){
      input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
    }
    countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
    countinput <- c(countinput, input4, input5, input6)
  }
  else{
    countinput = input$CountInput
  }
  if(input$SplitInput == "Both"){
    splitinput = c("Right", "Left")
  }
  else{
    splitinput = input$SplitInput
  }
  if(input$PitcherInput != "All") {
    table <- table %>% filter(Pitcher %in% input$PitcherInput)
    table2 <- table %>% filter(Pitcher %in% input$PitcherInput)
  }
  
  table <- table %>%
    filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide %in% splitinput, TaggedPitchType %in% pitchinput, Counts %in% countinput) %>%
    group_by('Pitch' = TaggedPitchType) %>%
    dplyr::summarize('No.' = n(),
                   'Strike %' = round(sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay"))/n(),3)*100,
                   in_zones = sum(is_in_zone(PlateLocHeight, PlateLocSide)),
                   "Zone %" = round(in_zones/n(), 3)*100,
                   out_zones = n() - in_zones,
                   chases = sum(is_o_swing(PlateLocHeight, PlateLocSide, PitchCall)),
                   "Chase %" = round(chases/out_zones, 3)*100,
                   'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
                                       sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")),3)*100,
                   "SwStr %" = round(sum(PitchCall %in% c("StrikeSwinging"))/n(), 3)*100,
                   'CSW %' = round(sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging"))/n(),3)*100) %>%
  select(-c(in_zones, out_zones, chases)) %>%
  select(Pitch, 'No.', 'Strike %', 'Zone %', 'Chase %', 'Whiff %', 'SwStr %', 'CSW %') %>%
  ungroup() %>%
    mutate('Usage %' = round(prop.table(No.), 3)*100)
  
  # table2 <- table2 %>%
  #   filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% pitchinput, Counts %in% countinput, BatterSide %in% splitinput) %>%
  #   dplyr::summarize('No.' = n(),
  #                    'Strike %' = round(sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay"))/n(),3)*100,
  #                    in_zones = sum(is_in_zone(PlateLocHeight, PlateLocSide)),
  #                    "Zone %" = round(in_zones/n(), 3)*100,
  #                    out_zones = n() - in_zones,
  #                    chases = sum(is_o_swing(PlateLocHeight, PlateLocSide, PitchCall)),
  #                    "Chase %" = round(chases/out_zones, 3)*100,
  #                    'Whiff %' = round(sum(PitchCall %in% c("StrikeSwinging"))/
  #                                        sum(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")),3)*100,
  #                    "SwStr %" = round(sum(PitchCall %in% c("StrikeSwinging"))/n(), 3)*100,
  #                    'CSW %' = round(sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging"))/n(),3)*100) %>%
  #   select(-c(in_zones, out_zones, chases)) %>%
  #   select(Pitch, 'No.', 'Strike %', 'Zone %', 'Chase %', 'Whiff %', 'SwStr %', 'CSW %')
  # 
  # table <- bind_rows(table, table2)
  
  # aux <- nrow(table) - 1
  # table$hiddenColumn <- 0
  # table$hiddenColumn[aux] <- 1
  tableFilter <- reactive({table})
  datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(visible = FALSE, targets = 0))))  %>%
  # formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,13,15,19,20), `border-right` = "solid 1px") %>% 
  # formatStyle(1:ncol(table), valueColumns = "hiddenColumn", `border-bottom` = styleEqual(1, "solid 3px")) %>%
  formatStyle('Strike %',
              backgroundColor = styleInterval(c(58.0, 62.0), c('lightcoral', 'white', 'lightgreen'))) %>%
  formatStyle('Zone %',
              backgroundColor = styleInterval(c(47.5, 50.5), c('lightcoral', 'white', 'lightgreen'))) %>%
  formatStyle('Whiff %',
              backgroundColor = styleInterval(c(20, 30), c('lightcoral', 'white', 'lightgreen'))) %>%
  formatStyle('Chase %',
              backgroundColor = styleInterval(c(26, 30), c('lightcoral', 'white', 'lightgreen'))) %>%
  formatStyle('CSW %',
              backgroundColor = styleInterval(c(26, 32), c('lightcoral', 'white', 'lightgreen'))) %>%
  formatStyle('SwStr %',
              backgroundColor = styleInterval(c(7, 13), c('lightcoral', 'white', 'lightgreen')))

})

output$pitch_movement_plot <- renderPlot({
  table <- game
  if(any(input$PitchInput == "All")){
    pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
  }
  else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
    input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")] 
    input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
    input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
    if(length(input1) == 1){
      input1 <- c("Fastball", "Sinker")
    }
    if(length(input2) == 1){
      input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
    }
    if(length(input3) == 1){
      input3 <- c("ChangeUp", "Splitter")
    }
    pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
    pitchinput <- c(pitchinput, input1, input2, input3)
  }
  else{
    pitchinput = input$PitchInput
  }
  
  if(any(input$CountInput == "All")){
    countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
  }
  else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
    input4 <- input$CountInput[input$CountInput %in% c("Even")] 
    input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
    input6 <- input$CountInput[input$CountInput %in% c("Behind")]
    if(length(input4) == 1){
      input4 <- c("0-0", "1-1", "2-2")
    }
    if(length(input5) == 1){
      input5 <- c("0-1", "0-2", "1-2")
    }
    if(length(input6) == 1){
      input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
    }
    countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
    countinput <- c(countinput, input4, input5, input6)
  }
  else{
    countinput = input$CountInput
  }
  if(input$SplitInput == "Both"){
    splitinput = c("Right", "Left")
  }
  else{
    splitinput = input$SplitInput
  }
  if(input$PitcherInput != "All") {
    table <- table %>% filter(Pitcher %in% input$PitcherInput)
  }

    dataFilter <- reactive({
      table %>%
        filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide %in% splitinput, TaggedPitchType %in% pitchinput, Counts %in% countinput)
    })
    means <- dataFilter() %>% 
      group_by(TaggedPitchType) %>% 
      dplyr::summarize(InducedVertBreak = mean(InducedVertBreak, na.rm = T),
                       HorzBreak = mean(HorzBreak, na.rm = T)) %>% 
      select(TaggedPitchType, InducedVertBreak, HorzBreak)
    
    ggplot(data = dataFilter(), aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
      scale_color_manual(values = pitch_colors) +
      labs(x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)", color = " ", title = "Pitch Movement") + 
      xlim(-28, 28) + ylim(-28, 28) +
      geom_segment(aes(x = 0, y = -28, xend = 0, yend = 28), linewidth = 1, color = "grey55") + 
      geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), linewidth = 1, color = "grey55") +
      geom_point(size = 3, na.rm = TRUE, alpha = 0.7, size = 2) +
      geom_point(data = means, shape = 21, size = 4, fill = 'white', color = 'black', stroke = 1.5) +
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
  }, width = 450, height = 450)

output$pitch_location_plot <- renderPlot({
  table <- game
  if(any(input$PitchInput == "All")){
    pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
  }
  else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
    input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")] 
    input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
    input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
    if(length(input1) == 1){
      input1 <- c("Fastball", "Sinker")
    }
    if(length(input2) == 1){
      input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
    }
    if(length(input3) == 1){
      input3 <- c("ChangeUp", "Splitter")
    }
    pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
    pitchinput <- c(pitchinput, input1, input2, input3)
  }
  else{
    pitchinput = input$PitchInput
  }
  
  if(any(input$CountInput == "All")){
    countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
  }
  else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
    input4 <- input$CountInput[input$CountInput %in% c("Even")] 
    input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
    input6 <- input$CountInput[input$CountInput %in% c("Behind")]
    if(length(input4) == 1){
      input4 <- c("0-0", "1-1", "2-2")
    }
    if(length(input5) == 1){
      input5 <- c("0-1", "0-2", "1-2")
    }
    if(length(input6) == 1){
      input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
    }
    countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
    countinput <- c(countinput, input4, input5, input6)
  }
  else{
    countinput = input$CountInput
  }
  if(input$SplitInput == "Both"){
    splitinput = c("Right", "Left")
  }
  else{
    splitinput = input$SplitInput
  }
  if(input$PitcherInput != "All") {
    table <- table %>% filter(Pitcher %in% input$PitcherInput)
  }
  
    dataFilter <- reactive({
      table %>%
        filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide %in% splitinput, TaggedPitchType %in% pitchinput, Counts %in% countinput)
    })

  ggplot(data = dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
    xlim(-3,3) + ylim(0,5) + labs(color = "", title = "Pitch Location") +
    scale_color_manual(values = pitch_colors) +
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, linewidth = 1, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), linewidth = 1, color = "black") + # maybe linewidth instead of size
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), linewidth = 1, color = "black") + 
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), linewidth = 1, color = "black") + 
    geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), linewidth = 1, color = "black") + 
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), linewidth = 1, color = "black") +
    geom_point(size = 3, na.rm = TRUE, alpha = 0.7) +
    theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
}, width = 350, height = 450)

output$pitch_tilt_plot <- renderPlot({
  convert_tilt <- function(time) {
    time_parts <- strsplit(time, ":")[[1]]
    hours <- as.numeric(time_parts[1])
    hours <- ifelse(hours == 12, 0, hours)
    minutes <- as.numeric(time_parts[2])
    return(hours + minutes/60)
  }
  game$plotTilt <- mapply(convert_tilt, game$Tilt)
  
  game <- game %>% subset(!is.na(plotTilt))
  
  table <- game
  if(any(input$PitchInput == "All")){
    pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
  }
  else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
    input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")] 
    input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
    input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
    if(length(input1) == 1){
      input1 <- c("Fastball", "Sinker")
    }
    if(length(input2) == 1){
      input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
    }
    if(length(input3) == 1){
      input3 <- c("ChangeUp", "Splitter")
    }
    pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
    pitchinput <- c(pitchinput, input1, input2, input3)
  }
  else{
    pitchinput = input$PitchInput
  }
  
  if(any(input$CountInput == "All")){
    countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
  }
  else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
    input4 <- input$CountInput[input$CountInput %in% c("Even")] 
    input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
    input6 <- input$CountInput[input$CountInput %in% c("Behind")]
    if(length(input4) == 1){
      input4 <- c("0-0", "1-1", "2-2")
    }
    if(length(input5) == 1){
      input5 <- c("0-1", "0-2", "1-2")
    }
    if(length(input6) == 1){
      input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
    }
    countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
    countinput <- c(countinput, input4, input5, input6)
  }
  else{
    countinput = input$CountInput
  }
  if(input$SplitInput == "Both"){
    splitinput = c("Right", "Left")
  }
  else{
    splitinput = input$SplitInput
  }
  if(input$PitcherInput != "All") {
    table <- table %>% filter(Pitcher %in% input$PitcherInput)
  }
  
  dataFilter <- reactive({
    table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide %in% splitinput, TaggedPitchType %in% pitchinput, Counts %in% countinput)
  })
  
  ggplot(data= dataFilter(), aes(x=plotTilt, fill=TaggedPitchType)) +
    scale_color_manual(values = pitch_colors) +
    labs(color = "", title = "Pitch Tilt") +
    geom_histogram(binwidth=.5) +
    geom_vline(xintercept = seq(0, 11, by = .5), colour = "white", size = 0.2) +
    coord_polar(start = -7.5/360*2*pi) +
    scale_x_continuous(breaks=seq(0, 11, by=1))+
    scale_y_continuous(limits = c(-40, 100)) +
    theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_blank())
}, width = 450, height = 450)

output$batted_ball_table <- renderDataTable({
  table <- game
  if(any(input$PitchInput == "All")){
    pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
  }
  else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
    input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")] 
    input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
    input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
    if(length(input1) == 1){
      input1 <- c("Fastball", "Sinker")
    }
    if(length(input2) == 1){
      input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
    }
    if(length(input3) == 1){
      input3 <- c("ChangeUp", "Splitter")
    }
    pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
    pitchinput <- c(pitchinput, input1, input2, input3)
  }
  else{
    pitchinput = input$PitchInput
  }
  
  if(any(input$CountInput == "All")){
    countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
  }
  else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
    input4 <- input$CountInput[input$CountInput %in% c("Even")] 
    input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
    input6 <- input$CountInput[input$CountInput %in% c("Behind")]
    if(length(input4) == 1){
      input4 <- c("0-0", "1-1", "2-2")
    }
    if(length(input5) == 1){
      input5 <- c("0-1", "0-2", "1-2")
    }
    if(length(input6) == 1){
      input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
    }
    countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
    countinput <- c(countinput, input4, input5, input6)
  }
  else{
    countinput = input$CountInput
  }
  if(input$SplitInput == "Both"){
    splitinput = c("Right", "Left")
  }
  else{
    splitinput = input$SplitInput
  }
  if(input$PitcherInput != "All") {
    table <- table %>% filter(Pitcher %in% input$PitcherInput)
  }
  
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide %in% splitinput, TaggedPitchType %in% pitchinput, PlayResult != "Undefined", Counts %in% countinput) %>% 
      group_by('Pitch' = TaggedPitchType) %>%
      dplyr::summarize('PA' = n(),
                       'BBE' = sum(PitchCall == "InPlay"),
                       'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                       'Max. EV' = round(max(ExitSpeed, na.rm = TRUE),1),
                       'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                       'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/sum(PitchCall == "InPlay"), 3)*100,
                       'Barrel %' = round(sum(Barrel, na.rm = TRUE)/sum(PitchCall == "InPlay"), 3)*100,
                       'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                       'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                       'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                       'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                       'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                       'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100,
                       'wOBA' = round(((.693*sum(PlayResult == "Walk") + .693*sum(PlayResult == "HitByPitch") + .884*sum(PlayResult == "Single") + 1.261*sum(PlayResult == "Double") + 1.601*sum(PlayResult == "Triple") + 2.072*sum(PlayResult == "HomeRun"))/(n()-sum(PlayResult == "IntentionalWalk"))),3)
                       
                       #  0.693*totalHBB$x +.884*totalS$x + 1.261*totalD$x + 1.601*totalT$x + 2.072*totalH$x)/(totalAB$x + totalHBB$x + totalSac$x
      ) %>%
      ungroup()
    # table2 <- game %>%
    #   filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide %in% splitinput, TaggedPitchType %in% pitchinput, PlayResult != "Undefined", Counts %in% countinput) %>% 
    #   dplyr::summarize('PA' = n(),
    #                    'BIP' = sum(PitchCall == "InPlay"),
    #                    'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
    #                    'Max. EV' = round(max(ExitSpeed, na.rm = TRUE),1),
    #                    'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
    #                    'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/sum(PitchCall == "InPlay"), 3)*100,
    #                    'Barrel %' = round(sum(Barrel, na.rm = TRUE)/sum(PitchCall == "InPlay"), 3)*100,
    #                    'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
    #                    'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
    #                    'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
    #                    'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
    #                    'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
    #                    'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100,
    #                    'wOBA' = round(((.693*sum(PlayResult == "Walk") + .693*sum(PlayResult == "HitByPitch") + .884*sum(PlayResult == "Single") + 1.261*sum(PlayResult == "Double") + 1.601*sum(PlayResult == "Triple") + 2.072*sum(PlayResult == "HomeRun"))/(n()-sum(PlayResult == "IntentionalWalk"))),3)
    #   )
    # table <- bind_rows(table, table2)
    
    # aux <- nrow(table) - 1
    # table$hiddenColumn <- 0
    # table$hiddenColumn[aux] <- 1
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(visible = FALSE, targets = c(0,ncol(table)))))) %>%
 #     formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(3,8,10,15), `border-right` = "solid 1px") %>% 
 #     formatStyle(1:ncol(table), valueColumns = "hiddenColumn", `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatStyle('wOBA',
                  backgroundColor = styleInterval(c(.300, .340), c('lightgreen', 'white', 'lightcoral'))) %>%
      formatStyle('Barrel %',
                  backgroundColor = styleInterval(c(7, 9), c('lightgreen', 'white', 'lightcoral'))) %>%
      formatStyle('Avg. EV',
                  backgroundColor = styleInterval(c(85, 89), c('lightgreen', 'white', 'lightcoral'))) %>%
      formatStyle('Hard Hit %',
                  backgroundColor = styleInterval(c(38, 42), c('lightgreen', 'white', 'lightcoral'))) %>%
      formatStyle('K %',
                  backgroundColor = styleInterval(c(20, 24), c('lightcoral', 'white', 'lightgreen'))) %>%
      formatStyle('BB %',
                  backgroundColor = styleInterval(c(6, 10), c('lightgreen', 'white', 'lightcoral')))
    
})


output$heat_map <- renderPlot({
  if(any(input$PitchInput == "All")){
    pitchinput = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter")
  }
  else if(any(input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed"))){
    input1 <- input$PitchInput[input$PitchInput %in% c("Primaries")] 
    input2 <- input$PitchInput[input$PitchInput %in% c("Breaking Balls")]
    input3 <- input$PitchInput[input$PitchInput %in% c("OffSpeed")]
    if(length(input1) == 1){
      input1 <- c("Fastball", "Sinker")
    }
    if(length(input2) == 1){
      input2 <- c("Cutter", "Curveball", "Slider", "Sweeper")
    }
    if(length(input3) == 1){
      input3 <- c("ChangeUp", "Splitter")
    }
    pitchinput <- input$PitchInput[!input$PitchInput %in% c("Primaries", "Breaking Balls", "OffSpeed")]
    pitchinput <- c(pitchinput, input1, input2, input3)
  }
  else{
    pitchinput = input$PitchInput
  }
  
  if(any(input$CountInput == "All")){
    countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
  }
  else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
    input4 <- input$CountInput[input$CountInput %in% c("Even")] 
    input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
    input6 <- input$CountInput[input$CountInput %in% c("Behind")]
    if(length(input4) == 1){
      input4 <- c("0-0", "1-1", "2-2")
    }
    if(length(input5) == 1){
      input5 <- c("0-1", "0-2", "1-2")
    }
    if(length(input6) == 1){
      input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
    }
    countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
    countinput <- c(countinput, input4, input5, input6)
  }
  else{
    countinput = input$CountInput
  }
  if(input$SplitInput == "Both"){
    splitinput = c("Right", "Left")
  }
  else{
    splitinput = input$SplitInput
  }
  
  dataFilter <- reactive({
    game %>%
      filter(Pitcher == input$PitcherInput,
             between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide %in% splitinput, TaggedPitchType %in% pitchinput, Counts %in% countinput)
  })
  ggplot(dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight)) +
    stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = F) +
    scale_fill_gradientn(colours = c("blue", "white", "red")) +
    annotate("rect", xmin = -1, xmax = 1,
             ymin = 1.6,ymax = 3.4,
             fill= NA,color= "black", 
             alpha = .1) +
    ylim(1, 4) + xlim(-1.8, 1.8) + theme_bw() + 
    theme_classic() +
    xlab("Horizontal Pitch Location") +
    ylab("Vertical Pitch Location") +
    ggtitle("Pitch Location Heat Map", subtitle = "Pitcher's Perspective") +
    facet_wrap(~TaggedPitchType, ncol = 3) +
    guides(fill = FALSE)
}, width = 700, height = 400)

output$pitch_usage_plot <- renderPlot({
  table <- game
  
  if(any(input$CountInput == "All")){
    countinput = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")
  }
  else if(any(input$CountInput %in% c("Even", "Ahead", "Behind"))){
    input4 <- input$CountInput[input$CountInput %in% c("Even")] 
    input5 <- input$CountInput[input$CountInput %in% c("Ahead")]
    input6 <- input$CountInput[input$CountInput %in% c("Behind")]
    if(length(input4) == 1){
      input4 <- c("0-0", "1-1", "2-2")
    }
    if(length(input5) == 1){
      input5 <- c("0-1", "0-2", "1-2")
    }
    if(length(input6) == 1){
      input6 <- c("1-0", "2-0", "3-0", "2-1", "3-1")
    }
    countinput <- input$CountInput[!input$CountInput %in% c("Even", "Ahead", "Behind")]
    countinput <- c(countinput, input4, input5, input6)
  }
  else{
    countinput = input$CountInput
  }
  if(input$SplitInput == "Both"){
    splitinput = c("Right", "Left")
  }
  else{
    splitinput = input$SplitInput
  }
  if(input$PitcherInput != "All") {
    table <- table %>% filter(Pitcher %in% input$PitcherInput)
  }

    dataFilter <- reactive({
      table %>%
        filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide %in% splitinput, Counts %in% countinput) %>%
        group_by(TaggedPitchType, Date) %>%
        dplyr::summarize('No.' = n()) %>%
        ungroup() %>%
        group_by(Date) %>%
        mutate('Usage' = round(prop.table(No.), 3)*100)
    })
    ggplot(data = dataFilter()) +
      geom_point(aes(x = Date, y = Usage, color = TaggedPitchType)) +
      geom_line(aes(x = Date, y = Usage, color = TaggedPitchType)) +
      scale_color_manual(values = pitch_colors) +
      labs(x = "Date", y = "Usage %", color = " ", title = "Usage % By Outing") +
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))

  
}, width = 900, height = 450)

output$pitch_velocity_plot <- renderPlot({
  dataFilter <- reactive({
    game %>%
      filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
      mutate(PitchNo = row_number())
  })
  ggplot(data = dataFilter()) + 
    geom_line(aes(y = RelSpeed, x = PitchNo, color = TaggedPitchType), size = 2) + 
    scale_color_manual(values = pitch_colors) +
    labs(x = "Pitch Count", y = "Pitch Velocity (MPH)", color = " ", title = "Pitch Velocity") + 
    ylim(65, 95) + 
    theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 12)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
}, width = 900, height = 400)
   
}

shinyApp(ui = ui, server = server)