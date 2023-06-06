library(shiny)
library(readr)
library(tidyverse)
library(DT)
library(sportyR)

# csv read
game <- read.csv("cleanedGames.csv")
game <- game %>% filter(PitcherTeam == 'UCLA')
game$Date <- as.Date(game$Date)
game$TaggedPitchType <- factor(game$TaggedPitchType, levels = c("Fastball", "Sinker","Cutter", "Curveball", "Slider", "Sweeper", "ChangeUp", "Splitter"))
# this line needs work
# game$pitch_type <- factor(game$pitch_type, levels = c("FF", "SI", "FC", "CS", "CU", "KC", "SL", "CH", "FS"))
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
  column(10, offset = 1,
         titlePanel("Trackman Shiny Application - Post-Game Report"),
         hr(style="border-color: black;"), 
         fluidRow(
           column(2, selectInput(inputId = "PitcherInput", label = "Select Pitcher", choices = sort(unique(game$Pitcher)))),
           column(2, dateRangeInput(inputId = "DateRangeInput", label = "Select Date Range", start = min(game$Date), end = max(game$Date))),
           column(2, selectInput(inputId = "SplitInput", label = "Select Batter Hand", choices = c("Both", sort(unique(game$BatterSide))))),
           column(2, selectInput(inputId = "PitchInput", label = "Select Pitch", choices = c("All", sort(levels(unique(game$TaggedPitchType)))), multiple = TRUE, selected = "All")),
           column(2, selectInput(inputId = "CountInput", label = "Select Count", choices = c("All", sort(unique(game$Counts))), multiple = TRUE, selected = "All")),
           column(2, selectInput(inputId = "ReportInput", label = "Select Report Type", choices = c("Pitch Metrics", "Batted Ball")))
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
  
  output$selected_hand <- renderText({paste(input$SplitInput)})
  
  output$selected_pitch <- renderText({paste(input$PitchInput)})
  
  output$selected_report <- renderText({paste(input$ReportInput)})
  
  output$selected_count <- renderText({paste(input$CountInput)})
  
  output$pitcher_summary_table <- renderDataTable({
    if(input$ReportInput == "Pitch Metrics"){
      if(input$SplitInput == "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
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
                           'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2),
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
          ungroup() %>%
          mutate('Usage %' = round(prop.table(No.), 3)*100)
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
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
          select(-c(in_zones, out_zones, chases))
        table <- bind_rows(table, table2)
      
      }
      else if (input$SplitInput == "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), Counts %in% input$CountInput) %>%
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
                           'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2),
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
          ungroup() %>% 
          mutate('Usage %' = round(prop.table(No.), 3)*100)
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), Counts %in% input$CountInput) %>%
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
          select(-c(in_zones, out_zones, chases))
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput == "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput) %>%
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
                           'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2),
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
          ungroup() %>%
          mutate('Usage %' = round(prop.table(No.), 3)*100)
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput) %>%
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
          select(-c(in_zones, out_zones, chases))
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput == "Both" & all(input$PitchInput != "All") & all(input$CountInput != "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput) %>%
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
                           'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2),
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
          ungroup() %>%
          mutate('Usage %' = round(prop.table(No.), 3)*100)
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput) %>%
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
          select(-c(in_zones, out_zones, chases))
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput != "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput) %>%
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
                           'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2),
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
          ungroup() %>%
          mutate('Usage %' = round(prop.table(No.), 3)*100)
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput) %>%
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
          select(-c(in_zones, out_zones, chases))
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput != "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, Counts %in% input$CountInput) %>%
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
                           'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2),
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
          ungroup() %>%
          mutate('Usage %' = round(prop.table(No.), 3)*100)
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), Counts %in% input$CountInput, BatterSide == input$SplitInput) %>%
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
          select(-c(in_zones, out_zones, chases))
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput != "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput) %>%
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
                           'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2),
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
          ungroup() %>%
          mutate('Usage %' = round(prop.table(No.), 3)*100)
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, BatterSide == input$SplitInput) %>%
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
          select(-c(in_zones, out_zones, chases))
        table <- bind_rows(table, table2)
      }
      else {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput) %>%
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
                           'HAA' = round(mean(HorzApprAngle, na.rm = TRUE), 2),
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
          ungroup() %>%
          mutate('Usage %' = round(prop.table(No.), 3)*100)
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput, BatterSide == input$SplitInput) %>%
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
          select(-c(in_zones, out_zones, chases))
        table <- bind_rows(table, table2)
      }
    }
    else{
      if(input$SplitInput == "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PlayResult != "Undefined") %>% 
          group_by('Pitch' = TaggedPitchType) %>%
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100
                           ) %>%
          ungroup()
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PlayResult != "Undefined") %>% 
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100)
        table <- bind_rows(table, table2)
          
                           
      }
      else if(input$SplitInput == "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PlayResult != "Undefined", Counts %in% input$CountInput) %>% 
          group_by('Pitch' = TaggedPitchType) %>%
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100
          ) %>%
          ungroup()
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PlayResult != "Undefined", Counts %in% input$CountInput) %>% 
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100)
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput == "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, PlayResult != "Undefined") %>% 
          group_by('Pitch' = TaggedPitchType) %>%
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100
          ) %>%
          ungroup()
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, PlayResult != "Undefined") %>% 
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100)
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput == "Both" & all(input$PitchInput != "All") & all(input$CountInput != "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, PlayResult != "Undefined", Counts %in% input$CountInput) %>% 
          group_by('Pitch' = TaggedPitchType) %>%
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100
          ) %>%
          ungroup()
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, PlayResult != "Undefined", Counts %in% input$CountInput) %>% 
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100)
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput != "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, PlayResult != "Undefined") %>% 
          group_by('Pitch' = TaggedPitchType) %>%
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100
          ) %>%
          ungroup()
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, PlayResult != "Undefined") %>% 
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100)
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput != "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")) {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, PlayResult != "Undefined", Counts %in% input$CountInput) %>% 
          group_by('Pitch' = TaggedPitchType) %>%
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100
          ) %>%
          ungroup()
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, PlayResult != "Undefined", Counts %in% input$CountInput) %>% 
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100)
        table <- bind_rows(table, table2)
      }
      else if (input$SplitInput != "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")) {
        table <- game %>%
          table <- game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, PlayResult != "Undefined") %>% 
            group_by('Pitch' = TaggedPitchType) %>%
            dplyr::summarize('PA' = n(),
                             'BIP' = sum(PitchCall == "InPlay"),
                             'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                             'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                             'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                             'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                             'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                             'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                             'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                             'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                             'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100
            ) %>%
            ungroup()
          table2 <- game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, PlayResult != "Undefined") %>% 
            dplyr::summarize('PA' = n(),
                             'BIP' = sum(PitchCall == "InPlay"),
                             'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                             'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                             'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                             'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                             'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                             'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                             'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                             'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                             'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100)
          table <- bind_rows(table, table2)
      }
      else {
        table <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, PlayResult != "Undefined", Counts %in% input$CountInput) %>% 
          group_by('Pitch' = TaggedPitchType) %>%
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100
          ) %>%
          ungroup()
        table2 <- game %>%
          filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, PlayResult != "Undefined", Counts %in% input$CountInput) %>% 
          dplyr::summarize('PA' = n(),
                           'BIP' = sum(PitchCall == "InPlay"),
                           'Avg. EV' = round(mean(ExitSpeed, na.rm = TRUE),1),
                           'Avg. LA' = round(mean(Angle, na.rm = TRUE),1),
                           'Hard Hit %' = round(sum(HardHit, na.rm = TRUE)/n(), 3)*100,
                           'K %' = round(sum(PlayResult == "Strikeout")/n(), 3)*100,
                           'BB %' = round(sum(PlayResult == "Walk")/n(), 3)*100,
                           'GB %' = round(sum(TaggedHitType == "GroundBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'FB %' = round(sum(TaggedHitType == "FlyBall")/sum(PitchCall == "InPlay"), 3)*100,
                           'LD %' = round(sum(TaggedHitType == "LineDrive")/sum(PitchCall == "InPlay"), 3)*100,
                           'PU %' = round(sum(TaggedHitType == "Popup")/sum(PitchCall == "InPlay"), 3)*100)
        table <- bind_rows(table, table2)
      }
    }
    aux <- nrow(table) - 1
    table$hiddenColumn <- 0
    table$hiddenColumn[aux] <- 1
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(visible = FALSE, targets = c(0,ncol(table)))))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,13,17), `border-right` = "solid 1px") %>% formatStyle(1:ncol(table), valueColumns = "hiddenColumn",
                                                                                                                            `border-bottom` = styleEqual(1, "solid 3px")) 
  })
  
  output$pitch_movement_plot <- renderPlot({
    if(input$ReportInput == "Pitch Metrics"){
      if(input$SplitInput == "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]))
        })
      }
      else if(input$SplitInput == "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), Counts %in% input$CountInput)
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput)
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, Counts %in% input$CountInput)
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput)
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput)
        })
      }
      else if(input$SplitInput != "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, BatterSide == input$SplitInput)
        })
      }
      else{
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput)
        })
      }
      
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
    }
    else{
      if(input$SplitInput == "Both" & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
            group_by(TaggedPitchType, Date) %>%
            dplyr::summarize('No.' = n()) %>%
            ungroup() %>%
            group_by(Date) %>%
            mutate('Usage' = round(prop.table(No.), 3)*100)
        })
      }
      else if(input$SplitInput == "Both" & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), Counts %in% input$CountInput) %>%
            group_by(TaggedPitchType, Date) %>%
            dplyr::summarize('No.' = n()) %>%
            ungroup() %>%
            group_by(Date) %>%
            mutate('Usage' = round(prop.table(No.), 3)*100)
        })
      }
      else if(input$SplitInput != "Both" & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput) %>%
            group_by(TaggedPitchType, Date) %>%
            dplyr::summarize('No.' = n()) %>%
            ungroup() %>%
            group_by(Date) %>%
            mutate('Usage' = round(prop.table(No.), 3)*100)
        })
      }
      else{
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, Counts %in% input$CountInput) %>%
            group_by(TaggedPitchType, Date) %>%
            dplyr::summarize('No.' = n()) %>%
            ungroup() %>%
            group_by(Date) %>%
            mutate('Usage' = round(prop.table(No.), 3)*100)
        })
      }
      ggplot(data = dataFilter()) +
        geom_point(aes(x = Date, y = Usage, color = TaggedPitchType)) +
        geom_line(aes(x = Date, y = Usage, color = TaggedPitchType)) +
        scale_color_manual(values = pitch_colors) +
        labs(x = "Date", y = "Usage %", color = " ", title = "Usage % By Outing") +
        theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
        theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
    }

  }, width = 450, height = 450)
  
  
  output$pitch_location_plot <- renderPlot({
    if(input$ReportInput == "Pitch Metrics"){
      if(input$SplitInput == "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]))
        })
      }
      else if(input$SplitInput == "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), Counts %in% input$CountInput)
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput)
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, Counts %in% input$CountInput)
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput)
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput)
        })
      }
      else if(input$SplitInput != "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, BatterSide == input$SplitInput)
        })
      }
      else{
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput)
        })
      }
    }
    else{
      if(input$SplitInput == "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == "InPlay")
        })
      }
      else if(input$SplitInput == "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == "InPlay", Counts %in% input$CountInput)
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, PitchCall == "InPlay")
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, PitchCall == "InPlay", Counts %in% input$CountInput)
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, PitchCall == "InPlay")
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, PitchCall == "InPlay", Counts %in% input$CountInput)
        })
      }
      else if(input$SplitInput != "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, PitchCall == "InPlay", BatterSide == input$SplitInput)
        })
      }
      else{
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, PitchCall == "InPlay", Counts %in% input$CountInput)
        })
      }
    }
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
  
  
  output$pitch_velocity_plot <- renderPlot({
    if(input$ReportInput == "Pitch Metrics"){
      if(input$SplitInput == "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
            group_by(TaggedPitchType) %>%
            dplyr::mutate(PitchNum = row_number())
        })
      }
      else if(input$SplitInput == "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), Counts %in% input$CountInput) %>%
            group_by(TaggedPitchType) %>%
            dplyr::mutate(PitchNum = row_number())
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput) %>%
            group_by(TaggedPitchType) %>%
            dplyr::mutate(PitchNum = row_number())
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, Counts %in% input$CountInput) %>%
            group_by(TaggedPitchType) %>%
            dplyr::mutate(PitchNum = row_number())
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput) %>%
            group_by(TaggedPitchType) %>%
            dplyr::mutate(PitchNum = row_number())
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput) %>%
            group_by(TaggedPitchType) %>%
            dplyr::mutate(PitchNum = row_number())
        })
      }
      else if(input$SplitInput != "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), TaggedPitchType %in% input$PitchInput, BatterSide == input$SplitInput) %>%
            group_by(TaggedPitchType) %>%
            dplyr::mutate(PitchNum = row_number())
        })
      }
      else{
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput) %>%
            group_by(TaggedPitchType) %>%
            dplyr::mutate(PitchNum = row_number())
        })
      }
      ggplot(data = dataFilter()) + 
        geom_line(aes(y = RelSpeed, x = PitchNum, color = TaggedPitchType), linewidth = 1.5) + 
        scale_color_manual(values = pitch_colors) +
        labs(x = "Pitch Count", y = "Pitch Velocity (MPH)", color = " ", title = "Pitch Velocity") + 
        theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 12)) +
        theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
    }
    else{
      if(input$SplitInput == "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == 'InPlay', TaggedHitType != 'Bunt', abs(Bearing) < 50) %>% 
            select(Bearing, Distance, Angle, ExitSpeed, TaggedHitType, PlayResult)
        })
      }
      else if(input$SplitInput == "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == 'InPlay', TaggedHitType != 'Bunt', abs(Bearing) < 50, Counts %in% input$CountInput) %>% 
            select(Bearing, Distance, Angle, ExitSpeed, TaggedHitType, PlayResult)
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == 'InPlay', TaggedHitType != 'Bunt', abs(Bearing) < 50, BatterSide == input$SplitInput) %>% 
            select(Bearing, Distance, Angle, ExitSpeed, TaggedHitType, PlayResult)
        })
      }
      else if(input$SplitInput != "Both" & any(input$PitchInput == "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == 'InPlay', TaggedHitType != 'Bunt', abs(Bearing) < 50, BatterSide == input$SplitInput, Counts %in% input$CountInput) %>% 
            select(Bearing, Distance, Angle, ExitSpeed, TaggedHitType, PlayResult)
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == 'InPlay', TaggedHitType != 'Bunt', abs(Bearing) < 50, TaggedPitchType %in% input$PitchInput) %>% 
            select(Bearing, Distance, Angle, ExitSpeed, TaggedHitType, PlayResult)
        })
      }
      else if(input$SplitInput == "Both" & all(input$PitchInput != "All") & all(input$CountInput != "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == 'InPlay', TaggedHitType != 'Bunt', abs(Bearing) < 50, TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput) %>% 
            select(Bearing, Distance, Angle, ExitSpeed, TaggedHitType, PlayResult)
        })
      }
      else if(input$SplitInput != "Both" & all(input$PitchInput != "All") & any(input$CountInput == "All")){
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == 'InPlay', TaggedHitType != 'Bunt', abs(Bearing) < 50, BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput) %>% 
            select(Bearing, Distance, Angle, ExitSpeed, TaggedHitType, PlayResult)
        })
      }
      else{
        dataFilter <- reactive({
          game %>%
            filter(Pitcher == input$PitcherInput, between(Date, input$DateRangeInput[1], input$DateRangeInput[2]), PitchCall == 'InPlay', TaggedHitType != 'Bunt', abs(Bearing) < 50, BatterSide == input$SplitInput, TaggedPitchType %in% input$PitchInput, Counts %in% input$CountInput) %>% 
            select(Bearing, Distance, Angle, ExitSpeed, TaggedHitType, PlayResult)
        })
      }
      geom_baseball(league = "MLB") +
        geom_point(data = dataFilter(), aes(round(Distance * sin(Bearing * pi / 180), 3), round(Distance * cos(Bearing * pi / 180), 3),
                       color = ExitSpeed)) +
        scale_color_gradient(low = 'blue', high = 'red') +
        labs(title = "Spray Chart", x = "", y = "") + 
        #theme_bw() + 
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.text = element_blank(),
              legend.position = "right",
              legend.text = element_text(size = 12))
    }

  }, width = 450, height = 450)
  
}

shinyApp(ui = ui, server = server)
