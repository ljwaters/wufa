# ----------------------------------------------------
# SETUP
# ----------------------------------------------------
library(tidyverse)
library(shiny)
library(shinydashboard)
library(readxl)
library(showtext)
library(usmap)


WUFA_path="/cloud/project/WUFA/"

source(paste(WUFA_path,"WUFA_master_ds.R",sep=""))
masterds<<-f.masterds(paste(WUFA_path,"Datasets",sep=""))
WUds <<- masterds %>%  
    filter(HOME == "WU" | AWAY == "WU") %>%
    select(
        GAME_ID,YEAR,OFF_T,TIME_TILL_THROW,TIME_TILL_CATCH,GN_LS,TIME_TILL_PRESSURE,YACONTACT,YACATCH
    ) %>%
    group_by(YEAR,GAME_ID,OFF_T) %>%
    summarize(
        across(.cols = everything(), ~ mean(.x, na.rm = TRUE))
    ) %>%
    pivot_longer(
        cols=TIME_TILL_THROW:YACATCH,
        names_to = "METRIC",
        values_to = "VALUE"
    )            
source(paste(WUFA_path,"theme_wu.R",sep=""))
source(paste(WUFA_path,"WUFA_functions.R",sep=""))
source(paste(WUFA_path,"WUFA_boxscores.R",sep=""))

boxscoreds<<-f.boxscore(paste(WUFA_path,"Boxscores",sep=""))
the_roster <<- read_csv("Roster(s)/WUFA Roster.csv")

source(paste(WUFA_path,"WUFA_server.R",sep=""))



teams=masterds %>% select(HOME,AWAY) %>% distinct() %>% 
    pivot_longer(
        HOME:AWAY,
        names_to="HOME_AWAY",
        values_to="TEAM") %>%
    select(TEAM) %>%
    drop_na() %>% distinct() %>% arrange(TEAM) %>% pull() %>% as.character()

opponents= masterds %>%
  filter(str_detect(GAME_ID, "WU")) %>%
  mutate(OPPONENT = ifelse(HOME == "WU", AWAY, HOME)) %>%
  pull(OPPONENT) %>%
  unique() %>%
  as.character()
  #setdiff(teams,"WU")

WU_SEASONS=rev(setdiff(unique(masterds$YEAR),NA))
WU_MAP_SEASONS=rev(setdiff(unique(the_roster$SEASON),NA))

# ----------------------------------------------------
# UI
# ----------------------------------------------------

ui = dashboardPage(
    skin = "black",
    dashboardHeader(
        title = tags$div("WILLAMETTE UNIVERSITY FOOTBALL ANALYTICS", style = "text-align: left width: 100%; font-family: 'Lora', serif; font-weight: bold; font-size: 40px background-color: black; color: darkred;"),
        titleWidth = "100%"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overall", tabName = "OVERALL", icon = icon("dashboard")),
            menuItem("Offense", tabName = "OFFENSE", icon = icon("chart-line")),
            menuItem("Defense", tabName = "DEFENSE", icon = icon("shield-alt")),
            menuItem("Recruiting", tabName = "RECRUITING", icon = icon("shield-alt")),
            tags$div(
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                ),
                HTML("<br>"),
                tags$img(
                    src = "https://d2o2figo6ddd0g.cloudfront.net/0/o/46jot1tbirrk1n/White_Mesh.jpg", width = "240px"
                )
            )        
        )
    ),
    dashboardBody(
        tags$head(
            tags$style(HTML("
 @import url('https://fonts.googleapis.com/css2?family=Lora:wght@400;700&display=swap');
        
        body, .wrapper, .main-sidebar, .box, .content-wrapper, .main-footer {
          background-color: black !important;
          color: white !important;
          font-family: 'Lora', serif !important;
        }

        .main-header .navbar {
          background-color: darkred !important;
          color: white !important;
          border-bottom: 1px solid white;
        }
        
        .skin-blue .logo{
          background-color: black !important;
          color: darkred !important;
        }
        
        .skin-blue .main-sidebar .sidebar-menu > li > a {
          color: white !important;
        }
        
        .box {
          background-color: black !important;
          border: 1px solid darkred !important;
        }

        .box-header {
          background-color: darkred !important;
          color: white !important;
        }
        .box {
          background-color: #000000;
          border: 1px solid #000000;
          padding: 10px;
          margin-bottom: 20px;
        }
        .skin-black .main-header .navbar {
          background-color: #000000 !important;
        }
        # .skin-black .main-header .logo {
        #   background-color: #000000 !important;
        #   color: #FFFFFF !important;
        # }
        .skin-black .main-sidebar {
          background-color: #000000 !important;
          color: #FFFFFF !important;
        }
        .skin-black .main-sidebar .sidebar-menu > li > a {
          color: #FFFFFF !important;
        }
        .box {
          background-color: #333333 !important;
          border: 1px solid #FFFFFF !important;
        }
        .box-title {
          color: #FFFFFF !important;
        }
      "))
        ),
        tabItems(
            # ----------------------------------------- OVERALL TAB -----------------------------------------
            tabItem(tabName = "OVERALL",
                fluidRow(
                    column(12, 
                        div(
                            style = "background-color: black; color: white; padding: 10px; text-align: center; font-family: 'Lora', serif; ",
                            tags$div(textOutput("HEADER_TEXT"), style = "text-align: center; width: 100%; font-family: 'Lora', serif; font-weight: bold; font-size: 20px;"),   
                            HTML("<br>"),
                            tags$div(tags$img(
                                src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcST_twtLL5koFK_UQu3p7s3yDTMVtyY1iCfww&s", width="240",height="200")
                            ),
                        ),
                    ),
                ),
                # Drop-down menu to filter year
                HTML("<p style='color:white'>"),
                selectInput("season", label = "Select year:",
                            choices = rev(WU_SEASONS),
                            width = '150px'), 
                HTML("</p>"),
                fluidRow(
                  column(
                    width = 12, 
                    plotOutput("overall_score_plot")  
                  )
                ),
                fluidRow(
                  box(title = "SCORES", status = "primary", solidHeader = TRUE, width = 12,
                      plotOutput("OVERALL_SCORES", height = "450px"))
                ),        
                fluidRow(
                    box(title = "TIME TILL THROW", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("OVERALL_TIME_TILL_THROW", height = "450px"))
                ),
                fluidRow(
                    box(title = "TIME TILL CATCH", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("OVERALL_TIME_TILL_CATCH", height = "450px"))
                ),
                fluidRow(
                    box(title = "TIME TILL PRESSURE", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("OVERALL_TIME_TILL_PRESSURE", height = "450px"))
                ),    
                fluidRow(
                    box(title = "GAIN/LOSS", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("OVERALL_GN_LS", height = "450px"))
                ),
                fluidRow(
                    box(title = "YDS AFTER CATCH", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("OVERALL_YACATCH", height = "450px"))
                ),
                fluidRow(
                    box(title = "YDS AFTER CONTACT", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("OVERALL_YACONTACT", height = "450px"))
                )
            ),  # END OF OVERALL TAB
            # ----------------------------------------- OFFENSE TAB -----------------------------------------
            tabItem(tabName = "OFFENSE",
                fluidRow(
                    column(12, 
                           tags$div("OFFENSE", style = "text-align: center; width: 100%; font-family: 'Lora', serif; font-weight: bold; font-size: 20px;"),   
                    ),
                ),
                HTML("<br>"),
                selectInput("OFFENSE_OPPONENT", "Opponent", selected=F, choices = c(opponents,"WU")),
                uiOutput("OFFENSE_YEAR"),
                uiOutput("OFFENSE_GAMES"),
                uiOutput("OFFENSE_FORMATION_FILTER"),
                uiOutput("OFFENSE_FORMATIONS"),
                fluidRow(
                    box(title = "GAIN/LOSS", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("OFFENSE_GN_LS"))
                ),
                fluidRow(
                    box(title = "RUN/PASS VS. DOWN?", status = "primary", solidHeader = TRUE, width = 12,
                      plotOutput("OFFENSE_RUN_PASS", height="750px"))
                ),
                fluidRow(
                    box(title = "RUN TYPE VS. DOWN", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("OFFENSE_RUN_TYPE", height="750px"))
                ),
                fluidRow(
                    box(title = "TOP 3 RUN FORMATIONS", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("OFFENSE_FORM_RUN_HIST", height="800px"))
                ),
                fluidRow(
                  box(title = "RUN TYPE VS. FORMATION", status = "primary", solidHeader = TRUE, width = 12,
                      plotOutput("NEW_OFF_RUN_FORM_HEAT", height="600px"))
                ),
            ), # END OF OFFENSE TAB
    # ----------------------------------------- DEFENSE TAB -----------------------------------------
            tabItem(tabName = "DEFENSE",
                fluidRow(
                    column(12, 
                           tags$div("DEFENSE", style = "text-align: center; width: 100%; font-family: 'Lora', serif; font-weight: bold; font-size: 20px;"),   
                    ),
                ),
                HTML("<br>"),
                selectInput("DEFENSE_OPPONENT", "Opponent", selected=F, choices = c(opponents,"WU")),
                uiOutput("DEFENSE_YEAR"),
                uiOutput("DEFENSE_GAMES"),
                uiOutput("DEFENSE_DOWN_SITUATION"),
                uiOutput("DEFENSE_DIST_SITUATION"),
                uiOutput("DEFENSE_SUBMIT"),
                HTML("<br>"),
                fluidRow(
                    box(title = "GAIN/LOSS", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_GN_LS")
                    )
                ),
                fluidRow(
                    box(title = "TIME TILL THROW", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_TIME_TILL_THROW")
                    )
                ),
                fluidRow(
                    box(title = "TIME TILL CATCH", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_TIME_TILL_CATCH")
                    )
                ),
                fluidRow(
                    box(title = "TIME TILL PRESSURE", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_TIME_TILL_PRESSURE")
                    )
                ),
                fluidRow(
                    box(title = "YARDS AFTER CATCH", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_YACATCH")
                    )
                ),
                fluidRow(
                    box(title = "YARDS AFTER CONTACT", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_YACONTACT")
                    )
                ),
                fluidRow(
                    box(title = "PERSONNEL", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_PERSONNEL")
                    )
                ),
                fluidRow(
                    box(title = "FORMATIONS", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_FORMATION")
                    )
                ),
                fluidRow(
                    box(title = "BACKFIELD", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_BACKFIELD")
                    )
                ),
                fluidRow(
                    box(title = "BACKSET", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_BACK_SET")
                    )
                ),
                # fluidRow(
                #     box(title = "RUN/PASS", status = "primary", solidHeader = TRUE, width = 12,
                #         plotOutput("DEFENSE_RUN_PASS")
                #     )
                # ),
                fluidRow(
                    box(title = "RUN CODE", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_OFF_RUN_CODE")
                    )
                ),
                fluidRow(
                    box(title = "RUN TYPE", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_OFF_RUN_TYPE")
                    )
                ),
                fluidRow(
                    box(title = "OFF PROTECTION", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_OFF_PROTECTION")
                    )
                ),
                fluidRow(
                    box(title = "PASS CODE", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_OFF_PASS_CODE")
                    )
                ),
                fluidRow(
                    box(title = "PASS RTS", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_OFF_PASS_RTS")
                    )
                ),
                fluidRow(
                    box(title = "RPO", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_RPO")
                    )
                ),
                fluidRow(
                    box(title = "DEFENSIVE FRONT", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_DEF_FRONT")
                    )
                ),
                fluidRow(
                    box(title = "PASS RUSH", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_PASS_RUSH")
                    )
                ),
                # fluidRow(
                #     box(title = "QB", status = "primary", solidHeader = TRUE, width = 12,
                #         plotOutput("DEFENSE_QB_JERSEY")
                #     )
                # ),
                fluidRow(
                    box(title = "INTENDED TARGET", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_INTENDED_TARGET")
                    )
                ),
                fluidRow(
                    box(title = "MOTION", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_MOTION")
                    )
                ),
                fluidRow(
                    box(title = "MOTION TYPE", status = "primary", solidHeader = TRUE, width = 12,
                        plotOutput("DEFENSE_MOTION_TYPE")
                    )
                )

            ), # END OF DEFENSE TAB
    
            
            tabItem(tabName = "RECRUITING",
                    # Drop-down menu to filter year
                    HTML("<p style='color:white'>"),
                    selectInput("recruit_season", label = "Select year:",
                                choices = rev(WU_MAP_SEASONS),
                                width = '150px'),
                fluidRow(
                  box(title = "THE MAP", status = "primary", solidHeader = TRUE, width = 12, height=1200,
                      plotOutput("RECRUITING_MAP"),
                  )
                ),
                fluidRow(
                  box(title = "THE MAP BY POSITION", status = "primary", solidHeader = TRUE, width = 12,height=1200,
                      plotOutput("RECRUITING_MAP_POS",height = "1000px", width = "1000px"),
                  )
                )
            ) # END OF RECRUITING
        ) # END OF TAB ITEMS
    ) # END OF DASHBOARD BODY
) # END OF UI

# Run the application
shinyApp(ui = ui, server = server)

