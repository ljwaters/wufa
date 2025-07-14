server = function(input, output, session) {
    # CREATE DIST. OPTIONS BASEDE ON def_down SELECTION
    first_down = c("10" = "10")
    second_down = c("1-3" = "1-3", "4-6" = "4-6","7-10" = "7-10", "11+" = "11+")
    third_down = c("1-2" = "1-2", "3" = "3", "4-6" = "4-6","7-10" = "7-10","11+" = "11+")
    
    ID_cols <<- c("DATE","YEAR","HOME","AWAY","GAME_ID","OFF_T", "PLAY_#")
    
    categorical_cols <<- setdiff(names(masterds),c(ID_cols,numeric_cols))

  ## ------------------------------ OVERALL TAB ------------------------------ 

    observeEvent(input$season, {

    
    # Season Stats for Overview Header
        output[['HEADER_TEXT']] = renderText(
            paste(input$season,"SEASON"))
        
        
        WU_season = WUds %>%  filter(YEAR==input$season) 
        
        print(WU_season)
    
    # Change to WU when our ds grows
    WUoffense <<- WU_season %>% filter(OFF_T == "WU")
    WUdefense <<- WU_season %>% filter(OFF_T != "WU")
    
    WUboxscore <<- boxscoreds %>% filter(YEAR==input$season)
    
        output[['OVERALL_TIME_TILL_THROW']] = renderPlot({
            f.METRIC_TREND(WUdefense,"TIME_TILL_THROW","seconds")
        }) 
        
        output[['OVERALL_TIME_TILL_CATCH']] = renderPlot({
            f.METRIC_TREND(WUdefense,"TIME_TILL_CATCH","seconds")
        }) 
        
        output[['OVERALL_TIME_TILL_PRESSURE']] = renderPlot({
            f.METRIC_TREND(WUdefense,"TIME_TILL_PRESSURE","seconds")
        }) 
        
        output[['OVERALL_GN_LS']] = renderPlot({
            f.METRIC_TREND(WUdefense,"GN_LS","yds")
        }) 
        
        output[['YACONTACT']] = renderPlot({
            f.METRIC_TREND(WUdefense,"YACONTACT","yds")
        })
        
        output[['YACATCH']] = renderPlot({
            f.METRIC_TREND(WUdefense,"YACATCH","yds")
        }) 
        
        if(nrow(WUboxscore)>0){
          output[["OVERALL_SCORES"]] = renderPlot({ f.OVERALL_SCORE(WUboxscore)})
        }
           
  })
  
  
  
  # ----------------------------------------------------------------------------
  # DEFENSE TAB
  # ----------------------------------------------------------------------------

    DEFENSE_filter_ds=NULL
  
  
    filterds_long_numeric=NULL
    filterds_long_categorical=NULL
  
  # Render in the numeric/categorical plots as NULL initially
    for (cur in numeric_cols) {
        output[[paste("DEFENSE", cur, sep="_")]] = NULL
    }
    for (cur in categorical_cols) {
        output[[paste("DEFENSE",cur,sep="_")]] = NULL
    }
      
    numeric_cols<<-c("GN_LS", "TIME_TILL_THROW","TIME_TILL_CATCH",
                   "TIME_TILL_PRESSURE","YACATCH","YACONTACT")  
    categorical_cols<<-setdiff(categorical_cols,c("ODK","HASH","DN", "DIST_CAT"))
  
  # CREATING THE REACTIVE YEAR UI
    observeEvent(input$DEFENSE_OPPONENT, {
        def_opponent<-input$DEFENSE_OPPONENT
        DEFENSE_filter_ds<<-masterds %>% 
            filter(
                str_detect(GAME_ID,input$DEFENSE_OPPONENT), 
                OFF_T == input$DEFENSE_OPPONENT
            )
        DEFENSE_YEAR_CHOICES = DEFENSE_filter_ds %>% distinct(YEAR) %>% pull()
      
        output[['DEFENSE_YEAR']] = renderUI({
            selectInput("DEFENSE_YEAR", "Select year", choices = DEFENSE_YEAR_CHOICES, multiple = TRUE)
        })
    })
  
    # CREATING THE GAMES UI
    observeEvent(input$DEFENSE_YEAR, {
        def_year<-input$DEFENSE_YEAR
        DEFENSE_filter_ds<<-masterds %>% 
            filter(
                str_detect(GAME_ID,input$DEFENSE_OPPONENT),
                YEAR %in% input$DEFENSE_YEAR,
                OFF_T == input$DEFENSE_OPPONENT
            )
        DEFENSE_GAME_CHOICES = DEFENSE_filter_ds %>% distinct(GAME_ID) %>% pull()
        
        output[['DEFENSE_GAMES']] = renderUI({
            selectInput("DEFENSE_GAMES", "Select game(s)", choices = DEFENSE_GAME_CHOICES, multiple = TRUE)
        })
    })
  
    # CREATING THE SITUATION UI
    observeEvent(input$DEFENSE_GAMES, {
        def_games<-input$DEFENSE_GAMES
        DEFENSE_filter_ds<<-masterds %>% 
            filter(
                str_detect(GAME_ID,input$DEFENSE_OPPONENT),
                YEAR %in% input$DEFENSE_YEAR,
                GAME_ID %in% input$DEFENSE_GAMES,
                OFF_T == input$DEFENSE_OPPONENT
            )
        DEFENSE_DN_CHOICES = c(1:4,"overall")
        DEFENSE_DIST_CHOICES = unique(na.omit(masterds$DIST_CAT))
        
        output[['DEFENSE_DOWN_SITUATION']] = renderUI({ #changing the name to DOWN_SITUATION
            radioButtons("DEFENSE_DOWN_SITUATION", label="Down", inline=T,choices = DEFENSE_DN_CHOICES) #changed more names
        })
    })
  
    observeEvent(input$DEFENSE_DOWN_SITUATION, {
        def_down<-input$DEFENSE_DOWN_SITUATION

        
        if(def_down=="overall"){
            DEFENSE_filter_ds<<- masterds %>% 
                filter(
                    str_detect(GAME_ID,input$DEFENSE_OPPONENT),
                    YEAR %in% input$DEFENSE_YEAR,
                    GAME_ID %in% input$DEFENSE_GAMES,
                    OFF_T == input$DEFENSE_OPPONENT
                )
        } else{
            DEFENSE_filter_ds<<- masterds %>% 
                filter(
                    str_detect(GAME_ID,input$DEFENSE_OPPONENT),
                    YEAR %in% input$DEFENSE_YEAR,
                    GAME_ID %in% input$DEFENSE_GAMES,
                    DN %in% input$DEFENSE_DOWN_SITUATION,
                    OFF_T == input$DEFENSE_OPPONENT
                )
            
        }

        if(def_down=="P"|def_down=="1"){
            var_choice = first_down
        }
        
        if(def_down=="2"|def_down=="overall"){
            var_choice = second_down
        }
        
        if(def_down=="3"|def_down=="4"){
            var_choice = third_down
        }
        
        
        output[['DEFENSE_DIST_SITUATION']] = renderUI({
            checkboxGroupInput("DEFENSE_DIST_SITUATION", label = "Distance", inline=T, 
                             choices = var_choice)
        })
        output[['DEFENSE_SUBMIT']] = renderUI({
            actionButton("DEFENSE_SUBMIT", "Submit", 
                 style = "background-color: darkred; color: white; font-weight: bold;"
            )
        })
    })
  
    observeEvent(input$DEFENSE_SUBMIT, {
        def_situation<-input$DEFENSE_DIST_SITUATION
        
        if(length(input$DEFENSE_DIST_SITUATION) > 0){
            DEFENSE_filter_ds<<-masterds %>%
                filter(
                    str_detect(GAME_ID,input$DEFENSE_OPPONENT),
                    YEAR %in% input$DEFENSE_YEAR,
                    GAME_ID %in% input$DEFENSE_GAMES,
                    DN==input$DEFENSE_DOWN_SITUATION,
                    DIST_CAT %in% input$DEFENSE_DIST_SITUATION,
                    OFF_T == input$DEFENSE_OPPONENT
                )
        }
        print("SUBMIT!")
      
        ds_pivot <<- DEFENSE_filter_ds %>%
            mutate(across(all_of(numeric_cols), as.character)) %>%
            pivot_longer(
                cols = all_of(c(categorical_cols, numeric_cols)),
                names_to = "METRIC", values_to = "VALUE"
            ) %>%
            select(OFF_T, DATE, GAME_ID, YEAR, DIST_CAT, DN , METRIC, VALUE)
      
        print(head(ds_pivot,20))
        # PIVOT AND SPLIT DEFENSE_filter_ds BY CAT/NUM
      
        DEFENSE_long_categorical <<- ds_pivot %>%
            filter(METRIC %in% categorical_cols)

        DEFENSE_long_numeric <<- ds_pivot %>%
            filter(METRIC %in% numeric_cols)%>%
            mutate(VALUE = as.numeric(VALUE))
      # 
      # 
      # # RENDER IN THE PLOTS
        for (i in categorical_cols) {
            local({
                cur <- i
                if (sum(is.na(DEFENSE_filter_ds[[cur]])) == length(DEFENSE_filter_ds[[cur]])) {
                    output[[paste("DEFENSE", cur, sep = "_")]] <- renderPlot({
                        NULL
                    })
                    print(cur)
                } else {
                    output[[paste("DEFENSE", cur, sep = "_")]] <- renderPlot({
                        f.DEFENSE_CATEGORICAL(
                            the_metric = cur,
                            #custom_title = cur, # categorical_df[cur, "categorical_title"],
                            the_data = DEFENSE_long_categorical
                        )
                    })
                }
            })
        }

        for (i in numeric_cols) {
            local({
                cur <- i
                if (sum(is.na(DEFENSE_filter_ds[[cur]])) == length(DEFENSE_filter_ds[[cur]])) {
                    output[[paste("DEFENSE", i, sep = "_")]] <- renderPlot({
                          NULL
                    })
                } else  {
                    output[[paste("DEFENSE", cur, sep = "_")]] <- renderPlot({
                        f.DEFENSE_NUMERIC(
                            the_metric = cur,
                            # custom_title = numeric_df[cur, "numeric_title"],
                            # custom_y = numeric_df[cur, "numeric_y_label"],
                            the_data = DEFENSE_long_numeric
                        )
                    })
                }
            })
        }
 })
  
  # ----------------------------------------------------------------------------
  # OFFENSE TAB
  # ----------------------------------------------------------------------------

    OFFENSE_filter_ds = NULL
  
  # CREATING THE REACTIVE OPPONENT UI
      observeEvent(input$OFFENSE_OPPONENT, {
            OFFENSE_filter_ds <<- masterds %>% 
                filter(str_detect(GAME_ID, input$OFFENSE_OPPONENT))
            
            OFFENSE_YEAR_CHOICES = OFFENSE_filter_ds %>% distinct(YEAR) %>% pull()

            output[['OFFENSE_YEAR']] = renderUI({
                selectInput("OFFENSE_YEAR", "Select year", choices = OFFENSE_YEAR_CHOICES, 
                            multiple = TRUE)
            })
      })
      
      # Offensive Year Reactivity
      observeEvent(input$OFFENSE_YEAR, {
            OFFENSE_filter_ds <<- masterds %>% 
                filter(
                    str_detect(GAME_ID, input$OFFENSE_OPPONENT), 
                    YEAR %in% input$OFFENSE_YEAR,
                    str_detect(GAME_ID, "WU")
                )
            
            OFFENSE_GAME_CHOICES = OFFENSE_filter_ds %>% distinct(GAME_ID) %>% pull()

            output[['OFFENSE_GAMES']] = renderUI({
                selectInput("OFFENSE_GAMES", "Select game", choices = OFFENSE_GAME_CHOICES, 
                        multiple = TRUE)
            })
      })
      # Offensive Formation Reactivity
      observeEvent(input$OFFENSE_GAMES, {
        output$OFFENSE_FORMATION_FILTER <- renderUI({
          selectInput("OFFENSE_FORMATION_FILTER", "Formation Filter", 
                      choices = c("ALL", "Top 3", "Top 5", "Top 7"))
        })
      })
      observeEvent(input$OFFENSE_FORMATION_FILTER, {
        OFFENSE_filter_ds <- masterds %>%
          filter(
            str_detect(GAME_ID, input$OFFENSE_OPPONENT), 
            YEAR %in% input$OFFENSE_YEAR,
            GAME_ID %in% input$OFFENSE_GAMES
          )
        
        TOP_FORM_CHOICES <- OFFENSE_filter_ds %>%
          drop_na(FORMATION) %>%
          count(FORMATION, sort = TRUE)
        
        FORM_LIMIT <- switch(input$OFFENSE_FORMATION_FILTER,
                        "Top 3" = 3,
                        "Top 5" = 5,
                        "Top 7" = 7,
                        "ALL" = nrow(TOP_FORM_CHOICES))
        
        OFFENSE_FORM_CHOICES <- TOP_FORM_CHOICES %>%
          slice_head(n = FORM_LIMIT) %>%
          pull(FORMATION)
        
        output$OFFENSE_FORMATIONS <- renderUI({
          selectInput("OFFENSE_FORMATIONS", "Select formation", 
                      choices = c('OVERALL', OFFENSE_FORM_CHOICES), 
                      multiple = TRUE)
        })
      })
      
       # Offensive Situation Reactivity
      observeEvent(input$OFFENSE_FORMATIONS, {
        
        OFFENSE_filter_ds_overall <<- masterds %>%
          filter(
            str_detect(GAME_ID, input$OFFENSE_OPPONENT), 
            YEAR %in% input$OFFENSE_YEAR, 
            GAME_ID %in% input$OFFENSE_GAMES
          )
        
        # Get just the user-requested formations, excluding OVERALL
        selected_formations <- setdiff(input$OFFENSE_FORMATIONS, "OVERALL")
        
        OFFENSE_filter_ds <<- OFFENSE_filter_ds_overall %>%
          filter(FORMATION %in% selected_formations)
        
      
            # MAKING GRAPHS
            output[["OFFENSE_GN_LS"]] = renderPlot({f.OFFENSE_GN_LS(OFFENSE_filter_ds, OFFENSE_filter_ds_overall, input$OFFENSE_FORMATIONS)})
            output[["OFFENSE_RUN_TYPE"]]=renderPlot({f.OFFENSE_RUN_TYPE(OFFENSE_filter_ds)  })  
            output[["OFFENSE_RUN_PASS"]]=renderPlot({f.OFFENSE_RUN_PASS(OFFENSE_filter_ds, OFFENSE_filter_ds_overall)  }) 
            output[["OFFENSE_FORM_RUN_HIST"]] = renderPlot({f.OFFENSE_FORM_RUN_HIST(OFFENSE_filter_ds, OFFENSE_filter_ds_overall, input$OFFENSE_FORMATIONS)})
            output[["NEW_OFF_RUN_FORM_HEAT"]]=renderPlot({f.NEW_OFF_RUN_FORM_HEAT(OFFENSE_filter_ds)})
            
      })
 # ----------------------------------------------------------------------------
 # RECRUITING
 # ----------------------------------------------------------------------------
      RECRUIT_filterds = NULL
      observeEvent(input$recruit_season, {
        
        RECRUIT_filterds=the_roster %>% filter(SEASON==input$recruit_season)
        output[["RECRUITING_MAP"]]=renderPlot({f.RECRUITING_MAP(RECRUIT_filterds,input$recruit_season)})
        output[["RECRUITING_MAP_POS"]]=renderPlot({f.RECRUITING_MAP_POS(RECRUIT_filterds,input$recruit_season)})
      })
}


#YEAR %in% input$OFFENSE_YEAR
