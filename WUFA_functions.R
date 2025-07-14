## ----------------------------------------------------------------------------
## GENERAL FUNCTIONS
## ----------------------------------------------------------------------------
library(showtext)
font_add_google("Lora", family = "lora")

theme_set(theme_bw(base_family = "lora", base_size=15))

universities <<- tibble(
    ABBREV = c("LU", "UR", "GFU", "ULV", "WU", "PU", "WHIT", "SU", "SOU", "RU", "LC", "PLU"),
    FULL_NAME = c(
        "Linfield University", 
        "University of Redlands", 
        "George Fox University", 
        "University of La Verne", 
        "Willamette University", 
        "Pacific University", 
        "Whitworth University", 
        "Simpson University", 
        "Southern Oregon University", 
        "Redlands University",
        "Lewis & Clark",
        "Pacific Lutheran"
    ) 
)


f.full_name=function(abbrev){
    universities %>% filter(ABBREV==abbrev) %>% pull(FULL_NAME)
}


## ----------------------------------------------------------------------------
## OVERALL TAB FUNCTIONS
## ----------------------------------------------------------------------------


f.METRIC_TREND = function(filterds,the_metric,the_units="") {
    filterds %>%
        filter(METRIC==the_metric) %>%
        group_by(GAME_ID) %>%
        summarize(avg = mean(VALUE, na.rm = TRUE)) %>%
        ggplot(aes(x = GAME_ID, y = avg, group = 1)) +
            geom_point(color = wu_gold) +  # Scatter plot
            geom_line(color = wu_red2, linewidth = 1.10) + 
            labs(
                title = str_replace_all(the_metric,"_"," "),
                x = "",
                y = the_units) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_x_discrete(label=scales::label_wrap(11))
}


# f.METRIC_TREND(WUds,"TIME_TILL_CATCH","s")

f.OVERALL_SCORE <- function(filterds) {
  filterds %>% 
    mutate(across(Q1_HOME_SCORE:FINAL_AWAY_SCORE, as.numeric))	%>%
    mutate(
      WU_HOME=(HOME_TEAM=="WU")
    ) %>%   
    pivot_longer(Q1_HOME_SCORE:FINAL_AWAY_SCORE, names_to = "the_label", values_to = "SCORE") %>%
    filter(!str_detect(the_label, "FINAL")) %>%
    separate_wider_delim(the_label, delim = "_", names = c("QUARTER", "HOME_VS_AWAY", NA)) %>%
    mutate(
      QUARTER = factor(QUARTER, levels = c("Q1", "Q2", "Q3", "Q4", "OT", "2OT")),
      HOME_VS_AWAY = case_when(
        HOME_VS_AWAY == "HOME" ~ HOME_TEAM,
        HOME_VS_AWAY == "AWAY" ~ AWAY_TEAM
      ),
      TEAM = if_else(HOME_VS_AWAY == HOME_TEAM, HOME_TEAM, AWAY_TEAM)
    ) %>%
    ggplot(aes(x = TEAM, y = SCORE, fill = QUARTER)) +
    geom_col(width = 0.8) +
    facet_wrap(~GAME_ID, scales = "free_x") +
    theme(axis.text.x = element_text(hjust = 1)) +
    scale_fill_manual(values=color_vec)
} 

# f.OVERALL_SCORE(ds)



## ----------------------------------------------------------------------------
## OFFENSE TAB FUNCTIONS
## ----------------------------------------------------------------------------


f.OFFENSE_GN_LS = function(filterds, overallds, input_formations){
  
  # Create "OVERALL" group
  if ("OVERALL" %in% input_formations) {
    overall_summary <- overallds %>%
      filter(!is.na(DN), !is.na(OFF_T)) %>%
      mutate(FORMATION = "OVERALL")
    
    combined <- bind_rows(filterds, overall_summary)
  } else {
    combined <- filterds
  }
  
  combined <- combined %>%
    filter(!is.na(DN), !is.na(OFF_T)) %>%
    mutate(FORMATION = factor(FORMATION, levels = unique(c("OVERALL", as.character(FORMATION)))))
  
  ggplot(combined, aes(x = DN, y = GN_LS, fill = FORMATION)) +
    geom_boxplot(color = "black", width = .3, position = position_dodge(width = .5)) +
    scale_x_discrete(labels = scales::label_wrap(15)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(title = "GAIN/LOSS", x = "DOWN", y = "YDS") +
    facet_wrap(. ~ GAME_ID, ncol = 2) +
    scale_fill_manual(values = color_vec, name = "FORMATION")
}




f.OFFENSE_RUN_TYPE = function(filterds) {
    ds =  filterds %>%
        filter(!is.na(OFF_RUN_TYPE)) %>%
        count(GAME_ID, DN, OFF_RUN_TYPE) %>%
        filter(n>0) 
    
    
    xmax = max(ds$n)+1
    
    ds %>%
        ggplot(aes(y = fct_reorder(OFF_RUN_TYPE,n), x = n,fill=DN)) +
        geom_col(color = "black") + 
        
        scale_y_discrete(labels = scales:: label_wrap(7)) +
        labs(
            title = "RUN TYPE",
            y = "",
            x = "Frequency"
        ) +
        facet_wrap(~GAME_ID,ncol=2) +
        scale_fill_manual(values=color_vec,name="DOWN")
}



## FORMATION RUN SPLIT GRAPH
f.OFFENSE_FORM_RUN_HIST = function(filterds, overallds, input_formations) {
  
  if ("OVERALL" %in% input_formations) {
    overall_ds <- overallds %>%
      filter(!is.na(OFF_RUN_TYPE), !toupper(OFF_RUN_TYPE) == "NAN",
             !is.na(DN), !is.na(OFF_T)) %>%
      mutate(FORMATION = "OVERALL") %>%
      group_by(FORMATION, OFF_RUN_TYPE) %>%
      summarize(count = n(), .groups = 'drop')
  } else {
    overall_ds <- NULL
  }
  
  # Formation-specific summary
  formation_ds <- filterds %>%
    group_by(FORMATION, OFF_RUN_TYPE) %>%
    summarize(count = n(), .groups = 'drop')
  
  # Combine both
  run_ds <- bind_rows(formation_ds, overall_ds) %>%
    filter(!is.na(OFF_RUN_TYPE), !toupper(OFF_RUN_TYPE) == "NAN",
           !is.na(FORMATION), !toupper(FORMATION) == "NAN")
  
  # Get top 3 formations by total count (including OVERALL if present)
  top_formations <- run_ds %>%
    group_by(FORMATION) %>%
    summarize(total_count = sum(count)) %>%
    arrange(desc(total_count)) %>%
    slice_head(n = 3)
  
  filtered_top_runs <- run_ds %>%
    filter(FORMATION %in% top_formations$FORMATION)
  
  ggplot(filtered_top_runs, aes(x = fct_reorder(OFF_RUN_TYPE, count, .desc = TRUE), y = count, fill = FORMATION)) +
    geom_col(position = "dodge", color = "black") +
    labs(
      x = "Run Type", 
      y = "Frequency", 
      title = "Top 3 Run Formations") +
    
    scale_fill_manual(values = color_vec) +
    facet_wrap(~FORMATION, ncol = 2, scales = "free") +
    theme(axis.text.x = element_text(hjust = 1, angle = 45))
  
}





## do we want to do an overall for this? how would that look??
f.NEW_OFF_RUN_FORM_HEAT = function(filterds){
  # Initialize an empty list to store data frames for each game
  game_list = list() 
  game_vec = filterds %>%
    distinct(GAME_ID, FORMATION, OFF_RUN_TYPE) %>%
    filter(!is.na(FORMATION) & !is.na(OFF_RUN_TYPE)) %>%
    distinct(GAME_ID) %>%
    pull(GAME_ID)
  # Iterate over each unique GAME_ID
  for (i in game_vec) {
    # Create a subset of the dataset for the current GAME_ID
    ds_i = filterds %>% 
      filter(GAME_ID == i) %>% 
      select(GAME_ID, FORMATION, OFF_RUN_TYPE) %>% 
    filter(!is.na(FORMATION) & !is.na(OFF_RUN_TYPE)) %>%
      expand_grid(unique(GAME_ID), unique(FORMATION), unique(OFF_RUN_TYPE)) %>%
    filter(!is.na(FORMATION) & !is.na(OFF_RUN_TYPE))
      
    count_ds = filterds %>%
      filter(GAME_ID == i) %>%
      count(GAME_ID, FORMATION, OFF_RUN_TYPE)
    
    print(count_ds)
    
    ds_i = left_join(ds_i, count_ds, by = c("GAME_ID" = "GAME_ID", "FORMATION" = "FORMATION", "OFF_RUN_TYPE" = "OFF_RUN_TYPE")) 
    # Add a new column for the count (assuming you need this to create the heatmap)
    #ds_i$n = 0  # initialize a column for count, assuming you're counting occurrences
    
    # Join the current data frame to the corresponding game in game_list (using full_join)
    game_list[[i]] = ds_i
  }
  
  # Combine all game-specific data frames into a single data frame
  find_ds = bind_rows(game_list) %>%
    mutate(
      n=ifelse(is.na(n),0,n),
      # OFF_RUN_TYPE=ifelse(is.na(OFF_RUN_TYPE), "NONE", OFF_RUN_TYPE)
    )
  
  # Plot the heatmap using ggplot2
  ggplot(find_ds, aes(x = fct_reorder(OFF_RUN_TYPE, n, .desc = TRUE), 
                      y = fct_reorder(FORMATION, n, .desc = TRUE), fill = n)) +
    geom_tile(color = "black") +
    scale_fill_gradient2(low="yellow", mid="orange", high="red3", name="# of plays", limits=c(0,max(find_ds$n, na.rm = T))) +
    labs(
      title = "Heatmap of Run Types and Formations",
      x = "Run Type",
      y = "Formation",
      fill = "Count"
    ) +
    
    facet_wrap(~GAME_ID, ncol = 2, scales = "free") +
    theme(axis.text.x = element_text(hjust = 1))
}


## ----------------------------------------------------------------------------
## DEFENSE TAB FUNCTIONS
## ----------------------------------------------------------------------------


f.DEFENSE_NUMERIC = function(the_metric = "TIME_TILL_THROW", the_data = masterds,custom_y=NA,custom_title=NA) {
    ds=the_data %>%
        filter(METRIC == the_metric) %>%
        group_by(GAME_ID) %>%
        summarise(the_avg = mean(VALUE, na.rm = TRUE))
    
    ds %>%
        ggplot(aes(x = GAME_ID, y = the_avg, group = 1)) +
        geom_line(color = "darkred", linewidth=1.10) +
        geom_hline(yintercept = mean(ds$the_avg), linetype = "dotted", col = "black", linewidth = 1.05) +
        geom_point(size = 2.5) +
        labs(
            title = ifelse(is.na(custom_title),the_metric,custom_title),
            x="",
            y=ifelse(is.na(custom_y),"",custom_title),
        ) +
        
        scale_x_discrete(labels = scales::label_wrap(11))
}


f.DEFENSE_CATEGORICAL = function(the_metric = "PERSONNEL", the_data = masterds, custom_title = NA, custom_y = NA) {
    ds <- the_data %>%
        filter(METRIC == the_metric) %>%
        mutate(VALUE = as.character(VALUE),
               VALUE = if_else(VALUE == "", NA, VALUE)) %>%
        drop_na(VALUE) %>%
        group_by(DATE, GAME_ID, VALUE) %>%
        summarize(n = n()) %>%
        arrange(DATE, desc(n))
    
    ## update to remove this part
    keep_these <- ds %>%
        group_by(VALUE) %>%
        summarize(n = sum(n)) %>%
        arrange(desc(n)) %>%
        #filter(n > 2) %>%
        pull(VALUE)
    ##
    
    proper_order <- ds %>%
        distinct(DATE, GAME_ID) %>%
        pull(GAME_ID)
    
    ds %>%
        filter(VALUE %in% keep_these) %>%
        mutate(VALUE = factor(VALUE, levels = rev(keep_these)),
               GAME_ID = factor(GAME_ID, levels = proper_order)) %>%
        ggplot(aes(x = n, y = VALUE)) +
        geom_col(color = "black", fill = "darkred") +
        facet_grid(. ~ GAME_ID) +
        
        labs(title = ifelse(is.na(custom_title), the_metric, custom_title),
             x = "Frequency",
             y = ifelse(is.na(custom_y), "", custom_y)
        )
}


## ----------------------------------------------------------------------------
## RECRUITING FUNCTIONS
## ----------------------------------------------------------------------------

f.RECRUITING_MAP=function(filterds=the_roster, the_season, col1="white",col2="yellow",col3="red3"){
  STATE_COUNT <- filterds %>%
    group_by(STATE) %>%
    summarize(n=n())
  
  # joining state data!
  states <- statepop %>%
    full_join(STATE_COUNT, by = c("abbr" = "STATE"))  
  
  plot_usmap(data = states, values = "n", labels_color = "black") +
    theme_void()  +
    labs(
      title = "Where are our players coming from?",
      subtitle = paste(the_season,"SEASON")
    )+
    scale_fill_gradient2(low=col1,mid=col2,high=col3, name="# players") +
    theme(
      text=element_text(family = "lora"),
      plot.title = element_text(face = "bold", size = 16, vjust=1),
      plot.subtitle=element_text(
        size = 14, color="cyan4")
    )
}

f.RECRUITING_MAP_POS=function(filterds=the_roster, the_season, col1="white",col2="yellow",col3="red3"){
  STATE_COUNT <- filterds %>%
    group_by(STATE, POS) %>%
    summarize(n=n())
  
  
  # all possible combinations of state and position
  positions=filterds %>% distinct(POS)
  the_states=statepop[,c("abbr","fips")]
  
  ds=expand_grid(positions,the_states)

  states=left_join(ds,STATE_COUNT, by = c("abbr" = "STATE","POS"="POS"))  
  
  plot_usmap(data = states, values = "n", labels_color = "black") +
    theme_void()  +
    labs(
      title = "Where are our players coming from?",
      subtitle = paste(the_season,"SEASON")
    )+
    facet_wrap(~ POS) +
    scale_fill_gradient2(low=col1,mid=col2,high=col3, name="# players") +
    theme(
      text=element_text(family = "lora"),
      plot.title = element_text(face = "bold", size = 16, vjust=1),
      plot.subtitle=element_text(
        size = 14, color="cyan4"),
      aspect.ratio = 0.6
    )
}



