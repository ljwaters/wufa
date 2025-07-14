library(tidyverse)
library(readxl)


## CREATING VARIABLE TYPES
numeric_cols <<- c("YARD_LN", "DIST", "GN_LS", 
                   "TIME_TILL_THROW", "TIME_TILL_CATCH",
                   "TIME_TILL_PRESSURE", "YACATCH", "YACONTACT")


f.masterds=function(folder_path){
  folder_path="/cloud/project/WUFA/Datasets"
  
  files = list.files(folder_path, full.names = TRUE)
  
  correct_game_details=tibble(files) %>%
    mutate(filenames=files) %>%
    separate_wider_delim(files,delim="_",names=c("DATE","HOME",NA,"AWAY")) %>%
    mutate(
      DATE=str_remove_all(DATE,folder_path),
      DATE=ymd(str_remove_all(DATE,"/")),
      AWAY=str_remove_all(AWAY,".xlsx")
    )
  
  data_list = list()
  
  for (file in files) {
    data_list[[file]] = tibble(filenames=file,read_excel(file, col_types = "text"))
  }
  
  
  masterds=bind_rows(data_list) %>%  select(-DATE,-YEAR,-HOME,-AWAY)
  
  masterds=left_join(correct_game_details,masterds,by=c("filenames"="filenames")) %>%
    select(-filenames)
  
  ## DATA WRANGLING
  masterds=masterds	%>%
    mutate(
      YEAR=year(DATE),
      GAME_ID=paste(HOME,"vs.",AWAY,format(DATE, "%m-%d-%y")),
      across(everything(), ~ toupper(.))) %>%
    mutate( 
      # across(everything(), ~ trimws(.)),
      across(everything(), ~ if_else(.=="NAN",NA,.))
    ) %>%
    mutate( 
      MOTION_DIRECTION=case_when(
        is.na(MOTION_DIRECTION) ~ NA,
        str_detect(MOTION_DIRECTION,"L") ~ "LEFT",
        str_detect(MOTION_DIRECTION,"R") ~ "RIGHT"
      ),
      HASH=case_when(
        str_detect(HASH,"L") ~ "LEFT",
        str_detect(HASH,"R") ~ "RIGHT",
        str_detect(HASH,"M") ~ "MIDDLE"
      ),
      DN=if_else(DN=="P" | as.numeric(DN)==0,"1.0",DN),
      DN=as.character(as.numeric(DN)),
      DIST = as.numeric(DIST),
      DIST_CAT = case_when(
        DN == 1 & DIST == 10.0 ~ "10",
        DN == 2 & DIST %in% 1.0:3.0 ~ "1-3",
        DN == 2 & DIST %in% 4.0:6.0 ~ "4-6",
        DN == 2 & DIST %in% 7.0:10.0 ~ "7-10",
        DN == 2 & DIST >= 11.0 ~ "11+",
        DN == 3 & DIST %in% 1.0:2.0 ~ "1-2",
        DN == 3 & DIST == 3.0 ~ "3",
        DN == 3 & DIST %in% 4.0:6.0 ~ "4-6",
        DN == 3 & DIST %in% 7.0:10.0 ~ "7-10",
        DN == 3 & DIST >= 11.0 ~ "11+",
        DN == 4 ~ "any",
        TRUE ~ NA_character_
      )
    ) %>%
    relocate(GAME_ID,.after=AWAY) %>%
    relocate(YEAR,.after=DATE) %>%
    select(-QB_DROP, -THROW_START, -THROW_FINISH) %>%
    filter((!is.na(`PLAY #`)) & !(is.na(DATE)))
  
  names(masterds)=str_replace_all(names(masterds), " ", "_")
  
  #print(names(masterds))
  print(unique(masterds$GAME_ID))
  masterds=masterds %>%
    mutate(
      across(all_of(numeric_cols), ~as.numeric(.))
    )
  
  # SETTING THE GAME ORDER TO BE CHRONOLOGICAL
  GAME_ID_LEVELS = masterds %>%
    distinct(DATE, GAME_ID) %>%
    na.omit() %>%
    arrange(DATE) %>%
    mutate(
      GAME_ID=factor(GAME_ID, ordered =TRUE)
    ) %>% pull(GAME_ID)
  
  print(GAME_ID_LEVELS)
  masterds = masterds %>%
    mutate(
      GAME_ID= factor(GAME_ID, levels= GAME_ID_LEVELS)
    )
  masterds
}

# PASS_RUSH, RPO - data quality issues?.  UPS vs. GFU -> need date

