library(tidyverse)
library(readxl)


f.boxscore=function(folder_path="/cloud/project/WUFA/Boxscores"){
	files = list.files(folder_path, full.names = TRUE)
	
	data_list = list()
	
	for (file in files) {
	    data_list[[file]] = read_excel(file)
	}
	
	boxscoreds = bind_rows(data_list)
  
	boxscoreds=boxscoreds %>%
	    mutate(
	      GAME_ID=paste(HOME_TEAM,"vs.",AWAY_TEAM,format(DATE,"%m/%d/%y")),
	    )
	
	GAME_ID_LEVELS = boxscoreds %>%
	  distinct(DATE, GAME_ID) %>%
	  na.omit() %>%
	  arrange(DATE) %>%
	  mutate(
	    GAME_ID=factor(GAME_ID, ordered =TRUE)
	  ) %>% pull(GAME_ID)

	
	boxscoreds=boxscoreds %>%
	  mutate(
	    GAME_ID=factor(GAME_ID,levels=GAME_ID_LEVELS)
	  )
	boxscoreds
}


# PASS_RUSH, RPO - data quality issues?.  UPS vs. GFU -> need date

