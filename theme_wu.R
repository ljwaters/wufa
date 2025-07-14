library(tidyverse)
library(showtext)
# font_add_google("Lora", family = "lora")
# font_add_google("Crimson Text", family = "crimson")
font_add_google("Aldrich", family = "aldrich")
showtext_auto()


theme_wu=function(default_size=18, the_font="aldrich"){
	theme( 
		text=element_text(family = the_font,size=default_size),
		plot.title = element_text(face = "bold", size = default_size*1.5, vjust=1),
		plot.subtitle=element_text(
			face = "italic", size = default_size*1.2, color="#BA0C2F"),
		axis.title=element_text(face = "bold", color="black"),
		axis.title.x=element_text(vjust=-1),
		axis.title.y=element_text(vjust=1),
		strip.text = element_text(face="bold", size=default_size*1.15,color="white"),
		plot.caption.position = "plot",
        plot.caption = element_text(vjust = -2, hjust = 1),
		plot.background = element_rect(	fill = "gray95", colour = "black"),
		panel.border = element_rect(
			color="black",fill=NA,linewidth = 0.5),
		panel.background=element_rect(fill = "white",color=NA),
		strip.background = element_rect(fill="black",color="black"),
		panel.grid.major = element_line(color="gray92"),
		panel.grid.minor=element_line(color="gray100"),
		# legend.position = "bottom",
		legend.title = element_text(face="bold",size=default_size*0.98),
		legend.text = element_text(size=default_size*0.95),
		legend.background = element_rect(fill="transparent",color=NA),
		legend.box.background = element_rect(
			color="black", fill="white"),
		legend.key.size = unit(0.4, "cm"),
		panel.spacing = unit(0.05, "in"),
		plot.margin = margin(
			t = 25,  # top margin
	        r = 12.5,  # right margin
	        b = 20,  # bottom margin
	        l = 12.5   # left margin
		)
	) 
}

wu_red="#BA0C2F"
wu_darkblue="#003B5C"
wu_gold="#C6AA76"
wu_lightteal="#67D2DF"
wu_red2="#9C0221"  # rgb(156, 2, 33, maxColorValue = 255)
wu_teal="#1C8587"  # rgb(28, 123, 135, maxColorValue = 255)
wu_skyblue="#2EB4FF" # rgb(46,180,255, maxColorValue = 255)
color_vec<<-c(wu_red2,wu_darkblue,wu_gold,wu_lightteal,"gray40",wu_skyblue,"darkorchid4","black")
