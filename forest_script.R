library("tidyverse")
library("gridExtra")

forest_data_bej <- read_csv("data/Algerian_forest_fires.csv", skip = 1, n_max = 122) %>% mutate(region = "Bejaia") %>% rowid_to_column("day_abs")
forest_data_sid <- read_csv("data/Algerian_forest_fires.csv", skip = 126) %>% mutate(region = "Sidi-Bel Abbes") %>%  rowid_to_column("day_abs")
forest_data <- rbind(forest_data_bej, forest_data_sid) %>% select(-year)
forest_data$FWI <- as.double(forest_data$FWI)
forest_data$month <- as.integer(forest_data$month)

forest_data %>% filter(Classes == "fire") %>% select(month, Temperature, Rain, FWI, region)

forest_data %>% group_by(Classes) %>% summarize_at(vars(Temperature, Rain, FFMC, DMC, DC, ISI, BUI, FWI), list(avg = mean))

####

#### PLOTS CON JITTER DE FWI Y TEMPEREATURA

p <- ggplot(forest_data)

p + aes(x = Temperature, y = FWI, colour = Classes) + geom_jitter()
ggsave("FWIvsT.png")

p + aes(x = Temperature, y = FWI, colour = Classes) + geom_jitter() + facet_wrap(Classes~region)
ggsave("FWIvsT_wrap.png")

###

##### COSAS EN FUNCIóN DEL DIA

p + aes(x = day_abs, y = Temperature, colour = Classes) + geom_line() + geom_point() + facet_grid(Classes~region)
ggsave("TvsDia_facet.png")

p + aes(x = day_abs, y = Rain) + geom_smooth()  + facet_grid(~region)
ggsave("RainvsDia_facet.png")

p + aes(x = day_abs, y = FWI, colour = Classes) + geom_jitter() + geom_smooth() 
ggsave("FWIvsDia.png")

###

### COSAS AGRUPADAS POR TEMPERATURA

data_byT <- forest_data %>% group_by(Temperature) %>% summarize_at(vars(FFMC, DMC, DC, ISI, BUI, FWI), list(avg = mean))

ggplot(data_byT) + aes(x = Temperature, y = FFMC_avg) + geom_line() + geom_point(size = 4)
ggsave("FFMCavgvsTemp.png")

ggplot(data_byT) + aes(x = Temperature, y = FWI_avg) + geom_point() + geom_smooth()  
ggsave("FWIavgvsTemp.png")

p1 <- ggplot(data_byT) + aes(x = Temperature, y = FFMC_avg) + geom_point() + geom_smooth()  
p2 <- ggplot(data_byT) + aes(x = Temperature, y = DMC_avg) + geom_point() + geom_smooth()  
p3 <- ggplot(data_byT) + aes(x = Temperature, y = DC_avg) + geom_point() + geom_smooth()  
p4 <- ggplot(data_byT) + aes(x = Temperature, y = ISI_avg) + geom_point() + geom_smooth()  
p5 <- ggplot(data_byT) +aes(x = Temperature, y = BUI_avg) + geom_point() + geom_smooth()  
p6 <- ggplot(data_byT) + aes(x = Temperature, y = FWI_avg) + geom_point() + geom_smooth()

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
g <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol=2) #
ggsave(file="grid_Temperature.png", g) 

###

### COSAS AGRUPADAS PoR MES

a <- forest_data %>% group_by(month, Classes) %>% summarize_at(vars(Temperature, Rain, FFMC, DMC, DC, ISI, BUI, FWI), list(avg = mean))
a1 <- ggplot(a) + aes(month, Temperature_avg, colour = Classes) + geom_point( size = 6)+ geom_line(linewidth = 3)
a2 <- ggplot(a) + aes(month, Rain_avg, colour = Classes) + geom_point( size = 6)+ geom_line(linewidth = 3)
a3 <- ggplot(a) + aes(month, FWI_avg, colour = Classes) + geom_point( size = 6)+ geom_line(linewidth = 3)

grid.arrange(a1, a2, a3, ncol = 1)
g <- arrangeGrob(a1, a2, a3, ncol=1) #
ggsave(file="grid_month1.png", g) 

b <- forest_data %>% group_by(month, region) %>% summarize_at(vars(Temperature, Rain, FFMC, DMC, DC, ISI, BUI, FWI), list(avg = mean))
b1 <- ggplot(b) + aes(month, Temperature_avg, colour = region) + geom_point( size = 6)+ geom_line(size = 3) 
b2 <- ggplot(b) + aes(month, Rain_avg, colour = region) + geom_point( size = 6)+ geom_line(size = 3)
b3 <- ggplot(b) + aes(month, FWI_avg, colour = region) + geom_point( size = 6)+ geom_line(size = 3)

grid.arrange(b1, b2, b3, ncol = 1, bottom  = "Month")
g <- arrangeGrob(b1, b2, b3, ncol=1) #
ggsave(file="grid_month2.png", g) 

###

#### CAMBIOS ESTÉTICOS


ggplot(forest_data %>% group_by(month) %>% summarise(rain_avg = mean(Rain))) +
  geom_col(aes(month, rain_avg, alpha = rain_avg), fill = "royalblue4",  width = 0.7, just = 0.43, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.2,1), limits = c(0, 1)) + 
  geom_text(aes(x = month, y= rain_avg,  label = round(rain_avg, digits = 2)), hjust = 1, nudge_y = -0.05, nudge_x = 0.07, fontface = "bold", size = 9, colour = "white") +
  coord_flip() +
  theme_void() + 
  theme(plot.title = element_text(size = 25, hjust = 0, face = "bold"), plot.subtitle = element_text(size = 15, hjust = 0), axis.text.y = element_text(size = 17, hjust = 1), plot.margin = margin(rep(10, 4)), plot.background = element_rect(fill = "white")) + 
  labs(title = "Promedio de lluvia por mes", subtitle = "Entre junio y septiembre") 

ggsave("promedioLluviaxmes.png")



ggplot(forest_data %>% group_by(month) %>% summarise(FWI_avg = mean(FWI))) +
  geom_col(aes(month, FWI_avg, alpha = FWI_avg), fill  = "firebrick", width = 0.7, just = 0.43, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.1,1), limits = c(0, 10)) + 
  coord_flip() + 
  geom_text(aes(x = month, y= FWI_avg,  label = round(FWI_avg, digits = 2)), hjust = 1, nudge_y = -0.25, nudge_x = 0.07, fontface = "bold", size = 9, colour = "white", stroke = "") +
  theme(plot.title = element_text(size = 25, hjust = 0, face = "bold"), 
        plot.subtitle = element_text(size = 15, hjust = 0),
        axis.text.y = element_text(size = 30, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(hjust=0),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(rep(25, 4))) + 
  labs(title = "Promedio de FWI por mes", subtitle = "Entre junio y septiembre", x = "Mes", y = "FWI") 

ggsave("promedioFWIxmes.png")

