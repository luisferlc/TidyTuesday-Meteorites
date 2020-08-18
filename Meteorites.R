meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")
view(meteorites)

#Ver los meteoritos que han caído (fell)
#Ver los meteoritos que cayeron en algún tiempo y después se encontraron (found)
#Ver los meteoritos por clase, para saber qué clase es la más/menos común (class)
#Ver los meteoritos por tamaño. Puede ser que en una región existan patrones (mass)

library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)


###########Creating map for Mexico
map <- map_data("world")

head(map)
tail(map)

map %>%
  filter(region == "Mexico") %>%
  arrange(long) %>%
  view()

map2 <- map %>%
  filter(region == "Mexico")


######################Exploring the data set of meteorites
head(meteorites)

#Finding NA's and filtering by the Mexico region: meteorites
meteorites <- meteorites %>%
filter(!is.na(geolocation)) %>%
  filter(between(lat,13,32),
         between(long,-120,-85)
         ) %>%
  mutate(mass = mass/1000)


view(meteorites)

#Top 10 classes most repeated: meteorites2 
meteorites %>%
  group_by(class) %>%
  summarise(
    Count = n()
  ) %>%
  arrange(desc(Count))

meteorites2 <- meteorites %>%
  filter(class %in% c("H5","Iron, IIIAB","L6","H4","H6","Iron, IIAB","Iron, ungrouped","L4","L5","Iron"))

##############Mapping meteors fallen in Mex (fell and found), and their mass in kg: p
p  <- ggplot(map2) +
  geom_polygon(aes(x=long, y = lat, group = group), fill ="#4b4453", color="black") +
  coord_fixed(xlim = c(-120,-85), ylim = c(13,33), ratio =1.2)


p + geom_point(meteorites, mapping=aes(x=long, y=lat, size = mass, color=fall), alpha = .6) +
      scale_color_manual(values=c("#00886d", "#2f057e")) +
    facet_wrap(~fall) +
    theme(
      axis.text = element_text(color = "black"),
      axis.title = element_text(face="bold"),
      plot.background = element_rect(fill = "white"), 
      plot.caption = element_text(color = "black"),
      plot.title = element_text(size = 15, face = "bold")
    ) +
    labs(title="Meteorites fall in Mexico",x="Longitude", y="Latitude", caption= "Data: Meteoritical Society by way of NASA \n Made by @ShiXiong3") +
    labs(color = "Fall", size = "Mass(kg)") 
      

#############Mapping the top 10 classes most repeated in Mexico: p2
p2  <- ggplot(map2) +
  geom_polygon(aes(x=long, y = lat, group = group), fill ="white", color="black") +
  coord_fixed(xlim = c(-120,-85), ylim = c(13,33), ratio =1.2)

p2 + geom_point(meteorites2, mapping=aes(x=long, y=lat, size = mass, color=class), alpha = .6) +
  scale_color_manual(
    values = c("#f70000","#ff9900","#88726c","#006300","#001cf7","#487e90","#281713","#d900c6","#71007d","#ff85a3"),
    labels= c("H5","Iron, IIIAB","L6","H4","H6","Iron, IIAB","Iron, ungrouped","L4","L5","Iron")
    ) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(face="bold"),
    plot.background = element_rect(fill = "white"), 
    plot.caption = element_text(color = "black"),
    plot.title = element_text(size = 15, face = "bold")
  ) +
  labs(title="Top 10 Classes of Meteorites fallen in Mexico",x="Longitude", y="Latitude", caption= "Data: Meteoritical Society by way of NASA \n Made by @ShiXiong3") +
  labs(color = "Class", size = "Mass(kg)") 
