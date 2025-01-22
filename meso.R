rm(list=ls())
graphics.off()

#install.packages("geobr",dependencies = T)
library(geobr)
library(sf)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(reshape2)
library(scales)
library(viridis)

conj_dados = list_geobr()

brasil <- read_state(code_state = "all", year = 2018)
plot(brasil)
ggplot() + geom_sf(data = brasil, fill="#2D3E50", color="#FEBF57", size=0.15,
                   show.legend = F)

micro_reg <- read_micro_region(code_micro = "all", year = 2020)
ggplot() + geom_sf(data = micro_reg, fill="#2D3E50", color="#FEBF57", size=0.15,
                   show.legend = F)

meso_reg <- read_meso_region(code_meso = "all", year = 2020)
ggplot() + geom_sf(data = meso_reg, fill="#2D3E50", color="#FEBF57", size=0.15,
                   show.legend = F)


ce <- read_state(code_state = 23, year = 2020)
plot(ce)
ggplot() + geom_sf(data = ce, fill="#2D3E50", color="#FEBF57", size=0.15,
                   show.legend = F)

################################## ESSE AQUI ###################################
CE_muni <- read_municipality(code_muni = 23, year = 2020)
ggplot() + geom_sf(data = CE_muni, fill="#2D3E50", color="#FEBF57", size=0.15,
                   show.legend = F)


ggplot(data = brasil, color=NA) + 
  geom_sf() + 
  geom_sf(data = CE_muni, fill="#2D3E50", color=gray(.5), size=0.15, show.legend = F) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.15, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-42, -37), ylim = c(-8.5, -2), expand = FALSE) +
  theme(panel.grid.major = element_line(color = "light blue", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "light blue")) +
  xlab("Longitude") + ylab("Latitude")


CE_muni$name_muni

data1 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/wind.csv", dec = ".", header = TRUE)
windPotential <- c(data1$Wind.potential)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = windPotential), color = "#0D0D0D", size = .15) +
  #labs(title="Temperatura média da superfície", caption='Fonte: Elaboração própria', size=8) + 
  labs(size = 8) + 
  scale_fill_gradient(low = "white", high = "#00008B", name = "[W/m2]",
                      limits = c(0.0, 4.0)) + theme_minimal()

data2 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/Suitable_wind.csv", dec = ".", header = TRUE)
windSuitable <- c(data2$Suitable_wind)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = windSuitable), color = "#0D0D0D", size = .15) +
  #labs(title="Temperatura média da superfície", caption='Fonte: Elaboração própria', size=8) + 
  labs(size = 8) + 
  scale_fill_gradient(low = "white", high = "#8B6508", name = "[km2]",
                      limits = c(0.0, 1220.0)) + theme_minimal()

data3 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/solar.csv", dec = ".", header = TRUE)
solarPotential <- c(data3$solar)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = solarPotential), color = "#0D0D0D", size = .15) +
  labs(size = 8) + 
  scale_fill_gradientn(
    colors=c("white","#ffff00","#d94701"),
    values=rescale(c(0.0,10.0,170.0)), name = "[W/m2]",
    limits=c(0.0,170.0)) +
  theme_minimal()

data4 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/Suitable_solar.csv", dec = ".", header = TRUE)
solarSuitable <- c(data4$suitableSolar)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = solarSuitable), color = "#0D0D0D", size = .15) +
  #labs(title="Temperatura média da superfície", caption='Fonte: Elaboração própria', size=8) + 
  labs(size = 8) + 
  scale_fill_gradient(low = "white", high = "#8B4513", name = "[km2]",
                      limits = c(0.0, 1600.0)) + 
  theme_minimal()
solarSuitable[1]
#data5 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/Suitable_solar.csv", dec = ".", header = TRUE)
#water <- c(data4$suitableSolar)
#ggplot() + 
#  geom_sf(data = CE_muni, aes(fill = water), color = "#0D0D0D", size = .15) +
  #labs(title="Temperatura média da superfície", caption='Fonte: Elaboração própria', size=8) + 
#  labs(size = 8) + 
#  scale_fill_gradient(low = "white", high = "#8B4513", name = "[km2]",
#                      limits = c(0.0, 990.0)) + theme_minimal()

data6 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/population.csv", dec = ".", header = TRUE)
pop <- c(data6$Population)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = pop), color = "#0D0D0D", size = .15) +
  #labs(title="Temperatura média da superfície", caption='Fonte: Elaboração própria', size=8) + 
  labs(size = 8)  + 
  scale_fill_gradientn(
    colors=c("white","#6c8eb1","#465464"),
    values=rescale(c(4700.0,370000.0,2704000.0)), name = "[#]",
    limits=c(4700.0,2704000.0)) +
  theme_minimal()

#+  scale_fill_gradient(low = "white", high = "#20B2AA", name = "[#]",
#                      limits = c(4700.0, 2704000.0)) + theme_minimal()

data7 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/gdp.csv", dec = ".", header = TRUE)
gdp <- c(data7$gdp)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = gdp), color = "#0D0D0D", size = .15) +
  #labs(title="Temperatura média da superfície", caption='Fonte: Elaboração própria', size=8) + 
  labs(size = 8) + 
  scale_fill_gradientn(
    colors=c("white","#fad25a","#ae8d0b"),
    values=rescale(c(40000.0,10000000.0,66000000.0)), name = "[R$]",
    limits=c(40000.0,66000000.0)) +
  theme_minimal()

#+scale_fill_gradient(low = "white", high = "#DAA520", name = "[R$]",
#                      limits = c(46000.0, 66000000.0)) + theme_minimal()

data8 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/temp.csv", dec = ".", header = TRUE)
temp <- c(data8$Temperature)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = temp), color = "#0D0D0D", size = .15) +
  #labs(title="Temperatura média da superfície", caption='Fonte: Elaboração própria', size=8) + labs(size = 8) + 
  scale_fill_gradient(low = "white", high = "#FF0000", name = "[Celsius]",
                      limits = c(20.8, 27.7)) + theme_minimal()

data9 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/distancia.csv", dec = ".", header = TRUE)
distancia <- c(data9$Distancia.subestacao)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = distancia), color = NA, size = .15) +
  #labs(title="Distância para subestação mais próxima", caption='Fonte: Elaboração própria', size=8) + 
  labs(size = 8) + 
  scale_fill_gradient(low = "white", high = "#8B1A1A", name = "[km]",
                      limits = c(0.0, 50.0)) + theme_minimal()

data10 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/HydroProd.csv", dec = ".", header = TRUE)
HydroProd <- c(data10$HydroProd)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = HydroProd), color = NA, size = .15) +
  #labs(title="Distância para subestação mais próxima", caption='Fonte: Elaboração própria', size=8) + 
  labs(size = 8) + 
  scale_fill_gradient(low = "white", high = "black", name = "[ton/year]",
                      limits = c(0.0, 666237.0)) + theme_minimal()

data11 <- read.csv("C:/Users/ji_le/OneDrive/Área de Trabalho/Coisas_do_Mestrado/Eólica/CE_Mesorregioes_2022/ranking.csv", dec = ".", header = TRUE)
Ranking <- c(data11$Ranking)
ggplot() + 
  geom_sf(data = CE_muni, aes(fill = Ranking), color = "black", size = .15) +
  #labs(title="Distância para subestação mais próxima", caption='Fonte: Elaboração própria', size=8) + 
  labs(size = 8) + 
  scale_fill_gradientn(
    #colours=topo.colors(7, rev=TRUE),
    colors=c("white","#a8ddb5","#3182bd"),
    values=rescale(c(184,1)), name = "[#]",
    limits=c(1,184),
    breaks = c(1,50,100,150,184),labels=c("1º","50º","100º","150º","184º")) + 
  theme_minimal()
  #scale_fill_gradient(low = "white", high = "black", name = "[#]",
   #                   limits = c(1.0, 184.0))
################################## ESSE AQUI ###################################

meso_reg <- read_meso_region(code_meso = 23, year = 2020)
ggplot() + geom_sf(data = meso_reg, fill="#2D3E50", color="#FEBF57", size=0.15,
                   show.legend = F)

MesoCE <- c(meso_reg$name_meso)
eolica <- c(1.0752,0.8768,0.4601,0.2500,0.5576,0.0340,0.9909)
solar <- c(0.6338,0.6274,0.2444,0.8678,0.9781,0.5559,0.8740)
populacao <- c(1441.656,1107.100,3866.237,920.008,564.641,393.731,947.207)

dat <- data.frame(MesoCE,meso_reg$geom,eolica,solar,populacao)
dat

ggplot() + 
  geom_sf(data = meso_reg, aes(fill = eolica), color = NA, size = .15) +
  labs(title="Capacidade Instalável de energia eólica",
       caption='Fonte: Elaboração própria', size=8)+ 
  labs(size = 8) + 
  scale_fill_gradient(low = "white", high = "#5D478B", name = "Densidade energética/n[W/m2]",
                      limits = c(0.0, 1.1)) + theme_minimal()

ggplot() + 
  geom_sf(data = meso_reg, aes(fill = solar), color = NA, size = .15) +
  labs(title="Capacidade Instalável de energia solar",
       caption='Fonte: Elaboração própria', size=8)+ 
  labs(size = 8) + 
  scale_fill_gradient(low = "white", high = "#FF8C00", name = "Densidade energética/n[W/m2]",
                      limits = c(0.2, 1.0)) + theme_minimal()
muniCE <- CE_muni$name_muni
ggplot() + 
  geom_sf(data = meso_reg, aes(fill = populacao), color = NA, size = .15) + 
  geom_sf(data = CE_muni, color= 'black', size=.15, alpha = 0.0) +
  #geom_sf_label(aes(label=muniCE),label.padding = unit(2, "mm"), size = 2)+
  scale_fill_gradient(low = "white", high = "#8B0A50", name = "Mesoregion",
                      limits = c(390.000, 4000.000)) + theme_minimal()

ggplot() +
  geom_sf(data = meso_reg, aes(fill = code_meso), 
          color = 'black', size = .15)  + 
  geom_sf(data = CE_muni, color= 'black', size=.15, alpha = 0.0) +
  scale_fill_gradientn(name = "Mesoregion",colours = terrain.colors(7, alpha=0.5, rev = FALSE)) +
  theme_minimal()

ggplot(data = brasil, color=NA) + 
  geom_sf(data = meso_reg) + 
  geom_sf(data = CE_muni) +
  coord_sf(xlim = c(-42, -37), ylim = c(-8.5, -2), expand = FALSE) +
  theme(panel.grid.major = element_line(color = "light blue", linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "light blue")) +
  xlab("Longitude") + ylab("Latitude")

CE_muni <- read_municipality(code_muni = 23, year = 2020)
ggplot() + geom_sf(data = CE_muni, size=0.15, show.legend = F) 
#+ geom_sf(data = meso_reg, aes(fill = pop), color = NA, size = .15)

MesoCE <- read_meso_region(code_meso = 23, year = 2020)
ggplot() + geom_sf(data = CE_muni, size=5, show.legend = F) + 
  geom_sf(data = MesoCE, fill=NA, color="#FEBF57", size=5,
                   show.legend = F)
sedeMuni <- subset(read_municipal_seat(), code_state==23)
ggplot() +
  geom_sf(data = sedeMuni, fill="#2D3E50", color="#FF0000", size=.15, show.legend = FALSE)
