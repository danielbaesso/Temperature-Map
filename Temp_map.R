install.packages('ggspatial')

library(ggplot2)
library(geobr)
library(raster)
library(fields)
library(ggspatial)

# Loading a directory
setwd('C:/DanielBaesso/map_temp')

# Reading csv database
dados.temp = read.csv('dados/dados_temperatura.csv')

# Reading a background image (.tif)
relevo = raster('dados/relevo_minas_gerais.tif')
plot(relevo)

# Viewing descriptive statistics of variables
modelo = lm(formula = temp~longitude+latitude+altitude, data = dados.temp)

names(dados.temp)
summary(modelo)

# Transforming data frame for manipulation
top.df = as.data.frame(relevo, xy=TRUE)

# Eliminating null information
top.df= na.omit(top.df)

# Changing the name of the variables
names(top.df) = c("long", "lat", "alt")
temp.mg = top.df

# Temperature calculation model used
temp.mg$temp = 23.49 - 0.25*top.df$long + 0.48*top.df$lat - 0.0053*top.df$alt

# Calculating
temp.mg$temp.cat = cut(temp.mg$temp, breaks=c(8,10, 12,14,16,18,20,22,24,26),
    labels=c('8-10', '10-12', '12-14', '14-16','16-18','18-20','20-22',
             '22-24','24-26'))

mg = read_state(code_state = "MG")

plot(mg)
class(mg)

# Plotting the map
ggplot(data=temp.mg)+
  geom_raster(aes(x=long,y=lat, fill=temp.cat))+
  geom_sf (data=mg,fill="transparent")+
  scale_fill_manual(values= tim.colors(9))+
  annotation_scale()+
  annotation_north_arrow(location="tl",
                         style = north_arrow_nautical())+
  labs(x=NULL,y=NULL,
       title = "Temperature Map of Minas Gerais", fill="ºC")+
  theme_light()

# Saving the final work
ggsave(filename = 'img/temp_map_mg.png')
