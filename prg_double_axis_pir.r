##############################################################################################################################
##POPULATION PYRAMID WITH A DECOMPOSITION FOR THREE VARIABLES
##
##CÉSAR CRISTANCHO. MARCH, 2020 (LAST UPDATED: MARCH, 2020)
##
##cacristanchof@gmail.com
##
##
##
##THERE IS NO WARRANTY FOR THIS CODE
##############################################################################################################################

library(tidyverse)
library(data.table)
library(ggthemes)
library(forcats)
library(rje)
library(gtable)


bd <- fread('poblacion.edades.simples.censos.csv')

bd$Hombres <- as.numeric(as.character(bd$Hombres ))
bd$Mujeres <- as.numeric(as.character(bd$Mujeres))
bd$edad <- as.numeric(as.character(bd$edad))

melt.pop <- melt(bd, id= c("Cod.Dep","Dep",  "edad", "anio" ) )

bds <- data.table(melt.pop)

bds <- bds[, den := sum(value) , keyby = list(anio, Cod.Dep, Dep)]
bds$Porcentaje <- bds$value / bds$den
bds <- subset(bds, Dep == 'Colombia')
bds$gen <- as.numeric(as.character(bds$anio)) - bds$edad


sizet <- 18; grillap <- '#E4E4E4'; grillas <- '#EAEAEA'; grillat <- '#f0f0f0'

p2 <- 
  ggplot( filter(bds, anio == '2018'), aes(edad -.2 , fill = variable, group = 1)) +  
  geom_vline( xintercept = seq(0, 100, 5), colour = grillat, linetype = 'solid') +  
  geom_hline(yintercept = seq(-.02,.02,.00125),colour = grillat, linetype = 'solid') +
  geom_vline( xintercept = seq(0, 100, 10), colour = grillas, linetype = 'solid') +  
  geom_hline(yintercept = seq(-.02,.02,.0025),colour = grillas, linetype = 'solid') +
  geom_vline( xintercept = seq(0, 100, 20), colour = grillap, linetype = 'solid') +  
  geom_hline(yintercept = seq(-.02,.02,.005),colour = grillap, linetype = 'solid') +
  geom_histogram(data = filter(bds, anio == '2018' & variable == "Mujeres"), alpha = .4,
                 aes(y = Porcentaje ,  fill = "M"), stat="identity", width = 1 ) +
  geom_histogram(data = filter(bds, anio == '2018' & variable == "Hombres"), alpha = .4,
                 aes(y = Porcentaje* -1, fill = "H" ), stat="identity", width = 1)  +  
  coord_flip() + xlab("") + ylab("") + theme_bw() + 
  scale_x_continuous(limits = c(-1, 101), breaks = seq(0,100, 10), 
                     labels = seq(2018, 1918, -10) ) +
  scale_y_continuous(limits = c(-.01, .01), breaks = seq(-.01,.01,.0025), 
                     labels = paste0(abs(seq(-.01,.01,.0025)) *  100,'%')   ) +
  scale_fill_manual(name= 'Sexo',   
                    values = c("H" = cubeHelix(16, start =0.9 , hue = 3)[6], 
                               "M" = cubeHelix(16, start =1.6  , hue = 3)[4], 
                               "white", "white") ) + 
  theme(axis.text.x = element_text(size = sizet , family="Helvetica", colour = 'black'),
        axis.text.y = element_text(size = sizet + .5, family="Helvetica", colour = 'black'),
        panel.grid = element_blank(), 
        plot.title = element_text(size = sizet + 4, family="Helvetica"),
        panel.border = element_rect(colour =  grillap, fill=NA, size = .5),
        axis.ticks = element_line(colour = 'black')
  )  + guides(fill = FALSE) +   theme_gray(base_size = 18  ) +
  labs(title = 
         'Colombia: pirámide de población según edades simples y generación. 1985', 
       subtitle = 'Fuente: DANE. (https://www.dane.gov.co/). Actualizado: 22 de Marzo de 2020', 
       caption = 'https://github.com/CescRf/population_pyramid_dual_axis.git
         cacristanchof@gmail.com')  

p1 <- ggplot(
  # subset(bds, anio == '1985'), 
  #            aes( x = gen +.2, y = Porcentaje, colour = variable, group = 1)
  ) +
  geom_blank(data = filter(bds, anio == '2018' & variable == "Mujeres"), 
             aes(y = Porcentaje ,  colour = "M"), stat="identity" ) +
  geom_blank(data = filter(bds, anio == '2018' & variable == "Hombres"), 
             aes(y = Porcentaje* -1, colour = "H" ), stat="identity")  +
  coord_flip() + xlab("") + ylab("") + 
  scale_x_reverse(limits = c(2019, 1917), breaks = seq(2018, 1918, -10), 
                  labels = seq(0, 100, 10) )+
  scale_y_continuous(limits = c(-.01, .01), breaks = seq(-.01,.01,.0025), 
                     labels = paste0(abs(seq(-.01,.01,.0025)) *  100,'%')   ) +
  theme(axis.text.x = element_text(size = sizet, family="Helvetica", colour = 'black'),
        axis.text.y = element_text(size = sizet + .5, family="Helvetica", colour = 'black'),
        panel.grid = element_blank(),
        panel.border = element_rect(colour =  grillap, fill=NA, size = .5),
        axis.ticks = element_line(colour = 'black') )+
  guides(fill = FALSE) +   theme_gray(base_size = 18  ) +
  labs(title = 
         'Colombia: pirámide de población según edades simples y generación. 2018', 
       subtitle = 'Fuente: DANE. (https://www.dane.gov.co/). Actualizado: 22 de Marzo de 2020', 
       caption = 'https://github.com/CescRf/population_pyramid_dual_axis.git
         cacristanchof@gmail.com')  


twet <- function (p1, p2) {
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
}
g2018 <- twet(p1            , p2) 

print(grid.arrange(g2018))
