library(bnlearn);library(readxl);library(dplyr);library(ggplot2);library(tidyr)#install.packages("BiocManager") BiocManager::install("Rgraphviz")
setwd("c:/users/walkerro/desktop/r scripts/bncompare")

join_df <- read.csv("wnai.csv") #from first wnai bayes net paper

#ritual
join_df$ritual <-   join_df$ceremony + join_df$lifecycle

join_df$Agriculture <- join_df$Agricultrual.production # factor(ifelse(join_df$Agricultrual.production == 1 | join_df$Agricultrual.production == 5, 0, 1))
join_df$ag <- factor(ifelse(join_df$Agriculture == 1, "Hunter-gatherer", 
                            ifelse(join_df$Agriculture < 5, "Incipient horticulture",
                                   ifelse(join_df$Agriculture < 6, "Non-food agriculture",
                                          ifelse(join_df$Agriculture < 7, "Agriculture <50%", "Agriculture >50%")))),
                     levels = c("Hunter-gatherer", "Incipient horticulture", "Non-food agriculture",'Agriculture <50%', 'Agriculture >50%' ))
levels(join_df$ag)
table(join_df$ag)
join_df$"Population_density" <- join_df$pop.density

library(ggplot2);library(sf);library(rnaturalearth);library(dplyr)
library(rnaturalearthdata);library(ggspatial);library(cowplot);theme_set(theme_cowplot())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world);library(maps)
ggplot(data = world) +
   geom_sf(fill="antiquewhite") + #[!is.na(df$rivtype),] 
   #geom_sf(data = white, colour = "red") +
   #geom_sf(data = black, colour = "black") +
   #geom_sf(data = merged[merged$River_Order > 7,], colour = "lightblue") +
   geom_point(data = join_df, aes(x = -Long, y = Lat, fill=ag, size = Population_density) , #material_culture) , #div_of_labor) , #Population_density),
              colour="black",pch=21, alpha = .8, alpha=.7) +
   scale_fill_manual(values=c('green','blue', "gray50", "yellow", 'red'), name="Agricultural intensity") + 
   #scale_color_continuous( name="Population density bins") + 
   # geom_sf(data = st_as_sf(river3), colour = "blue") +
   #geom_tile(data = dfpred, mapping = aes(x = x, y = y, fill=Prediction), alpha = .7, size = 1) +
   coord_sf(xlim = c(-140, -103), ylim = c(29.2, 60), expand = FALSE) +
   annotation_scale(location = "bl", width_hint = 0.4) +
   #annotation_north_arrow(location = "bl", which_north = "true", 
   #                       pad_x = unit(.3, "in"), pad_y = unit(.3, "in"),
   #                       style = north_arrow_fancy_orienteering) +
   xlab("") + ylab("") +
   #ggtitle("Manioc map", subtitle = "(with phosphorus)") +
   theme(legend.position = c(0.01, 0.3),
         axis.title=element_blank(),
         axis.text=element_blank(),
         axis.ticks=element_blank(),
         #panel.background = element_blank(),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.line = element_line(colour = "black"),
         panel.border = element_rect(colour = "black", fill=NA, size=1),
         #panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
         panel.background = element_rect(fill = "aliceblue"))
#ggsave("map.pdf") #, units = "in", width = 7, height=5)

#select certain nodes
culture.nodes <- c('Agriculture', #'technology',
                   'material_culture', #'Subsistence', 
                   'ritual', #'supernatural', 
                   'pop.density' ,
                  # 'div_of_labor', 'econ_dist', 'marriage',
                   'Politics', 'war' #'property'
                   )
all <- join_df[, culture.nodes  ]
all <- data.frame(lapply(all, as.numeric))

library(corrplot)
res <- cor(all)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#bootstrap multiple network structures
set.seed(1)
boot <- boot.strength(all, R = 10001, cpdag = FALSE,
                      algorithm = "tabu")
boot[boot$strength > 0.8 & boot$direction >= 0.5, ]
avg.boot <- averaged.network(boot, threshold = .8) #was .8
plot(boot)
avg.boot
strength.plot(avg.boot, boot, threshold=.8, #highlight = c('Politics'),
              layout = "dot", #dot, neato, twopi, circo and fdp
              #      groups = list(c('Technology', "Material culture",'Population density', 'Agricultural intensity', 'Subsistence')),
              #groups = list(c("Subsistence","Technology","Agricultural intensity")),
              #highlight = list(nodes = c("Supernatural",'Ritual','Division of labor', "Politics", "War","Marriage","Property","Economic distribution"), col = "tomato", fill = "orange"),
              shape = "rectangle") #circle ellipse rectangle
(fit = bn.fit(avg.boot, all))

print(bn.cv(all, bn = avg.boot, k = nrow(all)) )
saveRDS(boot, file='all_boot1')
boot = readRDS(file='all_boot1')

library(flextable)
round_and_format <- function(number) {rr <- round(number,3)
as.character(rr)}
myft <- flextable(boot[boot$strength > 0.5 & boot$direction >= 0.5, ]);myft <- set_formatter(myft,
                                                                                             strength = round_and_format,direction = round_and_format)
myft <- autofit(myft);print(myft, preview = "docx")

