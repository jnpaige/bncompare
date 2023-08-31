setwd("C:/users/walkerro/desktop/r scripts/bncompare")

# bring in data
df <- read.csv("binford.csv")
names(df)
str(df)

table(df$systate3recod, df$systate3)

hist(df$war1)
hist(df$shaman)
hist(df$density)
hist(df$qtstor)
hist(df$polygrecod)
table(df$systate3recod)

#include missing
df2 <- df[,c('war1', 'shaman', 'density', 'qtstor', 'systate3recod'
             #'fishing', 'sed', 'leader', 'class', 
             #'headm', 'slave', 'agedif', 'divorce'
             )]
colSums(is.na(df2))
df2$density <- log(as.numeric(df2$density))
hist(df2$density)

#recoding
df2$systate3recod <- ifelse(df$systate3recod == 6 | df$systate3recod == 8, 1, 0)
df2$war1[df2$war1 == 5] <- 4

library(bnlearn);library(ggplot2);library(dplyr)
df2 %>% tidyr::gather(variable, value) %>% 
  ggplot(aes(value)) + geom_histogram(bins = 15) + 
  facet_wrap(~variable, scales = 'free_x')

#discretize density
disc <- function(x, k) {
  cut(x, breaks=c(quantile(x, na.rm = T, probs = seq(0, 1, by = 1/k))), #labels=c("0-20","20-40","40-60","60-80","80-100"), 
      include.lowest=TRUE)}
df2$density <- disc(df2$density, 4)
table(df2$density)

df2 <- data.frame(lapply(df2 , as.factor))
colSums(is.na(df2))
sum(colSums(is.na(df2)))/(6*339)

names(df2)
names(df2) <- c('war', 'shaman', 'populationDensity', 'storage', 'ranked') #, 'horticulture') #'class', 'headman', 'slave', 'agedif', 'divorce')

start = random.graph(nodes = names(df2), method = 'ic-dag', num = 10001)
outlist = lapply(start, function(net) {
  newdf <- df2[sample(nrow(df2),replace=TRUE),]
  structural.em(newdf, maximize = "tabu")}) #score = "bde", iss = 10, start = net) })
arcs = custom.strength(outlist, nodes = names(df2), cpdag = FALSE)
arcs[arcs$strength > 0.5 & arcs$direction >= 0.5, ]
plot(arcs)
avg.bayesianNetwork = averaged.network(arcs, threshold = 0.5) #plot(avg.bayesianNetwork)
(fit = bn.fit(avg.bayesianNetwork, df2))

pdf("binford.pdf") #, units="in", width=5, height=5, res=300)
strength.plot(avg.bayesianNetwork, arcs, threshold=.5, 
              layout = "dot", #dots, neato, twopi, circo and fdp
              #groups = list(c("Subsistence","Technology","Agricultural intensity")),
              highlight = list(nodes = c("populationDensity"), col = "tomato", fill = "orange"),
              shape = "rectangle") #circle ellipse rectangle
dev.off()
saveRDS(arcs, file='binford') 
arcs = readRDS(file='binford') 

library(flextable)
round_and_format <- function(number) {rr <- round(number,3)
as.character(rr)}
myft <- flextable(arcs[arcs$strength > 0.5 & arcs$direction >= 0.5, ]);myft <- set_formatter(myft,
                                                                                             strength = round_and_format,direction = round_and_format)
myft <- autofit(myft);print(myft, preview = "docx")

