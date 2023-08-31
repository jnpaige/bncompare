setwd("C:/Users/walkerro/Desktop/R scripts/bncompare")
df <- read.csv('ethnoatlas.csv')

library(dplyr)
usedf <- df %>% select(popd, EA028, EA034, EA033, EA066) #, npp)
names(usedf) <- c('populationDensity', 'agriculture', 'highGods', 'hierarchy', 'class') #, 'npp')

#recode class
usedf$class[usedf$class == 3] <- 4
table(usedf$class)

#recode ag
hist(usedf$agriculture)
usedf$agriculture <- ifelse(usedf$agriculture < 5 & usedf$agriculture > 1, 3, usedf$agriculture)
usedf$agriculture[usedf$agriculture == 5] <- 6

library(bnlearn);library(ggplot2)
usedf %>% tidyr::gather(variable, value) %>% 
  ggplot(aes(value)) + geom_histogram(bins = 15) + 
  facet_wrap(~variable, scales = 'free_x')

#discretize population density
disc <- function(x, k) {
  cut(x, breaks=c(quantile(x, na.rm = T, probs = seq(0, 1, by = 1/k))), #labels=c("0-20","20-40","40-60","60-80","80-100"), 
      include.lowest=TRUE)}
usedf$populationDensity <- disc(usedf$populationDensity, 3)
table(usedf$populationDensity)

usedf <- data.frame(lapply(usedf , as.factor))
colSums(is.na(usedf))
sum(colSums(is.na(usedf)))/(5*1533)

names(usedf)
usedf <- usedf[rowSums(is.na(usedf)) != ncol(usedf), ]
dag <- structural.em(usedf, return.all = TRUE,
                     maximize = "tabu", #maximize.args = list(tabu = 50, max.tabu = 50),
                     #fit = "bayes", fit.args = list(iss = 1),
                     impute = "parents", max.iter = 3) #, blacklist = blacklist) #si.hiton.pc, gs, hc, fast.iamb, iamb, mmhc, rsmax2 etc
plot(dag$dag)#dag$imputed #undirected.arcs(dag)

score(dag$dag, na.omit(usedf)) #, type ='bic')

(fit = bn.fit(dag$dag, usedf)) #library(parallel);cl = makeCluster(3)

# bootstrap multiple network structures https://www.bnlearn.com/examples/missing-score/
start = random.graph(nodes = names(usedf), method = 'ic-dag', num = 10001)
system.time({
  netlist = lapply(start, function(net) {
  newdf <- usedf[sample(nrow(usedf),replace=TRUE),]
  structural.em(newdf, maximize = "tabu"
                #maximize.args=list(blacklist=bl)
  )}) #, start = net, score = "bde", iss = 10, start = net) })
})
arcs = custom.strength(netlist, nodes = names(usedf), cpdag = FALSE)
arcs[arcs$strength > 0.5 & arcs$direction >= 0.5, ]
plot(arcs)
avg.bayesianNetwork = averaged.network(arcs, threshold = 0.5) #plot(avg.bayesianNetwork)
(fit = bn.fit(avg.bayesianNetwork, usedf))

pdf("ea.pdf") #, units="in", width=5, height=5, res=300)
strength.plot(avg.bayesianNetwork, arcs, threshold=.5, 
              layout = "dot", #dot, neato, twopi, circo and fdp
              #groups = list(c("Subsistence","Technology","Agricultural intensity")),
              highlight = list(nodes = c("populationDensity"), col = "tomato", fill = "orange"),
              shape = "rectangle") #circle ellipse rectangle
dev.off()
saveRDS(arcs, file='ea1') #
arcs = readRDS(file='ea1') 

library(flextable)
round_and_format <- function(number) {rr <- round(number,3)
as.character(rr)}
myft <- flextable(arcs[arcs$strength > 0.5 & arcs$direction >= 0.5, ]);myft <- set_formatter(myft,
                                                                                             strength = round_and_format,direction = round_and_format)
myft <- autofit(myft);print(myft, preview = "docx")



#prediction
predict(fit, node = "fixity", data=na.omit(usedf), prob = TRUE)

#cpdist(fit, #event = ('fixity' == 6), nodes = c('fixity'), evidence = ('popd' == c("(5,7]")))

