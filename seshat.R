library(bnlearn);library(readxl);library(dplyr);library(ggplot2);library(tidyr);library(lme4)#install.packages("BiocManager") BiocManager::install("Rgraphviz")
setwd("C:/Users/walkerro/Desktop/R scripts/bncompare")
df <- read.csv("seshat.csv") #362 x 24
df$X <- NULL
library(ggplot2); library(cowplot)

df$popd <- df$pop/df$terr
df$logpopd <- log(df$popd)
df$militaryLevels[df$militaryLevels < 1] <- 1
df$adminLevels[df$adminLevels <1] <- 1
df$settHier[df$settHier < 1] <- 1
df$religiousLevels[df$religiousLevels < 1] <- 1

# static bayes net
names(df)
usedf <- df[,c(24, 15:18)] #20, 10 are logpop and terr
colSums(is.na(usedf))
sum(colSums(is.na(usedf)))/(5*357)

names(usedf)
names(usedf) <- c('populationDensity',"settlementHierarchy","adminLevels"   ,
                  "militaryLevels" ,"religiousLevels" )
usedf <- usedf[rowSums(is.na(usedf)) != ncol(usedf), ]
dag <- structural.em(usedf, return.all = TRUE,
                     maximize = "tabu", #maximize.args = list(tabu = 50, max.tabu = 50),
                     #fit = "bayes", fit.args = list(iss = 1),
                     impute = "parents", max.iter = 3) #, blacklist = blacklist) #si.hiton.pc, gs, hc, fast.iamb, iamb, mmhc, rsmax2 etc
plot(dag$dag)#dag$imputed #undirected.arcs(dag)

score(dag$dag, na.omit(usedf)) #, type ='bic')

fit = bn.fit(dag$dag, usedf) #library(parallel);cl = makeCluster(3)

# bootstrap multiple network structures https://www.bnlearn.com/examples/missing-score/
start = random.graph(nodes = names(usedf), method = 'ic-dag', num = 10001)
netlist = lapply(start, function(net) {
  newdf <- usedf[sample(nrow(usedf),replace=TRUE),]
  structural.em(newdf, maximize = "tabu", start = net)}) #score = "bde", iss = 10, start = net) })
arcs = custom.strength(netlist, nodes = names(usedf), cpdag = FALSE)
arcs[arcs$strength > 0.5 & arcs$direction >= 0.5, ]
plot(arcs)
avg.bayesianNetwork = averaged.network(arcs, threshold = 0.5) #plot(avg.bayesianNetwork)
(fit = bn.fit(avg.bayesianNetwork, usedf))

pdf("seshat.pdf") #, units="in", width=5, height=5, res=300)
strength.plot(avg.bayesianNetwork, arcs, threshold=.5,
              layout = "dot", #dots, neato, twopi, circo and fdp
              groups = list(c("militaryLevels", 'populationDensity')),
              highlight = list(nodes = c("populationDensity"), col = "tomato", fill = "orange"),
              shape = "rectangle") #circle ellipse rectangle
dev.off()

saveRDS(arcs, file='seshat')
arcs = readRDS(file='seshat')

library(flextable)
round_and_format <- function(number) {rr <- round(number,3)
as.character(rr)}
myft <- flextable(arcs[arcs$strength > 0.5 & arcs$direction >= 0.5, ]);myft <- set_formatter(myft,
                                                                                             strength = round_and_format,direction = round_and_format)
myft <- autofit(myft);print(myft, preview = "docx")

