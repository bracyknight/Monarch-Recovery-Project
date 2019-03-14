library(ggplot2)
library(tidyr)
require(scales)

#winners <- (126:138)

pc <- prac.cost.sum[winners+1,c(13,1:11)] %>%
  gather(key = 'num')
pc$scenario <- rep(winners+1)

colnames(pc) <- c('practice','cost','scenario')
pc$scenario <- as.factor(pc$scenario)
pc$practice <- as.factor(pc$practice)
levels(pc$practice)
pc$practice <- factor(pc$practice, levels = c("Seed","Hydro-seeding","Drill.Seeding","Broadcast.Herbicide" ,"Broadcast.Seeding","Brush.Management","Fence", "Mowing","Prescribed.Burning","Spot Herbicide", "Tilling " ))
pc$practice <- factor(pc$practice, levels = rev(levels(pc$practice)))

pc2 <-pc[order(pc$practice), ]

g <- ggplot(data=pc, aes(x=scenario, y = cost/1000000000, fill = practice))+
  geom_bar(stat="identity")
  #theme(axis.title.x=element_blank(),
   #     axis.text.x=element_blank()) 
g+ scale_fill_manual(values =myp)

g<- g +scale_fill_brewer(palette='Set3')
g + theme_classic() + 
  ylab('Cost (in billions)')

sc <- scenario.costs[winners+1,c(39,2:8)] %>%
  gather(key = num)
sc$scenario <- rep(winners+1)

colnames(sc) <- c('sector','cost','scenario')
sc$scenario <- as.factor(sc$scenario)

g <- ggplot(data=sc, aes(x=scenario, y = cost/1000000000, fill = sector))+
  geom_bar(stat="identity")
  #theme(axis.title.x=element_blank()) 
g<- g+scale_fill_brewer(palette='Set3')
g + theme_classic() + 
  ylab('Cost (in billions)')+
 theme(axis.text=element_text(size=14),axis.title.y = element_text(size =16),
       axis.title.x = element_text(size =16),legend.text=element_text(size=12))+
  xlab('Thogmartin Scenario')

