
library(ggplot2)
library(RColorBrewer)

#View(scenario.costs)

p1 <- ggplot(data = scenario.costs, aes(total))
p1 + geom_histogram(binwidth = 10000000, fill = 'blue')+
  xlim(7500000000, 10000000000)

#View(scenario.costs[which(scenario.costs$total > 5000000000),])
## cost efficiency plot
## 
cost.efficiency <- data.frame(tm$num,scenario.costs[,1]/tm$mydif)

plot(tm$mydif,cost.efficiency$Cost.Stem)
winners <- tm$num[which(tm$mydif > 1.3*10^9)] #which scenarios meet the 1.3 bn goal?
#198 201 202 205 206 209 210 213 214 217 218

#### Below is pie chart of any chosen scnario
#### 
#sc <- 216 #input scenario here
for(i in 1:length(winners)){
  sc <- winners[i]
  source("monarch.pie.R")
  pie(as.numeric(k2[2:7]), labels = plabs, col=plotcols,
      main = paste("Pie Chart of Scenario: ",sc ," (", 
                   tm$Scenario[which(tm$num == sc)],")", sep = ""),
      cex = 0.75)
  s.t <- round(scenario.costs$total[which(tm$num == sc)]/1000000000,3)
  mtext(paste("Total Cost: ","$",s.t," billion.  ","Total New Stems: ", 
              prettyNum(round(tm$mydif[which(tm$num == sc)], 0),big.mark=","), 
              ' Cost/Stem: $', round(cost.efficiency$Cost.Stem[which(cost.efficiency$Scenario == sc)],2),  sep = ""))
}


#Pie charts of practice costs by scenario
for(i in 1:length(winners)){
  sc <- winners[i]
  source("code/practice.costs.pie.R")
  pie(as.numeric(k2[1:11]), labels = plabs, col=plotcols,
      main = paste("Pie Chart of Scenario: ",sc ," (", 
                   tm$Scenario[which(tm$num == sc)],")", sep = ""),
      cex = 0.75)
  s.t <- round(prac.cost.sum$total[which(tm$num == sc)]/1000000000,3)
  mtext(paste("Total Cost: ","$",s.t," billion.  ","Total New Stems: ", prettyNum(round(tm$mydif[which(tm$num == sc)], 0),big.mark=","), ' Cost/Stem: $', round(cost.efficiency$Cost.Stem[which(cost.efficiency$Scenario == sc)],2),  sep = ""))
}

k2 <- scenario.costs[which(scenario.costs$num==sc),]
scenarios <- tm$Scenario[which(tm$num==sc)]

cost.efficiency <- data.frame(tm$num,scenario.costs[,1]/tm$mydif)
colnames(cost.efficiency) <- c('Scenario','Cost.Stem')

plotcols <- brewer.pal(6, "Paired")

plabs <- round(as.numeric(k2[2:7])/1000000,0)
plabs <- paste("$",plabs,sep = "")
plabs <- paste(names(k2[2:7]),plabs, sep = " ")
pie(as.numeric(k2[2:7]), labels = plabs, col=plotcols,
    main = paste("Pie Chart of Scenario: ",sc ," (", 
                 tm$Scenario[which(tm$num == sc)],")", sep = ""),
    cex = 0.75)
s.t <- round(rowSums(k2[,2:7])/1000000000,3)
mtext(paste("Total Cost: ","$",s.t," billion.  ","Total New Stems: ", prettyNum(round(tm$mydif[which(tm$num == sc)], 0),big.mark=","), ' Cost/Stem: $', round(cost.efficiency$Cost.Stem[which(cost.efficiency$Scenario == sc)],2),  sep = ""))

