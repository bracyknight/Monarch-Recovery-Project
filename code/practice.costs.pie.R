#monarch cost pie chart script
#
k2 <- prac.cost.sum[which(prac.cost.sum$num==sc),]

scenarios <- tm$Scenario[which(tm$num==sc)]

cost.efficiency <- data.frame(tm$num,scenario.costs[,1]/tm$mydif)
colnames(cost.efficiency) <- c('Scenario','Cost.Stem')

plotcols <- brewer.pal(6, "Paired")

plabs <- round(as.numeric(k2[1:11])/as.numeric(k2[12]),3)*100
plabs <- paste(plabs,"%",sep = "")
plabs <- paste(names(k2[1:11]),plabs, sep = " ")


