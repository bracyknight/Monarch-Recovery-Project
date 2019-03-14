#Monarch Summary Data



r1<-with(pc, tapply(cost, practice, mean))
r1
View(r1)

r2 <- round((r1/(sum(r1))*100),2)
r2
       
  sum(r2[c(1:9)]) #total percent all non-hyro or seed costs
names(r2)

winners <- 127:138
#Average cost of scenarios that fit the bill
ave.cost <- round((mean(scenario.costs[winners+1,1])/1000000000),2);ave.cost
#10.95 billion using thogmartin's max reasonable
#6.28 billion if 200 stems/acre for all restoration is assumed
#0.63 billion of 2,000 stems/acre


#Summary Barplot

#my pallete
myp <- c('#ff6347','#1B685B','#d66f19','#d66f19','#f4f3ea','#6dc9dd','orange','#df1313','#c2df55','#8e9b80','#dbcab0')

pc2 <- prac.cost.sum[winners+1,c(13,1:11)] %>% gather(key = num)
colnames(pc2) <- c('scenario','practice','cost')

pc3 <- aggregate(cost~practice, FUN = median, data = pc2)
#pc3 <- with(pc3,  pc3[order(practice) , ])
pc3$practice <- factor(pc3$practice, levels = c("Seed","Hydro-seeding","Drill.Seeding","Broadcast.Herbicide" ,"Broadcast.Seeding","Brush.Management","Fence", "Mowing","Prescribed.Burning","Spot Herbicide", "Tilling " ))

pc3$practice <- factor(pc3$practice, levels = rev(levels(pc3$practice)))

g <- ggplot(data=pc2, aes( y = cost/1000000000, fill = practice))+
  geom_bar(stat="identity")
#theme(axis.title.x=element_blank(),
#     axis.text.x=element_blank()) 
g <- ggplot(pc3, aes(1, cost/sum(pc3$cost), fill=practice)) + geom_bar(stat="identity")
g<- g +scale_fill_brewer(palette='Set3')
g + theme_classic() + 
  ylab('Percent Total Cost')

g+ scale_fill_manual(values =myp)

g<- ggplot(pc3, aes(x = practice, y=100*cost/sum(pc3$cost), fill= practice)) + geom_bar(stat = 'identity')
g<- g +scale_fill_brewer(palette='Set3')+
   theme_classic() + 
  ylab('Percent Total Cost')+
  xlab('Practice')+
  theme_grey(base_size = 18)+
  theme(legend.position="none")+
  xlab('')
g + theme(axis.text.x = element_text(angle = 60, hjust = 1))

#cost of non-farm scenarios
names(tm)
View(tm[winners+1,])

View(tm[winners+1,36])
mean(tm[winners+1,36])/1000000000
