#Cost loop version 3. 


#Bring in Data
max.pot <- read.csv("data/max_potential_byclass.csv") #bring in max potential stems/acre/class

max.pot <- max.pot[2,] #keep only max potential productivity
max.pot <- max.pot[,-1] #remove row names
#max.pot[1,] <- rep(200, 30) #Test to see what 200 stems/acre universally applied would do
#max.pot[1,] <- rep(600, 30) #Test to see what 600 stems/acre universally applied would do (this is HQT's stated max observed)
#max.pot[1,] <- rep(2000, 30) #Test to see what 2000 stems/acre universally applied would do (our estimate of max benefit to monarch)
costs <- read.csv('data/practice.costs.csv') #new line item costs
costs2 <- read.csv('data/practice.costs.mjv.csv') #MJV survey cost numbers

#ap.prac <- read.csv('applied.practices.csv') #original dataset based only on my interview
ap.prac <- read.csv('data/applied.practices.tgpc.csv') #revised dataset based upon Tallgrass Prarie Center Interview with Justin S. 



#Derive necessary data
prac.costs <- ap.prac[,2:31]*costs[,2] #calculate the costs for each practice
#cl.costs <- colSums(prac.costs) #this is the total cost calculation to use for main work
cl <- names(db[c(3:11,16:23,28:35,37:40, 42)]) #get list of land cover classes for analysis
db1 <- cbind(db[,1:2],db[,cl]) #select columns used in this study

#Establish names
farm <- c("cl001", "cl002", "cl003", "cl004", "cl005", "cl007", "cl008" ,     "cl009",'cl014',	'cl015')
pasture <- c(	'cl078',	'cl079')
grass <- c('cl052',	'cl076',	'cl077',	'cl098'	,'cl099')
roads <- c('cl110',	'cl120',	'cl140',	'cl174')
rails <- 'cl200'
power <- 'cl100'
urban <- c('cl021',	'cl022',	'cl023',	'cl024',	'cl025',	'cl026')

scenario.costs <- matrix(nrow=length(tm$Scenario),ncol = 38 )
colnames(scenario.costs) <- c('total','farm','pasture','grassland','roads','rails','power','urban', cl )
rownames(scenario.costs) <- tm$no
scenario.costs <- data.frame(scenario.costs)

acres <- rep(NA, 218)

#Summaries by practice
prac.cost.sum <- matrix(nrow = nrow(scenario.costs), ncol = nrow(ap.prac))
prac.cost.sum <- data.frame(prac.cost.sum)
colnames(prac.cost.sum) <- ap.prac[,1]

#Step 1: Calculate base stems
base <- db1 #repositorty for base stems number
base[,] <- NA #clean this to make sure no relict data
base[,1:2] <- db1[,1:2]

for(i in 1:length(cl)){
  base[,cl[i]] <- m3[,cl[i]]*tm[1,cl[i]] 
}
base$total <- rowSums(base[,3:32], na.rm = T)
total.stems <- sum(base$total)



#Step 2: Calc (a) new stems per scenario, (b) cost per scenario, (c) cost by sector, and (d) cost by practice

for(k in 2:nrow(tm)){
  
  #(a) new stems for this scenario
  db2 <- db1
  db2[3:32] <- NA
  db3 <- db2
  
  for(i in 1:length(cl)){
    db2[,cl[i]] <- m3[,cl[i]]*tm[k,cl[i]] #new stems calculated here
  }
  
  diff <- (db2[,cl]-base[,cl]) #subtract tested from baseline
  diff <- cbind(base[,1:2],diff)
  
  max.pot.d <- max.pot[rep(1:nrow(max.pot),each=length(diff$cnty_name)),]  #repeat the max for number of rows in diff
  x1 <- diff[,cl] #set aside the difference, now use x1
  x1[cl] <- x1[cl]/max.pot.d[cl] #number of acres needed to be restored for class in county.
  #x1$state_name <- as.factor(x1$state_name)
  acres[k] <- sum(x1)
  
  #(b) cost for this scenario 
  for(j in 1:length(cl)){
    db3[,j+2] <- x1[,j]*rep(sum(prac.costs[j]),nrow(x1))
  }
  
  #(c) cost by sector
  scenario.costs[k,1] <- sum(db3[,cl]) #Total Cost of Scenario
  scenario.costs[k,2] <- sum(colSums(db3[,farm]))# ADD *5 if you expect 10-year turnover rate mapped to 50 yr timeframe
  scenario.costs[k, 3] <- sum(colSums(db3[,pasture]))# ADD *5 if you expect 10-year turnover rate mapped to 50 yr timeframe
  scenario.costs[k, 4] <- sum(colSums(db3[,grass]))
  scenario.costs[k, 5] <- sum(colSums(db3[,roads]))
  scenario.costs[k, 6] <- sum((db3[,rails]))
  scenario.costs[k, 7] <- sum((db3[,power]))
  scenario.costs[k, 8] <- sum((db3[,urban]))
  sector.cost <- colSums(db3[cl])
  scenario.costs[k,cl] <- sector.cost[cl]  
  
  #Calculate costs of each practice for the given scenario in this loop
  for(j in 1:ncol(prac.cost.sum)){
    #this multiplies each row of the diff (which is increase in acres) by the cost per acre
    # of each applied practice. It then calcs the sum by practice and puts it in the right column
    prac.cost.sum[k,j] <-sum(mapply('*',x1[cl], prac.costs[j,]))
  }
  
}

prac.cost.sum$total <- rowSums(prac.cost.sum)

scenario.costs[1,] <- 0 #Set baseline to zero costs
scenario.costs$num <- seq(from = 0, to = 218, by =1 )

prac.cost.sum[1,] <- 0 #Set baseline to zero costs
prac.cost.sum$num <- seq(from = 0, to = 218, by =1 )

prac.cost.sum$total- scenario.costs$total
which(prac.cost.sum$total- scenario.costs$total < -0.01)






