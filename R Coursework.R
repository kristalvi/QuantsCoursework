#setting working directory
setwd("###")
##use \\ or /

#download package
library()

.libPaths("C:\\Rlibs")

install.packages("ISLR")
install.packages("wesanderson")
install.packages("RColorBrewer")
install.packages("modest") 
install.packages("ggplot2")
install.packages("fitdistrplus")
install.packages("lattice")
install.packages("MASS")

#https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode

library(ISLR)
library(wesanderson)
library(RColorBrewer)
library(modest)
library(ggplot2)
library(fitdistrplus)
library(lattice)
library(MASS)


?ISLR
?wesanderson

head(OJ)
unique(OJ$StoreID)
unique(OJ$STORE)

oJ <- OJ[1:10]

#to check if data is complete https://stackoverflow.com/questions/22253995/finding-no-of-rows-with-missing-data-in-r
sum(!complete.cases(oJ))
oJ[complete.cases(oJ),]
oJ[!complete.cases(oJ),]

#declare factors
oJ$StoreID <- as.factor(oJ$StoreID)
oJ$WeekofPurchase <- as.factor(oJ$WeekofPurchase)
oJ$SpecialCH <- as.factor(oJ$SpecialCH)
oJ$SpecialMM <- as.factor(oJ$SpecialMM)

#prepare count of occurrence
summary(oJ[c(2,3,8,9)])

#frequency tables
round(prop.table(table(oJ$StoreID))*100,2)

#define a function to create freq tables
freq <- function(x) {
  round(prop.table(table(x))*100,2)}
freq(oJ$WeekofPurchase)
freq(oJ$StoreID)
freq(oJ$SpecialCH)
freq(oJ$SpecialMM)

length(unique(oJ$WeekofPurchase))

#contigency tables

  #StoreID against Purchases
OJ_Table <- table(oJ[c(1,3)]);             OJ_Table
data.frame(matrix(paste(round(prop.table(OJ_Table,1)*100,2),"%",sep=""),ncol=7,byrow=FALSE))

  #StoreID against SpecialCH
OJ_Table <- table(oJ[c(3,8)]);             OJ_Table
data.frame(matrix(paste(round(prop.table(OJ_Table,1)*100,2),"%",sep=""),ncol=2,byrow=FALSE))
  #StoreID against SpecialMM
OJ_Table <- table(oJ[c(3,9)]);              OJ_Table
data.frame(matrix(paste(round(prop.table(OJ_Table,1)*100,2),"%",sep=""),ncol=2,byrow=FALSE))

#sprintf("%0.1f%%", prop.table(tab) * 100)
OJ_Table <- table(oJ$StoreID)
OJ_Table
prop.table(OJ_Table)
dimnames(OJ_Table)
sprintf("%0.2f%%", prop.table(OJ_Table)*100)

#per Purchase
OJ_Table <- table(oJ$Purchase)
BarPlot <- barplot(OJ_Table,main="Bar Graph for Total Count per Purchase Type",
                   xlab="Purchase Type",ylab="Count of Purchases",col=brewer.pal(2,"Pastel2"))
text(BarPlot,250,paste(round(OJ_Table/sum(OJ_Table)*100,2),"%",sep=" "),adj=1,cex=1.35,col="maroon4",pos=3)
text(BarPlot,450,OJ_Table,adj=1,cex=1.35,col="navyblue",pos=3)

#per week
OJ_Table <- table(oJ$WeekofPurchase)
BarPlot <- barplot(OJ_Table,main="Bar Graph for Total Count of Purchases per Week",
                   xlab="Week of Purchase",ylab="Count of Purchases",col="gray78")
text(BarPlot,3,paste(round(OJ_Table/sum(OJ_Table)*100,1),"%",sep=""),adj=1,cex=.62,col="maroon4",pos=3)
text(BarPlot,5,OJ_Table,adj=1,cex=.62,col="navyblue",pos=3)

#per store
OJ_Table <- table(oJ$StoreID)
BarPlot <- barplot(OJ_Table,main="Bar Graph for Total Count of Purchases per Store",
                   xlab="StoreID",ylab="Count of Purchases",col="rosybrown2")
text(BarPlot,30,paste(round(OJ_Table/sum(OJ_Table)*100,2),"%",sep=" "),adj=1,cex=1,col="maroon4",pos=3)
text(BarPlot,50,OJ_Table,adj=1,cex=1,col="navyblue",pos=3)

?par
par(mfrow=c(1,2))
?title
#per SpecialCH
OJ_Table <- table(oJ$SpecialCH)
BarPlot <- barplot(OJ_Table,main="Bar Graph for Total Count of \nPurchases per SpecialCH Status",
                  cex.main=.88,xlab="SpecialCH",ylab="Count of Purchases",col="wheat")
text(BarPlot,20,paste(round(OJ_Table/sum(OJ_Table)*100,2),"%",sep=" "),adj=1,cex=1,col="maroon4",pos=3)
text(BarPlot,200,OJ_Table,adj=1,cex=1,col="navyblue",pos=3)

#per SpecialMM
OJ_Table <- table(oJ$SpecialMM)
BarPlot <- barplot(OJ_Table,main="Bar Graph for Total Count of \nPurchases per SpecialMM Status",
                  cex.main=.88,xlab="SpecialMM",ylab="Count of Purchases",col="powderblue")
text(BarPlot,20,paste(round(OJ_Table/sum(OJ_Table)*100,2),"%",sep=" "),adj=1,cex=1,col="maroon4",pos=3)
text(BarPlot,200,OJ_Table,adj=1,cex=1,col="navyblue",pos=3)

#END

sum(BarPlot)
scale <- scale_color_manual(values = wes_palette(5, "darjeeling"))
scale <- wes_palette(name = "Darjeeling", type="continuous")
#2,3,8,9
    
  #based on week number
  OJ_Table <- table(oJ[2])
  prop.table(OJ_Table)
  plot(OJ_Table)

#weeknumber vs special offer
OJ_Table <- table(oJ[c(2,8)])
OJ_Table

#stores
head(OJ)
barplot(OJ_Table,main="Bar Plot per Store ID",xlab="StoreID",ylab="Count of Data",col=brewer.pal(5,"Dark2"))

#stores vs prices
head(OJ)
boxplot(LoyalCH~StoreID, data=oJ)
boxplot(PriceCH~StoreID, data=oJ)
boxplot(PriceMM~StoreID, data=oJ)

summary(oJ$PriceMM~oJ$StoreID)

tapply(oJ$PriceCH, oJ$StoreID, quantile)

tapply(oJ$PriceMM, oJ$StoreID, quantile)

hist(oJ$PriceCH)

ggplot2.histogram(data=oJ$PriceCH,xName="",groupName='StoreID')

hist(PriceCH~StoreID, data=OJ)

#stores and purchases


ftable(OJ_Table)

  OJ_Table <- table(oJ[c(3,1)])
  OJ_Table
  
#Continuous
  
  summary(oJ[c(4:7,10)])
  
  #mode
  approximate_mode <- function(x) {
    d <- density(x)
    d$x[which.max(d$y)]
  }
  
  
  approximate_mode(oJ$PriceCH)
  approximate_mode(oJ$PriceMM)
  approximate_mode(oJ$DiscCH)
  approximate_mode(oJ$DiscMM)
  
  mlv(oJ$PriceCH, method="mfv")
  
  #https://discuss.analyticsvidhya.com/t/is-there-any-inbuilt-function-for-calculation-of-mode-in-r/5711
  mode2 <- function(x) {
    count_num <- unique(x)
    count_num [which.max(tabulate(match(x, count_num )))]
  }
  
  mode2(oJ$PriceCH)
  mode2(oJ$PriceMM)
  mode2(oJ$DiscCH)
  mode2(oJ$DiscMM)
  mode2(oJ$LoyalCH)
  
  range(oJ$PriceCH)
  range(oJ$PriceMM)
  range(oJ$DiscCH)
  range(oJ$DiscMM)
  range(oJ$LoyalCH)
  
  IQR(oJ$PriceCH)
  IQR(oJ$PriceMM)
  IQR(oJ$DiscCH)
  IQR(oJ$DiscMM)
  IQR(oJ$LoyalCH)
  
  #outliers
  index.out1 <- oJ$PriceCH < quantile(oJ$PriceCH,0.25)-(1.5*IQR(oJ$PriceCH))
  oJ$PriceCH[index.out1]
  
  index.out2 <- oJ$Price > quantile(oJ$PriceCH,0.75)+(1.5*IQR(oJ$PriceCH))
  oJ$PriceCH[index.out2]
  
  length(oJ$PriceCH[index.out1]) + length(oJ$PriceCH[index.out2])
  
  #function to count outliers
      countOutliers <- function(x){
        index.out1 <- x < quantile(x,0.25)-(1.5*IQR(x));    x[index.out1]
        index.out2 <- x > quantile(x,0.25)+(1.5*IQR(x));    x[index.out2]
        length(x[index.out1]) + length(x[index.out2])
      }
  #function to view outliers
      DefOutliers <- function(x){
        index.out1 <- x < quantile(x,0.25)-(1.5*IQR(x));    x[index.out1]
        index.out2 <- x > quantile(x,0.25)+(1.5*IQR(x));    x[index.out2]
        c(x[index.out1],x[index.out2])
      }
  #function to view non-outliers
      NonOutliers <- function(x){
        index.out1 <- x >= quantile(x,0.25)-(1.5*IQR(x)) & x <= quantile(x,0.25)+(1.5*IQR(x))
        c(x[index.out1])
      }
    
  countOutliers(oJ$PriceCH); countOutliers(oJ$PriceMM); countOutliers(oJ$DiscCH); countOutliers(oJ$DiscMM)
  countOutliers(oJ$LoyalCH); DefOutliers(oJ$PriceCH); DefOutliers(oJ$DiscCH); DefOutliers(oJ$DiscMM);
  NonOutlies(oj$PriceCH); NonOutliers(oJ$DiscCH); NonOutliers(oJ$DiscMM)
  
  unique(DefOutliers(oJ$DiscCH))sn
  
  sd(oJ$PriceCH); sd(oJ$PriceMM)
  sd(oJ$DiscCH)
  sd(oJ$DiscMM)
  sd(oJ$LoyalCH)
  
  #https://stat.ethz.ch/R-manual/R-devel/library/stats/html/shapiro.test.html
  
  shapiro.test(oJ$PriceCH)
  shapiro.test(oJ$PriceMM)
  shapiro.test(oJ$DiscCH)
  shapiro.test(oJ$DiscMM)
  shapiro.test(oJ$LoyalCH)
  
  qqplot(oJ$PriceCH)
  
  hist(oJ$PriceCH, main="Histogram for PriceCH",xlab="PriceCH")
  hist(oJ$PriceMM, main="Histogram for PriceMM",xlab="PriceMM")
  hist(oJ$DiscCH, main="Histogram for DiscountCH",xlab="Disc")
  hist(oJ$DiscMM, main="Histogram for DiscountMM",xlab="DiscMM")
  hist(oJ$LoyalCH, main="Histogram for LoyalCH",xlab="LoyalCH")
  
  mu.oJ = mean(oJ$PriceCH)
  sd.oJ = sd(oJ$PriceCH)
  
  hist(oJ$PriceCH, freq=F) + points(oJ$PriceCH,dnorm(oJ$PriceCH,mean=mu.oJ,sd=sd.oJ))
  
  #https://www.statmethods.net/graphs/density.html
  
  sm.density.compare(OJ$PriceCH, oJ$StoreID, xlab="Miles Per Gallon")
  
  ggplot(oJ) + geom_density(aes(x = oJ$PriceCH, fill = oJ$StoreID), alpha = 0.2)
  ggplot(oJ) + geom_density(aes(x = oJ$PriceMM, fill = StoreID), alpha = 0.2)
  ggplot(oJ) + geom_density(aes(x = oJ$DiscCH, fill = StoreID), alpha = 0.2)
  ggplot(oJ) + geom_density(aes(x = oJ$DiscMM, fill = StoreID), alpha = 0.2)
  ggplot(oJ) + geom_density(aes(x = oJ$LoyalCH, fill = StoreID), alpha = 0.2)
  
  head(oJ)  
  #https://stackoverflow.com/questions/21563864/ggplot2-overlay-density-plots-r
  #http://homepage.divms.uiowa.edu/~luke/classes/STAT4580/histdens.html
  
  
  ggplot(oJ) + geom_density(aes(x = PriceCH, fill = "lightcoral"), alpha = 0.2)
  ggplot(oJ) + geom_density(aes(x = PriceCH, fill = StoreID), alpha = 0.2)
  
  #https://stackoverflow.com/questions/1497539/fitting-a-density-curve-to-a-histogram-in-r
  par(mfrow=c(3,5))
  hist(oJ$PriceCH, prob=TRUE, col="lavenderblush2",main="Histogram & Density Plot \nof PriceCH",xlab="PriceCH") 
  lines(density(oJ$PriceCH), col="red4", lwd=2)
  hist(oJ$PriceMM, prob=TRUE, col="steelblue1",main="Histogram & Density Plot \nof PriceMM",xlab="PriceMM")
  lines(density(oJ$PriceMM), col="midnightblue", lwd=2)
  hist(oJ$DiscCH, prob=TRUE, col="aquamarine2",main="Histogram & Density Plot \nof DiscCH",xlab="DiscCH")
  lines(density(oJ$DiscCH), col="forestgreen", lwd=2)
  hist(oJ$DiscMM, prob=TRUE, col="bisque2",main="Histogram & Density Plot \nof DiscMM",xlab="DiscMM")
  lines(density(oJ$DiscMM), col="darkmagenta", lwd=2)
  hist(oJ$LoyalCH, prob=TRUE, col="honeydew3",main="Histogram & Density Plot \nof LoyalCH",xlab="LoyalCH",nclass=38)
  lines(density(oJ$LoyalCH), col="gray0", lwd=2)
  boxplot(oJ$PriceCH, col="lavenderblush2",main="Boxplot for PriceCH",ylim=c(1.7,2.3))
  boxplot(oJ$PriceMM, col="steelblue1",main="Boxplot for PriceMM")
  boxplot(oJ$DiscCH, col="aquamarine2",main="Boxplot for DiscCH",ylim=c(0.0,0.8))
  boxplot(oJ$DiscMM, col="bisque2",main="Boxplot for DiscMM")
  boxplot(oJ$LoyalCH, col="honeydew3",main="Boxplot for LoyalCH")
  qqnorm(oJ$PriceCH, col="red4", main="Normal Q-Q Plot\nfor PriceCH", ylim=c(1.7,2.3)); qqline(oJ$PriceCH)
  qqnorm(oJ$PriceMM, col="midnightblue", main="Normal Q-Q Plot\nfor PriceMM"); qqline(oJ$PriceMM)
  qqnorm(oJ$DiscCH, col="forestgreen", main="Normal Q-Q Plot\nfor DiscCH", ylim=c(0.0,0.8)); qqline(oJ$DiscCH)
  qqnorm(oJ$DiscMM, col="darkmagenta", main="Normal Q-Q Plot\nfor DiscMM"); qqline(oJ$DiscMM)
  qqnorm(oJ$LoyalCH, col="gray0", main="Normal Q-Q Plot\nfor LoyalCH"); qqline(oJ$LoyalCH)
  
  #checking for other distributions
  
  fitdistr(oJ$DiscCH, "lognormal")
  qqplot(oJ$DiscCH, "log-normal")
  data2lnorm(oJ$DiscCH, plot=TRUE, forceNA = FALSE, ...)
  
  #relationship between variables
  #https://www.statmethods.net/graphs/scatterplot.html
  ?splom

  super.sym <- trellis.par.get("superpose.symbol")
  splom(oJ[c(4:7,10)], groups=oJ$StoreID, data=oJ,
        panel=panel.superpose, 
        key=list(title="Three Cylinder Options",
                 columns=5,
                 points=list(pch=super.sym$pch[1:5],
                             col=super.sym$col[1:5]),
                 text=list(c("4 Cylinder","6 Cylinder","8 Cylinder"))))
  
  pairs(~PriceCH+PriceMM+DiscCH+DiscMM+LoyalCH,data=oJ, main="Scatterplot Matrix for Numerical Factors",
        col=rgb(0,100,0,50,maxColorValue=255),panel=panel.smooth)
  
  pairs(~PriceCH+PriceMM+DiscCH+DiscMM+LoyalCH,data=oJ,col=rgb(0,100,0,50,maxColorValue=255),panel=panel.smooth)
  
  
  round(cor(oJ[c(4:
              7,10)]),2)
  
  summary(lm(PriceCH ~ PriceMM, data=oJ))$r.squared; summary(lm(PriceCH ~ DiscCH, data=oJ))$r.squared
  summary(lm(PriceCH ~ DiscMM, data=oJ))$r.squared; summary(lm(PriceCH ~ LoyalCH, data=oJ))$r.squared
  summary(lm(PriceMM ~ DiscCH, data=oJ))$r.squared; summary(lm(PriceMM ~ DiscMM, data=oJ))$r.squared
  summary(lm(PriceMM ~ LoyalCH, data=oJ))$r.squared; summary(lm(DiscCH ~ DiscMM, data=oJ))$r.squared
  summary(lm(DiscCH ~ LoyalCH, data=oJ))$r.squared; summary(lm(DiscMM ~ LoyalCH, data=oJ))$r.squared
  
  #SECTION 2
  #create contingency tables
  #StoreID vs Purchase
  iD.Purchase <- table(oJ[c(3,1)]); iD.Purchase
  chisq.test(iD.Purchase, correct=FALSE)
  #StoreID vs SpecialCH
  iD.SpecialCH <- table(oJ[c(3,8)]); iD.SpecialCH
  chisq.test(id.SpecialCH, correct=FALSE)
  #StoreID vs SpecialMM
  iD.SpecialMM <- table(oJ[c(3,9)]); iD.SpecialMM
  chisq.test(iD.SpecialMM, correct=FALSE)
  #Purchase vs SpecialCH
  pur.SpecialCH <- table(oJ[c(1,8)]); pur.SpecialCH
  chisq.test(pur.SpecialCH, correct=FALSE)
  #Purchase vs SpecialMM
  pur.SpecialMM <- table(oJ[c(1,9)]); pur.SpecialMM
  chisq.test(pur.SpecialMM,correct=FALSE)
  #cannot use
    specialCH.SpecialMM <- table(oJ[c(8,9)]); specialCH.SpecialMM
    chisq.test(specialCH.SpecialMM,correct=FALSE)
  #purchaseWeekofPurchase
  pur.WeekOfPurchase <- table(oJ[c(1,2)]); pur.WeekOfPurchase
    countcell <- pur.WeekOfPurchase < 5  
      9/104
    chisq.test(pur.WeekOfPurchase,correct=FALSE)
  
      ?chisq.test
  #cannot use Special CH vs SpecialMM
  #SpecialCH vs SpecialMM
  OJ_Table <- table(oJ[c(8,9)]); OJ_Table 
  
  #Crammer's V Test V = sqrt(X^2 / [nobs * (min(ncols, nrows) - 1)])
  cv_id.Purchase <- sqrt(121.55 / (1070* (min(2,5)-1)))
  cv_pur.SpecialCH <- sqrt(18.86 / (1070* (min(2,2)-1)))
  cv_pur.SpecialMM <- sqrt(32.69 / (1070 * (min(2,2)-1)))
  cv_id.SpecialCH <- sqrt(175.83 / (1070 * (min(2, 5)-1)))
  cv_id.SpecialMM <- sqrt(101.44 / (1070 * (min(5,2)-1)))
  cv_pur.WeekOfPurchase <- sqrt(112.37 / (1070 * (min(52,2)-1)))
  
  cv_id.Purchase; cv_pur.SpecialCH;cv_pur.SpecialMM; cv_id.SpecialCH;
  cv_id.SpecialMM; cv_pur.WeekOfPurchase
  
  #SECTION 3
  #Predictive Analysis
  
  head(oJ)
  #A.	Purchase and Loyal CH
  pur.LoyalCH <- table(oJ[c(1,10)])
  #B.	SpecialCH and LoyalCH
  specialCH.LoyalCH <- table(oJ[c(8,10)])
  #C.	SpecialMM and LoyalCH
  specialMM.LoyalCH <- table(oJ[c(9,10)])
  #D. Purchase and PriceCH
  pur.LoyalCH <- table(oJ[c(1,4)])
  
  pur.LoyalCH
  ?t.test
  t.test(oJ$LoyalCH ~oJ$Purchase, alternative = "greater", var.equal = FALSE)
  t.test(oJ$LoyalCH ~oJ$SpecialCH, alternative = "less", var.equal = FALSE)
  t.test(oJ$LoyalCH ~oJ$SpecialMM, alternative = "greater", var.equal = FALSE)
  t.test(oJ$PriceCH ~oJ$Purchase, alternative = "two.sided", var.equal = FALSE)
  
  #descriptive for categorical vs numerical
  
  boxplot(oJ$PriceCH, col="lavenderblush2",main="Boxplot for PriceCH",ylim=c(1.7,2.3))
  boxplot(oJ$PriceMM, col="steelblue1",main="Boxplot for PriceMM")
  boxplot(oJ$DiscCH, col="aquamarine2",main="Boxplot for DiscCH",ylim=c(0.0,0.8))
  boxplot(oJ$DiscMM, col="bisque2",main="Boxplot for DiscMM")
  
  #https://stackoverflow.com/questions/19876505/boxplot-show-the-value-of-mean
  par(mfrow=c(1,3))
  means <- aggregate(LoyalCH ~  Purchase, oJ, mean)
  boxplot(LoyalCH ~ Purchase, oJ, col=brewer.pal(2,"Pastel2"),xlab="Purchase",
          ylab="LoyalCH",main="LoyalCH per Purchase Group")
      points(1:2, means$LoyalCH, cex=2, col = "red")
      text(1:2, means$LoyalCH + 0.08, labels = round(means$LoyalCH*100,2))
  means <- aggregate(LoyalCH ~  SpecialCH, oJ, mean)
  boxplot(LoyalCH ~ SpecialCH, oJ, col="wheat",xlab="SpecialCH",
          ylab="LoyalCH",main="LoyalCH per SpecialCH Group")
      points(1:2, means$LoyalCH, cex=2, col = "red")
      text(1:2, means$LoyalCH + 0.08, labels = round(means$LoyalCH*100,2))
  means <- aggregate(LoyalCH ~  SpecialMM, oJ, mean)
  boxplot(LoyalCH ~ SpecialMM, oJ, col="powderblue",xlab="SpecialMM",
          ylab="LoyalCH",main="LoyalCH per SpecialMM Group")
      points(1:2, means$LoyalCH, cex=2, col = "red")
      text(1:2, means$LoyalCH + 0.08, labels = round(means$LoyalCH*100,2))
      
  #run anova
      aov.out = aov(LoyalCH ~ StoreID, data=oJ)
      summary(aov.out)
      TukeyHSD(aov.out)
      summary.lm(aov.out)
      head(oJ)
      
      
      aov.out = aov(LoyalCH ~ WeekofPurchase, data=oJ)
      summary(aov.out)
      TukeyHSD(aov.out)
      summary.lm(aov.out)
      
  #building the model
      
      head(oJ)
      
      #foward
      fit <- lm(LoyalCH ~ Purchase + WeekofPurchase + StoreID + PriceCH
                + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM,data=oJ)
      summary(fit)
      step <- stepAIC(fit, direction="forward")
      step$anova # display results
      summary(step)
      
      
      #Backward
      fit <- lm(LoyalCH ~ Purchase + WeekofPurchase + StoreID + PriceCH
                + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM,data=oJ)
      step <- stepAIC(fit, direction="backward")
      step$anova # display results
      summary(fit)
      
      fit <- lm(LoyalCH ~ Purchase + WeekofPurchase + PriceCH
                + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM,data=oJ)
      step <- stepAIC(fit, direction="backward")
      step$anova # display results
      summary(fit)
      
      ?stepAIC()
      fit <- lm(LoyalCH ~ Purchase + StoreID + PriceCH
                + PriceMM + DiscCH + DiscMM + SpecialCH + SpecialMM
                ,data=oJ)
      step <- stepAIC(fit, direction="both")
      summary(fit)
      
      #Forward based on p-value
      fit <- lm(LoyalCH ~ Purchase, data=oJ); summary(fit) #iteration 1
      fit <- lm(LoyalCH ~ Purchase + SpecialCH, data=oJ); summary(fit) #iteraton 2
      fit <- lm(LoyalCH ~ Purchase + SpecialCH + SpecialMM, data=oJ); summary(fit) #iteration 3
      
      fit_forward <- lm(LoyalCH ~ Purchase, data=oJ); summary(fit_forward)
      
      #stop at iteration2
      fit_backward <- fit
      fit_backward <- lm(LoyalCH ~ as.factor(Purchase=='MM') + as.factor(StoreID==3) + as.factor(StoreID==4) + PriceCH, data=oJ)
      
      #Backward Selection
      fit <- lm(LoyalCH ~ ., data=oJ); summary(fit); fit <- lm(LoyalCH ~ . - WeekofPurchase, data=oJ); summary(fit); fit <- lm(LoyalCH ~ . - WeekofPurchase - 
      PriceMM, data=oJ); summary(fit); fit <- lm(LoyalCH ~ . - WeekofPurchase - PriceMM - DiscCH, data=oJ); summary(fit); fit <- lm(LoyalCH ~ Purchase + 
      as.factor(StoreID==1) + as.factor(StoreID==2) + as.factor(StoreID==3) + as.factor(StoreID==4) + PriceCH + DiscMM + SpecialCH, data=oJ); summary(fit)
      fit <- lm(LoyalCH ~ Purchase + as.factor(StoreID==1) + as.factor(StoreID==2) + as.factor(StoreID==3) + as.factor(StoreID==4) + PriceCH + DiscMM, 
      data=oJ); summary(fit); fit <- lm(LoyalCH ~ Purchase + as.factor(StoreID==2) + as.factor(StoreID==3) + as.factor(StoreID==4) + PriceCH + DiscMM, 
      data=oJ); summary(fit); fit <- lm(LoyalCH ~ Purchase + as.factor(StoreID==2) + as.factor(StoreID==3) + as.factor(StoreID==4) + PriceCH, data=oJ); 
      summary(fit) 
      
      par(mfrow=c(1,4))
      fit_backward <- lm(LoyalCH ~ Purchase + PriceCH, data=oJ); summary(fit_backward); res = rstandard(fit_backward)
      plot(oJ$PriceCH, res, col = "midnightblue", main = "Residual Plot for PriceCH"); abline(0, 0, col = "red")
      plot(fitted(fit_backward), res, col = "violet", main =" Residual Plot Against Fitted Values") ; abline(0, 0, col = "red")
      qqnorm(res, col = "orange", main = "Q-Q Plot of Residuals") ; qqline(res, col = "red"); 
      hist(res, main="Histogram of Residuals")
      
      #anova(fit_forward, fit_backward)
      
      #Step
      index.Special <- oJ$SpecialCH == 0
      index.Special0 <- oJ$Special[index.Special]
      
      
      fit <- lm(LoyalCH ~ Purchase, data=oJ)
      summary(fit)
      fit <- lm(LoyalCH ~ Purchase + SpecialCH, data=oJ)
      summary(fit)
      fit <- lm(LoyalCH ~ Purchase + SpecialCH + SpecialMM, data=oJ)
      summary(fit)
        #remove SpecialMM1
      fit <- lm(LoyalCH ~ Purchase + SpecialCH, data=oJ)
      summary(fit)
      
      
      ##NEW
      #http://homepages.inf.ed.ac.uk/bwebb/statistics/ANOVA_in_R.pdf
      with(oJ, tapply(LoyalCH, StoreID, mean))
      with(oJ, tapply(LoyalCH, StoreID, var))
      with(oJ, bartlett.test(LoyalCH ~ StoreID))
      lm.out <- with(oJ, lm(LoyalCH ~ StoreID))
      summary(lm.out)
      
      oneway.test(LoyalCH ~ StoreID, data=oJ)
      
      #Weeknumber
      with(oJ, tapply(LoyalCH, WeekofPurchase, mean))
      with(oJ, tapply(LoyalCH, WeekofPurchase, var))
      with(oJ, bartlett.test(LoyalCH ~ WeekofPurchase))
      lm.out <- with(oJ, lm(LoyalCH ~ WeekofPurchase))
      summary(lm.out)
      
      #residuals
      
  
      