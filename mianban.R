setwd("C:/Users/guojing/Desktop/biye")
library(reshape2)

city=read.csv("city.csv")
library(reshape2)
city2=melt(city)
colnames(city2)=c("prov","year","城市化率")

factory=read.csv("factory.csv")
factory2=melt(factory)

gdp=read.csv("per_prov_gdp.csv")
gdp2=melt(gdp)

fdi=read.csv("fdi.csv")
fdi2=melt(fdi)

cpi=read.csv("cpi.csv")
cpi2=melt(cpi)

fac_output=read.csv("fac_output.csv")
fac_output2=melt(fac_output)

garbage_treat=read.csv("garbage_treat.csv")
garbage_treat2=melt(garbage_treat)

per_bus=read.csv("per_bus.csv")
per_bus2=melt(per_bus)

per_garden=read.csv("per_garden.csv")
per_garden2=melt(per_garden)

so2=read.csv("so2.csv")
so2=melt(so2)

water=read.csv("water.csv")
water2=melt(water)


all_data <- cbind(city2,gdp2$value,cpi2$value,fdi2$value,factory2$value,
               fac_output2$value,per_bus2$value,per_garden2$value,
               so2$value,water2$value,garbage_treat2$value)
colnames(all_data)=c("prov","year","city","per_gdp","cpi","fdi",
                  "factory","output","per_bus","per_green","so2","water","garbage")
all_data$year=gsub("X","",all_data$year)
all_data$year=gsub("年","",all_data$year)

mianban=all_data[,1:10]

data2=all_data[,3:13]

data3=all_data[which(all_data$year==2017),]



####面板分析

tlist1<-xts(mianban$cpi,as.Date(mianban$year))
adf.test(tlist1)
tlist2<-xts(data$index2,as.Date(data$updatetime))
adf.test(tlist2)



##########new分析
environment=read.csv("final.csv",header = T)
environment=environment[1:272,]
library(missForest)
miss.e=missForest(environment[,2:17])
e=miss.e$ximp

fa.parallel(a, n.obs = 200, fa = "both", n.iter = 200)
fa_model1 <- fa(a, nfactors = 2, rotate = "none", fm = "ml")
fa_model2 <- fa(a, nfactors = 3, rotate = "varimax", fm = "ml")
factor.plot(fa_model2)
fa.diagram(fa_model2, simple = T)

##bartelle检验
cortest.bartlett(e) 
##kmo检验
kmo = function( data ){
  
  library(MASS)
  X <- cor(as.matrix(data))
  iX <- ginv(X)
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
  # correlation matrix. That is the
  # negative of the partial correlations,
  # partialling out all other variables.
  
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  
  # Reporting the conclusion
  if (kmo >= 0.00 && kmo < 0.50){
    test <- 'The KMO test yields a degree of common variance
    unacceptable for FA.'
  } else if (kmo >= 0.50 && kmo < 0.60){
    test <- 'The KMO test yields a degree of common variance miserable.'
  } else if (kmo >= 0.60 && kmo < 0.70){
    test <- 'The KMO test yields a degree of common variance mediocre.'
  } else if (kmo >= 0.70 && kmo < 0.80){
    test <- 'The KMO test yields a degree of common variance middling.'
  } else if (kmo >= 0.80 && kmo < 0.90){
    test <- 'The KMO test yields a degree of common variance meritorious.'
  } else {
    test <- 'The KMO test yields a degree of common variance marvelous.'
  }
  
  ans <- list(  overall = kmo,
                report = test,
                individual = MSA,
                AIS = AIS,
                AIR = AIR )
  return(ans)
  
}    # end of kmo()
kmo(e)

