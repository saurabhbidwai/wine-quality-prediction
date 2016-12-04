setwd("C:/Users/DELL/Desktop/Aegis/Machine Learning/Data")
wine=read.csv("winequality-red (1).csv",stringsAsFactors = TRUE)

str(wine)
summary(wine)
cor(wine)
plot(wine)
pairs(wine)
plot(wine$free.sulfur.dioxide,wine$density)
hist(wine$free.sulfur.dioxide)

boxplot(wine)

boxplot(wine, outline = FALSE)
library("psych")
pairs.panels(wine)

minmax=function(a){
  anew=(a-mean(a))/(max(a)-min(a))
}

wine[,1]=minmax(wine[,1])
wine[,4]=minmax(wine[,4])
wine[,6]=minmax(wine[,6])
wine[,7]=minmax(wine[,7])

for(j in 1:ncol(wine)){
  v=summary(wine[,j])
  u=v[5]+1.5*IQR(v)
  l=v[2]-1.5*IQR(v)
  
  for(i in 1:nrow(wine)){
    if(wine[i,j]>u || wine[i,j]<l){
      wine[i,j]=mean(wine[,j])
    }
  }
}

#wine$alcohol1=ifelse(wine$alcohol>=12.5 & wine$alcohol<=14.5,1,0)
#wine$pH1=ifelse(wine$pH>=3.3 & wine$pH<=3.6,1,0)

library(corrplot)
corrplot(cor(wine))

#fit61=lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,data=wine)#0.3561

#fit61=lm(formula = quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = wine)#0.3567

#fit61=lm(quality~volatile.acidity+residual.sugar+free.sulfur.dioxide+density+alcohol,wine)#0.3176

#fit61=lm(quality~volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,data=wine)#0.3561

#fit61=lm(quality~fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,data=wine)#0.3559

#fit61=lm(quality~volatile.acidity+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+density+pH+sulphates+alcohol,data=wine)#0.3547

#fit61=lm(quality~volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=wine)#0.3471

#fit61=lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,data=wine)#0.3561

#fit61=lm(formula = quality ~ fixed.acidity:density + volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol , data = wine)

fit61=lm(formula = quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = wine1)

plot(fit61)
summary(fit61)

library("car")
vif(fit61)

step(fit61)
