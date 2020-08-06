setwd("C:/Users/dania/Documents/Pitt/BioStats/Module 4")
x <- read.table("op.txt",sep="\t",header=T,stringsAsFactors=F)
x$SITE.CODED <- factor(x$SITE.CODED)
x 

glm <- glm(OP~fev1.pct+fvc.pct+dlco.pct+PREOP.CA+PREOP.CAD+PREOP.COPD+PREOP.DM+PREOP.GERD+PREOP.HTN+T.SIZE+SITE.CODED, family="binomial",data=x)
summary(glm)
anova(glm, test="LR")
library(car)
Anova(glm, type=2, test="LR") #same four are significant, taking all but those four out of the model


glm2 <- glm(OP~fev1.pct+PREOP.COPD+T.SIZE+SITE.CODED,family="binomial",data=x)
anova(glm2,test="LR")
Anova(glm2, type=2, test="LR")

#this bit is making predictions based on our model what the probability is of a patient getting a lobectomy or segmentectomy
#I had trouble making this graph but this code should work, if it doesn't LMK
#the graph is taking each patient's OP type and the probability of them getting that operation--a super predictive model will have good separation between the probabilities and the OP type, ours is not great bc it spans the width of the Y axis
OP <- c(0,	1,	0,	0,	0,	0,	0,	1,	0,	1,	1,	1,	0,	0,	1,	1,	1,	0,	0,	0,	0,	1,	0,	1,	1,	0,	1,	0,	0,	1,	0,	0,	1,	0,	0,	1,	0,	0,	0,	1,	0,	1,	1,	0,	0,	1,	0,	0,	0,	1,	1,	1,	0,	0,	0,	0,	1,	0,	1,	1,	0,	1,	0,	0,	0,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	0,	0,	0,	1,	1,	0,	1,	0,	1,	1,	1,	0,	1,	0,	0,	0,	1,	0,	0,	0,	0,	1,	1,	1,	1,	0,	0,	1,	0,	0,	0,	1,	1,	0,	1,	0,	1,	0,	0,	0,	1,	0,	1,	0,	1,	1,	1,	1,	1,	1,	1,	1,	1,	0,	1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	0,	1,	0,	1,	1,	1,	1,	1,	0,	1,	0,	0,	1,	1,	0,	1,	1,	1,	1,	0,	1,	1,	1,	1,	1,	1,	0,	1,	1,	1,	0,	0,	1,	0,	0,	0,	0,	0,	1,	0,	0,	1,	1,	1,	0,	1,	0,	0,	0,	0,	1,	1,	0,	1,	0,	1,	0,	1,	0,	0,	0,	1,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	1,	1,	1,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	1,	0,	0,	1,	1,	0,	0,	0,	1,	1,	1,	1,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	1,	0,	0, 1,	1,	0,	0,	0,	0,	1,	0,	1,	0,	0,	1,	0,	0,	0,	1,	0,	0,	0,	0,	1,	0,	0,	1,	1,	0,	0,	1,	1,	0,	0,	1,	0,	1,	0,	0,	1,	0,	0,	1,	0,	1,	1,	0,	0,	0,	0,	0,	0,	1,	1,	0,	1,	1,	0,	0,	0,	1,	0,	0,	0,	0,	1,	1,	1,	0,	1,	0,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	1,	1,	1,	1,	0,	0,	0)
stripchart(predict(glm2,type="response")~OP,vertical=T, na.action = na.omit,pch=1,col="navy",
           xlab="Operation Type",ylab="Estimated P(Operation Type)",
           xaxt="n")
axis(1,at=1:2,c("Lobectomy","Segmentectomy"))

library(epiDisplay)
roc1 <- lroc(glm,graph=TRUE,line.col="red",grid=FALSE)
roc2 <- lroc(glm2,add=TRUE,line.col="navy",grid=FALSE) #ROC curve for initial model and fitted model, area under ROC tells you how informative your variables are to the response variable, in this case OP
#highest AUROC is 1, anything above 0.5 is at least somewhat predictive