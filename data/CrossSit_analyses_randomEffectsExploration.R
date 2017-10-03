# CrossSit Analyses - Supplement
# Supplement to analyses for "The company objects keep: Linking referents together during cross-situational word learning"
# Martin Zettersten, Erica Wojcik, Viridiana Benitez, & Jenny Saffran (submitted)

# The goal of this script is to illustrate the robustness of the reported results to alternative random effect structure.
# To this end, we explore the results of the main models from the paper when fitting alternative random effects structures.

#Load R packages/ custom scripts
source("summarizeData.R") #script for summarizing data, courtesy of "R cookbook" (http://www.cookbook-r.com/)
library(utils) #version 3.3.1
library(plyr) #version 1.8.4
library(ggplot2) #version 2.2.1
library(lme4) #version 1.1-13
library(stats) #version 3.3.1
library(cowplot) #version 0.8.0
library(AICcmodavg) #version 2.1-1
library(reshape2) #version 1.4.2
library(gtools) #version 3.5.0

### Define function to create plots of effects under different random effect structures ###
exploreRandomEffects=function(data,modelFormulaFixed,randomEffects,randomEffectsNames,parameterNumbers,indexEffect,labels=c("paper","alternative \nrandom effects structure")) {
  temp=data.frame()
  for (i in 1:length(randomEffects)) {
    print(i)
    randomEffect=randomEffects[i]
    #construct the formula with the current random effects structure
    curFormula=paste(modelFormulaFixed,randomEffect,sep=" ")
    #fit the model with the selected random effects structure
    curModel=glmer(curFormula,data=data,family=binomial, control=glmerControl(optimizer="bobyqa"))
    statMatrix=summary(curModel)$coefficients
    #pvalue with significance
    pvalue=statMatrix[indexEffect,4]
    if (pvalue<.001) {
      signifLabel="***"
    } else if (pvalue<.01) {
      signifLabel="**"
    } else if (pvalue<.05) {
      signifLabel="*"
    } else if (pvalue<.1) {
      signifLabel="+"
    } else {
      signifLabel="n.s."
    }
    
    if (i==1) {
      label=labels[1]
    } else {
      label=labels[2]
    }
    #CIs
    curCI=confint(curModel, method="Wald")
    lowerCI=curCI[parameterNumber[i]+indexEffect,1]
    upperCI=curCI[parameterNumber[i]+indexEffect,2]
    
    curDataFrame=data.frame(randomEffect=randomEffect,randomEffectName=randomEffectsNames[i],type=label,beta=statMatrix[indexEffect,1],zvalue=statMatrix[indexEffect,3],pvalue=pvalue, signifLabel=signifLabel,lowerCI=lowerCI, upperCI=upperCI)
    temp=rbind(temp,curDataFrame)
  }
  return(temp)
}


#Read in data
d = read.table("CrossSit.txt",sep="\t", header=TRUE)

##DATA
#subset for experiment 1 data
exp1D = subset(d, expName=="exp1")
#subset the data for experiment 2
exp2D = subset(d, expName=="exp2")
#subset the data for experiment 3
exp3D = subset(d, expName=="exp3_uniform"|expName=="exp3_skewed")


####analyze word learning#####

####Experiment 1 Word Learning####

##a. The model from the manuscript.
#model reported in the manuscript with maximal random effects structure
mLearn_exp1=glmer(isRight~offset(logit(offset.25))+orderByTargetRoleC+(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC|targetRole),data=subset(exp1D, trialKind=="training"),family=binomial, control=glmerControl(optimizer="bobyqa"))
summary(mLearn_exp1)

#Fixed effects structure (constant)
modelFormulaFixed="isRight~offset(logit(offset.25))+orderByTargetRoleC+"
#define different random effects and a list of their parameters
randomEffects=c("(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC|targetRole)","(1|ResponseID)","(1+orderByTargetRoleC|ResponseID)","(1|targetRole)","(1+orderByTargetRoleC|targetRole)","(1|ResponseID)+(1|targetRole)","(1+orderByTargetRoleC|ResponseID)+(1|targetRole)","(1|ResponseID)+(1+orderByTargetRoleC|targetRole)")
randomEffectsNames=c("(1+trial|participant)+\n(1+trial|item)","(1|participant)","(1+trial|participant)","(1|item)","(1+trial|item)","(1|participant)+\n(1|item)","(1+trial|participant)+\n(1|item)","(1|participant)+\n(1+trial|item)")
parameterNumber=c(6,1,3,1,3,2,4,4)
#index of the effect in the model
indexEffect=2
#create a data frame for different effects
learn_1=exploreRandomEffects(data=subset(exp1D, trialKind=="training"),modelFormulaFixed,randomEffects=randomEffects,randomEffectsNames=randomEffectsNames,parameterNumber=parameterNumber,indexEffect=indexEffect,labels=c("paper","alternative \nrandom effects structure"))
#create plot
learn1=ggplot(learn_1,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0,linetype="dashed", size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ylim(0,0.26)+
  ggtitle("Word Learning Slope: Experiment 1")
learn1
##Experiment 2 Word Learning####
##a. The model from the manuscript - maximal random effects structure
mLearn_exp2=glmer(isRight~offset(logit(offset.25))+orderByTargetRoleC+(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC|targetRole),data=subset(exp2D, trialKind=="training"),family=binomial, control=glmerControl(optimizer="bobyqa"))
summary(mLearn_exp2)

#Fixed effects structure (constant)
modelFormulaFixed="isRight~offset(logit(offset.25))+orderByTargetRoleC+"
#define different random effects and a list of their parameters
randomEffects=c("(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC|targetRole)","(1|ResponseID)","(1+orderByTargetRoleC|ResponseID)","(1|targetRole)","(1+orderByTargetRoleC|targetRole)","(1|ResponseID)+(1|targetRole)","(1+orderByTargetRoleC|ResponseID)+(1|targetRole)","(1|ResponseID)+(1+orderByTargetRoleC|targetRole)")
randomEffectsNames=c("(1+trial|participant)+\n(1+trial|item)","(1|participant)","(1+trial|participant)","(1|item)","(1+trial|item)","(1|participant)+\n(1|item)","(1+trial|participant)+\n(1|item)","(1|participant)+\n(1+trial|item)")
parameterNumber=c(6,1,3,1,3,2,4,4)
#index of the effect in the model
indexEffect=2
#create a data frame for different effects
learn_2=exploreRandomEffects(data=subset(exp2D, trialKind=="training"),modelFormulaFixed,randomEffects=randomEffects,randomEffectsNames=randomEffectsNames,parameterNumber=parameterNumber,indexEffect=indexEffect,labels=c("paper","alternative \nrandom effects structure"))
#create plot
learn2=ggplot(learn_2,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0,linetype="dashed", size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ylim(0,0.26)+
  ggtitle("Word Learning Slope: Experiment 2")
learn2

####Experiment 3 Word Learning####
#model from the manuscript
mLearn_exp3=glmer(isRight~offset(logit(offset.25))+orderByTargetRoleC*distributionC+(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC*distributionC|targetRole),data=subset(exp3D, trialKind=="training"),family=binomial, control=glmerControl(optimizer="bobyqa"))
summary(mLearn_exp3)

#Fixed effects structure (constant)
modelFormulaFixed="isRight~offset(logit(offset.25))+orderByTargetRoleC*distributionC+"
#define different random effects and a list of their parameters
randomEffects=c("(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC*distributionC|targetRole)",
                "(1|ResponseID)",
                "(1+orderByTargetRoleC|ResponseID)",
                "(1|targetRole)",
                "(1+orderByTargetRoleC|targetRole)",
                "(1+distributionC|targetRole)", 
                "(1+orderByTargetRoleC+orderByTargetRoleC:distributionC|targetRole)",
                "(1+distributionC+orderByTargetRoleC:distributionC|targetRole)",
                "(1+orderByTargetRoleC*distributionC|targetRole)",
                "(1|ResponseID)+(1|targetRole)",
                "(1|ResponseID)+(1+orderByTargetRoleC|targetRole)",
                "(1|ResponseID)+(1+distributionC|targetRole)",
                "(1|ResponseID)+(1+orderByTargetRoleC+orderByTargetRoleC:distributionC|targetRole)",
                "(1|ResponseID)+(1+distributionC+orderByTargetRoleC:distributionC|targetRole)",
                "(1|ResponseID)+(1+orderByTargetRoleC*distributionC|targetRole)",
                "(1+orderByTargetRoleC|ResponseID)+(1|targetRole)",
                "(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC|targetRole)",
                "(1+orderByTargetRoleC|ResponseID)+(1+distributionC|targetRole)",
                "(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC+orderByTargetRoleC:distributionC|targetRole)",
                "(1+orderByTargetRoleC|ResponseID)+(1+distributionC+orderByTargetRoleC:distributionC|targetRole)"
                )
randomEffectsNames=c("(1+trial|participant)+\n(1+trial*condition|item)",
                     "(1|participant)",
                     "(1+trial|participant)",
                     "(1|item)",
                     "(1+trial|item)",
                     "(1+condition|item)",
                     "(1+trial+trial:condition|item)",
                     "(1+condition+trial:condition|item)",
                     "(1+trial*condition|item)",
                     "(1|participant)+\n(1|item)",
                     "(1|participant)+\n(1+trial|item)",
                     "(1|participant)+\n(1+condition|item)",
                     "(1|participant)+\n(1+trial+trial:condition|item)",
                     "(1|participant)+\n(1+condition+trial:condition|item)",
                     "(1|participant)+\n(1+trial*condition|item)",
                     "(1+trial|participant)+\n(1|item)",
                     "(1+trial|participant)+\n(1+trial|item)",
                     "(1+trial|participant)+\n(1+condition|item)",
                     "(1+trial|participant)+\n(1+trial+trial:condition|item)",
                     "(1+trial|participant)+\n(1+condition+trial:condition|item)")
parameterNumber=c(13,1,3,1,3,3,6,6,10,2,4,4,7,7,11,4,6,6,9,9)
#index of the effect in the model
indexEffect=2
#create a data frame for different effects
learn_3=exploreRandomEffects(data=subset(exp3D, trialKind=="training"),modelFormulaFixed,randomEffects=randomEffects,randomEffectsNames=randomEffectsNames,parameterNumber=parameterNumber,indexEffect=indexEffect,labels=c("paper","alternative \nrandom effects structure"))
#create plot
learn3=ggplot(learn_3,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0,linetype="dashed", size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ylim(0,0.26)+
  ggtitle("Word Learning Slope: Experiment 3")
learn3


# first align the top-row plot (plot.iris) with the left-most plot of the
# bottom row (plot.mpg)
plots <- align_plots(learn1, learn3, align = 'v', axis = 'l')
# then build the bottom row
top_row <- plot_grid(plots[[1]],learn2, align = 'h', labels=c("A","B"), rel_widths = c(1, 1.3))
# then combine with the top row for final plot
p=plot_grid(top_row,plots[[2]], ncol = 1, labels=c("","C"),rel_heights = c(1, 1.2))
p

####analyze object-object association memory####

####Experiment 1 Object-object association memory####

#a. The models from the paper.
##Main model - maximal random effects structure.
# object-object association memory model (by-item effect grouped by paired objects)
mMemory_exp1=glmer(isRight~offset(logit(offset.33))+(1|ResponseID)+(1|pairKind),data=subset(exp1D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp1)

#Fixed effects structure (constant)
modelFormulaFixed="isRight~offset(logit(offset.33))+"
#All possible random-effects structures, including lower-order
randomEffects=c("(1|ResponseID)+(1|pairKind)","(1|pairKind)","(1|ResponseID)","(1|ResponseID)+(1|exemplar)","(1|exemplar)")
randomEffectsNames=c("(1|participant)+\n(1|paired item)","(1|paired item)","(1|participant)","(1|participant)+\n(1|item)","(1|item)")
parameterNumber=c(2,1,1,2,1)
indexEffect=1
#create a data frame for different effects
memory_1=exploreRandomEffects(data=subset(exp1D, trialKind=="test"),modelFormulaFixed,randomEffects=randomEffects,randomEffectsNames=randomEffectsNames,parameterNumbers=parameterNumber,indexEffect=indexEffect,labels=c("paper","alternative \nrandom effects structure"))
#create plot
mem1=ggplot(memory_1,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0,linetype="dashed", size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ylim(-0.5,0.9)+
  ggtitle("Experiment 1 Intercept")
mem1

####Experiment 2 Object-object association memory####

#a. The models from the paper.
##Main model - maximal random effects structure.
# object-object association memory model (by-item effect grouped by paired objects)
mMemory_exp2=glmer(isRight~offset(logit(offset.33))+(1|ResponseID)+(1|pairKind),data=subset(exp2D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp2)

#Fixed effects structure (constant)
modelFormulaFixed="isRight~offset(logit(offset.33))+"
#All possible random-effects structures, including lower-order
randomEffects=c("(1|ResponseID)+(1|pairKind)","(1|pairKind)","(1|ResponseID)","(1|ResponseID)+(1|exemplar)","(1|exemplar)")
randomEffectsNames=c("(1|participant)+\n(1|paired item)","(1|paired item)","(1|participant)","(1|participant)+\n(1|item)","(1|item)")
parameterNumber=c(2,1,1,2,1)
indexEffect=1
#create a data frame for different effects
memory_2=exploreRandomEffects(data=subset(exp2D, trialKind=="test"),modelFormulaFixed,randomEffects=randomEffects,randomEffectsNames=randomEffectsNames,parameterNumbers=parameterNumber,indexEffect=indexEffect,labels=c("paper","alternative \nrandom effects structure"))
#create plot
mem2=ggplot(memory_2,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0,linetype="dashed", size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ylim(-0.5,0.9)+
  ggtitle("Experiment 2 Intercept")
mem2

####Comparing Experiment 1 & 2 Object-object association memory####
##Main model - maximal random effects structure.
mMemory_exp12=glmer(isRight~offset(logit(offset.33))+distributionC+(1|ResponseID)+(1+distributionC|pairKind),data=subset(d, trialKind=="test"&(expName=="exp1"|expName=="exp2")),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp12)
confint(mMemory_exp12,method="Wald")

#Fixed effects structure (constant)
modelFormulaFixed="isRight~offset(logit(offset.33))+distributionC+"
#All possible random-effects structures, including lower-order
randomEffects=c("(1|ResponseID)+(1+distributionC|pairKind)","(1|pairKind)","(1|ResponseID)","(1|ResponseID)+(1|pairKind)","(1|ResponseID)+(1+distributionC|exemplar)","(1|exemplar)","(1|ResponseID)+(1|exemplar)")
randomEffectsNames=c("(1|participant)+\n(1+distribution\n|paired item)","(1|paired item)","(1|participant)","(1|participant)+\n(1|paired item)","(1|participant)+\n(1+distribution\n|item)","(1|item)","(1|participant)+\n(1|item)")
parameterNumber=c(4,1,1,2,4,1,2)
indexEffect=2
#create a data frame for different effects
memory_12=exploreRandomEffects(data=subset(d, trialKind=="test"&(expName=="exp1"|expName=="exp2")),modelFormulaFixed,randomEffects=randomEffects,randomEffectsNames=randomEffectsNames,parameterNumbers=parameterNumber,indexEffect=indexEffect,labels=c("paper","alternative \nrandom effects structure"))
#create plot
mem12=ggplot(memory_12,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0,linetype="dashed", size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ylim(-0.5,0.9)+
  ggtitle("Experiment 1 vs. 2 Difference")
mem12

####Experiment 3 Skewed Condition Object-object association memory####
##Main model - maximal random effects structure.
exp3D$distributionSkewed = ifelse(exp3D$distributionC==-0.5,-1,
                                  ifelse(exp3D$distributionC==0.5,0,NA))
mMemory_exp3Skewed=glmer(isRight~offset(logit(offset.33))+distributionSkewed+(1|ResponseID)+(1+distributionSkewed|pairKind),data=subset(exp3D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp3Skewed)
confint(mMemory_exp3Skewed,method="Wald")

#Fixed effects structure (constant)
modelFormulaFixed="isRight~offset(logit(offset.33))+distributionSkewed+"
#All possible random-effects structures, including lower-order
#Model failed to converge for andom effects structure "(1|ResponseID)+(1+distributionSkewed|exemplar)" 
randomEffects=c("(1|ResponseID)+(1+distributionSkewed|pairKind)","(1|pairKind)","(1|ResponseID)","(1|ResponseID)+(1|pairKind)","(1|ResponseID)+(1+distributionSkewed|exemplar)","(1|exemplar)","(1|ResponseID)+(1|exemplar)")
randomEffectsNames=c("(1|participant)+\n(1+distribution\n|paired item)","(1|paired item)","(1|participant)","(1|participant)+\n(1|paired item)","(1|participant)+\n(1+distribution\n|item)","(1|item)","(1|participant)+\n(1|item)")
parameterNumber=c(4,1,1,2,4,1,2)
indexEffect=1
#create a data frame for different effects
memory_3_skewed=exploreRandomEffects(data=subset(exp3D, trialKind=="test"),modelFormulaFixed,randomEffects=randomEffects,randomEffectsNames=randomEffectsNames,parameterNumbers=parameterNumber,indexEffect=indexEffect,labels=c("paper","alternative \nrandom effects structure"))
#create plot
mem3_skewed=ggplot(memory_3_skewed,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0,linetype="dashed", size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ylim(-0.5,0.9)+
  ggtitle("Experiment 3 Skewed Condition")
mem3_skewed

####Experiment 3 Uniform Condition Object-object association memory####
##Main model - maximal random effects structure.
exp3D$distributionUniform = ifelse(exp3D$distributionC==-0.5,0,
                                   ifelse(exp3D$distributionC==0.5,1,NA))
mMemory_exp3Uniform=glmer(isRight~offset(logit(offset.33))+distributionUniform+(1|ResponseID)+(1+distributionUniform|pairKind),data=subset(exp3D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp3Uniform)
confint(mMemory_exp3Uniform,method="Wald")

#Fixed effects structure (constant)
modelFormulaFixed="isRight~offset(logit(offset.33))+distributionUniform+"
#All possible random-effects structures, including lower-order
randomEffects=c("(1|ResponseID)+(1+distributionUniform|pairKind)","(1|pairKind)","(1|ResponseID)","(1|ResponseID)+(1|pairKind)","(1|ResponseID)+(1+distributionUniform|exemplar)","(1|exemplar)","(1|ResponseID)+(1|exemplar)")
randomEffectsNames=c("(1|participant)+\n(1+distribution\n|paired item)","(1|paired item)","(1|participant)","(1|participant)+\n(1|paired item)","(1|participant)+\n(1+distribution\n|item)","(1|item)","(1|participant)+\n(1|item)")
parameterNumber=c(4,1,1,2,4,1,2)
indexEffect=1
#create a data frame for different effects
memory_3_uniform=exploreRandomEffects(data=subset(exp3D, trialKind=="test"),modelFormulaFixed,randomEffects=randomEffects,randomEffectsNames=randomEffectsNames,parameterNumbers=parameterNumber,indexEffect=indexEffect,labels=c("paper","alternative \nrandom effects structure"))
#create plot
mem3_uniform=ggplot(memory_3_uniform,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0,linetype="dashed", size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ylim(-0.5,0.9)+
  ggtitle("Experiment 3 Uniform Condition")
mem3_uniform

####Experiment 3 Condition Difference Object-object association memory####
##Main model - maximal random effects structure.
mMemory_exp3=glmer(isRight~offset(logit(offset.33))+distributionC+(1|ResponseID)+(1+distributionC|pairKind),data=subset(exp3D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp3)
confint(mMemory_exp3,method="Wald")

#Fixed effects structure (constant)
modelFormulaFixed="isRight~offset(logit(offset.33))+distributionC+"
#All possible random-effects structures, including lower-order
randomEffects=c("(1|ResponseID)+(1+distributionC|pairKind)","(1|pairKind)","(1|ResponseID)","(1|ResponseID)+(1|pairKind)","(1|ResponseID)+(1+distributionC|exemplar)","(1|exemplar)","(1|ResponseID)+(1|exemplar)")
randomEffectsNames=c("(1|participant)+\n(1+distribution\n|paired item)","(1|paired item)","(1|participant)","(1|participant)+\n(1|paired item)","(1|participant)+\n(1+distribution\n|item)","(1|item)","(1|participant)+\n(1|item)")
parameterNumber=c(4,1,1,2,4,1,2)
indexEffect=2
#create a data frame for different effects
memory_3=exploreRandomEffects(data=subset(exp3D, trialKind=="test"),modelFormulaFixed,randomEffects=randomEffects,randomEffectsNames=randomEffectsNames,parameterNumbers=parameterNumber,indexEffect=indexEffect,labels=c("paper","alternative \nrandom effects structure"))
#create plot
mem3=ggplot(memory_3,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0,linetype="dashed", size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ylim(-0.5,0.9)+
  ggtitle("Experiment 3 Condition Difference")
mem3

mem=plot_grid(mem1,mem2,mem12,mem3_skewed,mem3_uniform,mem3,ncol=3, labels=c("A","B","C","D","E","F"))
quartz()
mem

####analyze object-object memory vs. word learning tradeoff####

#### Experiment 1 Object-object memory vs. word learning tradeoff####
#a. The models from the paper.
##Main model - maximal random effects structure.
mTradeoff_exp1=glmer(isRight~offset(logit(offset.25))+memAccExemplarTrialC+(1+memAccExemplarTrialC|ResponseID)+(1+memAccExemplarTrialC|targetRole), data=subset(exp1D,trialKind=="training"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mTradeoff_exp1)

modelFormulaFixed="isRight~offset(logit(offset.25))+memAccExemplarTrialC+"
randomEffects=c("(1+memAccExemplarTrialC|ResponseID)+(1+memAccExemplarTrialC|targetRole)","(1|ResponseID)","(1+memAccExemplarTrialC|ResponseID)","(1|targetRole)","(1+memAccExemplarTrialC|targetRole)","(1|ResponseID)+(1|targetRole)","(1+memAccExemplarTrialC|ResponseID)+(1|targetRole)","(1|ResponseID)+(1+memAccExemplarTrialC|targetRole)")
randomEffectNames=c("(1+memoryAccuracy|participant)+\n(1+memoryAccuracy|item)","(1|participant)","(1+memoryAccuracy|participant)","(1|item)","(1+trial|item)","(1|participant)+\n(1|item)","(1+memoryAccuracy|participant)+\n(1|item)","(1|participant)+\n(1+memoryAccuracy|item)")
parameterNumber=c(6,1,3,1,3,2,4,4)
indexEffect=2
tradeoff_1=exploreRandomEffects(subset(exp1D,trialKind=="training"),modelFormulaFixed,randomEffects,randomEffectNames,parameterNumber, indexEffect,labels=c("paper","alternative \nrandom effects structure"))
tradeoff1=ggplot(tradeoff_1,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  ylim(-0.65,0.65)+
  geom_hline(yintercept=0, linetype="dashed",size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ggtitle("Experiment 1")
tradeoff1

#### Experiment 2 Object-object memory vs. word learning tradeoff####
#a. The models from the paper.
##Main model - maximal random effects structure.
mTradeoff_exp2=glmer(isRight~offset(logit(offset.25))+memAccExemplarTrialC+(1+memAccExemplarTrialC|ResponseID)+(1+memAccExemplarTrialC|targetRole), data=subset(exp2D,trialKind=="training"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mTradeoff_exp2)
confint(mTradeoff_exp2,method="Wald")

modelFormulaFixed="isRight~offset(logit(offset.25))+memAccExemplarTrialC+"
randomEffects=c("(1+memAccExemplarTrialC|ResponseID)+(1+memAccExemplarTrialC|targetRole)","(1|ResponseID)","(1+memAccExemplarTrialC|ResponseID)","(1|targetRole)","(1+memAccExemplarTrialC|targetRole)","(1|ResponseID)+(1|targetRole)","(1+memAccExemplarTrialC|ResponseID)+(1|targetRole)","(1|ResponseID)+(1+memAccExemplarTrialC|targetRole)")
randomEffectNames=c("(1+memoryAccuracy|participant)+\n(1+memoryAccuracy|item)","(1|participant)","(1+memoryAccuracy|participant)","(1|item)","(1+trial|item)","(1|participant)+\n(1|item)","(1+memoryAccuracy|participant)+\n(1|item)","(1|participant)+\n(1+memoryAccuracy|item)")
parameterNumber=c(6,1,3,1,3,2,4,4)
indexEffect=2
tradeoff_2=exploreRandomEffects(subset(exp2D,trialKind=="training"),modelFormulaFixed,randomEffects,randomEffectNames,parameterNumber, indexEffect,labels=c("paper","alternative \nrandom effects structure"))
tradeoff2=ggplot(tradeoff_2,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0, linetype="dashed",size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ggtitle("Experiment 2")+
  ylim(-0.65,0.65)
tradeoff2

#### Experiment 1 vs.2 Object-object memory vs. word learning tradeoff####
#compare Exp 1 & Exp 2
mTradeoff_exp12=glmer(isRight~offset(logit(offset.25))+distributionC*memAccExemplarTrialC+(1+memAccExemplarTrialC|ResponseID)+(1+memAccExemplarTrialC*distributionC|targetRole), data=subset(d, trialKind=="training"&(expName=="exp1"|expName=="exp2")),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mTradeoff_exp12)
confint(mTradeoff_exp12,method="Wald")

modelFormulaFixed="isRight~offset(logit(offset.25))+distributionC*memAccExemplarTrialC+"
randomEffects=c("(1+memAccExemplarTrialC|ResponseID)+(1+distributionC*memAccExemplarTrialC|targetRole)",
                "(1|ResponseID)",
                "(1+memAccExemplarTrialC|ResponseID)",
                "(1|targetRole)",
                "(1+memAccExemplarTrialC|targetRole)",
                "(1+distributionC|targetRole)",
                "(1+distributionC*memAccExemplarTrialC|targetRole)",
                "(1|ResponseID)+(1|targetRole)",
                "(1|ResponseID)+(1+memAccExemplarTrialC|targetRole)",
                "(1|ResponseID)+(1+distributionC|targetRole)",
                "(1|ResponseID)+(1+distributionC*memAccExemplarTrialC|targetRole)",
                "(1+memAccExemplarTrialC|ResponseID)+(1|targetRole)",
                "(1+memAccExemplarTrialC|ResponseID)+(1+memAccExemplarTrialC|targetRole)",
                "(1+memAccExemplarTrialC|ResponseID)+(1+distributionC|targetRole)")
randomEffectNames=c("(1+memoryAccuracy|participant)+\n(1+experiment*memoryAccuracy|item)",
                    "(1|participant)",
                    "(1+memoryAccuracy|participant)",
                    "(1|item)",
                    "(1+trial|item)",
                    "(1+experiment|item)",
                    "(1+experiment*trial|item)",
                    "(1|participant)+\n(1|item)",
                    "(1|participant)+\n(1+memoryAccuracy|item)",
                    "(1|participant)+\n(1+experiment|item)",
                    "(1|participant)+\n(1+memoryAccuracy*experiment|item)",
                    "(1+memoryAccuracy|participant)+\n(1|item)",
                    "(1+memoryAccuracy|participant)+\n(1+memoryAccuracy|item)",
                    "(1+memoryAccuracy|participant)+\n(1+experiment|item)")
parameterNumber=c(13,1,3,1,3,3,10,2,4,4,11,4,6,6)
indexEffect=4
tradeoff_12=exploreRandomEffects(subset(d,trialKind=="training"&(expName=="exp1"|expName=="exp2")),modelFormulaFixed,randomEffects,randomEffectNames,parameterNumber, indexEffect,labels=c("paper","alternative \nrandom effects structure"))
tradeoff12=ggplot(tradeoff_12,aes(randomEffectName,beta,fill=type,label=signifLabel))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=lowerCI,ymax=upperCI),width=0.2)+
  geom_label()+
  ylab("Parameter Estimate")+
  xlab("Random Effects Structure")+
  theme_bw()+
  geom_hline(yintercept=0, linetype="dashed",size=1.2)+
  theme(axis.text.x  = element_text(angle=90,hjust=1, vjust=0.5,size=12), legend.position="none")+
  ggtitle("Experiment 1 vs. 2 Interaction")
tradeoff12


p=plot_grid(tradeoff1,tradeoff2,tradeoff12,labels=c("A","B","C"),ncol=3)
quartz()
p


