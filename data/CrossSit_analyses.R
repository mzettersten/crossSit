# CrossSit Analyses
# Analyses for "The company objects keep: Linking referents together during cross-situational word learning"
# Martin Zettersten, Erica Wojcik, Viridiana Benitez, & Jenny Saffran (submitted)

# This is a walkthrough of all analyses reported in the paper.

#Load R packages/ custom scripts
source("summarizeData.R") #script for summarizing data, courtesy of "R cookbook" (http://www.cookbook-r.com/)
library(utils)
library(plyr)
library(ggplot2) #for plotting
library(lme4)
library(stats)
library(cowplot)
library(AICcmodavg)
library(reshape2)

#Read in data
d = read.table("CrossSit.txt",sep="\t", header=TRUE)

#####EXPERIMENT 1#####

#subset for experiment 1 data
exp1D = subset(d, expName=="exp1")


#get general age, gender distribution info
exp1_subjectInfo=ddply(exp1D,.(ResponseID,expName),summarize,Age=Age[1],Gender=Gender[1], dur=ExperimentDuration[1],nativeLangEng=NativeLangEng[1],nativeLang=NativeLang[1])
table(exp1_subjectInfo$Gender)
mean(exp1_subjectInfo$Age)
range(exp1_subjectInfo$Age)
mean(exp1_subjectInfo$dur)
sd(exp1_subjectInfo$dur)
table(exp1_subjectInfo$nativeLangEng)

##analyze word learning##

#accuracy by subject and overall
exp1_subjAccuracy=ddply(exp1D,.(ResponseID,trialKind,expName),summarize, accuracy=mean(isRight))
wordLearningOverall_exp1=summarySE(subset(exp1_subjAccuracy,trialKind=="training"),measurevar="accuracy")
wordLearningOverall_exp1

#Word Learning Model
mLearn_exp1=glmer(isRight~offset(logit(offset.25))+orderByTargetRoleC+(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC|targetRole),data=subset(exp1D, trialKind=="training"),family=binomial, control=glmerControl(optimizer="bobyqa"))
summary(mLearn_exp1)
#compute Wald 95% CIs
confint(mLearn_exp1,method="Wald")

##analyze object-object association memory##

#overall accuracy
memoryOverall_exp1=summarySE(subset(exp1_subjAccuracy,trialKind=="test"),measurevar="accuracy")
memoryOverall_exp1

# object-object association memory model (by-item effect grouped by paired objects)
mMemory_exp1=glmer(isRight~offset(logit(offset.33))+(1|ResponseID)+(1|pairKind),data=subset(exp1D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp1)
confint(mMemory_exp1,method="Wald")
#Check: identical results when fitting the model with by-item effects grouped by individual objects rather than pairs
# object-object association memory model (by-item effect grouped by individual items)
mMemory_exp1=glmer(isRight~offset(logit(offset.33))+(1|ResponseID)+(1|exemplar),data=subset(exp1D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp1)
confint(mMemory_exp1,method="Wald")

#Test moderate vs. high co-occurrence pairs (by-item effect grouped by paired objects)
exp1D$cooccurPairTypeC = varRecode(as.character(exp1D$cooccurPairType),c("moderate","high"),c(-0.5,0.5))
mMemoryPairs_exp1=glmer(isRight~offset(logit(offset.33))+cooccurPairTypeC+(1+cooccurPairTypeC|ResponseID)+(1|pairKind),data=subset(exp1D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemoryPairs_exp1)
#Check: identical results when fitting the model with by-item effects grouped by individual objects rather than pairs
#Test weak vs. strong pairs (by-item effect grouped by paired objects)
mMemoryPairs_exp1=glmer(isRight~offset(logit(offset.33))+cooccurPairTypeC+(1+cooccurPairTypeC|ResponseID)+(1|exemplar),data=subset(exp1D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemoryPairs_exp1)

##analyze object-object memory vs. word learning tradeoff##

mTradeoff_exp1=glmer(isRight~offset(logit(offset.25))+memAccExemplarTrialC+(1+memAccExemplarTrialC|ResponseID)+(1+memAccExemplarTrialC|targetRole), data=subset(exp1D,trialKind=="training"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mTradeoff_exp1)
confint(mTradeoff_exp1,method="Wald")

#####EXPERIMENT 2#####

#subset the data for experiment 2
exp2D = subset(d, expName=="exp2")

#get general age, gender distribution info
exp2_subjectInfo=ddply(exp2D,.(ResponseID,expName),summarize,Age=Age[1],Gender=Gender[1], dur=ExperimentDuration[1],nativeLang=NativeLang[1],nativeLangEng=NativeLangEng[1])
table(exp2_subjectInfo$Gender)
mean(exp2_subjectInfo$Age)
range(exp2_subjectInfo$Age)
mean(exp2_subjectInfo$dur)
sd(exp2_subjectInfo$dur)
table(exp2_subjectInfo$nativeLangEng)

##analyze word learning##

#overall accuracy
exp2_subjAccuracy=ddply(exp2D,.(ResponseID,trialKind,expName),summarize, accuracy=mean(isRight))
wordLearningOverall_exp2=summarySE(subset(exp2_subjAccuracy,trialKind=="training"),measurevar="accuracy")
wordLearningOverall_exp2

#Word Learning Model
mLearn_exp2=glmer(isRight~offset(logit(offset.25))+orderByTargetRoleC+(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC|targetRole),data=subset(exp2D, trialKind=="training"),family=binomial, control=glmerControl(optimizer="bobyqa"))
summary(mLearn_exp2)
#compute Wald 95% CIs
confint(mLearn_exp2,method="Wald")

#Compare Experiment 1 & 2
mLearn_exp12=glmer(isRight~offset(logit(offset.25))+distributionC*orderByTargetRoleC+(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC:distributionC|targetRole),data=subset(d, trialKind=="training"&(expName=="exp1"|expName=="exp2")),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mLearn_exp12)
confint(mLearn_exp12,method="Wald")
#note: the model including full by-item random effects structure did not converge, so we fit the model including only the random slope for the interaction, following Barr (2013)

##analyze object-object association memory##

#overall accuracy
memoryOverall_exp2=summarySE(subset(exp2_subjAccuracy,trialKind=="test"),measurevar="accuracy")
memoryOverall_exp2
# object-object association memory model
mMemory_exp2=glmer(isRight~offset(logit(offset.33))+(1|ResponseID)+(1|pairKind),data=subset(exp2D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp2)
confint(mMemory_exp2,method="Wald")
#Compare Experiment 1 & 2
mMemory_exp12=glmer(isRight~offset(logit(offset.33))+distributionC+(1|ResponseID)+(1+distributionC|pairKind),data=subset(d, trialKind=="test"&(expName=="exp1"|expName=="exp2")),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp12)
confint(mMemory_exp12,method="Wald")

#Check: similar results when fitting the model with by-item effects grouped by individual objects rather than pairs
# object-object association memory model
mMemory_exp2=glmer(isRight~offset(logit(offset.33))+(1|ResponseID)+(1|exemplar),data=subset(exp2D, trialKind=="test"),family=binomial,glmerControl(optimizer="bobyqa"))
summary(mMemory_exp2)
confint(mMemory_exp2,method="Wald")
#Compare Experiment 1 & 2
mMemory_exp12=glmer(isRight~offset(logit(offset.33))+distributionC+(1|ResponseID)+(1+distributionC|exemplar),data=subset(d, trialKind=="test"&(expName=="exp1"|expName=="exp2")),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp12) #p - value marginal, p = .0543
confint(mMemory_exp12,method="Wald")

##analyze object-object memory vs. word learning tradeoff##
#Experiment 2 alone
mTradeoff_exp2=glmer(isRight~offset(logit(offset.25))+memAccExemplarTrialC+(1+memAccExemplarTrialC|ResponseID)+(1+memAccExemplarTrialC|targetRole), data=subset(exp2D,trialKind=="training"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mTradeoff_exp2)
confint(mTradeoff_exp2,method="Wald")

#compare Exp 1 & Exp 2
mTradeoff_exp12=glmer(isRight~offset(logit(offset.25))+distributionC*memAccExemplarTrialC+(1+memAccExemplarTrialC|ResponseID)+(1+memAccExemplarTrialC*distributionC|targetRole), data=subset(d, trialKind=="training"&(expName=="exp1"|expName=="exp2")),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mTradeoff_exp12)
confint(mTradeoff_exp12,method="Wald")

#####EXPERIMENT 3#####

#subset the data for experiment 3
exp3D = subset(d, expName=="exp3_uniform"|expName=="exp3_skewed")

#get general age, gender distribution info
exp3_subjectInfo=ddply(exp3D,.(ResponseID,expName),summarize,Age=Age[1],Gender=Gender[1], dur=ExperimentDuration[1],nativeLang=NativeLang[1],nativeLangEng=NativeLangEng[1])
table(exp3_subjectInfo$Gender)
mean(exp3_subjectInfo$Age)
range(exp3_subjectInfo$Age)
mean(exp3_subjectInfo$dur)
sd(exp3_subjectInfo$dur)
table(exp3_subjectInfo$expName)
table(exp3_subjectInfo$nativeLangEng)

##analyze word learning##

#overall accuracy
exp3_subjAccuracy=ddply(exp3D,.(ResponseID,trialKind,expName),summarize, accuracy=mean(isRight))
wordLearningOverall_exp3=summarySE(subset(exp3_subjAccuracy,trialKind=="training"),measurevar="accuracy")
wordLearningCond_exp3=summarySE(subset(exp3_subjAccuracy,trialKind=="training"),measurevar="accuracy", groupvars=c("expName"))
wordLearningOverall_exp3
wordLearningCond_exp3

#Word Learning Model
mLearn_exp3=glmer(isRight~offset(logit(offset.25))+distributionC*orderByTargetRoleC+(1+orderByTargetRoleC|ResponseID)+(1+orderByTargetRoleC*distributionC|targetRole),data=subset(exp3D, trialKind=="training"),family=binomial, control=glmerControl(optimizer="bobyqa"))
summary(mLearn_exp3)
#compute Wald 95% CIs
confint(mLearn_exp3,method="Wald")[14:17,]


##analyze object-object association memory##

#overall accuracy
memoryOverall_exp3=summarySE(subset(exp3_subjAccuracy,trialKind=="test"),measurevar="accuracy")
memoryOverall_exp3
memoryCond_exp3=summarySE(subset(exp3_subjAccuracy,trialKind=="test"),measurevar="accuracy", groupvars=c("expName"))
memoryCond_exp3

# object-object association memory model
mMemory_exp3=glmer(isRight~offset(logit(offset.33))+distributionC+(1|ResponseID)+(1+distributionC|pairKind),data=subset(exp3D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp3)
confint(mMemory_exp3,method="Wald")

#recode to test skewed condition against chance
exp3D$distributionSkewed = varRecode(exp3D$distributionC,c(-0.5,0.5),c(-1,0))
mMemory_exp3Skewed=glmer(isRight~offset(logit(offset.33))+distributionSkewed+(1|ResponseID)+(1+distributionSkewed|pairKind),data=subset(exp3D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp3Skewed)
confint(mMemory_exp3Skewed,method="Wald")
#recode to test skewed condition against chance
exp3D$distributionUniform = varRecode(exp3D$distributionC,c(-0.5,0.5),c(0,1))
mMemory_exp3Uniform=glmer(isRight~offset(logit(offset.33))+distributionUniform+(1|ResponseID)+(1+distributionUniform|pairKind),data=subset(exp3D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp3Uniform)
confint(mMemory_exp3Uniform,method="Wald")

#Check: identical results when fitting the model with by-item effects grouped by individual objects rather than pairs
mMemory_exp3=glmer(isRight~offset(logit(offset.33))+distributionC+(1|ResponseID)+(1+distributionC|exemplar),data=subset(exp3D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp3)
confint(mMemory_exp3,method="Wald")

#recode to test skewed condition against chance
exp3D$distributionSkewed = varRecode(exp3D$distributionC,c(-0.5,0.5),c(-1,0))
mMemory_exp3Skewed=glmer(isRight~offset(logit(offset.33))+distributionSkewed+(1|ResponseID)+(1+distributionSkewed|exemplar),data=subset(exp3D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp3Skewed)
confint(mMemory_exp3Skewed,method="Wald")
#recode to test skewed condition against chance
exp3D$distributionUniform = varRecode(exp3D$distributionC,c(-0.5,0.5),c(0,1))
mMemory_exp3Uniform=glmer(isRight~offset(logit(offset.33))+distributionUniform+(1|ResponseID)+(1+distributionUniform|exemplar),data=subset(exp3D, trialKind=="test"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mMemory_exp3Uniform)
confint(mMemory_exp3Uniform,method="Wald")

##analyze object-object memory vs. word learning tradeoff##
mTradeoff_exp3=glmer(isRight~offset(logit(offset.25))+distributionC*memAccExemplarTrialC+(1+memAccExemplarTrialC|ResponseID)+(1+distributionC*memAccExemplarTrialC|targetRole), data=subset(exp3D,trialKind=="training"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mTradeoff_exp3)
confint(mTradeoff_exp3,method="Wald")
#test skewed condition
mTradeoff_exp3Skewed=glmer(isRight~offset(logit(offset.25))+distributionSkewed*memAccExemplarTrialC+(1+memAccExemplarTrialC|ResponseID)+(1+distributionSkewed*memAccExemplarTrialC|targetRole), data=subset(exp3D, trialKind=="training"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mTradeoff_exp3Skewed)
confint(mTradeoff_exp3Skewed,method="Wald")
#test skewed condition
mTradeoff_exp3Uniform=glmer(isRight~offset(logit(offset.25))+distributionUniform*memAccExemplarTrialC+(1+memAccExemplarTrialC|ResponseID)+(1+distributionC*memAccExemplarTrialC|targetRole), data=subset(exp3D, trialKind=="training"),family=binomial,control=glmerControl(optimizer="bobyqa"))
summary(mTradeoff_exp3Uniform)
confint(mTradeoff_exp3Uniform,method="Wald")


#####Plots#####

#####Experiments 1, 2, & 3#####

###Word Learning Experiments 1, 2, & 3###

dLearningOrder1=summarySEwithin(data=subset(d, (expName=="exp1"|expName=="exp2")&trialKind=="training"), measurevar="isRight", betweenvars=c("expName"), withinvars=c("orderByTargetRole"),
                                idvar="ResponseID", na.rm=FALSE, conf.interval=.95, .drop=TRUE)

#graph
pd=position_dodge(.3)
#plot mean accuracy across subjects for first to sixth occurrence of a label
pLearn1 <- ggplot(data = subset(dLearningOrder1), aes(x = orderByTargetRole, y = isRight,color=expName,group=expName))
pLearn1=pLearn1+geom_line(position=pd, size=2)+
  geom_errorbar(width=.2, aes(ymin=isRight-se, ymax=isRight+se), position=pd, size=1, color="black")+
  geom_point(shape=21, size=3, fill="white", position=pd)+
  geom_hline(yintercept=.25, linetype="dashed", size=2)+
  ylim(0.2,0.6)+
  scale_color_brewer(name="Co-Occurrence Condition",    # Legend label, use darker colors
                     breaks=c("exp1", "exp2"),
                     labels=c("Skewed (Experiment 1)", "Uniform (Experiment 2)"),
                     palette="Set1")+
  theme_classic()+
  ggtitle("With Visual Context Cue")+
  xlab("Trial Number")+
  ylab("Mean Accuracy")+
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"))+  
  theme(axis.title.x = element_text(face="bold",size=20),
        axis.text.x  = element_text(size=16,face="bold",color="black"))+
  theme(axis.title.y = element_text(face="bold",size=20),
        axis.text.y  = element_text(size=18,face="bold",color="black"))+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(legend.text = element_text(size = 16, face = "bold"))+
  theme(legend.title = element_text(size = 16, face = "bold"),legend.position=c(0.4,0.85))+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text", label = "Chance performance", x = 5, y = 0.27, size = 5, color = "black")


#Word Learning Exp 2
dLearningOrder2=summarySEwithin(subset(d, (expName=="exp3_uniform"|expName=="exp3_skewed")&trialKind=="training"), measurevar="isRight", betweenvars=c("expName"), withinvars=c("orderByTargetRole"),
                                idvar="ResponseID", na.rm=FALSE, conf.interval=.95, .drop=TRUE)

#graph
pd=position_dodge(.3)
#plot mean accuracy across subjects for first to sixth occurrence of a label
pLearn2 <- ggplot(data = subset(dLearningOrder2), aes(x = orderByTargetRole, y = isRight,color=expName,group=expName))
pLearn2=pLearn2+geom_line(position=pd, size=2)+
  geom_errorbar(width=.2, aes(ymin=isRight-se, ymax=isRight+se), position=pd, size=1, color="black")+
  geom_point(shape=21, size=3, fill="white", position=pd)+
  geom_hline(yintercept=.25, linetype="dashed", size=2)+
  ylim(0.2,0.6)+
  scale_color_brewer(name="Co-Occurrence Condition",    # Legend label, use darker colors
                     breaks=c("exp3_skewed", "exp3_uniform"),
                     labels=c("Skewed (Experiment 3)", "Uniform (Experiment 3)"),
                     palette="Set1")+
  theme_classic()+
  ggtitle("Without Visual Context Cue")+
  xlab("Trial Number")+
  ylab("Mean Accuracy")+
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"))+  
  theme(axis.title.x = element_text(face="bold",size=20),
        axis.text.x  = element_text(size=16,face="bold",color="black"))+
  theme(axis.title.y = element_text(face="bold",size=20),
        axis.text.y  = element_text(size=18,face="bold",color="black"))+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(legend.text = element_text(size = 16, face = "bold"))+
  theme(legend.title = element_text(size = 16, face = "bold"),legend.position=c(0.4,0.85))+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text", label = "Chance performance", x = 5, y = 0.27, size = 5, color = "black")

#plot graphs together
#quartz(width=10,height=6) #only use with Mac
Learn_123 =  plot_grid(pLearn1,pLearn2, labels = c("A", "B"), label_size = 26, align = "h")
Learn_123

###Memory Exp 1, 2, & 3###

#total memory
subjMemory=ddply(subset(d,trialKind=="test"),.(expName,ResponseID),summarize, meanAcc=mean(isRight), sd=sd(isRight), se=sd(isRight)/sqrt(length(isRight)))
memory=summarySE(subjMemory, measurevar="meanAcc", groupvars=c("expName"))

#Memory Exp 1 & Exp 2
pMem1=ggplot(data = subset(memory, expName=="exp1"|expName=="exp2"), aes(x=expName,y = meanAcc, fill=expName)) +
  geom_bar(position=position_dodge(width=.9),stat="identity", color="black",size=1.2) + 
 # geom_jitter(data=subset(subjMemory, expName=="exp1"|expName=="exp2"), fill="grey", width=0.3, height=0) +
  geom_hline(yintercept=.33, linetype="dashed",size=1.2) +
  ylab("Mean accuracy") +
  #ylim(0,0.6)+
  theme_classic(base_size=18) +
  xlab("Co-Occurrence Condition\nExperiment 1      Experiment 2") +
  ggtitle("With Visual Context Cue") +
  theme(title = element_text(size=16, hjust=.65))+
  theme(axis.title.x=element_text(hjust=0.5)) +
  theme(axis.title.y=element_text(hjust=0.5)) +
  geom_errorbar(aes(ymin=meanAcc-se, ymax=meanAcc+se),
                width=.2,                 
                position=position_dodge(.9),size=1.2)+
  scale_fill_brewer(name="Co-Occurrence Condition",    # Legend label, use darker colors
                    breaks=c("exp1", "exp2"),
                    labels=c("Skewed", "Uniform"),
                    palette="Set1")+
  scale_x_discrete(breaks=c("exp1", "exp2"),
                   labels=c("Skewed", "Uniform"))+
  scale_y_continuous(breaks=seq(0, 6, 0.1),limits=c(0,0.6))+
  theme(legend.position="none")+
  theme(axis.line.x = element_line(color = "black",size=1.2), axis.line.y = element_line(color = "black",size=1.2))+  
  theme(axis.title.x = element_text(face="bold",size=18),
        axis.text.x  = element_text(size=18,face="bold",color="black"))+
  theme(axis.title.y = element_text(face="bold",size=18),
        axis.text.y  = element_text(size=18,face="bold",color="black"))+
  theme(plot.title = element_text(face="bold", size=24))+
  theme(legend.text = element_text(size = 16, face = "bold"))+
  theme(legend.title = element_text(size = 18, face = "bold"))+
  geom_path(aes(x=c(1,2),y=c(0.57,0.57),fill=NULL), size=1.2)+
  annotate("text",x=1.5,y=0.58, label="*",size=14)+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=1,y=0.51, label="***",size=14)
#Memory Experiment 3
pMem2=ggplot(data = subset(memory, expName=="exp3_skewed"|expName=="exp3_uniform"), aes(x=expName,y = meanAcc, fill=expName)) +
  geom_bar(position=position_dodge(width=.9),stat="identity", color="black",size=1.2) + 
  geom_hline(yintercept=.33, linetype="dashed",size=1.2) +
  ylab("Mean accuracy") +
  #ylim(0,0.6)+
  theme_classic(base_size=18) +
  xlab("Co-Occurrence Condition \nExperiment 3") +
  ggtitle("Without Visual Context Cue") +
  theme(title = element_text(size=16, hjust=.65))+
  theme(axis.title.x=element_text(hjust=0.5)) +
  theme(axis.title.y=element_text(hjust=0.5)) +
  geom_errorbar(aes(ymin=meanAcc-se, ymax=meanAcc+se),
                width=.2,                 
                position=position_dodge(.9),size=1.2)+
  scale_fill_brewer(name="Co-Occurrence Condition",    # Legend label, use darker colors
                    breaks=c("exp3_uniform","exp3_skewed"),
                    labels=c("Uniform","Skewed"),
                    palette="Set1")+
  scale_x_discrete(breaks=c("exp3_uniform","exp3_skewed"),
                   labels=c( "Uniform","Skewed"))+
  scale_y_continuous(breaks=seq(0, 6, 0.1),limits=c(0,0.6))+
  theme(legend.position="none")+
  theme(axis.line.x = element_line(color = "black",size=1.2), axis.line.y = element_line(color = "black",size=1.2))+  
  theme(axis.title.x = element_text(face="bold",size=18),
        axis.text.x  = element_text(size=18,face="bold",color="black"))+
  theme(axis.title.y = element_text(face="bold",size=18),
        axis.text.y  = element_text(size=18,face="bold",color="black"))+
  theme(plot.title = element_text(face="bold", size=24))+
  theme(legend.text = element_text(size = 16, face = "bold"))+
  theme(legend.title = element_text(size = 18, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_path(aes(x=c(1,2),y=c(0.57,0.57),fill=NULL), size=1.2)+
  annotate("text",x=1.5,y=0.58, label="*",size=14)+
  annotate("text",x=1,y=0.51, label="*",size=14)

#Memory for Experiments 1, 2, & 3
#with cowplot
#quartz(width=11,height=6) #only use with Mac
Mem_123 =  plot_grid(pMem1,pMem2, labels = c("A", "B"), label_size = 26, align = "h")
Mem_123

####Learning Memory Tradeoff Exp 1,2,3####

pX=expand.grid(memAccExemplarTrialC=c(-0.5,0.5),distributionC=c(-0.5,0.5),offset.25=c(0.25))
pX$expNameC=as.factor(pX$expNameC)
pX$ResponseID=NA
pX$TargetRole=NA
pY1 <- as.data.frame(predictSE.merMod(mTradeoff_exp12,newdata=pX)) #Predictions for Experiments 1 & 2
pY2 <- as.data.frame(predictSE.merMod(mTradeoff_exp3,newdata=pX)) #Predictions for Experiments 3
pX$fit1=pY1$fit
pX$se.fit1=pY1$se.fit
pX$fit2=pY2$fit
pX$se.fit2=pY2$se.fit
pX$expNameF = as.factor(varRecode(pX$distributionC, c(0.5,-0.5),c("Skewed","Uniform")))
#Mem Learning Tradeoff Exp 1 vs. Exp 2
pMemLearn1=ggplot(data = pX, aes(x=expNameF,y = fit1, fill=expNameF,alpha=as.factor(memAccExemplarTrialC))) +
  geom_bar(position=position_dodge(width=.9),stat="identity", color="black",size=1.2) + 
  geom_hline(yintercept=.25, linetype="dashed",size=1.2) +
  ylab("Word Learning Accuracy") +
  #ylim(0,0.6)+
  theme_classic(base_size=18) +
  xlab("Co-Occurrence Condition\nExperiment 1      Experiment 2") +
  ggtitle("With Visual Context Cue") +
  theme(title = element_text(size=16, hjust=.65))+
  theme(axis.title.x=element_text(hjust=0.5)) +
  theme(axis.title.y=element_text(hjust=0.5)) +
  geom_errorbar(aes(ymin=fit1-se.fit1, ymax=fit1+se.fit1,group=as.factor(memAccExemplarTrialC),alpha=NULL),
                width=.2,                 
                position=position_dodge(.9),size=1.2)+
  scale_fill_brewer(name="Co-Occurrence Condition",    # Legend label, use darker colors
                    palette="Set1")+
  scale_x_discrete()+
  scale_alpha_discrete(name="Memory Trial Accuracy",
                       breaks=c(-0.5,0.5),
                       labels=c("Incorrect", "Correct"))+
  scale_y_continuous(breaks=seq(0, 6, 0.1),limits=c(0,0.6))+
  theme(legend.position="none")+
  guides(fill=FALSE)+
  theme(axis.line.x = element_line(color = "black",size=1.2), axis.line.y = element_line(color = "black",size=1.2))+  
  theme(axis.title.x = element_text(face="bold",size=18),
        axis.text.x  = element_text(size=18,face="bold",color="black"))+
  theme(axis.title.y = element_text(face="bold",size=18),
        axis.text.y  = element_text(size=18,face="bold",color="black"))+
  theme(plot.title = element_text(face="bold", size=24))+
  theme(legend.text = element_text(size = 16, face = "bold"))+
  theme(legend.title = element_text(size = 18, face = "bold"))+
  geom_path(aes(x=c(0.77,0.77,1.23,1.23),y=c(0.5,0.5,0.5,0.5),fill=NULL,alpha=NULL), size=1.2)+
  theme(plot.title = element_text(hjust = 0.5))+
  #geom_path(aes(x=c(1.77,1.77,2.23,2.23),y=c(0.5,0.5,0.5,0.5),fill=NULL,alpha=NULL), size=1.2)+
  geom_path(aes(x=c(1,1,2,2),y=c(0.55,0.55,0.55,0.55),fill=NULL,alpha=NULL), size=1.2)+
  annotate("text",x=1,y=0.51, label="*",size=14)+
  #annotate("text",x=2,y=0.53, label="+",size=10)+
  annotate("text",x=1.5,y=0.56, label="**",size=14)

pMemLearn2=ggplot(data = pX, aes(x=expNameF,y = fit2, fill=expNameF,alpha=as.factor(memAccExemplarTrialC))) +
  geom_bar(position=position_dodge(width=.9),stat="identity", color="black",size=1.2) + 
  geom_hline(yintercept=.25, linetype="dashed",size=1.2) +
  ylab("Word Learning Accuracy") +
  #ylim(0,0.6)+
  theme_classic(base_size=18) +
  xlab("Co-Occurrence Condition\nExperiment 3") +
  ggtitle("Without Visual Context Cue") +
  theme(title = element_text(size=16, hjust=.65))+
  theme(axis.title.x=element_text(hjust=0.5)) +
  theme(axis.title.y=element_text(hjust=0.5)) +
  geom_errorbar(aes(ymin=fit2-se.fit2, ymax=fit2+se.fit2,group=as.factor(memAccExemplarTrialC),alpha=NULL),
                width=.2,                 
                position=position_dodge(.9),size=1.2,show.legend=FALSE)+
  scale_fill_brewer(name="Co-Occurrence Condition",    # Legend label, use darker colors
                    palette="Set1")+
  scale_x_discrete()+
  scale_alpha_discrete(name="Object-Object Association Accuracy",
                       breaks=c(-0.5,0.5),
                       labels=c("Incorrect", "Correct"))+
  scale_y_continuous(breaks=seq(0, 6, 0.1),limits=c(0,0.6))+
  theme(legend.position=c(0.3,0.85))+
  guides(fill=FALSE)+
  #theme(axis.line.x = element_line(color = "black",size=1.2), axis.line.y = element_line(color = "black",size=1.2))+  
  theme(axis.line.x = element_line(color = "black",size=1.2))+
  theme(axis.title.x = element_text(face="bold",size=18),
        axis.text.x  = element_text(size=18,face="bold",color="black"))+
  #theme(axis.title.y = element_text(face="bold",size=18),
  #      axis.text.y  = element_text(size=18,face="bold",color="black"))+
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())+
  theme(plot.title = element_text(face="bold", size=24))+
  theme(legend.text = element_text(size = 16, face = "bold"))+
  theme(legend.title = element_text(size = 18, face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y="")

#Memory Tradeoffs for Experiments 1,2,3
#with cowplot
quartz(width=11,height=6) #only use with Mac
MemLearn_123 =  plot_grid(pMemLearn1,pMemLearn2, labels = c("A", "B"), label_size = 26, align = "h")
MemLearn_123

#Figure 2: Distributions
#####Object-Object Co-Occurrence Conditions####
skewed=data.frame(Object=c("Object 1","Object 2", "Object 3", "Object 4", "Object 5", "Object 6", "Object 7", "Object 8"),
                  Object1=c(NA,18,9,9,9,9,9,9),
                  Object2=c(18,NA,9,9,9,9,9,9),
                  Object3=c(9,9,NA,18,9,9,9,9),
                  Object4=c(9,9,18,NA,9,9,9,9),
                  Object5=c(9,9,9,9,NA,14,11,11),
                  Object6=c(9,9,9,9,14,NA,11,11),
                  Object7=c(9,9,9,9,11,11,NA,14),
                  Object8=c(9,9,9,9,11,11,14,NA))
colnames(skewed)=c("Object","Object 1","Object 2", "Object 3", "Object 4", "Object 5", "Object 6", "Object 7", "Object 8")
skewed_long <- melt(skewed,
                    id.vars=c("Object"),
                    measure.vars=c("Object 1","Object 2", "Object 3", "Object 4", "Object 5", "Object 6", "Object 7", "Object 8"),
                    variable.name="variable",
                    value.name="measurement")
skewed_long$Object=factor(skewed_long$Object, levels=rev(levels(skewed_long$Object)))
skewed_long$condition="skewed"

uniform=data.frame(Object=c("Object 1","Object 2", "Object 3", "Object 4", "Object 5", "Object 6", "Object 7", "Object 8"),
                   Object1=c(NA,11,10,10,11,10,10,10),
                   Object2=c(11,NA,11,10,10,10,10,10),
                   Object3=c(10,11,NA,10,10,11,10,10),
                   Object4=c(10,10,10,NA,10,10,11,11),
                   Object5=c(11,10,10,10,NA,10,10,11),
                   Object6=c(10,10,11,10,10,NA,11,10),
                   Object7=c(10,10,10,11,10,11,NA,10),
                   Object8=c(10,10,10,11,11,10,10,NA))
colnames(uniform)=c("Object","Object 1","Object 2", "Object 3", "Object 4", "Object 5", "Object 6", "Object 7", "Object 8")
uniform_long <- melt(uniform,
                     id.vars=c("Object"),
                     measure.vars=c("Object 1","Object 2", "Object 3", "Object 4", "Object 5", "Object 6", "Object 7", "Object 8"),
                     variable.name="variable",
                     value.name="measurement")
uniform_long$Object=factor(uniform_long$Object, levels=rev(levels(uniform_long$Object)))
uniform_long$condition="uniform"

distributions=rbind(skewed_long, uniform_long)

p=ggplot(distributions, aes(variable, Object, label=measurement)) + 
  geom_tile(aes(fill = measurement),colour = "white") + 
  #scale_fill_gradient(na.value="white",low="#034e7b", high="#e31a1c")+
  scale_fill_distiller(palette="RdBu",na.value="white")+
  scale_x_discrete(name="") +
  scale_y_discrete(name="") +
  theme_classic()+
  theme(legend.position="none")+
  theme(axis.line.x = element_line(color = "black",size=1.2), axis.line.y = element_line(color = "black",size=1.2))+  
  theme(axis.text.x  = element_text(angle=90,vjust=0.5,size=16,face="bold"))+
  theme(axis.text.y  = element_text(size=16,face="bold"))+
  geom_text(fontface="bold",size=8)+
  facet_grid(~condition)+
  theme(strip.text.x = element_text(size=16, face="bold"),
        strip.background = element_rect(colour="black", size=3))

#quartz(width=10,height=6) #only use with Mac
p
