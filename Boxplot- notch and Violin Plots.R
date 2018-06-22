#--------Include Libraries-----------------------#
#-------------------------------------------------#

library(data.table)
library(dplyr) #to view glimpse of df
library(ggplot2)
library(ggbeeswarm)
#-----------------------Import Data Set--------------#
#----------------------------------------------------#
df<-read.delim("closerdata.tab",header=TRUE, sep="\t")
summary(df)
#get list of attributes
attributes(df)

#----------------------Subsetting the data set--------------------------#
df$ncdsid<-NULL
data<-df[c("n622","bmi50","N8SMOKIG","N8EXSMER", "nd8genh","nd8pain","nd8rlmp","nd8phhe","nd8rlme","nd8enfa","nd8socf","nd8csp14")]

#-------------------Remove missing values from dataset-----------------#

#remove NA from data
data<-data[complete.cases(data),]
#-------------Add labels to Categories of Data Set...............
#.......................................

#add labels to the int variables 

data$n622<-factor(data$n622,levels=c(1,2),labels=c("male","female"))
data$N8SMOKIG<-factor(data$N8SMOKIG,levels=c(1,2,3,4,-1),labels=c("never", "used to","occasionally","daily","n/a"))
data$N8EXSMER<-factor(data$N8EXSMER,levels=c(-1,1,2),labels=c("not applicable","Yes","No"))




data<-subset(data,data$N8SMOKIG!='n/a')
data$N8SMOKIG<-factor(data$N8SMOKIG)



#---------------Production of Boxplots

#------------Boxplot1: General Health of all members on the basis of smoking status-------------

p1<-ggplot(data, aes(x=N8SMOKIG, y=nd8genh, group = interaction(N8SMOKIG,N8EXSMER), fill=N8EXSMER)) 
p1+geom_boxplot()+labs(title="    Smoking vs. General Health\n                  (Age 50)",x ="Smoking Status", y = "General Health(Age50)", fill="Regular Smoker?")+ scale_fill_brewer(type="seq", palette=6, direction=1)
#  scale_fill_grey(start=1, end=0.7)
#ggtitle("        Effects of Smoking on General Health of  at Age 50" )

#-------------------------------Boxplot with Notches---------------
#--------------------------------------------------------------

p1<-ggplot(data, aes(x=N8SMOKIG, y=nd8genh, group = interaction(N8SMOKIG,N8EXSMER), fill=N8EXSMER)) 
p1+geom_boxplot(notch=T)+labs(title="    Smoking vs. General Health\n                  (Age 50)",x ="Smoking Status", y = "General Health(Age 50)", fill="Regular Smoker?")+scale_fill_brewer(type="seq", palette=6, direction=1)


#------------Production of Violin Plot

par(mfrow=c(2,1))
p <- ggplot(data, aes(x=N8SMOKIG, y=nd8genh,fill=N8EXSMER,group = interaction(N8SMOKIG,N8EXSMER))) + 
  geom_violin(trim=FALSE)+geom_boxplot(width=0.3, position=position_dodge(0.75),fill="white")
p + labs(title="Smoking vs General Health \n                  (Age 50)",x ="Smoking Status", y = "General Health(Age 50)", fill="Regular Smoker?")+scale_fill_brewer(type="seq", palette=6, direction=1)


p1<-ggplot(data, aes(x=N8SMOKIG, y=nd8genh, group = interaction(N8SMOKIG,N8EXSMER), fill=N8EXSMER)) 
p1+geom_boxplot(notch=T)+labs(title="    Smoking vs. General Health\n                  (Age 50)",x ="Smoking Status", y = "General Health(Age 50)", fill="Regular Smoker?")+scale_fill_brewer(type="seq", palette=6, direction=1)



#-------------------BeeSwarm Graph of General Health on Basis of Smoking Status
p3<-ggplot(data, aes(x=N8SMOKIG, y=nd8genh, color=factor(N8EXSMER)))+geom_quasirandom(dodge.width = 0.9)
p3+labs(title="    Smoking vs. General Health\n                  (Age 50)",x ="Smoking Status", y = "General Health(Age 50)",colour="Regular Smoker?")+scale_color_grey(start=1,end=0.1)

#----------------------------------------Male Data

mdata<-subset(data,data$n622=="male") 
mdata$n622<-factor(mdata$n622)
p <- ggplot(data, aes(x=N8SMOKIG, y=nd8genh,fill=interaction(n622,N8EXSMER),group = interaction(n622,N8SMOKIG,N8EXSMER))) + 
  geom_violin(trim=FALSE)
p+labs(title="Smoking vs General Health \n       (Male vs. Female)",x ="Smoking Status", y = "General Health(Age 50)",colour="Regular Smoker?")+scale_fill_brewer(type="seq",palette=4,direction=6)

p <- ggplot(data, aes(x=N8SMOKIG, y=nd8genh,fill=N8EXSMER ,group = interaction(N8SMOKIG,N8EXSMER))) + 
  geom_violin(trim=FALSE) +geom_boxplot(width=0.3, position=position_dodge(0.75),fill="white")

p+labs(title="Smoking vs General Health (Age 50)  ",x ="Smoking Status ", y = "General Health(Age 50) ", fill="Regular Smoker?")+scale_fill_brewer(type="seq", palette=6, direction=1)

#----------------------------------------Female Data

fdata<-subset(data,data$n622=="female") 
fdata$n622<-factor(fdata$n622)
p <- ggplot(fdata, aes(x=N8SMOKIG, y=nd8genh,fill=interaction(n622,N8EXSMER),group = interaction(n622,N8SMOKIG,N8EXSMER))) + 
  geom_violin(trim=FALSE)

p <- ggplot(fdata, aes(x=N8SMOKIG, y=nd8genh,fill=N8EXSMER ,group = interaction(N8SMOKIG,N8EXSMER))) + 
  geom_violin(trim=FALSE) +geom_boxplot(width=0.3, position=position_dodge(0.75),fill="white")
p+labs(title="Smoking vs General Health (Age 50) \n             (Female) ",x ="Smoking Status ", y = "General Health(Age 50) ", fill="Regular Smoker?")+scale_fill_brewer(type="seq", palette=6, direction=1)



