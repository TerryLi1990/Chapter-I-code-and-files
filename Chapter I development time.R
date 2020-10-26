setwd("D:/OneDrive/RStudio/file")
options(max.print=1000000)
gc()
#read input data and replace NA to 0
temp.name<-c()
for (i in 1:6){ 
  assign(paste0('data',i), read.csv(print(paste0('4b development time keep outlier rep',i,'.csv')),header = T)) ##data<-table.csv
  assign(paste0('data_na',i), get(paste0('data',i)))
  temp.name<-c(temp.name,paste0("data_na",i))
  temp<-get(temp.name[i])
  temp[is.na(temp)]<-0
  assign(paste0('data_na',i),temp)
}
#density ~ total #collected in a vial
total<-c()
temp.name.1<-c()
temp.name.2<-c()
for (i in 1:6){
  assign(paste0('density.sumdf',i),data.frame())
  assign(paste0('density.df',i),data.frame())
  temp.name.1<-c(temp.name.1,paste0('density.sumdf',i))
  temp<-get(temp.name.1[i])
    for (j in 1:ncol(get(paste0('data_na',i)))){
      for (k in seq(1,15,2)){
        for (r in 1: (nrow( get(paste0('data_na',i))  )/32) ){
      total<-sum(total, sum(get(paste0('data_na',i))[k+(r-1)*32,j],get(paste0('data_na',i)) [k+1+(r-1)*32,j],
                            get(paste0('data_na',i)) [k+16+(r-1)*32,j],get(paste0('data_na',i)) [k+17+(r-1)*32,j]) )
        }
        temp[k,j]<-total
        temp[k+1,j]<-total
        temp[k+16,j]<-total
        temp[k+17,j]<-total
        assign(paste0('density.df',i),temp)
        total<-c()
      }
    }
  temp.name.2<-c(temp.name.2,paste0('density.df',i))
  temp<-get(temp.name.2[i])
  temp.df<-data.frame()
    for (s in 1:(nrow(get(paste0('data_na',i)))/32)){
      temp.df<-rbind(temp.df,temp)
      }
  assign(paste0('density.df',i),temp.df)
}
#column of individual, line, vial, sex, site
for (i in 1:6){
  assign(paste0('individual',i), c(seq(1,sum(get(paste0('data',i)),na.rm=TRUE),1))) ##individual column, total #fly collected, from 1 to n, n=total flies collected
  assign(paste0('line',i),c()) #line column, count total number of each line then rep() with the line number
  assign(paste0('vial',i),c()) #vial column; line1 vial 1-8, line2 vial 10-17, line 3 vial 19-26, ... line 20 = (20-1)x9 +1 = vial 172-179, line 21 = 181-188; set  9, 18, 27, etc to 8, 17, etc
  assign(paste0('sex',i),c()) #sex column, 1=male, 2=female
  assign(paste0('site',i),c()) #site 0=vial; 1=cotton.
  assign(paste0('density',i),c()) #larval and pupal density, ~emerged or #collected in a vial include both male and female, vial and cotton group
  for (j in 1:ncol(get(paste0('data_na',i)))){ ##column line number
    assign(paste0('line',i), c(get(paste0('line',i)),rep(j,sum(get(paste0('data',i))[,j],na.rm = TRUE))) )
    for (k in 1: nrow(get(paste0('data_na',i)))){ ##rows
      assign(paste0('vial',i),c( get(paste0('vial',i)), rep(round ((j-1)*9 + (k %% 16)/2+0.2),get(paste0('data_na',i))[k,j]) )) #j<-1, k in 1:32, round() = 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 0 1 1 2 2 ...
                                                                                                                                #j<-2, k in 1:32, round() = 10 10 11 11 12 12... 16 16 17 9 10 10 
      assign(paste0('sex',i),c( get(paste0('sex',i)), rep(k %% 2,get(paste0('data_na',i))[k,j]))) 
      assign(paste0('site',i),c( get(paste0('site',i)), rep(round(k%%32/16 + 2.45),get(paste0('data_na',i))[k,j]) )) #1 to 16 = 3 (vial); 17 to 31 = 4 (cotton); 32 = 2 ; 33-48 = 1-16, etc. 
      assign(paste0('density',i),c( get(paste0('density',i)), rep(get(paste0('density.df',i))[k,j] ,get(paste0('data_na',i))[k,j]) ) ) 
    }
  }
}
##solve vial. when k=16,32,48,64,... vial = 0,0,9,9, so change 0 to 8 (line1 1-8), change 9 to 17 (line2 10-17) ... change 180 to 188 (line21 181-188)
temp.name<-c()
for (i in 1:6){
    temp.name<-c(temp.name,paste0('vial',i))
    temp<-get(temp.name[i])
    for (l in seq(180,0,-9)){ ## l = 180 171 162 153 144 135 126 117 108  99  90  81  72  63  54  45  36  27  18  9  0
    temp[temp==l]<-l+8
    }
    assign(paste0('vial',i),temp)
}
#sex column, 1=male, 2=female
temp.name<-c()
for (i in 1:6){
  temp.name<-c(temp.name,paste0('sex',i))
  temp<-get(temp.name[i])
  temp[temp==0]<-2
  assign(paste0('sex',i),temp)
}
#site column, 3 to 0 =vial, 2,4 to 1=cotton
temp.name<-c()
for (i in 1:6){
  temp.name<-c(temp.name,paste0('site',i))
  temp<-get(temp.name[i])
  temp[temp==3]<-0 
  temp[temp==2|temp==4]<-1 
  assign(paste0('site',i),temp)
}
#development time= emergence time (halfway time point between two collection time) MINUS egg lay time (halfway point between females in and out)
  #example for trial 1.20 females in at Feb 22 7pm 2017; 20 females removed at Feb 24 11am 2017; so egg lay time = Feb 23rd 3pm 2017.total table: 18. Estimated eclosing time: 1: March 5th 3pm; 2: 5th 6pm; 3: 6th 5am; 4: 6th 3pm; 5: 7th 5am; 6: 8th 3am; 7: 8th 5am; 8: 8th 5am; 9: 9th 1am; 10: 9th 3am; 11: 9th 3am; 12: 9th 4am; 13: 9th 11pm; 14: 10th 3am; 15: 10th 5am; 16: 11th 3am; 17: 12th 4am; 18: 13th 1am. See '3b development time experiment schedule' for detailed calculation.estimated eclosing time: 10days for the first collection. 1,240h; 2,243; 3,254; 4,264; 5,278; 6,300; 7,302; 8,302; 9,322; 10,324; 11,324; 12,325; 13,344; 14,348; 15,350; 16,372; 17,397; 18,418. 
time.df1<-data.frame(seq(1,32*18,32),c(240,243,254,264,278,300,302,302,322,324,324,325,344,348,350,372,397,418))
time.df2<-data.frame(seq(1,32*9,32),c(273,277,290,302,314,326,336,346,360))
time.df3<-data.frame(seq(1,32*4,32),c(278,296,342,394))
time.df4<-data.frame(seq(1,32*9,32),c(253,257,267,276,290,304,315,326,337))
time.df5<-data.frame(seq(1,32*9,32),c(205,215,228,238,249,256,267,288,322))
time.df6<-data.frame(seq(1,32*6,32),c(226,236,256,280,304,332))
for (i in 1:6){
  assign(paste0('time',i),c()) 
  for (j in 1:ncol(get(paste0('data_na',i)))){ #j is number of lines
    for (l in 1:nrow(get(paste0('time.df',i)))){ #l is collect time point
      for (k in get(paste0('time.df',i))[l,1]:(get(paste0('time.df',i))[l,1]+31)){ #l =1, k in 1:32; l=2, k in 33:64; etc.
        assign(paste0('time',i),c(get(paste0('time',i)),rep(get(paste0('time.df',i))[l,2],get(paste0('data_na',i))[k,j] )))
      }   
    }
  }  
}
#uniform line across trials#
#trial2
for (i in 20:3){line2[line2==i]<-i+1}
#trial3
for (i in 17:8){line3[line3==i]<-i+4}
for (i in 7:3){line3[line3==i]<-i+3}
line3[line3==2]<-4
line3[line3==1]<-2
#old version with strains removed due to low density;for (i in 15:7){line3[line3==i]<-i+5};for (i in 6:2){line3[line3==i]<-i+4};line3[line3==1]<-2
#trial4
for (i in 18:5){line4[line4==i]<-i+3}
for (i in 4:2){line4[line4==i]<-i+2}
#trial5
line5[line5==6]<-19;line5[line5==5]<-17;line5[line5==4]<-14;line5[line5==3]<-9;line5[line5==2]<-6;line5[line5==1]<-2
#trial6
line6[line6==5]<-19;line6[line6==4]<-14;line6[line6==3]<-9;line6[line6==2]<-6;line6[line6==1]<-2
#old version with strains removed due to low density;line6[line6==4]<-19;line6[line6==3]<-14;line6[line6==2]<-6;line6[line6==1]<-2
#new vial number for each trial
temp.name<-c()
for (i in 1:6){
  temp.name<-c(temp.name,paste0('vial',i))
  temp<-get(temp.name[i])
  temp<-temp+1000*i
  assign(paste0('vial',i),temp)
}
#change dummy variable (default category) to from strain 303 to strain 335 (line 1 (303) --> line 6, line 6 (335) --> line 1)
temp.name<-c()
for (i in 1:6){
  temp.name<-c(temp.name,paste0('line',i))
  temp<-get(temp.name[i])
  temp<-replace(temp,temp==1,100)
  temp<-replace(temp,temp==6,1)
  temp<-replace(temp,temp==100,6)
  assign(paste0('line',i),temp)
}

###############################################################################################################################################################
#6 trials
#individual data frame
for (i in 1:6){
  assign(paste0('individual.trial',i),c()) 
  assign(paste0('individual.trial',i), data.frame(get(paste0('individual',i)),rep(i,length(get(paste0('individual',i)))),get(paste0('line',i)),get(paste0('vial',i)),get(paste0('sex',i)),get(paste0('time',i)),get(paste0('site',i)), get(paste0('density',i))))
}
individual.trial<-rbind(individual.trial1,individual.trial2,individual.trial3,individual.trial4,individual.trial5,individual.trial6)
colnames(individual.trial)[1] <- "individual"
colnames(individual.trial)[2] <- "trial"
colnames(individual.trial)[3] <- "line"
colnames(individual.trial)[4] <- "vial"
colnames(individual.trial)[5] <- "sex" #1=male, 2=female
colnames(individual.trial)[6] <- "time"
colnames(individual.trial)[7] <- "site" #0=vial, 1=cotton
colnames(individual.trial)[8] <- "density" #emerged or #collected in a vial, include both cotton and vial group, male and female
individual.6trials<-individual.trial
#write.csv(individual.6trials, file ="individual.6trials.csv")
#setwd("D:/OneDrive/RStudio/file")
#individual.6trials<-read.csv("individual.6trials.csv",header = T)
#change variable to factor
individual.6trials$trial<-factor(individual.6trials$trial)
individual.6trials$line<-factor(individual.6trials$line)
individual.6trials$vial<-factor(individual.6trials$vial)
individual.6trials$sex<-factor(individual.6trials$sex)
#time 
library(lme4)
library(lmerTest)
options(max.print=1000000)
#with density model
gc()
time.6trials.densitymodel<-lmer(time~trial*line*sex*density+ (1|vial), data=individual.6trials,REML=T)
summary.time.6trials.densitymodel<-data.frame(summary(time.6trials.densitymodel)$coefficients)
write.csv(summary.time.6trials.densitymodel, file ="summary.time.6trials.densitymodel.csv")
#test random effect
time.6trials.densitymodel.norandom<-lm(time~trial*line*sex*density, data=individual.6trials)
anova(time.6trials.densitymodel,time.6trials.densitymodel.norandom)
#with density anova
gc()
anova.time.6trials.densitymodel<-data.frame(anova(time.6trials.densitymodel))
write.csv(anova.time.6trials.densitymodel, file ="anova.time.6trials.densitymodel.csv")

#without density model
gc()
time.6trials.nodensitymodel<-lmer(time~trial*line*sex+ (1|vial), data=individual.6trials,REML=T)
summary.time.6trials.nodensitymodel<-data.frame(summary(time.6trials.nodensitymodel)$coefficients)
write.csv(summary.time.6trials.nodensitymodel, file ="summary.time.6trials.nodensitymodel.csv")
#test random effect
time.6trials.nodensitymodel.norandom<-lm(time~trial*line*sex, data=individual.6trials)
anova(time.6trials.nodensitymodel,time.6trials.nodensitymodel.norandom)
#without density anova
gc()
anova.time.6trials.nodensitymodel<-data.frame(anova(time.6trials.nodensitymodel))
write.csv(anova.time.6trials.nodensitymodel, file ="anova.time.6trials.nodensitymodel.csv")
##########################################################################################################################
# 4 trials
#individual data frame
for (i in 1:4){
  assign(paste0('individual.trial',i), data.frame(get(paste0('individual',i)),rep(i,length(get(paste0('individual',i)))),get(paste0('line',i)),get(paste0('vial',i)),get(paste0('sex',i)),get(paste0('time',i)),get(paste0('site',i)), get(paste0('density',i))))
}
individual.trial<-rbind(individual.trial1,individual.trial2,individual.trial3,individual.trial4)
colnames(individual.trial)[1] <- "individual"
colnames(individual.trial)[2] <- "trial"
colnames(individual.trial)[3] <- "line"
colnames(individual.trial)[4] <- "vial"
colnames(individual.trial)[5] <- "sex" #1=male, 2=female
colnames(individual.trial)[6] <- "time"
colnames(individual.trial)[7] <- "site" #0=vial, 1=cotton
colnames(individual.trial)[8] <- "density" #emerged or #collected in a vial, include both cotton and vial group, male and female
individual.4trials<-individual.trial
#change variable to factor
individual.4trials$trial<-factor(individual.4trials$trial)
individual.4trials$line<-factor(individual.4trials$line)
individual.4trials$vial<-factor(individual.4trials$vial)
individual.4trials$sex<-factor(individual.4trials$sex)
#time
library(lme4)
library(lmerTest)
options(max.print=1000000)
#with density 
gc()
time.4trials.densitymodel<-lmer(time~trial*line*sex*density+ (1|vial), data=individual.4trials,REML=T)
summary.time.4trials.densitymodel<-data.frame(summary(time.4trials.densitymodel)$coefficients)
write.csv(summary.time.4trials.densitymodel, file ="summary.time.4trials.densitymodel.csv")
#test random effect
time.4trials.densitymodel.norandom<-lm(time~trial*line*sex*density, data=individual.4trials)
anova(time.4trials.densitymodel,time.4trials.densitymodel.norandom)
#with density anova
gc()
anova.time.4trials.densitymodel<-data.frame(anova(time.4trials.densitymodel))
write.csv(anova.time.4trials.densitymodel, file ="anova.time.4trials.densitymodel.csv")

#without density model
gc()
time.4trials.nodensitymodel<-lmer(time~trial*line*sex+ (1|vial), data=individual.4trials,REML=T)
summary.time.4trials.nodensitymodel<-data.frame(summary(time.4trials.nodensitymodel)$coefficients)
write.csv(summary.time.4trials.nodensitymodel, file ="summary.time.4trials.nodensitymodel.csv")
#test random effect
time.4trials.nodensitymodel.norandom<-lm(time~trial*line*sex, data=individual.4trials)
anova(time.4trials.densitymodel,time.4trials.nodensitymodel.norandom)
#without density anova
gc()
anova.time.4trials.nodensitymodel<-data.frame(anova(time.4trials.nodensitymodel))
write.csv(anova.time.4trials.nodensitymodel, file ="anova.time.4trials.nodensitymodel.csv")
##########################################################################################################################
#trial 1-6 separate model and anova
#setwd("D:/OneDrive/RStudio/file")
#individual.6trials<-read.csv("individual.6trials.csv",header = T)
library(lme4)
library(lmerTest)
options(max.print=1000000)
individual.6trials$trial<-factor(individual.6trials$trial)
individual.6trials$line<-factor(individual.6trials$line)
individual.6trials$vial<-factor(individual.6trials$vial)
individual.6trials$sex<-factor(individual.6trials$sex)
for (i in 1:6){
  assign(paste0('individual.trial',i),c()) 
  assign(paste0('individual.trial',i),individual.6trials[individual.6trials$trial==i,] )
  write.csv(get(paste0('individual.trial',i)), file = paste0('individual.trial',i,'.csv'))
}
#with density model 
for (i in 1:6){
  assign(paste0('time.densitymodel.trial',i),lmer(time~line*sex*density+ (1|vial), data=get(paste0('individual.trial',i)),REML=T))
  assign(paste0('summary.time.densitymodel.trial',i), data.frame(summary(get(paste0('time.densitymodel.trial',i)))$coefficients))
  write.csv(get(paste0('summary.time.densitymodel.trial',i)), file=paste0('summary.time.densitymodel.trial',i,'.csv'))
  assign(paste0('anova.time.densitymodel.trial',i), data.frame(anova(get(paste0('time.densitymodel.trial',i)))))
  write.csv(get(paste0('anova.time.densitymodel.trial',i)),file =paste0('anova.time.densitymodel.trial',i,'.csv'))
  gc()
}
#without density model 
for (i in 1:6){
  assign(paste0('time.nodensitymodel.trial',i),lmer(time~line*sex+ (1|vial), data=get(paste0('individual.trial',i)),REML=T))
  assign(paste0('summary.time.nodensitymodel.trial',i), data.frame(summary(get(paste0('time.nodensitymodel.trial',i)))$coefficients))
  write.csv(get(paste0('summary.time.nodensitymodel.trial',i)), file=paste0('summary.time.nodensitymodel.trial',i,'.csv'))
  assign(paste0('anova.time.nodensitymodel.trial',i), data.frame(anova(get(paste0('time.nodensitymodel.trial',i)))))
  write.csv(get(paste0('anova.time.nodensitymodel.trial',i)),file =paste0('anova.time.nodensitymodel.trial',i,'.csv'))
  gc()
}
#find 'vial' significance 
for (i in 1:6){
  assign(paste0('time.nodensitymodel.norandom.trial',i),lm(time~line*sex, data=get(paste0('individual.trial',i))))
  print(anova(get(paste0('time.nodensitymodel.trial',i)), get(paste0('time.nodensitymodel.norandom.trial',i))))
  print(summary(get(paste0('time.nodensitymodel.trial',i))))
}
###############################################################################################################################################################
#Appendix I 
#t-tests between male and female within strains, average and variance time of each strain in each trial
for(i in 1:6){
  assign( paste0('linelist',i), as.numeric(levels(data.frame(table(get(paste0('line',i))))[,1])) )
  assign(paste0('ttest.pvalue',i),c())
  assign(paste0('ttest.lowerci',i),c())
  assign(paste0('ttest.upperci',i),c())
  assign(paste0('ttest.male.estimate',i),c())
  assign(paste0('ttest.female.estimate',i),c())
  assign(paste0('male.variance',i),c())
  assign(paste0('female.variance',i),c())
  for (j in get(paste0('linelist',i))){
    assign(paste0('ttest.pvalue',i) , c(get(paste0('ttest.pvalue',i)), t.test(individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==2],individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==1])$p.value))
    assign(paste0('ttest.lowerci',i) , c(get(paste0('ttest.lowerci',i)), t.test(individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==2],individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==1])$conf.int[1]))
    assign(paste0('ttest.upperci',i) , c(get(paste0('ttest.upperci',i)), t.test(individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==2],individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==1])$conf.int[2]))
    assign(paste0('ttest.male.estimate',i) , c(get(paste0('ttest.male.estimate',i)),    as.numeric(data.frame(t.test(individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==2],individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==1])$estimate)[,1])[2]))
    assign(paste0('ttest.female.estimate',i) , c(get(paste0('ttest.female.estimate',i)),as.numeric(data.frame(t.test(individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==2],individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==1])$estimate)[,1])[1]))
    assign(paste0('male.variance',i) , c(get(paste0('male.variance',i)),var(individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==1])))
    assign(paste0('female.variance',i) , c(get(paste0('female.variance',i)),var(individual.6trials$time[individual.6trials$trial==i & individual.6trials$line==j & individual.6trials$sex==2])))
  }
}

for(i in 1:6){
assign(paste0('ttest.table',i),cbind(rep(i,length(get(paste0('linelist',i)))) ,get(paste0('linelist',i)),get(paste0('ttest.pvalue',i)),get(paste0('ttest.lowerci',i)),
       get(paste0('ttest.upperci',i)),get(paste0('ttest.male.estimate',i)),get(paste0('ttest.female.estimate',i)),get(paste0('male.variance',i)),get(paste0('female.variance',i))))}
ttest.table<-rbind(ttest.table1,ttest.table2,ttest.table3,ttest.table4,ttest.table5,ttest.table6)
ttest.table<-data.frame(ttest.table)
colnames(ttest.table)[1] <- "trial"
colnames(ttest.table)[2] <- "line"
colnames(ttest.table)[3] <- "p-value"
colnames(ttest.table)[4] <- "lowerCI"
colnames(ttest.table)[5] <- "upperCI" 
colnames(ttest.table)[6] <- "male.estimate" 
colnames(ttest.table)[7] <- "female.estimate" 
colnames(ttest.table)[8] <- "male.variance" 
colnames(ttest.table)[9] <- "female.variance" 
ttest.table<-cbind(ttest.table,ttest.table$female.estimate-ttest.table$male.estimate)
colnames(ttest.table)[10] <- "F-M" 
write.csv(ttest.table,file="ttest.table.csv")
ttest.table<-read.csv("ttest.table.csv",header = T)
rearrange.ttest.table<-ttest.table
library(tidyverse)
rearrange.ttest.table<-rearrange.ttest.table %>% arrange(line)
write.csv(rearrange.ttest.table,file="rearrange.ttest.table.csv")
##################################################################################################################################
#Tukey
#library(multcompView)
#library(lsmeans)
#leastsquare = lsmeans(time.model2, pairwise ~ trial* line * sex, adjust="tukey")       ###  Tukey-adjusted comparisons
##################################################################################################################################
#ggplot
library(ggplot2)
library(ggpmisc)
library(tidyverse)
library(Hmisc)
library(lme4)
library(nlme)
library(extrafont)
for(i in 1:6){
  assign(paste0('viallist',i),as.numeric(levels(data.frame(table(get(paste0('vial',i))))[,1])) )
  }
viallist<-c(viallist1,viallist2,viallist3,viallist4,viallist5,viallist6)
sexdif.time.byvial<-c()
density.byvial<-c()
mean.time.byvial<-c()
for (i in viallist){ #vial average female time - male time
    sexdif.time.byvial<-c(sexdif.time.byvial,mean(individual.6trials$time[individual.6trials$vial==i&individual.6trials$sex==2])-mean(individual.6trials$time[individual.6trials$vial==i&individual.6trials$sex==1]))
    mean.time.byvial<-c(mean.time.byvial, mean(individual.6trials$time[individual.6trials$vial==i]))
    density.byvial<-c(density.byvial, mean(individual.6trials$density[individual.6trials$vial==i] ))
}
trialnumber<-c(rep(1,length(viallist1)),rep(2,length(viallist2)),rep(3,length(viallist3)),rep(4,length(viallist4)),rep(5,length(viallist5)),rep(6,length(viallist6)))
density.vial.df<-data.frame(trialnumber,density.byvial,sexdif.time.byvial,mean.time.byvial)
density.vial.df$trialnumber<-factor(density.vial.df$trialnumber)
##################F-M development time ~ vial density
#trials together
ggplot(data = density.vial.df, aes(x=density.byvial, y=sexdif.time.byvial))  + 
  geom_point(aes(color=trialnumber)) +
  scale_color_manual(name="Trial",values = c("#FC4E07","#D16103","#F0E442", "#52854C","#00AFBB","purple"))+
  geom_smooth(method='lm',formula = y~x)+
  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE,label.x.npc = "right")+
  scale_x_continuous(breaks=c(seq(100,500,100)),limits = c(0,430), expand = c(0,0))+
  scale_y_continuous(breaks=seq(-25,20,5), limits = c(-25,20),expand = c(0,0))+ #set limit <20 removed 3 data points and >-25 removed 1 data point
  geom_hline(yintercept=0, linetype="dashed")+
  scale_linetype_manual(values=c("twodash"))+
  xlab("Density by vial")+
  ylab("Average vial Female's minus Male's development time(h) ~ density")+
  theme(legend.position = c(0.9,0.2),
        legend.background = element_rect(fill=alpha('grey', 0.4)))
#trials separate
ggplot(data = density.vial.df, aes(x=density.byvial, y=sexdif.time.byvial))  + 
  geom_point() +
  facet_wrap(trialnumber~.)+
  geom_smooth(method='lm',formula = y~x)+
  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE,label.x.npc = "right")+
  scale_x_continuous(breaks=c(seq(100,500,100)),limits = c(0,430), expand = c(0,0))+
  scale_y_continuous(breaks=seq(-25,20,5), limits = c(-25,20),expand = c(0,0))+ #set limit <20 removed 3 data points and >-25 removed 1 data point
  geom_hline(yintercept=0, linetype="dashed")+
  scale_linetype_manual(values=c("twodash"))+
  xlab("Density by vial")+
  ylab("Average vial Female's minus Male's development time(h) ~ density (by trial)")+
  theme(legend.position = c(0.97,0.68),
        legend.background = element_rect(fill=alpha('grey', 0.4)))
##################average vial development time ~ vial density
#trials together
ggplot(data = density.vial.df, aes(x=density.byvial,y=mean.time.byvial))  + 
  geom_point(aes(color=trialnumber)) +
  scale_color_manual(name="Trial",values = c("#FC4E07","#D16103","#F0E442", "#52854C","#00AFBB","purple"))+
  geom_smooth(method='lm',formula = y~x)+
  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE,label.x.npc = "right")+
  scale_x_continuous(breaks=c(seq(100,500,100)),limits = c(0 , 450), expand = c(0,0))+
  scale_y_continuous(breaks=seq(200,350,50), limits = c(200,350),expand = c(0,0))+
  geom_hline(yintercept=0, linetype="dashed")+
  scale_linetype_manual(values=c("twodash"))+
  xlab("Density by vial")+
  ylab("Average vial development time(h) ~ density")+
  theme(legend.position = c(0.9,0.2),
        legend.background = element_rect(fill=alpha('grey', 0.4)))
#trials separate
ggplot(data = density.vial.df, aes(x=density.byvial,y=mean.time.byvial))  + 
  geom_point() +
  facet_wrap(trialnumber~.)+
  geom_smooth(method='lm',formula = y~x)+
  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse=TRUE,label.x.npc = "right",label.y=0.1)+
  scale_x_continuous(breaks=c(seq(100,500,100)),limits = c(0 , 450), expand = c(0,0))+
  scale_y_continuous(breaks=seq(200,350,50), limits = c(200,350),expand = c(0,0))+
  geom_hline(yintercept=0, linetype="dashed")+
  scale_linetype_manual(values=c("twodash"))+
  xlab("Density by vial")+
  ylab("Average vial development time(h) ~ density (by trial)")+
  theme(legend.position = c(0.95,0.8),
        legend.background = element_rect(fill=alpha('grey', 0.4)))
##################strain f-m development time ~ trial
t.test.table<-read.csv(file="ttest.table.csv",header = T)
t.test.table<-t.test.table[,-1]
t.test.table<-t.test.table[-3,]#remove line 3 because only trial 1 has line 3
t.test.table<-cbind(t.test.table,t.test.table$female.estimate-t.test.table$male.estimate)
colnames(t.test.table)[8] <- "FminusM"
significance<-c()
for (i in 1:nrow(t.test.table)) {
  if (is.na(t.test.table$p.value[i])) {
    significance[i]<-NA}
  else if (t.test.table$p.value[i]<=0.0005813953){ ##filled triangle 17
    significance[i]<-2}
  else if (t.test.table$p.value[i]<=0.01){ ##filled circle 16 
    significance[i]<-1}
  else {significance[i]<-0 ##p>0.01, open circle 1
  }
}
t.test.table<-cbind(t.test.table,significance)
t.test.table$significance<-as.factor(t.test.table$significance)
strainlist<-c("l1 335","l2 304","l4 313","l5 324","l6 303","l7 357","l8 360","l9 362","l10 379","l11 380","l12 391","l13 399","l14 427","l15 437","l16 486","l17 517","l18 707","l19 732","l20 786","l21 852",
              "l1 335","l2 304","l4 313","l5 324","l6 303","l7 357","l8 360","l9 362","l10 379","l11 380","l12 391","l13 399","l14 427","l15 437","l16 486","l17 517","l18 707","l19 732","l20 786","l21 852",
              "l1 335","l2 304","l4 313",                  "l7 357","l8 360","l9 362","l10 379",          "l12 391","l13 399","l14 427","l15 437","l16 486","l17 517","l18 707","l19 732","l20 786","l21 852",
              "l1 335",         "l4 313","l5 324","l6 303",         "l8 360","l9 362","l10 379","l11 380","l12 391","l13 399","l14 427","l15 437","l16 486","l17 517","l18 707","l19 732","l20 786","l21 852",
              "l1 335","l2 304",                                             "l9 362",                                        "l14 427",                    "l17 517",          "l19 732",
              "l1 335","l2 304",                                             "l9 362",                                        "l14 427",                                        "l19 732")
t.test.table<-cbind(strainlist,t.test.table)
t.test.table$strainlist = factor(t.test.table$strainlist, levels=c("l1 335","l2 304","l4 313","l5 324","l6 303","l7 357","l8 360","l9 362","l10 379","l11 380","l12 391","l13 399","l14 427","l15 437","l16 486","l17 517","l18 707","l19 732","l20 786","l21 852"))
#plot
ggplot(data = t.test.table, aes(x=trial, y=FminusM))  + 
  geom_point(aes(shape=significance)) +
  facet_wrap(strainlist~.)+
  scale_shape_manual(values = c(1, 16, 17),name=str_wrap("t-tests p-value",8),labels = c("nonsig", "p<0.01", "p<0.00058"))+
  geom_errorbar(aes(ymin=lowerCI, ymax=higherCI),width=0.4) +
  geom_hline(yintercept=0, linetype="dashed")+
  scale_linetype_manual(values=c("twodash"))+
  xlab("Trial")+
  ylab("Average strain Female's minus Male's development time (h)") +
  scale_x_continuous(breaks=seq(1,6,1))+
  scale_y_continuous(breaks=seq(-15,10,5),limits = c(-18,14),expand = c(0,0))+
  theme_bw()
##################################################################################################################################
