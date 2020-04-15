library(dplyr)
library(ggplot2)

'%!in%' <- function(x,y)!('%in%'(x,y))

setwd('D:/Twitter_Study_FYP/Analysis2_Jan2020/Data_FYP/')

participants <- read.csv('FYP_Twitter_Participants_03.02.csv')
sentiments <- read.csv('FYP_Twitter_Sentiments_03.02_v2.csv')

FYP_df <- merge(participants,sentiments,by="Id")

#exclude people who failed the attention check but keep those who were not asked it (version4_free_data)
#FYP_df <- subset(FYP_df,(FYP_df$OCI_6 == 1 | is.na(FYP_df$OCI_6)))



#reverse scored items: 2, 5, 6, 11, 12, 14, 16, 17, 18, 20  
FYP_df$SDS_2 <- 5 - FYP_df$SDS_2
FYP_df$SDS_5 <- 5 - FYP_df$SDS_5
FYP_df$SDS_6 <- 5 - FYP_df$SDS_6
FYP_df$SDS_11 <- 5 - FYP_df$SDS_11
FYP_df$SDS_12 <- 5 - FYP_df$SDS_12
FYP_df$SDS_14 <- 5 - FYP_df$SDS_14
FYP_df$SDS_16 <- 5 - FYP_df$SDS_16
FYP_df$SDS_17 <- 5 - FYP_df$SDS_17
FYP_df$SDS_18 <- 5 - FYP_df$SDS_18
FYP_df$SDS_20 <- 5 - FYP_df$SDS_20

#SDS

FYP_df$SDS_Total <- rowSums(FYP_df %>% select(colnames(FYP_df)[grepl("SDS",colnames(FYP_df))]))
FYP_df$Dep_ep_pastyear <- as.factor(FYP_df$Dep_ep_pastyear)
FYP_df$Depression_Physician <- as.factor(FYP_df$Depression_Physician)

#SDS by self-reported depressive episode in the past year 
p2 <- ggplot(FYP_df, aes(x=Dep_ep_pastyear, y=SDS_Total)) + geom_boxplot() +
  geom_point(aes(fill=SDS_Total),position = position_jitterdodge())
p2

summary(glm(Dep_ep_pastyear ~ SDS_Total, data = FYP_df, family = "binomial"))

#SDS by self-reported depression diagnosis by physician  
dep_phys_plot <- ggplot(FYP_df, aes(x=Depression_Physician, y=SDS_Total)) + geom_boxplot() +
  geom_point(aes(fill=SDS_Total),position = position_jitterdodge())
dep_phys_plot 

summary(glm(Dep_ep_pastyear ~ SDS_Total, data = FYP_df, family = "binomial"))

#difference in volume by depressive episode in past year 
volume_plot <- ggplot(FYP_df, aes(x=Dep_ep_pastyear, y=volume)) + geom_boxplot() +
  geom_point(aes(fill=volume),position = position_jitterdodge())
volume_plot 

#difference in volume by depressive episode in past year 
reply_plot <- ggplot(FYP_df, aes(x=Dep_ep_pastyear, y=reply)) + geom_boxplot() +
  geom_point(aes(fill=reply),position = position_jitterdodge())
reply_plot 



summary(glm(Dep_ep_pastyear ~ SDS_Total, data = FYP_df, family = "binomial"))












