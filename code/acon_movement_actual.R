####Load all packages####
lapply(list("rstudioapi","tidyverse","glmmTMB","effects","ggplot2","patchwork"),require,character.only=TRUE)

####change working directory####
current_path <- dirname(getActiveDocumentContext()$path)
setwd(dirname(current_path))

hurdle_model_data <- read.csv("data/hurdle_model_data.csv")

#Hurdle model of movement distance by sex
hurdle_sex_model <- glmmTMB(dist ~ sex + (1|id), ziformula = ~ sex+ (1|id), data = hurdle_model_data, family = ziGamma(link="log"))
#No effect of sex on move/stay, but males have longer step length
summary(hurdle_sex_model)

#Hurdle model of movement distance in roadsides by sex
hurdle_sex_model_road<-glmmTMB(dist~sex+road_residence_count+(1|id),ziformula = ~ sex+road_residence_count+(1|id),data=hurdle_model_data,family=ziGamma(link="log"))
#No effect of sex on move/stay, but males have longer step length
summary(hurdle_sex_model_road)

#Hurdle model of movement distance in powerline clearcuts by sex
hurdle_sex_model_powerline<-glmmTMB(dist~sex+powerline_residence_count+(1|id),ziformula = ~ sex+powerline_residence_count+(1|id),data=hurdle_model_data,family=ziGamma(link="log"))
#No effect of sex on move/stay, but males have longer step length
summary(hurdle_sex_model_powerline)

#parameter estimates
AppI_Table1_raw <- modelsummary::get_estimates(hurdle_sex_model)
AppI_Table1 <- AppI_Table1_raw[c("term","component","estimate","std.error","conf.low","conf.high","effect")]
AppI_Table1 <- data.frame(lapply(AppI_Table1, function(x) if(is.numeric(x)) round(x, 2) else x))
print(AppI_Table1)

#Run binomial generalized linear mixed model with all movements to create model prediction plots
hurdle_model_data$move<-ifelse(hurdle_model_data$dist>0,1,0)

binary_road_model<-glmmTMB(as.factor(move)~road_residence_count+(1|id),data=hurdle_model_data,family=binomial)
summary(binary_road_model)

binary_powerline_model<-glmmTMB(as.factor(move)~powerline_residence_count+(1|id),data=hurdle_model_data,family=binomial)
summary(binary_powerline_model)

binary_road_predict<-Effect(c("road_residence_count"),binary_road_model,xlevels=list(road_residence_count=c("0","1")))
binary_road_predict.df<-data.frame(binary_road_predict)

binary.road.predict.plot<-ggplot(binary_road_predict.df,aes(x=factor(road_residence_count,levels=c("1","0")),y=fit))+
  geom_point(size=2)+
  geom_linerange(aes(ymin=lower,ymax=upper), size = 1)+ 
  scale_y_continuous(expand=c(0,0),limits=c(0,1),breaks=seq(0,1,.25))+
  scale_x_discrete(labels=c("0"="No","1"="Yes"))+
  xlab("Within Road Buffer Habitat?")+
  ylab("Probability of Movement")+
  theme_bw() + 
  theme(    axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
            axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())

print(binary.road.predict.plot) 


binary_powerline_predict<-Effect(c("powerline_residence_count"),binary_powerline_model,xlevels=list(powerline_residence_count=c("0","1")))
binary_powerline_predict.df<-data.frame(binary_powerline_predict)

binary.powerline.predict.plot<-ggplot(binary_powerline_predict.df,aes(x=factor(powerline_residence_count,levels=c("1","0")),y=fit))+
  geom_point(size=2)+
  geom_linerange(aes(ymin=lower,ymax=upper), size = 1)+ 
  scale_y_continuous(expand=c(0,0),limits=c(0,1),breaks=seq(0,1,.25))+
  scale_x_discrete(labels=c("0"="No","1"="Yes"))+
  xlab("Within Powerline Clear Cut?")+
  ylab("Probability of Movement")+
  theme_bw() + 
  theme(    axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
            axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())

print(binary.powerline.predict.plot)

binary.road.predict.plot.combo<-ggplot(binary_road_predict.df,aes(x=factor(road_residence_count,levels=c("1","0")),y=fit))+
  geom_point(size=2)+
  geom_linerange(aes(ymin=lower,ymax=upper), size = 1)+ 
  scale_y_continuous(expand=c(0,0),limits=c(0,1),breaks=seq(0,1,.25))+
  scale_x_discrete(labels=c("0"="No","1"="Yes"))+
  annotate("text",x=2.3,y=.9,label="A.",parse=TRUE,hjust=0,size=10)+
  xlab("Within Road Buffer Habitat?")+
  ylab("Probability of Movement")+
  theme_bw() + 
  theme(    axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
            axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())

binary.powerline.predict.plot.combo<-ggplot(binary_powerline_predict.df,aes(x=factor(powerline_residence_count,levels=c("1","0")),y=fit))+
  geom_point(size=2)+
  geom_linerange(aes(ymin=lower,ymax=upper), size = 1)+ 
  scale_y_continuous(expand=c(0,0),limits=c(0,1),breaks=seq(0,1,.25))+
  scale_x_discrete(labels=c("0"="No","1"="Yes"))+
  annotate("text",x=2.3,y=.9,label="B.",parse=TRUE,hjust=0,size=10)+
  xlab("Within Powerline Clear Cut?")+
  ylab("")+
  theme_bw() + 
  theme(    axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
            axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())

Fig5_binary.combo.plot<-binary.road.predict.plot.combo+binary.powerline.predict.plot.combo
print(Fig5_binary.combo.plot) 


####Random Paths Analysis####
#load data
true_crossings.filtered.female <- read.csv("data/true_crossings_females.csv")
true_crossings.filtered.male <- read.csv("data/true_crossings_males.csv")

rand_angle_crossings_female.filtered <- read.csv("data/rand_angle_crossings_females.csv")
rand_angle_crossings_male.filtered <- read.csv("data/rand_angle_crossings_males.csv")
rand_point_female.filtered <- read.csv("data/rand_point_crossings_females.csv")
rand_point_male.filtered <- read.csv("data/rand_point_crossings_males.csv")

#summarize data
rand_crossings_male.filtered%>%
  group_by(rep)%>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_point_male_summary.filtered

rand_crossings_female.filtered%>%
  group_by(rep)%>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_point_female_summary.filtered


rand_angle_crossings_male.filtered%>%
  group_by(rep)%>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_angle_male_summary.filtered
rand_angle_crossings_female.filtered%>%
  group_by(rep)%>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_angle_female_summary.filtered


#create percentile value (Pi) function
ecdf_fun <- function(x,perc) ecdf(x)(perc)

#Create Random Paths Plots
trail_randpoint_male<-ggplot(rand_point_male_summary.filtered,aes(trail_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.male$trail_crossing_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.male$trail_crossing_count)-2,y=175*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_male_summary.filtered$trail_crossings,sum(true_crossings.filtered.male$trail_crossing_count))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,125),breaks=seq(0,125,25))+
  scale_y_continuous(expand=c(0,0),limits=c(0,175),breaks=seq(0,175,25))+
  ylab("Frequency")+
  xlab("Trail Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


road_randpoint_male<-ggplot(rand_point_male_summary.filtered,aes(road_crossings))+
  geom_histogram(binwidth=10,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.male$road_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.male$road_count)+1,y=175*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_male_summary.filtered$road_crossings,sum(true_crossings.filtered.male$road_count))),parse=TRUE,hjust=0,size=4)+
  annotate("text",label="♂",x=225*0.9,y=175*0.85,size = 10, family = "Arial Unicode MS")+
  scale_x_continuous(expand=c(0,0),limits=c(0,225),breaks=seq(0,225,75))+
  scale_y_continuous(expand=c(0,0),limits=c(0,175),breaks=seq(0,175,25))+
  ylab("Frequency")+
  xlab("Road Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))



powerline_poly_randpoint_male<-ggplot(rand_point_male_summary.filtered,aes(powerline_residence))+
  geom_histogram(binwidth=2,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.male$powerlines_poly_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.male$powerlines_poly_count)-1,y=600*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_male_summary.filtered$powerline_residence,sum(true_crossings.filtered.male$powerlines_poly_count))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,100),breaks=seq(0,100,20))+
  scale_y_continuous(expand=c(0,0),limits=c(0,600),breaks=seq(0,600,100))+
  ylab("Frequency")+
  xlab("Points in Powerline Clearing per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


trail_randangle_male<-ggplot(rand_angle_male_summary.filtered,aes(trail_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.male$trail_crossing_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.male$trail_crossing_count)-2,y=160*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_male_summary.filtered$trail_crossings,sum(true_crossings.filtered.male$trail_crossing_count))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,120),breaks=seq(0,120,20))+
  scale_y_continuous(expand=c(0,0),limits=c(0,160),breaks=seq(0,160,40))+
  ylab("Frequency")+
  xlab("Trail Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


road_randangle_male<-ggplot(rand_angle_male_summary.filtered,aes(road_crossings))+
  geom_histogram(binwidth=10,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.male$road_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.male$road_count)+1,y=125*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_male_summary.filtered$road_crossings,sum(true_crossings.filtered.male$road_count))),parse=TRUE,hjust=0,size=4)+
  annotate("text",label="♂",x=250*0.9,y=125*0.85,size = 10, family = "Arial Unicode MS")+
  scale_x_continuous(expand=c(0,0),limits=c(0,250),breaks=seq(0,250,50))+
  scale_y_continuous(expand=c(0,0),limits=c(0,125),breaks=seq(0,125,25))+
  ylab("Frequency")+
  xlab("Road Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

powerline_poly_randangle_male<-ggplot(rand_angle_male_summary.filtered,aes(powerline_residence))+
  geom_histogram(binwidth=2,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.male$powerlines_poly_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.male$powerlines_poly_count)-1,y=150*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_male_summary.filtered$powerline_residence,sum(true_crossings.filtered.male$powerlines_poly_count))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,60),breaks=seq(0,60,10))+
  scale_y_continuous(expand=c(0,0),limits=c(0,150),breaks=seq(0,150,25))+
  ylab("Frequency")+
  xlab("Points in Powerline Clearing per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

road_res_randpoint_male<-ggplot(rand_point_male_summary.filtered,aes(road_residence))+
  geom_histogram(binwidth=25,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.male$road_residence),size=1)+
  annotate("text",x=sum(true_crossings.filtered.male$road_residence)-2,y=350*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_male_summary.filtered$road_residence,sum(true_crossings.filtered.male$road_residence))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,300),breaks=seq(0,300,100))+
  scale_y_continuous(expand=c(0,0),limits=c(0,350),breaks=seq(0,350,50))+
  ylab("Frequency")+
  xlab("Points in Road Buffer per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

road_res_randangle_male<-ggplot(rand_angle_male_summary.filtered,aes(road_residence))+
  geom_histogram(binwidth=25,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.male$road_residence),size=1)+
  annotate("text",x=sum(true_crossings.filtered.male$road_residence)-2,y=350*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_male_summary.filtered$road_residence,sum(true_crossings.filtered.male$road_residence))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,300),breaks=seq(0,300,50))+
  scale_y_continuous(expand=c(0,0),limits=c(0,350),breaks=seq(0,350,50))+
  ylab("Frequency")+
  xlab("Points in Road Buffer per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

#female plots
trail_randpoint_female<-ggplot(rand_point_female_summary.filtered,aes(trail_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.female$trail_crossing_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.female$trail_crossing_count)-2,y=200*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_female_summary.filtered$trail_crossings,sum(true_crossings.filtered.female$trail_crossing_count))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,100),breaks=seq(0,100,25))+
  scale_y_continuous(expand=c(0,0),limits=c(0,200),breaks=seq(0,200,25))+
  ylab("Frequency")+
  xlab("Trail Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


road_randpoint_female<-ggplot(rand_point_female_summary.filtered,aes(road_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.female$road_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.female$road_count)+1,y=250*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_female_summary.filtered$road_crossings,sum(true_crossings.filtered.female$road_count))),parse=TRUE,hjust=0,size=4)+
  annotate("text",label="♀",x=100*0.92,y=250*0.85,size = 10, family = "Arial Unicode MS")+
  scale_x_continuous(expand=c(0,0),limits=c(0,100),breaks=seq(0,100,20))+
  scale_y_continuous(expand=c(0,0),limits=c(0,250),breaks=seq(0,250,50))+
  ylab("Frequency")+
  xlab("Road Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


powerline_poly_randpoint_female<-ggplot(rand_point_female_summary.filtered,aes(powerline_residence))+
  geom_histogram(binwidth=2,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.female$powerlines_poly_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.female$powerlines_poly_count)-1,y=700*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_female_summary.filtered$powerline_residence,sum(true_crossings.filtered.female$powerlines_poly_count))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,80),breaks=seq(0,80,20))+
  scale_y_continuous(expand=c(0,0),limits=c(0,700),breaks=seq(0,700,100))+
  ylab("Frequency")+
  xlab("Points in Powerline Clearing per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


trail_randangle_female<-ggplot(rand_angle_female_summary.filtered,aes(trail_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.female$trail_crossing_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.female$trail_crossing_count)-2,y=200*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_female_summary.filtered$trail_crossings,sum(true_crossings.filtered.female$trail_crossing_count))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,75),breaks=seq(0,75,25))+
  scale_y_continuous(expand=c(0,0),limits=c(0,200),breaks=seq(0,200,50))+
  ylab("Frequency")+
  xlab("Trail Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


road_randangle_female<-ggplot(rand_angle_female_summary.filtered,aes(road_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.female$road_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.female$road_count)+1,y=300*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_female_summary.filtered$road_crossings,sum(true_crossings.filtered.female$road_count))),parse=TRUE,hjust=0,size=4)+
  annotate("text",label="♀",x=90*0.92,y=300*0.85,size = 10, family = "Arial Unicode MS")+
  scale_x_continuous(expand=c(0,0),limits=c(0,90),breaks=seq(0,90,30))+
  scale_y_continuous(expand=c(0,0),limits=c(0,300),breaks=seq(0,300,50))+
  ylab("Frequency")+
  xlab("Road Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

powerline_poly_randangle_female<-ggplot(rand_angle_female_summary.filtered,aes(powerline_residence))+
  geom_histogram(binwidth=2,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.female$powerlines_poly_count),size=1)+
  annotate("text",x=sum(true_crossings.filtered.female$powerlines_poly_count)-1,y=300*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_female_summary.filtered$powerline_residence,sum(true_crossings.filtered.female$powerlines_poly_count))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,80),breaks=seq(0,80,20))+
  scale_y_continuous(expand=c(0,0),limits=c(0,300),breaks=seq(0,300,50))+
  ylab("Frequency")+
  xlab("Points in Powerline Clearing per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

road_res_randpoint_female<-ggplot(rand_point_female_summary.filtered,aes(road_residence))+
  geom_histogram(binwidth=25,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.female$road_residence),size=1)+
  annotate("text",x=sum(true_crossings.filtered.female$road_residence)-2,y=350*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_female_summary.filtered$road_residence,sum(true_crossings.filtered.female$road_residence))),
           parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,250),breaks=seq(0,250,50))+
  scale_y_continuous(expand=c(0,0),limits=c(0,350),breaks=seq(0,350,50))+
  ylab("Frequency")+
  xlab("Points in Road Buffer per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

road_res_randangle_female<-ggplot(rand_angle_female_summary.filtered,aes(road_residence))+
  geom_histogram(binwidth=25,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings.filtered.female$road_residence),size=1)+
  annotate("text",x=sum(true_crossings.filtered.female$road_residence)-2,y=300*0.92,label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_female_summary.filtered$road_residence,sum(true_crossings.filtered.female$road_residence))),parse=TRUE,hjust=1,size=4)+
  scale_x_continuous(expand=c(0,0),limits=c(0,250),breaks=seq(0,250,50))+
  scale_y_continuous(expand=c(0,0),limits=c(0,300),breaks=seq(0,300,50))+
  ylab("Frequency")+
  xlab("Points in Road Buffer per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

#write rand angle figure
Fig2_rand_point_all <- (road_randpoint_female + road_randpoint_male)/
  (trail_randpoint_female + trail_randpoint_male)/
  (powerline_poly_randpoint_female + powerline_poly_randpoint_male)/
  (road_res_randpoint_female + road_res_randpoint_male)
print(Fig2_rand_point_all)

#write rand angle/rand point figure
Fig3_rand_angle_all <- (road_randangle_female + road_randangle_male)/
  (trail_randangle_female + trail_randangle_male)/
  (powerline_poly_randangle_female + powerline_poly_randangle_male)/
  (road_res_randangle_female + road_res_randangle_male)
print(Fig3_rand_angle_all)

#Leave-one-individual-out-Cross-Validation for Random Angle Analysis
#LOOCV with id-only
LOOCV_angle_final_id_female<-data.frame(id=character(length(unique(true_crossings.filtered.female$id))),
                                        trail_crossing_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        #                                stream_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        road_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        #                                 powerlines_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        powerlines_poly_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        road_residence_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))))
count<-0
for(j in unique(true_crossings.filtered.female$id)){
  count<-count+1
  LOOCV<-subset(rand_angle_crossings_female.filtered,paste0(id)!=j)
  LOOCV%>%
    group_by(rep)%>%
    summarize(trail_count=sum(trail_crossing_count),
              road_count=sum(road_count),
              powerlines_poly_count=sum(powerlines_poly_count),
              road_residence=sum(road_residence))->temp_summary
  true_crossings.filtered.female%>%
    subset(paste0(id)!=paste0(sprintf("%02s",j)))%>%
    summarize(trail_count=sum(trail_crossing_count),
              road_count=sum(road_count),
              powerlines_poly_count=sum(powerlines_poly_count),
              road_residence=sum(road_residence))->temp_true_summary
  LOOCV_angle_final_id_female$id[count]<-j
  LOOCV_angle_final_id_female$trail_crossing_LOOCV[count]<-ecdf_fun(temp_summary$trail_count,temp_true_summary$trail_count)
  LOOCV_angle_final_id_female$road_LOOCV[count]<-ecdf_fun(temp_summary$road_count,temp_true_summary$road_count)
  LOOCV_angle_final_id_female$powerlines_poly_LOOCV[count]<-ecdf_fun(temp_summary$powerlines_poly_count,temp_true_summary$powerlines_poly_count)
  LOOCV_angle_final_id_female$road_residence_LOOCV[count]<-ecdf_fun(temp_summary$road_residence,temp_true_summary$road_residence)
}

LOOCV_angle_final_id_male<-data.frame(id=character(length(unique(true_crossings.filtered.male$id))),
                                      trail_crossing_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      #stream_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      road_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      #powerlines_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      powerlines_poly_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      road_residence_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))))
count<-0
for(j in unique(rand_angle_crossings_male.filtered$id)){
  count<-count+1
  LOOCV<-subset(rand_angle_crossings_male.filtered,paste0(id)!=paste0(j))
  LOOCV%>%
    group_by(rep)%>%
    summarize(trail_count=sum(trail_crossing_count),
              road_count=sum(road_count),
              powerlines_poly_count=sum(powerlines_poly_count),
              road_residence=sum(road_residence))->temp_summary
  true_crossings.filtered.male%>%
    subset(paste0(id)!=paste0(sprintf("%02s",j)))%>%
    summarize(trail_count=sum(trail_crossing_count),
              road_count=sum(road_count),
              powerlines_poly_count=sum(powerlines_poly_count),
              road_residence=sum(road_residence))->temp_true_summary
  LOOCV_angle_final_id_male$id[count]<-j
  LOOCV_angle_final_id_male$trail_crossing_LOOCV[count]<-ecdf_fun(temp_summary$trail_count,temp_true_summary$trail_count)
  LOOCV_angle_final_id_male$road_LOOCV[count]<-ecdf_fun(temp_summary$road_count,temp_true_summary$road_count)
  LOOCV_angle_final_id_male$powerlines_poly_LOOCV[count]<-ecdf_fun(temp_summary$powerlines_poly_count,temp_true_summary$powerlines_poly_count)
  LOOCV_angle_final_id_male$road_residence_LOOCV[count]<-ecdf_fun(temp_summary$road_residence,temp_true_summary$road_residence)
}

LOOCV_angle_long_id_male<-pivot_longer(LOOCV_angle_final_id_male,!c("id"),names_to="feature",values_to="crossings")
LOOCV_angle_long_id_female<-pivot_longer(LOOCV_angle_final_id_female,!c("id"),names_to="feature",values_to="crossings")

AppIII_Fig1_LOOCV.angle.male.plot<-ggplot(LOOCV_angle_long_id_male,aes(x=factor(feature,levels=c("road_LOOCV","trail_crossing_LOOCV","powerlines_poly_LOOCV","road_residence_LOOCV")),y=crossings))+
  ggbeeswarm::geom_beeswarm()+ 
  scale_y_continuous(expand=c(.01,0),limits=c(0,1),breaks=seq(0,1,0.25))+
  scale_x_discrete(labels=c(#"stream_LOOCV"="Stream Crossing",
    "road_LOOCV"="Road Crossing",
    "trail_crossing_LOOCV"="Trail Crossing",
    "powerlines_poly_LOOCV"="Powerline Residence",
    "road_residence_LOOCV"="Location Within\n5m of Road"))+
  ylab(bquote(italic(P[i])))+
  annotate("text",label="♂",x=4.3,y=1*0.1,size = 10, family = "Arial Unicode MS")+
  theme_bw() + 
  theme(    axis.title.x = element_blank(),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=18, color="black", vjust=1),
            axis.text.x = element_text(angle=45,hjust=1,family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())

print(AppIII_Fig1_LOOCV.angle.male.plot)

AppIII_Fig2_LOOCV.angle.female.plot<-ggplot(LOOCV_angle_long_id_female,aes(x=factor(feature,levels=c("stream_LOOCV","road_LOOCV","trail_crossing_LOOCV","powerlines_poly_LOOCV","road_residence_LOOCV")),y=crossings))+
  ggbeeswarm::geom_beeswarm()+ 
  scale_y_continuous(expand=c(.01,0),limits=c(0,1),breaks=seq(0,1,0.25))+
  scale_x_discrete(labels=c(#"stream_LOOCV"="Stream Crossing",
    "road_LOOCV"="Road Crossing",
    "trail_crossing_LOOCV"="Trail Crossing",
    "powerlines_poly_LOOCV"="Powerline Residence",
    "road_residence_LOOCV"="Location Within\n5m of Road"))+
  ylab(bquote(italic(P[i])))+
  annotate("text",label="♀",x=4.3,y=1*0.1,size = 10, family = "Arial Unicode MS")+
  theme_bw() + 
  theme(    axis.title.x = element_blank(),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=18, color="black", vjust=1),
            axis.text.x = element_text(angle=45,hjust=1,family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())


print(AppIII_Fig2_LOOCV.angle.female.plot)


#Leave-one-individual-out-Cross-Validation for Random Point-Random Angle Analysis
#LOOCV with id-only
LOOCV_point_final_id_female<-data.frame(id=character(length(unique(true_crossings.filtered.female$id))),
                                        trail_crossing_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        #                                       stream_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        road_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        #                                        powerlines_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        powerlines_poly_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))),
                                        road_residence_LOOCV=numeric(length(unique(true_crossings.filtered.female$id))))
count<-0
for(j in unique(true_crossings.filtered.female$id)){
  count<-count+1
  LOOCV<-subset(rand_crossings_female.filtered,paste0(id)!=j)
  LOOCV%>%
    group_by(rep)%>%
    summarize(trail_count=sum(trail_crossing_count),
              road_count=sum(road_count),
              powerlines_poly_count=sum(powerlines_poly_count),
              road_residence=sum(road_residence))->temp_summary
  true_crossings.filtered.female%>%
    subset(paste0(id)!=paste0(sprintf("%02s",j)))%>%
    summarize(trail_count=sum(trail_crossing_count),
              road_count=sum(road_count),
              powerlines_poly_count=sum(powerlines_poly_count),
              road_residence=sum(road_residence))->temp_true_summary
  LOOCV_point_final_id_female$id[count]<-j
  LOOCV_point_final_id_female$trail_crossing_LOOCV[count]<-ecdf_fun(temp_summary$trail_count,temp_true_summary$trail_count)
  LOOCV_point_final_id_female$road_LOOCV[count]<-ecdf_fun(temp_summary$road_count,temp_true_summary$road_count)
  LOOCV_point_final_id_female$powerlines_poly_LOOCV[count]<-ecdf_fun(temp_summary$powerlines_poly_count,temp_true_summary$powerlines_poly_count)
  LOOCV_point_final_id_female$road_residence_LOOCV[count]<-ecdf_fun(temp_summary$road_residence,temp_true_summary$road_residence)
}

LOOCV_point_final_id_male<-data.frame(id=character(length(unique(true_crossings.filtered.male$id))),
                                      trail_crossing_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      #                                      stream_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      road_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      #                                      powerlines_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      powerlines_poly_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))),
                                      road_residence_LOOCV=numeric(length(unique(true_crossings.filtered.male$id))))
count<-0
for(j in unique(rand_crossings_male.filtered$id)){
  count<-count+1
  LOOCV<-subset(rand_crossings_male.filtered,paste0(id)!=paste0(j))
  LOOCV%>%
    group_by(rep)%>%
    summarize(trail_count=sum(trail_crossing_count),
              road_count=sum(road_count),
              powerlines_poly_count=sum(powerlines_poly_count),
              road_residence=sum(road_residence))->temp_summary
  true_crossings.filtered.male%>%
    subset(paste0(id)!=paste0(sprintf("%02s",j)))%>%
    summarize(trail_count=sum(trail_crossing_count),
              road_count=sum(road_count),
              powerlines_poly_count=sum(powerlines_poly_count),
              road_residence=sum(road_residence))->temp_true_summary
  LOOCV_point_final_id_male$id[count]<-j
  LOOCV_point_final_id_male$trail_crossing_LOOCV[count]<-ecdf_fun(temp_summary$trail_count,temp_true_summary$trail_count)
  LOOCV_point_final_id_male$road_LOOCV[count]<-ecdf_fun(temp_summary$road_count,temp_true_summary$road_count)
  LOOCV_point_final_id_male$powerlines_poly_LOOCV[count]<-ecdf_fun(temp_summary$powerlines_poly_count,temp_true_summary$powerlines_poly_count)
  LOOCV_point_final_id_male$road_residence_LOOCV[count]<-ecdf_fun(temp_summary$road_residence,temp_true_summary$road_residence)
}

LOOCV_point_long_id_male<-pivot_longer(LOOCV_point_final_id_male,!c("id"),names_to="feature",values_to="crossings")
LOOCV_point_long_id_female<-pivot_longer(LOOCV_point_final_id_female,!c("id"),names_to="feature",values_to="crossings")

AppIII_Fig3_LOOCV.point.male.plot<-ggplot(LOOCV_point_long_id_male,aes(x=factor(feature,levels=c("road_LOOCV","trail_crossing_LOOCV","powerlines_poly_LOOCV","road_residence_LOOCV")),y=crossings))+
  ggbeeswarm::geom_beeswarm()+ 
  scale_y_continuous(expand=c(.01,0),limits=c(0,1),breaks=seq(0,1,0.25))+
  scale_x_discrete(labels=c(#"stream_LOOCV"="Stream Crossing",
    "road_LOOCV"="Road Crossing",
    "trail_crossing_LOOCV"="Trail Crossing",
    "powerlines_poly_LOOCV"="Powerline Residence",
    "road_residence_LOOCV"="Location Within\n5m of Road"))+
  ylab(bquote(italic(P[i])))+
  annotate("text",label="♂",x=4.3,y=1*0.1,size = 10, family = "Arial Unicode MS")+
  theme_bw() + 
  theme(    axis.title.x = element_blank(),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=18, color="black", vjust=1),
            axis.text.x = element_text(angle=45,hjust=1,family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())

print(AppIII_Fig3_LOOCV.point.male.plot)

AppIII_Fig4_LOOCV.point.female.plot<-ggplot(LOOCV_point_long_id_female,aes(x=factor(feature,levels=c("road_LOOCV","trail_crossing_LOOCV","powerlines_poly_LOOCV","road_residence_LOOCV")),y=crossings))+
  ggbeeswarm::geom_beeswarm()+ 
  scale_y_continuous(expand=c(.01,0),limits=c(0,1),breaks=seq(0,1,0.25))+
  scale_x_discrete(labels=c(#"stream_LOOCV"="Stream Crossing",
    "road_LOOCV"="Road Crossing",
    "trail_crossing_LOOCV"="Trail Crossing",
    "powerlines_poly_LOOCV"="Powerline Residence",
    "road_residence_LOOCV"="Location Within\n5m of Road"))+
  ylab(bquote(italic(P[i])))+
  annotate("text",label="♀",x=4.3,y=1*0.1,size = 10, family = "Arial Unicode MS")+
  theme_bw() + 
  theme(    axis.title.x = element_blank(),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=18, color="black", vjust=1),
            axis.text.x = element_text(angle=45,hjust=1,family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())


print(AppIII_Fig4_LOOCV.point.female.plot)

####Step Selection Function Analysis####
tracks_ssf_final_male <- read.csv("data/tracks_ssf_final_male.csv")
tracks_ssf_final_female <- read.csv("data/tracks_ssf_final_female.csv")
#male model
tracks_ssf_final_male <- read.csv("data/tracks_ssf_final_male.csv")
tracks_ssf_final_female <- read.csv("data/tracks_ssf_final_female.csv")

TMBStruc.male = glmmTMB(used ~ -1 + trail_crossing_bin + road_bin + powerlines_poly_bin +  
                          road_residence_count + (1|str_ID) + 
                          (0 + trail_crossing_bin | snakeid) + 
                          (0 + road_bin | snakeid)   + 
                          (0 + powerlines_poly_bin | snakeid) + 
                          (0 + road_residence_count | snakeid) , 
                        family=poisson, data=tracks_ssf_final_male, doFit=FALSE) 

TMBStruc.male$parameters$theta[1] = log(1e3) 
TMBStruc.male$mapArg = list(theta=factor(c(NA,1:4)))

glmm.TMB.bin.random.male <- glmmTMB:::fitTMB(TMBStruc.male)
summary(glmm.TMB.bin.random.male)

confint(glmm.TMB.bin.random.male)

#female model
TMBStruc.female = glmmTMB(used ~ -1 + trail_crossing_bin + road_bin + powerlines_poly_bin +  
                            road_residence_count +  (1|str_ID) + 
                            (0 + trail_crossing_bin | snakeid) + 
                            (0 + road_bin | snakeid)   + 
                            (0 + powerlines_poly_bin | snakeid) +
                            (0 + road_residence_count | snakeid), 
                          family=poisson, data=tracks_ssf_final_female, doFit=FALSE) 

TMBStruc.female$parameters$theta[1] = log(1e3) 
TMBStruc.female$mapArg = list(theta=factor(c(NA,1:4)))

glmm.TMB.bin.random.female <- glmmTMB:::fitTMB(TMBStruc.female)
summary(glmm.TMB.bin.random.female)

confint(glmm.TMB.bin.random.female)

#Create individual data frames and plots with Relative Selection Strength
# For males
male_conf_int_log_rss <- confint(glmm.TMB.bin.random.male)

# For females
female_conf_int_log_rss <- confint(glmm.TMB.bin.random.female)


# For example, to get just the conditional fixed effects:
male_log_rss_ci_df <- as.data.frame(male_conf_int_log_rss)
male_log_rss_ci_fixed <- male_log_rss_ci_df[c("trail_crossing_bin", "road_bin", "powerlines_poly_bin", "road_residence_count"), ] # Adjust names if needed

female_log_rss_ci_df <- as.data.frame(female_conf_int_log_rss)
female_log_rss_ci_fixed <- female_log_rss_ci_df[c("trail_crossing_bin", "road_bin", "powerlines_poly_bin", "road_residence_count"), ] # Adjust names if needed

# Now, to get the RSS values and their CIs, you exponentiate these:

# For males
male_rss_summary_from_confint <- male_log_rss_ci_fixed %>%
  mutate(
    feature = factor(c("Trail\nCrossing", "Road\nCrossing", "Powerlines\nResidence", "Location Within\n5m of Road"),
                     levels = c("Road\nCrossing", "Trail\nCrossing", "Powerlines\nResidence", "Location Within\n5m of Road")),
    rss_estimate = exp(Estimate), # 'Estimate' is the column name in confint() output
    rss_lower_ci = exp(`2.5 %`), # '2.5 %' is the column name for lower CI
    rss_upper_ci = exp(`97.5 %`) # '97.5 %' is the column name for upper CI
  ) %>%
  arrange(feature)

# For females
female_rss_summary_from_confint <- female_log_rss_ci_fixed %>%
  mutate(
    feature = factor(c("Trail\nCrossing", "Road\nCrossing", "Powerlines\nResidence", "Location Within\n5m of Road"),
                     levels = c("Road\nCrossing", "Trail\nCrossing", "Powerlines\nResidence", "Location Within\n5m of Road")),
    rss_estimate = exp(Estimate),
    rss_lower_ci = exp(`2.5 %`),
    rss_upper_ci = exp(`97.5 %`)) %>%
  arrange(feature)

#Plot with all features combined
all.features.male.plot<-ggplot(male_rss_summary_from_confint, aes(x=factor(feature,levels=unique(feature)),y=log(rss_estimate)))+
  geom_point(size=2)+
  geom_linerange(aes(ymin=log(rss_lower_ci),ymax=log(rss_upper_ci)), size = 1)+ 
  annotate("text",label="♂",x=4.15,y=-2.5*0.85,size = 16, family = "Arial Unicode MS")+
  scale_y_continuous(expand=c(0,0),limits=c(-3,1.5),breaks=seq(-3,1.5,0.5))+
  scale_x_discrete()+
  geom_hline(yintercept=0,linetype="dashed",size=1)+
  ylab("")+
  theme_bw() + 
  theme(    axis.title.x = element_blank(),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
            axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#Create plot
print(all.features.male.plot)

male_rss_summary_from_confint %>%
  select(feature, Estimate, `2.5 %`, `97.5 %`, rss_estimate, rss_lower_ci, rss_upper_ci) %>%
  mutate(across('feature', str_replace, '\n', ' ')) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  rename(Feature = feature,
         `β Estimate` = Estimate,
         `β Lower` = `2.5 %`,
         `β Upper` = `97.5 %`,
         `RSS Estimate` = rss_estimate,
         `RSS Lower` = rss_lower_ci,
         `RSS Upper` = rss_upper_ci) %>%
  remove_rownames() -> male_rss_summary
print(male_rss_summary)

all.features.female.plot<-ggplot(female_rss_summary_from_confint, aes(x=factor(feature,levels=unique(feature)),y=log(rss_estimate)))+
  geom_point(size=2)+
  geom_linerange(aes(ymin=log(rss_lower_ci),ymax=log(rss_upper_ci)), size = 1)+ 
  annotate("text",label="♀",x=4.15,y=-2.5*0.85,size = 16, family = "Arial Unicode MS")+
  scale_y_continuous(expand=c(0,0),limits=c(-3,1.5),breaks=seq(-3,1.5,0.5))+
  scale_x_discrete()+
  geom_hline(yintercept=0,linetype="dashed",size=1)+
  ylab("Log-transformed RSS")+
  theme_bw() + 
  theme(    axis.title.x = element_blank(),
            axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
            axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
            axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
            panel.grid.minor = element_blank(),panel.grid.major = element_blank())

#Create plot
print(all.features.female.plot)


female_rss_summary_from_confint %>%
  select(feature, Estimate, `2.5 %`, `97.5 %`, rss_estimate, rss_lower_ci, rss_upper_ci) %>%
  mutate(across('feature', str_replace, '\n', ' ')) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  rename(Feature = feature,
         `β Estimate` = Estimate,
         `β Lower` = `2.5 %`,
         `β Upper` = `97.5 %`,
         `RSS Estimate` = rss_estimate,
         `RSS Lower` = rss_lower_ci,
         `RSS Upper` = rss_upper_ci) %>%
  remove_rownames()  -> female_rss_summary
print(female_rss_summary)

#create combined plots for male/female SSF
Fig4_SSF_all_sexes<-all.features.female.plot+all.features.male.plot
print(Fig4_SSF_all_sexes)
