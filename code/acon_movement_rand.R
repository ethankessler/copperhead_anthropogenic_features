####Load all packages####
lapply(list("rstudioapi","lubridate","tidyverse","adehabitatHR", "sf", "glmmTMB",
            "effects","ggplot2","patchwork","mapview","amt"),require,character.only=TRUE)

####change working directory####
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))

set.seed(1234)

####generate random location data####
#load study area polygon
rand_area<-st_read("GIS_Data/PV_MCP.shp")

#generate snake info
snakes_tracked = 14
acon_info <- data.frame(id            = sprintf("%02d",c(1:snakes_tracked)),
                        sex           = rep(c("male", "female"),7),
                        years.tracked = sample(1:3, 14, replace = TRUE),
                        year.started  = NA) 
for (i in 1:nrow(acon_info)){
  acon_info$year.started[[i]] <- sample(2018:(2024-acon_info$years.tracked[[i]]),1)
}

#create gamma distribution table using results from study
phi <- 1.35
intercept_log_mean <- 3.07325
sexmale_effect <- 0.48454
# Calculate the Shape parameter (alpha)
## Shape is constant for both groups: alpha = 1 / phi
gamma_shape <- 1 / phi

# Calculate the Mean distance (mu) for each sex
# female is the reference level.
mean_female <- exp(intercept_log_mean)
mean_male   <- exp(intercept_log_mean + sexmale_effect)

# Calculate the Rate parameter (beta) for each sex
## Rate = Shape / Mean
rate_female <- gamma_shape / mean_female
rate_male   <- gamma_shape / mean_male

gamma_table <- data.frame(
  sex = c("female", "male"),
  mean_dist = c(mean_female, mean_male),
  shape = gamma_shape,
  rate = c(rate_female, rate_male))

# Zero-inflation (Hurdle) model parameters
intercept_zi <- 0.2517
sexmale_effect_zi <- -0.2421

# --- Calculations for Female (Reference Level) ---
prob_zero_female <- plogis(intercept_zi)

# --- Calculations for Male ---
prob_zero_male <- plogis(intercept_zi + sexmale_effect_zi)

#blank data frame
acon <- data.frame(c())

for (i in 1:nrow(acon_info)) {
  id                 <- acon_info$id[[i]]
  sex                <- acon_info$sex[[i]]
  years_tracked      <- acon_info$years.tracked[[i]]
  year_started       <- acon_info$year.started[[i]]
  days_tracked       <- sample(25:165, years_tracked)
  for (j in 0:(years_tracked-1)) {
    days             <- days_tracked[[j+1]]
    temp_tracks      <- data.frame(id = rep(id, days))
    starting_loc     <- st_sample(rand_area, size=1)
    temp_tracks$year <- rep(year_started+j, days)
    temp_tracks$date_time[1] <- mdy_hm(paste0(1,"-",1,"-",year_started+j," 12:00"))
    temp_tracks$x[1] <- st_coordinates(starting_loc)[1,1]
    temp_tracks$y[1] <- st_coordinates(starting_loc)[1,2]
    for (k in 2:days) {
      dist <- ifelse(sex == "male", 
                     ifelse(runif(1) <= prob_zero_male,
                            0,
                            rgamma(1, shape = gamma_table$shape[2], rate = gamma_table$rate[2])),
                     ifelse(runif(1) <= prob_zero_female,
                            0,
                            rgamma(1, shape = gamma_table$shape[1], rate = gamma_table$rate[1])))
      dir  <- runif(1, 0, 2*pi)
      x.dist <- dist * cos(dir)
      y.dist <- dist * sin(dir)
      temp_tracks$x[k] <- temp_tracks$x[[k-1]] + x.dist
      temp_tracks$y[k] <- temp_tracks$y[[k-1]] + y.dist
      temp_tracks$date_time[k] <- temp_tracks$date_time[[k-1]]  + 60*60*24
    }
    acon <- rbind(acon, temp_tracks)
  }
}

#subset by year
acon_2018<-subset(acon, year==2018)
acon_2019<-subset(acon, year==2019)
acon_2020<-subset(acon, year==2020)
acon_2021<-subset(acon, year==2021)
acon_2022<-subset(acon, year==2022)
acon_2023<-subset(acon, year==2023)

####Correlated Random Walk Analysis####
#create trajectories if skipped above steps
id2018<-acon_2018$id
time2018<-as.POSIXct(acon_2018$date_time)
utm2018<-as.data.frame(acon_2018[c("x","y")])
acon_ltraj_2018 = as.ltraj(xy = utm2018, date = time2018, id = id2018, typeII=TRUE)

id2019<-acon_2019$id
time2019<-as.POSIXct(acon_2019$date_time)
utm2019<-as.data.frame(acon_2019[c("x","y")])
acon_ltraj_2019 = as.ltraj(xy = utm2019, date = time2019, id = id2019, typeII=TRUE)

id2020<-acon_2020$id
time2020<-as.POSIXct(acon_2020$date_time)
utm2020<-as.data.frame(acon_2020[c("x","y")])
acon_ltraj_2020 = as.ltraj(xy = utm2020, date = time2020, id = id2020, typeII=TRUE)

id2021<-acon_2021$id
time2021<-as.POSIXct(acon_2021$date_time)
utm2021<-as.data.frame(acon_2021[c("x","y")])
acon_ltraj_2021 = as.ltraj(xy = utm2021, date = time2021, id = id2021, typeII=TRUE)

id2022<-acon_2022$id
time2022<-as.POSIXct(acon_2022$date_time)
utm2022<-as.data.frame(acon_2022[c("x","y")])
acon_ltraj_2022 = as.ltraj(xy = utm2022, date = time2022, id = id2022, typeII=TRUE)

id2023<-acon_2023$id
time2023<-as.POSIXct(acon_2023$date_time)
utm2023<-as.data.frame(acon_2023[c("x","y")])
acon_ltraj_2023 = as.ltraj(xy = utm2023, date = time2023, id = id2023, typeII=TRUE)

####Sex-specific differences in movement lengths####
#create data frame with movement distances
acon_movement_all<-c()

for(j in c(2018:2023)){
  for(i in 1:length(get(paste0("acon_ltraj_",j)))){
    id=attr(get(paste0("acon_ltraj_",j))[[i]],"id");
    movement<-get(paste0("acon_ltraj_",j))[[i]];
    movement$id<-rep(id,length.out=length(movement$x));
    movement$sex<-rep(ifelse(id %in% sprintf("%02d",c(1,2,14,22,27,29,39)),"male","female"),length.out=length(movement$x))
    acon_movement_all<-rbind(acon_movement_all,movement)
  }
}
acon_movement_all$road_residence_count<-NA
acon_movement_all$powerline_residence_count<-NA

for(i in 1:length(acon_movement_all$id)){
  temp_point<-sfheaders::sf_point(obj=data.frame(x=acon_movement_all$x[i],y=acon_movement_all$y[i]),x="x",y="y")
  st_crs(temp_point)<-32615
  acon_movement_all$road_residence_count[i]<-ifelse(lengths(st_intersects(temp_point,st_transform(roads.poly,crs=st_crs(32615))))>0,1,0)
  acon_movement_all$powerline_residence_count[i]<-ifelse(lengths(st_intersects(temp_point,st_transform(powerlines_poly,crs=st_crs(32615))))>0,1,0)
}

#Hurdle model of movement distance by sex
hurdle_sex_model<-glmmTMB(dist~sex + (1|id),ziformula = ~ sex+ (1|id),data=subset(acon_movement_all,dt < (3*60*60*24)),family=ziGamma(link="log"))
#No effect of sex on move/stay, but males have longer step length
summary(hurdle_sex_model)

#Hurdle model of movement distance in roadsides by sex
hurdle_sex_model_road<-glmmTMB(dist~sex+road_residence_count+(1|id),ziformula = ~ sex+road_residence_count+(1|id),data=subset(acon_movement_all,dt < (3*60*60*24)),family=ziGamma(link="log"))
#No effect of sex on move/stay, but males have longer step length
summary(hurdle_sex_model_road)

#Hurdle model of movement distance in powerline clearcuts by sex
hurdle_sex_model_powerline<-glmmTMB(dist~sex+powerline_residence_count+(1|id),ziformula = ~ sex+powerline_residence_count+(1|id),data=subset(acon_movement_all,dt < (3*60*60*24)),family=ziGamma(link="log"))
#No effect of sex on move/stay, but males have longer step length
summary(hurdle_sex_model_powerline)


#parameter estimates
hurdle_sex_model_est_raw<-modelsummary::get_estimates(hurdle_sex_model)
hurdle_sex_model_est<-hurdle_sex_model_est_raw[c("term","component","estimate","std.error","conf.low","conf.high","effect")]
hurdle_sex_model_est<-data.frame(lapply(hurdle_sex_model_est, function(x) if(is.numeric(x)) round(x, 2) else x))
print(hurdle_sex_model_est)

#Run binomial generalized linear mixed model with all movements to create model prediction plots
acon_movement_all$move<-ifelse(acon_movement_all$dist>0,1,0)

binary_road_model<-glmmTMB(as.factor(move)~road_residence_count+(1|id),data=acon_movement_all,family=binomial)
summary(binary_road_model)

binary_powerline_model<-glmmTMB(as.factor(move)~powerline_residence_count+(1|id),data=acon_movement_all,family=binomial)
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
#create empty data frame
rand_crossings<-data.frame(id=character(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                           year=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                           rep=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                           trail_crossing_count=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                           road_count=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                           powerlines_poly_count=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                           road_residence=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                           locations=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000))

#Random Point/Random Angle
count<-0
for(i in unique(year(acon$date_time))){
  print(i)
  for(j in 1:length(get(paste("acon_ltraj_",i,sep="")))){
    print(attr(get(paste("acon_ltraj_",i,sep=""))[[j]],"id"))
    for(k in 1:1000){
      count<-count+1
      temp<-get(paste("acon_ltraj_",i,sep=""))[[j]]
      temp$moved_yet<-cumsum(temp$dist)
      rand_point<-st_coordinates(st_sample(rand_area,1))
      rand_angle<-runif(1,-pi,pi)
      temp$new.abs.angle<-temp$abs.angle+rand_angle
      temp$new.dx<-round(replace_na(temp$dist*cos(temp$new.abs.angle),0),5)
      temp$new.dy<-round(replace_na(temp$dist*sin(temp$new.abs.angle),0),5)
      temp$new.x[1]<-rand_point[1]
      temp$new.y[1]<-rand_point[2]
      for(l in 1:(length(temp$x)-1)){
        temp$new.x[l+1]<-temp$new.x[l]+temp$new.dx[l]
        temp$new.y[l+1]<-temp$new.y[l]+temp$new.dy[l]
      }
      df<-data.frame(easting=temp$new.x,
                     northing=temp$new.y,
                     prev_easting=c(NA,temp$new.x[c(1:length(temp$new.x)-1)]),
                     prevnorthing=c(NA,temp$new.y[c(1:length(temp$new.y)-1)]))
      ptmat <- df %>%
        as.matrix() %>%
        .[2:nrow(.), ]
      linesegs <- split(ptmat, 1:nrow(ptmat)) %>%
        lapply(., function(x) {
          x <- matrix(x, nrow = 2, byrow = T)
          x <- st_linestring(x)})
      temp_line <- st_sfc(linesegs )%>%
        st_sf('ID' = rep(attr(get(paste("acon_ltraj_",i,sep=""))[[j]],"id"),length(.)),
              'start_date'=as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$date[c(1:length(as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$x)-1)], 
              crs=st_crs(32615))
      rand_crossings$id[count]<-attr(get(paste("acon_ltraj_",i,sep=""))[[j]],"id")
      rand_crossings$year[count]<-i
      rand_crossings$trail_crossing_count[count]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615))))) > 0, npts(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615)))), 0)
      rand_crossings$road_count[count]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615))))) > 0, npts(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615)))),0)
      rand_crossings$powerlines_poly_count[count]<-length(st_geometry(st_intersection(st_as_sf(df, coords = c("easting", "northing"), crs = 32615),st_transform(powerlines_poly,crs=st_crs(32615)))))
      rand_crossings$road_residence[count]<-length(st_geometry(st_intersection(st_as_sf(df, coords = c("easting", "northing"), crs = 32615),st_transform(roads.poly,crs=st_crs(32615)))))
      rand_crossings$locations[count]<-length(df$easting)
      rand_crossings$rep[count]<-k
    }
  }
}

#separate by sex and summarize by rep
rand_crossings %>%
  subset(id %in% subset(acon_info, sex == "male")$id) %>%
  group_by(rep) %>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence)) -> rand_point_summary_males

rand_crossings %>%
  subset(id %in% subset(acon_info, sex == "female")$id) %>%
  group_by(rep) %>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence)) -> rand_point_summary_females
rand_crossings$true<-0

#create blank data frame for random angle
rand_angle_crossings<-data.frame(id=character(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                                 year=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                                 rep=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                                 trail_crossing_count=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                                 road_count=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                                 powerlines_poly_count=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                                 road_residence=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000),
                                 locations=numeric(length(unique(paste(acon$id,year(acon$date_time))))*1000))

#Random angle
count<-0
for(i in unique(year(acon$date_time))){
  print(i)
  for(j in 1:length(get(paste("acon_ltraj_",i,sep="")))){
    print(attr(get(paste("acon_ltraj_",i,sep=""))[[j]],"id"))
    for(k in 1:1000){
      count<-count+1
      temp<-get(paste("acon_ltraj_",i,sep=""))[[j]]
      rand_angle<-runif(1,-pi,pi)
      temp$new.abs.angle<-temp$abs.angle+rand_angle
      temp$new.dx<-round(replace_na(temp$dist*cos(temp$new.abs.angle),0),5)
      temp$new.dy<-round(replace_na(temp$dist*sin(temp$new.abs.angle),0),5)
      temp$new.x[1]<-temp$x[1]
      temp$new.y[1]<-temp$y[1]
      for(l in 2:(length(temp$x))){
        temp$new.x[l]<-temp$new.x[l-1]+temp$new.dx[l-1]
        temp$new.y[l]<-temp$new.y[l-1]+temp$new.dy[l-1]
      }
      df<-data.frame(easting=temp$new.x,
                     northing=temp$new.y,
                     prev_easting=c(NA,temp$new.x[c(1:length(temp$new.x)-1)]),
                     prevnorthing=c(NA,temp$new.y[c(1:length(temp$new.y)-1)]))
      ptmat <- df %>%
        as.matrix() %>%
        .[2:nrow(.), ]
      linesegs <- split(ptmat, 1:nrow(ptmat)) %>%
        lapply(., function(x) {
          x <- matrix(x, nrow = 2, byrow = T)
          x <- st_linestring(x)})
      temp_line <- st_sfc(linesegs )%>%
        st_sf('ID' = rep(attr(get(paste("acon_ltraj_",i,sep=""))[[j]],"id"),length(.)),'start_date'=as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$date[c(1:length(as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$x)-1)], crs=st_crs(32615))
      rand_angle_crossings$id[count]<-attr(get(paste("acon_ltraj_",i,sep=""))[[j]],"id")
      rand_angle_crossings$year[count]<-i
      rand_angle_crossings$trail_crossing_count[count]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615))))) > 0, npts(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615)))), 0)
      rand_angle_crossings$road_count[count]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615))))) > 0, npts(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615)))),0)
      rand_angle_crossings$powerlines_poly_count[count]<-length(st_geometry(st_intersection(st_as_sf(df, coords = c("easting", "northing"), crs = 32615),st_transform(powerlines_poly,crs=st_crs(32615)))))
      rand_angle_crossings$road_residence[count]<-length(st_geometry(st_intersection(st_as_sf(df, coords = c("easting", "northing"), crs = 32615),st_transform(roads.poly,crs=st_crs(32615)))))
      rand_angle_crossings$locations[count]<-length(df$easting)
      rand_angle_crossings$rep[count]<-k
    }
  }
}

#separate by sex and summarize by rep
rand_angle_crossings%>%
  subset(id %in% subset(acon_info, sex == "male")$id) %>%
  group_by(rep)%>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_angle_summary_males

rand_angle_crossings%>%
  subset(id %in% subset(acon_info, sex == "female")$id) %>%
  group_by(rep)%>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_angle_summary_females
rand_angle_crossings$true<-0

####Linear feature crossing in true paths####
#create blank data frame
true_crossings<-data.frame(id=character(length(unique(paste(acon$id,year(acon$date_time))))),
                           year=numeric(length(unique(paste(acon$id,year(acon$date_time))))),
                           rep=numeric(length(unique(paste(acon$id,year(acon$date_time))))),
                           trail_crossing_count=numeric(length(unique(paste(acon$id,year(acon$date_time))))),
                           road_count=numeric(length(unique(paste(acon$id,year(acon$date_time))))),
                           powerlines_poly_count=numeric(length(unique(paste(acon$id,year(acon$date_time))))),
                           road_residence=numeric(length(unique(paste(acon$id,year(acon$date_time))))),
                           locations=numeric(length(unique(paste(acon$id,year(acon$date_time))))))

#find crossings from true data
true_crossings$rep<-0
count<-0
for(i in unique(year(acon$date_time))){
  for(j in 1:length(get(paste("acon_ltraj_",i,sep="")))){
    count<-count+1
    df<-data.frame(easting=as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$x,
                   northing=as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$y,
                   prev_easting=c(NA,as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$x[c(1:length(as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$x)-1)]),
                   prevnorthing=c(NA,as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$y[c(1:length(as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$y)-1)]))
    ptmat <- df %>%
      as.matrix() %>%
      .[2:nrow(.), ]
    linesegs <- split(ptmat, 1:nrow(ptmat)) %>%
      lapply(., function(x) {
        x <- matrix(x, nrow = 2, byrow = T)
        x <- st_linestring(x)})
    temp_line <- st_sfc(linesegs )%>%
      st_sf('ID' = rep(attr(get(paste("acon_ltraj_",i,sep=""))[[j]],"id"),length(.)),'start_date'=as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$date[c(1:length(as.data.frame(get(paste("acon_ltraj_",i,sep=""))[[j]])$x)-1)], crs=st_crs(32615))
    true_crossings$id[count]<-attr(get(paste("acon_ltraj_",i,sep=""))[[j]],"id")
    true_crossings$year[count]<-i
    true_crossings$trail_crossing_count[count]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615))))) > 0, npts(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615)))), 0)
    true_crossings$road_count[count]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615))))) > 0, npts(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615)))),0)
    true_crossings$powerlines_poly_count[count]<-length(st_geometry(st_intersection(st_as_sf(df, coords = c("easting", "northing"), crs = 32615),st_transform(powerlines_poly,crs=st_crs(32615)))))
    true_crossings$road_residence[count]<-length(st_geometry(st_intersection(st_as_sf(df, coords = c("easting", "northing"), crs = 32615),st_transform(roads.poly,crs=st_crs(32615)))))
    true_crossings$locations[count]<-length(df$easting)
  }
}

true_crossings$true<-1

true_crossings%>%
  subset(id %in% subset(acon_info, sex == "male")$id) ->true_crossings_males

true_crossings%>%
  subset(id %in% subset(acon_info, sex == "female")$id) ->true_crossings_females

ecdf_fun <- function(x,perc) ecdf(x)(perc)

#Create Random Paths Plots
trail_randpoint_male<-ggplot(rand_point_summary_males,aes(trail_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_males$trail_crossing_count),size=1)+
  annotate("text",x=sum(true_crossings_males$trail_crossing_count)-2,y=175*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_summary_males$trail_crossings,sum(true_crossings_males$trail_crossing_count))),
           parse=TRUE,hjust=1,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,125),breaks=seq(0,125,25))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,175),breaks=seq(0,175,25))+
  ylab("Frequency")+
  xlab("Trail Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


road_randpoint_male<-ggplot(rand_point_summary_males,aes(road_crossings))+
  geom_histogram(binwidth=10,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_males$road_count),size=1)+
  annotate("text",x=sum(true_crossings_males$road_count)+1,y=175*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_summary_males$road_crossings,sum(true_crossings_males$road_count))),
           parse=TRUE,hjust=0,size=4)+
  annotate("text",label="♂",x=225*0.9,y=175*0.85,size = 10, family = "Arial Unicode MS")+
#  scale_x_continuous(expand=c(0,0),limits=c(0,225),breaks=seq(0,225,75))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,175),breaks=seq(0,175,25))+
  ylab("Frequency")+
  xlab("Road Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))



powerline_poly_randpoint_male<-ggplot(rand_point_summary_males,aes(powerline_residence))+
  geom_histogram(binwidth=2,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_males$powerlines_poly_count),size=1)+
  annotate("text",x=sum(true_crossings_males$powerlines_poly_count)+2,y=600*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_summary_males$powerline_residence,sum(true_crossings_males$powerlines_poly_count))),
           parse=TRUE,hjust=0,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,100),breaks=seq(0,100,20))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,600),breaks=seq(0,600,100))+
  ylab("Frequency")+
  xlab("Points in Powerline Clearing per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


trail_randangle_male<-ggplot(rand_angle_summary_males,aes(trail_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_males$trail_crossing_count),size=1)+
  annotate("text",x=sum(true_crossings_males$trail_crossing_count)-2,y=160*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_summary_males$trail_crossings,sum(true_crossings_males$trail_crossing_count))),
           parse=TRUE,hjust=1,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,120),breaks=seq(0,120,20))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,160),breaks=seq(0,160,40))+
  ylab("Frequency")+
  xlab("Trail Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


road_randangle_male<-ggplot(rand_angle_summary_males,aes(road_crossings))+
  geom_histogram(binwidth=10,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_males$road_count),size=1)+
  annotate("text",x=sum(true_crossings_males$road_count)+5,y=300*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_summary_males$road_crossings,sum(true_crossings_males$road_count))),
           parse=TRUE,hjust=0,size=4)+
  annotate("text",label="♂",x=250*0.9,y=300*0.85,size = 10, family = "Arial Unicode MS")+
#  scale_x_continuous(expand=c(0,0),limits=c(0,250),breaks=seq(0,250,50))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,300),breaks=seq(0,300,50))+
  ylab("Frequency")+
  xlab("Road Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

powerline_poly_randangle_male<-ggplot(rand_angle_summary_males,aes(powerline_residence))+
  geom_histogram(binwidth=2,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_males$powerlines_poly_count),size=1)+
  annotate("text",x=sum(true_crossings_males$powerlines_poly_count)+1,y=1000*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_summary_males$powerline_residence,sum(true_crossings_males$powerlines_poly_count))),
           parse=TRUE,hjust=0,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,20),breaks=seq(0,20,5))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,1000),breaks=seq(0,1000,250))+
  ylab("Frequency")+
  xlab("Points in Powerline Clearing per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

road_res_randpoint_male<-ggplot(rand_point_summary_males,aes(road_residence))+
  geom_histogram(binwidth=25,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_males$road_residence),size=1)+
  annotate("text",x=sum(true_crossings_males$road_residence)+2,y=350*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_summary_males$road_residence,sum(true_crossings_males$road_residence))),
           parse=TRUE,hjust=0,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,300),breaks=seq(0,300,100))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,350),breaks=seq(0,350,50))+
  ylab("Frequency")+
  xlab("Points in Road Buffer per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

road_res_randangle_male<-ggplot(rand_angle_summary_males,aes(road_residence))+
  geom_histogram(binwidth=25,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_males$road_residence),size=1)+
  annotate("text",x=sum(true_crossings_males$road_residence)-2,y=450*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_summary_males$road_residence,sum(true_crossings_males$road_residence))),
           parse=TRUE,hjust=1,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,150),breaks=seq(0,150,50))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,450),breaks=seq(0,450,50))+
  ylab("Frequency")+
  xlab("Points in Road Buffer per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

#female plots
trail_randpoint_female<-ggplot(rand_point_summary_females,aes(trail_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_females$trail_crossing_count),size=1)+
  annotate("text",x=sum(true_crossings_females$trail_crossing_count)-2,y=200*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_summary_females$trail_crossings,sum(true_crossings_females$trail_crossing_count))),
           parse=TRUE,hjust=1,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,100),breaks=seq(0,100,25))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,200),breaks=seq(0,200,25))+
  ylab("Frequency")+
  xlab("Trail Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


road_randpoint_female<-ggplot(rand_point_summary_females,aes(road_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_females$road_count),size=1)+
  annotate("text",x=sum(true_crossings_females$road_count)+1,y=250*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_summary_females$road_crossings,sum(true_crossings_females$road_count))),
           parse=TRUE,hjust=0,size=4)+
  annotate("text",label="♀",x=100*0.92,y=250*0.85,size = 10, family = "Arial Unicode MS")+
#  scale_x_continuous(expand=c(0,0),limits=c(0,100),breaks=seq(0,100,20))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,250),breaks=seq(0,250,50))+
  ylab("Frequency")+
  xlab("Road Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


powerline_poly_randpoint_female<-ggplot(rand_point_summary_females,aes(powerline_residence))+
  geom_histogram(binwidth=2,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_females$powerlines_poly_count),size=1)+
  annotate("text",x=sum(true_crossings_females$powerlines_poly_count)+1,y=700*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_summary_females$powerline_residence,sum(true_crossings_females$powerlines_poly_count))),
           parse=TRUE,hjust=0,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,80),breaks=seq(0,80,20))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,700),breaks=seq(0,700,100))+
  ylab("Frequency")+
  xlab("Points in Powerline Clearing per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


trail_randangle_female<-ggplot(rand_angle_summary_females,aes(trail_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_females$trail_crossing_count),size=1)+
  annotate("text",x=sum(true_crossings_females$trail_crossing_count)-2,y=200*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_summary_females$trail_crossings,sum(true_crossings_females$trail_crossing_count))),
           parse=TRUE,hjust=1,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,75),breaks=seq(0,75,25))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,200),breaks=seq(0,200,50))+
  ylab("Frequency")+
  xlab("Trail Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))


road_randangle_female<-ggplot(rand_angle_summary_females,aes(road_crossings))+
  geom_histogram(binwidth=5,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_females$road_count),size=1)+
  annotate("text",x=sum(true_crossings_females$road_count)+1,y=150*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_summary_females$road_crossings,sum(true_crossings_females$road_count))),
           parse=TRUE,hjust=0,size=4)+
  annotate("text",label="♀",x=140*0.92,y=150*0.85,size = 10, family = "Arial Unicode MS")+
#  scale_x_continuous(expand=c(0,0),limits=c(0,140),breaks=seq(0,140,20))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,150),breaks=seq(0,150,50))+
  ylab("Frequency")+
  xlab("Road Crossings per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

powerline_poly_randangle_female<-ggplot(rand_angle_summary_females,aes(powerline_residence))+
  geom_histogram(binwidth=2,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_females$powerlines_poly_count),size=1)+
  annotate("text",x=sum(true_crossings_females$powerlines_poly_count)+1,y=300*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_summary_females$powerline_residence,sum(true_crossings_females$powerlines_poly_count))),
           parse=TRUE,hjust=0,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,80),breaks=seq(0,80,20))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,300),breaks=seq(0,300,50))+
  ylab("Frequency")+
  xlab("Points in Powerline Clearing per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

road_res_randpoint_female<-ggplot(rand_point_summary_females,aes(road_residence))+
  geom_histogram(binwidth=25,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_females$road_residence),size=1)+
  annotate("text",x=sum(true_crossings_females$road_residence)-2,y=350*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_point_summary_females$road_residence,sum(true_crossings_females$road_residence))),
           parse=TRUE,hjust=1,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,250),breaks=seq(0,250,50))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,350),breaks=seq(0,350,50))+
  ylab("Frequency")+
  xlab("Points in Road Buffer per Path")+
  theme_bw() + 
  theme(axis.title.x = element_text(family="serif", margin = margin(t = 10, r = 0, b = 0, l = 0), face = "bold", size=14, color="black"),
        axis.title.y = element_text(family="serif", margin = margin(t = 0, r = 5, b = 0, l = 0), face = "bold", size=14, color="black", vjust=1),
        axis.text.x = element_text(family="serif", face = "bold", size=12, color="black",  margin = margin(t=0, r=0, b=0, l=0)),
        axis.text.y = element_text(family="serif", face = "bold", size=12, color="black", margin = margin(t=0, r=0, b=0, l=0)),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank(),plot.margin=margin(t = 0.5, r = 0.5, b = 0, l = 0,"cm"))

road_res_randangle_female<-ggplot(rand_angle_summary_females,aes(road_residence))+
  geom_histogram(binwidth=25,colour="black",fill="grey",boundary=0)+
  geom_vline(xintercept=sum(true_crossings_females$road_residence),size=1)+
  annotate("text",x=sum(true_crossings_females$road_residence)+2,y=350*0.92,
           label=paste0("~italic(P[i])== ",ecdf_fun(rand_angle_summary_females$road_residence,sum(true_crossings_females$road_residence))),
           parse=TRUE,hjust=0,size=4)+
#  scale_x_continuous(expand=c(0,0),limits=c(0,250),breaks=seq(0,250,50))+
#  scale_y_continuous(expand=c(0,0),limits=c(0,350),breaks=seq(0,350,50))+
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
LOOCV_angle_final_id_female<-data.frame(id=character(length(unique(true_crossings_females$id))),
                                        trail_crossing_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        #                                stream_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        road_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        #                                 powerlines_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        powerlines_poly_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        road_residence_LOOCV=numeric(length(unique(true_crossings_females$id))))

#separate by sex and summarize by rep
rand_angle_crossings%>%
  subset(id %in% subset(acon_info, sex == "male")$id) %>%
  group_by(id, year, rep)%>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_angle_LOOCV_summary_males

rand_angle_crossings%>%
  subset(id %in% subset(acon_info, sex == "female")$id) %>%
  group_by(id, year, rep)%>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_angle_LOOCV_summary_females


count<-0
for(j in unique(true_crossings_females$id)){
  count<-count+1
  LOOCV<-subset(rand_angle_LOOCV_summary_females,paste0(id)!=j)
  LOOCV%>%
    group_by(rep)%>%
    summarize(trail_count=sum(trail_crossings),
              road_count=sum(road_crossings),
              powerlines_poly_count=sum(powerline_residence),
              road_residence=sum(road_residence))->temp_summary
  true_crossings_females%>%
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

LOOCV_angle_final_id_male<-data.frame(id=character(length(unique(true_crossings_males$id))),
                                      trail_crossing_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      #stream_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      road_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      #powerlines_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      powerlines_poly_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      road_residence_LOOCV=numeric(length(unique(true_crossings_males$id))))
count<-0
for(j in unique(true_crossings_males$id)){
  count<-count+1
  LOOCV<-subset(rand_angle_LOOCV_summary_males,paste0(id)!=paste0(j))
  LOOCV%>%
    group_by(rep)%>%
    summarize(trail_count=sum(trail_crossings),
              road_count=sum(road_crossings),
              powerlines_poly_count=sum(powerline_residence),
              road_residence=sum(road_residence))->temp_summary
  true_crossings_males%>%
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
#separate by sex and summarize by rep
rand_crossings%>%
  subset(id %in% subset(acon_info, sex == "male")$id) %>%
  group_by(id, year, rep) %>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_point_LOOCV_summary_males

rand_crossings%>%
  subset(id %in% subset(acon_info, sex == "female")$id) %>%
  group_by(id, year, rep)%>%
  summarize(road_crossings=sum(road_count),
            trail_crossings=sum(trail_crossing_count),
            powerline_residence=sum(powerlines_poly_count),
            road_residence=sum(road_residence))->rand_point_LOOCV_summary_females

#LOOCV with id-only
LOOCV_point_final_id_female<-data.frame(id=character(length(unique(true_crossings_females$id))),
                                        trail_crossing_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        #                                       stream_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        road_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        #                                        powerlines_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        powerlines_poly_LOOCV=numeric(length(unique(true_crossings_females$id))),
                                        road_residence_LOOCV=numeric(length(unique(true_crossings_females$id))))


count<-0
for(j in unique(true_crossings_females$id)){
  count<-count+1
  LOOCV<-subset(rand_point_LOOCV_summary_females,paste0(id)!=j)
  LOOCV%>%
    group_by(rep)%>%
    summarize(trail_count=sum(trail_crossings),
              road_count=sum(road_crossings),
              powerlines_poly_count=sum(powerline_residence),
              road_residence=sum(road_residence))->temp_summary
  true_crossings_females%>%
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

LOOCV_point_final_id_male<-data.frame(id=character(length(unique(true_crossings_males$id))),
                                      trail_crossing_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      #                                      stream_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      road_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      #                                      powerlines_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      powerlines_poly_LOOCV=numeric(length(unique(true_crossings_males$id))),
                                      road_residence_LOOCV=numeric(length(unique(true_crossings_males$id))))
count<-0
for(j in unique(true_crossings_males$id)){
  count<-count+1
  LOOCV<-subset(rand_point_LOOCV_summary_males,paste0(id)!=paste0(j))
  LOOCV %>%
    group_by(rep)%>%
    summarize(trail_count=sum(trail_crossings),
              road_count=sum(road_crossings),
              powerlines_poly_count=sum(powerline_residence),
              road_residence=sum(road_residence))->temp_summary
  true_crossings_males%>%
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
acon_ssf <- left_join(acon, acon_info %>% select(id, sex), by = "id")

#create track
track_all<-make_track(acon_ssf,x,y,date_time,id=id)

#format year
acon_ssf$year_id<-paste0(acon_ssf$year,acon$id)
acon_ssf$x1<-acon_ssf$x+rnorm(length(acon$x),0,0.1)
acon_ssf$y1<-acon_ssf$y+rnorm(length(acon$y),0,0.1)

#make track
acon_male<-subset(acon_ssf,sex == "male")
track_male<-make_track(acon_male,x1,y1,date_time,id=year_id)
track_male_nest<-track_male|>nest(data=-"id")

acon_female<-subset(acon_ssf,sex == "female")
track_female<-make_track(acon_female,x1,y1,date_time,id=year_id)
track_female_nest<-track_female|>nest(data=-"id")

#create steps
track_male_nest_steps <- track_male_nest |> 
  mutate(steps = map(data, function(x) 
    x |> steps()))

track_female_nest_steps <- track_female_nest |> 
  mutate(steps = map(data, function(x) 
    x |> steps()))

#nested randoms
track_male_nest_steps |> unnest(cols = steps)|>random_steps(n_control=10)->track_male_nest_steps_rand
track_female_nest_steps |> unnest(cols = steps)|>random_steps(n_control=10)->track_female_nest_steps_rand

#remove NA values
tracks_ssf_male<-drop_na(track_male_nest_steps_rand,x2_,y2_)
tracks_ssf_female<-drop_na(track_female_nest_steps_rand,x2_,y2_)

#create columns
tracks_ssf_male$trail_crossing_count<-NA
tracks_ssf_male$road_count<-NA
tracks_ssf_male$powerlines_poly_count<-NA
tracks_ssf_male$road_residence_count<-NA

#determine whether true and random steps cross features
for(i in 1:length(tracks_ssf_female$id)){
  temp_line<-sfheaders::sf_linestring(obj=data.frame(x=c(tracks_ssf_female$x1_[i],tracks_ssf_female$x2_[i]),
                                                     y=c(tracks_ssf_female$y1_[i],tracks_ssf_female$y2_[i])),
                                      x="x",y="y")
  temp_point<-sfheaders::sf_point(obj=data.frame(x=tracks_ssf_female$x2_[i],y=tracks_ssf_female$y2_[i]),x="x",y="y")
  st_crs(temp_line)<-32615
  st_crs(temp_point)<-32615
  tracks_ssf_female$trail_crossing_count[i]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615))))) > 0, 
                                                    npts(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615)))), 0)
  tracks_ssf_female$road_count[i]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615))))) > 0, 
                                          npts(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615)))), 0)
  tracks_ssf_female$powerlines_residence_count[i]<-ifelse(lengths(st_intersects(temp_point,st_transform(powerlines_poly,crs=st_crs(32615))))>0,1,0)
  tracks_ssf_female$road_residence_count[i]<-ifelse(lengths(st_intersects(temp_point,st_transform(roads.poly,crs=st_crs(32615))))>0,1,0)
}

for(i in 1:length(tracks_ssf_male$id)){
  temp_line<-sfheaders::sf_linestring(obj=data.frame(x=c(tracks_ssf_male$x1_[i],tracks_ssf_male$x2_[i]),
                                                     y=c(tracks_ssf_male$y1_[i],tracks_ssf_male$y2_[i])),
                                      x="x",y="y")
  temp_point<-sfheaders::sf_point(obj=data.frame(x=tracks_ssf_male$x2_[i],y=tracks_ssf_male$y2_[i]),x="x",y="y")
  st_crs(temp_line)<-32615
  st_crs(temp_point)<-32615
  tracks_ssf_male$trail_crossing_count[i]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615))))) > 0, 
                                                  npts(st_intersection(temp_line,st_transform(trails,crs=st_crs(32615)))), 0)
  tracks_ssf_male$road_count[i]<-ifelse(length(st_geometry(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615))))) > 0, 
                                        npts(st_intersection(temp_line,st_transform(roads,crs=st_crs(32615)))), 0)
  tracks_ssf_male$powerlines_residence_count[i]<-ifelse(lengths(st_intersects(temp_point,st_transform(powerlines_poly,crs=st_crs(32615))))>0,1,0)
  tracks_ssf_male$road_residence_count[i]<-ifelse(lengths(st_intersects(temp_point,st_transform(roads.poly,crs=st_crs(32615))))>0,1,0)
}

#finalize
tracks_ssf_final_male<-subset(tracks_ssf_male,substr(id,5,6) %in% subset(acon_info, sex == "male")$id)

tracks_ssf_final_female<-subset(tracks_ssf_female, substr(id,5,6) %in% subset(acon_info, sex == "female")$id)

#male data prep for conditional logistic regression without random effects
d.map.male <- data.frame(case_=unique(tracks_ssf_final_male$step_id_),str_ID=1:length(unique(tracks_ssf_final_male$step_id_)))
tracks_ssf_final_male$str_ID<-d.map.male[match(tracks_ssf_final_male$step_id_,d.map.male$case_),"str_ID"]
tracks_ssf_final_male <- tracks_ssf_final_male[order(tracks_ssf_final_male$str_ID),]
tracks_ssf_final_male$used<-ifelse(tracks_ssf_final_male$case_==TRUE,1,0)
tracks_ssf_final_male$snakeid<-substr(tracks_ssf_final_male$id,1,2)
tracks_ssf_final_male$roads_bin<-ifelse(tracks_ssf_final_male$road_count>0,1,0)

#female data prep for conditional logistic regression without random effects
d.map.female <- data.frame(case_=unique(tracks_ssf_final_female$step_id_),str_ID=1:length(unique(tracks_ssf_final_female$step_id_)))
tracks_ssf_final_female$str_ID<-d.map.female[match(tracks_ssf_final_female$step_id_,d.map.female$case_),"str_ID"]
tracks_ssf_final_female <- tracks_ssf_final_female[order(tracks_ssf_final_female$str_ID),]
tracks_ssf_final_female$used<-ifelse(tracks_ssf_final_female$case_==TRUE,1,0)
tracks_ssf_final_female$snakeid<-substr(tracks_ssf_final_female$id,1,2)
tracks_ssf_final_female$roads_bin<-ifelse(tracks_ssf_final_female$road_count>0,1,0)

#binary mixed effects conditional logistic regression
#male model
tracks_ssf_final_male$trail_crossing_bin<-ifelse(tracks_ssf_final_male$trail_crossing_count>0,1,0)
tracks_ssf_final_male$powerlines_poly_bin<-ifelse(tracks_ssf_final_male$powerlines_residence_count>0,1,0)
tracks_ssf_final_male$road_bin<-ifelse(tracks_ssf_final_male$road_count>0,1,0)


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
tracks_ssf_final_female$trail_crossing_bin<-ifelse(tracks_ssf_final_female$trail_crossing_count>0,1,0)
tracks_ssf_final_female$powerlines_poly_bin<-ifelse(tracks_ssf_final_female$powerlines_residence_count>0,1,0)
tracks_ssf_final_female$road_bin<-ifelse(tracks_ssf_final_female$road_count>0,1,0)


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
