library(foreach)
?`foreach-package`
library(data.table)
data1<- read.csv("data.csv")
df<- setDT(data1)
names(df)
df[,game_event_id :=NULL]
df[,team_id :=NULL]
df[,game_id :=NULL]
df[,team_name :=NULL]
df[,shot_made_flag:=as.factor(shot_made_flag)]
df[,opponent:=NULL]
df[,shot_id:=NULL]
str(df)
df[matchup%like%"@" , matchup:="Away"]
df[matchup%like%"vs." , matchup:="Home"]
df[,season_num:= as.numeric(season)]
df[,game_date :=as.Date(game_date)]
names(df)
train<- df[!is.na(shot_made_flag), ]
test<- df[is.na(shot_made_flag), ]
regular <- train[playoffs == 0]
playoffs <- train[playoffs == 1]

#court photo to get a actual feel
install.packages("jpeg")
library(grid)
library(jpeg)
courtimg <- readJPEG('lakers.jpg')
courtimg <- rasterGrob(courtimg, width=unit(1,"npc"), height=unit(1,"npc"))


plot_kobe <- function(data, cat, alpha = 1){
  p <- ggplot(data, aes(data[, loc_x], data[, loc_y], color = data[[cat]]))+
    annotation_custom(courtimg, -300, 300, -115, 900)+
    geom_point(alpha = alpha)+
    theme_bw()+
    ylim(-80, 400)+
    xlim(-280, 280)+
    xlab('X')+
    ylab('Y')+
    scale_color_manual('Shots', values = c('#e61919', '#009999'))
  return(p)
}


ggplot(train[action_type == 'Jump Shot'], aes(color = shot_made_flag)) +
  annotation_custom(courtimg, -300, 300, -115, 900)+
  stat_density2d(geom = 'polygon', contour = T, n = 500, aes(x = loc_x, y = loc_y, 
                                                             fill = ..level.., alpha = ..level..))+
  theme_bw()+
  ylim(-80, 400)+
  xlim(-280, 280)+
  xlab('X')+
  ylab('Y')+
  scale_fill_gradient('Density', low = '#b3d1ff', high = '#003d99')+
  scale_color_manual('Shots', values = c('#ff0000', '#00ffcc'))+
  facet_wrap(~shot_made_flag)
library(ggplot2)




plot_kobe(train[minutes_remaining<=1 & period==4 & seconds_remaining<=24] , cat='shot_made_flag')


table(train$shot_type)
table(train$seconds_remaining)

percentage<- function(data , type=0){
  if(type==0){
    shots<- as.numeric(as.character(data[,shot_made_flag]))
  }else if(type==2){
    shots<- as.numeric(as.character(data[shot_type=='2PT Field Goal ', shot_made_flag]))
  }else{
    shots<- as.numeric(as.character(data[shot_type=='3PT Field Goal', shot_made_flag]))
  }
  percentages<- sum(shots)/length(shots)
  return(percentages)
}


table(train$minutes_remaining)
shoot_percentage(train[minutes_remaining<=1 & period==4 & seconds_remaining<=24] ,2)
table(train$shot_made_flag)



shoot_percentage <- function(data, type = 0){
  if (type == 0){
    shots <- as.numeric(as.character(data[, shot_made_flag]))
  } else if (type == 2){
    shots <- as.numeric(as.character(data[shot_type == '2PT Field Goal'
                                          , shot_made_flag]))
  } else {
    shots <- as.numeric(as.character(data[shot_type == '3PT Field Goal'
                                          , shot_made_flag]))
  }
  percentage <- sum(shots)/length(shots)
  return(percentage)
}
shoot_percentage(df[minutes_remaining<=1 & period==4 & seconds_remaining<=24 & action_type=="Jump"],2)
shoot_percentage(train[minutes_remaining<=1 & period==4 & seconds_remaining<=24 & matchup=="Home"],2)



#shot percentage by factor

train$per<- shoot_percentage(train[minutes_remaining<=1 & seconds_remaining<=24 &  period==1],2)
train[,se:=NULL]
table(df$shot_zone_area)
df[shot_zone_area%like%"Left" , shot_zone_area:="Left"]
df[shot_zone_area%like%"Right" , shot_zone_area:="Right"]
library(dplyr)
c1 <- function(feat) {
  feat <- substitute(feat)
  df %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +
    theme_void() +
    ggtitle(paste(feat))
}

c1(shot_zone_area)
df[shot_zone_area%like%"Back" , shot_zone_area:="Back Court"]
df[shot_zone_area%like%"Center" , shot_zone_area:="Center"]
c1(shot_zone_basic)

str(df)
table(df$action_type)
names(as.character((df$action_type)))

trainDataset<-dataset[complete.cases(dataset),]
temp<- as.character( sapply(strsplit(df$action_type," "),function(x) if(length(x)>=3) paste(x[1])
                            else "0"))

df$action_type<- droplevels(df$action_type)
#action type
for(i in c("Alley","Cutting","Driving","Dunk","Hook","Fadeaway","Finger","Floating","Follow","Jump","Layup","Pullup" , "Putback" , "Reverse" , "Running" , "Slam", "Step" , "Tip" , "Turnaround")) df[action_type%like% "i" , action_type:="i"]  <- grepl(i,df$action_type)
df[action_type%like%"Alley" , action_type:="Alley"]
df[action_type%like%"Cutting" , action_type:="Cutting"]
df[action_type%like%"Driving" , action_type:="Driving"]
df[action_type%like%"Dunk" , action_type :="Dunk"]
df[action_type%like%"Hook" , action_type := "Hook"]
df[action_type%like%"Fadeaway" , action_type :="Fadeaway"]
df[action_type%like%"Finger" , action_type := "Finger"]
df[action_type%like%"Floating" , action_type:="Floating"]
df[action_type%like%"Follow" , action_type:="Follow"]
df[action_type%like%"Jump" , action_type:= "Jump"]
df[action_type%like%"Layup" , action_type:="Layup"]
df[action_type%like%"Pullup" , action_type:="Pullup"]
df[action_type%like%"Putback" , action_type:="Putback"]
df[action_type%like%"Reverse" , action_type:="Reverse"]
df[action_type%like%"Running" , action_type:="Running"]
df[action_type%like%"Slam" , action_type:="Slam"]
df[action_type%like%"Step" , action_type:="Step"]
df[action_type%like%"Tip" , action_type:="Tip"]
df[action_type%like%"Turnaround" , action_type:="Turnaround"]

df$action_type<- droplevels(df$action_type)
table(df$action_type)

install.packages("ire")


actiontype <- df %>%
  group_by(action_type) %>%
  summarise(number = n()
  )



ordered<-order(actiontype$number)
actiontype<- actiontype[ordered,]
a<-1:nrow(actiontype)
actiontype$action_type_enumerated_sort_enumerated<-a
s<-sum(actiontype$num_animals)  
actiontype$number<-NULL
df1<-full_join(df,actiontype,by="action_type")




mf<- meadf$minutes_remaining
name<- df$action_type
names_freq<- table(name)/length(name)
name<- cbind(name , names_freq)
name<- as.data.frame(name)
df1<- cbind(name,df1)



cor(train$loc_y , as.numeric(train$shot_distance))
df[,loc_y:=NULL]
df[,combined_shot_type:=NULL]
df[,opponent:=NULL]


