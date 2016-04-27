data1<- read.csv("data.csv")
train<- data1[!is.na(data1$shot_made_flag), ]
test<- data1[is.na(data1$shot_made_flag), ]
train$shot_made_flag<- as.factor(train$shot_made_flag)
names(train)
train$shot_made_flag<- factor(train$shot_made_flag , levels = c("1" , "0"))
library(ggplot2)
library(dplyr)
#visualization of the accuracy of the player

p<- function(feat){
  feat<- substitute(feat)
  ggplot(train , aes_q(x=feat)) + geom_bar(aes(fill=shot_made_flag) , stat = "count" , position = "fill" )
  
  + scale_fill_brewer(palette = "Set1", direction = -1) +
    ggtitle(paste("accuracy by", feat))
  
}

table(train$seconds_remaining)

c1 <- function(feat) {
  feat <- substitute(feat)
  train %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +
    theme_void() +
    ggtitle(paste(feat))
}


names(train)
c1(combined_shot_type)


ggplot() + geom_point(data=filter(train , combined_shot_type== "Jump Shot") , aes(x=lon , y=lat ) , color = "grey", alpha = 0.3, size = 2) +
                        geom_point(data = filter(train, combined_shot_type != "Jump Shot"),
                                   aes(x = lon, y = lat, 
                                       color = combined_shot_type), alpha = 0.7, size = 3) +
                        ylim(c(33.7, 34.0883)) +
                        scale_color_brewer(palette = "Set1") +
                        theme_void() +
                        
                      ggtitle("Shot Types")
names(train)
c1(shot_zone_area)
c1(shot_zone_basic)
c1(shot_zone_range)




pplot(minutes_remaining)
pplot <- function(feat) {
  feat <- substitute(feat)
  ggplot(data = train, aes_q(x = feat)) +
    geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    ggtitle(paste("accuracy by", feat))
  
}
pplot(seconds_remaining)
pplot(period)
pplot(season) + coord_flip()
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)


pplot(shot_distance) + xlim(0, 60)
pplot(combined_shot_type)
pplot(opponent) + coord_flip()


train$time<- with(train , train$seconds_remaining/60 + train$minutes_remaining)
test$time<- with(test , test$seconds_remaining/60 + test$minutes_remaining)
pplot(time)  
pplot(period)
str(train)
table(train$period)
as.matrix(prop.table(train$period))

train$time<- with(train , train$time*train$period)
test$time<- with(test , test$time*test$period)

table(train$shot_type)

train$shot_type<- as.character(train$shot_type)
train$shot_type<- substr(train$shot_type , 1, 3)
train$shot_type<- as.factor(train$shot_type)
test$shot_type<- substr(test$shot_type , 1 , 3)
test$shot_type<- as.factor(test$shot_type)
table(test$shot_type)
str(train)
table(train$matchup)

str(train)
clean<- function(x){
  
}

train$season<- substr(train$season,1,4)
table(train$season)
test$season<- substr(test$season,1,4)
table(test$season)
str(train)
library(lubridate)

train$year<- year(train$game_date)
test$year<- year(test$game_date)
train$season<- as.numeric(train$season)
train$year1<- with(train , train$year-train$season)
table(train$year1)
test$season<- as.numeric(test$season)
test$year1<- with(test , test$year-test$season)
test$year1<- with(test , test$year1+1)
train$year1<- with(train , train$year1+1)
str(train)
train<-train[,-c(1,3,4,12,20,21,22,23,25)]
test<- test[,-c(1,3,4,12,20,21,22,23,25)]
table(train$shot_type)
table(train$combined_shot_type)
train$shot_type<- substr(train$shot_type , 1,1)
table(train$shot_type)
test$shot_type<- substr(test$shot_type , 1, 1)
table(train$shot_distance)
train$dis<- with(train , sqrt((train$loc_x)^2 + (train$loc_y)^2))
test$dis<- with(test,sqrt((test$loc_x)^2 + (test$loc_y)^2))
str(train)
train$dis1<- with(train , sqrt(train$lat*-1*train$lon))
test$dis1<- with(test , sqrt(test$lat*-1*test$lon))
str(train)
train<- train[,-c(1,3,4,5,6,7,8,9,12,13,20,21,22,23,24,25)]
test<- test[,-c(1,3,4,5,6 ,7,8,9,12,13,20,21,22,23,24,25)]
names(train)

train<- dummy.data.frame(train , names = c( 'combined_shot_type','shot_type', 'shot_zone_area' , 'shot_zone_basic' , 'shot_zone_range' , sep="_")  )
gbm1 <- gbm(
  shot_made_flag ~ .,
  data=train,
  distribution="bernoulli",
  shrinkage=0.05,
  n.trees=400,
  interaction.depth=6L,
  train.fraction=0.88,
  keep.data=FALSE,
  verbose=TRUE
)
library(gbm)
str(train)
train$shot_type<- as.factor(train$shot_type)
?gbm
str(train)
train<- train[,-30]
test<- test[,-30]
train$shot_made_flag<- as.character(train$shot_made_flag)
test$shot_made_flag<- as.character(test$shot_made_flag)
train$shot_made_flag<- as.numeric(train$shot_made_flag)
test$shot_made_flag<- as.numeric(test$shot_made_flag)
train$shot_type<- as.numeric(train$shot_type)
test$shot_type<- as.numeric(test$shot_type)
library(xgboost)
library(Matrix)
X <- sparse.model.matrix(shot_made_flag~., data = train)
str(train)
Y <- as.numeric(train$shot_made_flag) 
numclass <- range(Y)[2] + 1

# set the parameter 
params <- list("objective" = "multi:softprob",
               "eta" = .1,
               "max_depth" = 8,
               "eval_metric" = "mlogloss",
               "num_class" = numclass,
               "subsample" = .8)
Y<- train$shot_made_flag
# cross-validation
nround =200
set.seed(123)
bst.cv <-  xgb.cv(params = params, data = as.matrix(train[,-10]), label = Y, nfold = 10, 
                  nround = nround, verbose = T)



gbm1 <- gbm(
  shot_made_flag ~ .,
  data=train,
  distribution="bernoulli",
  shrinkage=0.05,
  n.trees=400,
  interaction.depth=6L,
  train.fraction=0.88,
  keep.data=FALSE,
  verbose=TRUE
)

str(test)
test$shot_type<- as.numeric(test$shot_type)
target<- train$shot_made_flag
set.seed(23)
model_xgb <- xgb.cv(data=as.matrix(train[,-10]), label=as.matrix(target), objective="multi:softprob", num_class=3, nfold=5, nrounds=130, eta=0.1, max_depth=6, subsample=0.9, colsample_bytree=0.9, min_child_weight=1, eval_metric="mlogloss", prediction=T)
set.seed(23)
model_xgb <- xgboost(as.matrix(train[,-10]), as.matrix(target), objective="multi:softprob", num_class=3, nrounds=130, eta=0.1, max_depth=6, subsample=0.9, colsample_bytree=0.9, min_child_weight=1, eval_metric='mlogloss')
print(xgb.importance(feature_names=colnames(train), model=model_xgb))
pred <- predict(bst.cv, as.matrix(test[,-10]))
pred_matrix <- as.data.frame(matrix(pred, nrow(test), 1, byrow=T))
submit <- as.data.frame(apply(pred_matrix))
names(pred_matrix) <- "shot_made_flag"
submit$shot_made_flag <- submit$shot_made_flag-0.5
pred_matrix$shot_id <- sample$shot_id
pred_matrix <- pred_matrix[,c("shot_id","shot_made_flag")]
write.csv(pred_matrix, "submit.csv", row.names = F)

sample<- read.csv("sample_submission.csv")




cv_error <- bst.cv$test.mlogloss.mean
tr_error <- bst.cv$train.mlogloss.mean
min <- which.min(cv_error)
print(paste(min, cv_error[min]))

x<- train[,-10]
z<- test[,-10]
str(test)

for(i in 1:100) 
  {
  print(paste('training model:', i))
  model <- xgboost(data = as.matrix(x), label = Y, params = params, nround = min)
  
  print(paste('applying prediction:', i))
  preds[[i]] <- predict(model, newdata = as.matrix(z))
} 

sample<- read.csv("sample_submission.csv")

result <- data.frame(sample$shot_id, result)
result<- result[,-1]
colnames(result) <- names(sample)
write.csv(result, file = "sub2.csv", row.names = F)
table(train$action_type)
pplot(loc_y)
names(train)
pplot(playoffs)
train$conference <- c("Eastern", "Eastern", "Western", "Eastern", "Eastern" ,"Western", "Eastern", "Eastern" , "Eastern" , "Western" , "Eastern", "Eastern", "Eastern", "Eastern" , "Western" , "Eastern", "Eastern", "Western", "Western", "Eastern", "Western" , "Eastern", "Eastern", "Western", "Western", "Western", "Western" , "Western", "Western", "Western", "Western", "Western", "Eastern" )
pplot(opponent) + 
cor(train$loc_y , train$shot_distance)
action<- summary(data1$action_type)
levels(data1$action_type)
plot(data$game_date , data$shot_id)
library(dplyr)
data2 = arrange(data1, game_date, period, desc(minutes_remaining), desc(seconds_remaining))
table(data2$shot_distance)
