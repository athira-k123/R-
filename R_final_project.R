#SLEEP_HEALTH_AND_LIFESTYLE DATASET

Dataset<-read.csv("D:\\R_dataset\\Sleep_health_and_lifestyle_dataset.csv")
View(Dataset)
missing_values<-is.na(Dataset)
missing_count<-colSums(missing_values)
missing_count

SleepDisorder_mapping <- c("None" = 0, "Sleep Apnea" = 1, "Insomnia" = 2)
Dataset$new_Sleep.Disorder <- SleepDisorder_mapping[Dataset$Sleep.Disorder]
print(Dataset)

#class_count<-table(Dataset$new_Sleep.Duration)
#print(class_count)

----------------------------------------------------------------------------------
#VISUALIZATIONS

library(ggplot2)


#Quality of Sleep VS Sleep Duration
-----------------------------------
ggplot(Dataset, aes(x = Quality.of.Sleep, y = Sleep.Duration)) + 
geom_point() +
geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') + 
labs(x = "Quality of Sleep", y = "Sleep Duration") 

ggplot(Dataset, aes(x = Quality.of.Sleep, y = Sleep.Duration, color= Gender))  + 
geom_point() +
geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') + 
labs(x = "Quality of Sleep", y = "Sleep Duration")
 
#Quality of Sleep VS Stress Level 
---------------------------------
ggplot(Dataset, aes(x = Quality.of.Sleep, y = Stress.Level)) + 
geom_point() +
geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') + 
labs(x = "Quality of Sleep", y = "Stress Level") 

ggplot(Dataset, aes(x = Quality.of.Sleep, y = Stress.Level, color = Gender)) + 
geom_point() +
geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x') + 
labs(x = "Quality of Sleep", y = "Stress Level")

#Quality of Sleep VS Stress Level filling colour Occupation
-----------------------------------------------------------
ggplot(Dataset, aes(x = Quality.of.Sleep, y = Stress.Level, fill = Occupation)) +
geom_bar(stat = "identity", position = "dodge")  +
facet_wrap(~Occupation, scales = "free") + theme_minimal() +
labs(x = "Quality of Sleep", y = "Stress Level")

#Stress level VS Age with filling colour Occupation
---------------------------------------------------
ggplot(Dataset, aes(x = Stress.Level, y = Age, fill = Occupation)) +
geom_bar(stat = "identity", position = "dodge")  +
facet_wrap(~Occupation, scales = "free") + theme_minimal() +
labs(x = "Stress Level", y = "Age")

#BMI VS Quality Of Sleep
------------------------
ggplot(Dataset, aes(x = BMI.Category, y = Quality.of.Sleep, fill = Gender)) +
geom_bar(stat = "identity", position = "dodge") +
scale_fill_manual(values = c("Female" = "lightpink", "Male" = "darkslateblue")) +
labs(x = "BMI Category", y = "Quality of Sleep")

#Correlation Matrix
-------------------
Dataset<-subset(Dataset,select=-Person.ID)
library(corrplot)
correlation_matrix <- cor(Dataset[sapply(Dataset, is.numeric)])
corrplot(correlation_matrix,
         method = "color",  
         col = colorRampPalette(c("blue", "white", "red"))(100),
         tl.col = "black",  
         tl.srt = 50,  
         tl.cex = 0.7,
         addCoef.col = "black"  )

-----------------------------------------------------------------------------------------
#Removing the column
Dataset<-subset(Dataset,select=-Sleep.Disorder)
View(Dataset)

#MODEL BUILDING#

#Splitting into Test and Train
library(caTools)
split=sample.split(Dataset,SplitRatio=0.75)
train<-subset(Dataset, split=="TRUE")
test<-subset(Dataset, split=="FALSE")

# 1)Decision Tree

#str(train)

library(party)
dt_model<- ctree(new_Sleep.Disorder ~ Sleep.Duration+Quality.of.Sleep+Physical.Activity.Level+Stress.Level+Heart.Rate+Daily.Steps, train)
plot(dt_model)
predict_model<-predict(dt_model, test)
mat <- table(test$new_Sleep.Disorder, predict_model)
class <-ifelse(predict_model >0.5,1,0)
mean(test$new_Sleep.Disorder==class)

----------------------------------------
  
# 2)RANDOM FOREST
set.seed(120)
library(randomForest)
rf_model<-randomForest(x=train[-13],
                       y=train$new_Sleep.Disorder,
                       ntree=500)
rf_model
y_pred = predict(rf_model, newdata = test[-13])
mat <-table(test$new_Sleep.Disorder ,y_pred)
predicted_class <-ifelse(y_pred >0.5,1,0)
mean(test$new_Sleep.Disorder==predicted_class)

mse <- mean((y_pred - test$new_Sleep.Disorder)^2)
rsquared <- 1 - (mse / var(test$new_Sleep.Disorder))

print(paste("Mean Squared Error (MSE):", mse))
print(paste("R-squared (R2):", rsquared))

-------------------------------------------------------
-------------------------------------------------------
confusion_mtx = table(test[, 13], y_pred)
confusion_mtx
plot(rf_model)
importance(rf_model)

varImpPlot(rf_model)



