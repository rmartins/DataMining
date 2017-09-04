library(needs)
needs(lubridate,
      dplyr,
      tidyr,
      Boruta,
      ggplot2,
      gridExtra,
      caret,
      rpart.plot,
      caTools,
      doMC)
	  
	  
 data <- read.csv("../input/No-show-Issue-Comma-300k.csv", stringsAsFactors = FALSE)
 
 str(data)
 
 data$Gender <- factor(data$Gender, levels = c("M", "F"))
data$AppointmentRegistration <- ymd_hms(data$AppointmentRegistration)
data$ApointmentData <- ymd_hms(data$ApointmentData)
data$DayOfTheWeek <- factor(data$DayOfTheWeek, 
                            levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday" , 
                                     "Saturday", "Sunday"))
# tirando hífen dos nomes
data$Status <- factor(make.names(data$Status))
data$Diabetes <- as.logical(data$Diabetes)
data$Alcoolism <- as.logical(data$Alcoolism)
data$HiperTension <- as.logical(data$HiperTension)
data$Handcap <- as.logical(data$Handcap)
data$Smokes <- as.logical(data$Smokes)
data$Scholarship <- as.logical(data$Scholarship)
data$Tuberculosis <- as.logical(data$Tuberculosis)
data$Sms_Reminder <- as.logical(data$Sms_Reminder)

range(data$Age)

#verificando idades negativas
sum(data$Age<0)

data <- data[data$Age>0,]


data$AwaitingTime <- abs(data$AwaitingTime)


dup_rows <- duplicated(data)
dup_rows_num <- sum(dup_rows)
dup_rows_num

status_table <- table(data$Status)
status_table

ggplot(data, aes(x=Status, fill=Status)) + geom_bar()


g_Age_1 <- ggplot(data, aes(x=Age)) + geom_histogram(bins=40)
g_Age_2 <- ggplot(data, aes(x=Status, y=Age, col=Status)) + geom_boxplot()
grid.arrange(g_Age_1, g_Age_2,ncol=2, top='Distribuição Idade')

g_Gender_1 <- ggplot(data, aes(x=Gender, fill=Gender)) + geom_bar(position="dodge")
g_Gender_2 <- ggplot(data, aes(x=Gender, fill=Status)) + geom_bar(position="fill")
grid.arrange(g_Gender_1, g_Gender_2,ncol=2, top='Distribuicao Sexo')

ggplot(data, aes(x=Sms_Reminder, fill=Status)) + geom_bar(position="fill")

dif_time <- abs(floor(difftime(data$AppointmentRegistration, data$ApointmentData, units = "days")))
sum(dif_time != data$AwaitingTime)

g_AwaitingTime_1 <- ggplot(data, aes(x=Status, y=AwaitingTime, col=Status)) + geom_boxplot()
g_AwaitingTime_2 <- ggplot(data, aes(x=AwaitingTime, fill=Status)) + 
                                geom_density(alpha=0.30) + 
                                coord_cartesian(xlim=c(0, 100))

grid.arrange(g_AwaitingTime_1, g_AwaitingTime_2,ncol=2, top='Distriuicao Espera')

g_Diabetes <- ggplot(data, aes(x=Diabetes, fill=Status)) + geom_bar(position="fill")
g_Alcoolism <- ggplot(data, aes(x=Alcoolism, fill=Status)) + geom_bar(position="fill")
g_HiperTension <- ggplot(data, aes(x=HiperTension, fill=Status)) + geom_bar(position="fill")
g_Handcap <- ggplot(data, aes(x=Handcap, fill=Status)) + geom_bar(position="fill")
g_Smokes <- ggplot(data, aes(x=Smokes, fill=Status)) + geom_bar(position="fill")
g_Scholarship <- ggplot(data, aes(x=Scholarship, fill=Status)) + geom_bar(position="fill")
g_Tuberculosis <- ggplot(data, aes(x=Tuberculosis, fill=Status)) + geom_bar(position="fill")

g_binary <- c(g_Diabetes, g_Alcoolism, g_HiperTension, g_Handcap, g_Smokes,
              g_Scholarship, g_Tuberculosis)
grid.arrange(g_Diabetes, g_Alcoolism, g_HiperTension, g_Handcap, ncol=2, top='impacto saúde')
grid.arrange(g_Smokes, g_Scholarship, g_Tuberculosis, ncol=2, top='Impacto variavel saúde')

split_data <- createDataPartition(data$Status, p = 0.7, list = FALSE)
train_data <- data[split_data,]
test_data <- data[-split_data,]

fitControl <- trainControl(method = "cv", 
							number = 5,
							summaryFunction = twoClassSummary, 
							classProbs = TRUE )

fit_rpart <- train(Status~.-AppointmentRegistration-ApointmentData-Handcap-Tuberculosis, 
                   train_data, method = "rpart", metric = "ROC",trControl = fitControl)
				   
pred_rpart <- predict(fit_rpart, test_data)
confusionMatrix(pred_rpart, test_data$Status)				   
						
rpart.plot(fit_rpart$finalModel, type = 2, fallen.leaves = F, cex = 1, extra = 2)


