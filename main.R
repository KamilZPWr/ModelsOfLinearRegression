setwd("C:/Users/micha/Documents/GitHub/ModelsOfLinearRegression")
library(readxl)
library(car)
library(mclust)
data <- read_excel("data.xlsx")
data$PPG <- round(data$PTS / data$`Games Played`, 2)
data$MPG <- round(data$MIN / data$`Games Played`, 2)
data$FTAPG <- round(data$FTA / data$`Games Played`, 2)
data$FGAPG <- round(data$FGA / data$`Games Played`, 2)
data$ThreeAPG <- round(data$ThreePA / data$`Games Played`, 2)
data$Pos <- as.factor(data$Pos)
data$Birth_Place <- as.factor(data$Birth_Place)

N<-1000 #powtórzenia Monte Carlo
checkConstMean <- function(res){# test stałej średniej residuów
  mean(
    sapply(1:N, function(...){
      t.test(sample(res,500))$p.value<0.05     
    })
  )
}
checkConstVar <- function(res){ # test stałej wariancji residuów
  mean(
    sapply(1:N, function(...){
      var.test(sample(res,500), sample(res,500))$p.value<0.05 
    })
  )
}

#MODEL1 - Wpływ fizyczności i czasu gry na zdobywane punkty
#1 same zmienne ciagle 
########################################################################
model_physical_1 <- lm(PPG ~ Height + MPG + Weight + Age, data = data)
summary(model_physical_1)
AIC(model_physical_1)
logLik(model_physical_1) 
shapiro.test(residuals(model_physical_1))
plot(model_physical_1)

model_physical_4 <- lm(PPG ~ Height + MPG + Weight + Age - 1, #to tworzy nowe odstające :x
                       data = data)
summary(model_physical_4)
plot(model_physical_4)
AIC(model_physical_4)
logLik(model_physical_4) 
shapiro.test(residuals(model_physical_4))
shapiro.test(residuals(model_physical_4)[-c(64, 110, 195, 274, 302, 410, 434 )])

model_physical_2 <- lm(PPG ~ MPG + Weight + Age - 1, data = data)
summary(model_physical_2)
AIC(model_physical_2)
logLik(model_physical_2) 
#plot(model_physical_2)
shapiro.test(residuals(model_physical_2))

model_physical_3 <- lm(PPG ~ MPG + BMI + Age - 1, data = data)
summary(model_physical_3)
AIC(model_physical_3)
logLik(model_physical_3)
#plot(model_physical_3)
shapiro.test(residuals(model_physical_3))

model_offense <- lm(PPG ~ FGAPG + FGp + ThreeAPG + ThreePp + FTAPG + FTp - 1, 
                    data = data)
summary(model_offense)
AIC(model_offense)
logLik(model_offense) 
shapiro.test(residuals(model_offense))
plot(model_offense)
#temp1 <- c(which(abs(residuals(model11)/sd(residuals(model11))) >= 3))
#temp2<- c(which(hatvalues(model11) > 6/length(residuals(model11)) * 3))
#[-temp1,]


########################################################
#2 dodatowe kategoryczne - Position
model_position <- lm(PPG ~ Pos + MPG + Age - 1, data = data) #jedna wyrzuca i dodaje jako stałą
summary(model_position)
AIC(model_position)
logLik(model_position)
shapiro.test(residuals(model_position))

#3 jeden z interakcjami - Birth_place  * tam gdzie interackja
# gracze z US grają więcej minut, wiecej zaufania od trenerów
model_origin <- lm(PPG ~  MPG * Birth_Place - 1, data = data) #jedna wyrzuca i dodaje jako stałą
summary(model_origin) #AGE JAKO INTERAKCJA?
AIC(model_origin)
logLik(model_origin)
shapiro.test(residuals(model_origin))

Anova(model_physical_1, type = 2)
anova(model_physical_1, model_physical_2, model_physical_3, model_physical_4)
mydata <- data.frame(data$PPG,  data$MPG, data$Height, data$Weight)
cor(mydata)
plot(model_physical_4)
abline(h = c(-3,3), col = "blue")  #odstające
abline(v = 4/length(residuals(model_physical_4)) * 3, col = "red") #wysoka dźwignia
which(abs(residuals(model_physical_4)/sd(residuals(model_physical_4))) > 3)
#Które są odstające dla modelu 4

#t.test(residuals(model))
#cor(data$PPG, data$PPG*data$MPG) tak sie interakcje znajduje?

temp<-c()
dowywalenia <- c()
i<-0
dane <- data.frame(data)
while (i==0){
model_physical_4 <- lm(PPG ~ Height + MPG + Weight + Age - 1, #to tworzy nowe odstające :x
                       data = dane) 
if (shapiro.test(residuals(model_physical_4))$`p.value` > 0.05)  {
  print('SW')
  break
  }
temp <- c(which(abs(residuals(model_physical_4)/sd(residuals(model_physical_4))) > 3))
if ( length(temp) == 0) {
  print('Brak odstajacych') 
  break
}
dowywalenia <- c(dowywalenia, temp)
dane <- dane[-temp,]
}
shapiro.test(residuals(model_physical_4))
View(dane)

temp2 <- c(which(hatvalues(model_physical_4) > 4/length(residuals(model_physical_4)) * 3))
dane <- dane[-temp2,]

###############3

dane <- data.frame(data)
temp1 <- c()
temp2 <- c()
while (TRUE){
  model11 <- lm(PPG ~ FGAPG + FG_p + ThreeAPG + ThreeP_p + FTAPG + FT_p - 1, 
                data = dane)
  shapiro.test(residuals(model11))
  if (shapiro.test(residuals(model11))$`p.value` > 0.05) {
    print('SW')
    break}
  if(length(dane)==0){
    print('empty')
    break
  }
  if(length(temp1)==0){
    print('temp1 out')
  }else{
    dane <- dane[-temp1,]
  }
  
  if(length(temp2)==0){
    print('temp2 out')
  }else{
    dane <- dane[-temp2,]
  }
  temp1 <- c(which(abs(residuals(model11)/sd(residuals(model11))) > 3))
  temp2 <- c(which(hatvalues(model11) > 6/length(residuals(model11)) * 3))
  if(length(temp2)==0 & length(temp1)==0){
    break
  }
}
###########################
model_offense <- lm(PPG ~ FGAPG + FGp + ThreeAPG + ThreePp + FTAPG + FTp - 1, 
              data = data)
summary(model_offense)
AIC(model_offense)
logLik(model_offense) 
shapiro.test(residuals(model_offense))
plot(model_offense)
#temp1 <- c(which(abs(residuals(model11)/sd(residuals(model11))) >= 3))
#temp2<- c(which(hatvalues(model11) > 6/length(residuals(model11)) * 3))
#[-temp1,]