setwd("C:/Users/micha/Documents/GitHub/ModelsOfLinearRegression")
library(readxl)
library(car)
data <- read_excel("data.xlsx")
data$PPG <- round(data$PTS / data$`Games Played`, 2)
data$MPG <- round(data$MIN / data$`Games Played`, 2)
data$FTAPG <- round(data$FTA / data$`Games Played`, 2)
data$FGAPG <- round(data$FGA / data$`Games Played`, 2)
data$ThreeAPG <- round(data$ThreePA / data$`Games Played`, 2)
data$Pos <- as.factor(data$Pos)  #czy to konieczne?
data$Birth_Place <- as.factor(data$Birth_Place)
data <- data[ , -which(names(data) %in% c("OREB","DREB","REB","AST","STL","TOV","BLK","PF","EFF","AST/TOV","STL/TOV", "Team"))]

ConstMeanMC <- function(res){# test stałej średniej residuów
  N <- 10000
  mean(
    sapply(1:N, function(...){  #losowe probki z danych
      t.test(sample(res, 50))$p.value < 0.05     
    })
  )
}
ConstVarMC <- function(res){ # test stałej wariancji residuów
  N <- 10000
  mean(
    sapply(1:N, function(...){
      var.test(sample(res, 50), sample(res, 50))$p.value < 0.05 
    })
  )
}

#MODEL1 same zmienne ciagle 
model_offense <- lm(PPG ~ MPG + FGAPG + FGp + ThreeAPG + ThreePp + FTAPG + FTp - 1, 
                    data = data)
summary(model_offense)
AIC(model_offense)
logLik(model_offense) 
shapiro.test(residuals(model_offense))
plot(model_offense)

ConstMeanMC(residuals(model_offense))
ConstVarMC(residuals(model_offense))

#MODEL 2 dodatowe kategoryczne - Position
model_position <- lm(PPG ~ Pos + MPG + Age - 1, data = data) 
summary(model_position)
AIC(model_position)
logLik(model_position)
shapiro.test(residuals(model_position))
plot(model_position)

ConstMeanMC(residuals(model_position))
ConstVarMC(residuals(model_position))


#MODEL 3 jeden z interakcjami - Birth_place 
model_origin <- lm(PPG ~  MPG * Birth_Place - 1, data = data)
summary(model_origin)
AIC(model_origin)
logLik(model_origin)
shapiro.test(residuals(model_origin))
plot(model_origin)

ConstMeanMC(residuals(model_origin))
ConstVarMC(residuals(model_origin))

###################################################################
#Dodatkowe modele
model_physical_4 <- lm(PPG ~ Height + MPG + Weight + Age - 1,
                       data = data)
summary(model_physical_4)
plot(model_physical_4)
AIC(model_physical_4)
logLik(model_physical_4) 
shapiro.test(residuals(model_physical_4))
shapiro.test(residuals(model_physical_4)[-c(64, 110, 195, 274, 302, 410, 434 )])

model_physical_2 <- lm(PPG ~ MPG + Weight + Height - 1, data = data)
summary(model_physical_2)
AIC(model_physical_2)
logLik(model_physical_2) 
shapiro.test(residuals(model_physical_2))

model_physical_3 <- lm(PPG ~ MPG + BMI + Age - 1, data = data)
summary(model_physical_3)
AIC(model_physical_3)
logLik(model_physical_3)
shapiro.test(residuals(model_physical_3))

################################################################################

Anova(model_physical_1, type = 2)
anova(model_physical_1, model_physical_2, model_physical_3, model_physical_4)
mydata <- data.frame(data$PPG,  data$MPG, data$Height, data$Weight)
cor(mydata)
plot(model_physical_4)
abline(h = c(-3,3), col = "blue")  #odstające
abline(v = 4/length(residuals(model_offense)) * 3, col = "black") #wysoka dźwignia
which(abs((residuals(model_offense) - mean(residuals(model_offense)))/
            sd(residuals(model_offense))) > 3)

###########################################
#Odrzucanie wartości odstających i o wysokiej dźwigni
dane <- data.frame(data)
temp1 <- c()
temp2 <- c()
while (TRUE){
  model11 <- lm(PPG ~ MPG + FGAPG + FGp + ThreeAPG + ThreePp + FTAPG + FTp - 1, 
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
  temp1 <- c(which(abs((residuals(model_offense) - mean(residuals(model_offense)))/sd(residuals(model11))) > 3))
  temp2 <- c(which(hatvalues(model11) > 7/length(residuals(model11)) * 3))
  if(length(temp2)==0 & length(temp1)==0){
    break
  }
}

############################### LOGARYTM

model_offense <- lm(PPG ~ MPG + FGAPG + FGp + ThreeAPG + ThreePp + FTAPG + FTp - 1, 
                    data = dane)
summary(model_offense)
shapiro.test(residuals(model_offense))
AIC(model_offense)
logLik(model_offense) 
plot(model_offense)

dane <- data
dane$PPG <- log(dane$PPG)
temp1 <- c(which(abs((residuals(model_offense) - mean(residuals(model_offense)))/sd(residuals(model_offense))) > 3))
temp2 <- c(which(hatvalues(model_offense) > 7/length(residuals(model_offense)) * 3))
dane <- dane[-c(temp1, temp2),]

############################### PIERWIASTEK
model_offense <- lm(PPG ~ MPG + FGAPG + FGp + ThreeAPG + ThreePp + FTAPG + FTp - 1, 
                    data = dane)
summary(model_offense)
shapiro.test(residuals(model_offense))
AIC(model_offense)
logLik(model_offense) 
plot(model_offense)

dane <- data
dane$PPG <- sqrt(dane$PPG)
temp1 <- c(which(abs((residuals(model_offense) - mean(residuals(model_offense)))/sd(residuals(model_offense))) > 3))
temp2 <- c(which(hatvalues(model_offense) > 7/length(residuals(model_offense)) * 3))
dane <- dane[-c(temp1, temp2),]


################################### KWADRAT
model_offense <- lm(PPG ~ MPG + FGAPG + FGp + ThreeAPG + ThreePp + FTAPG + FTp - 1, 
                    data = dane)
summary(model_offense)
shapiro.test(residuals(model_offense))
AIC(model_offense)
logLik(model_offense) 
plot(model_offense)

dane <- data
dane$PPG <- (dane$PPG)^2
temp1 <- c(which(abs((residuals(model_offense) - mean(residuals(model_offense)))/sd(residuals(model_offense))) > 3))
temp2 <- c(which(hatvalues(model_offense) > 7/length(residuals(model_offense)) * 3))
dane <- dane[-c(temp1, temp2),]
