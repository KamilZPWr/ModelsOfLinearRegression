#setwd("C:/Users/micha/Documents/GitHub/ModelsOfLinearRegression")
library(readxl)
data <- read_excel("data.xlsx")

data$PPG <- round(data$PTS / data$`Games Played`, 2)
data$MPG <- round(data$MIN / data$`Games Played`, 2)

#MODEL1 - Wpływ fizyczności i czasu gry na zdobywane punkty
#1 same zmienne ciagle 
########################################################################
model_physical_1 <- lm(PPG ~ Height + MPG + Weight + Age, data = data)
summary(model_physical_1)
AIC(model_physical_1)
logLik(model_physical_1) 

model_physical_2 <- lm(PPG ~ MPG + Weight + Age, data = data)
summary(model_physical_2)
#plot(model_physical_2)
AIC(model_physical_2)
logLik(model_physical_2) 

model_physical_3 <- lm(PPG ~ MPG + BMI + Age, data = data)
summary(model_physical_3)
AIC(model_physical_3)
logLik(model_physical_3)

########################################################
#2 dodatowe kategoryczne - Position
model_position <- lm(PPG ~ Pos + FG_p + ThreeP_p + FT_p + FGA, data = data) #jedna wyrzuca i dodaje jako stałą
summary(model_position)
AIC(model_position)
logLik(model_position)

#3 jeden z interakcjami - Birth_place  * tam gdzie interackja