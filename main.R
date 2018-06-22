#setwd("C:/Users/micha/Documents/GitHub/ModelsOfLinearRegression")
library(readxl)
library(car)
data <- read_excel("data.xlsx")

data$PPG <- round(data$PTS / data$`Games Played`, 2)
data$MPG <- round(data$MIN / data$`Games Played`, 2)

#MODEL1 - Wpływ fizyczności i czasu gry na zdobywane punkty
#1 same zmienne ciagle 
########################################################################
model_physical_1 <- lm(PPG ~ Height + MPG + Weight + Age, data = data)
summary(model_physical_1)
#plot(model_physical_1)
AIC(model_physical_1)
logLik(model_physical_1) 
shapiro.test(residuals(model_physical_1))

model_physical_4 <- lm(PPG ~ Height + MPG + Weight + Age - 1, data = data)
summary(model_physical_4)
AIC(model_physical_4)
#plot(model_physical_4)
logLik(model_physical_4) 
shapiro.test(residuals(model_physical_4))

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


########################################################
#2 dodatowe kategoryczne - Position
model_position <- lm(PPG ~ Pos + MPG - 1, data = data) #jedna wyrzuca i dodaje jako stałą
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
#t.test(residuals(model))
#cor(data$PPG, data$PPG*data$MPG) tak sie interakcje znajduje?