if (!require("tidyverse")) install.packages("tidyverse")
if (!require("corrplot")) install.packages("corrplot")
if (!require("lmtest")) install.packages("lmtest")
if (!require("car")) install.packages("car")
library(tidyverse)
library(corrplot)
library(lmtest)
library(car)

# Učitavanje skupa podataka
data <- read.csv('data.csv', sep = ";", dec = ".", header = TRUE)

# Provjera strukture podataka
str(data)
head(data)

# Zamjena NA vrijednosti srednjim vrijednostima
data$`Weight`[is.na(data$`Weight`)] <- mean(data$`Weight`, na.rm = TRUE)
data$`Height`[is.na(data$`Height`)] <- mean(data$`Height`, na.rm = TRUE)

# a) Izračun BMI
data$BMI <- (data$`Weight` / (data$`Height`)^2) * 703

# b) Opisivanje varijabli
summary(data)

# Odabrati proizvoljnu varijablu za prikaz
plot(density(data$Weight))
hist(data$Weight)
boxplot(data$Weight)
plot(data$Weight, data$Height) # Usporedba dvaju varijabli

# b) Grafički prikaz
pairs(data)

# Izdvajanje samo numeričkih varijabli
numeric_data <- data[sapply(data, is.numeric)]

# c) Izračun matrice korelacija samo za numeričke varijable
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# c) Prikaz matrice korelacija
print(cor_matrix)

# c) Grafički prikaz matrice korelacija
corrplot(cor_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# d) Ispitivanje normalnosti samo za numeričke varijable
apply(numeric_data, 2, function(x) shapiro.test(x)$p.value)

# e) Formiranje nove varijable BMI_faktor
data$BMI_faktor <- cut(data$BMI, 
                       breaks = c(-Inf, 20, 25, 30, Inf), 
                       labels = c("Pothranjenost", "Idealna tezina", "Prekomjerna tjelesna masa", "Pretilost"))

# f) Lista varijabli za provođenje ANOVA
variables <- c("Percent.body.fat", "Neck", "Chest", "Abdomen.2", "Hip", "Thigh", "Knee", "Ankle", "Biceps", "Forearm", "Wrist")

# f) Provjera distribucije BMI faktora
table(data$BMI_faktor)

for (var in variables) {
  # Ispitivanje pretpostavaka za ANOVA
  print(paste("Testing for variable:", var))
  
  # Ako postoji više od jedne grupe u BMI faktoru za trenutnu varijablu
  if (length(unique(data[!is.na(data[[var]]) & !is.na(data$BMI_faktor),]$BMI_faktor)) > 1) {
    
    # Shapiro-Wilkov test za normalnost
    shapiro_p_value <- shapiro.test(data[[var]])$p.value
    print(paste("Shapiro-Wilk test p-value for", var, ":", shapiro_p_value))
    
    # Bartlettov test za homogenost varijanci
    bartlett_p_value <- bartlett.test(data[[var]] ~ data$BMI_faktor)$p.value
    print(paste("Bartlett's test p-value for", var, ":", bartlett_p_value))
    
    # Ako su pretpostavke zadovoljene, provodi se ANOVA
    if (shapiro_p_value > 0.05 && bartlett_p_value > 0.05) {
      # Izvođenje ANOVA
      anova_result <- aov(data[[var]] ~ data$BMI_faktor, data = data)
      print(summary(anova_result))
      
      # Post-hoc test ako je ANOVA značajna
      if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
        print(paste("ANOVA is significant for", var, ". Performing Tukey HSD test."))
        print(TukeyHSD(anova_result))
      }
      
      # Ako pretpostavke nisu zadovoljene, provodi se Kruskal-Wallisov test
    } else {
      print(paste("Assumptions not met for", var, ". Performing Kruskal-Wallis test."))
      print(kruskal.test(data[[var]], data$BMI_faktor))
    }
  }
}

# g) Linearna regresija
data$BMI_faktor <- as.factor(data$BMI_faktor)

# Definiranje modela regresije
model <- lm(`Percent.body.fat` ~ . - BMI, data = data)

# Prikaz rezultata modela regresije
summary(model)

# Izbor modela na temelju AIC
step_model <- step(model, direction = "both")
summary(step_model)

# Prikaz koeficijenata modela
coef(step_model)

# Dijagnostički grafovi
par(mfrow = c(2, 2)) # za prikaz više grafova na jednom prozoru
plot(step_model, which = 1:4) # Prikaz svih dijagnostičkih grafova

# Test normalnosti reziduala
shapiro.test(resid(step_model))

# Test homoskedastičnosti (pogledajte p-vrijednost)
bptest(step_model)

# Test autocorrelation (Durbin-Watson test)
dwtest(step_model)

# Multicollinearity test
vif(step_model) # Variance Inflation Factors
