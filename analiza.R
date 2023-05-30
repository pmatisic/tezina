# Učitavanje potrebnih paketa
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("corrplot")) install.packages("corrplot")
library(tidyverse)
library(corrplot)

# Učitavanje dataseta
data <- read.csv('data.csv', sep = ";", dec = ".", header = TRUE)

# Provjera strukture podataka
str(data)
head(data)

# Zamjena NA vrijednosti srednjim vrijednostima
data$`Weight..lbs.`[is.na(data$`Weight..lbs.`)] <- mean(data$`Weight..lbs.`, na.rm = TRUE)
data$`Height..inches.`[is.na(data$`Height..inches.`)] <- mean(data$`Height..inches.`, na.rm = TRUE)

# Izračun BMI
data$BMI <- data$`Weight..lbs.` / (data$`Height..inches.` * 0.0254)^2 * 703

# Formiranje nove varijable BMI_faktor
data$BMI_faktor <- cut(data$BMI, 
                       breaks = c(-Inf, 20, 25, 30, Inf), 
                       labels = c("Pothranjenost", "Idealna tezina", "Prekomjerna tjelesna masa", "Pretilost"))

# Opisivanje varijabli
summary(data)

# Graficki prikaz
pairs(data)

# Izračun matrice korelacija
cor_matrix <- cor(data, use = "pairwise.complete.obs")

# Prikaz matrice korelacija
print(cor_matrix)

# Grafički prikaz matrice korelacija
corrplot(cor_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Ispitivanje normalnosti razdiobe
apply(data, 2, function(x) shapiro.test(x)$p.value)

# Ispitivanje normalnosti samo za numeričke varijable
numeric_vars <- sapply(data, is.numeric)
numeric_data <- data[, numeric_vars]
apply(numeric_data, 2, function(x) shapiro.test(x)$p.value)

# Lista varijabli za provođenje ANOVA
variables <- c("Percent.body.fat.from.Siri.s..1956..equation", "Neck.circumference", "Chest.circumference", "Abdomen.2.circumference", "Hip.circumference", "Thigh.circumference", "Knee.circumference", "Ankle.circumference", "Biceps..extended..circumference", "Forearm.circumference", "Wrist.circumference")

# Provjera distribucije BMI faktora
table(data$BMI_faktor)

for (var in variables) {
  # Ispitivanje pretpostavaka za ANOVA
  print(paste("Testing for variable:", var))
  
  # Normalnost
  print(paste("Shapiro-Wilk test p-value for", var, ":", shapiro.test(data[[var]])$p.value))
  
  # Ako postoji više od jedne grupe u BMI faktoru za trenutnu varijablu
  if (length(unique(data[!is.na(data[[var]]) & !is.na(data$BMI_faktor),]$BMI_faktor)) > 1) {
    # Homogenost varijanci
    print(paste("Bartlett's test p-value for", var, ":", bartlett.test(data[[var]] ~ data$BMI_faktor)$p.value))
  }
  
  # Izvođenje ANOVA
  anova_result <- aov(data[[var]] ~ data$BMI_faktor, data = data)
  print(summary(anova_result))
  
  # Post-hoc test ako je ANOVA značajna
  if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
    print(paste("ANOVA is significant for", var, ". Performing Tukey HSD test."))
    print(TukeyHSD(anova_result))
  }
}
