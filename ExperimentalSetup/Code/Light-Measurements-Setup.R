library(readxl)

# Load data, change file path accordingly!
df <- read_excel("C:/R/research project/exp setup/light-measurements-tanks-2304.xlsx")

# Rename:
colnames(df) <- c("tank","point","intensity")

# Convert to factors
df$tank  <- factor(df$tank)
df$point <- factor(df$point)

# Test for differences between tanks
anova_between <- aov(intensity ~ tank, data = df)
summary(anova_between)
TukeyHSD(anova_between)