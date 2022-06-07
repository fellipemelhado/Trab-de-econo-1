rm(list=ls())
load.lib <- c("data.table","foreign","lmtest","stargazer","sandwich","car", 'dplyr', 'sandwich', 'MASS', 'AER', 'RCurl', 'readr')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


url1 = 'https://raw.githubusercontent.com/fellipemelhado/Trab-de-econo-1/main/base.csv'
df = read_csv(url(url1))

df = df %>% mutate(ln_pop = log(Pop), ln_pib = log(PIB_mi), ln_pibpc = log(GDPpc)) #N preciso ajustar o valor para 0
df$pol = ifelse(df$Sal_min == 7.25, 0, 1) #Se o estado tem politica de salario minimo = 1
summary(df)

form1 = as.formula('Desemprego_p ~ pol + ln_pop + ln_pib + Em_p + urbano_p + ln_pibpc')
form2 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + ln_pibpc')

OLS1 = lm(form1, data = df)
coeftest(OLS1, vcov = vcovHC(OLS1, "HC0"))
summary(OLS1)

OLS2 = lm(form2, data = df)
coeftest(OLS2, vcov = vcovHC(OLS2, "HC0"))
summary(OLS2)




#Regressao d multiplicao: (i.e ver o "peso" de uma do PIBpc qnd eh urbano)