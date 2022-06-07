rm(list=ls())
load.lib <- c("data.table","foreign","lmtest","stargazer","sandwich","car", 'dplyr', 'ggplot2', 'MASS', 'AER', 'RCurl', 'readr')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


url1 = 'https://raw.githubusercontent.com/fellipemelhado/Trab-de-econo-1/main/base.csv'
df = read_csv(url(url1))

df = df %>% mutate(ln_pop = log(Pop), ln_pib = log(PIB_mi), ln_pibpc = log(GDPpc)) #N preciso ajustar o valor para 0
df$pol = ifelse(df$Sal_min == 7.25, 0, 1) #Se o estado tem politica de salario minimo = 1
summary(df)


#sera feito uma escadinha
form = as.formula('Desemprego_p ~ Sal_min')
form1 = as.formula('Desemprego_p ~ Sal_min + ln_pop')
form2 = as.formula('Desemprego_p ~ Sal_min + Pop')
form3 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib')
form4 = as.formula('Desemprego_p ~ Sal_min + ln_pop + PIB_mi')
form5 = as.formula('Desemprego_p ~ Sal_min + Pop + ln_pib')
form6 = as.formula('Desemprego_p ~ Sal_min + Pop + PIB_mi')
form7 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p')
form8 = as.formula('Desemprego_p ~ Sal_min + ln_pop + PIB_mi + Em_p')
form9 = as.formula('Desemprego_p ~ Sal_min + Pop + ln_pib + Em_p')
form10 = as.formula('Desemprego_p ~ Sal_min + Pop + Pib_mi + Em_p')
form11 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p')
form12 = as.formula('Desemprego_p ~ Sal_min + ln_pop + PIB_mi + Em_p + urbano_p')
form13 = as.formula('Desemprego_p ~ Sal_min + Pop + ln_pib + Em_p + urbano_p')
form14 = as.formula('Desemprego_p ~ Sal_min + Pop + Pib_mi + Em_p + urbano_p')
form15 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + ln_pibpc')
form16 = as.formula('Desemprego_p ~ Sal_min + ln_pop + PIB_mi + Em_p + urbano_p + ln_pibpc')
form17 = as.formula('Desemprego_p ~ Sal_min + Pop + ln_pib + Em_p + urbano_p + ln_pibpc')
form18 = as.formula('Desemprego_p ~ Sal_min + Pop + Pib_mi + Em_p + urbano_p + ln_pibpc')
form19 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + GDPpc')
form20 = as.formula('Desemprego_p ~ Sal_min + ln_pop + PIB_mi + Em_p + urbano_p + GDPpc')
form21 = as.formula('Desemprego_p ~ Sal_min + Pop + ln_pib + Em_p + urbano_p + GDPpc')
form22 = as.formula('Desemprego_p ~ Sal_min + Pop + Pib_mi + Em_p + urbano_p + GDPpc')
form23 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + ln_pibpc + maxbenefit')
form24 = as.formula('Desemprego_p ~ Sal_min + ln_pop + PIB_mi + Em_p + urbano_p + ln_pibpc + maxbenefit')
form25 = as.formula('Desemprego_p ~ Sal_min + Pop + ln_pib + Em_p + urbano_p + ln_pibpc + maxbenefit')
form26 = as.formula('Desemprego_p ~ Sal_min + Pop + Pib_mi + Em_p + urbano_p + ln_pibpc + maxbenefit')
form27 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + GDPpc + maxbenefit')
form28 = as.formula('Desemprego_p ~ Sal_min + ln_pop + PIB_mi + Em_p + urbano_p + GDPpc + maxbenefit')
form29 = as.formula('Desemprego_p ~ Sal_min + Pop + ln_pib + Em_p + urbano_p + GDPpc + maxbenefit')
form30 = as.formula('Desemprego_p ~ Sal_min + Pop + Pib_mi + Em_p + urbano_p + GDPpc + maxbenefit')




#form2 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + ln_pibpc + maxbenefit ')
#form3 = as.formula('Desemprego_p ~ pol + ln_pop + ln_pib + Em_p + urbano_p + ln_pibpc + maxbenefit ')

OLS = lm(form, df)
Ols1 = lm()




OLS1 = lm(form1, data = df)
coeftest(OLS1, vcov = vcovHC(OLS1, "HC0"))
summary(OLS1)

OLS2 = lm(form2, data = df)
coeftest(OLS2, vcov = vcovHC(OLS2, "HC0"))
summary(OLS2)

cor(df$Desemprego_p, df$maxbenefit)
ggplot(df, aes(PIB_mi, Desemprego_p)) + geom_point() + geom_smooth(method = 'lm')
cor(df$PIB_mi, df$GDPpc)


#Regressao d multiplicao: (i.e ver o "peso" de uma do PIBpc qnd eh urbano)
