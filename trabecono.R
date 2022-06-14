rm(list=ls())
load.lib <- c("data.table","foreign","lmtest","stargazer","sandwich","car", 'dplyr', 'ggplot2', 'MASS', 'AER', 'RCurl', 'readr', 'usmap', 'scale', 'ggthemes')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


url1 = 'https://raw.githubusercontent.com/fellipemelhado/Trab-de-econo-1/main/base.csv'
df = read_csv(url(url1))

df = df %>% mutate(ln_pop = log(Pop), ln_pib = log(PIB_mi)) #N preciso ajustar o valor para 0
df = rename(df, state = Estado)
df$pol = ifelse(df$Sal_min == 7.25, 0, 1) #Se o estado tem politica de salario minimo = 1
summary(df)


#sera feito uma escadinha
form = as.formula('Desemprego_p ~ Sal_min')
form1 = as.formula('Desemprego_p ~ Sal_min + ln_pop')
form2 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib')
form3 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p')
form4 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p')
form5 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + maxbenefit')
form6 = as.formula('Desemprego_p ~ Sal_min + ln_pop + ln_pib + Em_p + urbano_p + maxbenefit + terceirog_p')



OLS = lm(form, df)
OLS1 = lm(form1, df)
OLS2 = lm(form2, df)
OLS3 = lm(form3, df)
OLS4 = lm(form4, df)
OLS5 = lm(form5, df)
OLS6 = lm(form6, df)
stargazer(OLS, OLS1, OLS2, OLS3, OLS4, OLS5, OLS6, title = "Resultados", align = TRUE, omit.stat = c("LL","ser","f"), no.space = TRUE)





rest1 = c('Sal_min', 'ln_pop', 'ln_pib', 'urbano_p', 'maxbenefit')
rest2 = c('Em_p')
a = linearHypothesis(OLS5, rest1, vcov. = hccm(OLS5, type = "hc0"))
b = linearHypothesis(OLS5, rest2, vcov. = hccm(OLS5, type = "hc0"))
a
b

stargazer(a,b, title = "Resultados", align = TRUE, omit.stat = c("LL","ser","f"), no.space = TRUE)

plot_usmap(data = df, values = "Desemprego_p", regions = 'states', color = "blue") + 
  scale_fill_continuous(low = "white", high = "green", name = "Porcentagme de desemprego", label = scales::comma) + 
  theme(legend.position = "right") +  theme_economist()


#Regressao d multiplicao: (i.e ver o "peso" de uma do PIBpc qnd eh urbano)
