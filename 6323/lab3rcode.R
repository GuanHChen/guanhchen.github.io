library(haven)
TEDS_2016 <- read_stata("https://github.com/datageneration/home/blob/master/DataProgramming/data/TEDS_2016.dta?raw=true")

regplot = function(x,y){
  fit = lm(y~x)
  plot(x,y)
  abline(fit, col = "red")
}

regplot(TEDS_2016$age, TEDS_2016$Tondu)

regplot(TEDS_2016$edu, TEDS_2016$Tondu)

regplot(TEDS_2016$income, TEDS_2016$Tondu)
