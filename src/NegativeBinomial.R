###################################
### Author:Eduardo Clark
### Project: Homicides and Fútbol
### Date: September 2013
### For mediotiempo.com
###################################

##Use a Negative Binomial Regression
##NOTE TO SELF (original coeficients reflect models without PreDayHom control variable, lower but still very significant coefs are produced with new control set)
m1 <- glm.nb(Homicidios ~ 
               Partido +
               as.factor(Anio) +
               as.factor(Month) + 
               as.factor(Dia) +
               PreDayHom ,
               data = Panel1) ##Negative Binomial with all games included

m2 <- glm.nb(Homicidios ~ 
               PartidoCasa +
               PartidoVisita +
               as.factor(Anio) +
               as.factor(Month) + 
               as.factor(Dia) +
               PreDayHom,
               data = Panel1) ##Negative Binomial (divided between Home and Away Games)

m3 <- glm.nb(Homicidios ~ 
               Ganador +
               as.factor(Anio) +
               as.factor(Month) + 
               as.factor(Dia), 
               data = Panel1) ##Negative Binomial (divided by winner)

m4 <- glmmadmb(Homicidios ~ 
              PartidoCasa +
               PartidoVisita + 
               as.factor(Anio) +
               as.factor(Month) + 
               as.factor(Dia) ,
               family="nbinom",
               zeroInflation=TRUE,
               data = Panel1) ##Negative Binomial with all games included using a zero-inflation model

m4$coefficients <- m4$b
#m3 <- glm(Homicidios ~ Partido + Anio + Month + Dia, family = "poisson", data = Panel1)## Poisson

m <- (est <- cbind(Estimate = coef(m1), confint(m1))) 
est <- cbind(Estimate = coef(m1), confint(m1)) ## CI for coeffs
exp(est) ## Get the non-log CI for the coeffs

### Use Hurdle Model
#H1 <- zeroinfl(Homicidios ~  Anio + Month + Dia | Partido , data = Panel1, dist="negbin")
#qcc.overdispersion.test(Panel1$Homicidios,type="poisson")

e1 <- m1; e1$coefficients <- exp(e1$coefficients)
e2 <- m2; e2$coefficients <- exp(e2$coefficients)
e3 <- m3; e3$coefficients <- exp(e3$coefficients)

#m5 <- m1
#m5$coefficients <- m4$b

screenreg(list(m1,m2,m3) , single.row=TRUE ,
          custom.model.names=c("Homicidios", "Homicidios", "Homicidios"),
          omit.coef = "as.factor",
          reoder.coef=c(1,21:26,2:20), caption="Football Games and Homicides",
          custom.note="Notes: Estimated coefficients from a negative binomial regression model are reported. Standard errors are in parentheses. Although not shown, controls for day of the week, month, and year are included."
)
   #      +,
   #      custom.coef.names=c("Intercept", "Game (Home or Away)", "Home Game", "Away Game",
   #                           "Team Wins", "Team Ties", "Team Loses", rep("as.factor",21)))


#anova(m1, m4, test = "Chisq")

texreg(file="latex-plots/R1",list(m1,m2,m3) , single.row=TRUE ,
                custom.model.names=c("Homicidios", "Homicidios", "Homicidios"),
                omit.coef = "as.factor",
                reoder.coef=c(1,21:26,2:20), caption="Football Games and Homicides",
                custom.note="Notes: Estimated coefficients from a negative binomial regression model are reported. Standard errors are in parentheses. Although not shown, controls for day of the week, month, and year are included."
)

texreg(file="latex-plots/R2",list(e1,e2,e3) , single.row=TRUE ,
             custom.model.names=c("Homicidios", "Homicidios", "Homicidios"),
             omit.coef = "as.factor",
             reoder.coef=c(1,21:26,2:20), caption="Football Games and Homicides (Exponential coefficients shown)",
             custom.note="Notes: Estimated coefficients from a negative binomial regression model are reported. Standard errors are in parentheses. Although not shown, controls for day of the week, month, and year are included."
)


