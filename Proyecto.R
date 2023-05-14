setwd("C:/Users/ivanr/Master/clases/Mod/Proyecto")

mod0 <- brm(Biomass ~ 1, family = lognormal, iter = 3000, data = db, cores = 4)

mod1 <- brm(Biomass ~ Ozone + Aphid + Endophyte + ( 1 + Ozone | Year) + (0 + Aphid | Year) + ( 0 + Endophyte | Year), family = lognormal, iter = 3000, data = db, cores = 4)

mod1.b <- brm(Biomass ~ Ozone + Aphid + Endophyte , family = lognormal, iter = 3000, data = db, cores = 4)

mod2 <- brm(Biomass ~ Ozone + Aphid + Endophyte + Ozone:Aphid + Ozone:Endophyte + Aphid:Endophyte + ( 1 + Ozone | Year) + (0 + Aphid | Year) + ( 0 + Endophyte | Year) + ( 0 + Ozone:Aphid | Year) +  ( 0 + Ozone:Endophyte| Year) + (0 + Aphid:Endophyte | Year), family = lognormal, iter = 3000, data = db, cores = 4)

mod2.b <- brm(Biomass ~ Ozone + Aphid + Endophyte + Ozone:Aphid + Ozone:Endophyte + Aphid:Endophyte, family = lognormal, iter = 3000, data = db, cores = 4)

mod2.c <- brm(Biomass ~ Ozone + Aphid + Endophyte + Ozone:Aphid + Ozone:Endophyte + Aphid:Endophyte + ( 1 + Ozone | Year), family = lognormal, iter = 3000, data = db, cores = 4)

mod3 <- brm(Biomass ~ Ozone + (1 + Ozone | Year), family = lognormal, iter = 3000, data = db, cores = 4)

mod4 <- brm(Biomass ~ Endophyte + (1 + Endophyte | Year), family = lognormal, iter = 3000, data = db, cores = 4)

mod5 <- brm(Biomass ~ Ozone + Endophyte + Ozone:Endophyte + ( 1 + Ozone | Year) + ( 0 + Endophyte | Year) + ( 0 + Ozone:Endophyte | Year), family = lognormal, iter = 3000, data = db, cores = 4)

mod6 <- brm(Biomass ~ Ozone + Aphid + Ozone:Aphid + ( 1 + Ozone | Year) + (0 + Aphid | Year) +  (0 + Ozone:Aphid | Year), family = lognormal, iter = 3000, data = db, cores = 4)

load("mod0.RData")
load(file = "mod1.RData")
load(file = "mod1b.RData")
load(file = "mod2.RData")
load(file = "mod2b.RData")
load(file = "mod2c.RData")
load(file = "mod3.RData")
load(file = "mod4.RData")
load(file = "mod5.RData")
load(file = "mod6.RData")

loo0 <- loo(mod0)
loo1 <- loo(mod1)
loo1.b <- loo(mod1.b)
loo2 <- loo(mod2)
loo2.b <- loo(mod2.b)
loo2.c <- loo(mod2.c)
loo3 <- loo(mod3)
loo4 <- loo(mod4)
loo5 <- loo(mod5)
loo6 <- loo(mod6)

looics <- data.frame(looic = c(loo0$estimates[3, 1], loo1$estimates[3, 1], loo1.b$estimates[3, 1], loo2$estimates[3, 1], loo2.b$estimates[3, 1], loo2.c$estimates[3, 1], loo3$estimates[3, 1], loo4$estimates[3, 1], loo5$estimates[3, 1], loo6$estimates[3, 1]), se = c(loo0$estimates[3, 2], loo1$estimates[3, 2], loo1.b$estimates[3, 2], loo2$estimates[3, 2], loo2.b$estimates[3, 2], loo2.c$estimates[3, 2], loo3$estimates[3, 2], loo4$estimates[3, 2], loo5$estimates[3, 2], loo6$estimates[3, 2]), modelo = c("mod0", "mod1", "mod1b", "mod2", "mod2b", "mod2c", "mod3", "mod4", "mod5", "mod6"))




p <- ggplot(data = looics, aes(x = reorder(modelo, looic), y = looic)) +
  labs(x = "Modelo", y = "LOOIC")+
  geom_point() +
  geom_errorbar(aes(ymin= looic - 2*se, ymax = looic + 2*se), width= 0.2) +
  theme(text = element_text(size = 18))

looics$w <- exp(-0.5*(looics$looic - min(looics$looic)))/sum(exp(-0.5*(looics$looic - min(looics$looic))))

x.i.1 <- c(0, 1)
x.i.2 <- c(0, 1)
x.i.3 <- c(0, 1)
z.i.1.  <- fitted(mod1, newdata= data.frame(Ozone = 1, Aphid = 1, Endophyte = 1), re_formula = NA)
z.i.1.b <- fitted(mod1, newdata= data.frame(Ozone = 1, Aphid = 1, Endophyte = 1), re_formula = NA)