

# Ex 1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dades <- read.csv(file = 'HDI-Data.csv')
head(dades)

summary(dades)

dades$HDI_group <- factor(dades$HDI_group,
                          levels = c(1,2,3,4),
                          labels = c("Low", "Medium","High", "Very high"))
dades$GDI_group <- factor(dades$GDI_group,
                          levels = c(1,2,3,4,5),
                          labels = c("Very high", "High", "Medium","Low", "Very low"))

plot(dades$HDI_group, main="Human Development \n Index Group",
     col="lightblue", ylim=c(0,50)); grid()
plot(dades$GDI_group, main="Gender Development \n Index Group",
     col="lightblue", ylim=c(0,50)); grid()

hist(dades$MYS, main="Mean Years of Schooling", col="lightblue"); grid()
hist(dades$SWSP, main="Share of Women Sits in Parliament", col="lightblue"); grid()
hist(dades$MMR, main="Maternal Mortality Rate", col="lightblue"); grid()
hist(dades$ABR, main="Adolescent Birth Rate", col="lightblue"); grid()
hist(dades$GNI, main="National Income", col="lightblue"); grid()


# Ex 2

plot(dades$MYS, dades$GNI)

cor(dades$GNI, dades$MYS)
(ct <- cor.test(dades$GNI, dades$MYS))
ct$p.value


# Ex 3

x <- c(29.8, 30.1, 30.5, 30.6, 31.3, 31.7, 32.6, 33.1, 32.7, 32.8)
y <- c(327,   456,  509,  497,  596,  573,  661,  741,  809,  717)
cor.test(x, y)


# Ex 4

mod1<-lm(GNI~MYS, data=dades)
(smod1 <- summary(mod1))
smod1$r.squared

residus <- dades$GNI - mod1$fitted.values
mean(residus)
sum(residus)

sum(residus * dades$MYS)

plot(dades$MYS, dades$GNI, ylim = c(-10000, max(dades$GNI))); abline(mod1, col=2)
plot(dades$MYS, residus); abline(h=0, col=2)

predict(mod1, newdata = data.frame(MYS=9), interval = "prediction", level = 0.95)

cor.test(dades$GNI, dades$ABR)

mod2<-lm(GNI~ABR, data=dades)
(smod2 <- summary(mod2))

mod2<-lm(GNI~ABR, data=dades)
(smod2 <- summary(mod2))

mean(mod2$residuals)
sum(mod2$residuals)
sum(mod2$residuals * dades$ABR)




