static1.5_log=read.csv(file.choose())
static1.5_log

library(lme4)
library(MuMIn)
library(sjPlot)
library(lmerTest)
library(ggeffects)
library(ggplot2)

low1.5=static1.5[static1.5$Diet=="Low",]
stand1.5=static1.5[static1.5$Diet=="Standard",]

#Sample Size:
table(static1.5$Diet,static1.5$Litsm)

low1.5sing=low1.5[low1.5$Litsm=="Singleton",]
low1.5mult=low1.5[low1.5$Litsm=="Multiple",]
stand1.5sing=stand1.5[stand1.5$Litsm=="Singleton",]
stand1.5mult=stand1.5[stand1.5$Litsm=="Multiple",]

#Antler Mass Mean:
mean(low1.5sing$antler.kg);mean(low1.5mult$antler.kg)
mean(stand1.5sing$antler.kg);mean(stand1.5mult$antler.kg)

#Antler Mass Range:
#Low
summary(low1.5sing$antler.kg);summary(low1.5mult$antler.kg)
summary(stand1.5sing$antler.kg);summary(stand1.5mult$antler.kg)


#Body Mass Mean
mean(low1.5sing$bm.kg);mean(low1.5mult$bm.kg)
mean(stand1.5sing$bm.kg);mean(stand1.5mult$bm.kg)

#Body Mass Range:
summary(low1.5sing$bm.kg);summary(low1.5mult$bm.kg)
summary(stand1.5sing$bm.kg);summary(stand1.5mult$bm.kg)

sd(static1.5$bm.kg)


table(static1.5$Diet,static1.5$DID)

#linear mixed effect model

library(lme4)
library(MuMIn)
library(lmerTest)
library(ggeffects)
library(ggplot2)


fm1=lmer(log(antler.kg)~log(bm.kg)*Diet+log(bm.kg)*Lit5+log(bm.kg)+log(bm.kg)*DaB+(1|DID),REML=TRUE, static1.5)#orig 5 litter types

fm2=lmer(log(antler.kg)~log(bm.kg)*Diet+log(bm.kg)*Litstwtr+log(bm.kg)*DaB+(1|DID),REML=TRUE, static1.5)#single,twin,trip

fm3=lmer(log(antler.kg)~log(bm.kg)*Diet+log(bm.kg)*Litsm+log(bm.kg)*DaB+(1|DID),REML=TRUE, static1.5)#single,mult

fm4=lmer(log(antler.kg)~log(bm.kg)*Diet+log(bm.kg)*Litsm+(1|DID),REML=TRUE, static1.5)#single,mult, no DaB

fm5=lmer(log(antler.kg)~log(bm.kg)*Diet+log(bm.kg)*Litstwtr+(1|DID),REML=TRUE, static1.5)#single,twin,trip, no DaB

fm6=lmer(log(antler.kg)~log(bm.kg)*Diet+log(bm.kg)*Lit5+(1|DID),REML=TRUE, static1.5)#orig 5 litter types, no DaB

fm7=lmer(log(antler.kg)~log(bm.kg)*Diet+log(bm.kg)*DaB+(1|DID),REML=TRUE, static1.5)#DaB, no litter type

fm8=lmer(log(antler.kg)~log(bm.kg)*Diet+(1|DID),REML=TRUE, static1.5)#No DaB, no litter type



summary(fm4)
confint(fm4,method=c("boot"),nsim=1000,oldNames=FALSE)

BIC=model.sel(fm1,fm2,fm3,fm4,fm5,fm6,fm7,fm8,rank = "BIC")

BIC
summary(fm4)
r.squaredGLMM(fm4)

#FM4 WINNER!

fm4_log=lmer(antler.kg~bm.kg*Diet+bm.kg*Litsm+(1|DID),REML=TRUE, static1.5_log)#single,mult, no DaB
summary(fm4_log)
confint(fm4_log,method=c("boot"),nsim=1000,oldNames=FALSE)

plot.new()

ggpred1.5_log <- ggpredict(fm4_log,terms = c("bm.kg [all]","Diet","Litsm"), type = "fixed", back.transform = FALSE)
ggpredformula1.5 <- ggpredict(fm4,terms = ~log10(bm.kg)*Energy+log10(bm.kg)*Litsm, type = "fixed", back.transform = FALSE)
pred1.5 <- ggpredict(fm4)

plot(
  ggpred1.5_log,
  ci = TRUE,
  ci.style = c("ribbon"),
  add.data = FALSE,
  limit.range = TRUE,
  residuals = FALSE,
  residuals.line = FALSE,
  collapse.group = FALSE,
  colors = "bw",
  alpha = 0.15,
  dodge = 0.25,
  use.theme = FALSE,
  dot.alpha = 0.35,
  jitter = 0.2,
  log.y = FALSE,
  case = NULL,
  show.legend = TRUE,
  show.title = TRUE,
  show.x.title = TRUE,
  show.y.title = TRUE,
  dot.size = NULL,
  line.size = 1.5,
  connect.lines = TRUE,
  one.plot = TRUE,
  rawdata = FALSE,
  residuals.type = FALSE,
) +
  labs(
    x = "Log Body Mass (kg)",
    y = "Log Antler Mass (kg)",
    title = ""
  ) + 
  set_theme(
    base = theme_classic(), 
    legend.title.face = NULL,
    legend.title.color = "grey30",
    legend.inside = TRUE,
    legend.color = "grey40",      
    legend.pos = c(.9,.2),  
    axis.title.size = 5,
    axis.title.color = "grey30",
    axis.textsize = 2,
    axis.textcolor = "black",
    axis.line.size = 2,
    axis.linecolor = "black",
    legend.size = 4,
    legend.title.size = 5,
    panel.major.gridcol = "grey90",
    panel.major.linetype = 1
  ) +
  theme(
    legend.key.size = unit(5,"line"),
    strip.text.x = element_text(size = 40, margin = margin(t=0.30,b=0.30,unit = "cm")),
    strip.text = element_text(colour = 'grey30'),
    strip.background =element_rect(fill="grey92"),
    axis.title.y = element_text(margin = margin(r=0.5, unit = "cm")),
    axis.title.x = element_text(margin = margin(t=0.5, unit = "cm")),
    axis.text.x = element_text(margin = margin(t=0.25, unit = "cm")),
    axis.text.y = element_text(margin = margin(r=0.25, unit = "cm")),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(size = 1.25),
    panel.spacing.x = unit(4,"line"),
    plot.margin = margin(r=1,l=1,unit="cm"))
