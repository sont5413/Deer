data=read.csv(file.choose())
data


library(lme4)
library(MuMIn)
library(lmerTest)
library(sjPlot)
library(ggeffects)
library(ggplot2)
library(plotrix)

life=lmer(antler.kg~bm.kg*Diet+I(bm.kg^2)*Diet+(1|ID)+(1|DID)+(1|SID),oalog)
summary(life)
confint(life,method=c("boot"),nsim=1000,oldNames=FALSE)
r.squaredGLMM(life)

pr <- ggpredict(life,terms = c("bm.kg [all]","Diet"),type = "fixed", back.transform = FALSE)

# customized plot:
plot(
  pr,
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
    plot.margin = margin(r=1,l=1,unit="cm"))

show_sjplot_pals()
options(legend.title.face)



