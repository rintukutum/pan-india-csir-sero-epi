rm(list=ls())
dir.create('./figures/joint/200321', showWarnings = FALSE)
roche.data <- read.csv(
  './data/2021-03-20/roche-data.csv',
  stringsAsFactors = FALSE
)
# 607
colnames(roche.data)[2:3] <- c('baseline','month3')
#roche.data$baseline[roche.data$baseline < 0] <- 0
#roche.data$month3[roche.data$month3 < 0] <- 0
roche.data$delta <- roche.data$month3 - roche.data$baseline
library(ggplot2)
library(ggpubr)
pdf('./figures/joint/200321/Figure-04-A-200321.pdf',width = 5,height = 5)
ggplot(roche.data,
       aes(x=log2(baseline),y=log2(month3))) +
  geom_abline(slope = 1,linetype='dashed') +
  annotate("rect", xmin = -4, xmax = 0, ymin = -4, ymax = 9,
           alpha = 0.08,fill='#c837abff') +
  annotate("rect", ymin = -4, ymax = 0, xmin = -4, xmax = 9,
           alpha = 0.08,fill='#0055d4ff') +
  geom_point(aes(size=abs(delta),fill=delta),shape=21,) +
  scale_fill_gradient2(
    name = "",
    midpoint = 0,
    low = '#0055d4ff',
    mid = 'white',
    high = '#c837abff') +

  scale_y_continuous(
    limits = c(-4,9),
    breaks = c(-4,-2,0,2,4,6,8),
    labels = c(-4,-2,0,2,4,6,8)
  ) +
  scale_x_continuous(
    limits = c(-4,9),
    breaks = c(-4,-2,0,2,4,6,8),
    labels = c(-4,-2,0,2,4,6,8)) +
  scale_size(name= "") +
  xlab('log2(Antibody) [baseline]') +
  ylab('log2(Antibody) [3 months]') +
  theme_pubr()
dev.off()


inhibition.data <- read.csv(
  './data/2021-03-20/antibody-neutra.csv',
  stringsAsFactors = FALSE
)
# 607
colnames(inhibition.data)[2:3] <- c('baseline','month3')

inhibition.data$month3[inhibition.data$month3 < 0] <- 0
# [1] -4.934597
# inhibition.data$baseline[inhibition.data$baseline < 0] <- 0
inhibition.data$delta <- inhibition.data$month3 - inhibition.data$baseline


pdf('./figures/joint/200321/Figure-04-B-200221.pdf',width = 5,height = 5)
ggplot(inhibition.data,
       aes(x=baseline,y=month3)) +
  annotate("rect", xmin = 0, xmax = 20, ymin = 0, ymax = 100,
           alpha = 0.08,fill='#2ca089ff') +
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 20,
           alpha = 0.08,fill='#aa0000ff') +
  geom_abline(intercept = 0, slope = 1,
              linetype='dashed') +
  geom_point(aes(size=abs(delta),fill=delta),shape=21) +
  geom_abline(slope = 1,linetype='dashed') +
  scale_fill_gradient2(
    #name = "\u0394 \n(3 months - baseline)",
    name = "",
    midpoint = 0,
    low = '#aa0000ff',
    mid = 'white',
    high = '#2ca089ff') +
  scale_size(name= "") +
  xlab('Neutralising antibody (%) [baseline]') +
  ylab('Neutralising antibody (%) [3 months]') +
  theme_pubr()
dev.off()
#-----------------
# 5/6 months data
rm(list=ls())
roche.data <- read.csv(
  './data/2021-03-20/roche-data-5-6-months-175-followup.csv',
  stringsAsFactors = FALSE
)
# 175
colnames(roche.data)[2:3] <- c('baseline','month6')
roche.data$delta <- roche.data$month6 - roche.data$baseline
library(ggplot2)
library(ggpubr)
dir.create('./figures/joint/200321',showWarnings = FALSE)
pdf('./figures/joint/200321//Figure-04-C-6months-175.pdf',width = 5,height = 5)
ggplot(roche.data,
       aes(x=log2(baseline),y=log2(month6))) +
  geom_abline(slope = 1,linetype='dashed') +
  annotate("rect", xmin = -4, xmax = 0, ymin = -4, ymax = 9,
           alpha = 0.08,fill='#c837abff') +
  annotate("rect", ymin = -4, ymax = 0, xmin = -4, xmax = 9,
           alpha = 0.08,fill='#0055d4ff') +
  geom_point(aes(size=abs(delta),fill=delta),shape=21,) +
  scale_fill_gradient2(
    name = "",
    midpoint = 0,
    low = '#0055d4ff',
    mid = 'white',
    high = '#c837abff') +

  scale_y_continuous(
    limits = c(-4,9),
    breaks = c(-4,-2,0,2,4,6,8),
    labels = c(-4,-2,0,2,4,6,8)
  ) +
  scale_x_continuous(
    limits = c(-4,9),
    breaks = c(-4,-2,0,2,4,6,8),
    labels = c(-4,-2,0,2,4,6,8)) +
  scale_size(name= "") +
  xlab('log2(Antibody) [baseline]') +
  ylab('log2(Antibody) [5/6 months]') +
  theme_pubr()
dev.off()

inhibition.data <- read.csv(
  './data/2021-03-20/antibody-neutra-5-6-months-175-followup.csv',
  stringsAsFactors = FALSE
)
# 175
colnames(inhibition.data)[2:3] <- c('baseline','month6')

inhibition.data$month6[inhibition.data$month6 < 0] <- 0
inhibition.data$baseline[inhibition.data$baseline < 0] <- 0
inhibition.data$delta <- inhibition.data$month6 - inhibition.data$baseline


pdf('./figures/joint/200321/Figure-04-D-6months-175.pdf',width = 5,height = 5)
ggplot(inhibition.data,
       aes(x=baseline,y=month6)) +
  annotate("rect", xmin = 0, xmax = 20, ymin = 0, ymax = 100,
           alpha = 0.08,fill='#2ca089ff') +
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 20,
           alpha = 0.08,fill='#aa0000ff') +
  geom_abline(intercept = 0, slope = 1,
              linetype='dashed') +
  geom_point(aes(size=abs(delta),fill=delta),shape=21) +
  geom_abline(slope = 1,linetype='dashed') +
  scale_fill_gradient2(
    #name = "\u0394 \n(3 months - baseline)",
    name = "",
    midpoint = 0,
    low = '#aa0000ff',
    mid = 'white',
    high = '#2ca089ff') +
  scale_size(name= "") +
  xlab('Neutralising antibody (%) [baseline]') +
  ylab('Neutralising antibody (%) [5/6 months]') +
  theme_pubr()
dev.off()
