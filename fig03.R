rm(list=ls())
combine.data <- read.csv(
  './data/261220/merged-data-261220.csv',
  stringsAsFactors = FALSE
)
colnames(combine.data)[7] <- 'ModeTransport'
combine.data$ModeTransport[combine.data$ModeTransport == 'public'] <- 'Public'

combine.data[combine.data == 'SU'] <- NA

model.data <- dplyr::mutate_if(combine.data, is.character, as.factor)
model.data$Diet <- factor(model.data$Diet,levels = c('V','NV'))
model.data$Smoking <- factor(model.data$Smoking,levels = c('Yes','No'))
model.data$Occupation <- factor(model.data$Occupation,levels = c('S','OS'))

bothM.occupation <- glm(Sero.Status~Occupation,
                        data = na.omit(model.data[,c('Sero.Status','Occupation')]),
                        family = binomial(link = "logit"))

bothM.diet <- glm(Sero.Status~Diet,
                  data = na.omit(model.data[,c('Sero.Status','Diet')]),
                  family = binomial(link = "logit"))
bothM.smoking <- glm(Sero.Status~Smoking,
                     data = na.omit(model.data[,c('Sero.Status','Smoking')]),
                     family = binomial(link = "logit"))
bothM.modT <- glm(Sero.Status~ModeTransport,
                  data = na.omit(model.data[,c('Sero.Status','ModeTransport')]),
                  family = binomial(link = "logit"))
bothM.gender <- glm(Sero.Status~Gender,
                    data = na.omit(model.data[,c('Sero.Status','Gender')]),
                    family = binomial(link = "logit"))

mod.info.new <- function(x){
  xx.sum <- summary(x)
  c.conf <- stats::confint(x)[-1,]
  df_ <- data.frame(
    OR = exp(coef(x)[-1]),
    min.conf = exp(c.conf[1]),
    max.conf = exp(c.conf[2]),
    VIF = faraway::vif(x),
    pval = xx.sum$coefficients[-1,4]
  )
  df_$var <- rownames(df_)
  df_
}

bothM.OR <- rbind(
  mod.info.new(bothM.occupation),
  mod.info.new(bothM.diet),
  mod.info.new(bothM.smoking),
  mod.info.new(bothM.modT),
  mod.info.new(bothM.gender)
)
rm(model.data)
female.data <- read.csv(
  './data/261220/female-data-261220.csv',
  stringsAsFactors = FALSE
)
colnames(female.data)[6] <- 'ModeTransport'
female.data$ModeTransport[female.data$ModeTransport == 'public'] <- 'Public'

female.data[female.data == 'SU'] <- NA
model.data <- dplyr::mutate_if(female.data, is.character, as.factor)
model.data$Occupation <- factor(model.data$Occupation,levels = c('S','OS'))

female.occupation <- glm(Sero.Status~Occupation,
                         data = na.omit(model.data[,c('Sero.Status','Occupation')]),
                         family = binomial(link = "logit"))
female.modT <- glm(Sero.Status~ModeTransport,
                   data = na.omit(model.data[,c('Sero.Status','ModeTransport')]),
                   family = binomial(link = "logit"))

female.OR <- rbind(
  mod.info.new(female.occupation),
  mod.info.new(female.modT)
)
rm(model.data)
male.data <- read.csv(
  './data/261220/male-data-261220.csv',
  stringsAsFactors = FALSE
)
colnames(male.data)[6] <- 'ModeTransport'
male.data$ModeTransport[male.data$ModeTransport == 'public'] <- 'Public'

male.data[male.data == 'SU'] <- NA

# "Occupation", "ModeTrasport" , "Smoking", "Diet"
model.data <- dplyr::mutate_if(male.data, is.character, as.factor)
model.data$Diet <- factor(model.data$Diet,levels = c('V','NV'))
model.data$Smoking <- factor(model.data$Smoking,levels = c('Yes','No'))
model.data$Occupation <- factor(model.data$Occupation,levels = c('S','OS'))

male.occupation <- glm(Sero.Status~Occupation,
                       data = na.omit(model.data[,c('Sero.Status','Occupation')]),
                       family = binomial(link = "logit"))

male.diet <- glm(Sero.Status~Diet,
                 data = na.omit(model.data[,c('Sero.Status','Diet')]),
                 family = binomial(link = "logit"))
male.smoking <- glm(Sero.Status~Smoking,
                    data = na.omit(model.data[,c('Sero.Status','Smoking')]),
                    family = binomial(link = "logit"))
male.modT <- glm(Sero.Status~ModeTransport,
                 data = na.omit(model.data[,c('Sero.Status','ModeTransport')]),
                 family = binomial(link = "logit"))
male.OR <- rbind(
  mod.info.new(male.occupation),
  mod.info.new(male.diet),
  mod.info.new(male.smoking),
  mod.info.new(male.modT)
)
bothM.OR$group <- 'Complete'
female.OR$group <- 'Female'
male.OR$group <- 'Male'
OR.all.ind <- rbind(
  bothM.OR,
  female.OR,
  male.OR
)

save(OR.all.ind,
     file = './data/261220/H-OR-all-ind.RData')

rm(list=ls())
female.data <- read.csv(
  './data/261220/female-data-261220.csv',
  stringsAsFactors = FALSE
)
colnames(female.data)[6] <- 'ModeTransport'
female.data$ModeTransport[female.data$ModeTransport == 'public'] <- 'Public'

female.data[female.data == 'SU'] <- NA

colnames(female.data)[c(2,3,6)]
# [1] "Sero.Status"   "Occupation"    "ModeTransport"
female.data.noNA <- na.omit(female.data[,c(2,3,6)])

model.data <- dplyr::mutate_if(female.data.noNA, is.character, as.factor)
model.data$Occupation <- factor(model.data$Occupation,levels = c('S','OS'))
source('./func-room-final.R')
female.mods <- runModelC.female(model.data)
female.mods.summary <- summary.MODS(female.mods)
PLOTS <- summary.plots(female.mods.summary)

med.output <- plyr::ddply(
  female.mods.summary,
  'var',
  function(x){
    data.frame(
      OR = median(x$OR),
      min.conf = median(x$min.conf),
      max.conf = median(na.omit(x$max.conf))
    )
  }
)

med.output$model <- 'sampled'

f.OR <- med.output
f.plots <- PLOTS
save(female.mods.summary,f.plots,f.OR,
     file = './data/H-female.summary.RData')

rm(list=ls())
male.data <- read.csv(
  './data/261220/male-data-261220.csv',
  stringsAsFactors = FALSE
)
colnames(male.data)[6] <- 'ModeTransport'
male.data$ModeTransport[male.data$ModeTransport == 'public'] <- 'Public'

colnames(male.data)[c(2,5:8)]

male.data[male.data == 'SU'] <- NA

# "Occupation", "ModeTrasport" , "Smoking", "Diet"

male.data.noNA <- na.omit(male.data[,c(2,5:8)])

model.data <- dplyr::mutate_if(male.data.noNA, is.character, as.factor)
model.data$Diet <- factor(model.data$Diet,levels = c('V','NV'))
model.data$Smoking <- factor(model.data$Smoking,levels = c('Yes','No'))
model.data$Occupation <- factor(model.data$Occupation,levels = c('S','OS'))

source('./func-room-final.R')
male.mods <- runModelC.gender(model.data)
male.mods.summary <- summary.MODS(male.mods)

PLOTS <- summary.plots(male.mods.summary)

med.output <- plyr::ddply(
  male.mods.summary,
  'var',
  function(x){
    data.frame(
      OR = median(x$OR),
      min.conf = median(x$min.conf),
      max.conf = median(na.omit(x$max.conf))
    )
  }
)

med.output$model <- 'sampled'

m.OR <- med.output
m.plots <- PLOTS
save(male.mods.summary,m.plots,m.OR,
     file = './data/H-male.summary.RData')

rm(list=ls())
combine.data <- read.csv(
  './data/261220/merged-data-261220.csv',
  stringsAsFactors = FALSE
)
colnames(combine.data)[7] <- 'ModeTransport'
combine.data$ModeTransport[combine.data$ModeTransport == 'public'] <- 'Public'

combine.data[combine.data == 'SU'] <- NA

# "Occupation", "ModeTrasport" , "Smoking", "Diet"
colnames(combine.data)[c(2,3:4,7:9)]
combine.data.noNA <- na.omit(combine.data[,c(2,3:4,7:9)])

model.data <- dplyr::mutate_if(combine.data.noNA, is.character, as.factor)
model.data$Diet <- factor(model.data$Diet,levels = c('V','NV'))
model.data$Smoking <- factor(model.data$Smoking,levels = c('Yes','No'))
model.data$Occupation <- factor(model.data$Occupation,levels = c('S','OS'))

source('./func-room-final.R')
both.mods <- runModelC.add(model.data)
both.mods.summary <- summary.MODS(both.mods)

PLOTS <- summary.plots(both.mods.summary)

med.output <- plyr::ddply(
  both.mods.summary,
  'var',
  function(x){
    data.frame(
      OR = median(x$OR),
      min.conf = median(x$min.conf),
      max.conf = median(x$max.conf)
    )
  }
)

med.output$model <- 'sampled'
bothG.OR <- med.output
bothG.plots <- PLOTS
bothG.mods.summary <- both.mods.summary

save(bothG.mods.summary,bothG.plots,bothG.OR,
     file = './data/H-bothG.summary.RData')

#------------ FIGURE
rm(list=ls())
source('./func-room-final.R')
load('./data/F-bothG.summary.RData')
bothG.OR$group <- 'Complete'
load('./data/H-female.summary.RData')
f.OR$group <- 'Female'
load('./data/H-male.summary.RData')
m.OR$group <- 'Male'
OR <- rbind(bothG.OR,
            f.OR,
            m.OR)
OR$group <- factor(OR$group,levels = c('Complete','Male','Female'))
load('./data/261220/H-OR-all-ind.RData')

write.csv(OR.all.ind,
          row.names = FALSE,
          file = './data/261220/H-00-Odd-ratio-full-data-ind-261220.csv')
OR.all.ind$model = 'full'
OR <- rbind(OR,
            OR.all.ind[,colnames(OR)])
OR$group <- factor(OR$group,levels = rev(levels(OR$group)))
p.OR.v2 <- ggplot(aes(x = OR, xmin = min.conf, xmax = max.conf, y = var,
                      group = model,color= model),
                  data = OR) +
  geom_vline(xintercept = 1, color = 'grey50', linetype = 'dashed') +
  geom_errorbarh(height = 0.25, position = position_dodge(width = 0.55)) +
  geom_point(size = 2, position = position_dodge(width = 0.55)) +
  facet_grid(facets = group~.) +
  xlab('Odds ratio (95% CI)') +
  scale_y_discrete(limit=c('GenderMale',
                           "OccupationOS",
                           "ModeTransportPublic",
                           "SmokingNo",
                           "DietNV"),
                   labels= rev(c('Diet (nv)','Smoking (no)','Transport (public)',
                                 'Occupation (os)','Gender (male)'))) +
  ylab('') + theme_pubr()


pdf('./figures/joint/261220/Figure-03-A.pdf',width = 3.65,height = 5.5)
p.OR.v2
dev.off()
# Figure 03 B

bothG.mods.summary$pval <- -log10(bothG.mods.summary$pval)
m.both <- reshape::melt(bothG.mods.summary[,c(1,2,5,6,7)])
m.both$variable <- factor(m.both$variable,
                          levels = c('OR','pval','VIF'))

idx.pval <- as.character(m.both$variable) == 'pval'
ggplot(m.both[idx.pval,],
       aes(x=var,y=value)) +
  geom_boxplot(col='#00bfc4ff',fill='grey95',outlier.color = 'grey50') +
  geom_hline(yintercept = 1.3,color='red',linetype='dashed') +
  theme_pubr()
p1 <- ggplot(m.both,
             aes(x=var,y=value)) +
  geom_boxplot(col='#00bfc4ff',fill='grey95',outlier.color = 'grey50') +
  #xlab('Variable') +
  xlab('') +
  facet_wrap(facets = 'variable',scales = 'free_y') +
  scale_x_discrete(
    limit=rev(c('GenderMale',
                "OccupationOS",
                "ModeTransportPublic",
                "SmokingNo",
                "DietNV")),
    labels= c('Diet (nv)',
              'Smoking (no)',
              'Transport (public)',
              'Occupation (os)',
              'Gender (male)')
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(
      angle=60,vjust = 1,hjust = 1),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

male.mods.summary$pval <- -log10(male.mods.summary$pval)
m.m <- reshape::melt(male.mods.summary[,c(1,2,5,6,7)])
m.m$variable <- factor(
  m.m$variable,
  levels = c('OR','pval','VIF')
)
idx.pval <- as.character(m.m$variable) == 'pval'
ggplot(m.m[idx.pval,],
       aes(x=var,y=value)) +
  geom_boxplot(col='#00bfc4ff',fill='grey95',outlier.color = 'grey50') +
  geom_hline(yintercept = 1.3,color='red',linetype='dashed') +
  theme_pubr()

p2 <- ggplot(m.m,
             aes(x=var,y=value)) +
  geom_boxplot(col='#00bfc4ff',
               fill='grey95',
               outlier.colour = 'grey50'
  ) +
  xlab('') +
  facet_wrap(facets = 'variable',scales = 'free_y') +
  scale_x_discrete(limit=rev(c('GenderMale',
                               "OccupationOS",
                               "ModeTransportPublic",
                               "SmokingNo",
                               "DietNV")),
                   labels= c('','','',
                             '','')) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle=60,vjust = 1,hjust = 1),
        strip.background = element_blank(),strip.text = element_blank())



female.mods.summary$pval <- -log10(female.mods.summary$pval)
m.f <- reshape::melt(female.mods.summary[,c(1,2,5,6,7)])
m.f$variable <- factor(m.f$variable,
                       levels = c('OR','pval','VIF'))
idx.pval <- as.character(m.f$variable) == 'pval'
ggplot(m.f[idx.pval,],
       aes(x=var,y=value)) +
  geom_boxplot(col='#00bfc4ff',fill='grey95',outlier.color = 'grey50') +
  geom_hline(yintercept = 1.3,color='red',linetype='dashed') +
  theme_pubr()

p3 <- ggplot(m.f,
             aes(x=var,y=value)) +
  geom_boxplot(col='#00bfc4ff',fill='grey95',outlier.colour = 'grey50') +
  xlab('') +
  facet_wrap(facets = 'variable',scales = 'free_y') +
  scale_x_discrete(limit=rev(c('GenderMale',
                               "OccupationOS",
                               "ModeTransportPublic",
                               "SmokingNo",
                               "DietNV")),
                   # labels= c('Diet (nv)','Smoking (no)','Transport (public)',
                   #           'Occupation (os)','Gender (male)'))
                   labels= c('','','',
                             '',''))+
  theme_pubr() +
  theme(axis.text.x = element_text(angle=60,vjust = 1,hjust = 1))
pdf('./figures/joint/261220/Figure-03-B.pdf',width = 4.5,height = 6)
gridExtra::grid.arrange(
  p3,p2,p1,nrow=3,heights=c(0.2875+0.015, 0.2875-0.015, 0.425))
dev.off()

pdf('./figures/joint/261220/Figure-03-A.pdf',width = 3.45,height = 5.5)
p.OR.v2
dev.off()

