rm(list=ls())

symptom <- read.csv(
  './data/231220/symptoms.csv',
  stringsAsFactors = FALSE
)
library(ggplot2)
library(ggpubr)
symptom$Symptoms <- factor(
  symptom$Symptoms,
  levels = rev(symptom$Symptoms[order(symptom$Rate)])
)
label.cols[length(label.cols)] <- 'black'
p1.symp <- ggplot(symptom,aes(x=Symptoms,y=Rate)) +
  geom_bar(stat='identity',fill='#786721ff') +
  geom_text(aes(y=Rate,label=paste0(round(Rate,0),'%')),
            angle=90,
            nudge_y = 2) +
  theme_pubr() +
  ylab('Rate(%)') +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))

fg <- read.csv(
  './data/231220/feature-gender.csv',
  stringsAsFactors = FALSE
)
fg <- plyr::ddply(fg,'feature',function(x){
  x$perc <- round((x$count/sum(x$count))*100,2)
  x
})
fg$gender <- factor(fg$gender,levels = c('Male','Female'))
p2.f <- ggplot(fg,aes(x=feature,y=perc)) +
  geom_bar(stat='identity',aes(fill=gender)) +
  scale_fill_manual(name='',values = c(Female='#808080ff',Male='#ccccccff')) +
  theme_pubr() +
  ylab('Percentage(%)') +
  xlab('') +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
        legend.position = 'top')
pdf('./figures/joint/231220/Suppl-Fig-symptom.pdf',width = 6,height = 5)
gridExtra::grid.arrange(p2.f,p1.symp,ncol=2,
                        widths=c(0.3,0.7))
dev.off()