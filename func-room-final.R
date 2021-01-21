library(ggplot2)
library(ggpubr)
runModelC.gender <- function(model.data){
  idx.neg <- which(as.character(model.data$Sero.Status) == 'Negative')
  MODS <- list()
  k <-  1
  for(j in 1:100){
    set.seed(k)
    idx.neg.random <- sample(idx.neg,length(idx.neg))
    idx.mod.pos <- which(as.character(model.data$Sero.Status) == 'Positive')
    idx.mod <- seq(1,length(idx.neg.random),length(idx.mod.pos))
    for( i in 1:length(idx.mod)){
      if(i != length(idx.mod)){
        start.idx <- idx.mod[i]
        end.idx <- idx.mod[i+1]-1
      }else{
        start.idx <- idx.mod[i]
        end.idx <- length(idx.neg)
      }
      idx.mod.neg <- idx.neg.random[start.idx:end.idx]
      tr.data <- model.data[c(idx.mod.neg,idx.mod.pos),]
      MODS[[k]] <- glm(
        formula = Sero.Status~Occupation+ModeTransport+Smoking+Diet,
        data=tr.data,
        family = binomial(link = "logit"))
      print(k)
      k <- k + 1
    }
  }
  names(MODS) <- paste0('MOD',1:length(MODS))
  return(MODS)
}
runModelC.female <- function(model.data){
  idx.neg <- which(as.character(model.data$Sero.Status) == 'Negative')
  MODS <- list()
  k <-  1
  for(j in 1:100){
    set.seed(k)
    idx.neg.random <- sample(idx.neg,length(idx.neg))
    idx.mod.pos <- which(as.character(model.data$Sero.Status) == 'Positive')
    idx.mod <- seq(1,length(idx.neg.random),length(idx.mod.pos))
    for( i in 1:length(idx.mod)){
      if(i != length(idx.mod)){
        start.idx <- idx.mod[i]
        end.idx <- idx.mod[i+1]-1
      }else{
        start.idx <- idx.mod[i]
        end.idx <- length(idx.neg)
      }
      idx.mod.neg <- idx.neg.random[start.idx:end.idx]
      tr.data <- model.data[c(idx.mod.neg,idx.mod.pos),]
      MODS[[k]] <- glm(
        formula = Sero.Status~Occupation+ModeTransport,
        data=tr.data,
        family = binomial(link = "logit"))
      print(k)
      k <- k + 1
    }
  }
  names(MODS) <- paste0('MOD',1:length(MODS))
  return(MODS)
}

mod.info <- function(x){
  xx.sum <- summary(x)
  c.conf <- stats::confint(x)[-1,]
  df_ <- data.frame(
    OR = exp(coef(x)[-1]),
    min.conf = exp(c.conf[,1]),
    max.conf = exp(c.conf[,2]),
    VIF = faraway::vif(x),
    pval = xx.sum$coefficients[-1,4]
  )
  df_$var <- rownames(df_)
  df_
}
summary.MODS <- function(MODS){
  MOD.summary <- plyr::ldply(
    MODS,
    .fun = mod.info,
    .id = 'MOD'
  )
  return(MOD.summary)
}
summary.plots <- function(MOD.summary){
  p.title <- paste0(length(unique(MOD.summary$MOD)), ' iterations')
  p1 <- ggplot(MOD.summary,
               aes(x=var,y=OR)) +
    geom_boxplot() +
    ggtitle(p.title) +
    xlab('Variable') +ylab('Odd ratio') +
    theme_pubr() +
    theme(axis.text.x = element_text(angle=60,vjust = 1,hjust = 1))
  p3 <- ggplot(MOD.summary,
               aes(x=var,y=VIF)) +
    geom_boxplot() +
    ggtitle(p.title) +
    xlab('Variable') +ylab('Variance inflation factor') +
    theme_pubr() +
    theme(axis.text.x = element_text(angle=60,vjust = 1,hjust = 1))
  
  p2 <- ggplot(MOD.summary,
               aes(x=var,y=-log10(pval))) +
    geom_boxplot() +
    geom_hline(yintercept = 1.3,color='red',linetype='dashed') +
    ggtitle(p.title) +
    xlab('Variable') +ylab('-log10(p-value)') +
    theme_pubr() +
    theme(axis.text.x = element_text(angle=60,vjust = 1,hjust = 1))
  return(list(p1,p2,p3))
}

runModelC <- function(model.data){
  idx.neg <- which(as.character(model.data$Sero.Status) == 'Negative')
  MODS <- list()
  k <-  1
  for(j in 1:100){
    set.seed(k)
    idx.neg.random <- sample(idx.neg,length(idx.neg))
    idx.mod.pos <- which(as.character(model.data$Sero.Status) == 'Positive')
    idx.mod <- seq(1,length(idx.neg.random),length(idx.mod.pos))
    for( i in 1:length(idx.mod)){
      if(i != length(idx.mod)){
        start.idx <- idx.mod[i]
        end.idx <- idx.mod[i+1]-1
      }else{
        start.idx <- idx.mod[i]
        end.idx <- length(idx.neg)
      }
      idx.mod.neg <- idx.neg.random[start.idx:end.idx]
      tr.data <- model.data[c(idx.mod.neg,idx.mod.pos),]
      MODS[[k]] <- glm(
        # formula = Sero.Status~Gender+Occupation+ModeTransport+Smoking+Diet,
        formula = Sero.Status~Occupation+ModeTransport+Smoking+Diet,
        data=tr.data,
        family = binomial(link = "logit"))
      print(k)
      k <- k + 1
    }
  }
  names(MODS) <- paste0('MOD',1:length(MODS))
  return(MODS)
}
runModelC.add <- function(model.data){
  idx.neg <- which(as.character(model.data$Sero.Status) == 'Negative')
  MODS <- list()
  k <-  1
  for(j in 1:100){
    set.seed(k)
    idx.neg.random <- sample(idx.neg,length(idx.neg))
    idx.mod.pos <- which(as.character(model.data$Sero.Status) == 'Positive')
    idx.mod <- seq(1,length(idx.neg.random),length(idx.mod.pos))
    for( i in 1:length(idx.mod)){
      if(i != length(idx.mod)){
        start.idx <- idx.mod[i]
        end.idx <- idx.mod[i+1]-1
      }else{
        start.idx <- idx.mod[i]
        end.idx <- length(idx.neg)
      }
      idx.mod.neg <- idx.neg.random[start.idx:end.idx]
      tr.data <- model.data[c(idx.mod.neg,idx.mod.pos),]
      MODS[[k]] <- glm(
        formula = Sero.Status~Gender+Occupation+ModeTransport+Smoking+Diet,
        data=tr.data,
        family = binomial(link = "logit"))
      print(k)
      k <- k + 1
    }
  }
  names(MODS) <- paste0('MOD',1:length(MODS))
  return(MODS)
}


runModelabo.gender <- function(model.data){
  idx.neg <- which(as.character(model.data$Sero.Status) == 'Negative')
  MODS <- list()
  k <-  1
  for(j in 1:100){
    set.seed(k)
    idx.neg.random <- sample(idx.neg,length(idx.neg))
    idx.mod.pos <- which(as.character(model.data$Sero.Status) == 'Positive')
    idx.mod <- seq(1,length(idx.neg.random),length(idx.mod.pos))
    for( i in 1:length(idx.mod)){
      if(i != length(idx.mod)){
        start.idx <- idx.mod[i]
        end.idx <- idx.mod[i+1]-1
      }else{
        start.idx <- idx.mod[i]
        end.idx <- length(idx.neg)
      }
      idx.mod.neg <- idx.neg.random[start.idx:end.idx]
      tr.data <- model.data[c(idx.mod.neg,idx.mod.pos),]
      MODS[[k]] <- glm(
        formula = Sero.Status~Gender+Rh+BG.A+BG.B+BG.AB+BG.O,
        data=tr.data,
        family = binomial(link = "logit"))
      print(k)
      k <- k + 1
    }
  }
  names(MODS) <- paste0('MOD',1:length(MODS))
  return(MODS)
}

mod.info.abo <- function(x){
  xx.sum <- summary(x)
  c.conf <- stats::confint(x)[-1,]
  idx.na <- is.na(coef(x)[-1])
  pval <- xx.sum$coefficients[-1,4]
  if(length(pval)!=nrow(c.conf)){
    pval <- c(pval,NA)
    names(pval)[nrow(c.conf)] <- rownames(c.conf)[nrow(c.conf)]
  }else{
    # nothing
  }
  df_ <- data.frame(
    OR = exp(coef(x)[-1]),
    min.conf = exp(c.conf[,1]),
    max.conf = exp(c.conf[,2]),
    VIF = faraway::vif(x),
    pval = pval
  )
  df_$var <- rownames(df_)
  df_
}
summary.MODS.abo <- function(MODS){
  MOD.summary <- plyr::ldply(
    MODS,
    .fun = mod.info.abo,
    .id = 'MOD'
  )
  return(MOD.summary)
}

