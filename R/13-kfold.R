



## load libraries
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes', 'performance', 'AICcmodavg')
lapply(libs, require, character.only = TRUE)


# Load data
DT <- readRDS("output/location-data/5-rdm-locs-sri-NN-N20.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## number of groups
length(unique(DT[iter == 1]$groupEnd))

## create unique step id by animal
DT[,'caribou_step_id_'] <- paste(DT$IDYr, DT$step_id_, sep = '_')

##### Set up data for K-fold ####

## values for k-fold
k = 5

## number of unique strata
x <- 1:length(unique(DT$caribou_step_id_))

spl <- split(x,cut(x,k,labels=FALSE))
  
# split strata randomly
newdata <- data.frame(caribou_step_id_ = unique(DT$caribou_step_id_))
  
## randomly sample strata for each fold
random_sample <- data.frame(caribou_step_id_ = sample(newdata$caribou_step_id_, 
                                              length(unique(DT$caribou_step_id_))))
random_sample$rand.vec <- 0
  
  for(i in 1:k){
    random_sample$rand.vec[min(spl[[i]]):max(spl[[i]])]<-i
  }

## combine randomly assigned strata back to original dataset  
data <- merge(newdata, random_sample, by = "caribou_step_id_", all.x = T )
  
DT2 <- merge(setDT(data), DT, by = "caribou_step_id_")

## exclude strata in fold 1 from data 
k1 <- glmmTMB(case_ ~ 
                ## step length
                I(log(sl_+1)) + 
                I(log(sl_+1)):(propForest + propLichen) +
                
                ## habitat variables
                 propForest + propLichen +
                
                ## social variables in interactions with movement and habitat 
                I(log(EndDist + 1)) + 
                I(log(StartDist + 1)):I(log(sl_+1)) +
                I(log(EndDist + 1)):(propForest + propLichen) + 
                
                I(log(sri+0.125)) +
                I(log(sri+0.125)):I(log(sl_+1)) +
                I(log(sri+0.125)):(propForest + propLichen) + 
                
                ## random effects  
                (1|caribou_step_id_) + 
                (0 + I(log(sl_+1)) | IDYr) +
                #(0 + propOpenMove:I(log(sri+0.125))| IDYr) +
                (0 + propForest:I(log(sri+0.125))| IDYr) +
                (0 + propLichen:I(log(sri+0.125))| IDYr) +
                (0 + I(log(sri+0.125)) | IDYr) +
                (0 + I(log(sl_+1)):I(log(sri+0.125))| IDYr) +
                #(0 + propOpenMove:I(log(EndDist+1)) | IDYr) +
                (0 + propForest:I(log(EndDist+1)) | IDYr) +
                (0 + propLichen:I(log(EndDist+1)) | IDYr) +
                (0 + I(log(EndDist+1)) | IDYr) +
                (0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr),
                
                family=poisson(), 
                data = DT2[rand.vec != 1],  
                map = list(theta=factor(c(NA,1:9))), 
                start = list(theta=c(log(1000), seq(0,0, length.out = 9))))
  

## exclude strata in fold 2 from data 
k2 <- glmmTMB(case_ ~ 
                ## step length
                I(log(sl_+1)) + 
                I(log(sl_+1)):(propForest + propLichen) +
                
                ## habitat variables
                propForest + propLichen +
                
                ## social variables in interactions with movement and habitat 
                I(log(EndDist + 1)) + 
                I(log(StartDist + 1)):I(log(sl_+1)) +
                I(log(EndDist + 1)):(propForest + propLichen) + 
                
                I(log(sri+0.125)) +
                I(log(sri+0.125)):I(log(sl_+1)) +
                I(log(sri+0.125)):(propForest + propLichen) + 
                
                ## random effects  
                (1|caribou_step_id_) + 
                (0 + I(log(sl_+1)) | IDYr) +
                #(0 + propOpenMove:I(log(sri+0.125))| IDYr) +
                (0 + propForest:I(log(sri+0.125))| IDYr) +
                (0 + propLichen:I(log(sri+0.125))| IDYr) +
                (0 + I(log(sri+0.125)) | IDYr) +
                (0 + I(log(sl_+1)):I(log(sri+0.125))| IDYr) +
                #(0 + propOpenMove:I(log(EndDist+1)) | IDYr) +
                (0 + propForest:I(log(EndDist+1)) | IDYr) +
                (0 + propLichen:I(log(EndDist+1)) | IDYr) +
                (0 + I(log(EndDist+1)) | IDYr) +
                (0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr),
              
              family=poisson(), 
              data = DT2[rand.vec != 2],  
              map = list(theta=factor(c(NA,1:9))), 
              start = list(theta=c(log(1000), seq(0,0, length.out = 9))))  


saveRDS(k2, "output/issa models/k2_issa.RDS")

## exclude strata in fold 3 from data 
k3 <- glmmTMB(case_ ~ 
                ## step length
                I(log(sl_+1)) + 
                I(log(sl_+1)):(propForest + propLichen) +
                
                ## habitat variables
                propForest + propLichen +
                
                ## social variables in interactions with movement and habitat 
                I(log(EndDist + 1)) + 
                I(log(StartDist + 1)):I(log(sl_+1)) +
                I(log(EndDist + 1)):(propForest + propLichen) + 
                
                I(log(sri+0.125)) +
                I(log(sri+0.125)):I(log(sl_+1)) +
                I(log(sri+0.125)):(propForest + propLichen) + 
                
                ## random effects  
                (1|caribou_step_id_) + 
                (0 + I(log(sl_+1)) | IDYr) +
                #(0 + propOpenMove:I(log(sri+0.125))| IDYr) +
                (0 + propForest:I(log(sri+0.125))| IDYr) +
                (0 + propLichen:I(log(sri+0.125))| IDYr) +
                (0 + I(log(sri+0.125)) | IDYr) +
                (0 + I(log(sl_+1)):I(log(sri+0.125))| IDYr) +
                #(0 + propOpenMove:I(log(EndDist+1)) | IDYr) +
                (0 + propForest:I(log(EndDist+1)) | IDYr) +
                (0 + propLichen:I(log(EndDist+1)) | IDYr) +
                (0 + I(log(EndDist+1)) | IDYr) +
                (0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr),
              
              family=poisson(), 
              data = DT2[rand.vec != 3],  
              map = list(theta=factor(c(NA,1:9))), 
              start = list(theta=c(log(1000), seq(0,0, length.out = 9))))

saveRDS(k3, "output/issa models/k3_issa.RDS")

## exclude strata in fold 4 from data 
k4 <- glmmTMB(case_ ~ 
                ## step length
                I(log(sl_+1)) + 
                I(log(sl_+1)):(propForest + propLichen) +
                
                ## habitat variables
                propForest + propLichen +
                
                ## social variables in interactions with movement and habitat 
                I(log(EndDist + 1)) + 
                I(log(StartDist + 1)):I(log(sl_+1)) +
                I(log(EndDist + 1)):(propForest + propLichen) + 
                
                I(log(sri+0.125)) +
                I(log(sri+0.125)):I(log(sl_+1)) +
                I(log(sri+0.125)):(propForest + propLichen) + 
                
                ## random effects  
                (1|caribou_step_id_) + 
                (0 + I(log(sl_+1)) | IDYr) +
                #(0 + propOpenMove:I(log(sri+0.125))| IDYr) +
                (0 + propForest:I(log(sri+0.125))| IDYr) +
                (0 + propLichen:I(log(sri+0.125))| IDYr) +
                (0 + I(log(sri+0.125)) | IDYr) +
                (0 + I(log(sl_+1)):I(log(sri+0.125))| IDYr) +
                #(0 + propOpenMove:I(log(EndDist+1)) | IDYr) +
                (0 + propForest:I(log(EndDist+1)) | IDYr) +
                (0 + propLichen:I(log(EndDist+1)) | IDYr) +
                (0 + I(log(EndDist+1)) | IDYr) +
                (0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr),
              
              family=poisson(), 
              data = DT2[rand.vec != 4],  
              map = list(theta=factor(c(NA,1:9))), 
              start = list(theta=c(log(1000), seq(0,0, length.out = 9))))

saveRDS(k4, "output/issa models/k4_issa.RDS")

## exclude strata in fold 5 from data 
k5 <- glmmTMB(case_ ~ 
                ## step length
                I(log(sl_+1)) + 
                I(log(sl_+1)):(propForest + propLichen) +
                
                ## habitat variables
                propForest + propLichen +
                
                ## social variables in interactions with movement and habitat 
                I(log(EndDist + 1)) + 
                I(log(StartDist + 1)):I(log(sl_+1)) +
                I(log(EndDist + 1)):(propForest + propLichen) + 
                
                I(log(sri+0.125)) +
                I(log(sri+0.125)):I(log(sl_+1)) +
                I(log(sri+0.125)):(propForest + propLichen) + 
                
                ## random effects  
                (1|caribou_step_id_) + 
                (0 + I(log(sl_+1)) | IDYr) +
                #(0 + propOpenMove:I(log(sri+0.125))| IDYr) +
                (0 + propForest:I(log(sri+0.125))| IDYr) +
                (0 + propLichen:I(log(sri+0.125))| IDYr) +
                (0 + I(log(sri+0.125)) | IDYr) +
                (0 + I(log(sl_+1)):I(log(sri+0.125))| IDYr) +
                #(0 + propOpenMove:I(log(EndDist+1)) | IDYr) +
                (0 + propForest:I(log(EndDist+1)) | IDYr) +
                (0 + propLichen:I(log(EndDist+1)) | IDYr) +
                (0 + I(log(EndDist+1)) | IDYr) +
                (0 + I(log(StartDist + 1)):I(log(sl_+1)) | IDYr),
              
              family=poisson(), 
              data = DT2[rand.vec != 5],  
              map = list(theta=factor(c(NA,1:9))), 
              start = list(theta=c(log(1000), seq(0,0, length.out = 9))))

saveRDS(k5, "output/issa models/k5_issa.RDS")


## assign iSSF scores back to original data by fold
for(i in 1:k){
    DT2$iSSFscores[DT2$rand.vec == i] <- exp(predict(eval(parse(text=paste("k",i,sep=""))), 
                                                    newdata = subset(DT2, rand.vec == i), 
                                                    type="link",
                                                    allow.new.levels=T))
}  


# Run the k-fold CV evaluation sensu Boyce et al. 2002
dataset <- DT2[complete.cases(DT2[,"iSSFscores"]),]
rho_model <- numeric(k) ## it will store Spearman's coefficients
resp="Use"

for (w in 1:k){
  fold <- subset(dataset,rand.vec == unique(dataset$rand.vec)[w])
  q.pp <- quantile(fold$iSSFscores,probs=seq(0,1,.1)) ## computing quantiles of RSF scores
  bin <- rep(NA,length(fold$iSSFscores))
  for (j in 1:20){
    bin[fold$iSSFscores>=q.pp[j]& fold$iSSFscores<q.pp[j+1]] = j  ## binning RSF scores (20 bins)
  }
  used<-eval(parse(text=paste("fold$",resp,sep="")))
  # --------------------------------------------------------
  a <- table(used,bin) ## area adjusted freq in used/available for each bin
  a <- t(a) #transpose the table
  a <- as.data.frame.matrix(a) ## the next few lines compute area-adjusted frequency of categories (bins) of RSF scores 
  a$areaadjusted <- rep(NA,length(20))
  sum0 <- sum(a[,1])
  sum1 <- sum(a[,2])
  a$areaadjusted <- (a[,2] / sum1 ) / (a[,1] / sum0)
  a$bins <- seq(1,10,by=1);a
  rho_model[w] <- with(a,cor.test(bins,areaadjusted,method="spearm"))$estimate }


## store Spearman correlation coefficients that will be used for final plots below ##
Rho_random_individuals <- rho_model

kfold<-mean(Rho_random_individuals)
error<-sd(Rho_random_individuals)
output<-list(kfold,error,Rho_random_individuals)
names(output)<-c("Kfold","SE","Scores")
out<-output


# pull out fixed effects
k1_eff <- broom.mixed::tidy(k1)
k1_eff$fold <- 1
k2_eff <- broom.mixed::tidy(k2)
k2_eff$fold <- 2
k3_eff <- broom.mixed::tidy(k3)
k3_eff$fold <- 3
k4_eff <- broom.mixed::tidy(k4)
k4_eff$fold <- 4
k5_eff <- broom.mixed::tidy(k5)
k5_eff$fold <- 5

all_fold <- rbind(setDT(k1_eff)[effect == "fixed"], 
                  setDT(k2_eff)[effect == "fixed"], 
                  setDT(k3_eff)[effect == "fixed"], 
                  setDT(k4_eff)[effect == "fixed"], 
                  setDT(k5_eff)[effect == "fixed"])

all_fold$fold <- as.factor(all_fold$fold)
all_fold$term2 <- rep(factor(unique(all_fold$term), levels= c(unique(rev(all_fold$term)))),5)

fwrite(all_fold, "output/issa models/all_k_fold.csv")

