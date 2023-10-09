#### FINAL WORK ####
setwd("C:/Users/DANIIL KHOKHRUNOV/Desktop/личные файлы/Учебы/6 semestr/Mimosemestr credit risk")
getwd()

dat <- read.csv("mortgage_sample.csv")
summary(dat)
#install.packages("scorecard")
library(scorecard)
#install.packages("dplyr")
library(dplyr)
#install.packages("caTools")
library(caTools)
#install.packages("ROCR")
library(ROCR) 


target<-c(rep(0, nrow(dat)))
dat <- cbind(dat, target)
dat_train <- filter(dat, sample == "public")
dat_test <- filter(dat, sample == "private")


for(i in 1:nrow(dat_train)){
  if(as.numeric(dat_train$default_time[i]) == 1){
    id_def <- dat_train$id[i]
    n <- min(sum(dat_train$id == id_def), 12)
    for(j in 1:n-1){ 
      dat_train$target[i-j] <- 1
    }
  }
}

decembers <- seq(from = 12, to = max(dat$time), by = 12)
dat_train <- dat_train[(dat_train$time %in% decembers), ]  ### fixed cohorts
workingdata <- dat_train

summary(workingdata)
workingdata <- replace_na(workingdata, repl = "median")   #### replace NA with median

workingdata <- workingdata[ ,-c(22:24)]

time_since_orig <- workingdata$time-workingdata$orig_time ### new variable time_since_orig
ncol(workingdata)
workingdata <- workingdata[ ,-21]    ### we have target so no default_time
workingdata <- workingdata[ , -c(1:2)]  ### id and time are not usefull for prediction
workingdata <- cbind(workingdata, time_since_orig)

bines <- woebin(workingdata, y = "target")
woebin_plot(bines)

cf <- cor(workingdata, method = "spearman") 

df <- woebin_ply(workingdata, bines)

corelacni_matice <- round(cor(df, method = "spearman"), 2)

corprom <- data.frame(matrix(NA, 1, 5))
colnames(corprom) <- c("prom1", "prom2", "koef", "target1", "target2")
for(i in 1:nrow(corelacni_matice)){
  for(j in 1:i){
    if(corelacni_matice[i,j] >= 0.5 & corelacni_matice[i,j] != 1){
      corprom[nrow(corprom)+1, ] <- c(rownames(corelacni_matice)[i], colnames(corelacni_matice)[j], corelacni_matice[i,j], corelacni_matice[i,20], corelacni_matice[j,20])
    }
  }
}
corprom <- corprom[-1, ]  ### prehlednejsi podoba

corval <- c()
for(i in 1:nrow(corprom)){
  if(corprom[i,4] > corprom[i,5]){
    corval <- append(corval, corprom[i, 2])
  } else {
    corval <- append(corval, corprom[i,1])
  }
}

df <- df[ ,setdiff(names(df), corval), with = FALSE]  ### deleting corelating rows
colnames(df)
wdnames <- c("target", "balance_time", "interest_rate_time", "REtype_CO_orig_time",
             "REtype_PU_orig_time", "REtype_SF_orig_time", "investor_orig_time",
             "FICO_orig_time", "LTV_orig_time", "time_since_orig")
colnames(workingdata)
nonameswd <- setdiff(colnames(workingdata), wdnames)
workingdata <- workingdata[ ,setdiff(names(workingdata),nonameswd), with = FALSE]


dtvf = var_filter(workingdata, "target")

dt_list = split_df(dtvf, "target")
label_list = lapply(dt_list, function(x) x$target)

bins <- woebin(dt_list$train, "target")

dtlst_woe = lapply(dt_list, function(d) woebin_ply(d, bins))

m = glm(target ~ ., family = binomial(), data = dtlst_woe$train)
summary(m)

cardprob2 <- scorecard(bins = bins, m, odds0 = 1/51, pdo = 50, points0 = 500)
# probtrain = predict(m, dtlst_woe$train, type = 'response')
# probtest = predict(m, dtlst_woe$test, type = 'response')
problst2 = lapply(dtlst_woe, function(x) predict(m, x, type='response'))
reslist <- list('card' = cardprob2, 'prob' = problst2)

perf_eva(pred = problst2, label = label_list, title = 'train', show_plot = c('ks','roc'))

score_list = lapply(dt_list, function(x) scorecard_ply(x, cardprob2))
dat$time_since_orig <- dat$time-dat$orig_time
perf_psi(score = score_list, label_list)

fin <- scorecard_ply(dat, cardprob2)

result <- cbind(dat, fin)


result$predict <- ifelse(result$score < 500, 1, 0)
write.csv(result, file = "prediction.csv")
