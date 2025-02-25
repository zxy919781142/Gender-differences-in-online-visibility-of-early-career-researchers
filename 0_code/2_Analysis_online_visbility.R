## install and read libray 

install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) { # Check if the package is installed
      install.packages(pkg, dependencies = TRUE) # Install if not installed
      library(pkg, character.only = TRUE) # Load the package
    } else {
      library(pkg, character.only = TRUE) # Load if already installed
    }
  }
}

## survival analysis
required_packages <- c("tidyverse","sjPlot","lme4", "dplyr", "car", "glmmTMB", "ggtext")
install_and_load(required_packages)

#**************************************************
#****0.1 Function for data _pocessing
#**************************************************
#### 
data_processing <- function(data) {
  
  data$academic_age<-as.character(data$academic_age)
  data$others_first<-as.character(data$others_first)
  data$gender<-relevel(factor(data$gender), ref = "male")
  data$Jr_Quantile<-relevel(factor(data$Jr_Quantile), ref = "Q4")
  data$cohort<-relevel(factor(data$cohort), ref = "2012")
  data$colla_ctr_Y<-relevel(factor(data$colla_ctr_Y), ref = "N")
  data$colla_aff_Y<-relevel(factor(data$colla_aff_Y), ref = "N")
  data$academic_age<-relevel(factor(data$academic_age), ref = "1")
  data$others_first<-relevel(factor(data$others_first), ref = "0")
  data$len_tweet_cate<-relevel(factor(data$len_tweet_cate), ref = "Low")
  
  data[,c("author_cnt")] <- scale(data[,c("author_cnt")])
  data[,c("pub_order")] <- scale(data[,c("pub_order")])
  
  data$tw_others_num_ori <- data$tw_others_num
  data[,c("tw_others_num")] <- scale(data[,c("tw_others_num")]) 
  
  data["len_tweet"][is.na(data["len_tweet"])] <- 0
  data$len_tweet_ori <- data$len_tweet
  data[,c("len_tweet")] <- scale(data[,c("len_tweet")])
  
  return(data)
}

sig_text_star<-function(diff,sig){
  if (sig<=0.01){
    #return (sprintf("%0.2f", diff) %p% supsc("***"))
    return (paste0(round(diff,2),"<sup>***</sup>"))
    
  }
  else if(sig<=0.05){ return (paste0(round(diff,2),"<sup>**</sup>"))}
  else if(sig<=0.1){ return (paste0(round(diff,2),"<sup>*</sup>"))}
  else{ return (round(diff,2))}
  
}
sig_star<-function(diff,sig,gender){
  if ((gender=="female")|(gender=="Female")){
    if(diff>0 & sig<=0.01){
      return ("***")
      
    }
    else if(diff>0 & sig<=0.05){ return ("**")}
    else if(diff>0 & sig<=0.1){ return ("*")}
    else{ return ("")}
    
  }
  else{
    if(diff<0 & sig<=0.01){
      return ("***")
      
    }
    else if(diff<0 & sig<=0.05){ return ("**")}
    else if(diff<0& sig<=0.1){ return ("*")}
    else{ return ("")}
  }
}

num_format<-function(num,format){
  if (format=="percent"){
    return (scales::percent(num,accuracy = 0.01) ) 
  }
  else if (format=="p.p."){
    return (round(num*100,2))
  }
  else{
    return (round(num,2))
  }
  
}
R_text <- function(diff,gender,format) {
  hjust=0
  if ((gender=="female")|(gender=="Female")){
    
    if (diff>0){
      text=num_format(diff,format)
      hjust=-1
    }
    else{
      text=""
    }
    
  }
  else {
    if (diff<0){
      text=num_format(diff,format)
      hjust=1
    }
    else{
      text=""
    }
  }
  return(c(text,hjust))
}

R_text_star <- function(diff,sig,gender) {
  if (gender=="female"){
    if (diff<=0){
      text=sig_star_text(diff,sig)
      
    }
    else {
      text=""
      
    }
    
  }
  else{
    if (diff>0){
      text=sig_star_text(diff,sig)
      
    }
    else {
      text="0"
      
    }
    
  }
  
  
  return(text)
}


#**************************************************
#****0.2  Function for marginal effect
#**************************************************

marginal_effect_gender_pro <- function(modelX,data,offset,first_author) {
  
  
  mf <- model.frame(modelX)
  if (offset==1) {
    colnames(mf)[ncol(mf)] = "pub_num_5y"
  } 
  
  model<-modelX
  
  mf_1 <- mf
  mf_2 <- mf
  mf_1$gender <- "female"
  mf_2$gender <- "male"
  
  if (first_author==0){
    
    index<-c("cohort","discipline","Jr_Quantile","seq_nr_grp_1")
  }
  else{
    index<-c("cohort","discipline","Jr_Quantile","academic_age")
  }
  AME_self_gender<-data.frame(matrix(ncol = 7))
  colnames(AME_self_gender) <- c("index","group","AME","sig","AME.low","AME.high","Female-to-Male.ratio")
  m<-1
  
  for (i in index){
    for (j in c(unique(data[[i]]))){
      
      #update_model<-update(model, ~ . - log((pub_num_5y)))
      yhat1 <- predict(model, newdata = mf_1[mf_1[[i]]==j,],type="response")
      
      
      yhat2 <- predict(model, newdata = mf_2[mf_2[[i]]==j,],type="response")
      
      
      # CI 95%
      #mean<-mean(yhat01-yhat00)
      #s<-sd(yhat01-yhat00)
      #n<-length(yhat01)
      n1<-length(yhat1)
      n2<-length(yhat2)
      
      
      m1<-mean(yhat1)
      s1<-sd(yhat1)
      
      m2<-mean(yhat2)
      s2<-sd(yhat2)
      
      sp = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
      margin<- qt(0.975,df=n1+n2-1)*sqrt(sp/n1 + sp/n2)
      sig<-t.test(yhat1,yhat2,alternative="two.sided",conf.level=0.95)
      
      if (m==1) {
        AME_self_gender<-data.frame(list(i,j, m2-m1,sig$p.value, m2-m1-margin,m2-m1+margin, m1/m2))
        colnames(AME_self_gender) <- c("index","group","AME.gender","sig.gender","AME.low.gender","AME.high.gender","Female-to-Male.ratio")
        
        
      }
      else{
        AME_self_gender<-rbind(AME_self_gender, list(i,j, m2-m1,sig$p.value, m2-m1-margin,m2-m1+margin, m1/m2))
        
      }
      m<-m+1
      
    }
  }
  ### plot*********************************
  
  AME_self_gender[AME_self_gender$index=="discipline", ]$group <- plyr::mapvalues(AME_self_gender[AME_self_gender$index=="discipline", ]$group, from=c("Medical and Health Sciences", "Engineering and Technology","Natural Sciences", "Social Sciences","Humanities","Agricultural Sciences","No_discipline_assigned" ), 
                                                                                  to=c("Med & Hea", "Eng & Tec","Natur", "Soci", "Hum","Agri","No_dis"))
  
  x<-AME_self_gender
  
  y<-x %>% filter(index!="Jr_Quantile")
  x<-x %>% filter(index=="Jr_Quantile") %>% arrange(match(group, c("Q1", "Q2", "Q3","Q4","Others")))
  x<-rbind(y,x)
  
  if (first_author==0){
    y<-x %>% filter(index!="seq_nr_grp_1")
    x<-x[x$index=="seq_nr_grp_1",] %>%arrange(match(group, c("first", "middle", "last")))
    x<-rbind(y,x)
  }
  else{
    y<-x %>% filter(index!="academic_age")
    x<-x[x$index=="academic_age",] %>%arrange(match(group, c("1", "2", "3")))
    x<-rbind(y,x)    
    
  }
  
  
  y<-x %>% filter(index!="cohort")
  x<-x[x$index=="cohort",] %>%
    arrange(match(group, c("2012", "2013", "2014","2015","2016")))
  x<-rbind(x,y)
  x$index<-factor(x$index,levels = index )
  
  
  
  return(x)
}


marginal_effect_gender_zinb <- function(modelX,data,model_type,re,allow){
  
  mf <- model.frame(modelX)
  
  model<-modelX
  
  
  mf_1 <- mf
  mf_2 <- mf
  mf_1$gender <- "female"
  mf_2$gender <- "male"
  
  
  if (("discipline_new" %in% names(model$frame)) & ("pub_before_cate" %in% names(model$frame))) {
    index <-c("cohort","discipline_new","Jr_Quantile","pub_before_cate")
  } else if ("discipline_new" %in% names(model$frame)){
    index <- c("cohort","discipline_new","Jr_Quantile","academic_age")
  } else {
    index <- c("cohort","discipline","Jr_Quantile","academic_age")
  }
  
  
  
  
  
  
  AME_self_gender<-data.frame(matrix(ncol = 7))
  colnames(AME_self_gender) <- c("index","group","AME","sig","AME.low","AME.high","Female-to-Male.ratio")
  m<-1
  
  for (i in index){
    for (j in c(unique(data[[i]]))){
      
      if (model_type=="zero_inflation"){
        yhat1 <- predict(model, newdata = mf_1[mf_1[[i]]==j,],type="zprob",re.form= re,allow.new.levels = allow)
        yhat2 <- predict(model, newdata = mf_2[mf_2[[i]]==j,],type="zprob",re.form= re,allow.new.levels = allow)
      }
      
      else if (model_type=="conditional"){
        
        yhat1 <- predict(model, newdata = mf_1[mf_1[[i]]==j,],type="conditional",re.form= re,allow.new.levels = allow)
        yhat2 <- predict(model, newdata = mf_2[mf_2[[i]]==j,],type="conditional",re.form= re,allow.new.levels = allow)
        
      }
      else{
        yhat1 <- predict(model, newdata = mf_1[mf_1[[i]]==j,],type="response",re.form= re,allow.new.levels = allow)
        yhat2 <- predict(model, newdata = mf_2[mf_2[[i]]==j,],type="response",re.form= re,allow.new.levels = allow)
        
      }
      
      
      n1<-length(yhat1)
      n2<-length(yhat2)
      
      
      m1<-mean(yhat1)
      s1<-sd(yhat1)
      
      m2<-mean(yhat2)
      s2<-sd(yhat2)
      
      sp = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
      margin<- qt(0.975,df=n1+n2-1)*sqrt(sp/n1 + sp/n2)
      sig<-t.test(yhat1,yhat2,alternative="two.sided",conf.level=0.95)
      
      if (m==1) {
        AME_self_gender<-data.frame(list(i,j, m1-m2,sig$p.value, m1-m2-margin,m1-m2+margin, m1/m2))
        colnames(AME_self_gender) <- c("index","group","AME.gender","sig.gender","AME.low.gender","AME.high.gender","Female-to-Male.ratio")
        
        
      }
      else{
        AME_self_gender<-rbind(AME_self_gender, list(i,j, m1-m2,sig$p.value, m1-m2-margin,m1-m2+margin, m1/m2))
        
      }
      m<-m+1
      
    }
  }
  ### plot*********************************
  
  if ("discipline_new" %in% names(model$frame)) {
    AME_self_gender[AME_self_gender$index=="discipline_new", ]$group <- 
      plyr::mapvalues(AME_self_gender[AME_self_gender$index=="discipline_new", ]$group, 
                      from=c('Computer and information sciences', 'Biological sciences',
                             'Clinical medicine', 'Physical sciences and astronomy',
                             'Chemical sciences', 'Other engineering and technologies',
                             'Other social sciences', 'Educational sciences', 'Mathematics',
                             'Basic medical research', 'Materials engineering',
                             'Mechanical engineering', 'Other natural sciences',
                             'Earth and related environmental sciences', 'Health sciences',
                             'Agricultural Sciences', 'Psychology',
                             'Sociology and political science', 'Humanities',
                             'Electrical engineering, electronic engineering,  information engineering ',
                             'Chemical engineering', 'Economics', 'unknown'), 
                      to=c("Com", "Bio", "Clinic", "Phys", "ChemS", "Other_eng", "Other_soc", "Edu", "Math", "Medi", "Mater", "Mech", "Other_nat", "Earth", "Health", "Agri", "Psy", "Socio", "Human", "Elec",
                           "ChemE","Econ","Unk"))
  } else {
    
    AME_self_gender[AME_self_gender$index=="discipline", ]$group <- plyr::mapvalues(AME_self_gender[AME_self_gender$index=="discipline_new", ]$group, from=c("Medical and Health Sciences", "Engineering and Technology","Natural Sciences", "Social Sciences","Humanities","Agricultural Sciences","No_discipline_assigned" ), 
                                                                                    to=c("Med & Hea", "Eng & Tec","Natur", "Soci", "Hum","Agri","No_dis"))
  }
  
  
  
  x<-AME_self_gender
  
  y<-x %>% filter(index!="Jr_Quantile")
  x<-x %>% filter(index=="Jr_Quantile") %>% arrange(match(group, c("Q1", "Q2", "Q3","Q4","Others")))
  x<-rbind(y,x)
  
  
  if ("academic_age" %in% names(model$frame)){
    y<-x %>% filter(index!="academic_age")
    x<-x[x$index=="academic_age",] %>%arrange(match(group, c("1", "2", "3")))
    x<-rbind(y,x)  
  }
  
  if ("pub_before_cate" %in% names(model$frame)){
    y<-x %>% filter(index!="pub_before_cate")
    x<-x[x$index=="pub_before_cate",] %>%arrange(match(group, c("0", "1", "2+")))
    x<-rbind(y,x)  
  }  
  
  
  
  y<-x %>% filter(index!="cohort")
  x<-x[x$index=="cohort",] %>%
    arrange(match(group, c("2012", "2013", "2014","2015","2016")))
  x<-rbind(x,y)
  x$index<-factor(x$index,levels = index )
  
  
  
  return(x)
}


marginal_effect_gender_zinb_gender <- function(modelX,data,model_type,re,allow){
  
  mf <- model.frame(modelX)
  
  model<-modelX
  
  
  mf_1 <- mf
  mf_2 <- mf
  mf_1$gender <- "female"
  mf_2$gender <- "male"
  
  
  if (("discipline_new" %in% names(model$frame)) & ("pub_before_cate" %in% names(model$frame))) {
    index <-c("cohort","discipline_new","Jr_Quantile","pub_before_cate")
  } else if ("discipline_new" %in% names(model$frame)){
    index <- c("cohort","discipline_new","Jr_Quantile","academic_age")
  } else {
    index <- c("cohort","discipline","Jr_Quantile","academic_age")
  }
  
  
  AME_self_gender<-data.frame(matrix(ncol = 7))
  colnames(AME_self_gender) <- c("index","group","gender","AME","sig","AME.low","AME.high")
  m<-1
  
  for (i in index){
    for (j in c(unique(data[[i]]))){
      
      if (model_type=="zero_inflation"){
        yhat1 <- predict(model, newdata = mf_1[mf_1[[i]]==j,],type="zprob",re.form= re,allow.new.levels = allow)
        yhat2 <- predict(model, newdata = mf_2[mf_2[[i]]==j,],type="zprob",re.form= re,allow.new.levels = allow)
      }
      
      else if (model_type=="conditional"){
        
        yhat1 <- predict(model, newdata = mf_1[mf_1[[i]]==j,],type="conditional",re.form= re,allow.new.levels = allow)
        yhat2 <- predict(model, newdata = mf_2[mf_2[[i]]==j,],type="conditional",re.form= re,allow.new.levels = allow)
        
      }
      else{
        yhat1 <- predict(model, newdata = mf_1[mf_1[[i]]==j,],type="response",re.form= re,allow.new.levels = allow)
        yhat2 <- predict(model, newdata = mf_2[mf_2[[i]]==j,],type="response",re.form= re,allow.new.levels = allow)
        
      }
      
      
      n1<-length(yhat1)
      n2<-length(yhat2)
      
      
      m1<-mean(yhat1)
      s1<-sd(yhat1)
      
      m2<-mean(yhat2)
      s2<-sd(yhat2)
      
      margin.1<- qt(0.975,df=n1-1)* s1/sqrt(n1)
      margin.2<- qt(0.975,df=n2-1)* s2/sqrt(n2)
      sig.1<-t.test(yhat1, mu = 0, alternative = "two.sided",conf.level=0.95)
      sig.2<-t.test(yhat2, mu = 0, alternative = "two.sided",conf.level=0.95)
      if (m==1) {
        AME_self_gender<-data.frame(list(i,j,"female",m1,sig.1$p.value, m1-margin.1,m1+margin.1))
        colnames(AME_self_gender) <- c("index","group","gender","AME","sig","AME.low","AME.high")
        AME_self_gender<-rbind(AME_self_gender, list(i,j,"male",m2,sig.2$p.value, m2-margin.2,m2+margin.2))
        
      }
      else{
        AME_self_gender<-rbind(AME_self_gender, list(i,j,"female",m1,sig.1$p.value, m1-margin.1,m1+margin.1))
        AME_self_gender<-rbind(AME_self_gender, list(i,j,"male",m2,sig.2$p.value, m2-margin.2,m2+margin.2))
      }
      m<-m+1
      
    }
  }
  ### plot*********************************
  
  
  if ("discipline_new" %in% names(model$frame)) {
    AME_self_gender[AME_self_gender$index=="discipline_new", ]$group <- 
      plyr::mapvalues(AME_self_gender[AME_self_gender$index=="discipline_new", ]$group, 
                      from=c('Computer and information sciences', 'Biological sciences',
                             'Clinical medicine', 'Physical sciences and astronomy',
                             'Chemical sciences', 'Other engineering and technologies',
                             'Other social sciences', 'Educational sciences', 'Mathematics',
                             'Basic medical research', 'Materials engineering',
                             'Mechanical engineering', 'Other natural sciences',
                             'Earth and related environmental sciences', 'Health sciences',
                             'Agricultural Sciences', 'Psychology',
                             'Sociology and political science', 'Humanities',
                             'Electrical engineering, electronic engineering,  information engineering ',
                             'Chemical engineering', 'Economics', 'unknown'), 
                      to=c("Com", "Bio", "Clinic", "Phys", "ChemS", "Other_eng", "Other_soc", "Edu", "Math", "Medi", "Mater", "Mech", "Other_nat", "Earth", "Health", "Agri", "Psy", "Socio", "Human", "Elec",
                           "ChemE","Econ","Unk"))
  } else {
    
    AME_self_gender[AME_self_gender$index=="discipline_new", ]$group <- plyr::mapvalues(AME_self_gender[AME_self_gender$index=="discipline_new", ]$group, from=c("Medical and Health Sciences", "Engineering and Technology","Natural Sciences", "Social Sciences","Humanities","Agricultural Sciences","No_discipline_assigned" ), 
                                                                                        to=c("Med & Hea", "Eng & Tec","Natur", "Soci", "Hum","Agri","No_dis"))
  }
  
  
  
  
  x<-AME_self_gender
  
  y<-x %>% filter(index!="Jr_Quantile")
  x<-x %>% filter(index=="Jr_Quantile") %>% arrange(match(group, c("Q1", "Q2", "Q3","Q4","Others")))
  x<-rbind(y,x)
  
  
  if ("academic_age" %in% names(model$frame)){
    y<-x %>% filter(index!="academic_age")
    x<-x[x$index=="academic_age",] %>%arrange(match(group, c("1", "2", "3")))
    x<-rbind(y,x)  
  }
  
  if ("pub_before_cate" %in% names(model$frame)){
    y<-x %>% filter(index!="pub_before_cate")
    x<-x[x$index=="pub_before_cate",] %>%arrange(match(group, c("0", "1", "2+")))
    x<-rbind(y,x)  
  }     
  
  
  
  y<-x %>% filter(index!="cohort")
  x<-x[x$index=="cohort",] %>%
    arrange(match(group, c("2012", "2013", "2014","2015","2016")))
  x<-rbind(x,y)
  x$index<-factor(x$index,levels = index )
  
  
  
  return(x)
}


marginal_effect_gender_zinb_gender_model <- function(model_list,data,model_type,re,allow){
  
  model_comparision<-data.frame(matrix(ncol = 10))
  colnames(model_comparision) <- c("Model","gender","predict","sig","predict.lower","predict.upper","AME","AME.sig","AME.low","AME.high")
  
  
  for (i in seq(1, length(model_list), 1)) {
    modelX<-model_list[[i]]
    mf <- model.frame(modelX)
    
    model<-modelX
    
    
    mf_1 <- mf
    mf_2 <- mf
    mf_1$gender <- "female"
    mf_2$gender <- "male"
    
    if (model_type=="zero_inflation"){
      yhat1 <- predict(model, newdata = mf_1,type="zprob",re.form= re,allow.new.levels = allow)
      yhat2 <- predict(model, newdata = mf_2,type="zprob",re.form= re,allow.new.levels = allow)
    }
    
    else if (model_type=="conditional"){
      
      yhat1 <- predict(model, newdata = mf_1,type="conditional",re.form= re,allow.new.levels = allow)
      yhat2 <- predict(model, newdata = mf_2,type="conditional",re.form= re,allow.new.levels = allow)
      
    }
    else{
      yhat1 <- predict(model, newdata = mf_1,type="response",re.form= re,allow.new.levels = allow)
      yhat2 <- predict(model, newdata = mf_2,type="response",re.form= re,allow.new.levels = allow)
      
    }
    
    x1<-meanCI(yhat1)
    x2<-meanCI(yhat2)
    
    
    n1<-length(yhat1)
    n2<-length(yhat2)
    
    
    m1<-mean(yhat1)
    s1<-sd(yhat1)
    
    m2<-mean(yhat2)
    s2<-sd(yhat2)
    
    sp = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
    margin<- qt(0.975,df=n1+n2-1)*sqrt(sp/n1 + sp/n2)
    if (s1==0 |s2==0){
      sig<-0
    }
    else{
      
      sig<-t.test(yhat1,yhat2,alternative="two.sided",conf.level=0.95)
      sig<-sig$p.value
    }
    
    
    if (i==0) {
      model_comparision<-data.frame(list(i,"Female",m1,x1$result$p,x1$result$lower,x1$result$upper, m1-m2,sig,  m1-m2-margin, m1-m2+margin))
      colnames(model_comparision) <- c("Model","gender","predict","sig","predict.lower","predict.upper","AME_F_M","AME.sig","AME.low","AME.high")
      model_comparision<-rbind(model_comparision, list(i,"Male",m2,x2$result$p,x2$result$lower,x2$result$upper, m2-m1,sig, m2-m1-margin,m2-m1+margin))
      
      
    }
    else{
      model_comparision<-rbind(model_comparision, list(i,"Female",m1,x1$result$p,x1$result$lower,x1$result$upper,  m1-m2,sig,  m1-m2-margin, m1-m2+margin))
      model_comparision<-rbind(model_comparision, list(i,"Male",m2,x2$result$p,x2$result$lower,x2$result$upper, m1-m2,sig, m1-m2-margin,m1-m2+margin))
    }
    
    
  }
  return(model_comparision)
}



#**********************************************
#****1. reading data
#**********************************************
data_first_author_robust <- read.csv(file = '.\\processed_data.csv')

data_first_author_robust_ctr20 <- read.csv(file = '.\\processed_data_ctr20.csv')

ctr20<-unique(data_first_author_robust_ctr20$most_ctr)



#**********************************************
#****2. data_preprocessong 
#**********************************************
data_first_author_robust$academic_age<-as.character(data_first_author_robust$academic_age)
data_first_author_robust$others_first<-as.character(data_first_author_robust$others_first)
data_first_author_robust$gender<-relevel(factor(data_first_author_robust$gender), ref = "male")
data_first_author_robust$Jr_Quantile<-relevel(factor(data_first_author_robust$Jr_Quantile), ref = "Q4")
data_first_author_robust$cohort<-relevel(factor(data_first_author_robust$cohort), ref = "2012")
data_first_author_robust$colla_ctr_Y<-relevel(factor(data_first_author_robust$colla_ctr_Y), ref = "N")
data_first_author_robust$colla_aff_Y<-relevel(factor(data_first_author_robust$colla_aff_Y), ref = "N")
data_first_author_robust$academic_age<-relevel(factor(data_first_author_robust$academic_age), ref = "1")
data_first_author_robust$others_first<-relevel(factor(data_first_author_robust$others_first), ref = "0")
#data_first_author_robust$len_tweet_cate<-relevel(factor(data_first_author_robust$len_tweet_cate), ref = "Low")

data_first_author_robust[,c("author_cnt")] <- scale(data_first_author_robust[,c("author_cnt")])
data_first_author_robust[,c("pub_order")] <- scale(data_first_author_robust[,c("pub_order")])
data_first_author_robust$pub_before_cate <- ifelse(data_first_author_robust$pub_before==0, "0",
                                                   ifelse(data_first_author_robust$pub_before>1, "2+", "1"))
data_first_author_robust[,c("max_coa_age")] <- scale(data_first_author_robust[,c("max_coa_age")])
data_first_author_robust$max_coa_fncr_5y_log <- log(data_first_author_robust$max_coa_fncr_5y + 1)
#data_first_author_robust[,c("max_coa_fncr_5y")] <- scale(data_first_author_robust[,c("max_coa_fncr_5y")])

data_first_author_robust$tw_others_num_ori <- data_first_author_robust$tw_others_num
data_first_author_robust[,c("tw_others_num")] <- scale(data_first_author_robust[,c("tw_others_num")]) 

data_first_author_robust["len_tweet"][is.na(data_first_author_robust["len_tweet"])] <- 0
data_first_author_robust$len_tweet_ori <- data_first_author_robust$len_tweet
#data_first_author_robust$len_tweet <- scale(data_first_author_robust[,c("len_tweet")])
#**************************
data_first_author_robust$discipline<-relevel(factor(data_first_author_robust$discipline), ref = "No_discipline_assigned")

data_first_author_robust$discipline_new<-data_first_author_robust$discipline_new_pub
data_first_author_robust$discipline_new<-relevel(factor(data_first_author_robust$discipline_new), ref = "unknown")
data_first_author_robust$Jr_Quantile<-relevel(factor(data_first_author_robust$Jr_Quantile), ref = "Others")

field_discipline<- data.frame(
  field = c('Agricultural', 'Engineering', 'Engineering', 'Engineering', 'Engineering', 'Engineering','Humanities','Medical',"Medical","Medical",
            "Natural","Natural","Natural","Natural","Natural","Natural","Natural","Social","Social","Social","Social","Social","Unk"),
  group = c("Agri", "ChemE","Elec","Mater","Mech","Other_eng","Human","Clinic","Health","Medi","Bio","ChemS","Com", "Earth","Math","Phys","Other_nat","Econ",
            "Edu", "Psy","Socio","Other_soc","Unk")
)

data_first_author_robust_noselection<-data_first_author_robust
data_first_author_robust$average_tw<-data_first_author_robust$len_tweet_ori/data_first_author_robust$Original_Tweeters
data_first_author_robust["average_tw"][is.na(data_first_author_robust["average_tw"])] <- 0
data_first_author_robust<-data_first_author_robust[data_first_author_robust$average_tw<=15,]


#data_MERM_processed_1_S$author_cnt_grp<-relevel(factor(data_MERM_processed
#*************************ctr-level**************************
ctr20 <- data_first_author_robust %>%
  group_by(most_ctr) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  slice_head(n = 20)  # Select the top 20 countries
ctr20<-ctr20$most_ctr
data_first_author_robust_ctr20<-data_first_author_robust[data_first_author_robust$most_ctr %in% ctr20,]







#**************************************************
#****1. For odd ratios of oneline visibility 
#**************************************************



first_author_glmm_withtw_rec_1 <- glmmTMB(len_tweet_ori ~ gender*(Jr_Quantile + discipline_new + cohort+ pub_before_cate + author_cnt  +colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y_log+max_coa_age)+ (1+gender|most_ctr), 
                                        data = data_first_author_robust, family = nbinom2,zi = ~ gender*(Jr_Quantile + discipline_new + cohort+ pub_before_cate + author_cnt  +colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y_log+max_coa_age)+ (1+gender|most_ctr),
                                        REML=TRUE)


first_author_glmm_withtw_rec_ori <- glmmTMB(len_tweet_ori ~ gender*(Jr_Quantile + discipline_new + cohort+ pub_before_cate + author_cnt  +colla_ctr_Y+colla_aff_Y)+ (1+gender|most_ctr), 
                                        data = data_first_author_robust, family = nbinom2,zi = ~ gender*(Jr_Quantile + discipline_new + cohort+ pub_before_cate + author_cnt  +colla_ctr_Y+colla_aff_Y)+ (1+gender|most_ctr),
                                        REML=TRUE) 



# pairwaise regression************************************************************
set.seed(10)
first_author_glmm_withtw_0 <- glmmTMB(len_tweet_ori ~ gender, 
                                      data = data_first_author_robust, family = nbinom2,zi = ~ gender,
                                      REML=TRUE)
mf_1 <- data_first_author_robust
mf_2 <- data_first_author_robust
mf_1$gender <- "female"
mf_2$gender <- "male"
mean(predict(first_author_glmm_withtw_rec, mf_1, type = "conditional",re.form=NULL))
mean(predict(first_author_glmm_withtw_rec, mf_2, type = "conditional",re.form=NULL))
mean(predict(first_author_glmm_withtw_6, mf_1, type = "conditional",re.form=NA))
mean(predict(first_author_glmm_withtw_6, mf_2, type = "conditional",re.form=NA))

mean(predict(first_author_glmm_withtw_0, mf_1, type = "response",re.form=NA))

first_author_glmm_withtw_1 <- glmmTMB(len_tweet_ori ~ gender*discipline_new, 
                                        data = data_first_author_robust, family = nbinom2,zi = ~ gender* discipline_new,
                                        REML=TRUE) 

first_author_glmm_withtw_2 <- glmmTMB(len_tweet_ori ~ gender*(cohort+discipline_new), 
                                      data = data_first_author_robust, family = nbinom2,zi = ~ gender*(cohort+discipline_new),
                                      REML=TRUE) 


first_author_glmm_withtw_3 <- glmmTMB(len_tweet_ori ~ gender*(cohort+discipline_new+Jr_Quantile), 
                                      data = data_first_author_robust, family = nbinom2,zi = ~ gender*(cohort+discipline_new+Jr_Quantile),
                                      REML=TRUE) 
first_author_glmm_withtw_4 <- glmmTMB(len_tweet_ori ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate), 
                                      data = data_first_author_robust, family = nbinom2,zi = ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate),
                                      REML=TRUE) 

first_author_glmm_withtw_6 <- glmmTMB(len_tweet_ori ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt), 
                                      data = data_first_author_robust, family = nbinom2,zi = ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt),
                                      REML=TRUE) 

first_author_glmm_withtw_5 <- glmmTMB(len_tweet_ori ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y), 
                                      data = data_first_author_robust, family = nbinom2,zi = ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y),
                                      REML=TRUE) 

first_author_glmm_withtw_7 <- glmmTMB(len_tweet_ori ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y_log), 
                                      data = data_first_author_robust, family = nbinom2,zi = ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y_log),
                                      REML=TRUE) 

first_author_glmm_withtw_8 <- glmmTMB(len_tweet_ori ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y_log+max_coa_age), 
                                      data = data_first_author_robust, family = nbinom2,zi = ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y_log+max_coa_age),
                                      REML=TRUE) 


tab_model(first_author_glmm_withtw_0,first_author_glmm_withtw_1,first_author_glmm_withtw_2,first_author_glmm_withtw_3,first_author_glmm_withtw_4,first_author_glmm_withtw_6,first_author_glmm_withtw_5,first_author_glmm_withtw_7,
          show.intercept = FALSE,show.aic = T, show.ci = FALSE, p.style = "stars",
          dv.labels = c("Model0","Model1", "Model2", "Model3", "Model4", "Model5", "Model6"),
          CSS = list(css.table = 'font-size: 8px;white-space:nowrap',
                     css.th="padding: 0.05cm",
                     css.tdata="white-space:nowrap",
                     css.col1="white-space: nowrap",
                     css.modelcolumn1="white-space: nowrap",
                     css.firsttablecol="wwhite-space: nowrap"
                     #css.tr td ="padding-bottom: 4px;"
          ))


#***********************************************
## 2.Marginal effect for logit model (0 vs no-0)  & zinb(count data):
#***********************************************

### 2.0 Marginal effects of gender in general (including model comparison)





### 2.1 Marginal effects of gender in general (including model comparison) 

Model_list=list(first_author_glmm_withtw_0,first_author_glmm_withtw_1,first_author_glmm_withtw_2,first_author_glmm_withtw_3,first_author_glmm_withtw_4,first_author_glmm_withtw_6,first_author_glmm_withtw_5,first_author_glmm_withtw_7,
                first_author_glmm_withtw_8)
n<-0
for (i in Model_list){
  #r2_values <- r.squaredGLMM(i)
  print (n)
  #print (r2_values[1])
  #print (r2_values[2])
  r2_values <- performance::r2(i)
  print (r2_values)
  n<-n+1
  
}

Model_list=list(first_author_glmm_0,first_author_glmm_1,first_author_glmm_2,first_author_glmm_3,first_author_glmm_4,first_author_glmm_5,first_author_glmm_6,first_author_glmm_7,
                first_author_glmm_8, first_author_glmm)
n<-0
for (i in Model_list){
  #r2_values <- r.squaredGLMM(i)
  print(summary(i))
  
}



model_marginal_gender<-marginal_effect_gender_zinb_gender_model(Model_list,data_first_author_robust,"response",NULL,TRUE)
x<-marginal_effect_gender_zinb_gender_model(list(first_author_glmm_withtw_8),data_first_author_robust,"response",NULL,TRUE)
model_marginal_gender<-na.omit(model_marginal_gender)
Model_index<-paste0("Model: ",(model_marginal_gender$Model-1))
model_marginal_gender<-cbind(model_marginal_gender,Model_index)

model_marginal_gender_condi<-marginal_effect_gender_zinb_gender_model(Model_list,data_first_author_robust,"conditional",NULL,TRUE)
model_marginal_gender_condi<-na.omit(model_marginal_gender_condi)
Model_index<-paste0("Model: ",(model_marginal_gender_condi$Model-1))
model_marginal_gender_condi<-cbind(model_marginal_gender_condi,Model_index)





Model_list.7=list(first_author_glmm_withtw_rec)

model_comparison_zi<-marginal_effect_gender_zinb_gender_model(Model_list.7,data_first_author_robust,"zero_inflation",NULL,TRUE)
model_comparison_zi<-na.omit(model_comparison_zi)
#model_comparison_zi$Model<-model_comparison_zi$Model-1

model_comparison_nb.1<-marginal_effect_gender_zinb_gender_model(Model_list.7,data_first_author_robust,"response",NA,TRUE)
model_comparison_nb.1<-na.omit(model_comparison_nb.1)
model_comparison_nb.1$Model_index<-"Model: 9 (excl. random effects)"
model_comparison_nb.1$Model<-10
model_comparison_nb.2<-marginal_effect_gender_zinb_gender_model(Model_list.7,data_first_author_robust,"response",NULL,TRUE)
model_comparison_nb.2<-na.omit(model_comparison_nb.2)
model_comparison_nb.2$Model_index<-"Model: 9 (incl. random effects)"
model_comparison_nb.2$Model<-11
#model_comparison_nb$Model<-model_comparison_nb$Model-1
model_marginal_gender<-rbind(model_marginal_gender,model_comparison_nb.1,model_comparison_nb.2)







ggplot(model_comparison)+
  geom_point( aes(y=AME_F_M,x=Model),color='#56B4E9',size=2, position=position_dodge(0.78), show.legend = FALSE)+
  geom_errorbar(aes(ymin=AME.high, ymax=AME.low,x=Model),color='#56B4E9', width=.1, position=position_dodge(0.78))+
  #geom_hline(yintercept=0.04)+
  scale_y_continuous(limits = c(0, 0.1),breaks=c(seq(0, 0.1,0.02)))+
  scale_x_continuous(limits = c(0,7),breaks=c(seq(0,7,1)))+
  coord_flip()+
  xlab('Model selection')+
  labs(y=expression(paste("Marginal effects of gender (", italic(" male-to-female")," ) in" ,bold(" Nan online mentions"))))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

#**************************************************
#' general gender gap in zi
#' + general gender gap in nb
#' 

res<-t(mapply(R_text, model_comparison_zi$AME,model_comparison_zi$gender,"p.p."))
model_comparison_zi<-cbind(model_comparison_zi,res)
model_comparison_zi <- model_comparison_zi %>% 
  rename("text.zi" = "1",
         "text.hjust" = "2")

sig.zi<-mapply(sig_star, model_comparison_zi$AME ,model_comparison_zi$AME.sig,model_comparison_zi$gender)
model_comparison_zi<-cbind(model_comparison_zi,sig.zi)
model_comparison_zi[,c("predict" ,"predict.lower","predict.upper","AME","AME.low","AME.high")]<-
  model_comparison_zi[,c("predict" ,"predict.lower","predict.upper","AME","AME.low","AME.high")]*100

ggplot(data=model_comparison_zi, aes(x=gender,color=gender)) +
  geom_point( aes(y=predict), stat='identity',size=2,position=position_dodge(0.78), show.legend = TRUE) + 
  geom_text(aes(y = max(AME) + 35,hjust=3, label =  paste0( text.zi,sig.zi),
                fontface=2),
            size =12/.pt, show.legend = FALSE)+
  geom_errorbar(aes(ymin=predict.lower, ymax=predict.upper), width=.1, position=position_dodge(0.78), show.legend = FALSE) +
  ylab("Predicted possibility of excess zero online mentions (p.p.)") +
  scale_y_continuous(limits = c(0, 40),breaks=c(seq(0, 40,10)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()


res<-t(mapply(R_text, model_comparison_nb$AME,model_comparison_nb$gender,"others"))
model_comparison_nb<-cbind(model_comparison_nb,res)
model_comparison_nb <- model_comparison_nb %>% 
  rename("text.nb" = "1",
         "text.hjust" = "2")
sig.nb<-mapply(sig_star, model_comparison_nb$AME ,model_comparison_nb$AME.sig,model_comparison_nb$gender)
model_comparison_nb<-cbind(model_comparison_nb,sig.nb)

ggplot(data=model_comparison_nb, aes(x=gender,color=gender,fill=gender)) +
  geom_bar( aes(y=predict), stat='identity',width=0.2,size=0.4,alpha=0.8,position="dodge2", show.legend = TRUE) + 
  geom_text(aes(y = max(AME) + 1.25,hjust=1.5, label =  paste0(text.nb,sig.nb),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=predict.lower, ymax=predict.upper), width=.2, size=1.5, position="dodge2", show.legend = FALSE) +
  ylab("Predicted counts of online mentions") +
  xlab("")+
  scale_y_continuous(limits = c(0, 1.3),breaks=c(seq(0,1.3,0.4)))+
  #scale_y_continuous(limits = c(0, 1),breaks=c(seq(0,1,0.2)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
    text = element_text(size=15)
  )


#*********model comparison in overall marginal effect of gender************

res<-t(mapply(R_text, model_marginal_gender$AME,model_marginal_gender$gender,"others"))
model_marginal_gender<-cbind(model_marginal_gender,res)
model_marginal_gender <- model_marginal_gender %>% 
  rename("text.nb" = "1",
         "text.hjust" = "2")
sig.nb<-mapply(sig_star, model_marginal_gender$AME ,model_marginal_gender$AME.sig,model_marginal_gender$gender)
model_marginal_gender<-cbind(model_marginal_gender,sig.nb)


res<-t(mapply(R_text, model_marginal_gender_condi$AME,model_marginal_gender_condi$gender,"others"))
model_marginal_gender_condi<-cbind(model_marginal_gender_condi,res)
model_marginal_gender_condi <- model_marginal_gender_condi %>% 
  rename("text.nb" = "1",
         "text.hjust" = "2")
sig.nb<-mapply(sig_star, model_marginal_gender_condi$AME ,model_marginal_gender$AME.sig,model_marginal_gender$gender)
model_marginal_gender_condi<-cbind(model_marginal_gender_condi,sig.nb)

ggplot(data=model_marginal_gender[model_marginal_gender$Model!=9,], aes(x=factor(Model-1),color=gender,fill=gender)) +
  #geom_bar( aes(y=predict), stat='identity',width=0.2,size=0.4,alpha=0.8,position="dodge2", show.legend = TRUE) +
  geom_bar( aes(y=predict), stat='identity',alpha=0.8,position="dodge2", show.legend = TRUE) + 
  geom_text(aes(y = max(predict) + 0.1, label =  paste0(text.nb,sig.nb),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=predict.lower, ymax=predict.upper), position="dodge2", show.legend = FALSE) +
  ylab("Predicted counts of online mentions") +
  xlab("Model")+
  scale_y_continuous(limits = c(0, 1.25),breaks=c(seq(0,1.25,0.5)))+
  #scale_y_continuous(limits = c(0, 1),breaks=c(seq(0,1,0.2)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
    text = element_text(size=15)
  )


ggplot(data=model_marginal_gender[(model_marginal_gender["Model"]==1)|(model_marginal_gender["Model"]==9),], aes(x=factor(Model-1),color=gender,fill=gender)) +
  #geom_bar( aes(y=predict), stat='identity',width=0.2,size=0.4,alpha=0.8,position="dodge2", show.legend = TRUE) +
  geom_bar( aes(y=predict), stat='identity',alpha=0.8,position="dodge2", show.legend = TRUE) + 
  geom_text(aes(y = max(predict) + 0.1, label =  paste0(text.nb,sig.nb),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=predict.lower, ymax=predict.upper), position=position_dodge(width=0.89), 
                show.legend = FALSE,width=0.6, size = 2) +
  ylab("Predicted counts of online mentions") +
  xlab("Model")+
  scale_y_continuous(limits = c(0, 1.5),breaks=c(seq(0,1.5,0.5)))+
  #scale_y_continuous(limits = c(0, 1),breaks=c(seq(0,1,0.2)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
    text = element_text(size=15),
    legend.position="bottom"
  )


#*********contry-level comparison in overall marginal effect of gender************

m<-1

mf_1<-data_first_author_robust_ctr20 
mf_2<-data_first_author_robust_ctr20
mf_1$gender<-"female"
mf_2$gender<-"male"
ctr_fe<-predict(first_author_glmm_withtw_rec, newdata = mf_1,type="conditional",re.form= NULL,allow.new.levels = TRUE)
ctr_m<-predict(first_author_glmm_withtw_rec, newdata = mf_2,type="conditional",re.form= NULL,allow.new.levels = TRUE)
mf_1<-cbind(mf_1,ctr_fe)
mf_2<-cbind(mf_2,ctr_m)

m=1
for (i in unique(data_first_author_robust_ctr20$most_ctr)){
  print(i)
  yhat1<-list(mf_1[mf_1$most_ctr==i,]$ctr_fe)[[1]]
  yhat2<-list(mf_2[mf_2$most_ctr==i,]$ctr_m)[[1]]
  n1<-length(yhat1)
  n2<-length(yhat2)
  
  
  m1<-mean(yhat1)
  s1<-sd(yhat1)
  
  m2<-mean(yhat2)
  s2<-sd(yhat2)
  
  margin.1<- qt(0.975,df=n1-1)* s1/sqrt(n1)
  margin.2<- qt(0.975,df=n2-1)* s2/sqrt(n2)
  sig.1<-t.test(yhat1, mu = 0, alternative = "two.sided",conf.level=0.95)
  sig.2<-t.test(yhat2, mu = 0, alternative = "two.sided",conf.level=0.95)
  
  sp = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
  margin<- qt(0.975,df=n1+n2-1)*sqrt(sp/n1 + sp/n2)
  sig<-t.test(yhat1,yhat2,alternative="two.sided",conf.level=0.95)
  
  if (m==1){
    ctr_marginal_gender<-data.frame(list(i,"Female",m1,sig.1$p.value,m1-margin.1,m1+margin.1, m1-m2,sig$p.value,  m1-m2-margin, m1-m2+margin))
    colnames(ctr_marginal_gender) <- c("Country","gender","predict","sig","predict.lower","predict.upper","AME_F_M","AME.sig","AME.low","AME.high")
    ctr_marginal_gender<-rbind(ctr_marginal_gender,list(i,"Male",m2,sig.2$p.value,m2-margin.2,m2+margin.2, m1-m2,sig$p.value,  m1-m2-margin, m1-m2+margin))
    
  }
  else{
    ctr_marginal_gender<-rbind(ctr_marginal_gender,list(i,"Female",m1,sig.1$p.value,m1-margin.1,m1+margin.1, m1-m2,sig$p.value,  m1-m2-margin, m1-m2+margin))
    
    ctr_marginal_gender<-rbind(ctr_marginal_gender,list(i,"Male",m2,sig.2$p.value,m2-margin.2,m2+margin.2, m1-m2,sig$p.value,  m1-m2-margin, m1-m2+margin))
  }
  m<-m+1
  
}



res<-t(mapply(R_text, ctr_marginal_gender$AME_F_M,ctr_marginal_gender$gender,"others"))
ctr_marginal_gender<-cbind(ctr_marginal_gender,res)
ctr_marginal_gender <- ctr_marginal_gender %>% 
  rename("text.nb" = "1",
         "text.hjust" = "2")
sig.nb<-mapply(sig_star, ctr_marginal_gender$AME_F_M ,ctr_marginal_gender$AME.sig,ctr_marginal_gender$gender)
ctr_marginal_gender<-cbind(ctr_marginal_gender,sig.nb)

ctr_marginal_gender$Country <- factor(ctr_marginal_gender$Country, levels = ctr20)
ggplot(data=ctr_marginal_gender, aes(x=factor(Country, levels=unique(ctr20)),color=gender,fill=gender)) +
  #geom_bar( aes(y=predict), stat='identity',width=0.2,size=0.4,alpha=0.8,position="dodge2", show.legend = TRUE) +
  geom_bar( aes(y=predict), stat='identity',alpha=0.8,position="dodge2", show.legend = TRUE) + 
  geom_text(aes(y = max(predict) + 0.1, label =  paste0(text.nb,sig.nb),fontface=2),
            size = 11/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_hline(yintercept=model_marginal_gender[(model_marginal_gender$Model==8)&(model_marginal_gender$gender=="Female"),"predict"], linetype="dashed",size=1, color = "#E69F00")+
  geom_hline(yintercept=model_marginal_gender[(model_marginal_gender$Model==8)&(model_marginal_gender$gender=="Male"),"predict"], linetype="dashed",size=1, color = "#56B4E9")+
  geom_errorbar(aes(ymin=predict.lower, ymax=predict.upper), position="dodge2", show.legend = FALSE) +
  ylab("Predicted counts of online mentions") +
  xlab("Country")+
  #scale_y_continuous(limits = c(0, 1.5),breaks=c(seq(0,1.5,0.5)))+
  #scale_y_continuous(limits = c(0, 1),breaks=c(seq(0,1,0.2)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
    text = element_text(size=15)
  )


#****************************************

ggplot(data=margin_pro_fa_count[(margin_pro_fa_count$group!="Others")&(margin_pro_fa_count$group!="No_dis"),], aes(x=fct_inorder(group),color="#56B4E9")) +
  facet_wrap(~index,scales="free")+
  #geom_point( aes(y=-`AME.gender`),size=2, position=position_dodge(0.78), show.legend = FALSE) + 
  geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=-AME.high.gender, ymax=-AME.low.gender), width=.1, position=position_dodge(0.78)) +
  geom_hline(yintercept=0, linetype="dashed")+
  ylab("Marginal effects of gender in self-promotion") +
  scale_y_continuous(limits = c(0.7, 1),breaks=c(seq(0.7, 1,0.1)))+
  #scale_y_continuous(limits = c(-0.12, 0),breaks=c(seq(-0.12, 0, 0.02)))+
  #scale_y_continuous(limits = c(-0.4, 0.4),breaks=c(seq(-0.4, 0.4, 0.2)))+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  theme_bw()

#'*************************************************




#'**************************************************
margin_pro_fa_zi<-marginal_effect_gender_zinb(first_author_glmm_withtw_rec,data_first_author_robust,"zero_inflation",NULL,TRUE)
margin_pro_fa_zi_gender<-marginal_effect_gender_zinb_gender(first_author_glmm_withtw_rec,data_first_author_robust,"zero_inflation",NULL,TRUE)



################### nb part*********************

## no cs discipline


margin_pro_fa_count<-marginal_effect_gender_zinb(first_author_glmm_withtw_7,data_first_author_robust,"response",NULL,TRUE)

margin_pro_fa_count_gender<-marginal_effect_gender_zinb_gender(first_author_glmm_withtw_7,data_first_author_robust,"response",NULL,TRUE)

data_nocom<-data_first_author_robust[data_first_author_robust$discipline_new!="Computer and information sciences",]
margin_pro_fa_count_gender_nocom<-marginal_effect_gender_zinb_gender(first_author_glmm_withtw_rec,data_nocom,"conditional",NULL,TRUE)

#### overall effect********************

nb_overall<-marginal_effect_gender_zinb_gender_model(list(first_author_glmm_withtw_0,first_author_glmm_withtw_8,first_author_glmm_withtw_rec),data_first_author_robust,"response",NULL,TRUE)
nb_overall<-na.omit(nb_overall)
res<-t(mapply(R_text, nb_overall$AME,nb_overall$gender,"others"))
nb_overall<-cbind(nb_overall,res)
nb_overall <- nb_overall %>% 
  dplyr::rename("text.nb" = "1",
         "text.hjust" = "2")
sig.nb<-mapply(sig_star, nb_overall$AME ,nb_overall$AME.sig,nb_overall$gender)
nb_overall<-cbind(nb_overall,sig.nb)

#### when we have estimates for all models


nb_overall<-model_marginal_gender[(model_marginal_gender$Model==8)|(model_marginal_gender$Model==1),]
nb_overall$index_new<-"Overall"



ggplot(data=nb_overall, aes(x=factor(Model-1),color=gender,fill=gender)) +
  facet_wrap(~factor(index_new ),scale="free")+
  #geom_bar( aes(y=predict), stat='identity',width=0.2,size=0.4,alpha=0.8,position="dodge2", show.legend = TRUE) +
  geom_bar( aes(y=predict), stat='identity',alpha=0.8,position="dodge2", show.legend = TRUE) + 
  geom_text(aes(y = max(predict) + 0.35, label =  paste0(text.nb,sig.nb),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=predict.lower, ymax=predict.upper), position=position_dodge(width=0.9), show.legend = FALSE,size=1.5,width=0.5) +
  ylab("Predicted counts of online mentions") +
  xlab("")+
  scale_y_continuous(limits = c(0, 1.5),breaks=c(seq(0,1.5,0.5)))+
  #scale_y_continuous(limits = c(0, 1),breaks=c(seq(0,1,0.2)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(
    axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
    text = element_text(size=15)
  )




#*********************************************************************


#'******************new
margin_pro_fa_count_gender<-margin_pro_fa_count_gender[(margin_pro_fa_count_gender$group!="Others")&(margin_pro_fa_count_gender$group!="Unk"),]
margin_pro_fa_count_gender<-margin_pro_fa_count_gender %>%
  group_by(index) %>%
  mutate(count = n_distinct(group))
margin_pro_fa_count_gender<-margin_pro_fa_count_gender %>% mutate( fac = count / 6)
margin_pro_fa_count_gender <- margin_pro_fa_count_gender %>%
 mutate(index_new = dplyr::recode(index,
                      "cohort" = "Cohort",
                      "discipline_new" = "Discipline",
                      "Jr_Quantile" = "Journal Rank",
                      "academic_age"="Academic Age"
  ))



df2<-merge(margin_pro_fa_count_gender, margin_pro_fa_count, by = c('index','group'))
#res<-t(mapply(R_text, df2$AME.gender,df2$gender,"others"))
#df2<-cbind(df2,res)
#df2 <- df2 %>% 
# rename("text.self" = "1",
#         "text.hjust" = "2")

margin_text<-t(mapply(R_text, df2$AME.gender,df2$gender,"others"))
margin_sig<-mapply(sig_star, df2$AME.gender,df2$sig.gender,df2$gender)
df2<-cbind(df2,margin_text)
df2<-df2%>% 
  dplyr::rename("margin_text" = "1",
       "text.hjust" = "2")
df2<-cbind(df2,margin_sig)
df2 <- df2 %>%
  mutate(index_new = dplyr::recode(index,
                            "cohort" = "Cohort",
                            "discipline_new" = "Discipline",
                            "Jr_Quantile" = "Journal Rank",
                            "pub_before_cate"="Previous Publications"
  ))
##,
color="azure4"
#ggplot(data=df2, aes(x=fct_inorder(group),color=gender,fill=gender, width = 0.9*fac)) +
ggplot(data=df2[df2$index_new!="Discipline",], aes(x=fct_inorder(group),color=gender,fill=gender)) +
  facet_wrap(~factor(index_new,c("Cohort","Previous Publications","Journal Rank")),scale="free")+
  geom_bar( aes(y=AME), stat='identity',size=0.8,alpha=0.8,position="dodge2", show.legend = TRUE) + 
  geom_text(aes(y = max(AME) + 0.1, label =  paste0(margin_text,margin_sig),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=AME.low, ymax=AME.high),size=1.5, position="dodge2", show.legend = FALSE) +
  ylab("Predicted counts of online mentions") +
  xlab("") +
  #scale_y_continuous(limits = c(0, 2),breaks=c(seq(0,2,0.4)))+
  #scale_y_continuous(limits = c(0, 1),breaks=c(seq(0,1,0.2)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=13),axis.text.y = element_text(size=14),
        text = element_text(size=15),,strip.text = element_text(size = 14))



### specific for discipline : Aug 2024##########################################
df2_dis<-df2[(df2$group!="Unk"),]
df2_dis <- df2_dis[df2_dis$index_new=="Discipline",]%>%left_join(field_discipline,by="group")
df2_dis <- df2_dis %>%
  mutate(group_ordered = ifelse(startsWith(group, "Other"), paste0("X", group), group))%>%
  arrange(field, group_ordered) %>%  # First order the dataframe by A and B
  mutate(group_ordered = factor(group_ordered, levels = unique(group_ordered)))
ggplot(data=df2_dis, aes(x=fct_inorder(group),color=gender,fill=gender)) +
  facet_wrap(~index_new,scale="free")+
  geom_bar( aes(y=AME), stat='identity',size=0.8,alpha=0.8,position="dodge2", show.legend = TRUE) + 
  geom_text(aes(y = max(AME) + 0.3, label =  paste0(margin_text,margin_sig),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=AME.low, ymax=AME.high),size=1.5, position="dodge2", show.legend = FALSE) +
  ylab("Predicted counts of online mentions") +
  xlab("") +
  #scale_y_continuous(limits = c(0, 2),breaks=c(seq(0,2,0.4)))+
  #scale_y_continuous(limits = c(0, 4.2),breaks=c(seq(0,4,1)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=13),axis.text.y = element_text(size=14),
        text = element_text(size=15),,strip.text = element_text(size = 14))

######################################################################










