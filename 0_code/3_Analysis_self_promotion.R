

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
data_first_author_robust <- read.csv(file = '.\\1_data\\processed_data_demo.csv')

data_first_author_robust_ctr20 <- read.csv(file = '.\\1_data\\processed_data_ctr20_demo.csv')


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








#**********************************************
#****2. For odd ratios of self-promotion / 
#****    co-authors' promotion /public media's promotion
#**********************************************


### first author publication******************************************************************

first_author_glmm <- glmmTMB(self_pro ~ gender*(others_first+tw_others_num+ cohort+Jr_Quantile + discipline_new  + pub_before_cate + author_cnt+colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y_log)+ (1+gender |most_ctr), 
                                data = data_first_author_robust, family = binomial)

##with coauthors (with_coauthor==1)
first_author_glmm_coauthor <- glmmTMB(coauthor_pro ~ gender*(tw_others_num+ cohort+Jr_Quantile + discipline_new  + pub_before_cate + author_cnt+colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y)+ (1+gender |most_ctr), 
                             data = data_first_author_robust[data_first_author_robust$with_coauthor==1,], family = binomial)


first_author_glmm_soc <- glmmTMB(soc_or_edu_pro ~ gender*(tw_others_num+ cohort+Jr_Quantile + discipline_new  + pub_before_cate + author_cnt+colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y)+ (1+gender |most_ctr), 
                                      data = data_first_author_robust, family = binomial)


first_author_onlyself <- glmmTMB(tw_only_self ~ gender*(others_first+tw_others_num+ cohort+Jr_Quantile + discipline  + academic_age + author_cnt+colla_ctr_Y+colla_aff_Y+pub_order)+ (1+gender |most_ctr), 
                             data = data_first_author_robust, family = binomial)


# pairwaise regression for self-pro************************************************************

first_author_glmm_0 <- glmmTMB(self_pro ~ gender, 
                             data = data_first_author_robust, family = binomial)


first_author_glmm_1 <- glmmTMB(self_pro ~ gender*discipline_new, 
                                      data = data_first_author_robust, family = binomial)

first_author_glmm_2 <- glmmTMB(self_pro ~ gender*(cohort+discipline_new), 
                               data = data_first_author_robust, family = binomial)

first_author_glmm_3 <- glmmTMB(self_pro ~ gender*(cohort+discipline_new+Jr_Quantile), 
                               data = data_first_author_robust, family = binomial)

first_author_glmm_4 <- glmmTMB(self_pro ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate), 
                               data = data_first_author_robust, family = binomial)
first_author_glmm_5 <- glmmTMB(self_pro ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt), 
                               data = data_first_author_robust, family = binomial)
first_author_glmm_6 <- glmmTMB(self_pro ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y), 
                               data = data_first_author_robust, family = binomial)

first_author_glmm_7 <- glmmTMB(self_pro ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y_log), 
                               data = data_first_author_robust, family = binomial)

first_author_glmm_8<- glmmTMB(self_pro ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y+max_coa_fncr_5y_log+others_first+tw_others_num), 
                               data = data_first_author_robust, family = binomial)

first_author_glmm_9 <- glmmTMB(self_pro ~ gender*(cohort+discipline_new+Jr_Quantile+pub_before_cate+ author_cnt  +colla_ctr_Y+colla_aff_Y+others_first+tw_others_num+max_coa_fncr_5y_log+max_coa_age), 
                               data = data_first_author_robust, family = binomial)




tab_model(first_author_glmm_0,first_author_glmm_1,first_author_glmm_2,first_author_glmm_3,first_author_glmm_4,first_author_glmm_5,
          first_author_glmm_6,first_author_glmm_7,first_author_glmm_8,first_author_glmm_9, first_author_glmm,
          show.intercept = TRUE,show.aic = T, show.ci = FALSE, p.style = "stars",
          dv.labels = c("Model0","Model1", "Model2", "Model3", "Model4", "Model5", "Model6", "Model7","Model_full"),
          CSS = list(css.table = 'font-size: 8px;white-space:nowrap',
                     css.th="padding: 0.05cm",
                     css.tdata="white-space:nowrap",
                     css.col1="white-space: nowrap",
                     css.modelcolumn1="white-space: nowrap",
                     css.firsttablecol="wwhite-space: nowrap"
                     #css.tr td ="padding-bottom: 4px;"
          ))
#*******************************************************************************


#***********************************************
## 3.Marginal effects of gender for model comparison) 
#***********************************************



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



res<-t(mapply(R_text, model_marginal_gender$AME,model_marginal_gender$gender,"others"))
model_marginal_gender<-cbind(model_marginal_gender,res)
model_marginal_gender <- model_marginal_gender %>% 
  rename("text.nb" = "1",
         "text.hjust" = "2")
sig.nb<-mapply(sig_star, model_marginal_gender$AME ,model_marginal_gender$AME.sig,model_marginal_gender$gender)
model_marginal_gender<-cbind(model_marginal_gender,sig.nb)



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


#***********************************************
## 4. comparision by discipline 
#***********************************************



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











#***********************************************
## 5. contry-level comparison in overall marginal effect of gender on self-promotion
#***********************************************



m<-1

mf_1<-data_first_author_robust_ctr20 
mf_2<-data_first_author_robust_ctr20
mf_1$gender<-"female"
mf_2$gender<-"male"
ctr_fe_self<-predict(first_author_glmm, newdata = mf_1,type="response",re.form= NULL,allow.new.levels = TRUE)
ctr_m_self<-predict(first_author_glmm, newdata = mf_2,type="response",re.form= NULL,allow.new.levels = TRUE)

refer_fe<-predict(first_author_glmm, newdata = mf_1,type="response",re.form= NA,allow.new.levels = TRUE)
refer_m<-predict(first_author_glmm, newdata = mf_2,type="response",re.form= NA,allow.new.levels = TRUE)
#mf_1<-subset(mf_1, select = -c(ctr_fe_self) )
#mf_2<-subset(mf_2, select = -c(ctr_m_self) )
mf_1<-cbind(mf_1,ctr_fe_self)
mf_2<-cbind(mf_2,ctr_m_self)

m=1
for (i in unique(data_first_author_robust_ctr20$most_ctr)){
  print(i)
  yhat1<-list(mf_1[mf_1$most_ctr==i,]$ctr_fe_self)[[1]]
  yhat2<-list(mf_2[mf_2$most_ctr==i,]$ctr_m_self)[[1]]
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
    ctr_marginal_gender_self<-data.frame(list(i,"Female",m1,sig.1$p.value,m1-margin.1,m1+margin.1, m1-m2,sig$p.value,  m1-m2-margin, m1-m2+margin))
    colnames(ctr_marginal_gender_self) <- c("Country","gender","predict","sig","predict.lower","predict.upper","AME_F_M","AME.sig","AME.low","AME.high")
    ctr_marginal_gender_self<-rbind(ctr_marginal_gender_self,list(i,"Male",m2,sig.2$p.value,m2-margin.2,m2+margin.2, m1-m2,sig$p.value,  m1-m2-margin, m1-m2+margin))
    
  }
  else{
    ctr_marginal_gender_self<-rbind(ctr_marginal_gender_self,list(i,"Female",m1,sig.1$p.value,m1-margin.1,m1+margin.1, m1-m2,sig$p.value,  m1-m2-margin, m1-m2+margin))
    
    ctr_marginal_gender_self<-rbind(ctr_marginal_gender_self,list(i,"Male",m2,sig.2$p.value,m2-margin.2,m2+margin.2, m1-m2,sig$p.value,  m1-m2-margin, m1-m2+margin))
  }
  m<-m+1
  
}

ctr_marginal_gender_self

ctr_marginal_gender_self[,c("predict" ,"predict.lower","predict.upper","AME_F_M" ,"AME.low","AME.high")]<-
  ctr_marginal_gender_self[,c("predict" ,"predict.lower","predict.upper","AME_F_M" ,"AME.low","AME.high")]*100

res<-t(mapply(R_text, ctr_marginal_gender_self$AME_F_M,ctr_marginal_gender_self$gender,"others"))
ctr_marginal_gender_self<-cbind(ctr_marginal_gender_self,res)
ctr_marginal_gender_self <- ctr_marginal_gender_self %>% 
  rename("text.nb" = "1",
         "text.hjust" = "2")
sig.nb<-mapply(sig_star, ctr_marginal_gender_self$AME_F_M ,ctr_marginal_gender_self$AME.sig,ctr_marginal_gender_self$gender)
ctr_marginal_gender_self<-cbind(ctr_marginal_gender_self,sig.nb)



ggplot(data=ctr_marginal_gender_self, aes(x=factor(Country, levels=ctr20),color=gender,fill=gender)) +
  #geom_bar( aes(y=predict), stat='identity',width=0.2,size=0.4,alpha=0.8,position="dodge2", show.legend = TRUE) +
  geom_point( aes(y=predict), stat='identity',size=2,position=position_dodge(0.78), show.legend = TRUE) + 
  geom_text(aes(y = max(predict) + 2, label =  paste0(text.nb,sig.nb),fontface=2),
            size = 11/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_hline(yintercept=mean(refer_fe)*100, linetype="dashed",size=1, color = "#E69F00")+
  geom_hline(yintercept=mean(refer_m)*100, linetype="dashed",size=1, color = "#56B4E9")+
  geom_errorbar(aes(ymin=predict.lower, ymax=predict.upper), position="dodge2", show.legend = FALSE) +
  ylab("Predicted probabilities of gender in self-promotion (p.p.)") +
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

#**************************************************




#***********************************************
## 7. predicted possbilities and marginal effects of gender
#***********************************************

ggplot(data=margin_pro_fa_self[(margin_pro_fa_self$group!="Others")&(margin_pro_fa_self$group!="Unk"),], aes(x=fct_inorder(group),color="#56B4E9")) +
  facet_wrap(~index,scales="free")+
  geom_point( aes(y=AME.gender),size=2, position=position_dodge(0.78), show.legend = FALSE) + 
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=AME.low.gender, ymax=AME.high.gender), width=.1, position=position_dodge(0.78)) +
  geom_hline(yintercept=0, linetype="dashed")+
  ylab("Marginal effects of gender in self-promotion") +
  #scale_y_continuous(limits = c(0.7, 1),breaks=c(seq(0.7, 1,0.1)))+
  #scale_y_continuous(limits = c(-0.12, 0),breaks=c(seq(-0.12, 0, 0.02)))+
  scale_y_continuous(limits = c(0, 0.02),breaks=c(seq(0, 0.02, 0.004)))+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  theme_bw()

### seperated by gender*************************************************
margin_pro_fa_self_gender<-margin_pro_fa_self_gender[(margin_pro_fa_self_gender$group!="Others")&(margin_pro_fa_self_gender$group!="ukn"),]
margin_pro_fa_self_gender <- margin_pro_fa_self_gender %>%
  mutate(index_new = dplyr::recode(index,
                            "cohort" = "Cohort",
                            "discipline_new" = "Discipline",
                            "Jr_Quantile" = "Journal Rank",
                            
                            "pub_before_cate"="Previous Publications"
  ))
df2_self<-merge(margin_pro_fa_self_gender, margin_pro_fa_self, by = c('index','group'))

#df2_self[df2_self$gender=="female",c("AME.gender","AME.low.gender","AME.high.gender")]<-
#  -df2_self[df2_self$gender=="female",c("AME.gender","AME.low.gender","AME.high.gender")]
df2_self <- df2_self %>%
  mutate(index_new = dplyr::recode(index,
                            "cohort" = "Cohort",
                            "discipline_new" = "Discipline",
                            "Jr_Quantile" = "Journal Rank",
                            "academic_age"="Academic Age",
                            "pub_before_cate"="Previous Publications"
  ))
res<-t(mapply(R_text, df2_self$AME.gender,df2_self$gender,"p.p."))
df2_self<-cbind(df2_self,res)
df2_self <- df2_self %>% 
  rename("text.self" = "1",
         "text.hjust" = "2")
margin_sig_self<-mapply(sig_star, df2_self$AME.gender,df2_self$sig.gender,df2_self$gender)
df2_self<-cbind(df2_self,margin_sig_self)
df2_self[,c("AME" ,"AME.low","AME.high")]<-
  df2_self[,c("AME" ,"AME.low","AME.high")]*100



ggplot(data=df2_self[df2_self$index_new!="Discipline",], aes(x=fct_inorder(group),color=gender)) +
  facet_wrap(~factor(index_new,c("Cohort","Previous Publications","Journal Rank")),scales="free")+
  geom_point( aes(y=AME),size=2, position=position_dodge(0.78), show.legend = TRUE) + 
  geom_text(aes(y = max(AME)+0.5, label =  paste0(text.self,margin_sig_self),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=AME.low, ymax=AME.high), width=0.3, position=position_dodge(0.78), show.legend = FALSE) +
  ylab("Predicted probabilities of self-promotion by gender (p.p.)") +
  xlab("") +
  #scale_y_continuous(label=scales::percent_format(accuracy =  1),limits = c(0, 0.06),breaks=c(seq(0, 0.06,0.02)))+
  scale_y_continuous(limits = c(0, 2.5),breaks=c(seq(0,2.5,1)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
        text = element_text(size=15))


### specific for discipline : Aug 2024##########################################


df2_self_dis <- df2_self[df2_self$index_new=="Discipline",]%>%left_join(field_discipline,by="group")
df2_self_dis <- df2_self_dis[df2_self_dis$group!="Unk",] %>%
  mutate(group_ordered = ifelse(startsWith(group, "Other"), paste0("X", group), group))%>%
  arrange(field, group_ordered) %>%  # First order the dataframe by A and B
  mutate(group_ordered = factor(group_ordered, levels = unique(group_ordered)))



ggplot(data=df2_self_dis, aes(x=fct_inorder(group),color=gender)) +
  facet_wrap(~factor(index_new),scales="free")+
  geom_point( aes(y=AME),size=2, position=position_dodge(0.78), show.legend = TRUE) + 
  geom_text(aes(y = max(AME)+0.7, label =  paste0(text.self,margin_sig_self),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  geom_errorbar(aes(ymin=AME.low, ymax=AME.high),width=0.5,size=1, position=position_dodge(0.8), show.legend = FALSE) +
  #ylab("Predicted probabilities of self-promotion (p.p.)") +
  xlab("") +
  #scale_y_continuous(limits = c(0, 2),breaks=c(seq(0,2,0.4)))+
  scale_y_continuous(limits = c(0, 9),breaks=c(seq(0,8,2)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=13),axis.text.y = element_text(size=14),
        text = element_text(size=15),,strip.text = element_text(size = 14))


######################################################################



#### AUg 2024: for coauthors' promotiom*********************************

margin_pro_fa_coauthor_gender<-margin_pro_fa_coauthor_gender[(margin_pro_fa_coauthor_gender$group!="Others")&(margin_pro_fa_coauthor_gender$group!="ukn"),]
margin_pro_fa_coauthor_gender <- margin_pro_fa_coauthor_gender %>%
  mutate(index_new = dplyr::recode(index,
                            "cohort" = "Cohort",
                            "discipline_new" = "Discipline",
                            "Jr_Quantile" = "Journal Rank",
                            "academic_age"="Academic Age",
                            "pub_before_cate"="Previous Publications"
  ))
df2_coauthor<-merge(margin_pro_fa_coauthor_gender, margin_pro_fa_coauthor, by = c('index','group'))

#df2_self[df2_self$gender=="female",c("AME.gender","AME.low.gender","AME.high.gender")]<-
#  -df2_self[df2_self$gender=="female",c("AME.gender","AME.low.gender","AME.high.gender")]
df2_coauthor <- df2_coauthor %>%
  mutate(index_new = dplyr::recode(index,
                            "cohort" = "Cohort",
                            "discipline_new" = "Discipline",
                            "Jr_Quantile" = "Journal Rank",
                            "academic_age"="Academic Age",
                            "pub_before_cate"="Previous Publications"
  ))
res<-t(mapply(R_text, df2_coauthor$AME.gender,df2_coauthor$gender,"p.p."))
df2_coauthor<-cbind(df2_coauthor,res)
df2_coauthor <- df2_coauthor %>% 
  rename("text.self" = "1",
         "text.hjust" = "2")
margin_sig_coauthor<-mapply(sig_star, df2_coauthor$AME.gender,df2_coauthor$sig.gender,df2_coauthor$gender)
df2_coauthor<-cbind(df2_coauthor,margin_sig_coauthor)
df2_coauthor[,c("AME" ,"AME.low","AME.high")]<-
  df2_coauthor[,c("AME" ,"AME.low","AME.high")]*100



ggplot(data=df2_coauthor[df2_coauthor$index_new!="Discipline",], aes(x=fct_inorder(group),color=gender)) +
  facet_wrap(~factor(index_new,c("Cohort","Previous Publications","Journal Rank")),scales="free")+
  geom_point( aes(y=AME),size=2, position=position_dodge(0.78), show.legend = TRUE) + 
  geom_text(aes(y = max(AME)+0.5, label =  paste0(text.self,margin_sig_coauthor),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=AME.low, ymax=AME.high), width=0.3, position=position_dodge(0.78), show.legend = FALSE) +
  ylab("Probabilities of promotion by co-authors (p.p)") +
  xlab("") +
  #scale_y_continuous(label=scales::percent_format(accuracy =  1),limits = c(0, 0.06),breaks=c(seq(0, 0.06,0.02)))+
  scale_y_continuous(limits = c(0, 3.3),breaks=c(seq(0,3.3,1)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
        text = element_text(size=15))


### specific for discipline : Aug 2024##########################################

df2_coauthor_dis <- df2_coauthor[(df2_coauthor$index_new=="Discipline")&(df2_coauthor$group!="Unk"),]%>%left_join(field_discipline,by="group")
df2_coauthor_dis <- df2_coauthor_dis %>%
  mutate(group_ordered = ifelse(startsWith(group, "Other"), paste0("X", group), group))%>%
  arrange(field, group_ordered) %>%  # First order the dataframe by A and B
  mutate(group_ordered = factor(group_ordered, levels = unique(group_ordered)))
  


ggplot(data=df2_coauthor_dis, aes(x=fct_inorder(group),color=gender)) +
  facet_wrap(~factor(index_new),scales="free")+
  geom_point( aes(y=AME),size=2, position=position_dodge(0.78), show.legend = TRUE) + 
  geom_text(aes(y = max(AME)+0.7, label =  paste0(text.self,margin_sig_coauthor),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  geom_errorbar(aes(ymin=AME.low, ymax=AME.high),width=0.4,size=0.8, position=position_dodge(0.78), show.legend = FALSE) +
  ylab("Probabilities of promotion by co-authors (p.p)") +
  xlab("") +
  #scale_y_continuous(limits = c(0, 2),breaks=c(seq(0,2,0.4)))+
  #scale_y_continuous(limits = c(0, 1),breaks=c(seq(0,1,0.2)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=13),axis.text.y = element_text(size=14),
        text = element_text(size=15),,strip.text = element_text(size = 14))


#### AUg 2024: for offical accounts' promotiom*********************************

margin_pro_fa_soc_gender<-margin_pro_fa_soc_gender[(margin_pro_fa_soc_gender$group!="Others")&(margin_pro_fa_soc_gender$group!="ukn"),]
margin_pro_fa_soc_gender <- margin_pro_fa_soc_gender %>%
  mutate(index_new = dplyr::recode(index,
                            "cohort" = "Cohort",
                            "discipline_new" = "Discipline",
                            "Jr_Quantile" = "Journal Rank",
                            "academic_age"="Academic Age",
                            "pub_before_cate"="Previous Publications"
  ))
df2_soc<-merge(margin_pro_fa_soc_gender, margin_pro_fa_soc, by = c('index','group'))

#df2_self[df2_self$gender=="female",c("AME.gender","AME.low.gender","AME.high.gender")]<-
#  -df2_self[df2_self$gender=="female",c("AME.gender","AME.low.gender","AME.high.gender")]
df2_soc <- df2_soc %>%
  mutate(index_new = dplyr::recode(index,
                            "cohort" = "Cohort",
                            "discipline_new" = "Discipline",
                            "Jr_Quantile" = "Journal Rank",
                            "academic_age"="Academic Age",
                            "pub_before_cate"="Previous Publications"
  ))
res<-t(mapply(R_text, df2_soc$AME.gender,df2_soc$gender,"p.p."))
df2_soc<-cbind(df2_soc,res)
df2_soc <- df2_soc %>% 
  rename("text.self" = "1",
         "text.hjust" = "2")
margin_sig_soc<-mapply(sig_star, df2_soc$AME.gender,df2_soc$sig.gender,df2_soc$gender)
df2_soc<-cbind(df2_soc,margin_sig_soc)
df2_soc[,c("AME" ,"AME.low","AME.high")]<-
  df2_soc[,c("AME" ,"AME.low","AME.high")]*100



ggplot(data=df2_soc[df2_soc$index_new!="Discipline",], aes(x=fct_inorder(group),color=gender)) +
  facet_wrap(~factor(index_new,c("Cohort","Previous Publications","Journal Rank")),scales="free")+
  geom_point( aes(y=AME),size=2, position=position_dodge(0.78), show.legend = TRUE) + 
  geom_text(aes(y = max(AME)+2, label =  paste0(text.self,margin_sig_soc),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  #geom_point( aes(y=`Female-to-Male.ratio`),size=2, position=position_dodge(0.78), show.legend = FALSE)+
  #geom_line(aes(group = 1,y=diff),color="black",linetype="dashed")+
  geom_errorbar(aes(ymin=AME.low, ymax=AME.high), width=0.3, position=position_dodge(0.78), show.legend = FALSE) +
  ylab("Probabilities of promotion by offical accounts (p.p)") +
  xlab("") +
  #scale_y_continuous(label=scales::percent_format(accuracy =  1),limits = c(0, 0.06),breaks=c(seq(0, 0.06,0.02)))+
  scale_y_continuous(limits = c(0, 16),breaks=c(seq(0,16,4)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
        text = element_text(size=15))


### specific for discipline : Aug 2024##########################################

df2_soc_dis <- df2_soc[(df2_soc$index_new=="Discipline")&(df2_soc$group!="Unk"),]%>%left_join(field_discipline,by="group")
df2_soc_dis <- df2_soc_dis %>%
  mutate(group_ordered = ifelse(startsWith(group, "Other"), paste0("X", group), group))%>%
  arrange(field, group_ordered) %>%  # First order the dataframe by A and B
  mutate(group_ordered = factor(group_ordered, levels = unique(group_ordered)))



ggplot(data=df2_soc_dis, aes(x=fct_inorder(group),color=gender)) +
  facet_wrap(~factor(index_new),scales="free")+
  geom_point( aes(y=AME),size=2, position=position_dodge(0.78), show.legend = TRUE) + 
  geom_text(aes(y = max(AME)+2, label =  paste0(text.self,margin_sig_soc),fontface=2),
            size = 14/.pt, show.legend = FALSE)+
  geom_errorbar(aes(ymin=AME.low, ymax=AME.high),width=0.4,size=1, position=position_dodge(0.8), show.legend = FALSE) +
  ylab("Probabilities of promotion by offical accounts (p.p)") +
  xlab("") +
  #scale_y_continuous(limits = c(0, 2),breaks=c(seq(0,2,0.4)))+
  scale_y_continuous(limits = c(0, 24),breaks=c(seq(0,24,6)))+
  scale_color_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  scale_fill_manual("Gender",values=c("#E69F00", "#56B4E9"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=13),axis.text.y = element_text(size=14),
        text = element_text(size=15),,strip.text = element_text(size = 14))





















