setwd("Documents/Freddie Mac/data")

install.packages("dplyr")
install.packages("ggplot2")
install.packages("taRifx")
library(dplyr)
library(ggplot2)
library(taRifx)

######### Part I: Examine changes in portfolio mix ###########
## This part uses origination data only ##

# read in origination files of 2008 and 2009
column_names=c('fico', 'dt_first_pi', 'flag_fthb', 'dt_matr', 'cd_msa', 'mi_pct', 'cnt_units', 'occpy_sts', 
               'cltv', 'dti', 'orig_upb', 'ltv', 'int_rt', 'channel', 'ppmt_pnlty', 'prod_type', 'st', 
               'prop_type', 'zipcode', 'id_loan', 'loan_purpose', 'orig_loan_term', 'cnt_borr', 'seller_name', 
               'servicer_name', 'flag_sc')
folder="/Users/zhixiaolin/Documents/Freddie Mac/data/"

read_modfile <- function(inputfile, qrtname) {
      tempfile <- read.csv(inputfile, header=FALSE, sep="|", stringsAsFactors=F, col.names=column_names)
      tempfile <- subset(tempfile, fico >= 550 & fico <= 850 & prop_type=='SF' & orig_loan_term==360)
      tempfile <- data.frame(tempfile[sample(1:nrow(tempfile), nrow(tempfile)*0.05, replace=FALSE),])
      tempfile$quarter=qrtname
      
      # define FICO band
      # FICO < 660 is considered subprime
      # FICO >= 780 is considered excellent  
      
      tempfile$fico_band <- ifelse(tempfile$fico < 600, '500-599', 
                                   ifelse(tempfile$fico < 660, '600-659',
                                          ifelse(tempfile$fico < 700, '660-699',
                                                 ifelse(tempfile$fico < 740, '700-739',
                                                        ifelse(tempfile$fico < 780, '740-779',
                                                               '780+')))))
      tempfile$subprime <- ifelse(tempfile$fico < 660, 1, 0)
      return(tempfile)
}

modfile_2009Q1 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q12009.txt", "2009Q1")
modfile_2009Q2 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q22009.txt", "2009Q2")
modfile_2009Q3 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q32009.txt", "2009Q3")
modfile_2009Q4 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q42009.txt", "2009Q4")

model_file=rbind(modfile_2009Q1, modfile_2009Q2, modfile_2009Q3, modfile_2009Q4)

hist(model_file$fico, main="FICO distribution", xlab='FICO')
plot(orig_upb ~ fico, data=model_file)
boxplot(model_file$fico)


# explore variables
ggplot(data=model_file)+geom_histogram(aes(x=fico), bins=15, fill='blue')
ggplot(data=model_file)+geom_density(aes(x=fico), fill='blue')
ggplot(model_file, aes(x=fico, y=orig_upb))+geom_point(aes(color=loan_purpose))
ggplot(model_file, aes(x=fico, y=orig_upb))+geom_point(aes(color=loan_purpose))+facet_wrap(~loan_purpose)

ggplot(data=model_file)+geom_histogram(aes(x=fico), bins=15, fill='blue')+facet_wrap(~loan_purpose)

ggplot(model_file, aes(y=orig_upb, x=loan_purpose))+geom_boxplot()
ggplot(model_file[which(model_file$orig_upb < 450000), c("fico", "loan_purpose", "orig_upb")], aes(y=orig_upb, x=loan_purpose))+geom_violin()


# read in performance files of 2008 and 2009
# select only 5-year performance for each loan
perf_names=c('id_loan','Period','Act_endg_upb','delq_sts','loan_age','mths_remng','repch_flag',
             'flag_mod','CD_Zero_BAL','Dt_zero_BAL','New_Int_rt','Amt_Non_Int_Brng_Upb','Dt_Lst_Pi',
             'MI_Recoveries','Net_Sale_Proceed','Non_MI_Recoveries','Expenses','legal_costs',
             'maint_pres_costs','taxes_ins_costs','misc_costs','actual_loss','modcost','stepmod_ind')
read_perffile <- function(inputfile, perf_end) {
  tempfile <- read.csv(inputfile, header=FALSE, sep="|", stringsAsFactors=F, col.names=perf_names)
  perffile <- merge(model_file, subset(tempfile, Period <= perf_end), by='id_loan')
  # Define Bad:
  # reposssed loans - REO
  # delinquency 90+ days
  # balance becomes zero, with delinquency flags 90+ days
  # loan term modified
  # Recovery is usually associated with foreclosure and repossession.  Therefore, if a loan shows recoevery, it must
  # have gone bad before. 
  # If the loan has experienced a decrease in interest rate, the loan has been modified.
  # A modified loan suggests the borrower has encountered financial hardship.
  
  
  perffile$tempbad <- ifelse(perffile$delq_sts=='R' | perffile$CD_Zero_BAL %in% c('3.0', '6.0', '9.0') |
                               perffile$flag_mod=='Y' | destring(perffile$delq_sts) > 2 |
                               perffile$Non_MI_Recoveries > 0 | perffile$MI_Recoveries > 0 |
                               perffile$Amt_Non_Int_Brng_Upb > 0 |
                               perffile$New_Int_rt < perffile$int_rt, 1, 0)
  
  perffile$tempbad[is.na(perffile$tempbad)] <- 0
  
  # Aggregate to one record per loan
  tempfile2 <- perffile %>%
    select(id_loan, tempbad) %>%
    group_by(id_loan) %>%
    summarise(bad=sum(tempbad))
  
  tempfile2$bad <- ifelse(tempfile2$bad > 0, 1, 0)
  
  # define prepayment
  perffile$prepaid <- ifelse(perffile$CD_Zero_BAL==1.0, 1, 0)
  
  perffile$prepaid[is.na(perffile$prepaid)] <- 0
  
  # Aggregate to one record per loan
  tempfile3 <- perffile %>%
    select(id_loan, prepaid) %>%
    group_by(id_loan) %>%
    summarise(pp=sum(prepaid))
  
  tempfile3$pp <- ifelse(tempfile3$pp > 0, 1, 0)
  
  tempfile4 <- merge(tempfile2, tempfile3, by="id_loan")
  tempfile4$pp <- ifelse(tempfile4$pp==1 & tempfile4$bad !=1, 1, 0)

  return(tempfile4)
}

orig_perf_2009Q1 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q12009.txt", 201403)
orig_perf_2009Q2 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q22009.txt", 201406)
orig_perf_2009Q3 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q32009.txt", 201409)
orig_perf_2009Q4 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q42009.txt", 201412)

orig_perf_combined=rbind(orig_perf_2009Q1, orig_perf_2009Q2, orig_perf_2009Q3, orig_perf_2009Q4)
head(orig_perf_combined)

mean(orig_perf_combined$pp, na.rm=TRUE)


# remove some objects to free some memory
rm(list=c("modfile_2009Q1", "modfile_2009Q2", "modfile_2009Q3", "modfile_2009Q4"))
rm(list=c("orig_perf_2009Q1", "orig_perf_2009Q2", "orig_perf_2009Q3", "orig_perf_2009Q4"))

# merge the performance file with origination file
loans_prepaid_cleaned <- merge(model_file, orig_perf_combined,by="id_loan")

# filter out invalid data
loans_prepaid_cleaned <- loans_prepaid_cleaned %>%
  filter(cltv < 999 & ltv < 999 & cnt_borr < 99 & mi_pct != 999)

# Make derived variables
# You can either tranform a raw variable or combine multiple variables to make a derived variable
loans_prepaid_cleaned$mi_flag <- ifelse(loans_prepaid_cleaned$mi_pct > 0, 1, 0)

# assign a random_num, which is to be used to split the sample for modeling later
loans_prepaid_cleaned <- loans_prepaid_cleaned %>%
  mutate(other_debt_to_value=cltv-ltv,
         other_debt_to_this_loan_ratio=other_debt_to_value/ltv,
         orig_upb_log=log(orig_upb),
         random_num=runif(nrow(loans_prepaid_cleaned)))


# drop variables unneeded by the model
loans_prepaid_cleaned <- select(loans_prepaid_cleaned, -c(seller_name, servicer_name, st, zipcode, prop_type, orig_loan_term, 
                                           cd_msa, dt_first_pi, quarter, prod_type, int_rt, dt_matr))

# split the sample into a modeling sample (60%) and a validation sample (40%)
samp_mod <- subset(loans_prepaid_cleaned, random_num > 0.4)
samp_val <- subset(loans_prepaid_cleaned, random_num <= 0.4)

# Explore behavior of variables
# character variables or numeric variables that do not need to be collapsed
attach(samp_mod)
explore_char <- function(xvar, xlabel){
  check_bad <- aggregate(bad ~ xvar, samp_mod, mean)
  
  ggplot(check_bad, aes(x=xvar, y=bad)) + 
    geom_bar(stat = "identity", color='blue', fill='blue') +scale_color_manual(values =c('blue')) +
    labs(title="Bad Rate") + ylab("Bad Rate") +xlab(xlabel)
  }

explore_char(flag_fthb, "flag_fthb")
explore_char(cnt_units, "Number of Units in the Property")
explore_char(occpy_sts, "Status of Occupancy")
explore_char(channel, "Channel")
explore_char(loan_purpose, "Loan Purpose")
explore_char(ppmt_pnlty, "Prepayment Penalty")
explore_char(flag_sc, "Super Conforming")
explore_char(mi_flag, "Mortgage Insurance")

# create dummy variables for some categorical variables
# Dummy variable is also a type of derived variable
# apply the treatment to both modeling and validation sample
samp_mod$investment_property <- ifelse(samp_mod$occpy_sts=='I', 1, 0)
samp_val$investment_property <- ifelse(samp_val$occpy_sts=='I', 1, 0)

samp_mod$prepay_penalty <- ifelse(samp_mod$ppmt_pnlty=="Y", 1, 0)
samp_val$prepay_penalty <- ifelse(samp_val$ppmt_pnlty=="Y", 1, 0)

samp_mod$super_conforming <- ifelse(samp_mod$flag_sc=='Y', 1, 0)
samp_val$super_conforming <- ifelse(samp_val$flag_sc=='Y', 1, 0)

# check means for three derived dummy variables
sapply(Filter(is.numeric, subset(samp_mod, select=c(investment_property, prepay_penalty, super_conforming))), mean)

## explore behavior of numeric variable
# cut each variable into bins of an equal distribution and see whether a monotonic trend emerges
install.packages("mltools")
library(mltools)
attach(samp_mod)
explore_num <- function(xvar, xlabel){
  # xbin <- cut(xvar, 10)  # This will cut the variable to equal space between values
  samp_mod[, "xbin"] <- bin_data(xvar, bins=10, binType = "quantile")  # This will cut the variable to an equal distribution
  check_pp <- aggregate(pp ~ xbin, samp_mod, mean)
  
  ggplot(check_pp, aes(x=xbin, y=pp)) + 
    geom_bar(stat = "identity", color='blue', fill='blue') +scale_color_manual(values =c('blue')) +
    labs(title="Prepayment Rate") + ylab("Prepayment Rate") +xlab(xlabel)
  
}
explore_num(fico, "FICO")
explore_num(dti, "Debt-to-Income Ratio")
explore_num(ltv, "Loan-to-Value Ratio")
explore_num(cltv, "Combined Loan-to-Value Ratio")
explore_num(orig_upb, "Original Loan Amount")
explore_num(other_debt_to_value, "'other_debt_to_value")
explore_num(other_debt_to_this_loan_ratio, "other_debt_to_this_loan_ratio")
explore_num(orig_upb_log, "log(Original Loan Amount")

# find the outliers for continuous values
# One can usually use 98th or 99th percentile as the top ceiling to suppress outliers
summary(subset(samp_mod, select=c(fico, dti, ltv, cltv, orig_upb, orig_upb_log)))
quantile(samp_mod$fico, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99, 1)) # quartile
quantile(samp_mod$dti, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99, 1))
quantile(samp_mod$ltv, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99, 1))
quantile(samp_mod$cltv, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99, 1))
quantile(samp_mod$orig_upb, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99, 1))
quantile(samp_mod$orig_upb_log, probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99, 1))


# cap the outlier for the modeling sample
attach(samp_mod)
fico <- ifelse(fico > 800, 800, fico)
dti <- ifelse(dti > 55 , 55, dti)
cltv <- ifelse(cltv > 95, 95, cltv)
ltv <- ifelse(ltv > 95, 95, ltv)
orig_upb <- ifelse(orig_upb > 520000, 520000, orig_upb)
orig_upb_log <- ifelse(orig_upb_log > 13.16, 13.16, orig_upb_log)
detach(samp_mod)



# cap the outlier for the validation sample
attach(samp_val)
fico <- ifelse(fico > 800, 800, fico)
dti <- ifelse(dti > 55 , 55, dti)
cltv <- ifelse(cltv > 95, 95, cltv)
ltv <- ifelse(ltv > 95, 95, ltv)
orig_upb <- ifelse(orig_upb > 520000, 520000, orig_upb)
orig_upb_log <- ifelse(orig_upb_log > 13.16, 13.16, orig_upb_log)
detach(samp_val)

# change the scale of orig_upb (original unpaid balance)
samp_mod$orig_upb_rescaled <- samp_mod$orig_upb/1000000
samp_val$orig_upb_rescaled <- samp_val$orig_upb/1000000

# Check the correlation between numeric variables
# Method 1
install.packages("psych")
library(psych)
pairs.panels(subset(samp_mod, select=c(fico, orig_upb, orig_upb_log, dti, ltv, cltv)))

# Method 2
pairs(fico ~ ., data=subset(samp_mod, select=c(fico, orig_upb, orig_upb_log, dti, ltv, cltv)))

# Method 3 - this one is most visually straightforward 
install.packages("corrplot")
library(corrplot)
x <- cor(subset(samp_mod, select=c(fico, orig_upb, orig_upb_log, dti, ltv, cltv)))
corrplot(x, type="upper", order="hclust")


# run logistic regression
mylogit <- glm(pp ~ fico+dti+ltv+orig_upb_rescaled+orig_upb_log+prepay_penalty+super_conforming+mi_flag, 
               data=samp_mod, family = "binomial")
mylogit
summary(mylogit)

# rerun logistic regression by retaining useful variables only
mylogit <- glm(pp ~ fico+ltv+dti+orig_upb+orig_upb_log+mi_flag, 
               data=samp_mod, family = "binomial")
mylogit
summary(mylogit)



# check model performance
samp_mod$predicted_score <- predict(mylogit, newdata = samp_mod, type = "response")
samp_val$predicted_score <- predict(mylogit, newdata = samp_val, type = "response")

check_score <- function(dataset) {
  attach(dataset)
  # xbin <- cut(xvar, 10)  # This will cut the variable to equal space between values
  dataset[, "xbin"] <- bin_data(predicted_score, bins=10, binType = "quantile")  # This will cut the variable to an equal distribution
  
  check_pp <- dataset %>%
    select(pp, xbin, predicted_score) %>%
    group_by(xbin) %>%
    summarize(predicted_pp=mean(predicted_score), actual_pp=mean(pp), score_label=median(predicted_score))
  as.data.frame
  check_pp
  
  ggplot(aes(x=1:nrow(check_bad), y = value, color = variable), data = check_pp) +  
    geom_line(aes(x=1:nrow(check_pp), y=predicted_pp, color="blue")) +
    geom_line(aes(x=1:nrow(check_pp), y=actual_pp, color="red")) +
    xlab("Score Decile") + ylab("Actual vs. Predicted") + labs("Actual Prepayment vs. Predicted Prepayment")
}
check_score(samp_mod)
check_score(samp_val)

