setwd("Documents/Freddie Mac/data")

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
      tempfile <- data.frame(tempfile[sample(1:nrow(tempfile), nrow(tempfile)*0.1, replace=FALSE),])
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

modfile_2008Q1 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q12008.txt", "2008Q1")
modfile_2008Q2 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q22008.txt", "2008Q2")
modfile_2008Q3 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q32008.txt", "2008Q3")
modfile_2008Q4 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q42008.txt", "2008Q4")
modfile_2009Q1 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q12009.txt", "2009Q1")
modfile_2009Q2 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q22009.txt", "2009Q2")
modfile_2009Q3 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q32009.txt", "2009Q3")
modfile_2009Q4 <- read_modfile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_Q42009.txt", "2009Q4")

model_file=rbind(modfile_2008Q1, modfile_2008Q2, modfile_2008Q3, modfile_2008Q4,
                 modfile_2009Q1, modfile_2009Q2, modfile_2009Q3, modfile_2009Q4)

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("taRifx")
library(taRifx)

file2 <- model_file %>%
  select(fico_band, quarter) %>%
  group_by(fico_band, quarter) %>%
  summarise(loan_cnt=n()) 

loan_cnt_by_quarter <- model_file %>%
  select(quarter) %>%
  group_by(quarter) %>%
  summarise(loan_sum=n()) 

file3 <- merge(file2, loan_cnt_by_quarter,by="quarter")
file3$loan_pct_by_quarter <- file3$loan_cnt/file3$loan_sum

ggplot(file3, aes(x=quarter, y=loan_pct_by_quarter)) +
  geom_bar(aes(color=fico_band, fill=fico_band), stat = "identity") +
  scale_color_manual(values = c("green", "red", 'yellow', 'purple','orange', 'blue')) +
  scale_fill_manual(values = c("green", "red", 'yellow', 'purple','orange', 'blue')) +
  labs(title="Change in FICO Mix", color="fico_band") + xlab("Quarter") + ylab("Loan Distribution by FICO")

# Examine changes in loan purpose
file2 <- model_file %>%
  select(loan_purpose, quarter) %>%
  group_by(loan_purpose, quarter) %>%
  summarise(loan_cnt=n()) 

file3 <- merge(file2, loan_cnt_by_quarter,by="quarter")
file3$loan_pct_by_quarter <- file3$loan_cnt/file3$loan_sum

ggplot(file3, aes(x=quarter, y=loan_pct_by_quarter)) +
  geom_bar(aes(color=loan_purpose, fill=loan_purpose), stat = "identity") +
  scale_color_manual(values = c("green", "red", 'blue')) +
  scale_fill_manual(values = c("green", "red", 'blue')) +
  labs(title="Change in Loan Purpose", color="loan_purpose") + xlab("Quarter") + ylab("Loan Distribution by Purpose")

# Examine change in % of subprime mortgages in portfolio
file2 <- model_file %>%
  select(subprime, quarter) %>%
  group_by(quarter) %>%
  summarise(subprime_pct=mean(subprime))

ggplot(file2, aes(x=quarter, y=subprime_pct)) + 
  geom_bar(stat = "identity", color='blue', fill='blue') +scale_color_manual(values =c('blue')) +
  labs(title="% Subprime in Loan Portfolio", color="subprime") + xlab("Quarter") + ylab("% of Mortgages")


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
  return(tempfile2)
}

orig_perf_2008Q1 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q12008.txt", 201303)
orig_perf_2008Q2 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q22008.txt", 201306)
orig_perf_2008Q3 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q32008.txt", 201309)
orig_perf_2008Q4 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q42008.txt", 201312)
orig_perf_2009Q1 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q12009.txt", 201403)
orig_perf_2009Q2 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q22009.txt", 201406)
orig_perf_2009Q3 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q32009.txt", 201409)
orig_perf_2009Q4 <- read_perffile("/Users/zhixiaolin/Documents/Freddie Mac/data/historical_data1_time_Q42009.txt", 201412)

orig_perf_combined=rbind(orig_perf_2008Q1, orig_perf_2008Q2, orig_perf_2008Q3, orig_perf_2008Q4, 
                         orig_perf_2009Q1, orig_perf_2009Q2, orig_perf_2009Q3, orig_perf_2009Q4)

bad_loan_all <- merge(orig_perf_combined, model_file, by='id_loan')


# compute average bad rate across quarters of 2008 and 2009
check_bad <- bad_loan_all %>%
  select(quarter, bad) %>%
  group_by(quarter) %>%
  summarise(avg_bad=mean(bad))

check_bad

ggplot(check_bad, aes(x=quarter, y=avg_bad)) + 
  geom_bar(stat = "identity", color='blue', fill='blue') +scale_color_manual(values =c('blue')) +
  labs(title="Change in Delinquency Rate", color="quarter") + xlab("Quarter") + ylab("Bad Rate")