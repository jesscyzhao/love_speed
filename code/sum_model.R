CUR_DIC = getwd()
setwd("..")
PARENT_DIC = getwd()
setwd(CUR_DIC)
DATA_DIC = paste(PARENT_DIC, "/data/", sep="")
DATA_SUB_DIC = paste(DATA_DIC, "toy_data_sum_more_wave_11_28/", sep="")
OUTPUT_DIC = paste(PARENT_DIC, "/output/sum_model_result/", sep="")

FEATURE_DATA = paste(DATA_DIC, "feature_select_ext.csv", sep="")
MODEL_DATA = paste(DATA_SUB_DIC, "TEST_SUM_MODEL.csv", sep="")
SAMPLE_DATA = paste(DATA_SUB_DIC, "TEST_SUM_SAMPLE.csv", sep="")
MODEL_FUNCTION = paste(PARENT_DIC, "/code/model_function.r", sep="")
MODEL_SCRIPT = paste(PARENT_DIC, "/code/model_script.r", sep="")


model_data = read.csv(MODEL_DATA)
sample_data = read.csv(SAMPLE_DATA)
feature_select_data = read.csv(FEATURE_DATA)
feature_select_data$raw_col
colnames(model_data)

source(MODEL_FUNCTION)

source(MODEL_SCRIPT)

library(stargazer)
setwd(PARENT_DIC)
setwd("..")
ROOT_DIC = getwd()
setwd(CUR_DIC)
final_report_dic = paste(ROOT_DIC, "/Final report/final_report/", sep="")

stargazer(result$M1, step_result$M1, type="text", 
          single.row=TRUE)

stargazer(result$M1, step_result$M1, type="latex", 
          single.row=TRUE, 
          covariate.labels=c("Rating of Attractiveness", "Rating of Ambition", "Rating of Shared Interest", "Overall score", "Prob of yes", "Go out freq.", "Importance of race", "Same goal yes", "Constant"), 
          out = paste(final_report_dic, "sum_m_complete.tex"), sep="")

stargazer(result_wave$M1, step_result_wave$M1, type="text", 
          no.space=TRUE, 
          single.row = TRUE)
stargazer(result_wave$M1, step_result_wave$M1, type="latex", 
          single.row=TRUE, 
          no.space = TRUE, 
          covariate.labels=c("Rating of Attractiveness", "Rating of Ambition", "Rating of Shared Interest", "Overall score", "Prob of yes", "Go out freq.", "Importance of race", "Same goal yes", "Constant"), 
          out = paste(final_report_dic, "sum_m_complete_wave.tex"), sep="")


stargazer(result_ind_iid$M1, single.row = TRUE, 
          type="text")
stargazer(result_ind_iid$M1, single.row=TRUE, 
          type="latex", out=paste(final_report_dic, "teamwork_ind_iid_ss.tex", sep=""))

library(MKmisc)
step_result_test = HLgof.test(fit = fitted(step_result$M1), obs=model_data_reduced$match)

all_numerical_variables = setdiff(complete_features, factor_features)
all_num_var_data = model_data_reduced[, all_numerical_variables]
colnames(all_num_var_data) = c("Pref. Attractive", "Pref. Sincere", "Pref. Intelligent", "Pref. Fun", "Pref. Ambition", "Pref. Shared Interest", "Rating Attractive", "Rating Sincere", "Rating Intelligence", "Rating Fun", "Rating Ambition", "Rating Shared Interest", "Overall Score", "Prob. to say yes", "Age", "Importance Race", "Importance Religion", "Date", "Go out")
num_variable_corr = cor(all_num_var_data)

require(ggplot2)
require(reshape2)
melted_corr = melt(num_variable_corr) 
ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill=value)) + geom_tile() + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1))+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.x = element_blank())