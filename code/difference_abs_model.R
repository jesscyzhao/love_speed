CUR_DIC = getwd()
setwd("..")
PARENT_DIC = getwd()
setwd(CUR_DIC)
DATA_DIC = paste(PARENT_DIC, "/data/", sep="")
DATA_SUB_DIC = paste(DATA_DIC, "data_abs_diff_12_2/", sep="")
OUTPUT_DIC = paste(PARENT_DIC, "/output/diff_abs_model_result/", sep="")

FEATURE_DATA = paste(DATA_DIC, "feature_select_ext.csv", sep="")
MODEL_DATA = paste(DATA_SUB_DIC, "TEST_ABS_DIFF_MODEL.csv", sep="")
SAMPLE_DATA = paste(DATA_SUB_DIC, "TEST_ABS_DIFF_SAMPLE.csv", sep="")
MODEL_FUNCTION = paste(PARENT_DIC, "/code/model_function.r", sep="")
MODEL_SCRIPT = paste(PARENT_DIC, "/code/model_script.r", sep="")

model_data = read.csv(MODEL_DATA)
sample_data = read.csv(SAMPLE_DATA)
feature_select_data = read.csv(FEATURE_DATA)

source(MODEL_FUNCTION)
source(MODEL_SCRIPT)



# Output lm to a table

M_wanted = fit.with.these.features(model_data_reduced, clean.up.sigificant.feature(extract.significant.feature(step_result$M1)))


## summary output
library(stargazer)
setwd(PARENT_DIC)
setwd("..")
ROOT_DIC = getwd()
setwd(CUR_DIC)
final_report_dic = paste(ROOT_DIC, "/Final report/final_report/", sep="")
stargazer(result$M1, step_result$M1, type="text",
          single.row = TRUE,
          out = paste(OUTPUT_DIC, "complete_feature.txt"))
# m complete
stargazer(result$M1, step_result$M1, M_wanted,  
          type="latex", 
          single.row = TRUE,
          covariate.labels=c("Preference of Attractiveness","Preference of Intelligence","Rate of Attractiveness","Probability of yes","Overall score", "Age", "Importance of religion", "Go out freq.", "Met no", "Same goal yes", "Date frequency", "Rating of fun", "Constant"), 
          out = paste(final_report_dic, "contrast_m_complete.tex", sep="")
)

stargazer(anova(M_wanted, step_result$M1, test="Chisq"), type="text")



# m wave effect
stargazer(result_wave$M1, step_result_wave$M1, type="text", 
          single.row=TRUE)
stargazer(result_wave$M1, step_result_wave$M1, type="latex", 
          single.row=TRUE, 
          covariate.labels=c("Rating of Attractiveness",
                             "Date Frequency", 
                             "Preference of Intelligence",
                             "Preference of Attractiveness",
                             "Probability of yes",
                             "Overall score", "Age", "Importance of religion", 
                             "Go out Frequency", "Wave2","Wave3", "Wave4", "Wave10", "Wave11",  "Wave15", "Wave16", "Wave17",  
                             "Met no", "Same goal yes", "Rating of Fun", "Constant"), 
          out = paste(final_report_dic, "contrast_m_complete_wave.tex", sep=""))

same_goal_pairs = model_data_reduced[model_data_reduced$samegoal == 1, c("iid", "pid")]
all_goals = model_data_no_na[, c("iid", "pid", "goal.iid", "goal.pid")]
same_goals = merge(same_goal_pairs, all_goals, by = c("iid", "pid"), how="left")
all(same_goals$goal.iid==same_goals$goal.pid)
table(same_goals$goal.iid)

stargazer(M_wave_effect, type="text", 
          single.row = TRUE, 
          out = paste(OUTPUT_DIC, "only_wave_efffect.txt"))
stargazer(M_wave_effect, type="latex", 
          single.row=TRUE, 
          out=paste(final_report_dic, "only_wave_effect.tex", sep=""))


## Individual effect 

stargazer(result_ind_iid$M1, single.row = TRUE, 
type="text")
stargazer(result_ind_iid$M1, single.row=TRUE, 
         type="latex", out=paste(final_report_dic, "contrast_ind_iid_ss.tex", sep=""))



## Check colineartiy 
all_numerical_variables = setdiff(complete_features, factor_features)
all_num_var_data = model_data_reduced[, all_numerical_variables]
colnames(all_num_var_data) = c("Pref. Attractive", "Pref. Sincere", "Pref. Intelligent", "Pref. Fun", "Pref. Ambition", "Pref. Shared Interest", "Rating Attractive", "Rating Sincere", "Rating Intelligence", "Rating Fun", "Rating Ambition", "Rating Shared Interest", "Overall Score", "Prob. to say yes", "Age", "Importance Race", "Importance Religion", "Date", "Go out")
num_variable_corr = cor(all_num_var_data)

require(ggplot2)
require(reshape2)
melted_corr = melt(num_variable_corr) 
ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill=value)) + geom_tile() + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1))+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(size=10, face="bold"))

## Goodness of fit 
# Model selection 
require(lmtest)
lrtest(result$M1, step_result$M1)
lrtest(result$M0, result$M1)
AIC(result$M0)
AIC(result$M1)
lrtest(step_result$M1, step_result_wave$M1)
lrtest(no_first_wave_result_step$M1, no_first_result_wave_step$M1)

## Goodness of fit Psudo r-squared 
library(pscl)

step_result_r_squared = pR2(glm(as.formula(step_result$M1$formula), data=model_data_reduced))[4]

step_result_wave_r_squared = pR2(glm(as.formula(step_result_wave$M1$formula), data=model_data_reduced))[4]

no_first_result_wave_step_r_squared = pR2(glm(as.formula(no_first_result_wave_step$M1$formula), data = model_data_no_first_wave))[4]


# Hosmer-lameshow test 
library(MKmisc)
HL_list = c(step_result$M1, step_result_wave$M1, no_first_wave_result_step$M1, no_first_result_wave_step$M1)

step_result_test = HLgof.test(fit = fitted(step_result$M1), obs=model_data_reduced$match)
step_result_wave_test = HLgof.test(fit = fitted(step_result_wave$M1), obs=model_data_reduced$match)

p_values = c(step_result_test$H$p.value, step_result_wave_test$H$p.value, no_first_result_step_test$H$p.value, no_first_result_wave_step_test$H$p.value)

HL_output = cbind(c("All variables", "All variables with wave", "No first wave all variables", "No first wave all variables with wave"), as.vector(p_values))

# Plot ROC 
library(plotROC)
roc_plot_df = data.frame(match=model_data_reduced$match, p = step_result$M1$fitted.values)
library(ggplot2)
basic_plot = ggplot(roc_plot_df, aes(d=match, m=p)) + geom_roc(n.cuts = 10, labelsize = 3, labelround = 3)
basic_plot
basic_plot+style_roc()
library(ROCR)
pred = prediction(roc_plot_df$p, roc_plot_df$match)
perf = performance(pred, measure="acc")
plot(perf)
