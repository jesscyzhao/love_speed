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

important_result = list(
    all_wave_complete_feature_M0 = result$M0,
    all_waves_complete_feature_M1 =  result$M1, 
    all_waves_compelte_feature_step = step_result$M1, 
    all_waves_complete_feature_wave_effect_M0 =  result_wave$M0,
    all_waves_complete_feature_wave_effect_M1 =  result_wave$M1,
    all_waves_complete_feature_wave_effect_step =  step_result_wave$M1, 
    all_waves_wave_effect_only = M_wave_effect,
    no_first_wave_complete_feature =  no_first_wave_result$M1, 
    no_first_wave_complete_feature_step =  no_first_wave_result_step$M1, 
    no_first_wave_complete_feature_wave_effect = no_first_result_wave$M1,
    no_first_wave_complete_wave_effect_step_M0 = no_first_result_wave_step$M0,
    no_first_wave_complete_feature_wave_effect_step = no_first_result_wave_step$M1
)




regression.summary.output(important_result, OUTPUT_DIC, "major_regression_summary")


## summary output
stargazer(result$M1, step_result$M1, type="text",
          covariate.labels=c("Pref. of Attractiveness","Pref. of Intelligence",
"Rate of Attractiveness","Pref. of Sincerity","Prob. of yes","Overall score", "Age", "Importance of religion", "Rate of Fun", "Date freq.", "Go out freq.", "same goal1"),
          single.row = TRUE,
          out = paste(OUTPUT_DIC, "complete_feature.txt"))

stargazer(result_wave$M1, step_result_wave$M1, type="text", 
          single.row=TRUE, 
          out = paste(OUTPUT_DIC, "wave_fixed_effect.txt"))
stargazer(no_first_result_wave$M1, no_first_result_wave_step$M1, type="text", 
          single.row=TRUE, 
          out = paste(OUTPUT_DIC, "no_wave_1_wave_fixed_effect.txt"))

stargazer(M_wave_effect, type="text", 
          single.row = TRUE, 
          out = paste(OUTPUT_DIC, "only_wave_efffect.txt"))


## Check colineartiy 
cor(model_data_reduced[, setdiff(complete_features, factor_features)])
pairs(model_data_reduced[, setdiff(complete_features, factor_features)])


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
HLgof.test(fit = fitted(step_result$M1), obs=model_data_reduced$match)

# 

# ROC curve 
## No factor????
formula_vec = paste("match ~", paste(setdiff(complete_features, factor_features), sep = "+"))
#step_result_roc = roc(match ~ pref_part_att+pref_part_sin+pref_part_int+pref_part_fun+pref_part_amb+pref_part_sha+part_att+part_sin+part_int+part_fun+part_amb+part_sha+part_lik+part_prob+age_part+imprace+imprelig+date+go_out, data = model_data_reduced)

for (vec in formula_vec){
    this_roc = roc(as.formula(vec), data = model_data_reduced) 
    print(this_roc)
    plot(this_roc, main=vec)
}





