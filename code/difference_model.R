CUR_DIC = getwd()
setwd("..")
PARENT_DIC = getwd()
setwd(CUR_DIC)
DATA_DIC = paste(PARENT_DIC, "/data/", sep="")
DATA_SUB_DIC = paste(DATA_DIC, "toy_data_ext_more_wave_11_28/", sep="")
OUTPUT_DIC = paste(PARENT_DIC, "/output/diff_model_result/", sep="")

FEATURE_DATA = paste(DATA_DIC, "feature_select_ext.csv", sep="")
MODEL_DATA = paste(DATA_SUB_DIC, "TEST_EXT_MODEL.csv", sep="")
SAMPLE_DATA = paste(DATA_SUB_DIC, "TEST_EXT_SAMPLE.csv", sep="")
MODEL_FUNCTION = paste(PARENT_DIC, "/code/model_function.r", sep="")
MODEL_SCRIPT = paste(PARENT_DIC, "/code/model_script.r", sep="")

model_data = read.csv(MODEL_DATA)
sample_data = read.csv(SAMPLE_DATA)
feature_select_data = read.csv(FEATURE_DATA)

source(MODEL_FUNCTION)
source(MODEL_SCRIPT)

# Summary does not change the estimate

require(broom)
require(knitr)
output=tidy(result$M1)
summary(result$M1)
summary(result_norm$M1)



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

print("hello world")

