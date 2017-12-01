CUR_DIC = getwd()
setwd("..")
PARENT_DIC = getwd()
setwd(CUR_DIC)
DATA_DIC = paste(PARENT_DIC, "/data/", sep="")
DATA_SUB_DIC = paste(DATA_DIC, "toy_data_sum_more_wave_11_28/", sep="")
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


