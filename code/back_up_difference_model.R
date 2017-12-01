CUR_DIC = getwd()
setwd("..")
PARENT_DIC = getwd()
setwd(CUR_DIC)
DATA_DIC = paste(PARENT_DIC, "/data/", sep="")
DATA_SUB_DIC = paste(DATA_DIC, "toy_data_ext_more_wave_11_28/", sep="")
FEATURE_DATA = paste(DATA_DIC, "feature_select_ext.csv", sep="")
MODEL_DATA = paste(DATA_SUB_DIC, "TEST_EXT_MODEL.csv", sep="")
SAMPLE_DATA = paste(DATA_SUB_DIC, "TEST_EXT_SAMPLE.csv", sep="")
MODEL_FUNCTION = paste(PARENT_DIC, "/code/model_function.r", sep="")


model_data = read.csv(MODEL_DATA)
sample_data = read.csv(SAMPLE_DATA)
feature_select_data = read.csv(FEATURE_DATA)
source(MODEL_FUNCTION)

########################################Data prep###############################################################
model_data = model_data[which(model_data$part_met.pid!=3), ]

factor_features = c("iid", "pid", "samerace.pid", "wave.iid", "part_met.pid")


model_data_reduced  = model_data[complete.cases(model_data), ]
model_data_reduced$samegoal = as.factor(sapply(model_data_reduced$goal.iid - model_data_reduced$goal.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced$samecareer = as.factor(sapply(model_data_reduced$career_c.iid- model_data_reduced$career_c.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced$samefield = as.factor(sapply(model_data_reduced$field_cd.iid- model_data_reduced$field_cd.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced = model_data_reduced[complete.cases(model_data_reduced),]
for (feature in factor_features){
    model_data[, feature] = as.factor(model_data[, feature])
}
###############################################################################################################

all_vars = colnames(model_data)
all_features = all_vars[which(!(all_vars %in% c("match.iid", "match.pid", "samerace.iid", "gender.iid", "gender.pid", "wave.pid") ))]


individual_feature = as.character(feature_select_data[which(feature_select_data$ind==1),"new_col"])
pair_wise_feature = as.character(feature_select_data[which(feature_select_data$pair==1), "new_col"])


## Looking from sample data
sample_data$wave = as.factor(sample_data$wave)
sample_data$iid = as.factor(sample_data$iid)
ind_dates = aggregate(sample_data$dec_part, by=list(sample_data$iid), sum)
ind_dates_with_wave = cbind(sample_data[!duplicated(sample_data$iid), c("iid", "wave")], dates = ind_dates[, "x"])

L = lm(dates~wave, data=data.frame(ind_dates_with_wave))
M = aov(dates~wave, data = data.frame(ind_dates_with_wave))
TukeyHSD(M)

wave_size = unlist(lapply(split(ind_dates_with_wave, ind_dates_with_wave$wave), function(x){length(unique(x$iid))}))
wave_dates = unlist(lapply(split(ind_dates_with_wave, ind_dates_with_wave$wave), function(x){sum(x$dates)}))
wave_proportion = wave_dates/wave_size

## Looking from model data 
wave_pair_counts = unlist(lapply(split(model_data_reduced, model_data_reduced$wave.iid), function(x){dim(x)[1]}))
success = aggregate(model_data_reduced$match.iid, by=list(model_data_reduced$wave.pid), sum)
success_by_wave = cbind(wave = success[,1], success = success[, 2], rate = success[, "x"]/wave_pair_counts)

success_failure_table = t(matrix(unlist(lapply(split(model_data_reduced, model_data_reduced$wave.iid), function(x){table(x$match.iid)})), nrow=2))
success_failure_table = cbind(c(1, 2, 3, 4, 10, 11, 15, 16, 17), success_failure_table)
colnames(success_failure_table) = c("wave", "failure", "success")

final_table_list = list()
for (i in 1:dim(success_failure_table)[1]){
    x = success_failure_table[i, ]
    final_table_list[[i]] = cbind(rep(x[1], x[2] + x[3]), c(rep(1, x[3]), rep(0, x[2])))
}
final_table = as.data.frame(do.call(rbind.data.frame, final_table_list))

colnames(final_table) = c("wave", "success")
final_table$wave = as.factor(final_table$wave)
L = lm(success~wave, data=final_table)
summary(L)
M = aov(success ~ wave, data = final_table)
TukeyHSD(M)
plot(TukeyHSD(M))





