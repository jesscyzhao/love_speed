CUR_DIC = getwd()
setwd("..")
PARENT_DIC = getwd()
setwd(CUR_DIC)
DATA_DIC = paste(PARENT_DIC, "/data/", sep="")
DATA_SUB_DIC = paste(DATA_DIC, "toy_data_ext_11_28/", sep="")
#OUTPUT_DIC = paste(PARENT_DIC, "output/toy_data_ext_11_28", sep="")
FEATURE_DATA = paste(DATA_DIC, "feature_select_ext.csv", sep="")
MODEL_DATA = paste(DATA_SUB_DIC, "TEST_EXT_MODEL.csv", sep="")
SAMPLE_DATA = paste(DATA_SUB_DIC, "TEST_EXT_SAMPLE.csv", sep="")
model_data = read.csv(MODEL_DATA)
sample_data = read.csv(SAMPLE_DATA)
feature_select_data = read.csv(FEATURE_DATA)
feature_select_data$raw_col
colnames(model_data)



fit.with.these.features = function(model_data, feature_lists){
    this_formula = paste("match.iid ~", paste(feature_lists, collapse="+"))   
    M = glm(formula = this_formula, data = model_data)
    return(M)
}

detect.and.process.na = function(sample_data){
    total_na = which(is.na(sample_data), arr.ind = TRUE)
    total_na = as.data.frame(cbind(total_na, iid =sample_data[total_na[,1], "iid"], pid = sample_data[total_na[,1], "pid"]))
    total_na$na_col_names =  sapply(total_na$col, function(x){colnames(sample_data)[x]})
    na_stats = data.frame(table(total_na$col))
    na_stats$na_col_names =  sapply(na_stats$Var1, function(x){colnames(sample_data)[as.numeric(levels(x))[x]]})
    iid_counts = by(total_na, total_na$col, FUN = function(x){length(unique(x$iid))})
    iid_counts_dat = cbind(col=as.numeric(names(iid_counts)), iid_counts)
    na_stats = merge(iid_counts_dat, na_stats, by.x="col", by.y = "Var1")
    pid_counts = by(total_na, total_na$col, FUN = function(x){length(unique(x$pid))})
    pid_counts_dat = cbind(col=as.numeric(names(pid_counts)), pid_counts)
    na_stats = merge(pid_counts_dat, na_stats, by="col")
    total_na_split_by_col_name = split(total_na, total_na$na_col_names)
    na_iid_pid_by_col_name = lapply(total_na_split_by_col_name, function(x){list(iid_list = unique(x$iid), pid_list = unique(x$pid))})
    return(list(na_stats = na_stats, na_iid_pid = na_iid_pid_by_col_name))
}



result = detect.and.process.na(model_data)
result[[1]]


factor_features = c("iid", "pid", "samerace.pid", "wave.iid", "dec_part", "part_met")

for (feature in factor_features){
    model_data[, feature] = as.factor(model_data[, feature])
}

all_vars = colnames(model_data)

all_features = all_vars[which(!(all_vars %in% c("match.iid", "match.pid", "samerace.iid", "gender.iid", "gender.pid", "wave.pid") ))]

model_data_reduced  = model_data[complete.cases(model_data), ]
dim(model_data_reduced)

individual_feature = as.character(feature_select_data[which(feature_select_data$ind==1),"new_col"])
pair_wise_feature = as.character(feature_select_data[which(feature_select_data$pair==1), "new_col"])

M_pair_wise = fit.with.these.features(model_data_reduced, pair_wise_feature)
summary(M_pair_wise)
### Individual fixed effect makes all the preference NA. Not linearly independent 

pair_wise_no_pref_feature = pair_wise_feature[grep("pref_", pair_wise_feature, invert=T)]
print(summary(fit.with.these.features(model_data_reduced, pair_wise_no_pref_feature)))


# This is also promising 
pair_wise_no_id_no_wave_no_race = pair_wise_feature[which(!(pair_wise_feature %in% c("iid", "pid", "race_part", "age_self")))]
print(summary(fit.with.these.features(model_data_reduced, pair_wise_no_id_no_wave_no_race)))
pair_wise_no_id_feature = c(pair_wise_feature[which(!(pair_wise_feature %in% c("iid", "pid", "race_part", "age_self")))], "samerace.iid", "wave.iid")
print(summary(fit.with.these.features(model_data_reduced, pair_wise_no_id_feature)))
pair_wise_no_id_feature_reduced = c("pref_part_att", "pref_part_sha", "part_fun", "imprace", "wave.iid")
print(summary(fit.with.these.features(model_data_reduced, pair_wise_no_id_feature_reduced)))

## Major question: can we replace individual fixed effect with preference? 


## Same goal seems to be significant 
model_data_reduced$samegoal = as.factor(sapply(model_data_reduced$goal.iid - model_data_reduced$goal.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced$samecareer = as.factor(sapply(model_data_reduced$career_c.iid- model_data_reduced$career_c.pid, function(x){if(x==0) 1 else 0}))
pair_wise_no_id_career_goal_feature = c(pair_wise_no_id_feature, "samegoal", "samecareer")
print(summary(fit.with.these.features(model_data_reduced, pair_wise_no_id_career_goal_feature)))


pair_wise_no_id_data = model_data[, pair_wise_feature]
M_single_pair_wise_feature = list()
for (feature in pair_wise_no_id_feature){
    M_single_pair_wise_feature[[as.character(feature)]] = fit.with.these.features(model_data_reduced, c(feature))
    print(M_single_pair_wise_feature[[as.character(feature)]])
}




