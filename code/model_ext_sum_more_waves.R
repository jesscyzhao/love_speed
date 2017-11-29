CUR_DIC = getwd()
setwd("..")
PARENT_DIC = getwd()
setwd(CUR_DIC)
DATA_DIC = paste(PARENT_DIC, "/data/", sep="")
DATA_SUB_DIC = paste(DATA_DIC, "toy_data_sum_more_wave_11_28/", sep="")
#OUTPUT_DIC = paste(PARENT_DIC, "output/toy_data_ext_11_28", sep="")
FEATURE_DATA = paste(DATA_DIC, "feature_select_ext.csv", sep="")
MODEL_DATA = paste(DATA_SUB_DIC, "TEST_SUM_MODEL.csv", sep="")
SAMPLE_DATA = paste(DATA_SUB_DIC, "TEST_SUM_SAMPLE.csv", sep="")
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

########################################Data prep###############################################################
model_data = model_data[which(model_data$part_met.pid!=3), ]

factor_features = c("iid", "pid", "samerace.pid", "wave.iid", "part_met.pid")

for (feature in factor_features){
    model_data[, feature] = as.factor(model_data[, feature])
}
model_data_reduced  = model_data[complete.cases(model_data), ]
model_data_reduced$samegoal = as.factor(sapply(model_data_reduced$goal.iid - model_data_reduced$goal.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced$samecareer = as.factor(sapply(model_data_reduced$career_c.iid- model_data_reduced$career_c.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced$samefield = as.factor(sapply(model_data_reduced$field_cd.iid- model_data_reduced$field_cd.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced = model_data_reduced[complete.cases(model_data_reduced),]
###############################################################################################################

all_vars = colnames(model_data)

all_features = all_vars[which(!(all_vars %in% c("match.iid", "match.pid", "samerace.iid", "gender.iid", "gender.pid", "wave.pid", "iid", "pid") ))]


individual_feature = as.character(feature_select_data[which(feature_select_data$ind==1),"new_col"])
pair_wise_feature = as.character(feature_select_data[which(feature_select_data$pair==1), "new_col"])

M_pair_wise = fit.with.these.features(model_data_reduced, c(pair_wise_feature[3:length(pair_wise_feature)], "iid", "pid"))
summary(M_pair_wise)
### Individual fixed effect makes all the preference NA. Not linearly independent 


pref_features = all_features[grep("pref_", all_features)]
rate_features = all_features[grep("part_", all_features)]
rate_features = rate_features[grep("pref_", rate_features, invert=T)]
rate_features = rate_features[which(!(rate_features %in% c("part_met.iid", "part_met.pid",  "dec_part.iid", "dec_part.pid")))]
individual_diff_features = c("age_part", "imprace", "imprelig", "date", "go_out", "samegoal", "samecareer", "samefield", "samerace.pid")


## What is special of wave 1 
M_wave_effect = fit.with.these.features(model_data_reduced, c("wave.iid"))
summary(M_wave_effect)

## Let's get rid of first wave and see
model_data_no_first_wave = model_data_reduced[which(model_data_reduced$wave.iid!=1), ]
M_no_first_wave = fit.with.these.features(model_data_no_first_wave, c("wave.iid"))
summary(M_no_first_wave)

print(summary(fit.with.these.features(model_data_no_first_wave, all_features)))

print(summary(fit.with.these.features(model_data_no_first_wave, pref_features)))

print(summary(fit.with.these.features(model_data_no_first_wave, rate_features)))

print(summary(fit.with.these.features(model_data_no_first_wave, individual_diff_features)))


propose_features = c( "age_part", "pref_part_sha", "part_att", "part_sha", "part_amb", "part_lik", "part_prob", "part_met.pid", "imprace",  "wave.iid", "go_out", "samegoal", "samecareer", "samefield", 'samerace.pid')


propose_features_ind_effect = c( "age_part", "pref_part_sha", "part_att", "part_amb", "part_lik", "part_prob", "part_met.pid", "imprace", "go_out", "samegoal", "samecareer", "iid", "pid")

M_1 = fit.with.these.features(model_data_no_first_wave, propose_features)
M_2 = fit.with.these.features(model_data_no_first_wave, propose_features_ind_effect)
print(summary(M_1))
print(summary(M_2))

anova(M_1, M_2, test="F")
AIC(M_1)
AIC(M_2)


# SO sofar, 1) saw first wave is different from the rest 2) first wave has higher success rate 3) fit without first wave, propose the features above. 

model_data_first_wave = model_data_reduced[which(model_data_reduced$wave.iid ==1), ]

print(summary(fit.with.these.features(model_data_first_wave, pref_features)))

print(summary(fit.with.these.features(model_data_first_wave, rate_features)))

print(summary(fit.with.these.features(model_data_first_wave, individual_diff_features)))

print(summary(fit.with.these.features(model_data_first_wave, c(pref_features, "part_sin", "samegoal", "samecareer"))))


