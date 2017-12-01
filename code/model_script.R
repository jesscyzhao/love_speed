FACTOR_FEATURES = c("iid", "pid", "samerace", "wave", "part_met", "samegoal", "samecareer", "samefield")

source(MODEL_FUNCTION)

########################################Data prep###############################################################
model_data = model_data[which(model_data$part_met.pid!=3), ]
all_vars = colnames(model_data)
all_features = all_vars[which(!(all_vars %in% c("match.iid", "match.pid", "samerace.iid", "gender.iid", "gender.pid", "wave.pid") ))]
individual_feature = as.character(feature_select_data[which(feature_select_data$ind==1),"new_col"])
original_pair_wise_feature = as.character(feature_select_data[which(feature_select_data$pair==1), "new_col"])
pair_wise_feature = c(original_pair_wise_feature[3:length(original_pair_wise_feature)], "samegoal", "samecareer", "samefield")

factor_features = FACTOR_FEATURES
model_data_no_na  = model_data[complete.cases(model_data), ]
model_data_reduced = model_data_no_na[, original_pair_wise_feature]
model_data_reduced$samegoal = as.factor(sapply(model_data_no_na$goal.iid - model_data_no_na$goal.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced$samecareer = as.factor(sapply(model_data_no_na$career_c.iid- model_data_no_na$career_c.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced$samefield = as.factor(sapply(model_data_no_na$field_cd.iid- model_data_no_na$field_cd.pid, function(x){if(x==0) 1 else 0}))
model_data_reduced$wave = model_data_no_na$wave.iid
model_data_reduced$samerace = model_data_no_na$samerace.iid
model_data_reduced$part_met = model_data_no_na$part_met.iid
model_data_reduced$match = model_data_no_na$match.iid

model_data_reduced = model_data_reduced[complete.cases(model_data_reduced),]
for (feature in factor_features){
    model_data_reduced[, feature] = as.factor(model_data_reduced[, feature])
}

###############################################################################################################




pref_features = all_features[grep("pref_", all_features)]
rate_features = all_features[grep("part_", all_features)]
rate_features = rate_features[grep("pref_", rate_features, invert=T)]
rate_features = rate_features[which(!(rate_features %in% c("part_met.iid", "part_met.pid",  "dec_part.iid", "dec_part.pid")))]
individual_diff_features = c("age_part", "imprace", "imprelig", "date", "go_out", "samegoal", "samecareer", "samefield", "samerace")

complete_features = c(pref_features, rate_features, individual_diff_features)

# No individual effect 
result=auto.fit.and.model.compare(model_data_reduced, complete_features, factor_features)

step_result = auto.fit.and.step.select(model_data_reduced, complete_features, factor_features)

print("complete features")
print(result$m1_sig_features)
print(step_result$m1_sig_features)


# With individual effect
result_ind = auto.fit.and.model.compare(model_data_reduced, c(complete_features, "iid", "pid"), factor_features)
## Currently it deosn't converge 

# With wave effect 
result_wave = auto.fit.and.model.compare(model_data_reduced, c(complete_features, "wave"))
step_result_wave = auto.fit.and.step.select(model_data_reduced, c(complete_features, "wave"))

print("complete features with wave effect")
print(result_wave$m1_sig_features)
print(step_result_wave$m1_sig_features)

### Individual fixed effect makes all the preference NA. Not linearly independent 
M_wave_effect = fit.with.these.features(model_data_reduced, c("wave"))
summary(M_wave_effect)

## Set up individual regression to see what's significant

## Let's get rid of first wave and see
model_data_no_first_wave = model_data_reduced[which(model_data_reduced$wave!=1), ]

M_no_first_wave = fit.with.these.features(model_data_no_first_wave, c("wave"))
summary(M_no_first_wave)

# No individual effect 
no_first_wave_result=auto.fit.and.model.compare(model_data_no_first_wave, complete_features, factor_features)
# With individual effect
no_first_wave_result_ind = auto.fit.and.model.compare(model_data_no_first_wave, c(complete_features, "iid", "pid"), factor_features)
## Currently it deosn't converge 

# With wave effect 
no_first_result_wave = auto.fit.and.model.compare(model_data_no_first_wave, c(complete_features, "wave"))

propose_features = clean.up.sigificant.feature(no_first_result_wave$m1_sig_features, factor_features)

propose_features_ind_effect = c(propose_features,  "iid", "pid")

M_1 = fit.with.these.features(model_data_no_first_wave, propose_features)
# Still doesn't converge
M_2 = fit.with.these.features(model_data_no_first_wave, propose_features_ind_effect)

print(summary(M_1))

AIC(M_1)
AIC(M_2)

feature_list = list(pref = pref_features, rate = rate_features, ind_diff= individual_diff_features)

by_wave_result = list()

model_data_reduced_by_wave = split(model_data_reduced, model_data_reduced$wave)

for (wave in names(model_data_reduced_by_wave)){
    print(wave)
    model_data_this_wave = model_data_reduced_by_wave[[as.character(wave)]]
    this_wave_result = list()
    for (feature_type in names(feature_list)){
        print(feature_type)
        feature_to_fit = if(wave==16 & feature_type == "ind_diff") setdiff(feature_list[[as.character(feature_type)]], c("samecareer", "samefield")) else feature_list[[as.character(feature_type)]]
        this_wave_result[[as.character(feature_type)]] = auto.fit.and.model.compare(model_data_this_wave, feature_to_fit)
    }
    this_wave_proposed_feature = clean.up.sigificant.feature(unlist(lapply(this_wave_result, function(x){x$m1_sig_features})))
    final_result = auto.fit.and.model.compare(model_data_this_wave, this_wave_proposed_feature)
    print("proposed feature")
    print (final_result$m1_sig_features)
    this_wave_result[["final_result"]] = final_result
    by_wave_result[[as.character(wave)]] = this_wave_result
}
