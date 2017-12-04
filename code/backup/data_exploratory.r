## Author: Chunyi Zhao 
## Data exploratory analysis script


## TODO 1) individual box plot (rating, prob, ...) 2) wave difference 

require(ggplot2)
require(ggthemes)
# CUR_DIC = getwd()
# setwd("..")
# PARENT_DIC = getwd()
# setwd(CUR_DIC)
# DATA_DIC = paste(PARENT_DIC, "/data/", sep="")
# DATA_SUB_DIC = paste(DATA_DIC, "toy_data_11_25/", sep="")
# OUTPUT_DIC = paste(PARENT_DIC, "output/test_toy_data_11_25", sep="")
# FEATURE_DATA = paste(DATA_DIC, "feature_select.csv", sep="")
# MODEL_DATA = paste(DATA_SUB_DIC, "TEST_MODEL.csv", sep="")
# SAMPLE_DATA = paste(DATA_SUB_DIC, "TEST_SAMPLE.csv", sep="")
# model_data = read.csv(MODEL_DATA)
# sample_data = read.csv(SAMPLE_DATA)
# feature_select_data = read.csv(FEATURE_DATA)



# individual_feature = as.character(feature_select_data[which(feature_select_data$ind==1),"new_col"])
# pair_wise_feature = as.character(feature_select_data[which(feature_select_data$pair==1), "new_col"])

individual.feature.plot = function(sample_data, feature_list, output_dic, individual_feature){
    attach(sample_data)
    agg_by_mean = aggregate(sample_data[, c("gender", "age_self")], by = list(iid), FUN=mean, na.rm=FALSE)
    agg_by_count = aggregate(sample_data[, c("match", "samerace")], by = list(iid), FUN=sum, na.rm=FALSE)
    detach(sample_data)
    
    ind_group_by_data = merge(agg_by_mean, agg_by_count, by = "Group.1")
    
    # age, gender, match, race 
    p = list()
    for (feature in colnames(ind_group_by_data)){
        print(feature)
        # p[[feature]] = as.data.frame(table(ind_group_by_data[, feature]))
        p[[feature]] = ggplot(data=as.data.frame(table(ind_group_by_data[, feature])), aes(x=Var1, y=Freq)) +
            geom_bar(stat="identity", fill="steelblue") + labs(x=as.character(feature)) + theme_economist() + geom_text(aes(label=Freq), vjust=1.6, color="white", size=4)
        print(p[[feature]])
    }
    
    
    ## Individual rating difference
    ggplot(data=data.frame(cbind(att_rate = sample_data$part_att, id = as.factor(sample_data[, "pid"]))), aes(x = as.factor(id), y=att_rate)) + geom_boxplot()
    
}

pair.wise.feature.plot = function(model_data, output_list, pair_wise_feature){
    q = list()
    for (feature in pair_wise_feature[3:length(pair_wise_feature)]){
        q[[feature]]=ggplot(data = data.frame(cbind(feature = model_data[, feature]), wave = as.factor(model_data[, "wave.iid"])), aes(x = wave, y = feature)) + geom_boxplot() + labs(y=feature) + theme_economist()
        print(q[[feature]])
        }
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


## Anova on average rating ~ age cohort + other individual specific varaibles ... 

## Rating of others ~ rating of self 

## match with age 
