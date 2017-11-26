## Author: Chunyi Zhao 
## Data processing script 

CUR_DIC = getwd()
setwd("..")
PARENT_DIC = getwd()
setwd(CUR_DIC)
DATA_DIC = paste(PARENT_DIC, "/data/", sep="")
RAW_DATA = paste(DATA_DIC, "speed_dating_data.csv", sep="")
FEATURE_DATA = paste(DATA_DIC, "feature_select.csv", sep="")
WAVE_LIST = c(1, 2, 3)
DATE = c(strsplit(as.character(Sys.time()), split=" ")[[1]][1], 
	strsplit(strsplit(as.character(Sys.time()), split=" ")[[1]][2], split=":")[[1]])
MARK = "TEST_"
LABEL = paste("wave", paste(WAVE_LIST, collapse="_"), paste(DATE, collapse="_"), sep="_")
WRITE = TRUE

choose.features = function(raw_data, feature_select_data){
	total_cols = colnames(raw_data) 
	select_cols = feature_select_data$raw_col 
	sample_data = raw_data[which(raw_data$wave %in% WAVE_LIST), total_cols %in% select_cols]
	print(colnames(sample_data))
	print(feature_select_data$new_col)
	colnames(sample_data) = feature_select_data$new_col 
	sample_group = unlist(lapply(split(sample_data, sample_data$gender), function(x){split(x, x$wave)}), recursive = FALSE)
	counts = unlist(lapply(sample_group, function(x){length(unique(x$iid))}))
	summary_table = matrix(counts, ncol=2)
	individual_feature = as.character(feature_select_data[which(feature_select_data$ind==1),"new_col"])
	pair_wise_feature = as.character(feature_select_data[which(feature_select_data$pair==1), "new_col"])
	result = pair.and.combine(sample_data, summary_table, individual_feature, pair_wise_feature)
	summary_table = result$summary
	colnames(summary_table) = c("M", "F", "Pairs")
	rownames(summary_table) = WAVE_LIST
	return(list(sample = sample_data, model = result$model, counts = summary_table))
}

pair.and.combine = function(sample_data, summary_table, individual_feature, pair_wise_feature){
    sample_data_by_wave = split(sample_data, sample_data$wave)
    model_data = data.frame()  
    pair_counts = c()
    i = 1
    for (i in 1:length(WAVE_LIST)){
        print(WAVE_LIST[i])
        this_group = sample_data_by_wave[[as.character(WAVE_LIST[i])]]
        this_group_by_gender = split(this_group, this_group$gender)
        A = this_group_by_gender$`0`[, individual_feature]
        B = this_group_by_gender$`1`[, individual_feature]
        combined = merge(A, B, by.x=c("iid", "pid"), by.y=c("pid", "iid"))
        
        X = this_group_by_gender$`0`[, pair_wise_feature]
        Y = this_group_by_gender$`1`[, pair_wise_feature]
        Y$pid=this_group_by_gender$`1`[, "iid"]
        Y$iid=this_group_by_gender$`1`[, "pid"]
        
        X_sorted = X[with(X, order(X$iid, X$pid)), ]
        Y_sorted = Y[with(Y, order(Y$iid, Y$pid)), ]
        
        if (all(X_sorted$iid == Y_sorted$iid) & all(X_sorted$pid == Y_sorted$pid)){
            iid = X_sorted$iid
            pid = X_sorted$pid
            XY_diff = cbind(iid, pid, X_sorted[, 3:length(pair_wise_feature)] - Y_sorted[, 3:length(pair_wise_feature)])
            
        }
        else{
            print("iid and pid do not match for male and female")
            stop()
        }
        
        combined = merge(combined, XY_diff, by=c("iid", "pid"))
        col_names = colnames(combined)
        col_names = gsub(".x", ".iid", col_names)
        col_names = gsub(".y", ".pid", col_names)
        
        colnames(combined) = col_names
        
        pair_counts = c(pair_counts, dim(combined)[1])
        
        model_data = rbind(model_data, combined)
    
    }
    
    summary_table= cbind(summary_table, pair_counts)
    
    return(list(model = model_data, summary = summary_table))
    
}


data.process = function(){
	raw_data = read.csv(RAW_DATA)
	feature_select_data = read.csv(FEATURE_DATA)
	result = choose.features(raw_data, feature_select_data)
	sample_data = result$sample
	model_data = result$model
	summary_table = result$counts
	if (WRITE){
       write.csv(sample_data, paste(DATA_DIC, paste(MARK, "SAMPLE_data_", LABEL, ".csv", sep=""), sep=""), row.names=FALSE)
        write.csv(summary_table, paste(DATA_DIC, paste(MARK, "SUMMARY_data_", LABEL, ".csv", sep=""), sep=""), row.names = FALSE)
        write.csv(model_data, paste(DATA_DIC, paste(MARK, "MODEL_data_", LABEL, ".csv", sep=""), sep=""), row.names = FALSE)
    }
}

data.process()
