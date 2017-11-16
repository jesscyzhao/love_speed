## Author: Chunyi Zhao 
## Data processing script 

PARENT_DIC = "C:\\Users\\Chunyi Zhao\\Projects\\AMS204\\love_speed"
DATA_DIC = paste(PARENT_DIC, "\\data\\", sep="")
RAW_DATA = paste(DATA_DIC, "speed_dating_data.csv", sep="")
FEATURE_DATA = paste(DATA_DIC, "feature_select.csv", sep="")
WAVE_LIST = c(1, 2, 3)
DATE = c(strsplit(as.character(Sys.time()), split=" ")[[1]][1], 
	strsplit(strsplit(as.character(Sys.time()), split=" ")[[1]][2], split=":")[[1]])
MARK = "TEST_"
LABEL = paste("wave", paste(WAVE_LIST, collapse="_"), paste(DATE, collapse="_"), sep="_")

choose.features = function(raw_data, feature_select_data, wave_list){
	total_cols = colnames(raw_data) 
	select_cols = feature_select_data$raw_col 
	sample_data = raw_data[which(raw_data$wave %in% wave_list), total_cols %in% select_cols]
	print(colnames(sample_data))
	print(feature_select_data$new_col)
	colnames(sample_data) = feature_select_data$new_col 
	sample_group = unlist(lapply(split(sample_data, sample_data$gender), function(x){split(x, x$wave)}), recursive = FALSE)
	counts = unlist(lapply(sample_group, function(x){length(unique(x$iid))}))
	summary_table = t(matrix(counts, nrow=3, ncol=2))
	rownames(summary_table) = c("M", "F")
	colnames(summary_table) = wave_list
	return(list(a = sample_data, b = summary_table))
}


data.process = function(){
	raw_data = read.csv(RAW_DATA)
	feature_select_data = read.csv(FEATURE_DATA)
	result = choose.features(raw_data, feature_select_data, WAVE_LIST)
	sample_data = result$a
	summary_table = result$b
	write.csv(sample_data, paste(DATA_DIC, paste(MARK, "SAMPLE_data_", LABEL, ".csv", sep=""), sep=""), row.names=FALSE)
	write.csv(summary_table, paste(DATA_DIC, paste(MARK, "SUMMARY_data_", LABEL, ".csv", sep=""), sep=""))
}


data.process()
