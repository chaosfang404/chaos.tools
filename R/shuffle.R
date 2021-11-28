shuffle <- function(
				.data,
				ref = "hg19"
){
	dt <- as.data.table(.data)


	chr_info <- chr_size(
					ref = ref,
					extra = T,
					mit = T
				) %>%
				setkey()

	first_3_names <- colnames(dt[,1:3])

	dt_first_3 <- dt[,1:3] %>%
					setnames(c("chr","start","end")) %>%
					setkey()

	if(ncol(dt) >=4)
	{
		dt_rest <- dt[,4:ncol(dt)]
	}else
	{
		dt_rest <- NULL
	}
	
	chr_character <- dt_first_3$chr %>% 
					grepl(pattern = "chr") %>% 
					unique()

	if(length(chr_character) == 1)
	{
		if(isFALSE(chr_character))
		{
			chr_info[
				,chr := gsub(chr,pattern = "chr",replacement = "")
			] %>%
			setkey()
		}
	}else
	{
		dt_first_3[
			,chr := gsub(chr,pattern = "chr",replacement = "")
		][
			,chr := paste0("chr",chr)
		] %>%
		setkey()
	}

	merge(
		dt_first_3,
		chr_info
	)[
		,peak_length := end - start
	][
		,range := length - peak_length
	][
		,new_start := sapply(range,function(x){sample(1:x,1)})
	][
		,new_end := new_start + peak_length
	][
		,.(chr,new_start,new_end)
	] %>%
	setnames(first_3_names) %>%
	cbind(dt_rest)
}