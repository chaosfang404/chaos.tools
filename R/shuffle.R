shuffle <- function(
				.data,
				ref = "hg19"
){
	chr_info <- chr_size(
					ref = ref,
					extra = T,
					mit = T
				) %>%
				setkey()

	first_3_names <- colnames(.data[,1:3])

	dt <- .data[,1:3] %>%
			setnames(c("chr","start","end")) %>%
			setkey()

	if(ncol(.data) >=4)
	{
		dt_rest <- .data[,4:ncol(.data)]
	}else
	{
		dt_rest <- NULL
	}
	
	chr_character <- dt[,chr] %>% 
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
		dt[
			,chr := gsub(chr,pattern = "chr",replacement = "")
		][
			,chr := paste0("chr",chr)
		] %>%
		setkey()
	}

	merge(
		dt,
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