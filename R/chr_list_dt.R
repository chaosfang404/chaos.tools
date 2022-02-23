chr_list_dt <- function(
					hic_file = NA,
					chr_list = NA,
					extra = FALSE,
					mit = FALSE,
					inter = "half"
){
	hic_file_chr <-	hic_file %>%
					strawr::readHicChroms() %>%
					.$name %>%
					.[!. %in% c("ALL","All")]

	chr_list <- chr_list %>% 
				unique() %>% 
				as.character() %>%
				.[! . %in% c(NA,NaN)] %>%
				gsub("chr","",.)

	if(length(chr_list) == 0)
	{
		chr_list <- NA
	}

	if(length(chr_list) == 1)
	{
		if(!is.na(chr_list))
		{
			inter <- "intra"
		}else
		{
			chr_list <- hic_file_chr

			if(isFALSE(extra))
			{
				chr_list <- chr_list[!str_detect(chr_list,"_")]
			}

			if(isFALSE(mit))
			{
				chr_list <- chr_list[!str_detect(chr_list,"M")]
			}
		}
	}else
	{
		hic_chr_symbol <-	hic_file_chr %>%
							sample(1) %>%
							str_detect("chr")

		if(isTRUE(hic_chr_symbol))
		{
			chr_list <- paste0("chr",chr_list)
		}
	}

	dt_comb(
		chr_list,
		rep = 2,
		inter = inter
	)
}