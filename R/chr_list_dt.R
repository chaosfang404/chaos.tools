chr_list_dt <- function(
					hic_file = NA,
					chr_list = NA,
					extra = FALSE,
					mit = FALSE,
					inter = "half"
){
	chr_list <- chr_list %>% 
				unique() %>% 
				as.character() %>%
				.[! . %in% c(NA,NaN)]

	if(length(chr_list) == 1 | length(chr_list) == 0)
	{
		if(!is.na(chr_list))
		{
			inter <- "intra"
		}else
		{
			chr_list <- hic_file %>%
						strawr::readHicChroms() %>%
						.$name %>%
						.[!. %in% c("ALL","All")]

			if(isFALSE(extra))
			{
				chr_list <- chr_list[!str_detect(chr_list,"_")]
			}

			if(isFALSE(mit))
			{
				chr_list <- chr_list[!str_detect(chr_list,"M")]
			}
		}
	}

	dt_comb(
		chr_list,
		rep = 2,
		inter = inter
	)
}