chr_list_dt <- function(
					hic_file = NA,
					chr_list = NA,
					extra = FALSE,
					mit = FALSE,
					inter = "half"
){
	hic_chr <-	hic_file %>%
				strawr::readHicChroms() %>%
				.$name %>%
				.[!grepl("all",.,ignore.case = T)]

	chr_list <- chr_list %>% 
				unique() %>% 
				as.character() %>%
				.[! . %in% c(NA,NaN)] %>%
				gsub("chr", "", ., ignore.case = T) %>%
				.[! . %in% ""]

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
			chr_list <- hic_chr

			if(isFALSE(extra))
			{
				chr_list <- chr_list %>% .[!grepl("_",.)]
			}

			if(isFALSE(mit))
			{
				chr_list <- chr_list %>% .[!grepl("M",.)]
			}
		}
	}else
	{
		random_chr <- sample(hic_chr,1)

		if(grepl("chr", random_chr, ignore.case = T))
		{
			chr_list <- random_chr |> 
						substr(1,3) |>
						paste0(chr_list)
		}
	}

	dt_comb(
		chr_list,
		rep = 2,
		inter = inter
	)
}
