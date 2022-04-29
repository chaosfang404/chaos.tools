chr_list <- function(
					hic_file = NA,
					chr_list = NA,
					extra = FALSE,
					mit = FALSE
){
	hic_chr <-	hic_file %>%
				strawr::readHicChroms() %>%
				.$name %>%
				.[!grepl("all",.,ignore.case = T)]

	random_chr <- hic_chr |> sample(1)

	if(grepl("chr", random_chr, ignore.case = T))
	{
		hic_chr_prefix <-	random_chr |> substr(1,3)
	}else
	{
		hic_chr_prefix <-	""
	}


	chr_list <- chr_list %>% 
				as.character() %>%
				.[! . %in% c(NA,NaN)] %>%
				gsub("chr", "", ., ignore.case = T) %>%
				.[! . %in% ""] %>%
				unique()

	if(length(chr_list) == 0)
	{
		chr_list <- NA
	}

	if(length(chr_list) == 1)
	{
		if(!is.na(chr_list))
		{
			chr_list <- paste0(hic_chr_prefix,chr_list)
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
		chr_list <- paste0(hic_chr_prefix,chr_list)
	}

	chr_list %>% .[. %in% hic_chr]
}


chr_list_dt <- function(
					hic_file = NA,
					chr_list = NA,
					extra = FALSE,
					mit = FALSE,
					inter = "half"
){
	chr_list <- chr_list(
					hic_file = hic_file,
					chr_list = chr_list,
					extra = extra,
					mit = mit
				)

	if(length(chr_list) == 1){inter <- "intra"}

	dt_comb(
		chr_list,
		rep = 2,
		inter = inter
	)
}
