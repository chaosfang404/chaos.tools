hic_interaction <- function(
						hic_file,
						chr_list = NA,
						norm = "KR",
						resolution = 1e6,
						inter = "all"
){
	chr_list <- chr_list %>% unique() %>% as.character()

	if(length(chr_list) == 1)
	{
		if(!is.na(chr_lsit))
		{
			chr_list_dt <- chr_list %>% rep(2) %>% combn(2) %>% t() %>% data.table()
		}else
		{
			chr_list <- data.table(
							readHicChroms(hic_file)
						)[
							name != "ALL",name
						]
			if(inter == "inter")
			{
				chr_list_dt <- chr_list %>% combn(2) %>% t() %>% data.table()

			}else if(inter == "intra")
			{
				chr_list_dt <- data.table(V1 = chr_list, V2 = chr_list)

			}else if(inter == "all")
			{
				chr_list_dt <- rbind( 
									chr_list %>% combn(2) %>% t() %>% data.table(),
									data.table(V1 = chr_list,V2 = chr_list)
								)
			}
		}
	}else
	{
		if(inter == "inter")
		{
			chr_list_dt <- chr_list %>% combn(2) %>% t() %>% data.table()

		}else if(inter == "intra")
		{
			chr_list_dt <- data.table(V1 = chr_list, V2 = chr_list)

		}else if(inter == "all")
		{
			chr_list_dt <- rbind( 
								chr_list %>% combn(2) %>% t() %>% data.table(),
								data.table(V1 = chr_list,V2 = chr_list)
							)
		}
	}

	data <- data.table(NULL)
	for (i in 1:nrow(chr_list_dt))
	{
		chr1 <- chr_list_dt[i,V1]
		chr2 <- chr_list_dt[i,V2]

		tmp <- data.table(
					strawr::straw(norm, hic_file, chr1, chr2, "BP", resolution)
				)[
					,chr1 := chr1
				][
					,chr2 := chr2
				] %>%
				rename_dt(chr1_bin =x, chr2_bin = y) %>%
				replace_na_dt(to = 0)
		data <- rbind(data,tmp)
	}
	data
}
