chr_list_dt <- function(
					hic_file = NA,
					chr_list = NA,
					inter = "all"
){
	chr_list <- chr_list %>% unique() %>% as.character()

	if(length(chr_list) == 1)
	{
		if(!is.na(chr_list))
		{
			chr_list_dt <- chr_list %>% rep(2) %>% combn(2) %>% t() %>% data.table()
		}else
		{
			chr_list <- data.table(
							strawr::readHicChroms(hic_file)
						)[
							name != "ALL",name
						] %>%
						as.character()
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
	chr_list_dt
}