chr_list_dt <- function(
					hic_file = NA,
					chr_list = NA,
					inter = "half"
){
	chr_list <- chr_list %>% 
				unique() %>% 
				as.character()

	if(length(chr_list) == 1)
	{
		if(!is.na(chr_list))
		{
			chr_list %>% 
			rep(2) %>% 
			combn(2) %>% 
			t() %>% 
			as.data.table()
		}else
		{
			as.data.table(
				strawr::readHicChroms(hic_file)
			)[
				name != "ALL",
				name
			] %>%
			as.character() %>%
			dt_comb(
				rep = 2,
				inter = inter
			)

		}
	}else
	{
		dt_comb(
			chr_list,
			rep = 2,
			inter = inter
		)
	}
}