dt_comb <- function(
				...,
				rep = NA,
				inter = "full"
){
	if(!is.na(rep))
	{
		if(rep == 2)
		{
			if(inter == "inter")
			{
				c(...) %>% 
				combn(2) %>% 
				t() %>% 
				as.data.table()
			}else if(inter == "intra")
			{
				data.table(
					V1 = c(...), 
					V2 = c(...)
				)
			}else if(inter == "half")
			{
				rbind( 
					c(...) %>% 
					combn(2) %>% 
					t() %>% 
					as.data.table(),
					data.table(
						V1 = c(...),
						V2 = c(...)
					)
				)
			}else if(inter == "full")
			{
				data.table(
					c(...),
					c(...)
				) %>%
				complete_dt()
			}else
			{
				print("inter, intra, half and full are accepted")
			}
		}else
		{
			list(...) %>%
			rep(rep) %>% 
			data.frame() %>% 
			complete_dt() %>%
			setnames(
				paste0("V",1:ncol(.))
			) %>%
			.[]
		}
	}else
	{
		sapply(
			list(...), 
			function(x){rep(x, length.out = max(sapply(list(...), length)))}
		) %>% 
		data.table() %>% 
		complete_dt()
	}
}

str_comb <- function(
				...,
				rep = NA,
				inter = "full",
				sep = "_"
){
	dt_comb(
		...,
		rep = rep,
		inter = inter
	) %>% 
	unite_dt(
		colnames(.),
		sep = sep
	) %>% 
	.[,V1]
}
