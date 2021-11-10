dt_comb <- function(
				...
){
	sapply(
		list(...),
		function(x){rep(x,length.out = max(sapply(list(...),length)))}
	) %>% 
	data.table() %>% 
	complete_dt()
}

str_comb <- function(
				...,
				sep = "_"
){
	dt_comb(...) %>% 
	unite_dt(
		colnames(.),
		sep = sep
	) %>% 
	.[,V1]
}


dt_comb2 <- function(
			x,
			inter = "all"
){
	if(inter == "inter")
	{
		x %>% 
		combn(2) %>% 
		t() %>% 
		as.data.table()

	}else if(inter == "intra")
	{
		data.table(
			V1 = x, 
			V2 = x
		)

	}else if(inter == "all")
	{
		rbind( 
			x %>% 
			combn(2) %>% 
			t() %>% 
			as.data.table(),
			data.table(
				V1 = x,
				V2 = x
			)
		)
	}
}