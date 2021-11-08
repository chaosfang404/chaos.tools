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
