require(magrittr)
require(data.table)

dt_comb <- function(
				.data,
				c2,
				...
){
	str_comb(.data,c2,...) %>%
		tstrsplit(split = "_") %>%
		setDT() %>%
		.[]   	
}
