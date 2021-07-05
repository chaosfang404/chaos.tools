require(magrittr)
require(data.table)

dt_comb <- function(
				.data,
				c2,
				c3 = NA,
				c4 = NA,
				c5 = NA,
				c6 = NA,
				c7 = NA,
				c8 = NA
){
	str_comb(.data,c2,c3,c4,c5,c6,c7,c8) %>%
		tstrsplit(split = "_") %>%
		setDT() %>%
		.[]   	
}
