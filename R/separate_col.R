separate_col <- function(
					.data,
					column,
					into = NA,
					sep = "[^[:alnum:]]+",
					remove = TRUE
){
	split_columns <- as.data.table(.data)%>%
						.[[column]] %>%
						tstrsplit(split = sep) %>%
						setDT()
    
	if(is.na(into))
	{
		into <- paste(
					"splited",
					column,
					seq(1,ncol(split_columns),1),
					sep = "_"
				)
	}
  
	split_columns %>%
		setnames(into) 

	if(isTRUE(remove))
	{
		data.table(.data)[
			,(column) := NULL
		][
			,names(split_columns) := split_columns
		][]
	} else
	{
		data.table(.data)[
			,names(split_columns) := split_columns
		][]
	}
}
