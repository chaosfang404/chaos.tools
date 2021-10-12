separate_col <- function(
					.data,
					column,
					into = "tmp_splited_columns_names",
					sep = "[^[:alnum:]]+",
					remove = TRUE
){
	split_columns <- as.data.table(.data)%>%
						.[[column]] %>%
						tstrsplit(split = sep) %>%
						setDT()
    
	if(identical(into,"tmp_splited_columns_names"))
	{
		into2 <- paste(
					"splited",
					column,
					seq(1,ncol(split_columns),1),
					sep = "_"
				)        
	} else
	{
		into2 <- into
	}
  
	split_columns %>%
		setnames(into2) 

	if(remove)
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
