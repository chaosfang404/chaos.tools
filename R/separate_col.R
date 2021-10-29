separate_col <- function(
					.data,
					column,
					into = NA,
					select = NA,
					sep = "[^[:alnum:]]+",
					remove = TRUE
){
	split_columns <- as.data.table(.data)%>%
						.[[column]] %>%
						tstrsplit(split = sep) %>%
						setDT()
 
	default_name <- paste(
						"splited",
						column,
						seq(1,ncol(split_columns),1),
						sep = "_"
					)

	setnames(split_columns,default_name)

	if(length(select) == 1)
	{
		if(is.na(select))
		{
			select = seq(1,ncol(split_columns),1)
		}
	}

	selected_columns <- paste("splited",column,select,sep = "_")
	split_columns <- split_columns[,..selected_columns]

	if(length(into) == 1)
	{
		if(is.na(into))
		{
			into <- default_name[select]
		}
	}

	setnames(split_columns,into)


	if(isTRUE(remove))
	{
		dt <- data.table(.data)[
					,(column) := NULL
				][
					,names(split_columns) := split_columns
				][]
	} else
	{
		dt <- data.table(.data)[
					,names(split_columns) := split_columns
				][]
	}
	dt
}
