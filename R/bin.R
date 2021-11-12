bin <- function(
				.data,
				column = NA,
				x = 0,
				positive = 1,
				negative = 0
){
	dt <- data.table(.data)

	if(length(column) == 1)
	{
		if(is.na(column))
		{
			column <- lapply(
							colnames(.data),
							function(i){if(is.numeric(dt[[i]])){i}}
						) %>% unlist
		}
	}

	numeric_dt <- dt[, ..column]
	numeric_dt[numeric_dt > x] <- positive
	numeric_dt[numeric_dt <= x] <- negative
	dt[,(column) := numeric_dt][]
}
