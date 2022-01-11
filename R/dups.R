dups <- function(
			.data,
			col = NA,
			all = TRUE
){
	dt <- data.table(.data)

	if(is.na(col))
	{
		dt[duplicated(dt)]
	}else
	{
		setnames(
			dt,
			old = col,
			new = "i_hope_you_will_not_find_out"
		)

		if(isTRUE(all))
		{
			dup_list <- dt[
							duplicated(i_hope_you_will_not_find_out),
							i_hope_you_will_not_find_out
						]

			dt2 <- dt[
						i_hope_you_will_not_find_out %in% dup_list
					][
						order(i_hope_you_will_not_find_out)
					]
		}else
		{
			dt2 <- dt[duplicated(i_hope_you_will_not_find_out)]
		}

		setnames(
			dt2,
			old = "i_hope_you_will_not_find_out", 
			new = col
		)[]
	}
}