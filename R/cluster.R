cluster <- function(
			.data,
			AR = "left",
			gene = NA
){
	if(AR == "left")
	{
		dt <- .data[left_AR > 0 & right_AR == 0]
	}else if(AR == "right")
	{
		dt <- .data[left_AR == 0 & right_AR > 0]
	}else if(AR == "both")
	{
		dt <- .data[left_AR > 0 & right_AR >0]
	}else if(AR == "none")
	{
		dt <- .data[left_AR == 0 & right_AR ==0]
	}

	if(is.na(gene))
	{
		dt
	}else if(gene == "left_up")
	{
		dt[left_up > 0 & left_down == 0 & right_up == 0 & right_down == 0]
	}else if(gene == "left_down")
	{
		dt[left_up == 0 & left_down > 0 & right_up == 0 & right_down == 0]
	}else if(gene == "right_up")
	{
		dt[left_up == 0 & left_down == 0 & right_up > 0 & right_down == 0]
	}else if(gene == "right_down")
	{
		dt[left_up == 0 & left_down == 0 & right_up == 0 & right_down > 0]
	}else if(gene == "left_up_right_down")
	{
		dt[left_up > 0 & left_down == 0 & right_up == 0 & right_down > 0]
	}else if(gene == "left_down_right_up")
	{
		dt[left_up == 0 & left_down > 0 & right_up > 0 & right_down == 0]
	}else if(gene == "left_up_down")
	{
		dt[left_up > 0 & left_down > 0 & right_up == 0 & right_down == 0]
	}else if(gene == "right_up_down")
	{
		dt[left_up == 0 & left_down == 0 & right_up > 0 & right_down > 0]
	}
}
