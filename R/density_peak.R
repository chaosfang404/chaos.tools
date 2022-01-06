density_peak <- function(
					.data
){
	d <-	data.table(
				x = density(.data)$x,
				y = density(.data)$y
			)

	find_peak <- function(
					i
	){
		if(d[i,y] > d[i - 1,y])
		{
			if(d[i,y] > d[i + 1,y])
			{
				d[i][,type := "peak"]
			}
		} else if(d[i,y] < d[i - 1,y])
		{
			if(d[i,y] < d[i + 1,y])
			{
				d[i][,type := "vally"]
			}
		}
	}

	2:(nrow(d) -1) %>%
	sapply(find_peak) %>%
	rbindlist()
}
