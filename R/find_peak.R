find_peak <-	function(
					.data,
					type = "density"
){
	if(type == "density")
	{
		d <-	data.table(
					x = density(.data)$x,
					y = density(.data)$y
				)
	} else if(type == "list")
	{
		d <-	data.table(
					x = 1:length(.data),
					y = .data
				)
	}

	core_fun <- function(
					i
	){
		if(d[i,y] > d[i - 1,y])
		{
			if(d[i,y] > d[i + 1,y])
			{
				d[i][,position := "peak"]
			}
		} else if(d[i,y] < d[i - 1,y])
		{
			if(d[i,y] < d[i + 1,y])
			{
				d[i][,position := "vally"]
			}
		}
	}

	2:(nrow(d) -1) %>%
	sapply(core_fun) %>%
	rbindlist()
}

## example
# ggplot() +
# geom_density(
# 	data = faithful,
# 	aes(waiting)
# ) +
# geom_point(
# 	data = find_peak(faithful$waiting),
# 	aes(x,y,color = position),
# 	size = 3
# ) +
# theme_prism() +
# theme(
# 	legend.title = element_blank(),
# 	legend.position = "none"
# ) +
# scale_y_continuous(guide = "prism_offset") +
# scale_x_continuous(guide = "prism_offset")