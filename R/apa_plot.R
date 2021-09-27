
apa_plot <- function(
				.data, 
				corner_size = 6, 
				min, 
				min_color = "#3c5488" , 
				max, 
				max_color = "#e64b35",
				smooth = False
){
	cs <- corner_size

	shift <- 0.53

	dt <- data.table(.data)

	l <- nrow(dt)

	a <- l - cs + 1

	left_top <- dt[1:6,1:6] %>% 
					unlist %>% 
					mean %>% 
					round(3)
	left_bottom <- dt[a:l,1:3] %>% 
					unlist %>% 
					mean %>% 
					round(3)
	right_top <- dt[1:3,a:l] %>% 
					unlist %>% 
					mean %>% 
					round(3)
	right_bottom <- dt[a:l,a:l] %>% 
					unlist %>% 
					mean %>% 
					round(3)

	if(!smooth)
	{
		dt %>% 
			mutate_dt(rn = factor(1:nrow(dt),levels = nrow(dt):1)) %>% 
			longer_dt(rn) %>% 
			ggplot(aes(name,rn, fill = value)) + 
			geom_tile() + 
			annotate(
				"rect",
				xmin= c(0,0,l - cs, l - cs) + shift,
				xmax = c(cs +1,cs + 1, l + 1, l + 1) - shift, 
				ymin = c(0, l - cs, 0, l - cs) + shift,
				ymax =c(cs + 1, l + 1, cs + 1, l + 1) - shift,
				color = "black",
				alpha = 0
			) +
			annotate(
				"text",
				x= (cs/2) + shift, 
				y = (cs/2) + shift, 
				label = left_bottom,
				size = 5
			) + 
			annotate(
				"text",
				x= (l - cs/2) + shift, 
				y = (cs/2) + shift, 
				label = right_bottom,
				size = 5
			) + 
			annotate(
				"text",
				x= (cs/2) + shift, 
				y = (l - cs/2) + shift, 
				label = left_top,
				size = 5
			) + 
			annotate(
				"text",
				x= (l - cs/2) + shift, 
				y = (l - cs/2) + shift, 
				label = right_top,
				size = 5
			) +
			theme_void() + 
			scale_fill_gradient2(
				limits = c(min,max),
				low = min_color, 
				mid = "white",
				high = max_color,
				midpoint = mean(min,max)
			)
			
	}else
	{
		dt %>% 
			mutate_dt(rn = factor(1:nrow(dt),levels = nrow(dt:1)) %>% 
			melt("rn") %>% 
			ggplot(aes(name,rn, fill = value)) + 
			geom_raster(interpolate = T) + 
			annotate(
				"rect",
				xmin= c(0,0,l - cs, l - cs) + shift,
				xmax = c(cs +1,cs + 1, l + 1, l + 1) - shift, 
				ymin = c(0, l - cs, 0, l - cs) + shift,
				ymax =c(cs + 1, l + 1, cs + 1, l + 1) - shift,
				color = "black",
				alpha = 0
			) +
			annotate(
				"text",
				x= (cs/2) + shift, 
				y = (cs/2) + shift, 
				label = left_bottom,
				size = 5
			) + 
			annotate(
				"text",
				x= (l - cs/2) + shift, 
				y = (cs/2) + shift, 
				label = right_bottom,
				size = 5
			) + 
			annotate(
				"text",
				x= (cs/2) + shift, 
				y = (l - cs/2) + shift, 
				label = left_top,
				size = 5
			) + 
			annotate(
				"text",
				x= (l - cs/2) + shift, 
				y = (l - cs/2) + shift, 
				label = right_top,
				size = 5
			) +
			theme_void() + 
			scale_fill_gradient2(
				limits = c(min,max),
				low = min_color, 
				mid = "white",
				high = max_color,
				midpoint = mean(c(min,max))
			)
	}
}
