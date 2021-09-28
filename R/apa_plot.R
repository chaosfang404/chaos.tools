
apa_plot <- function(
				.data, 
				corner_size = 6, 
				min = NA, 
				min_color = "#3c5488" , 
				max = NA, 
				max_color = "#e64b35",
				smooth = FALSE,
    			border_color = "black",
    			number_size = 5,
    			number_color = "black",
    			digit = 3
){
    cs <- corner_size

	shift <- 0.53

	dt <- data.table(.data)

	l <- nrow(dt)

	a <- l - cs + 1

	central_pixel <- paste0(
						"V",
						ceiling(l/2),
						ceiling(l/2)
					)

	all <- unlist(dt)

	p2m <- (all[central_pixel]/mean(all[names(all) != central_pixel])) %>% round(digit)
	p2ul <- (all[central_pixel]/mean(unlist(dt[1:cs,1:cs]))) %>% round(digit)
	p2ur <- (all[central_pixel]/mean(unlist(dt[1:cs,a:l]))) %>% round(digit)
	p2ll <- (all[central_pixel]/mean(unlist(dt[a:l,1:cs]))) %>% round(digit)
	p2lr <- (all[central_pixel]/mean(unlist(dt[a:l,a:l]))) %>% round(digit)

	if(is.na(min)){min <- min(all)}
	if(is.na(max)){max <- max(all)}

	p_base <- dt[,rn := factor(1:.N,levels = .N:1)] %>% 
			melt("rn") %>%
			ggplot(aes(variable, rn, fill = value))

	if(smooth)
	{
		p_heatmap <- p_base + geom_raster(interpolate = T)
	}else
	{
		p_heatmap <- p_base + geom_tile()
	}

	p_heatmap + 
	annotate(
			"rect",
			xmin= c(0,0,l - cs, l - cs) + shift,
			xmax = c(cs +1,cs + 1, l + 1, l + 1) - shift, 
			ymin = c(0, l - cs, 0, l - cs) + shift,
			ymax =c(cs + 1, l + 1, cs + 1, l + 1) - shift,
			color = border_color,
			alpha = 0
	) +
	annotate(
		"text",
		x= (cs/2) + shift, 
		y = (cs/2) + shift, 
		label = p2ll,
		size = number_size,
        color = number_color
	) + 
	annotate(
		"text",
		x= (l - cs/2) + shift, 
		y = (cs/2) + shift, 
		label = p2lr,
		size = number_size,
        color = number_color
	) + 
	annotate(
		"text",
		x= (cs/2) + shift, 
		y = (l - cs/2) + shift, 
		label = p2ul,
		size = number_size,
        color = number_color
	) + 
	annotate(
		"text",
		x= (l - cs/2) + shift, 
		y = (l - cs/2) + shift, 
		label = p2ur,
		size = number_size,
        color = number_color
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
