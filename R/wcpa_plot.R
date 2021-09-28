wcpa_plot = function(
				.data,
				name = NA,
				res = "2500000",
				norm = "KR",
				chr = c(1:22,"X","Y"),
				font_size = "10",
				border_color = "white",
				scales = "fixed"
){
	dt <- wcpa(.data)[
				,chr1 := factor(chr1,levels = chr)
			][
				,chr2 := factor(chr2,levels = rev(chr))
			][
				resolution == res &
				normalization == norm
			]

	if(length(name) > 1)
	{
		dt <- dt[
				sample %in% name
			][
				,sample := factor(sample,levels = name)
			]
	} else if(length(name) == 1)
	{
		if(!is.na(name))
		{
			dt <- dt[sample == name]
		}
	}

	p_base <- dt %>%
				ggplot(aes(chr1,chr2,fill = WCPA)) + 
				geom_tile(
					color = border_color,
					size = 0.2
				) + 
				scale_fill_gradient2(
					low = "#3c5488",
					mid = "white",
					high = "#e64b35",
					midpoint = mean(c(min(dt$WCPA),max(dt$WCPA)))
				) + 
				labs(
					x = element_blank(),
					y = element_blank()
				) + 
				theme(
					plot.background = element_blank(),
					panel.grid.major = element_blank(),
					panel.background = element_blank(),
					panel.grid.minor = element_blank(),
					axis.ticks.x = element_blank(), 
					axis.ticks.y = element_blank(),
					axis.text.x = element_text(size = font_size),
					axis.text.y = element_text(size = font_size)
				)

	if(length(unique(dt$sample)) == 1)
	{
		p_base

	}else
	{
		p_base + 
		facet_wrap(
			~sample,
			ncol = 2,
			scales = scales
		)
	}

}