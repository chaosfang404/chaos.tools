wcpa_plot = function(
				.data,
				name = NA,
				res = "2500000",
				norm = "KR",
				chr = c(1:22,"X","Y"),
				axis_size = "10",
				border_color = "#FFFFFF",
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
					mid = "#FFFFFF",
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
					axis.text.x = element_text(size = axis_size),
					axis.text.y = element_text(size = axis_size),
					legend.key.widt = unit(0.4,"cm")
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


wcpa_compare_plot <- function(
						.data,
						control = "",
						observe = "",
						res = "2500000",
						norm = "KR",
						chr = c(1:22,"X","Y"),
						axis_size = "10",
						border_color = "#FFFFFF",
						scales = "fixed",
						min = NA,
						min_color = "#3c5488",
						max = NA,
						max_color = "#e64b35"
){
	dt <- wcpa(.data)[
				sample %in% c(observe,control),
				.(
					sample,
					chr1 = factor(chr1,levels = chr),
					chr2 = factor(chr2,levels = rev(chr)),
					WCPA
				)
			] %>% 
			mutate_when(sample == control,sample = "control") %>%
			mutate_when(sample == observe,sample = "observe") %>%
			wider_dt(name = "sample",value = "WCPA") %>%
			mutate_dt(overlap = observe/control) 

	if(is.na(min)){min <- min(dt$overlap)}
	if(is.na(max)){max <- max(dt$overlap)}

	dt %>%
	ggplot(aes(chr1,chr2,fill = overlap)) + 
	geom_tile(
		color = border_color,
		size = 0.2
	) + 
	scale_fill_gradient2(
		limits = c(min,max),
		low = "#3c5488",
		mid = "#FFFFFF",
		high = "#e64b35",
		midpoint = 1,
		breaks = seq(min,max,0.01)
	) + 
	labs(
		title = paste(observe,control,sep = " / "),
		x = element_blank(),
		y = element_blank()
	) + 
	theme(
		plot.title = element_text(hjust = 0.5),
		plot.background = element_blank(),
		panel.grid.major = element_blank(),
		panel.background = element_blank(),
		panel.grid.minor = element_blank(),
		axis.ticks.x = element_blank(), 
		axis.ticks.y = element_blank(),
		axis.text.x = element_text(size = axis_size),
		axis.text.y = element_text(size = axis_size),
		legend.title = element_blank(),
		legend.key.widt = unit(0.4,"cm")
	) 
}


