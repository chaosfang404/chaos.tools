wcpa_pre <- function(
				hic_file,
				name = NA,
				norm = "NONE",
				chr_list = c(1:22,"X","Y"),
				resolution = 2.5e6
){
	chr_list <- as.character(chr_list)

	if(is.na(name))
	{
		name <- basename(hic_file,".hic")
	}

	res_label <- resolution %>%
					format(
						scientific = F,
						trim = T
					)

	count_data <- data.table(NULL)
	for(chr1 in chr_list)
	{
		for(chr2 in chr_list)
		{
			if(chr1 != chr2)
			{
				tmp <- data.table(
							strawr::straw(norm,hic_file,chr1,chr2,"BP",resolution)
						) %>%
						na.omit() %>%
						.[,counts] %>%
						sum()
				count_data <- rbind(
									count_data,
									data.table(
										sample = name,
										resolution = resolution,
										normalization = norm,
										chr1 = str_replace(chr1,"chr",""),
										chr2 = str_replace(chr2,"chr",""),
										interaction = tmp
									)
								)
			}
		}
	}
	count_data
}


wcpa <- function(
			hic_file,
			name = NA,
			norm = "NONE",
			chr_list = c(1:22,"X","Y"),
			resolution = 2.5e6
){
	wcpa_pre(
		hic_file = hic_file,
		name = name,
		norm = norm,
		chr_list = chr_list,
		resolution = resolution
	)[
		chr1 != chr2
	][
		,
		.(chr1,chr2,interaction,sample_total = sum(interaction)/2),
		.(sample,resolution,normalization)
	][
		,
		.(chr2,interaction,sample_total,chr1_total = sum(interaction)),
		.(sample,resolution,normalization,chr1)
	][
		,
		.(chr1,interaction,sample_total,chr1_total,chr2_total = sum(interaction)),
		.(sample,resolution,normalization,chr2)
	][
		,
		.(sample,resolution,normalization,chr1,chr2,interaction,sample_total,chr1_total,chr2_total)
	][
		order(sample,resolution,normalization,chr1,chr2)
	][
		,
		WCPA := interaction/(((chr1_total/sample_total)*(chr2_total/(sample_total - chr1_total)) + (chr2_total/sample_total)*(chr1_total/(sample_total - chr2_total))) * sample_total/2)
	][]
}

wcpa_matrix <- function(
					hic_file,
					name = NA,
					norm = "NONE",
					chr_list = c(1:22,"X","Y"),
					resolution = 2.5e6
){
	sample_matrix <- data.table(hic = hic_file, name = name)

	dt <- data.table(NULL)
	for (i in 1:nrow(sample_matrix))
	{
		dt1 <- wcpa(
					sample_matrix[i,hic],
					name = sample_matrix[i,name],
					norm = norm,
					chr_list = chr_list,
					resolution = resolution
				)[
					resolution == resolution & normalization == norm,
					.(sample,chr1, chr2, WCPA)
				]
		dt <- rbind(dt,dt1)
	}

	complete_dt(
		dt,
		sample = name,
		chr1 = chr_list,
		chr2 = chr_list,
		fill = NA
	)[
		,chr1 := factor(chr1, levels = chr_list)
	][
		,chr2 := factor(chr2, levels = rev(chr_list))
	][]
}


wcpa_plot <- function(
				hic_file,
				name = NA,
				resolution = 2.5e6,
				norm = "NONE",
				chr_list = c(1:22,"X","Y"),
				axis_size = "10",
				border_color = "#FFFFFF",
				scales = "fixed",
				legend_breaks = NA,
				min_color = "#3c5488",
				mid_color = "#FFFFFF",
				max_color = "#e64b35"
){

	dt <- wcpa_matrix(
				hic_file,
				name = name,
				norm = norm,
				chr_list = chr_list,
				resolution = resolution
			)

	if(length(legend_breaks) == 1)
	{
		if(is.na(legend_breaks))
		{
			breaks_min <- dt[!is.na(WCPA),WCPA] %>% min()
			breaks_max <- dt[!is.na(WCPA),WCPA] %>% max()

			legend_breaks <- seq(
								breaks_min,
								breaks_max,
								(breaks_max - breaks_min)/4
							) %>%
							round(2)
		}
	}

	p_base <- dt %>%
				ggplot(aes(chr1,chr2,fill = WCPA)) + 
				geom_tile(
					color = border_color,
					size = 0.2
				) + 
				scale_fill_gradient2(
					low = min_color,
					mid = mid_color,
					high = max_color,
					midpoint = mean(c(min(legend_breaks),max(legend_breaks))),
					breaks = legend_breaks
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
					legend.key.widt = unit(0.4,"cm"),
					legend.title = element_blank()
				)
	if(length(unique(dt$sample)) > 1)
	{
		p_base + facet_wrap(~sample)
	}else
	{
		p_base
	}
}



wcpa_compare_plot <- function(
						control_hic,
						observe_hic,
						control_name = NA,
						observe_name = NA,
						resolution = 2.5e6,
						norm = "NONE",
						chr_list = c(1:22,"X","Y"),
						axis_size = 10,
						border_color = "#FFFFFF",
						scales = "fixed",
						min = NA,
						min_color = "#3c5488",
						max = NA,
						max_color = "#e64b35"
){
	if(is.na(control_name)){control_name <- basename(control_hic,".hic")}
	if(is.na(observe_name)){observe_name <- basename(observe_hic,".hic")}

	control_matrix <- wcpa_matrix(
						control_hic,
						name = control_name,
						norm = norm,
						chr_list = chr_list,
						resolution = resolution
					)[,sample := "control"] %>%
					dcast(
						chr1 + chr2 ~ sample,
						value.var = "WCPA"
					)

	observe_matrix <- wcpa_matrix(
						observe_hic,
						name = observe_name,
						norm = norm,
						chr_list = chr_list,
						resolution = resolution
					)[,sample := "observe"] %>%
					dcast(
						chr1 + chr2 ~ sample,
						value.var = "WCPA"
					)

	dt <- full_join_dt(
				control_matrix,
				observe_matrix,
				by = c("chr1","chr2")
			) %>%
			mutate_dt(WCPA = observe/control) %>%
			select_dt(chr1,chr2,WCPA)

	if(is.na(min)){min <- min(dt$WCPA)}
	if(is.na(max)){max <- max(dt$WCPA)}

	dt %>%
	ggplot(aes(chr1,chr2,fill = WCPA)) + 
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
		breaks = seq(min,max,0.1)
	) + 
	labs(
		title = paste(observe_name,control_name,sep = " / "),
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