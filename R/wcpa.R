##' @title WCPA(Whole Chromosomal Positioning analysis)
##' @description calculation of WCPA with interaction data.
##' interaction data is extracted with straw from .hic files.
##' only .hic file is accepted.
##' @author Chao Fang


wcpa_pre <- function(
				hic_file,
				name = NA,
				norm = "NONE",
				chr_list = NA
){
	chr_list_dt <- chr_list_dt(
					hic_file = hic_file,
					chr_list = chr_list,
					inter = "inter"
				)

	if(is.na(name))
	{
		name <- base_name(hic_file,".hic")
	}

	lowest_res <- hic_file %>%
					strawr::readHicBpResolutions() %>% 
					.[order(-.)] %>% 
					.[1] %>% 
					as.numeric()

	count_data_func <- function(
							x
	){
		tmp <- data.table(
					strawr::straw(norm,hic_file,x[1],x[2],"BP",lowest_res)
				) %>%
				na.omit() %>%
				.[,counts] %>%
				sum()

		data.table(
			sample = name,
			normalization = norm,
			chr1 = gsub(x[1],pattern = "chr",replacement = ""),
			chr2 = gsub(x[2],pattern = "chr",replacement = ""),
			interaction = tmp
		)
	}

	apply(chr_list_dt,1,count_data_func) %>% rbindlist()
}


wcpa <- function(
			hic_file,
			name = NA,
			norm = "NONE",
			chr_list = NA
){
	dt <- 	wcpa_pre(
				hic_file = hic_file,
				name = name,
				norm = norm,
				chr_list = chr_list
			)

	chr_total <- 	sapply(
						unique(c(dt$chr1,dt$chr2)),
						function(x){dt[chr1 == x | chr2 == x,sum(interaction)]}
					) %>% 
					as.data.table(keep.rownames = "chr") %>% 
					.[,.(chr,chr_total = .)]

	merge(
		dt,
		chr_total,
		by.x = "chr1",
		by.y = "chr"
	) %>%
	setnames("chr_total","chr1_total") %>%
	merge(
		chr_total,
		by.x = "chr2",
		by.y = "chr"
	) %>%
	.[
		,.(sample,normalization,chr1,chr2,interaction,chr1_total,chr2_total = chr_total)
	] %>%
	.[
		,sample_total := sum(interaction)
	] %>%
	.[
		,
		WCPA := interaction/(((chr1_total/sample_total)*(chr2_total/(sample_total - chr1_total)) + (chr2_total/sample_total)*(chr1_total/(sample_total - chr2_total))) * sample_total/2)
	] %>%
	.[]
}

wcpa_matrix <- function(
					hic_file,
					name = NA,
					norm = "NONE",
					chr_list = NA
){
	if(length(name) == 1)
	{
		if(is.na(name))
		{
			name <- base_name(hic_file)
		}
	}

	if(length(chr_list) == 1)
	{
		if(is.na(chr_list))
		{
			chr_list <- data.table(
							strawr::readHicChroms(hic_file)
						)[
							name != "ALL",name
						] %>%
						as.character()
		}
	}

	sample_matrix <- data.table(hic = hic_file, name = name)

	dt_func <- function(
					x
	){
		wcpa(
			hic_file = x[1],
			name = x[2],
			norm = norm,
			chr_list = chr_list
		)[
			normalization == norm,
			.(sample,chr1, chr2, WCPA)
		]
	}

	dt <- apply(sample_matrix,1,dt_func) %>%
			rbindlist()
	dt2 <- rbind(
				dt,
				dt[,.(sample,chr1 = chr2,chr2 = chr1,WCPA)]
			)
	complete_dt(
		dt2,
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
				chr_list = chr_list
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
					legend.key.width = unit(0.4,"cm"),
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
	if(is.na(control_name)){control_name <- base_name(control_hic,".hic")}
	if(is.na(observe_name)){observe_name <- base_name(observe_hic,".hic")}

	control_matrix <- wcpa_matrix(
						control_hic,
						name = control_name,
						norm = norm,
						chr_list = chr_list
					)[,sample := "control"] %>%
					dcast(
						chr1 + chr2 ~ sample,
						value.var = "WCPA"
					)

	observe_matrix <- wcpa_matrix(
						observe_hic,
						name = observe_name,
						norm = norm,
						chr_list = chr_list
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