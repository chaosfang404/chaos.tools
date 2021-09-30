##' @title WCPA(Whole Chromosomal Positioning analysis)
##' @description calculation of WCPA with interaction data.
##' interaction data could be extracted with straw from .hic files.
##' The data should have the following 6 columns : 
##' sample_name, resolution, normalization_method, chr1, chr2, interaction
##' colnames is not essential.
##' @author Chao Fang


wcpa = function(.data)
{
	setnames(
		data.table(.data),
		c("sample","resolution","normalization","chr1","chr2","interaction")
	)[
		chr1 != chr2,
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


wcpa_plot = function(
				.data,
				name = NA,
				res = 2500000,
				norm = "KR",
				chr = c(1:22,"X","Y"),
				axis_size = "10",
				border_color = "#FFFFFF",
				scales = "fixed",
				legend_breaks = NA,
				min_color = "#3c5488",
				mid_color = "#FFFFFF",
				max_color = "#e64b35"
){
	dt <- wcpa(.data)[
				resolution == res & normalization == norm,
				.(sample, chr1, chr2, WCPA)
			]

	if(length(name) ==1)
	{
		if(is.na(name))
		{
			name <- dt[,sample] %>% unique()
		}
	}

	if(length(legend_breaks) ==1)
	{
		if(is.na(legend_breaks))
		{
			legend_breaks <- seq(
								min(dt[,WCPA]),
								max(dt[,WCPA]),
								(max(dt[,WCPA]) - min(dt[,WCPA]))/4,
							) %>%
							round(2)
		}
	}

	p_base <- dt[sample %in% name] %>%
				complete_dt(
					sample = name,
					chr1 = chr,
					chr2 = chr,
					fill = NA
				) %>%
				.[,sample := factor(sample, levels = name)] %>%
				.[,chr1 := factor(chr1, levels = chr)] %>%
				.[,chr2 := factor(chr2, levels = rev(chr))] %>%
				ggplot(aes(chr1,chr2,fill = WCPA)) + 
				geom_tile(
					color = border_color,
					size = 0.2
				) + 
				scale_fill_gradient2(
					low = min_color,
					mid = mid_color,
					high = max_color,
					midpoint = mean(c(min(dt[,WCPA]),max(dt[,WCPA]))),
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

	if(length(name) == 1)
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
						control,
						observe,
						res = 2500000,
						norm = "KR",
						chr = c(1:22,"X","Y"),
						axis_size = 10,
						border_color = "#FFFFFF",
						scales = "fixed",
						min = NA,
						min_color = "#3c5488",
						max = NA,
						max_color = "#e64b35"
){
	dt <- wcpa(.data) %>%
			.[
				sample %in% c(observe,control) &
				normalization == norm &
				resolution == res,
				.(sample, chr1, chr2, WCPA)
			] %>%
			complete_dt(
				sample = c(observe,control),
				chr1 = chr,
				chr2 = chr,
				fill = NA
			) %>% 
			.[, chr1 := factor(chr1,levels = chr)] %>%
			.[, chr2 := factor(chr2,levels = rev(chr))] %>%
			.[sample == control, sample := "control"] %>%
			.[sample == observe, sample := "observe"] %>%
			wider_dt(name = "sample",value = "WCPA") %>%
			.[,overlap := observe/control] 

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
		#breaks = seq(min,max,0.1)
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


wcpa_matrix = function(
				.data,
				name,
				res = 2500000,
				norm = "KR",
				chr = c(1:22,"X","Y"),
				format = "data.table"
){
	wcpa(.data)[
			sample == name & 
			resolution == res &
			normalization == norm &
			chr1 %in% chr & 
			chr2 %in% chr
	][
		, 
		.(
			chr1 = factor(chr1,levels = chr),
			chr2 = factor(chr2,levels = chr),
			WCPA
		)
	] %>%
	tidyfst::wider_dt(
		chr1,
		name = "chr2",
		value = "WCPA"
	) %>%
	rename_dt(chr = chr1)
}
