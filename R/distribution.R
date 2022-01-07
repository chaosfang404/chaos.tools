chr_omit <- function(
				.data
){
	gsub(
		.data,
		pattern = "chr",
		replacement = "",
		ignore.case = T
	)
}

distribution <- function(
					reference,
					reference_name = NA,
					align,
					align_name = NA,
					expand = 10000,
					flank_slice_number = 25,
					body_size = NA,
					body_slice_number = 50,
					trim = "ceiling",
					direction_col_number = NA,
					random_times = 3,
					genome = "hg19",
					ref_keep = NA,
					align_keep = NA
){
	if(!is.na(body_size))
	{
		body_slice_number <- body_size * flank_slice_number / expand
	}

	if(length(reference_name) == 1)
	{
		if(is.na(reference_name))
		{
			reference_name <- base_name(reference)
		}
	}

	if(length(align_name) == 1)
	{
		if(is.na(align_name))
		{
			align_name <- base_name(align)
		}
	}

	ref_dt <- data.table(
						rep(reference,each = length(expand)),
						rep(reference_name,each = length(expand)),
						rep(expand,each = length(reference))
					)

	align_dt <- data.table(
						align,
						align_name) 

	ref_expand_slice <- function(
							x
	){
		expand_slice(
			fread(x[1])[,V1 := chr_omit(V1)],
			expand = as.numeric(x[3]),
			flank_slice_number = flank_slice_number,
			body_slice_number = body_slice_number,
			direction_col_number = direction_col_number,
			trim = trim
		)[
			,reference := x[2]
		][
			,expand := as.numeric(x[3])
		]
	}

	ref_info <- ref_dt %>%
				apply(1,ref_expand_slice) %>%
				rbindlist()

	ref_essential_col <- c("chr","start","end","block","reference","expand")

	ref_rest_col <- colnames(ref_info) %>% 
							.[! . %in% ref_essential_col]

	ref_rest_col_new <- paste0("ref_",ref_rest_col)

	ref_info %>%
	setnames(
		old = ref_rest_col,
		new = ref_rest_col_new
	) %>%
	setkey(chr,start,end)


	read_align <- function(x)
	{
		fread(
			x[1]
		)[
			,V1 := chr_omit(V1)
		][
			,align := x[2]
		]
	}
	
	align_real <- align_dt %>%
				apply(1,read_align) %>%
				rbindlist() %>%
				.[,exp := "real"]

	if(random_times > 0)
	{
		align_random_fun <- function(
								x
		){
			shuffle(
				align_real,
				ref = genome
			)[
				,exp := paste0("random_",x[1])
			]
		}

		align_random <- data.table(
							1:random_times
						) %>%
						apply(1,align_random_fun) %>%
						rbindlist()
		random_col <- paste0("random_",1:random_times)
	}else
	{
		align_random <- NULL
		random_col <- NULL
	}



	align_info <- rbind(
					align_real,
					align_random
				) %>%
				setnames(
					old = c("V1","V2","V3"),
					new = c("chr","start","end")
				)

	align_essential_col <- c("chr","start","end","align","exp")

	align_rest_col <- colnames(align_info) %>% 
							.[! . %in% align_essential_col]

	if(length(align_rest_col) > 0)
	{
		setnames(
			align_info,
			old = align_rest_col,
			new = paste0("align_",align_rest_col)
		)
	}

	setkey(align_info,chr,start,end)

	if(length(ref_keep) == 1)
	{
		if(!is.na(ref_keep))
		{
			ref_keep_col <- paste0("ref_V",ref_keep)
		}else
		{
			ref_keep_col <- NULL
		}
	}else
	{
		ref_keep_col <- paste0("ref_V",ref_keep)
	}

	if(length(align_keep) == 1)
	{
		if(!is.na(align_keep))
		{
			align_keep_col <- paste0("align_V",align_keep)			
		}else
		{
			align_keep_col <- NULL
		}
	}else
	{
		align_keep_col <- paste0("align_V",align_keep)
	}

	overlap <-	foverlaps(
					align_info,
					ref_info,
					nomatch = NULL
				)

	left_col <- c("block","reference","expand",ref_keep_col,"align","exp",align_keep_col)

	distribution <- overlap[,.N, left_col] %>%
					wider_dt(name = "exp", value = "N", fill = 0)

	if(random_times > 0)
	{
		mean_random <- apply(distribution[,..random_col],1,mean)
	}else
	{
		mean_random <- 0
	}

	distribution[,relative := real - mean_random]

	matrix <- overlap[,.N,c(left_col,"ref_V4")] %>% 
				wider_dt(name = "exp", value = "N",fill = 0)

	list(
		overlap = overlap,
		distribution = distribution,
		matrix = matrix
	)
}


distribution_plot <- function(
						.data,
						start = "start",
						end = "end",
						flank_slice_number = 25,
						body_slice_number = 50,
						expand = 10000,
						lab_x = "",
						lab_y = "relative Peak count",
						legend_position = c(0.9,0.9)
){
	.data %>%
	ggplot() +
	theme_bw() +
	scale_color_npg() + 
	scale_x_continuous(
		breaks = c(
					1,
					flank_slice_number + 1, 
					flank_slice_number + body_slice_number, 
					flank_slice_number * 2 + body_slice_number
		),
		labels = c(
					paste0(expand/-1000, "k"),
					"start",
					"end",
					paste0(expand/1000, "k")
		),
		expand = c(0, 0)
	) + 
	labs(
		x = lab_x,
		y = lab_y
	) +
	theme(
		legend.title = element_blank(),
		legend.background = element_blank(),
		legend.key = element_blank(),
		legend.position = legend_position
	)
}


## .data should be the overlap data from distribution function
## such as dt$overlap

gene_on_boarder <-	function(
						.data,
						block_expand = 10,
						borders = c(26,75),
						gene_ID_column = "align_V4",
						regulation_column = "align_V6",
						plot = TRUE
){
	dt <-	.data %>%
			as.data.table() %>%
			setnames(
				old = c(regulation_column,gene_ID_column),
				new = c("regulation","gene_ID")
			)

	calc_gene_number <-	function(
							x
	){
		t <- as.numeric(x[1])
	
		n <- dt[
				block %in% c((borders[1] - t):(borders[1] + t),(borders[2] - t):(borders[2] + t)) &
				regulation == x[2],
				gene_ID
			] %>%
			unique() %>%
			length()
	
		data.table(block = x[1],regulation = x[2],number = n)
	}


	tmp <- expand.grid(
				n = (0:block_expand),
				type = unique(dt$regulation),
				stringsAsFactors = F
			) %>%
			as.data.table() %>%
			apply(1,calc_gene_number) %>%
			rbindlist()

	if(isTRUE(plot))
	{
		tmp %>%
		ggplot(aes(as.numeric(block),log2(number),color = regulation)) +
		geom_line() +
		theme_prism() +
		scale_color_npg() +
		geom_point() +
		geom_text(
			aes(label = number),
			hjust= -0.2,
			vjust = 1,
			show.legend = F
		) +
		scale_y_continuous(guide = "prism_offset") +
		scale_x_continuous(guide = "prism_offset") +
		theme(legend.position = "bottom") +
		labs(
			title = "genes on TAD border region",
			x = "blocks that expand from border", 
			y = "log2(gene number on border region)"
		)
	} else
	{
		tmp %>%
		dcast(
			regulation ~ block,
			value.var = "number"
		)
	}
}
