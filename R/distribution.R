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

#	ref_dt <- data.table(
#						rep(reference,each = length(expand)),
#						rep(reference_name,each = length(expand)),
#						rep(expand,each = length(reference))
#					)

	ref_dt <-	data.table(
					reference,
					reference_name,
					expand
				)

	align_dt <-	data.table(
					align,
					align_name
				) 

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

	ref_rest_col <-	colnames(ref_info) %>% 
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
		tmp <- fread(x[1])
		
		n_col <- ncol(tmp)

		setnames(
			tmp,
			paste0("V",1:n_col)
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



## distribution2() is simplified distribution()
## the input pair_dt should contain group,ref,ref_file,align,align_file

distribution2 <- function(
					pair_dt,
					expand = 1e5,
					flank_slice_number = 25,
					body_size = NA,
					body_slice_number = 50,
					trim = "ceiling",
					direction_col_number = NA,
					random_times = 3,
					genome = "hg19"
){
	if(!is.na(body_size))
	{
		body_slice_number <- body_size * flank_slice_number / expand
	}

	align_dt <- pair_dt[,.(group,align,align_file)] |> unique()

	ref_dt <- pair_dt[,.(group,ref,ref_file)] |> unique()

	ref_info <-	ref_dt |>
				apply(
					1,
					function(
							x
					){
						setnames(
							fread(x[3])[,1:3],
							c("chr","start","end")
						)[
							,group := x[1]
						][
							,ref := x[2]
						][
							,domain := paste0(x[2],".",1:.N)
						] |>
						expand_slice(
							expand = expand,
							flank_slice_number = flank_slice_number,
							body_slice_number = body_slice_number,
							direction_col_number = direction_col_number,
							trim = trim
						)
					}
				) |>
				rbindlist() |>
				setkey(chr,start,end)

	align_real <-	align_dt |>
					apply(
						1,
						function(x)
						{
							setnames(
								fread(x[3])[,1:3],
								c("chr","start","end")
							)[
								,chr := chr_omit(chr)
							][	
								,group := x[1]
							][
								,align := x[2]
							][
								,peak := paste0(x[2],".",1:.N)
							][
								,exp := "real"
							]
						}
					) %>%
					rbindlist() 

	if(random_times > 0)
	{
		align_random <- lapply(
							1:random_times,
							function(x)
							{
								shuffle(
									align_real,
									ref = genome
								)[
									,exp := paste0("random_",x)
								]
							}
						) %>%
						rbindlist()

		random_col <- paste0("random_",1:random_times)
	}else
	{
		align_random <- NULL
		random_col <- NULL
	}



	align_info <-	rbind(
						align_real,
						align_random
					) |>
					setkey(chr,start,end)

	overlap <-	apply(
					pair_dt,
					1,
					function(x)
					{
						foverlaps(
							align_info[group == x[1] & align == x[4]],
							ref_info[group == x[1] & ref == x[2]],
							nomatch = NULL
						)
					}
				) %>%
				rbindlist()

	distribution <-	overlap[,.N,.(block,group,ref,align,exp)] |> 
					dcast(
						group + ref + align + block ~ exp,
						fill = 0,
						value.var = "N"
					)

	if(random_times > 0)
	{
		mean_random <- apply(distribution[,..random_col],1,mean)
	}else
	{
		mean_random <- 0
	}

	distribution[
		,mean_random := mean_random
	][
		,relative := real - mean_random
	]

	list(
		overlap = overlap,
		distribution = distribution
	)
}

distribution_plot <- function(
						.data,
						start = "start",
						end = "end",
						flank_slice_number = 25,
						body_slice_number = 50,
						expand = 1e5,
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
					start,
					end,
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


## the .data imported into location should be the overlap data from distribution function
## such as dt$overlap

location <-	function(
				.data,
				block_expand = 10,
				type = "border",
				align_id = "align_V5",
				align_group = NA,
				ref_id = "ref_V4",
				ref_group = NA,
				ref_borders = c(26,75),
				plot = "number"
){
	dt <-	.data[exp == "real"] %>%
			as.data.table() %>%
			setnames(
				old = c(align_id,ref_id),
				new = c("align_id","ref_id")
			)

	if(is.na(align_group))
	{
		align_group <- "align_group"
		dt[,align_group := "align"]
	}

	if(is.na(ref_group))
	{
		ref_group <- "ref_group"
		dt[,ref_group := "ref"]
	}

	setnames(
		dt,
		old = c(ref_group,align_group),
		new = c("ref_group","align_group")
	)

	l <- ref_borders[1]
	r <- ref_borders[2]

	calc_gene_number <-	function(
							x
	){
		b <- as.numeric(x)

		if(type == "border")
		{
			sub_blocks <-	c(
								(l - b):(l + b),
								(r - b):(r + b)
							)
		} else if(type == "body")
		{
			sub_blocks <- (l - b):(r + b)
		}

		dt[
			block %in% sub_blocks,
			.(ref_group,align_group,align_id)
		] %>%
		unique() %>%
		.[
			,.N,
			.(ref_group,align_group)
		] %>%
		.[
			,block := b
		]
	}

	count_result <-	0:block_expand %>%
					lapply(calc_gene_number) %>%
					rbindlist() %>%
					setnames("N","number") %>%
					.[
						,diff := number - c(0,number[1:(.N -1)]),
						.(ref_group,align_group)
					] %>%
					.[]

	if(plot == "none")
	{
		count_result
	}else if(plot == "number")
	{
		count_result %>%
		ggplot(aes(as.numeric(block),log2(number),color = align_group)) +
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
		scale_y_continuous(
			guide = "prism_offset"
		) +
		scale_x_continuous(
			guide = "prism_offset",
			breaks = seq(0,10,length.out = 6),
			labels = seq(0,10,length.out = 6)
		) +
		theme(legend.position = "bottom") +
		labs(
			title = "genes on TAD border region",
			x = "blocks that expand from border", 
			y = "log2(gene number on border region)"
		) +
		facet_grid(ref_group ~ .)
	} else if(plot == "diff")
	{
		count_result %>%
		ggplot(aes("x",diff,fill = as.factor(block))) +
		geom_bar(stat = "identity",position = "fill") +
		coord_polar(theta = "y",direction = -1) +
		scale_fill_manual(
			values = colorRampPalette(
						RColorBrewer::brewer.pal(9, "Set1")
					)(length(unique(count_result$block)))
		) +
		theme_void() + 
		theme(
			legend.title = element_blank(),
			legend.position = "bottom",
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank()
		) +
		facet_grid(ref_group ~ align_group)
	}
}
