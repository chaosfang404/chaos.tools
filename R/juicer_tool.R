juicer_tool <- function(
					cmd = "eigenvector",
					hic_file,
					chr = NA,
					resolution = 1e6,
					norm = "KR",
					juicer_tool_path = "~/local/juicer_chaos/common/feature_tools.jar",
					matrix_type = 2
){
	chr <-	chr_list(
				hic_file = hic_file,
				chr_list = chr
			)

	if(cmd == "eigenvector")
	{
		paste(
			"java -jar",juicer_tool_path, cmd, "-p",toupper(norm), hic_file, chr,"BP",resolution,sep = " "
		) %>%
		system(inter = T) %>%
		.[!grepl("warn",.,ignore.case = T)] %>%
		as.numeric() %>%
		as.data.table() %>%
		setnafill(fill = 0) %>%
		setnames("eigen") %>%
		.[]

	}else if(cmd == "pearsons")
	{
		matrix <-	paste(
						"java -jar",juicer_tool_path, cmd, "-p",toupper(norm), hic_file, chr,"BP",resolution,sep = " "
					) %>%
					system(inter = T) %>%
					.[!grepl("warn",.,ignore.case = T)] %>%
					.[!grepl("reading",.,ignore.case = T)] %>%
					strsplit(" ") %>% 
					sapply(as.numeric) %>%
					as.data.table() %>%
					setnafill(fill = 0) %>%
					setnames(paste0("bin_",1:ncol(.)))

		if(matrix_type == 1)
		{
			matrix
		}else if(matrix_type == 2)
		{
			x <- matrix |> colnames() |> factor()
			matrix[,x := x] %>%
			melt(
				"x", 
				variable.name = "y", 
				value.name = "pearsons"
			) %>%
			.[,x := factor(x,levels = levels(y))] %>%
			.[]
		}
	}else if(cmd == "validate")
	{
		paste(
			"java -jar",juicer_tool_path, cmd, hic_file,sep = " "
		) %>%
		system(inter = F)
	}
}


pearsons_plot <- function(
					hic_file,
					resolution = 25e4,
					chr,
					min = -0.5,
					max = 0.5,
					min_color = "#00004a",
					max_color = "#e64b35",
					smooth = TRUE
){
	p_base <-	juicer_tool(
					cmd = "pearsons",
					hic_file = hic_file,
					resolution = resolution,
					chr = chr,
					matrix_type = 2
				)[
					pearsons >= max, pearsons := max
				][
					pearsons <= min, pearsons := min
				] %>%
				ggplot(
					aes(x,y,fill = pearsons)
				) + 
				scale_fill_gradient2(
					limits = c(min,max),
					low = min_color,
					high = max_color
				) + 
				theme(
					axis.text = element_blank(),
					axis.title = element_blank(),
					axis.ticks = element_blank(),
					legend.position = "none"
				)
	if(isFALSE(smooth))
	{
		p_base + geom_tile()
	}else
	{
		p_base + geom_raster()
	}	
}

juicer_eigen <- function(
					hic_file,
					resolution = 1e6,
					chr_list = NA,
					norm = "KR",
					ref = "hg19",
					juicer_tool_path = "~/local/juicer_chaos/common/feature_tools.jar",
					annotation = "~/Data/Reference/hg19/annotation/gencode.v38lift37.annotation.gff3.gz"
){
	chr_list <- chr_list(
					hic_file = hic_file,
					chr_list = chr_list
				)
	
	eigen_func <- 	function(
						x
	){
		juicer_tool(
			cmd = "eigenvector",
			hic_file = hic_file,
			chr = x[1],
			resolution = resolution,
			norm = norm, 
			juicer_tool_path = juicer_tool_path
		)
	}

	eigen <- 	chr_list |>
				lapply(eigen_func) |>
				rbindlist()

	gd <- 	gene_density(
				ref = ref,
				resolution = resolution,
				annotation = annotation
			)[
				chr %in% c(chr_list, paste0("chr",chr_list)),
				.(chr,start,end,gene_number)
			]

	a <- cbind(eigen,gd)

	pos_bin <- a[eigen > 0,.N,.(chr)][,.(chr,pos_bin = N)] %>% setkey()
	neg_bin <- a[eigen < 0,.N,.(chr)][,.(chr,neg_bin = N)] %>% setkey()
	pos_gene_number <- a[eigen > 0, sum(gene_number),.(chr)][,.(chr,pos_gene_number = V1)] %>% setkey()
	neg_gene_number <- a[eigen < 0, sum(gene_number),.(chr)][,.(chr,neg_gene_number = V1)] %>% setkey()

	merge(pos_bin,neg_bin) %>%
	merge(pos_gene_number) %>%
	merge(neg_gene_number) %>%
	.[
		,correction := pos_gene_number/pos_bin - neg_gene_number/neg_bin
	] %>%
	merge(a) %>%
	.[
		,corrected_eigen := fifelse(correction > 0, eigen, eigen * -1)
	] %>%
	.[,resolution := resolution] %>%
	.[]
}

juicer_eigen_plot_data <- function(
							.data,
							correction = TRUE
){
	predict_0_all <- function(
						x
	){
		chrom <- x[1]
		res <- as.numeric(x[2])
	
		if(isTRUE(correction))
		{
			d <- .data[
					chr == chrom & resolution == res,
					.(y = corrected_eigen)
				][
					,x := 1:.N
				]
		}else
		{
			d <- .data[
					chr == chrom & resolution == res,
					.(y = eigen)
				][
					,x := 1:.N
				]
		}

		predict_0 <- function(
						i
		){
			f <- lm(x ~ y, d[i:(i+1),])
			if (f$qr$rank < 2) return(NULL)
			r <- predict(f, newdata = data.table(y = 0))
			if(d[i,]$x < r & r < d[i+1,]$x)
			return(data.table(x = r,y = 0))
			else return(NULL)
		}
		
		rbind(
			d,
			sapply(1:(nrow(d)-1),predict_0) %>% rbindlist()
		) %>% 
		.[,chr := chrom] %>%
		.[,resolution := res]
	}

	chr_list <- unique(.data$chr)

	data.table(
		chr = chr_list, 
		resolution = unique(.data$resolution)
	) %>%
	complete_dt() %>%
	apply(1,predict_0_all) %>%
	rbindlist() %>%
	.[,chr := factor(chr,stringr::str_sort(chr_list,numeric = T))]
}


juicer_eigen_plot <- function(
						.data,
						correction = TRUE,
						up_color = "#e64b35",
						down_color = "#4dbbd5"
){
	dt <- juicer_eigen_plot_data(
				.data,
				correction = correction
			)

	res <- unique(dt$resolution)
	ylim = max(abs(dt$y)) + 0.01

	ggplot(dt,aes(x,y)) + 
	geom_area(
		data = subset(dt, y <= 0), 
#		fill = scales::hue_pal()(4)[3],
		fill = down_color,
		position = position_dodge(width = 0)
	) + 
	geom_area(
		data = subset(dt, y >= 0), 
#		fill = scales::hue_pal()(4)[1],
		fill = up_color,
		position = position_dodge(width = 0)
	) + 
	labs(
		x = "", 
		y = "Eigenvector"
	) + 
	theme(
		panel.background = element_rect(fill = "transparent", color = "NA"),
		panel.grid = element_blank()
	) + 
	theme(plot.title = element_text(size = 10, hjust = 0.5)) + 
	theme(axis.line = element_line(colour ="black")) +
	ylim(ylim*(-1),ylim) + 
	theme(axis.title.y = element_text(size = 5)) +
#	scale_x_continuous(
#		breaks = c(1+len_bin/5*(0:5)),
#		labels = c(paste0(start+len/5*(0:5),"Mb"))
#	) +
	facet_grid(chr ~ resolution,scales = "free")
}

eigen_switch <- function(
					ctl = "DMSO_EtOH.hic",
					obs = "DMSO_DHT.hic",
					resolution = 25e4,
					correction = TRUE
){
	e <-	data.table(c("ctl","obs"),c(ctl,obs)) %>%
			apply(
				1,
				function(x){
					dt <- juicer_eigen(
						x[2],
						resolution = resolution
					)[
						,No := 1:.N,
						.(chr)
					]

					if(isTRUE(correction))
					{
						dt[,.(chr,No,corrected_eigen)] %>%
						setnames(
							old = "corrected_eigen",
							new = x[1]
						) %>%
						setkey()
					}else
					{
						dt[,.(chr,No,eigen)] %>%
						setnames(
							old = "eigen",
							new = x[1]
						) %>%
						setkey()
					}
				}
			)

	m <- merge(
			e[[1]],
			e[[2]]
		)[
			ctl > 0 & obs > 0, status := "conserved_A"
		][
			ctl < 0 & obs < 0, status := "conserved_B"
		][
			ctl > 0 & obs < 0, status := "A_to_B"
		][
			ctl < 0 & obs > 0, status := "B_to_A"
		]

	c <-	chr_size() %>%
			apply(
				1,
				function(x){
					seq_dt(
						0,
						as.numeric(x[2]),
						slice_size = resolution
					)[
						,chr := x[1]
					][
						,No := 1:.N,
						.(chr)
					][
						,V1 := V1 + 1
					] %>%
					setnames(
						old = c("V1","V2"),
						new = c("start","end")
					)
				}
			) %>%
			rbindlist() %>%
			setkey()

	merge(m, c)[,.(chr,start,end,No,ctl,obs,status)]
}
