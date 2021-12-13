eigen <- function(
			hic_file,
			resolution = 1e6,
			norm = "KR",
			juicer_tool_path = "~/local/juicer/common/juicer_tools.jar",
			annotation = "~/Data/Reference/hg19/annotation/gencode.v38lift37.annotation.gff3.gz"
){
	chr_list <- strawr::readHicChroms(hic_file[1])$name %>% 
				.[. != "ALL"]

	res <- resolution

	eigen_func <- 	function(
						x
	){
		juicer_tool(
			cmd = "eigenvector",
			hic_file = hic_file,
			chr = x[1],
			resolution = res,
			norm = norm, 
			juicer_tool_path = juicer_tool_path
		)
	}

	eigen <- 	data.table(chr_list) %>%
				apply(1, eigen_func) %>% 
				rbindlist()

	gd <- 	gene_density(
				resolution = res,
				annotation = annotation
			)[
				chr %in% chr_list | chr %in% paste0("chr",chr_list),
				.(chr,start,end,gene_number)
			]

	a <- cbind(gd,eigen)

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
		correction > 0,
		corrected_eigen := eigen
	] %>%
	.[
		correction < 0,
		corrected_eigen := eigen * -1
	] %>%
	.[,resolution := res] %>%
	.[]
}

eigen_plot_data <- function(
						.data
){
	predict_0_all <- function(
						x
	){
		chrom <- x[1]
		res <- as.numeric(x[2])
	
		d <- 	.data[
					chr == chrom & resolution == res,
					.(y = corrected_eigen)
				][
					,x := 1:.N
				]

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

	data.table(
		chr = unique(.data$chr), 
		resolution = unique(.data$resolution)
	) %>%
	complete_dt() %>%
	apply(1,predict_0_all) %>%
	rbindlist()
}


eigen_plot <- function(
				.data
){
	dt <- eigen_plot_data(.data)	

	res <- unique(dt$resolution)
	ylim = max(abs(dt$y))+0.01

	ggplot(dt,aes(x,y)) + 
	geom_area(
		data = subset(dt, y <= 0), 
		fill = scales::hue_pal()(4)[3], 
		position = position_dodge(width = 0)
	) + 
	geom_area(
		data = subset(dt, y >= 0), 
		fill = scales::hue_pal()(4)[1],
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