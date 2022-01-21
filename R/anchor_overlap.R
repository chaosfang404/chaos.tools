reform <-	function(
				.data,
				c = "V1",
				s = "V2",
				e = "V3",
				p = "V4",
				ref = FALSE
){
	if(isFALSE(ref))
	{
		p_name <- "peak"
	}else
	{
		p_name <- "anchor"
	}

	.data %>%
	as.data.table() %>%
	setnames(
		c(c,s,e,p),
		c("chr","start","end",p_name)
	) %>%
	.[,`:=`("chr",chr_omit(chr))] %>%
	setkey(chr,start,end) %>%
	.[]
}

anchor_overlap <-	function(
						d0 = anchor,
						d1 = tss,
						d2 = tre,
						d3 = cis_element,
						d4 = chip,
						d5 = SE
){
	foverlaps(
		d0,
		rbind(d1,d2,d3,d4,d5) %>%
		setkey(chr,start,end),
		nomatch = NULL
	)[
		,.N,
		.(loop,loop_type,anchor,peak_type,description)
	][
		,description2 := fcase(
			description == "High-CTCF", "CTCF",
			description == "High-H3K27ac", "AC",
			description == "High-H3K27ac,High-CTCF", "both",
			description == "not_sig", "ns"
		)
	][
		is.na(description2),
		description2 := description
	][
		,peak_type := gsub("cis-element","CE",peak_type)
	][
		,.(loop,anchor,peak_type,description2,N)
	]
}

anchor_overlap_stat <-	function(
							.data
){

	sub <-	function(
				.data = t,
				l1 = "TRE",
				l1_status = "acquired",
				l2 = "TSS",
				l2_status = "up"
	){
		left_l1 <- paste("left",l1,l1_status,sep = "_")
		right_l1 <- paste("right",l1,l1_status,sep = "_")
		left_l2 <- paste("left",l2,l2_status,sep = "_")
		right_l2 <- paste("right",l2,l2_status,sep = "_")

		.data[
			peak_type %in% c(l1,l2)
		] %>%
		dcast(
			loop ~ anchor  + peak_type + description2,
			value.var = "N",
			fill = 0
		) %>%
		setnames(
			c(left_l1,right_l1,left_l2,right_l2),
			c("left_l1","right_l1","left_l2","right_l2")
		) %>%
		.[
			(left_l1 > 0 & right_l2 > 0) | 
			(left_l2 > 0 & right_l1 > 0),
			.(loop,left_l1,right_l1,left_l2,right_l2)
		] %>%
		.[
			,status := paste(l1,l1_status,l2,l2_status,sep = "_")
		]
	}

	.data[,.(peak_type,description2)] %>% 
	unique() %>% 
	.[,l1 := paste(peak_type,description2,sep = "_")] %>% 
	.[,l1] %>% 
	dt_comb(rep = 2,inter = "inter")%>% 
	separate_col("V1",into = c("l1","l1_status")) %>% 
	separate_col("V2",into = c("l2","l2_status")) %>% 
	.[
		(l1 != l2) &
		!((l1 == "AR" & l2 == "ERG") | (l1 == "ERG" & l2 == "AR"))
	] %>% 
	apply(
		1,
		function(x)
		{
			sub(
				.data = .data,
				l1 = x[1],
				l1_status = x[2],
				l2 = x[3],
				l2_status = x[4]
			)
		}
	) %>%
	rbindlist() %>%
	separate_col(
		"loop",
		select = 1,
		into = "loop_type",
		remove = F
	)
}

anchor_overlap_stat_plot <-	function(
								.data,
								order = "acquired",
								remove = "stable"
){
	all_loop_type <- c("acquired","strengthened","stable","weakened","lost")

	.data[,.N,.(status,loop_type)] %>%
	complete_dt(
		c("status","loop_type"),
		fill = 0
	) %>%
	.[loop_type != remove] %>%
	.[!status %in% status[str_detect(status,"AR_0h")]] %>%
	.[,p := N/sum(N),.(status)] %>%
	.[,status := factor(status,.[loop_type == order][order(p),status])] %>%
	.[,loop_type := factor(loop_type,all_loop_type[!all_loop_type %in% remove])] %>%
	ggplot(aes(status,N,fill = loop_type)) + 
	geom_bar(stat = "identity",position = "fill") + 
	theme_bw() + 
	theme(axis.text.x = element_text(angle = 45,hjust = 1)) + 
	scale_fill_npg()
}
