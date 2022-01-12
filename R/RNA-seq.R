rnaseq123 <- function(
				ctl_featurecount_file,
				obs_featurecount_file,
				gtf_file = "~/Data/Reference/hg19/annotation/gencode.v38lift37.annotation.gtf.gz",
				fold_change = 1.5,
				p_value = 0.05
){
	x <- readDGE(
			c(ctl_featurecount_file,obs_featurecount_file),
			columns = c(1,7),
			sep = "\t",
			skip = 1
		)

	group <- c(
				rep("ctl",length(ctl_featurecount_file)),
				rep("obs",length(obs_featurecount_file))
			)

	x$samples$group <- group

	gtf_info <- gtf_file %>%
				import() %>%
				as.data.table() %>%
				.[
					type == "gene",
					.(gene_id,gene_name,chr = seqnames,start,end,strand)
				]
	## it will take few minutes to load the gtf_file
	## filter the unneeded rows
	
	x$genes <- gtf_info[,.(gene_id,gene_name)] %>%
				unique()

	#get the correspondece between gene_id (Ensembl ID) and gene_name
	chrM_genes <- gtf_info[
						chr == "chrM",
						.(chr,gene_id,gene_name)
					] %>%
					unique()

	x <-	x[
				filterByExpr(x, group = group), 
				keep.lib.sizes = F
			] %>%
			calcNormFactors(method = "TMM")

	design <- model.matrix(~0+group)
	colnames(design) <- gsub("group", "", colnames(design))

	contr.matrix <- makeContrasts(
						obsvsctl = obs-ctl, 
						levels = colnames(design)
					)

	result <- NULL

	result$DE <-	x %>%
					voomWithQualityWeights(design, plot = F) %>%
					lmFit(design) %>%
					contrasts.fit(contrasts = contr.matrix) %>%
					eBayes() %>%
					topTable(n = Inf) %>%
					as.data.table() %>%
					.[order(-logFC)] %>%
					.[
						,`:=`(
							log10P = -log10(adj.P.Val),
							regulation = fcase(
								adj.P.Val < p_value & logFC > log2(fold_change), "up",
								adj.P.Val < p_value & logFC < -log2(fold_change), "down",
								default = "not_sig"
							)	
						)
					] %>%
					.[! gene_id %in% chrM_genes$gene_id] %>%
					setnames(
						old = "logFC",
						new = "log2FC"	
					) %>%
					setkey(gene_id,gene_name)

	result$gene <-	gtf_info %>%
					setkey(
						gene_id,
						gene_name
					) %>%
					.[
						result$DE
					] %>%
					.[
						,.(gene_id,gene_name,chr,start,end,strand,regulation)
					]

	result$tss <-	result$gene[
						,TSS_start := fifelse(strand == "+",start,end - 1)
					][
						,TSS_end := TSS_start + 1
					][
						,.(gene_id,gene_name,chr,start = TSS_start,end = TSS_end,strand,regulation)
					]
	result
}


volcano_plot <- function(
					.data,
					top_gene_number = 10,
					fold_change = 1.5,
					p_value = 0.05,
					special.gene = NULL
){
	dt <-	.data %>%
			as.data.table() %>%
			.[
				,regulation := fcase(
					adj.P.Val < p_value & log2FC > log2(fold_change), "up",
					adj.P.Val < p_value & log2FC < -log2(fold_change), "down",
					default = "not_sig"	
				)
			]

	for (i in c("up","down"))
	{
		assign(
			paste0(i,".topgene"),
			dt[
				regulation == i
			][
				order(-abs(log2FC))
			] %>%
			head(top_gene_number) %>%
			.[,gene_name]
		)
	}

	dt[
		gene_name %in% c(up.topgene,down.topgene,special.gene),
		Label := gene_name
	][
		,Group2 := regulation
	]

	if(!is.null(special.gene))
	{
		dt[
			Label %in% special.gene, 
			Group2 := "Special"
		]
	}
	
	p_base <- dt %>%
				ggplot(aes(log2FC,log10P,color = Group2)) + 
				geom_point(size = 0.8) +		
				geom_hline(
					yintercept = -log10(0.05),
					linetype = "dashed"
				) +
				geom_vline(
					xintercept = c(-log2(fold_change),log2(fold_change)),
					linetype = "dashed"
				) +
				theme_base() +
				geom_text_repel(
					label = dt$Label,
					max.overlaps = Inf,
					box.padding = 1
				) +
				annotate(
					geom = "text",
					x=min(dt$log2FC),
					y=-log10(0.05),
					hjust = 0,
					vjust = 1,
					label = paste0("p-value = ",p_value)
				) +
				annotate(
					geom = "text",
					x=log2(fold_change) + 0.1,
					y=max(dt$log10P) + 1,
					hjust = 0,
					label = paste0("Fold Change threshold = ",fold_change)
				) + 
				xlab(label = expression(log[2]("Fold Change"))) +
				ylab(label = expression(-log[10](adj.p-Value))) + 
				guides(color = "none") + 
				theme(plot.margin = unit(rep(1,4),'lines'))

	if(!is.null(special.gene))
	{
		p_base + 
		scale_color_manual(values = c("#3C5488","#BBBBBB","#00a087","#E64B35")) 
	}else
	{
		p_base + 
		scale_color_manual(values = c("#3C5488","#BBBBBB","#E64B35"))
	}
}


sub_volcano_plot <-	function(
						id = NA,
						name = NA,
						fold_change = NA,
						p_value = NA,
						ref_data = rna_seq_result$DE,
						plot = TRUE,
						label_position = c(-4,12,4,12)
){
	total_stat <-	sapply(
						c("up","down"),
						function(x){
							ref_data[,.N,.(regulation)][regulation == x,N]
						}
					)

	if(is.na(fold_change))
	{
		dt <- ref_data
	}else
	{
		dt <- ref_data[log2FC > log2(fold_change) | log2FC < -log2(fold_change)]
	}

	if(!is.na(p_value))
	{
		dt <- dt[adj.P.Val < p_value]
	}


	if(length(id) == 1)
	{
		if(is.na(id))
		{
			id <- ref_data$gene_id
		}
	}

	if(length(name) == 1)
	{
		if(is.na(name))
		{
			name <- ref_data$gene_name
		}
	}

	dt <-	dt[gene_id %in% id & gene_name %in% name]

	dt_stat <-	sapply(
					c("up","down"),
					function(x){
						dt[,.N,.(regulation)][regulation == x,N]
					}
				)

	label_up	<-	paste0("N = ",dt_stat[1]," / ",total_stat[1],"\n",round(dt_stat[1]/total_stat[1] * 100,2),"%")
	label_down	<-	paste0("N = ",dt_stat[2]," / ",total_stat[2],"\n",round(dt_stat[2]/total_stat[2] * 100,2),"%")

	if(isTRUE(plot))
	{
		volcano_plot(
			dt,
			top_gene_number = 0
		) + 
		theme(plot.title = element_text(hjust = 0.5)) +
		annotate("text",x = label_position[1], y = label_position[2], label = label_down) + 
		annotate("text",x = label_position[3], y = label_position[4], label = label_up)
	} else
	{
		dt
	}
}
