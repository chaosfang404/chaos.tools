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

	keep.exprs <- filterByExpr(x, group = group) 
	x <- x[keep.exprs, keep.lib.sizes = FALSE] %>%
			calcNormFactors(method = "TMM")

	design <- model.matrix(~0+group)
	colnames(design) <- gsub("group", "", colnames(design))

	contr.matrix <- makeContrasts(
						obsvsctl = obs-ctl, 
   						levels = colnames(design)
					)

	x %>%
	voomWithQualityWeights(design, plot = F) %>%
	lmFit(design) %>%
	contrasts.fit(contrasts = contr.matrix) %>%
	eBayes() %>%
	topTable(n = Inf) %>%
	as.data.table() %>%
	.[,log10P := -log10(adj.P.Val)] %>%
	.[order(-logFC)] %>%
	setnames(
		new = "log2FC",
		old = "logFC"
	) %>%
	.[,Group := "not_sig"] %>%
	.[
		adj.P.Val < p_value & log2FC > log2(fold_change), 
		Group := "up"
	] %>%
	.[
		adj.P.Val < p_value & log2FC < -log2(fold_change), 
		Group := "down"
	]  %>%
	.[
		! gene_id %in% chrM_genes$gene_id
	]

}


volcano_plot <- function(
					.data,
					top_gene_number = 10,
					fold_change = 1.5,
					p_value = 0.05,
					special.gene = NULL
){
	dt <- as.data.table(.data)

	for (i in c("up","down"))
	{
		assign(
			paste0(i,".topgene"),
			dt[
				Group == i
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
		,Group2 := Group
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
					x=-5,
					y=1.1,  
					label = paste0("p-value = ",p_value)
				) +
				annotate(
					geom = "text",
					x=3.2,
					y=16,  
					label = paste0("Fold Change threshold = ",fold_change)
				) + 
				xlab(label = expression(log[2]("Fold Change"))) +
				ylab(label = expression(-log[10](adj.p-Value))) + 
				guides(color = "none") + 
				theme(plot.margin = unit(rep(1,4),'lines'))

	if(!is.null(special.gene))
	{
		p  <- p_base + 
				scale_color_manual(values = c("#3C5488","#BBBBBB","#00a087","#E64B35")) 
	}else
	{
		p <- p_base + 
				scale_color_manual(values = c("#3C5488","#BBBBBB","#E64B35"))
	}

	p
}