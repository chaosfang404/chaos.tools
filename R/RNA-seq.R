rnaseq123 <- function(
				featurecount_files,
				samples,
				groups,
				gtf_file = "~/Data/Reference/hg19/annotation/gencode.v38lift37.annotation.gtf.gz",
				fold_change_threshold = 1.5,
				special.gene = NULL
){
	if(length(samples) == 1)
	{
		if(is.na(samples))
		{
			samples <- base_name(featurecount_files)
		}
	}

	x <- readDGE(
			featurecount_files,
			columns = c(1,7),
			sep = "\t",
			skip = 1
		)

	x$samples$group <- group

	gtf_info <- gtf_file %>%
				import() %>%
				as.data.table() %>%
				.[type == "gene"] %>%
				.[,.(gene_id,gene_name,chr = seqnames,start,end,strand)]
	## it will take few minutes to load the gtf_file
	## filter the unneeded rows
	
	x$genes <- gtf_info[,.(gene_id,gene_name)] %>%
				unique()

	#get the correspondece between gene_id (Ensembl ID) and gene_name
	chrM_genes <- gtf_info[chr == "chrM",.(chr,gene_id,gene_name)] %>%
					unique()

	keep.exprs <- filterByExpr(x, group=group) 
	x <- x[keep.exprs, keep.lib.sizes=FALSE] %>%
			calcNormFactors(method = "TMM")

	design <- model.matrix(~0+group)
	colnames(design) <- gsub("group", "", colnames(design))

	contr.matrix <- makeContrasts(
						observationvscontrol = ovservation-control, 
   						levels = colnames(design)
					)

	x %>%
	voomWithQualityWeights(design, plot=FALSE) %>%
	lmFit(design) %>%
	contrasts.fit(contrasts=contr.matrix) %>%
	eBayes() %>%
	topTable(n = Inf) %>%
	.[,log10P := -log10(adj.P.Val)] %>%
	.[order(-logFC)] %>%
	setnames(
		new = "log2FC",
		old = "logFC"
	) %>%
	.[,Group := "not_sig"] %>%
	.[
		adj.P.Val < 0.05 & log2FC > log2(fold_change_threshold), 
		Group := "up"
	] %>%
	.[
		adj.P.Val < 0.05 & log2FC < -log2(fold_change_threshold), 
		Group := "down"
	]  %>%
	.[
		! gene_id %in% chrM_genes$gene_id
	]

}


volcano_plot <- function(
					.data,
					top_gene_number = 10,
					fold_change_threshold = 1.5,
					special.gene = NULL
){
	for (i in c("up","down"))
	{
		assign(
			paste0(i,".topgene"),
			.data[Group == i] %>%
			.[order(-abs(log2FC)] %>%
			head(top_gene_number) %>%
			.[,gene_name]
		)
	}

	.data[
		gene_name %in% c(up.topgene,down.topgene,special.gene),
		Label := gene_name
	][
		Group2 := Group
	][
		Label %in% special.genes, 
		Group2 := "Special"
	] %>%
	ggplot(aes(log2FC,log10P,color = Group2)) + 
	geom_point(size = 0.8) +		
	geom_hline(
		yintercept = -log10(0.05),
		linetype = "dashed"
	) +
	geom_vline(
		xintercept = c(-log2(fold_change_threshold),log2(fold_change_threshold)),
		linetype = "dashed"
	) +
	theme_base() +
	scale_color_manual(values = c("#3C5488","#BBBBBB","#00a087","#E64B35")) +
	geom_text_repel(
		label = .$Label,
		max.overlaps = Inf,
		box.padding = 1
	) +
	annotate(
		geom = "text",
		x=-5,
		y=1.1,  
		label = "p-value = 0.05"
	) +
	annotate(
		geom = "text",
		x=3.2,
		y=16,  
		label = paste0("Fold Change threshold = ",fold_change_threshold)
	) + 
	xlab(label = expression(log[2]("Fold Change"))) +
	ylab(label = expression(-log[10](adj.p-Value))) + 
	guides(color = "none")
}