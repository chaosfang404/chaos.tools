eigen_correction <- function(
						hic_file,
						resolution = 1e6,
						norm = "KR",
						juicer_tool_path = "~/local/juicer/common/juicer_tools.jar",
						annotation = "~/Data/Reference/hg19/annotation/gencode.v38lift37.annotation.gff3.gz"
){
	chr_list <- strawr::readHicChroms(hic_file)$name %>% 
				.[. != "ALL"]

	eigen_func <- function(
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

	eigen <- 	data.table(chr_list) %>%
				apply(1, eigen_func) %>% 
				rbindlist()

	gd <- 	gene_density(
				resolution = resolution,
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
	.[]
}