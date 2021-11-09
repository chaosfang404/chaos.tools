chr_size <- function(
				ref = "hg19",
				mit = FALSE,
				extra = FALSE,
				online = FALSE
){
	all_genome <- system.file(
						"extdata",
						"chr_size.info.txt",
						package = "chaos.tools"
					) %>%
					fread()

	genome_list <- all_genome[,V1] %>% unique()

	ucsc_url <- paste0("https://hgdownload.soe.ucsc.edu/goldenPath/",ref,"/bigZips/",ref,".chrom.sizes")

	if (ref == "list")
	{
		genome_list
	}else
	{
		
		if(ref %in% genome_list)
		{
			if(isTRUE(online))
			{
				dt <- fread(ucsc_url) %>%
						setnames(c("chr","length")) %>%
						.[,name := ref] %>%
						.[str_detect(chr,"_"),group := "extra"] %>%
						.[chr == "chrM", group := "mitochondrion"] %>%
						.[is.na(group), group := "main",.(name,chr,length,group)] %>%
						.[order(chr)]
			}else
			{
				dt <- all_genome[
							V1 == ref,
							.(name = V1,chr = V2,length = V3,group = V4)
						]
			}
		}else
		{
			dt <- fread(ucsc_url) %>%
					setnames(c("chr","length")) %>%
					.[,name := ref] %>%
					.[str_detect(chr,"_"),group := "extra"] %>%
					.[chr == "chrM", group := "mitochondrion"] %>%
					.[is.na(group), group := "main",.(name,chr,length,group)] %>%
					.[order(chr)]
		}

		if(isFALSE(mit))
		{
			dt[group != "mitochondrion"]
		}
	
		if(isFALSE(extra))
		{
			dt[group != "extra",.(chr,length)]
		}else
		{
			dt[,.(chr,length)]
		}
	}
}
