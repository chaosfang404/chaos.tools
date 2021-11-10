online_genome_process <- function(
							ref
){
	fread(
		paste0("https://hgdownload.soe.ucsc.edu/goldenPath/",ref,"/bigZips/",ref,".chrom.sizes")
	)[
		,.(chr = V1, length = V2)
	][
		,name := ref
	][
		str_detect(chr,"_"),group := "extra"][chr == "chrM", group := "mitochondrion"
	][
		is.na(group), group := "main",.(name,chr,length,group)
	][
		,group := factor(group,levels = c("main","mitochondrion","extra"))
	][
		order(group,chr)
	]
}

chr_size <- function(
				ref = "hg19",
				mit = FALSE,
				extra = FALSE,
				online = FALSE
){
	local_genome <- system.file(
						"extdata",
						"chr_size.info.txt",
						package = "chaos.tools"
					) %>%
					fread()

	genome_list <- local_genome[,name] %>% 
					unique()

	if (ref == "list")
	{
		genome_list
	}else
	{
		
		if(ref %in% genome_list)
		{
			if(isTRUE(online))
			{
				dt <- online_genome_process(ref = ref)
			}else
			{
				dt <- local_genome[
							name == ref
						][
							,group := factor(group,levels = c("main","mitochondrion","extra"))
						]
			}
		}else
		{
			dt <- online_genome_process(ref = ref)
		}

		if(isFALSE(mit))
		{
			dt <- dt[group != "mitochondrion"]
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
