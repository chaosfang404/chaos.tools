chr_size <- function(
				ref = "hg19",
				mit = FALSE,
				extra = FALSE
){
	all_genome <- fread(system.file("extdata","chr_size.info.txt",package = "chaos.tools"))

	if (ref == "list")
	{
		all_genome[,V1] %>% unique()
	}else
	{
		if(isFALSE(mit))
		{
			dt <- all_genome[V1 == ref & V4 != "mitochondrion"]
		}else
		{
			dt <- all_genome[V1 == ref]
		}
	
		if(isFALSE(extra))
		{
			dt[V4 != "extra",.(chr = V2,length = V3)]
		}else
		{
			dt[,.(chr = V2,length = V3)]
		}
	}
}