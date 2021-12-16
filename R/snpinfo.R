snpinfo <- function(
				rs
) {

	mart.snp <- biomaRt::useMart(
					"https://grch37.ensembl.org", 
					biomart="ENSEMBL_MART_SNP",
					dataset="hsapiens_snp"
				)
	biomaRt::getBM(
		attributes = c( "refsnp_id","chr_name","chrom_start","chrom_end","allele"),
		filters = "snp_filter",
		values = rs, 
		mart = mart.snp
	) %>%
	as.data.table()
}