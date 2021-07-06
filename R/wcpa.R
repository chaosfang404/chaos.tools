##' @title WCPA(Whole Chromosomal Positioning analysis)
##' @description calculation of WCPA with interaction data.
##' interaction data could be extracted with straw from .hic files.
##' The data should have the following 6 columns : 
##' sample_name, resolution, normalization_method, chr1, chr2, interaction
##' colnames is not essential.
##' @author Chao Fang

wcpa = function(.data)
{
	dt <- as.data.table(.data)
	
	names(dt) <- c("sample","resolution","normalization","chr1","chr2","interaction")

	dt[
		,
		.(chr1,chr2,interaction,sample_total = sum(interaction)/2),
		.(sample,resolution,normalization)	
	][
		,
	  	.(chr2,interaction,sample_total,chr1_total = sum(interaction)),
	  	.(sample,resolution,normalization,chr1)
	][
		,
		.(chr1,interaction,sample_total,chr1_total,chr2_total = sum(interaction)),
		.(sample,resolution,normalization,chr2)
	][
		,
		.(sample,resolution,normalization,chr1,chr2,interaction,sample_total,chr1_total,chr2_total)
	][
		order(sample,resolution,normalization,chr1,chr2)
	][
		,
		WCPA := interaction/(((chr1_total/sample_total)*(chr2_total/(sample_total - chr1_total)) + (chr2_total/sample_total)*(chr1_total/(sample_total - chr2_total))) * sample_total/2)
	][]
}
