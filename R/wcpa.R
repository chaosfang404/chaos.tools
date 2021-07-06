wcpa = function(.data)
{
	require(data.table)

	dt <- data.table(.data)

	names(dt) <- c("sample","resolution","normalization","chr1","chr2","interaction")

	dt[
		,
		list(chr1,chr2,interaction,sample_total = sum(interaction)/2),
		list(sample,resolution,normalization)	
	][
		,
	  	list(chr2,interaction,sample_total,chr1_total = sum(interaction)),
	  	list(sample,resolution,normalization,chr1)
	][
		,
		list(chr1,interaction,sample_total,chr1_total,chr2_total = sum(interaction)),
		list(sample,resolution,normalization,chr2)
	][
		,
		list(sample,resolution,normalization,chr1,chr2,interaction,sample_total,chr1_total,chr2_total)
	][
		order(sample,resolution,normalization,chr1,chr2)
	][
		,
		WCPA := interaction/(((chr1_total/sample_total)*(chr2_total/(sample_total - chr1_total)) + (chr2_total/sample_total)*(chr1_total/(sample_total - chr2_total))) * sample_total/2)
	][]
}
