require(data.table)

wcpa = function(.data
){
	names(.data) <- c("sample","resolution","normalization","chr1","chr2","interaction")
	.data[
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
