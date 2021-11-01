percent <- function(
				.data,
				column,
				ext = 4
){
	data.table(
		.data
	)[
		,.N,
		column
	][
		,p := round(N/sum(N),ext)
	][
		,P := paste0(p*100,"%")
	][]
}
