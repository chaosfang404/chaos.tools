percent <- function(
				.data,
				column,
				ext = 2,
				group = NULL
){
	.data[
		,.N, column
	][
		,p := N/sum(N), group
	][
		,P := paste0(round(100*p,ext),"%")
	][]
}
