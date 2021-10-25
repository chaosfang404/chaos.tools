update_chaos <- function(
){
	detach("package:chaos.tools", unload = T)
	
	devtools::install_github("chaosfang404/chaos.tools")

	library("chaos.tools")
}