update_chaos <- function(
){
	if("chaos.tools" %in% (.packages()))
	{
		detach("package:chaos.tools", unload = T)
	}
	
	devtools::install_github("chaosfang404/chaos.tools")

	library("chaos.tools")
}