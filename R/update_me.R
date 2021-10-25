update_me <- function(
				package = "chaos.tools"
){
	detach(
	 	paste0("package:", package), 
	 	unload = TRUE
	 )
	
	devtools::install_github("chaosfang404/chaos.tools")

	library(package)
}