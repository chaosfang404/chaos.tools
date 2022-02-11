chaos_tmp <-	function(
					n = 15,
					type = "default",
					replace = TRUE,
					date = TRUE
){
	if(isTRUE(date))
	{
		d <- paste0("_",Sys.Date())
	}else
	{
		d <- NULL
	}

	ln <- c(LETTERS,letters,0:9)
	greak <- c("α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ","ν","ξ","ο","π","ρ","σ","ς","τ","υ","φ","χ","ψ","ω")
	symbol <- c("·","~","！","@","#","$","%","^","&","*","(",")","_","-","+","=","|",",",".","/","<",">","?",";","'",":","[","]","{","}")

	if(type == "default")
	{
		source <- ln
	}
	else if(type %in% c("number", "n"))
	{
		source <- 0:9
	}else if(type %in% c("character", "c"))
	{
		source <- c(LETTERS,letters)
	}else if(type %in% c("greak_mixed", "g"))
	{
		source <- c(ln,greak)
	}else if(type %in% c("symbol_mixed", "s"))
	{
		source <- c(ln,symbol)
	}else if(type %in% c("mixed", "m"))
	{
		source <- c(ln,greak,symbol)
	}

	sample(x = source, size = n, replace = replace) %>%
	paste(collapse = "") %>%
	paste0(d)
}