chaos_survplot <- function(
	x,
	type = "pct",
	legend = c("Male","Female"),
	color = c("#E7B800", "#2E9FDF")
){
	if(type == "pct")
	{
		ggsurvplot(
			fit = x,
			fun = "pct",
			pval = TRUE,
			conf.int = TRUE,
			risk.table = "abs_pct",  # absolute number and percentage at risk.
			risk.table.y.text.col = T,# colour risk table text annotations.
			risk.table.y.text = FALSE,# show bars instead of names in text annotations
			xlab = "Time in days", # customize X axis label.
			break.time.by = 200,     # break X axis in time intervals by 200.
			risk.table.col = "strata", # Change risk table color by groups
			linetype = "strata", # Change line type by groups
			surv.median.line = "hv", # Specify median survival
			ncensor.plot = TRUE,      # plot the number of censored subjects at time t
			legend.labs = c("Male", "Female"), 
			ggtheme = theme_bw(), # Change ggplot2 theme
			palette = c("#E7B800", "#2E9FDF")
		)
	}else if(type == "event")
	{
		ggsurvplot(
			fit = x,
			fun = "event",
			pval = TRUE,
			conf.int = TRUE,
			risk.table.col = "strata", # Change risk table color by groups
			legend.labs = c("Male","Female"),
			break.time.by = 200,
			xlab = "Time in days",
			ggtheme = theme_bw(), # Change ggplot2 theme
			palette = c("#E7B800", "#2E9FDF")
		)
	}else if(type == "cumhaz")
	{
		ggsurvplot(
			fit = x,
			fun = "cumhaz",
			pval = TRUE,
			conf.int = TRUE,
			risk.table.col = "strata", # Change risk table color by groups
			legend.labs = c("Male","Female"),
			break.time.by = 200,
			xlab = "Time in days",
			ggtheme = theme_bw(), # Change ggplot2 theme
			palette = c("#E7B800", "#2E9FDF")
		)
	}
}
