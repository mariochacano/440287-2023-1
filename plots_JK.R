# Jack-Knife Chart
jk_plot <- function(df){
  D=sum(a$y)
  N=sum(a$x)
  Q=nrow(a)
  plot <- df %>% ggplot() +
                 geom_point(aes(x=x, y=y)) + 
                 xlab("Frecuency (n)") + ylab("MTTR (h)") + 
                 geom_text(aes(x=x, y=y, label=desc), size=2, position=position_jitter(width=0, height=0)) +
                 geom_hline(yintercept=(D/N), color="red") + #limit_y
                 geom_vline(xintercept=(N/Q), color="red") + #limit_x
                 scale_x_log10() + scale_y_log10() +
                 theme_bw()
  return(plot)
}

# Add Jack-Knife UnReliability Curve
jk_plot_add_ua <- function(df,t,color){
  df_ua <- data.frame(x=seq_along(1:max(df$x)),y=t/seq(from=1, to=max(df$x)))
  ua_line <- geom_line(data=df_ua, aes(x=x, y=y), color=color)
  return(ua_line)
}
