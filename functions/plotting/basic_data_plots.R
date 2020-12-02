# Function to plot training and validation data.
# The parameter `...` can include anything that can be shared between both
# the training line and validation line. This includes `lty`.

plot_train_valid_data <- function(data,
                                  config,
                                  x="toy",
                                  y="p",
                                  mfrow=c(1, 1),
                                  oma=c(3.5, 3.5, 0, 0),
                                  par=c(0.3, 0.3, 0.3, 0.3),
                                  xlab="Time of year",
                                  ylab="Power [kW]",
                                  colors=c("black", "blue"),
                                  lty=c(1, 1),
                                  ...)
{
  # Set up plots
  par(mfrow = mfrow)
  par(oma = oma)
  par(mar = par)
  
  # Get plotting limits from `config`
  plot_seq <- c(config$N_train - config$N_train_vis,
                config$N_train,
                config$N_train + config$N_valid_vis)
  
  # Plot training
  plot(data[[x]][plot_seq[1]:plot_seq[2]],
       data[[y]][plot_seq[1]:plot_seq[2]],
       type='l',
       xlim=c(min(data[[x]][plot_seq[1]:plot_seq[3]]),
              max(data[[x]][plot_seq[1]:plot_seq[3]])),
       ylim=c(min(data[[y]][plot_seq[1]:plot_seq[3]]),
              max(data[[y]][plot_seq[1]:plot_seq[3]])),
       col=colors[1],
       axes=FALSE,
       frame.plot=TRUE,
       lty=lty[1],
       ...)
  
  # Plot validation
  lines(data[[x]][(plot_seq[2]+1):plot_seq[3]],
        data[[y]][(plot_seq[2]+1):plot_seq[3]],
        col=colors[2],
        lty=lty[2],
        ...)
  
  # Axes
  if (x == 't') {axis.POSIXct(1)}
  else {axis(1)}
  axis(2)
  
  # Labels
  mtext(xlab, side=1, line=2.5)
  mtext(ylab, side=2, line=2.5)
}

plot_fit <- function(preds,
                     conf_high,
                     conf_low,
                     config,
                     x="toy",
                     lty=c(2, 3),
                     ...)
{
  # Plot validation
  lines(data[[x]][(plot_seq[2]+1):plot_seq[3]],
        preds[1:config$N_valid_vis],
        lty=lty[1],
        ...)
  
  # Plot conf 1
  lines(data[[x]][(plot_seq[2]+1):plot_seq[3]],
        conf_high[1:config$N_valid_vis],
        lty=lty[2],
        ...)
  
  # Plot conf 2
  lines(data[[x]][(plot_seq[2]+1):plot_seq[3]],
        conf_low[1:config$N_valid_vis],
        lty=lty[2],
        ...)
}