
#' A function that plots the surface of a bivariate normal distribution
#'
#' @param meanvec A matrix with 2 rows andone column that stores the mean of the bivariate normal distribution
#' @param covmat A 2 by 2 symmetric covariance matrix
#' @param type A character stating the type of plot from the plot.surface function of the fields package ("c" is a contour, "p" is a perspective/drape plot)
#' @param xlab A character for the x-axis label
#' @param ylab A character for the y-axis label
#' @param zlab A character for the z-axis label
#' @param main A character for the main label
#' @return A plot of the surface of a bivariate normal with meanvec as the mean vector and covmat as the covariance matrix
#' @export
plot_bvn_surface = function(meanvec, covmat, type, xlab, ylab, zlab, main){

  #creating values for x and y axes based on estimated values from model
  x = matrix(seq(meanvec[1,1]-4*sqrt(covmat[1,1]), meanvec[1,1]+4*sqrt(covmat[1,1]), .1*sqrt(covmat[1,1])), ncol=1)
  y = matrix(seq(meanvec[2,1]-4*sqrt(covmat[2,2]), meanvec[2,1]+4*sqrt(covmat[2,2]), .1*sqrt(covmat[2,2])), ncol=1)
  z = matrix(0,nrow = dim(x)[1],ncol = dim(y)[1])

  for (i in 1:dim(x)[1]){
    for (j in 1:dim(y)[1]){
      z[i,j] = mnormt::dmnorm(c(x[i,1],y[j,1]),mean = t(meanvec), varcov = covmat, log = FALSE)
    }
  }

  grid.list=list(x = x, y = y)
  z1 = z[1:dim(x)[1]-1,1:dim(y)[1]-1]
  mygrid = fields::make.surface.grid(grid.list)
  out = list(x = grid.list$x, y = grid.list$y, z = z)

  fields::plot.surface(out,type=type, xlab=xlab, ylab=ylab, zlab=zlab, main=main)
  return(TRUE)
}
