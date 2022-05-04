split_quadrant <- function(spp,sq){
  temporal_1 <- spp
  temporal_1$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[1]])
  temporal_2 <- spp
  temporal_2$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[2]])
  temporal_3 <- spp
  temporal_3$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[3]])
  temporal_4 <- spp
  temporal_4$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[4]])
  temporal_5 <- spp
  temporal_5$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[5]])
  temporal_6 <- spp
  temporal_6$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[6]])
  temporal_7 <- spp
  temporal_7$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[7]])
  temporal_8 <- spp
  temporal_8$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[8]])
  temporal_9 <- spp
  temporal_9$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[9]])
  temporal_10 <- spp
  temporal_10$window <- spatstat.geom::as.owin(quadrats(spp, 5, 5)[sq[10]])
  train_ppp <- subset.ppp(spp,
                          !(dplyr::between(x,
                                           temporal_1$window$xrange[1],
                                           temporal_1$window$xrange[2]) & 
                              dplyr::between(y,
                                             temporal_1$window$yrange[1],
                                             temporal_1$window$yrange[2]) ) |
                            (dplyr::between(x,
                                            temporal_2$window$xrange[1],
                                            temporal_2$window$xrange[2]) &
                               dplyr::between(y,
                                              temporal_2$window$yrange[1],
                                              temporal_2$window$yrange[2]) ) |
                            (dplyr::between(x,
                                            temporal_3$window$xrange[1],
                                            temporal_3$window$xrange[2]) &
                               dplyr::between(y,
                                              temporal_3$window$yrange[1],
                                              temporal_3$window$yrange[2]) ) |
                            (dplyr::between(x,
                                            temporal_4$window$xrange[1],
                                            temporal_4$window$xrange[2]) &
                               dplyr::between(y,
                                              temporal_4$window$yrange[1],
                                              temporal_4$window$yrange[2]) ) |
                            (dplyr::between(x,
                                            temporal_5$window$xrange[1],
                                            temporal_5$window$xrange[2]) &
                               dplyr::between(y,
                                              temporal_5$window$yrange[1],
                                              temporal_5$window$yrange[2]) ) |
                            (dplyr::between(x,
                                            temporal_6$window$xrange[1],
                                            temporal_6$window$xrange[2]) &
                               dplyr::between(y,
                                              temporal_6$window$yrange[1],
                                              temporal_6$window$yrange[2]) ) |
                            (dplyr::between(x,
                                            temporal_7$window$xrange[1],
                                            temporal_7$window$xrange[2]) &
                               dplyr::between(y,
                                              temporal_7$window$yrange[1],
                                              temporal_7$window$yrange[2]) ) |
                            (dplyr::between(x,
                                            temporal_8$window$xrange[1],
                                            temporal_8$window$xrange[2]) &
                               dplyr::between(y,
                                              temporal_8$window$yrange[1],
                                              temporal_8$window$yrange[2]) ) |
                            (dplyr::between(x,
                                            temporal_9$window$xrange[1],
                                            temporal_9$window$xrange[2]) &
                               dplyr::between(y,
                                              temporal_9$window$yrange[1],
                                              temporal_9$window$yrange[2]) ) |
                            (dplyr::between(x,
                                            temporal_10$window$xrange[1],
                                            temporal_10$window$xrange[2]) &
                               dplyr::between(y,
                                              temporal_10$window$yrange[1],
                                              temporal_10$window$yrange[2]) ) )
  return(train_ppp)
}
