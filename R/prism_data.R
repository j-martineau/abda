#' @name prism_data
#' @title Generate a data set of measurements of randomly generated rectangular
#'   prisms.
#' @param n Integer scalar giving the number of rectangular prisms to randomly
#'   generate.
#' @param r Numeric scalar in the interval -1 to 1 giving a correlation among
#'   all fundamental measures (width, height, depth, density, elevation,
#'  horizontal velocity, and vertical velocity).
#' @return A data.frame with the variables described in \code{prism_defs()} for
#'   \code{n} randomly generated rectangular prisms.
#' @export
prism_data <- function(n, r) {
  R        <- matrix(r, nrow = 9, ncol = 9); diag(R) <- 1                        # correlation matrix
  M        <- c(10, 10, 10,  10, 10, 10, 0, 0, 0)                                # means
  names(M) <- c("DENSITY", "ELEVATION" , "GRAVITY", "LENGTH.X", "LENGTH.Y",      # variable labels
                "LENGTH.Z","VELOCITY.X", "VELOCITY.Y", "VELOCITY.Z")
  X    <- MASS::mvrnorm(n = n, mu = M, Sigma = R)                                # simulated data with specified cases, means, and correlation
  DENS <- X[, 1] ; ELEV <- X[, 2] ; GRAV <- X[, 3]                               # extract the variables from the simulated data
  LX.. <- X[, 3] ; LY.. <- X[, 4] ; LZ.. <- X[, 5]
  VX.. <- X[, 6] ; VY.. <- X[, 7] ; VZ.. <- X[, 8]
  LX.2 <- LX..^2 ; LY.2 <- LY..^2 ; LZ.2 <- LZ..^2
  VX.2 <- VX..^2 ; VY.2 <- VY..^2 ; VZ.2 <- VZ..^2
  AXY. <-      LX.. * LY.. ; AXZ. <-      LX.. * LZ..                            # area of the x-y and x-z faces
  AYZ. <-      LY.. * LZ.. ; AXYZ <- 2 * (AXY. + AXZ. + AYZ.)                    # area of the y-z face and total surface area
  CXY. <- 2 * (LX.. + LY..); CXZ. <- 2 * (LX.. + LZ..)                           # circumference of the x-y and x-z faces
  CYZ. <- 2 * (LY.. + LZ..); CXYZ <- 4 * (LX.. + LY.. + LZ..)                    # circumference of the y-z face and the sum of all edge lengths
  DXY. <- sqrt(LX.2 + LY.2); DXZ. <- sqrt(LX.2 + LZ.2)                           # length of the x-y and x-z face diagonals
  DYZ. <- sqrt(LY.2 + LZ.2); DXYZ <- sqrt(LX.2 + LY.2 + LZ.2)                    # length of the y-z face diagonal and the internal 3-D diagonal
  VXY. <- sqrt(VX.2 + VY.2); VXZ. <- sqrt(VX.2 + VZ.2)                           # velocity in the x-y plane and x-z plane
  VYZ. <- sqrt(VY.2 + VZ.2); VXYZ <- sqrt(VX.2 + VY.2 + VZ.2)                    # velocity in the y-z plane and total velocity
  VOLM <- LX.. * LY.. * LZ..                                                     # volume
  MASS <- VOLM * DENS                                                            # mass
  KNTC <- MASS * VXYZ                                                            # kinetic energy
  PTNT <- MASS * ELEV * GRAV                                                     # potential energy
  TOTL <- KNTC + PTNT                                                            # total energy
  y <- tibble::tibble(                                                           # results
    DENSITY      = DENS, ELEVATION   = ELEV, GRAVITY     = GRAV,
    LENGTH.X     = LX.., LENGTH.Y    = LY.., LENGTH.Z    = LZ..,
    VELOCITY.X   = VX.., VELOCITY.Y  = VY.., VELOCITY.Z  = VZ..,
    area.xy      = AXY., area.xz     = AXZ., area.yz     = AYZ.,
    area.surface = AXYZ, diag.xy     = DXY., diag.xz     = DXZ.,
    diag.yz      = DYZ., diag.xyz    = DXYZ, circ.xy     = CXY.,
    circ.xz      = CXZ., circ.yz     = CYZ., circ.xyz    = CXYZ,
    velocity.xy  = VXY., velocity.xz = VXZ., velocity.yz = VYZ.,
    velocity.xyz = VXYZ, volume      = VOLM, mass        = MASS,
    kinetic      = KNTC, potential   = PTNT, total       = TOTL
  )
  y
}
