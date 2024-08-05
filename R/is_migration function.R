#' Designated long-distance bird migration pattern per species per cell per week
#'
#' @param i species
#' @param j week
#'
#' @export
is_migration_A <- function(i,j){ #Bird turnover category I
  tp1 <- c(1:4,11:23,30:52)
  tp2 <- c(13:25,37:50)
  tp3 <- c(5:22,27:48)
  tp4 <- c(12:34,37:41,44:48)
  tp5 <- c(11:22,31:44)
  (i %in% 1) && (j %in% tp1 || j %in% (tp1+52)) ||
    (i %in% 2) && (j %in% tp2 || j %in% (tp2+52)) ||
    (i %in% 3) && (j %in% tp3 || j %in% (tp3+52)) ||
    (i %in% 4) && (j %in% tp4 || j %in% (tp4+52)) ||
    (i %in% 5) && (j %in% tp5 || j %in% (tp5+52))
}

is_migration_B <- function(i,j){ #Bird turnover category II
  tp1 <- c(5:10,24:27)
  tp2 <- c(1:12,26:33, 51:52)
  tp3 <- c(1:11,35:36,42:43,49:52)
  tp4 <- c(1:10,23:30,45:52)
  (i %in% 1) && (j %in% tp1 || j %in% (tp1+52)) ||
    (i %in% 2) && (j %in% tp2 || j %in% (tp2+52)) ||
    (i %in% 4) && (j %in% tp3 || j %in% (tp3+52)) ||
    (i %in% 5) && (j %in% tp4 || j %in% (tp4+52))
}

is_migration_C <- function(i,j){ #Bird turnover category III
  tp1 <- c(1:4,23:26,49:52)
  (i %in% 3) && (j %in% tp1 || j %in% (tp1+52))
}

is_migration_D <- function(i,j){ #Bird turnover category IV
  tp1 <- c(28:29)
  tp2 <- c(34:36)
  (i %in% 1) && (j %in% tp1 || j %in% (tp1+52)) ||
    (i %in% 2) && (j %in% tp2 || j %in% (tp2+52))
}

is_migration_E <- function(i,j){ #Bird turnover for the first week of a new season
  tp <- c(1, 53)
  (i %in% c(1:5)) && (j %in% tp || j %in% (tp+52))
}