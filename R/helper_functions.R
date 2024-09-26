C_ij <- function(table){
  C <- matrix(nrow = nrow(table), ncol=ncol(table))
  n <- nrow(table)
  m <- ncol(table)
  for(i in 1:n){
    for(j in 1:m){
      if((i==1&j==m)|(i==n & j==1)){
        C[i,j] <- 0
      }
      else if(i==1 | j==1){
        C[i,j] <- sum(table[(i+1):n, (j+1):m])
      }
      else if(i==n | j==m){
        C[i,j] <- sum(table[1:(i-1), 1:(j-1)])
      }
      else{
        C[i,j] <- sum(table[(i+1):n, (j+1):m]) + sum(table[1:(i-1), 1:(j-1)])
      }
    }
  }
  return(C)
}

D_ij <- function(table){
  D <- matrix(nrow = nrow(table), ncol=ncol(table))
  n <- nrow(table)
  m <- ncol(table)
  for(i in 1:n){
    for(j in 1:m){
      if((i==1&j==1)|(i==n & j==m)){
        D[i,j] <- 0
      }
      else if(i==1 | j==m){
        D[i,j] <- sum(table[(i+1):n, 1:(j-1)])
      }
      else if(j==1 | i==n){
        D[i,j] <- sum(table[1:(i-1), (j+1):m])
      }
      else{
        D[i,j] <- sum(table[(i+1):n, 1:(j-1)]) + sum(table[1:(i-1), (j+1):m])
      }
    }
  }
  return(D)
}
table <- table(data_D$g1, data_D$X)

P <- function(table){
  C <- C_ij(table)
  return(sum(C*table))
}
Q <- function(table){
  D <- D_ij(table)
  return(sum(D*table))
}
D_r <- function(table){
  N <- sum(table)
  return(N^2 - sum(rowSums(table)^2))
}

D <- function(table){
  return((P(table)-Q(table))/D_r(table))
}
G <- function(table){
  P_val <- P(table)
  Q_val <- Q(table)
  return((P_val-Q_val)/(P_val+Q_val))
}


D_ASE1 <- function(table){
  N <- sum(table)
  D_val <- D_r(table)
  C_mat <- C_ij(table)
  D_mat <- D_ij(table)
  P_val <- P(table)
  Q_val <- Q(table)
  n_sums <- rowSums(table)
  return((2/D_val^2)*sqrt(sum(table*(D_val*(C_mat-D_mat)-(P_val-Q_val)*(N-n_sums))^2)))
}
D_ASE1(crosstab_D)

D_ASE0 <- function(table){
  N <- sum(table)
  D_val <- D_r(table)
  C_mat <- C_ij(table)
  D_mat <- D_ij(table)
  P_val <- P(table)
  Q_val <- Q(table)
  n_sums <- rowSums(table)
  return(2/D_val *sqrt(sum(table*(C_mat-D_mat)^2) - 1/N*(P_val-Q_val)^2))
}
D_ASE0(table)

D_ASE0_unif <- function(table){
  n <- nrow(table)
  m <- ncol(table)
  N <- sum(table)
  return(sqrt((4*(m^2-1)*(n+1))/(9*N*m^2*(n-1))))
}
D_ASE0_unif(table)

G_ASE1 <- function(table){
  N <- sum(table)
  D_val <- D_r(table)
  C_mat <- C_ij(table)
  D_mat <- D_ij(table)
  P_val <- P(table)
  Q_val <- Q(table)
  n_sums <- rowSums(table)
  return(4/(P_val+Q_val)^2 * sqrt(sum(table*(Q_val*C_mat-P_val*D_mat)^2)))
}

G_ASE0 <- function(table){
  N <- sum(table)
  D_val <- D_r(table)
  C_mat <- C_ij(table)
  D_mat <- D_ij(table)
  P_val <- P(table)
  Q_val <- Q(table)
  n_sums <- rowSums(table)
  return(2/(P_val+Q_val) * sqrt(sum(table*(C_mat-D_mat)^2)-1/N * (P_val-Q_val)^2))
}

G_ASE0_unif <- function(table){
  n <- nrow(table)
  m <- ncol(table)
  N <- sum(table)
  return(sqrt((4*(m+1)*(n+1))/(9*N*(m-1)*(n-1))))
}