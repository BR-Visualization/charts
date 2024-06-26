library(tibble)

# parameter setup
ntrt = 3
nvisits = 6
ncat = 5
n = 100

set.seed(1234)

# create data with characteristics
df <- data.frame(
  usubjid = rep(1:(ntrt*n), each = nvisits),
  visit = rep(1:nvisits, ntrt*n),
  trtn = rep(1:ntrt, each = (n*nvisits))
)

# initial outcomes from first visit
df <- df %>% add_column(brcatn = NA)
count <- c(30,45,150,45,30)
df[df$visit == 1,]$brcatn <- sample(c(rep(1,count[1]), rep(2,count[2]),
                                    rep(3,count[3]), rep(4,count[4]),
                                    rep(5,count[5])))

# pseudo-transition matrix
mat_p <- matrix(c(0.15,0.10,0.60,0.10,0.05,
                  0.10,0.15,0.60,0.10,0.05,
                  0.10,0.10,0.60,0.10,0.10,
                  0.10,0.10,0.60,0.15,0.05,
                  0.00,0.00,0.00,0.00,1.00),
                ncat,ncat, byrow = TRUE)

mat_l <- matrix(c(0.55,0.30,0.10,0.03,0.02,
                  0.35,0.45,0.15,0.03,0.02,
                  0.25,0.3,0.4,0.02,0.02,
                  0.05,0.1,0.25,0.45,0.15,
                  0.00,0.00,0.00,0.00,1.00),
                ncat,ncat, byrow = TRUE)

mat_h <- matrix(c(0.65,0.20,0.05,0.07,0.03,
                  0.40,0.45,0.05,0.05,0.05,
                  0.30,0.45,0.15,0.08,0.02,
                  0.02,0.3,0.18,0.30,0.20,
                  0.00,0.00,0.00,0.00,1.00),
                ncat,ncat, byrow = TRUE)
mat <- list(mat_p, mat_l, mat_h)

# simulate individual outcome data over visits using pseudo-transition matrix
for(i in 1:(ntrt*n)){
  init <- df[((i-1)*nvisits + 1),]$brcatn
  group <- df[((i-1)*nvisits + 1),]$trtn
  val <- rep(0,(nvisits-1))
  for(j in 1:(nvisits-1)){
    val[j] <- sample(1:ncat,prob = mat[[group]][init,],size=1)
  }
  if(ncat %in% val){
    val[match(ncat,val):(nvisits-1)] <- ncat
  }
  df[((i-1)*nvisits + 2):(i*nvisits),]$brcatn <- val
}

# create treatments and categories as factors
# they can be manipulated as numeric variables but display text
treatments = c("Placebo (N=500)", "Low Dose (N=500)", "High Dose (N=500)")
df$trt = factor(df$trtn, labels = treatments)

categories<-c("Benefit w/o AE", "Benefit with AE", "No Benefit no AE",
              "No Benefit with AE", "Withdrew")
df$brcat <- factor(df$brcatn, labels = categories)

comp_outcome <- df

usethis::use_data(comp_outcome, overwrite = TRUE)
