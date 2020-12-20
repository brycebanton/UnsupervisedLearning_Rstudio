Principal\_Component\_Analysis
================

There are two general methods to perform PCA in R :

Spectral decomposition which examines the covariances / correlations
between variables — princomp() Singular value decomposition which
examines the covariances / correlations between individuals — prcomp()

# Reading data set for Places set and librarys in

``` r
library(grid)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
library(vegan)
```

    ## Loading required package: permute

    ## Loading required package: lattice

    ## This is vegan 2.5-7

``` r
library(rgl)
library(cluster) 
places <- read.csv("C:/Users/Bryce/Desktop/RStudio/DSCI 415/Places.csv")

names(places)
```

    ##  [1] "City"     "Climate"  "Housing"  "HlthCare" "Crime"    "Transp"  
    ##  [7] "Educ"     "Arts"     "Recreat"  "Econ"     "Long"     "Lat"     
    ## [13] "Pop"

``` r
places.sub <- places[,2:10]
names(places.sub)
```

    ## [1] "Climate"  "Housing"  "HlthCare" "Crime"    "Transp"   "Educ"     "Arts"    
    ## [8] "Recreat"  "Econ"

``` r
placesScaled <- scale(places.sub)
var(placesScaled)
```

    ##              Climate   Housing   HlthCare      Crime     Transp       Educ
    ## Climate   1.00000000 0.3862907 0.21330254 0.19238731 0.07911993 0.06451976
    ## Housing   0.38629070 1.0000000 0.45300968 0.13422166 0.27192150 0.19793386
    ## HlthCare  0.21330254 0.4530097 1.00000000 0.30465943 0.47031707 0.49023403
    ## Crime     0.19238731 0.1342217 0.30465943 1.00000000 0.28657843 0.07441627
    ## Transp    0.07911993 0.2719215 0.47031707 0.28657843 1.00000000 0.33599250
    ## Educ      0.06451976 0.1979339 0.49023403 0.07441627 0.33599250 1.00000000
    ## Arts      0.22697420 0.4485678 0.86579604 0.38948261 0.46480557 0.37327898
    ## Recreat   0.21350903 0.4222884 0.32539695 0.34462654 0.36471624 0.07780718
    ## Econ     -0.10008274 0.2694328 0.06927074 0.25999471 0.05924675 0.11970035
    ##               Arts    Recreat        Econ
    ## Climate  0.2269742 0.21350903 -0.10008274
    ## Housing  0.4485678 0.42228838  0.26943277
    ## HlthCare 0.8657960 0.32539695  0.06927074
    ## Crime    0.3894826 0.34462654  0.25999471
    ## Transp   0.4648056 0.36471624  0.05924675
    ## Educ     0.3732790 0.07780718  0.11970035
    ## Arts     1.0000000 0.37865945  0.07567330
    ## Recreat  0.3786594 1.00000000  0.17353051
    ## Econ     0.0756733 0.17353051  1.00000000

``` r
pca <- princomp(placesScaled)
names(pca)
```

    ## [1] "sdev"     "loadings" "center"   "scale"    "n.obs"    "scores"   "call"

``` r
summary(pca)
```

    ## Importance of components:
    ##                           Comp.1    Comp.2    Comp.3    Comp.4     Comp.5
    ## Standard deviation     1.8433481 1.1001301 1.0667753 0.9581851 0.86659983
    ## Proportion of Variance 0.3786991 0.1348862 0.1268310 0.1023242 0.08369832
    ## Cumulative Proportion  0.3786991 0.5135853 0.6404163 0.7427405 0.82643887
    ##                            Comp.6     Comp.7     Comp.8     Comp.9
    ## Standard deviation     0.79287155 0.70110562 0.56309129 0.34646229
    ## Proportion of Variance 0.07006243 0.05478308 0.03533761 0.01337801
    ## Cumulative Proportion  0.89650130 0.95128438 0.98662199 1.00000000

``` r
plot(pca, type="line", main = "Scree plot")
```

![](Principal_Component_Analysis_files/figure-gfm/doing%20pca-1.png)<!-- -->
The Scree plot shows that we will keep two principal components

``` r
pca$loadings
```

    ## 
    ## Loadings:
    ##          Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8 Comp.9
    ## Climate   0.206  0.218  0.690  0.137  0.369  0.375         0.362       
    ## Housing   0.357  0.251  0.208  0.512 -0.233 -0.142  0.231 -0.614       
    ## HlthCare  0.460 -0.299                0.103 -0.374         0.186 -0.716
    ## Crime     0.281  0.355 -0.185 -0.539  0.524               -0.430       
    ## Transp    0.351 -0.180 -0.146 -0.303 -0.404  0.468  0.583              
    ## Educ      0.275 -0.483 -0.230  0.335  0.209  0.502 -0.426 -0.189  0.111
    ## Arts      0.463 -0.195        -0.101  0.105 -0.462         0.204  0.686
    ## Recreat   0.328  0.384        -0.190 -0.530        -0.628  0.151       
    ## Econ      0.135  0.471 -0.607  0.422  0.160         0.150  0.405       
    ## 
    ##                Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8 Comp.9
    ## SS loadings     1.000  1.000  1.000  1.000  1.000  1.000  1.000  1.000  1.000
    ## Proportion Var  0.111  0.111  0.111  0.111  0.111  0.111  0.111  0.111  0.111
    ## Cumulative Var  0.111  0.222  0.333  0.444  0.556  0.667  0.778  0.889  1.000

``` r
biplot(pca)
```

![](Principal_Component_Analysis_files/figure-gfm/pca%20loadings%20and%20biplot-1.png)<!-- -->
The biplot shows that there are some outliers in the data set. More
specifically looks like observations 213,234,65,314,43,179,270, and 58.
When I started to take out the outliers more become out liers so I
decided to leave them in.

# Loading in Breast Cancer Data

``` r
breast <- read.csv("C:/Users/Bryce/Desktop/RStudio/DSCI 415/BreastDiag.csv")
names(breast)
```

    ##  [1] "Diagnosis"   "Radius"      "Texture"     "Perimeter"   "Area"       
    ##  [6] "Smoothness"  "Compactness" "Concavity"   "ConcavePts"  "Symmetry"   
    ## [11] "FracDim"     "serad"       "setex"       "seperi"      "searea"     
    ## [16] "sesmoo"      "secomp"      "seconc"      "seconpts"    "sesym"      
    ## [21] "sefd"        "wrad"        "wtex"        "wperi"       "warea"      
    ## [26] "wsmoo"       "wcomp"       "wconc"       "wconpts"     "wsym"       
    ## [31] "wfd"

To make things simpler I subset the data to columns I know the names
too. The others I do not know what they mean.

Also doing the correlation between the different predictors and plotting
it.

``` r
breast.sub <- breast[,1:11]
breast.sub <- subset(breast.sub, select = -c(Area,Perimeter,Diagnosis))

breast.cor = cor(breast.sub)
corrplot(breast.cor, order = "hclust")
```

![](Principal_Component_Analysis_files/figure-gfm/subsetting%20breast%20data%20and%20recoding-1.png)<!-- -->

Doing analysis on Breast cancer data set and plotting the scree/biplot
to help see how many principal compenets to keep

``` r
breast.sub.pca <- scale(breast.sub)

breast.sub.pca <- princomp(breast.sub.pca)
breast.sub.pca$sdev
```

    ##    Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7    Comp.8 
    ## 2.0687175 1.3491775 0.9078950 0.7055180 0.6096215 0.3032849 0.2620292 0.1782129

``` r
plot(breast.sub.pca, type="line", main = "Scree plot")
```

![](Principal_Component_Analysis_files/figure-gfm/PCA%20on%20subset-1.png)<!-- -->

``` r
biplot(breast.sub.pca)
```

![](Principal_Component_Analysis_files/figure-gfm/PCA%20on%20subset-2.png)<!-- -->

# Reading Data set in for NHL forwards

``` r
NHlfor =  read.csv("C:/Users/Bryce/Desktop/RStudio/DSCI 415/NHLfor.csv")
nhlfor.sub <- NHlfor[,-c(1,2,4,5)]
nhlfor.scale = scale(nhlfor.sub)  
nhlfor.pca2 = prcomp(nhlfor.scale)
summary(nhlfor.pca2)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5    PC6     PC7
    ## Standard deviation     3.5188 1.9604 1.7555 1.59574 1.35643 1.2060 1.12978
    ## Proportion of Variance 0.3439 0.1067 0.0856 0.07073 0.05111 0.0404 0.03546
    ## Cumulative Proportion  0.3439 0.4507 0.5363 0.60702 0.65813 0.6985 0.73399
    ##                            PC8     PC9    PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     1.11949 1.03493 0.98070 0.93233 0.91258 0.87055 0.83845
    ## Proportion of Variance 0.03481 0.02975 0.02672 0.02415 0.02313 0.02105 0.01953
    ## Cumulative Proportion  0.76880 0.79855 0.82527 0.84941 0.87255 0.89360 0.91313
    ##                           PC15    PC16    PC17    PC18    PC19    PC20    PC21
    ## Standard deviation     0.69085 0.67578 0.64531 0.62321 0.60245 0.52346 0.47502
    ## Proportion of Variance 0.01326 0.01269 0.01157 0.01079 0.01008 0.00761 0.00627
    ## Cumulative Proportion  0.92638 0.93907 0.95064 0.96142 0.97151 0.97912 0.98539
    ##                           PC22    PC23    PC24    PC25    PC26    PC27    PC28
    ## Standard deviation     0.43607 0.34557 0.24684 0.22892 0.21184 0.17932 0.08860
    ## Proportion of Variance 0.00528 0.00332 0.00169 0.00146 0.00125 0.00089 0.00022
    ## Cumulative Proportion  0.99067 0.99399 0.99568 0.99713 0.99838 0.99927 0.99949
    ##                           PC29    PC30    PC31    PC32    PC33    PC34
    ## Standard deviation     0.08195 0.06644 0.06460 0.05027 0.01649 0.01474
    ## Proportion of Variance 0.00019 0.00012 0.00012 0.00007 0.00001 0.00001
    ## Cumulative Proportion  0.99968 0.99980 0.99992 0.99999 0.99999 1.00000
    ##                             PC35      PC36
    ## Standard deviation     5.446e-11 2.695e-16
    ## Proportion of Variance 0.000e+00 0.000e+00
    ## Cumulative Proportion  1.000e+00 1.000e+00

``` r
plot(nhlfor.pca2, type="line", main = "Scree plot")
```

![](Principal_Component_Analysis_files/figure-gfm/nhl%20data-1.png)<!-- -->

``` r
nhlfor.cor = cor(nhlfor.sub)
corrplot(nhlfor.cor, order = "hclust")
```

![](Principal_Component_Analysis_files/figure-gfm/nhl%20data-2.png)<!-- -->

``` r
biplot(nhlfor.pca2)
```

![](Principal_Component_Analysis_files/figure-gfm/nhl%20data-3.png)<!-- -->

``` r
nhlfor.pca2$loadings[,1:2]
```

    ## NULL

``` r
pc1for <- nhlfor.pca2$x[,1]
pc2for <- nhlfor.pca2$x[,2]

NHlfor$PositionColor = ifelse(NHlfor$Position == 'LW',1,ifelse(NHlfor$Position == 'RW',2,0))
plot(pc1for,pc2for, type = "n")
text(pc1for,pc2for, labels = as.character(NHlfor$Player),col= as.numeric(NHlfor$PositionColor)+5)
```

![](Principal_Component_Analysis_files/figure-gfm/nhl%20data-4.png)<!-- -->

# Image Compression using PCA

``` r
GirlHat = read.csv('C:/Users/Bryce/Desktop/RStudio/DSCI 415/GirlwithHat.txt')
names(GirlHat)[1:50]
```

    ##  [1] "Y"   "X1"  "X2"  "X3"  "X4"  "X5"  "X6"  "X7"  "X8"  "X9"  "X10" "X11"
    ## [13] "X12" "X13" "X14" "X15" "X16" "X17" "X18" "X19" "X20" "X21" "X22" "X23"
    ## [25] "X24" "X25" "X26" "X27" "X28" "X29" "X30" "X31" "X32" "X33" "X34" "X35"
    ## [37] "X36" "X37" "X38" "X39" "X40" "X41" "X42" "X43" "X44" "X45" "X46" "X47"
    ## [49] "X48" "X49"

``` r
X = as.matrix(GirlHat[,-1])
x = seq(1:342)
y = seq(1:342)
par(pty="s")
image(x,y,X,col=gray((0:341)/341))
```

![](Principal_Component_Analysis_files/figure-gfm/printing%20img-1.png)<!-- -->
You can flip the image by doing the below code

``` r
par(pty="s")
X = apply(X,2,rev)
image(x,y,t(X),col=gray((0:341)/341))
```

![](Principal_Component_Analysis_files/figure-gfm/flipping%20img-1.png)<!-- -->

## Doing SVD on image

``` r
Xsvd = svd(X)
Xsvd$d[1:100]
```

    ##   [1] 169.8831450  27.7070887  21.4901925  16.9194793  15.4021686  14.5127204
    ##   [7]  12.0057965  10.7363715   8.8506194   8.2958627   6.9903938   6.3962857
    ##  [13]   6.2446630   6.0993503   5.7403705   5.6020132   5.4846945   4.8541030
    ##  [19]   4.5804678   4.4483930   4.3284620   4.1331781   3.8036200   3.5261088
    ##  [25]   3.4858363   3.4109601   3.2142791   3.1184794   3.0401461   2.9885597
    ##  [31]   2.7932424   2.7568325   2.7174339   2.7073164   2.6355058   2.5888812
    ##  [37]   2.5344429   2.5046815   2.4290473   2.4019975   2.3335297   2.2320734
    ##  [43]   2.2214706   2.1746607   2.1195658   2.0751659   2.0042316   1.9899940
    ##  [49]   1.9496544   1.9212357   1.8889337   1.8415948   1.7984741   1.7773555
    ##  [55]   1.7370913   1.6900752   1.6758318   1.6654965   1.6418754   1.6096536
    ##  [61]   1.5797415   1.5671153   1.5406670   1.5112244   1.4849483   1.4348266
    ##  [67]   1.4084934   1.3736059   1.3670433   1.3585713   1.3157449   1.2749949
    ##  [73]   1.2561978   1.2395967   1.2271299   1.2016318   1.1737088   1.1672778
    ##  [79]   1.1406095   1.1319696   1.1195818   1.1032419   1.0934276   1.0748342
    ##  [85]   1.0577393   1.0329042   1.0243921   1.0130424   1.0024680   0.9870664
    ##  [91]   0.9848263   0.9605501   0.9525543   0.9402360   0.9110593   0.9052684
    ##  [97]   0.8871735   0.8809918   0.8646206   0.8507517

``` r
# k = 2
par(pty="s")
D2 = diag(Xsvd$d[1:2])
U2 = Xsvd$u[,1:2]
V2 = Xsvd$v[,1:2]
Xapprox2 = U2%*%D2%*%t(V2)
image(x,y,t(Xapprox2),col=gray((0:341)/341))
```

![](Principal_Component_Analysis_files/figure-gfm/compressed%20image-1.png)<!-- -->

# Image function

``` r
image.svd = function(Xsvd,k=10){
    par(pty="s")
    Dk = diag(Xsvd$d[1:k])
    Uk = Xsvd$u[,1:k]
    Vk = Xsvd$v[,1:k]
    Xk = Uk%*%Dk%*%t(Vk)
    p = dim(Xsvd$u)[1]
    x = seq(1:p)
    y = x
    image(x,y,t(Xk),col=gray((0:(p-1))/(p-1)))
    title(paste(k," Dimensional Approximation"))
}
image.svd(Xsvd,k=10)
```

![](Principal_Component_Analysis_files/figure-gfm/image%20function-1.png)<!-- -->

Lets adjust K to 50.

``` r
image.svd(Xsvd,k=50)
```

![](Principal_Component_Analysis_files/figure-gfm/adjusting%20k-1.png)<!-- -->
