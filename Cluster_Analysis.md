Cluster Analysis
================

## Librarys

``` r
library(cluster)
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
library(rgl)
library(FactoMineR)
library(factoextra)
```

    ## Loading required package: ggplot2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

## Loading Data

``` r
Tennis <- read.csv("C:/Users/Bryce/Desktop/RStudio/DSCI 415/Tennis Racquets.csv")
```

## Subsetting data and making correlation plot

``` r
tennis.sub <- Tennis[,2:7]
tennis <- as.matrix(tennis.sub)

tennis.cor <- cor(tennis)
corrplot(tennis.cor, order = "hclust")
```

![](Cluster_Analysis_files/figure-gfm/correlation%20plot-1.png)<!-- -->

## Scaling data,finding distance (Multidemensional (cmdscale)), and plotting in 2d & 3d

``` r
tennis <- scale(tennis)
tennis.dist <- dist(tennis)
tennis.mds = cmdscale(tennis.dist, k=2)

plot(tennis.mds, type = "n")
text(tennis.mds, labels = as.character(Tennis$Racquet), col = as.numeric(Tennis$Cluster))
```

![](Cluster_Analysis_files/figure-gfm/scaleing%20and%20finding%20distance%20measures-1.png)<!-- -->

``` r
plot3d(tennis.mds, type = "n")
text3d(tennis.mds, texts = as.character(Tennis$Racquet), col = as.numeric(Tennis$Cluster))
```

## We can also use PCA to help visualize in lower demensional space

There are two general methods to perform PCA in R :

Spectral decomposition which examines the covariances / correlations
between variables (prcomp) Singular value decomposition which examines
the covariances / correlations between individuals (princomp)

Remember tennis was converted into a matrix

``` r
tennis.pca = princomp(tennis)
summary(tennis.pca)
```

    ## Importance of components:
    ##                           Comp.1    Comp.2    Comp.3     Comp.4     Comp.5
    ## Standard deviation     1.9648664 0.9193503 0.7665944 0.55703706 0.39529650
    ## Proportion of Variance 0.6648983 0.1455631 0.1012093 0.05343888 0.02691133
    ## Cumulative Proportion  0.6648983 0.8104614 0.9116707 0.96510960 0.99202093
    ##                             Comp.6
    ## Standard deviation     0.215244212
    ## Proportion of Variance 0.007979068
    ## Cumulative Proportion  1.000000000

``` r
tennis.pca$loadings
```

    ## 
    ## Loadings:
    ##               Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6
    ## length         0.376  0.293  0.720  0.202  0.462       
    ## static.weight -0.473  0.247         0.258         0.800
    ## balance       -0.433 -0.178  0.410  0.530 -0.373 -0.439
    ## swingweight   -0.289  0.857 -0.110 -0.186        -0.367
    ## headsize       0.453  0.197  0.235 -0.164 -0.802  0.176
    ## beamwidth      0.399  0.220 -0.491  0.742              
    ## 
    ##                Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6
    ## SS loadings     1.000  1.000  1.000  1.000  1.000  1.000
    ## Proportion Var  0.167  0.167  0.167  0.167  0.167  0.167
    ## Cumulative Var  0.167  0.333  0.500  0.667  0.833  1.000

``` r
biplot(tennis.pca)
```

![](Cluster_Analysis_files/figure-gfm/PCR%20analysis-1.png)<!-- -->

## Making a plot of Principal Components and plotting them

From the summary of the PCA it says to keep 2 principal components (keep
anything \< 1)

``` r
tennis.prcomp <- prcomp(tennis)

pc1<- tennis.prcomp$x[,1]
pc2 <- tennis.prcomp$x[,2]

plot(pc1,pc2, type = "n")
text(pc1,pc2, labels = as.character(Tennis$Racquet),col= as.numeric(Tennis$Cluster))
```

![](Cluster_Analysis_files/figure-gfm/storing%20Principal%20Compenents-1.png)<!-- -->

``` r
biplot(tennis.prcomp)
```

![](Cluster_Analysis_files/figure-gfm/storing%20Principal%20Compenents-2.png)<!-- -->

# Finding outliers and inveestigating why the stick out

``` r
racq <- c(22,2,3,8,18)
racq
```

    ## [1] 22  2  3  8 18

``` r
Tennis[racq,1:length(Tennis)]
```

    ##                Racquet length static.weight balance swingweight headsize
    ## 22        SolincoTour8  27.00         10.58   1.000         280       98
    ## 2             Asics116  27.25          9.60   0.000         287      116
    ## 3             Asics125  27.50          9.60   0.000         303      125
    ## 8  DunlopBiomimetic700  27.25          9.70  -0.375         302      110
    ## 18        HeadRadicalS  27.00         10.40   0.500         290      100
    ##    beamwidth Cluster
    ## 22      21.0       3
    ## 2       25.0       1
    ## 3       28.0       1
    ## 8       28.0       1
    ## 18      23.5       3

## Doing Manhattan distance metric and making a hierarchical cluster using ward.D

Then we cut the tree at 4

``` r
dman <- dist(tennis,method = "manhattan")
deward <- hclust(dman, method = "ward.D")
plot(deward)
```

![](Cluster_Analysis_files/figure-gfm/using%20manhattan%20distance%20metric-1.png)<!-- -->

``` r
racq.clust <- cutree(deward, k=4)
racq.clust
```

    ##  [1] 1 1 1 2 3 1 4 1 2 4 4 4 4 2 2 2 2 3 4 2 2 3 1 1 2 1 3 3 4 2 1

## Making cluster dendrogram and labling by racquet name

``` r
racname<- Tennis$Racquet

plot(deward,labels = racname, col = as.numeric(Tennis$Cluster),cex = .7)
```

![](Cluster_Analysis_files/figure-gfm/cluster%20denogram-1.png)<!-- -->

## Making another plot using the cut tree at 4

Another 2d plot made by multidimensional scaling but colored by the cut
group (4) for the trees.

``` r
plot(tennis.mds, type = "n")
text(tennis.mds,labels = as.character(Tennis$Racquet),col= as.numeric(racq.clust))
```

![](Cluster_Analysis_files/figure-gfm/plot%20with%20cut%20tree-1.png)<!-- -->

# Moving on to Genetic Data set

## Loading in data set

``` r
BCGenes <- read.csv("C:/Users/Bryce/Desktop/RStudio/DSCI 415/BCGenes.csv")
```

## Subsetting what we dont want and doing PCA on

\*\*NOTE: There is one column that is left in so we can use on hibillage
later in plots

``` r
bc.sub = BCGenes[,-c(1:14)]
bc.PCA = PCA(bc.sub,quali.sup=1)
```

![](Cluster_Analysis_files/figure-gfm/subsetting%20and%20doing%20PCA-1.png)<!-- -->![](Cluster_Analysis_files/figure-gfm/subsetting%20and%20doing%20PCA-2.png)<!-- -->

``` r
bc.PCA$eig
```

    ##         eigenvalue percentage of variance cumulative percentage of variance
    ## comp 1  45.7651607            13.34261245                          13.34261
    ## comp 2  44.3621432            12.93356946                          26.27618
    ## comp 3  26.1393070             7.62078920                          33.89697
    ## comp 4  19.3730540             5.64812070                          39.54509
    ## comp 5  12.1509111             3.54253967                          43.08763
    ## comp 6  11.8135760             3.44419126                          46.53182
    ## comp 7   9.9569254             2.90289371                          49.43472
    ## comp 8   8.7555231             2.55263065                          51.98735
    ## comp 9   7.7150952             2.24929890                          54.23665
    ## comp 10  7.1438578             2.08275738                          56.31940
    ## comp 11  6.7544044             1.96921411                          58.28862
    ## comp 12  6.4027828             1.86670054                          60.15532
    ## comp 13  5.7173145             1.66685553                          61.82217
    ## comp 14  5.4853624             1.59923102                          63.42140
    ## comp 15  5.3119696             1.54867919                          64.97008
    ## comp 16  5.0315271             1.46691753                          66.43700
    ## comp 17  4.8091127             1.40207367                          67.83907
    ## comp 18  4.6236437             1.34800109                          69.18708
    ## comp 19  4.3983977             1.28233168                          70.46941
    ## comp 20  4.2346756             1.23459931                          71.70401
    ## comp 21  4.0954642             1.19401287                          72.89802
    ## comp 22  3.9180909             1.14230056                          74.04032
    ## comp 23  3.6028673             1.05039863                          75.09072
    ## comp 24  3.4451246             1.00440950                          76.09513
    ## comp 25  3.2340725             0.94287829                          77.03801
    ## comp 26  3.1031648             0.90471278                          77.94272
    ## comp 27  3.0534121             0.89020762                          78.83293
    ## comp 28  2.8856803             0.84130620                          79.67423
    ## comp 29  2.8356463             0.82671905                          80.50095
    ## comp 30  2.7222413             0.79365634                          81.29461
    ## comp 31  2.5811006             0.75250747                          82.04712
    ## comp 32  2.4749841             0.72156970                          82.76869
    ## comp 33  2.4442525             0.71261007                          83.48130
    ## comp 34  2.3789746             0.69357859                          84.17487
    ## comp 35  2.3092215             0.67324243                          84.84812
    ## comp 36  2.2974663             0.66981526                          85.51793
    ## comp 37  2.2082232             0.64379685                          86.16173
    ## comp 38  2.1259414             0.61980798                          86.78154
    ## comp 39  2.0649379             0.60202271                          87.38356
    ## comp 40  2.0502339             0.59773582                          87.98130
    ## comp 41  1.9431891             0.56652742                          88.54782
    ## comp 42  1.8193286             0.53041649                          89.07824
    ## comp 43  1.7746008             0.51737632                          89.59562
    ## comp 44  1.7502322             0.51027178                          90.10589
    ## comp 45  1.7242979             0.50271075                          90.60860
    ## comp 46  1.6504046             0.48116751                          91.08977
    ## comp 47  1.5714044             0.45813540                          91.54790
    ## comp 48  1.5227425             0.44394825                          91.99185
    ## comp 49  1.5066822             0.43926596                          92.43112
    ## comp 50  1.4720079             0.42915682                          92.86027
    ## comp 51  1.4345349             0.41823174                          93.27850
    ## comp 52  1.3620571             0.39710120                          93.67561
    ## comp 53  1.3306636             0.38794856                          94.06355
    ## comp 54  1.3063348             0.38085563                          94.44441
    ## comp 55  1.2353658             0.36016494                          94.80457
    ## comp 56  1.1755457             0.34272468                          95.14730
    ## comp 57  1.1432013             0.33329483                          95.48059
    ## comp 58  1.0625541             0.30978254                          95.79038
    ## comp 59  1.0440484             0.30438730                          96.09476
    ## comp 60  1.0258826             0.29909114                          96.39386
    ## comp 61  0.9951424             0.29012897                          96.68398
    ## comp 62  0.9740131             0.28396884                          96.96795
    ## comp 63  0.9154127             0.26688418                          97.23484
    ## comp 64  0.8830222             0.25744088                          97.49228
    ## comp 65  0.8421696             0.24553049                          97.73781
    ## comp 66  0.8030950             0.23413849                          97.97195
    ## comp 67  0.7774366             0.22665789                          98.19860
    ## comp 68  0.7474957             0.21792877                          98.41653
    ## comp 69  0.7091775             0.20675730                          98.62329
    ## comp 70  0.6634232             0.19341785                          98.81671
    ## comp 71  0.6360637             0.18544132                          99.00215
    ## comp 72  0.5940707             0.17319845                          99.17535
    ## comp 73  0.5740288             0.16735533                          99.34270
    ## comp 74  0.5400254             0.15744181                          99.50015
    ## comp 75  0.5050221             0.14723675                          99.64738
    ## comp 76  0.4518535             0.13173572                          99.77912
    ## comp 77  0.3000282             0.08747177                          99.86659
    ## comp 78  0.2441774             0.07118873                          99.93778
    ## comp 79  0.2134195             0.06222142                         100.00000

## Plotting the indiviudals

Habillage is used because there was one column left in that was
non-numeric

``` r
var.results = get_pca_var(bc.PCA)
var.results
```

    ## Principal Component Analysis Results for variables
    ##  ===================================================
    ##   Name       Description                                    
    ## 1 "$coord"   "Coordinates for the variables"                
    ## 2 "$cor"     "Correlations between variables and dimensions"
    ## 3 "$cos2"    "Cos2 for the variables"                       
    ## 4 "$contrib" "contributions of the variables"

``` r
fviz_pca_ind(bc.PCA, habillage=1)
```

![](Cluster_Analysis_files/figure-gfm/plotting%20individuals-1.png)<!-- -->

## Dendrogram and banner

``` r
bc.sub_2 = BCGenes[,-c(1:15)]
genes.agnes =  agnes(bc.sub_2, metric = "manhattan", method = "ward")
plot(genes.agnes)
```

![](Cluster_Analysis_files/figure-gfm/doing%20another%20dendrogram%20using%20agnes-1.png)<!-- -->![](Cluster_Analysis_files/figure-gfm/doing%20another%20dendrogram%20using%20agnes-2.png)<!-- -->

## Cutting the tree at and making table

This helps to see what is in the groups. For example in group one their
are 13 Basal-Like, 1 HER2-enriched, and 1 Luminal A

``` r
grps <- cutree(genes.agnes, k=6)
table(grps,bc.sub$PAM50mRNA)
```

    ##     
    ## grps Basal-like HER2-enriched Luminal A Luminal B
    ##    1         13             1         1         0
    ##    2          2             1         4         0
    ##    3          4             2         0         0
    ##    4          0             7         0        10
    ##    5          0             1         1        11
    ##    6          0             1        17         4

## Using different methods with Agnes to see how the trees compare

``` r
genes.agnes_complete =  agnes(bc.sub_2, metric = "manhattan", method = "complete")
plot(genes.agnes_complete)
```

![](Cluster_Analysis_files/figure-gfm/using%20different%20methods%20for%20dendrograms-1.png)<!-- -->![](Cluster_Analysis_files/figure-gfm/using%20different%20methods%20for%20dendrograms-2.png)<!-- -->

``` r
genes.agnes_average =  agnes(bc.sub_2, metric = "manhattan", method = "average")
plot(genes.agnes_average)
```

![](Cluster_Analysis_files/figure-gfm/using%20different%20methods%20for%20dendrograms-3.png)<!-- -->![](Cluster_Analysis_files/figure-gfm/using%20different%20methods%20for%20dendrograms-4.png)<!-- -->

## Finding the optimal amount of clusters using different methods

``` r
fviz_nbclust(bc.sub_2,kmeans,k.max=15,method="gap_stat")
```

![](Cluster_Analysis_files/figure-gfm/finding%20optimal%20amount%20of%20clusters%20using%20different%20methods-1.png)<!-- -->

``` r
fviz_nbclust(bc.sub_2,kmeans,k.max=15,method="silhouette")
```

![](Cluster_Analysis_files/figure-gfm/finding%20optimal%20amount%20of%20clusters%20using%20different%20methods-2.png)<!-- -->

``` r
fviz_nbclust(bc.sub_2,kmeans,k.max=15,method="wss")
```

![](Cluster_Analysis_files/figure-gfm/finding%20optimal%20amount%20of%20clusters%20using%20different%20methods-3.png)<!-- -->

## Doing kmeans on 4 clusters and plotting

``` r
bc.kmeans = kmeans(bc.sub_2,4)
table(bc.kmeans$cluster,BCGenes$PAM50mRNA)
```

    ##    
    ##     Basal-like HER2-enriched Luminal A Luminal B
    ##   1          0            12         2        10
    ##   2         16             0         1         0
    ##   3          0             0         8        15
    ##   4          3             1        12         0

``` r
fviz_cluster(bc.kmeans,data=bc.sub_2)
```

![](Cluster_Analysis_files/figure-gfm/cutting%20at%204%20clusters-1.png)<!-- -->

## Making heat map on the clusters

``` r
library(mixOmics)
```

    ## Loading required package: MASS

    ## Loading required package: lattice

    ## 
    ## Loaded mixOmics 6.14.0
    ## Thank you for using mixOmics!
    ## Tutorials: http://mixomics.org
    ## Bookdown vignette: https://mixomicsteam.github.io/Bookdown
    ## Questions, issues: Follow the prompts at http://mixomics.org/contact-us
    ## Cite us:  citation('mixOmics')

``` r
par('mar')
```

    ## [1] 5.1 4.1 4.1 2.1

``` r
par(mar=c(1,1,1,1))
par('mar')
```

    ## [1] 1 1 1 1

``` r
bc.sub_2 = scale(bc.sub[,-1])
bc.dist = dist(bc.sub_2) 
bc.hclust = hclust(bc.dist,method="ward.D")  
grps = cutree(bc.hclust,k=4)
cim(bc.sub_2,cluster="both",color = rainbow(100),
        dist.method=c("euclidean","correlation"),
        clust.method=c("ward.D","ward.D"),
        row.sideColors=as.character(grps))
```

    ## Error in cim plot: figure margins too large. See ?cim for help.
