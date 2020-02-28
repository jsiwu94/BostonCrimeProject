Understanding The Effect of Social Network on Sales and Demand Spillover for Amazon Books
-----------------------------------------------------------------------------------------

This post I am going to uncover the effect that social network has on
the Amazon Book Sales. Social Network is a very powerful source in
today’s business. Every

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

    library(igraph)

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:igraph':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(sqldf)

    ## Loading required package: gsubfn

    ## Loading required package: proto

    ## Loading required package: RSQLite

    library(ggplot2)
    library(psych)

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

Looking at The Data
-------------------

    ##     Source Target
    ## 50      12    261
    ## 357     74    282
    ## 369     77    422
    ## 381     79     82
    ## 565    117    131
    ## 582    120    439

    head(product1)

    ##    id
    ## 12 12
    ## 33 33
    ## 39 39
    ## 45 45
    ## 74 74
    ## 77 77
    ##                                                                                                       title
    ## 12 Fantastic Food with Splenda : 160 Great Recipes for Meals Low in Sugar, Carbohydrates, Fat, and Calories
    ## 33                                                                           Double Jeopardy (T*Witches, 6)
    ## 39                                                                           Night of Many Dreams : A Novel
    ## 45                                                                     Beginning ASP.NET Databases using C#
    ## 74                                                      Service Delivery (It Infrastructure Library Series)
    ## 77                                                                                     Water Touching Stone
    ##    group salesrank review_cnt downloads rating
    ## 12  Book     24741         12        12    4.5
    ## 33  Book     97166          4         4    5.0
    ## 39  Book     57186         22        22    3.5
    ## 45  Book     48408          4         4    4.0
    ## 74  Book     27507          2         2    4.0
    ## 77  Book     27012         11        11    4.5

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

    net1 <- graph.data.frame(purch1, directed=T)

    ## 2. Create a variable named in-degree
    in_degree <- degree(net1, mode="in")
    head(in_degree)

    ##  12  74  77  79 117 120 
    ##   5   1   3   0   9   3

    # 3. Create a variable named out-degree
    out_degree <- degree(net1, mode="out")
    all_degree <- degree(net1, mode="all")
    all_degree[all_degree == max(all_degree)]

    ## 4429   33 
    ##   53   53

With that, we pick Book 33 since it has 53 incoming nodes. Now let’s
plot the subcomponent.

The first plot is the general network. Insight:

    ## 4. Choosing product id =  33 
    sub <- subcomponent(net1, "33", mode = "all")

    # 5. Visualize the subcomponent
    graph <- induced_subgraph(net1, sub)
    V(graph)$label <- V(graph)$name
    V(graph)$degree <- degree(graph)

    set.seed(222)
    plot(graph,
         vertex.color=rainbow(33),
         vertex.size=V(graph)$degree*0.08,
         edge.arrow.size=0.01,
         vertex.label.cex=0.03,
         layout=layout.fruchterman.reingold)

![](Social_Network_Analysis_files/figure-markdown_strict/unnamed-chunk-3-1.png)

No let’s look at the main authorities and the diameter nodes. Insight:

    ##plot 2 to see authorities and diameter
    diam <- get_diameter(graph, directed=T)

    vcol <- rep("lightsteelblue2", vcount(graph))
    vcol[diam] <- "pink"
    ecol <- rep("lightsteelblue2", ecount(graph))
    ecol[E(graph, path=diam)] <- "black" 

    set.seed(222)
    plot(graph,
         vertex.color=vcol,
         vertex.size=V(graph)$degree/3,
         edge.arrow.size=0.10,
         vertex.label.cex=V(graph)$degree*0.03,
         vertex.label.color="black",
         vertex.label.family = "sans",
         layout=layout.fruchterman.reingold)

![](Social_Network_Analysis_files/figure-markdown_strict/unnamed-chunk-4-1.png)
analyzing the diamter nodes. Insight:

    #analyze the Diameter Nodes
    print(as_ids(diam))

    ##  [1] "37895" "27936" "21584" "10889" "11080" "14111" "4429"  "2501"  "3588" 
    ## [10] "6676"

    diameterbooks <- product[product$id %in%  as_ids(diam),]
    diameterbooks[order(-diameterbooks$salesrank), ]

    ##          id                                                      title group
    ## 4390   4429                   Harley-Davidson Panheads, 1948-1965/M418  Book
    ## 6608   6676                                             Song of Eagles  Book
    ## 27613 27936                     Numerology For Personal Transformation  Book
    ## 21376 21584                                           A Year and a Day  Book
    ## 10790 10889                                 Sixpence Bride (Timeswept)  Book
    ## 3558   3588                     A Fourth Treasury of Knitting Patterns  Book
    ## 10980 11080 Counter Intelligence: Where to Eat in the Real Los Angeles  Book
    ## 2481   2501          The Narcissistic Family : Diagnosis and Treatment  Book
    ## 37464 37895              Sons and Lovers (Signet Classics (Paperback))  Book
    ## 13976 14111                    Memories, Dreams, Reflections (Vintage)  Book
    ##       salesrank review_cnt downloads rating
    ## 4390     147799          3         3    4.5
    ## 6608     130216          1         1    5.0
    ## 27613    111939          1         1    5.0
    ## 21376    107460         52        52    4.0
    ## 10790     96977         16        16    4.5
    ## 3558      91126          1         1    5.0
    ## 10980     28673         13        13    5.0
    ## 2481       9727         19        19    5.0
    ## 37464      9236         70        70    4.0
    ## 13976      4818         38        38    4.5

Now let’s measure the statistics. Insight:

    diameter <- diameter(graph, directed=T, weights=NA)
    edge_density <- edge_density(graph, loops=F)
    mean_distance <- mean_distance(graph, directed=T)
    data.frame(Statistics = c("diameter","edge_density","mean_distance"),
                  Value = c(diameter,edge_density,mean_distance))

    ##      Statistics       Value
    ## 1      diameter 9.000000000
    ## 2  edge_density 0.001436951
    ## 3 mean_distance 2.167236662

Degree Centrality

    degree_centrality <- centr_degree(graph, mode="all")
    paste("degree centrality - centralization:",degree_centrality$centralization)

    ## [1] "degree centrality - centralization: 0.027940579512858"

Closeness

    closeness <- closeness(graph, mode="all", weights=NA) 
    head(sort(closeness, decreasing = TRUE))

    ##           33          626       242813         4429          224         2558 
    ## 0.0001612383 0.0001585289 0.0001571092 0.0001557632 0.0001496110 0.0001478852

Betweeness

    between <- betweenness(graph, directed=T, weights=NA)
    head(sort(between, decreasing = TRUE))

    ##  2501  4429  3588 31513 30106 60266 
    ##   298   260   150    92    64    62

Hub Score

    hub_score <- hub.score(graph)$vector
    head(sort(hub_score), descreasing=TRUE)

    ## 1817 2071 2505 3032 3119 3588 
    ##    0    0    0    0    0    0

Authority Score

    authority_score <- authority.score(graph)$vector
    head(sort(authority_score), descreasing=TRUE)

    ##  626 2423 2501 4429 7325 7544 
    ##    0    0    0    0    0    0

Looking at the Degree Distribution

    all_degree_df <- data.frame(id = names(all_degree), degree=all_degree)
    qplot(all_degree_df$degree,
          geom="histogram",
          binwidth = 0.5,  
          main = "Degree Distribution", 
          xlab = "Degree",  
          ylab="Frequency",
          fill=I("blue"), 
          col=I("red"), 
          alpha=I(.2),
          xlim=c(0,20))

    ## Warning: Removed 6 rows containing non-finite values (stat_bin).

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](Social_Network_Analysis_files/figure-markdown_strict/unnamed-chunk-12-1.png)

Cummulative Frequency Dist

    ggplot(all_degree_df, aes(degree, colour = "skyblue")) + stat_ecdf() +
            ggtitle("Cummulative Frequency Distribution") +
            ylab("Cummulative Frequency")

![](Social_Network_Analysis_files/figure-markdown_strict/unnamed-chunk-13-1.png)

Merging the data together and adding social network analysis into the
main data

    # 7. Create neighbors variable
    names(purch1)[1] <- "id"
    sid <- as_ids(sub)
    sub_prod <- product1[product1$id %in% sid,]
    neighbors_mean_variables <- as.data.frame(purch1 %>%
                                  group_by(Target) %>%
                                  inner_join(sub_prod, by="id") %>%
                                  summarize(nghb_mn_rating = mean(rating),
                                        nghb_mn_salesrank = mean(salesrank),
                                        nghb_mn_review_cnt=mean(review_cnt)))
                      


    in_df <- data.frame(id = names(in_degree), in_degree)
    out_df <- data.frame(id = names(out_degree), out_degree)
    closeness <- data.frame(id = names(closeness), closeness)
    betweenness <- data.frame(id = names(between), between)
    authority <- data.frame(id = names(authority_score), authority_score)
    hub <- data.frame(id = names(hub_score), hub_score)

    in_df$id<-as.numeric(as.character(in_df$id))
    out_df$id<-as.numeric(as.character(out_df$id))
    closeness$id<-as.numeric(as.character(closeness$id))
    betweenness$id<-as.numeric(as.character(betweenness$id))
    authority$id<-as.numeric(as.character(authority$id))
    hub$id<-as.numeric(as.character(hub$id))

    names(neighbors_mean_variables)[1] <-"id"

    data <- sub_prod %>% inner_join(neighbors_mean_variables, by = "id") 
    data <- data  %>% inner_join(in_df, by = "id") 
    data <- data  %>% inner_join(out_df, by = "id") 
    data <- data %>% inner_join(closeness, by="id") 
    data <- data %>% inner_join(betweenness, by="id") 
    data <- data %>% inner_join(authority, by="id") 
    data <- data %>% inner_join(hub, by="id")


    head(data)

    ##    id
    ## 1  33
    ## 2  77
    ## 3  78
    ## 4 130
    ## 5 148
    ## 6 187
    ##                                                                                    title
    ## 1                                                         Double Jeopardy (T*Witches, 6)
    ## 2                                                                   Water Touching Stone
    ## 3                                                 The Ebony Cookbook: A Date With a Dish
    ## 4 The O'Reilly Factor: The Good, the Bad, and the Completely Ridiculous in American Life
    ## 5                                                                               Firebird
    ## 6                         Words for Smart Test Takers (Academic Test Preparation Series)
    ##   group salesrank review_cnt downloads rating nghb_mn_rating nghb_mn_salesrank
    ## 1  Book     97166          4         4    5.0       4.103774          82153.26
    ## 2  Book     27012         11        11    4.5       4.666667          41744.00
    ## 3  Book    140480          3         3    4.5       4.500000          73179.00
    ## 4  Book     29460        375       375    3.5       4.500000          19415.00
    ## 5  Book     77008         42        42    4.0       0.000000          46701.00
    ## 6  Book     17104          4         4    5.0       4.500000         133546.67
    ##   nghb_mn_review_cnt in_degree out_degree    closeness between authority_score
    ## 1          21.075472        53          0 1.612383e-04       0    1.000000e+00
    ## 2           4.000000         3          1 9.045681e-05      12    4.449831e-17
    ## 3         157.818182        11          0 1.191753e-04       0    5.753636e-04
    ## 4           6.000000         1          1 1.077935e-04       1    2.473186e-17
    ## 5           0.000000         1          1 1.009897e-04       2    2.567663e-17
    ## 6           3.666667         3          3 1.076774e-04       2    2.431071e-05
    ##      hub_score
    ## 1 0.000000e+00
    ## 2 2.239872e-16
    ## 3 1.140518e-17
    ## 4 5.531568e-04
    ## 5 3.592652e-05
    ## 6 5.989914e-04

looking at the summary stat for each variable

    ##                    vars   n     mean       sd   median  trimmed      mad  min
    ## salesrank             1 518 70850.97 45410.37 68466.50 69754.01 59099.40   64
    ## review_cnt            2 518    26.34    72.68     6.00    10.65     7.41    0
    ## downloads             3 518    26.26    72.65     6.00    10.56     7.41    0
    ## rating                4 518     3.94     1.46     4.50     4.29     0.74    0
    ## nghb_mn_rating        5 518     3.88     1.32     4.33     4.15     0.62    0
    ## nghb_mn_salesrank     6 518 73422.41 37494.73 73840.15 72895.76 43153.08 1596
    ## nghb_mn_review_cnt    7 518    25.58    68.32     8.00    11.83     8.90    0
    ## in_degree             8 518     2.26     3.89     1.00     1.56     0.00    1
    ## out_degree            9 518     1.35     0.85     1.00     1.33     1.48    0
    ## closeness            10 518     0.00     0.00     0.00     0.00     0.00    0
    ## between              11 518     6.59    20.53     2.00     3.13     2.97    0
    ## authority_score      12 518     0.00     0.04     0.00     0.00     0.00    0
    ## hub_score            13 518     0.04     0.19     0.00     0.00     0.00    0
    ##                       max  range  skew kurtosis      se
    ## salesrank          149844 149780  0.16    -1.28 1995.22
    ## review_cnt           1015   1015  7.10    72.99    3.19
    ## downloads            1015   1015  7.11    73.12    3.19
    ## rating                  5      5 -1.91     2.55    0.06
    ## nghb_mn_rating          5      5 -1.86     2.79    0.06
    ## nghb_mn_salesrank  149844 148248  0.10    -0.83 1647.42
    ## nghb_mn_review_cnt   1015   1015  8.40    98.06    3.00
    ## in_degree              53     52  9.20   107.78    0.17
    ## out_degree              4      4  0.30     0.07    0.04
    ## closeness               0      0  0.04    -0.56    0.00
    ## between               298    298 10.16   125.83    0.90
    ## authority_score         1      1 22.42   504.54    0.00
    ## hub_score               1      1  4.77    20.85    0.01

    ## Warning in min(diff(breaks)): no non-missing arguments to min; returning Inf

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

    ## Warning in cor(x, y, use = "pairwise", method = method): the standard deviation
    ## is zero

![](Social_Network_Analysis_files/figure-markdown_strict/unnamed-chunk-15-1.png)
Model 1

    p1 <- glm(salesrank ~ review_cnt+downloads+
                      rating+in_degree+
                      out_degree+closeness+between+
                      hub_score+authority_score+
                      nghb_mn_review_cnt+nghb_mn_salesrank+
                      nghb_mn_rating
              , data = data, family = "poisson")
    summary(p1)

    ## 
    ## Call:
    ## glm(formula = salesrank ~ review_cnt + downloads + rating + in_degree + 
    ##     out_degree + closeness + between + hub_score + authority_score + 
    ##     nghb_mn_review_cnt + nghb_mn_salesrank + nghb_mn_rating, 
    ##     family = "poisson", data = data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -363.25  -160.45    -7.61   122.01   519.58  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error   z value Pr(>|z|)    
    ## (Intercept)         1.119e+01  1.108e-03 10096.697   <2e-16 ***
    ## review_cnt         -2.868e-02  1.877e-04  -152.749   <2e-16 ***
    ## downloads           2.457e-02  1.879e-04   130.759   <2e-16 ***
    ## rating             -7.061e-03  1.098e-04   -64.314   <2e-16 ***
    ## in_degree           2.801e-03  6.819e-05    41.069   <2e-16 ***
    ## out_degree          5.646e-02  2.057e-04   274.476   <2e-16 ***
    ## closeness          -1.789e+01  7.874e+00    -2.272   0.0231 *  
    ## between            -7.349e-04  1.111e-05   -66.157   <2e-16 ***
    ## hub_score           2.452e-01  8.593e-04   285.400   <2e-16 ***
    ## authority_score     1.895e-01  4.754e-03    39.861   <2e-16 ***
    ## nghb_mn_review_cnt  7.386e-04  1.969e-06   375.165   <2e-16 ***
    ## nghb_mn_salesrank   2.057e-07  4.498e-09    45.733   <2e-16 ***
    ## nghb_mn_rating     -9.723e-03  1.253e-04   -77.613   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 16968896  on 517  degrees of freedom
    ## Residual deviance: 15315200  on 505  degrees of freedom
    ## AIC: 15321778
    ## 
    ## Number of Fisher Scoring iterations: 5

Model 2

    p2 <- glm(salesrank ~ log(review_cnt+1)+log(downloads+1)+
                            log(rating+1)+log(in_degree+1)+
                            log(out_degree+1)+closeness+log(between+1)+
                            log(hub_score+1)+log(authority_score+1)+
                      log(nghb_mn_review_cnt+1)+log(nghb_mn_salesrank+1)+
                      log(nghb_mn_rating+1)
                      , data = data, family = "poisson")

    summary(p2)

    ## 
    ## Call:
    ## glm(formula = salesrank ~ log(review_cnt + 1) + log(downloads + 
    ##     1) + log(rating + 1) + log(in_degree + 1) + log(out_degree + 
    ##     1) + closeness + log(between + 1) + log(hub_score + 1) + 
    ##     log(authority_score + 1) + log(nghb_mn_review_cnt + 1) + 
    ##     log(nghb_mn_salesrank + 1) + log(nghb_mn_rating + 1), family = "poisson", 
    ##     data = data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -334.64  -165.84   -20.24   122.84   395.53  
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                  1.093e+01  2.941e-03 3716.91   <2e-16 ***
    ## log(review_cnt + 1)         -4.664e-01  3.263e-03 -142.96   <2e-16 ***
    ## log(downloads + 1)           2.970e-01  3.265e-03   90.95   <2e-16 ***
    ## log(rating + 1)              1.513e-01  3.451e-04  438.43   <2e-16 ***
    ## log(in_degree + 1)          -1.088e-01  4.484e-04 -242.56   <2e-16 ***
    ## log(out_degree + 1)          9.994e-02  5.246e-04  190.50   <2e-16 ***
    ## closeness                   -5.452e+02  7.956e+00  -68.52   <2e-16 ***
    ## log(between + 1)             6.065e-03  1.940e-04   31.26   <2e-16 ***
    ## log(hub_score + 1)           2.868e-01  1.232e-03  232.87   <2e-16 ***
    ## log(authority_score + 1)     8.500e-01  5.135e-03  165.53   <2e-16 ***
    ## log(nghb_mn_review_cnt + 1)  6.363e-02  1.460e-04  435.68   <2e-16 ***
    ## log(nghb_mn_salesrank + 1)   3.367e-02  2.375e-04  141.75   <2e-16 ***
    ## log(nghb_mn_rating + 1)     -7.052e-02  3.978e-04 -177.25   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 16968896  on 517  degrees of freedom
    ## Residual deviance: 14957037  on 505  degrees of freedom
    ## AIC: 14963614
    ## 
    ## Number of Fisher Scoring iterations: 5

The End!
