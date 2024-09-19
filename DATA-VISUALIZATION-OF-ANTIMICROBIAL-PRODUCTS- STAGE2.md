<h2 align="center">
<kbd>TRACK: AMR IN CANCER</kbd>
<br/>
DATA ANALYSIS AND VISUALIZATION
<br/>
Pipeline of antimicrobial agents in clinical development (WHO - Nov 2021)
</h2>

<h4 align="center">
CONTRIBUTORS
</h4> 
<div align="center">
<table><tr><td> 
  
Adaradohun Samson (***@Adara***); Nwokocha Amarachi (***@Amara***); </br>
Lakshana Bakthavachalam (***@Lakshana***); Astrid Liliana Vargas Sanchez (***@Liliana***); </br>
Mahesh Rani Kamilus (***@Mahesh***)

</td></tr></table>
</div>

***

</br>

WHO reviewed the publicly available information on antibacterial
agents in pre-clinical and clinical development to assess the drug
candidates action against WHO identified bacterial priority pathogens,
*Mycobacterium tuberculosis*, and *Clostridium difficile*.</br>

In Nov 2021, a total of 80 products were in clinical development (3 in
pre-registration phase). Among these 80 candidates 46 were antibiotics
and 34 were non-traditional antibacterial agents. Through this pipeline
the products can be explored by type, phase of clinical development,
activity against pathogens,innovativeness, route of administration,
antibiotic class and developers.</br>

Let’s try to analyse and visualize the data. </br>

``` r
# IMPORTING DATA AS CSV FILE
AMR_PRODUCTS <- read.csv(file="AMR_DATA.csv")
head(AMR_PRODUCTS)
```

    ##                 Data.source Date.of.update  Product.name Alternative.name
    ## 1 WHO AMR pipeline analysis         Nov-21 Bacteriophage                 
    ## 2 WHO AMR pipeline analysis         Nov-21 Bacteriophage                 
    ## 3 WHO AMR pipeline analysis         Nov-21 Bacteriophage                 
    ## 4 WHO AMR pipeline analysis         Nov-21 Bacteriophage                 
    ## 5 WHO AMR pipeline analysis         Nov-21 Bacteriophage                 
    ## 6 WHO AMR pipeline analysis         Nov-21 Bacteriophage                 
    ##      Product.type              Non.traditionals.categories Antibacterial.class
    ## 1 Non-traditional Bacteriophages and phage-derived enzymes       Bacteriophage
    ## 2 Non-traditional Bacteriophages and phage-derived enzymes       Bacteriophage
    ## 3 Non-traditional Bacteriophages and phage-derived enzymes       Bacteriophage
    ## 4 Non-traditional Bacteriophages and phage-derived enzymes       Bacteriophage
    ## 5 Non-traditional Bacteriophages and phage-derived enzymes       Bacteriophage
    ## 6 Non-traditional Bacteriophages and phage-derived enzymes       Bacteriophage
    ##                       Indications Route.of.administration R.D.phase
    ## 1 Gram positive and Gram negative              Inhalation Phase III
    ## 2 Gram positive and Gram negative              Inhalation Phase III
    ## 3 Gram positive and Gram negative              Inhalation Phase III
    ## 4 Gram positive and Gram negative              Inhalation Phase III
    ## 5 Gram positive and Gram negative              Inhalation Phase III
    ## 6 Gram positive and Gram negative              Inhalation Phase III
    ##   Clinical.trials                             Developer  Pathogen.category
    ## 1     NCT04682964 Tashkent Pediatric Medical\nInstitute Priority pathogens
    ## 2     NCT04682964 Tashkent Pediatric Medical\nInstitute Priority pathogens
    ## 3     NCT04682964 Tashkent Pediatric Medical\nInstitute Priority pathogens
    ## 4     NCT04682964 Tashkent Pediatric Medical\nInstitute Priority pathogens
    ## 5     NCT04682964 Tashkent Pediatric Medical\nInstitute Priority pathogens
    ## 6     NCT04682964 Tashkent Pediatric Medical\nInstitute Priority pathogens
    ##              Pathogen.name           Pathogen.activity
    ## 1 Streptococcus pneumoniae    Other priority pathogens
    ## 2    Staphylococcus aureus    Other priority pathogens
    ## 3   Pseudomonas aeruginosa Critical priority pathogens
    ## 4 Other priority pathogens    Other priority pathogens
    ## 5    Neisseria gonnorhoeae    Other priority pathogens
    ## 6      Helicobacter pylori    Other priority pathogens
    ##   Active.against.priority.pathogens. Innovative. NCE.
    ## 1                                Yes         N/A  Yes
    ## 2                                Yes         N/A  Yes
    ## 3                                Yes         N/A  Yes
    ## 4                                Yes         N/A  Yes
    ## 5                                N/A         N/A  Yes
    ## 6                                N/A         N/A  Yes
    ##   Mycobacterium.tuberculosis Clostridioides.difficile
    ## 1                       <NA>                     <NA>
    ## 2                       <NA>                     <NA>
    ## 3                       <NA>                     <NA>
    ## 4                       <NA>                     <NA>
    ## 5                       <NA>                     <NA>
    ## 6                       <NA>                     <NA>

``` r
library(ggplot2)
library(dplyr)
```

``` r
# DATA STRATIFICATION
Antibiotics <- AMR_PRODUCTS %>% filter(Product.type == "Antibiotics")
Non_traditional <- AMR_PRODUCTS %>% filter(Product.type == "Non-traditional")

COUNT_1 <- unique(Non_traditional$Product.name)
L1 <- length(COUNT_1)

COUNT_2 <- unique(Antibiotics$Product.name)
L2 <- length(COUNT_2)

PRODUCT_TYPE <- data.frame("Type" = c("Non_traditional","Antibiotics"),"Count" = c(L1,L2))
PRODUCT_TYPE
```

    ##              Type Count
    ## 1 Non_traditional    34
    ## 2     Antibiotics    46

The data was subsetted based on product type. **46 antibiotics and 34
non-traditional agents** were found as mentioned. </br>

``` r
#TARGET PATHOGEN
AMR_PRODUCTS$Pathogen.name <- as.factor(AMR_PRODUCTS$Pathogen.name)
ggplot(data = AMR_PRODUCTS, aes(y= `Pathogen.name`))+ geom_bar(aes(fill = Pathogen.name), color = "black")+ labs(title = "TARGET PATHOGEN", y= "Pathogen", x= "Frequency")+ theme_classic()+ theme(plot.title  = element_text(face = "bold"),legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=5))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-3-1.png)

Most of the drugs studied are targeted at ***Staphylococcus aureus***,
while the fewest are focused on ***Mycobacterium tuberculosis***. </br>

``` r
# TARGET OF ANTIMICROBIAL AGENTS: SPECTRUM OF ACTIVITY
Target <- AMR_PRODUCTS %>% select(Pathogen.name, Product.type)
Target <- table(Target)
Target <- as.data.frame(Target)
Target
```

    ##                       Pathogen.name    Product.type Freq
    ## 1           Acinetobacter baumannii     Antibiotics   28
    ## 2   All critical priority pathogens     Antibiotics   28
    ## 3                Campylobacter spp.     Antibiotics   28
    ## 4          Clostridioides difficile     Antibiotics    5
    ## 5       Critical priority pathogens     Antibiotics   28
    ## 6                  Enterobacterales     Antibiotics   28
    ## 7              Enterococcus faecium     Antibiotics   28
    ## 8  Gram-positive priority pathogens     Antibiotics   28
    ## 9               Helicobacter pylori     Antibiotics   28
    ## 10       Mycobacterium tuberculosis     Antibiotics   13
    ## 11            Neisseria gonnorhoeae     Antibiotics   28
    ## 12         Other priority pathogens     Antibiotics   28
    ## 13           Pseudomonas aeruginosa     Antibiotics   28
    ## 14            Staphylococcus aureus     Antibiotics   29
    ## 15         Streptococcus pneumoniae     Antibiotics   28
    ## 16          Acinetobacter baumannii Non-traditional   21
    ## 17  All critical priority pathogens Non-traditional   21
    ## 18               Campylobacter spp. Non-traditional   21
    ## 19         Clostridioides difficile Non-traditional   12
    ## 20      Critical priority pathogens Non-traditional   21
    ## 21                 Enterobacterales Non-traditional   21
    ## 22             Enterococcus faecium Non-traditional   21
    ## 23 Gram-positive priority pathogens Non-traditional   21
    ## 24              Helicobacter pylori Non-traditional   21
    ## 25       Mycobacterium tuberculosis Non-traditional    1
    ## 26            Neisseria gonnorhoeae Non-traditional   21
    ## 27         Other priority pathogens Non-traditional   21
    ## 28           Pseudomonas aeruginosa Non-traditional   21
    ## 29            Staphylococcus aureus Non-traditional   21
    ## 30         Streptococcus pneumoniae Non-traditional   21

``` r
ggplot(Target, aes(x = Pathogen.name, fill = Freq )) + geom_bar(position = "dodge", color="black") + facet_wrap(~ Product.type) + labs(title = "SPECTRUM OF ANTIMICROBIALS AGAINST PATHOGENS", fill = "Count of drugs against pathogens", x= "Pathogen")+ theme_classic()+  theme(plot.title  = element_text(face = "bold")) + theme(axis.text.x = element_text(angle = 90))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# ANTIBIOTICS - TARGET PATHOGENS
ggplot(data = Antibiotics, aes(x = Product.name)) + geom_bar(aes(fill = Pathogen.name), color ="black")+ labs(title = "ANTIBIOTICS - TARGET PATHOGENS", y = "TARGET", x = "PRODUCT", fill= "Target")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"), legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=5))+ theme(axis.text.x = element_text(angle = 90))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
# NON-TRADITIONALS - TARGET PATHOGEN
ggplot(data = Non_traditional, aes(x = Product.name)) + geom_bar(aes(fill = Pathogen.name), color ="black")+ labs(title = "NON-TRADITIONALS PRODUCTS - TARGET PATHOGENS", y = "TARGET", x = "PRODUCT", fill= "Target")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"), legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=5))+ theme(axis.text.x = element_text(angle = 90))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-4-3.png)

From table and graphs, we can infer details on the spectrum of activity
of antibiotics and non-traditional. Most of the products are
broad-spectrum, effective against all priority pathogens.

Narrow-spectrum antibiotics and non-traditional specifically against
*Mycobacterium tuberculosis* are
**GSK3036656,BTZ-043,Delpazolid,GSK2556286,Macozinone,OPC-167832,Telacebec,TBI-223,TBI-166,TBAJ-876,TBAJ586,TBA7371**
and **BVL-GSK098** respectively.

Narrow-spectrum antibiotics and non-traditional specifically against
*Clostridium difficile* are
**CRS3123,DNV3837,Ibezapolstat,MGB-BP-3,Ridinilazole** and
**ART24,BB128,DAV132,CP101,IM01,LMN201,MET2,RBX2660,RBX7455,SER101,SYN-004,VE303**
respectively.

More antibiotics target ***Mycobacterium tuberculosis*** when compared to
non-traditional and the reverse holds true for ***Clostridium difficile***
</br>

``` r
# ROUTE OF ADMINISTRATION
Route <- AMR_PRODUCTS %>% select(Product.name, Route.of.administration)
Route <- distinct(Route)
Route <- as.data.frame(Route)
ggplot(data = Route, aes(x = Route.of.administration)) + geom_bar(aes(fill = Route.of.administration), color ="black")+ labs(title = "ROUTE OF ADMINISTRATION",  x = "Route", fill= "Route")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"))+ theme(axis.text.x = element_text(angle = 90))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
Route
```

    ##                                  Product.name Route.of.administration
    ## 1                               Bacteriophage              Inhalation
    ## 2                                     9MW1411                      IV
    ## 3                                       AB103                      IV
    ## 4                                    Afabicin                IV, Oral
    ## 5                                       ALS-4                    Oral
    ## 6                                     AP-PA02              Inhalation
    ## 7                                      AR-301                      IV
    ## 8                                      AR-302                      IV
    ## 9                                    ARX-1796                    Oral
    ## 10                                  Benapenem                      IV
    ## 11                                    BWC0977                      IV
    ## 12                                    BX004-A              Inhalation
    ## 13                                      CAL02                      IV
    ## 14                                     CF-301                      IV
    ## 15                                 Delpazolid                    Oral
    ## 16                    Durlobactam + sulbactam                      IV
    ## 17                                   EBL-1003                      IV
    ## 18                  Enmetazobactam + cefepime                      IV
    ## 19                      ETX0282 + cefpodoxime                    Oral
    ## 20                    Ftortiazinon + cefepime                    Oral
    ## 21                                Gepotidacin                IV, Oral
    ## 22                                 GSK3882347                    Oral
    ## 23                                   KBP-7072                    Oral
    ## 24                                   LBP-EC01                      IV
    ## 25                                    LMN-101                    Oral
    ## 26                                  LSVT-1701                      IV
    ## 27                                      MRX-8                      IV
    ## 28                     Nacubactam + meropenem                      IV
    ## 29                              Nafithromycin                    Oral
    ## 30                                     OligoG              Inhalation
    ## 31                                      Phage                      IV
    ## 32                          QPX7728 + QPX2014                      IV
    ## 33                          QPX7728 + QPX2015                    Oral
    ## 34                                    QPX9003                      IV
    ## 35                                     RG6006                      IV
    ## 36                                   Rhu-pGSN                      IV
    ## 37                              Solithromycin               IV & oral
    ## 38                                    SPR-206                      IV
    ## 39 Sulopenem, sulopenem etzadroxil/probenecid                IV, Oral
    ## 40                                  SVT-1C469                    Oral
    ## 41                   Taniborbactam + cefepime                      IV
    ## 42                                   TNP-2092                IV, Oral
    ## 43                                   TNP-2198                    Oral
    ## 44                                    TRL1068                      IV
    ## 45                                     TXA709                IV, Oral
    ## 46                     VNRX-7145 + ceftibuten                    Oral
    ## 47             XNW4107 +imipenem \n+ cilastin                      IV
    ## 48                                     YPT-01              Inhalation
    ## 49                      Zidebactam + cefepime                      IV
    ## 50                               Zoliflodacin                    Oral
    ## 51                                 GSK3036656                    Oral
    ## 52                                    BTZ-043                    Oral
    ## 53                                 BVL-GSK098                    Oral
    ## 54                                 GSK2556286                    Oral
    ## 55                                 Macozinone                    Oral
    ## 56                                 OPC-167832                    Oral
    ## 57                                  Sutezolid                    Oral
    ## 58                                   TBA-7371                    Oral
    ## 59                                   TBAJ-587                    Oral
    ## 60                                   TBAJ-876                    Oral
    ## 61                                    TBI-166                    Oral
    ## 62                                    TBI-223                    Oral
    ## 63                                  Telacebec                    Oral
    ## 64                                      ART24                    Oral
    ## 65                                      BB128             Colonoscopy
    ## 66                                      CP101                    Oral
    ## 67                                    CRS3123                    Oral
    ## 68                                     DAV132                    Oral
    ## 69                                    DNV3837                      IV
    ## 70                               Ibezapolstat      Oral, Not absorbed
    ## 71                                      IM-01                    Oral
    ## 72                                    LMN-201                    Oral
    ## 73                                      MET-2                    Oral
    ## 74                                   MGB-BP-3      Oral, Not absorbed
    ## 75                                    RBX2660                   Enema
    ## 76                                    RBX7455                    Oral
    ## 77                               Ridinilazole      Oral, Not absorbed
    ## 78                                    SER-109                    Oral
    ## 79                                    SYN-004                    Oral
    ## 80                                      VE303                    Oral

**Oral** is the most preferred route of administration, seconded by
**IV**. </br>

``` r
# CLINICAL DEVELOPMENT PHASE OF PRODUCTS
Phase <- AMR_PRODUCTS %>% select( Product.name,R.D.phase, Product.type)
Phase <- distinct(Phase)
Phase <- as.data.frame(Phase)
ggplot(Phase, aes(x = Product.type, fill = R.D.phase )) + geom_bar(position = "dodge", color="black") + labs(title = "ANTIMICROBIAL PRODUCTS - CLINICAL DEVELOPMENT PHASE", fill = "Phase", x= "Product type")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
#Antibiotics - Clinical phase
AB_PHASE <- Antibiotics %>% select(Product.name, R.D.phase)
AB_PHASE <- distinct(AB_PHASE)
AB_PHASE
```

    ##                                  Product.name       R.D.phase
    ## 1                                    Afabicin        Phase II
    ## 2                                    ARX-1796         Phase I
    ## 3                                   Benapenem       Phase III
    ## 4                                     BWC0977         Phase I
    ## 5                                  Delpazolid        Phase II
    ## 6                     Durlobactam + sulbactam       Phase III
    ## 7                                    EBL-1003         Phase I
    ## 8                   Enmetazobactam + cefepime       Phase III
    ## 9                       ETX0282 + cefpodoxime         Phase I
    ## 10                                Gepotidacin       Phase III
    ## 11                                   KBP-7072         Phase I
    ## 12                                      MRX-8         Phase I
    ## 13                     Nacubactam + meropenem         Phase I
    ## 14                              Nafithromycin       Phase III
    ## 15                          QPX7728 + QPX2014         Phase I
    ## 16                          QPX7728 + QPX2015         Phase I
    ## 17                                    QPX9003         Phase I
    ## 18                                     RG6006         Phase I
    ## 19                              Solithromycin Preregistration
    ## 20                                    SPR-206         Phase I
    ## 21 Sulopenem, sulopenem etzadroxil/probenecid       Phase III
    ## 22                   Taniborbactam + cefepime       Phase III
    ## 23                                   TNP-2092        Phase II
    ## 24                                   TNP-2198        Phase II
    ## 25                                     TXA709         Phase I
    ## 26                     VNRX-7145 + ceftibuten         Phase I
    ## 27             XNW4107 +imipenem \n+ cilastin         Phase I
    ## 28                      Zidebactam + cefepime         Phase I
    ## 29                               Zoliflodacin       Phase III
    ## 30                                 GSK3036656        Phase II
    ## 31                                    BTZ-043        Phase II
    ## 32                                 GSK2556286         Phase I
    ## 33                                 Macozinone         Phase I
    ## 34                                 OPC-167832        Phase II
    ## 35                                  Sutezolid        Phase II
    ## 36                                   TBA-7371        Phase II
    ## 37                                   TBAJ-587         Phase I
    ## 38                                   TBAJ-876         Phase I
    ## 39                                    TBI-166         Phase I
    ## 40                                    TBI-223         Phase I
    ## 41                                  Telacebec        Phase II
    ## 42                                    CRS3123        Phase II
    ## 43                                    DNV3837        Phase II
    ## 44                               Ibezapolstat        Phase II
    ## 45                                   MGB-BP-3        Phase II
    ## 46                               Ridinilazole       Phase III

``` r
#Non-traditional - Clinical phase
NT_PHASE <- Non_traditional %>% select(Product.name, R.D.phase)
NT_PHASE <- distinct(NT_PHASE)
NT_PHASE
```

    ##               Product.name       R.D.phase
    ## 1            Bacteriophage       Phase III
    ## 2                  9MW1411         Phase I
    ## 3                    AB103 Preregistration
    ## 4                    ALS-4         Phase I
    ## 5                  AP-PA02        Phase II
    ## 6                   AR-301       Phase III
    ## 7                   AR-302        Phase II
    ## 8                  BX004-A        Phase II
    ## 9                    CAL02         Phase I
    ## 10                  CF-301       Phase III
    ## 11 Ftortiazinon + cefepime        Phase II
    ## 12              GSK3882347         Phase I
    ## 13                LBP-EC01         Phase I
    ## 14                 LMN-101        Phase II
    ## 15               LSVT-1701        Phase II
    ## 16                  OligoG        Phase II
    ## 17                   Phage        Phase II
    ## 18                Rhu-pGSN        Phase II
    ## 19               SVT-1C469         Phase I
    ## 20                 TRL1068         Phase I
    ## 21                  YPT-01        Phase II
    ## 22              BVL-GSK098         Phase I
    ## 23                   ART24         Phase I
    ## 24                   BB128 Preregistration
    ## 25                   CP101        Phase II
    ## 26                  DAV132        Phase II
    ## 27                   IM-01        Phase II
    ## 28                 LMN-201         Phase I
    ## 29                   MET-2         Phase I
    ## 30                 RBX2660       Phase III
    ## 31                 RBX7455         Phase I
    ## 32                 SER-109       Phase III
    ## 33                 SYN-004        Phase II
    ## 34                   VE303        Phase II

Information on the clinical development phase of the antimicrobial
products is visualized and tabulated.

**Antibiotics:** Phase I - 22; Phase II - 14; Phase III - 9;
Pre-registration - 1

**Non-traditional:** Phase I - 12; Phase II - 15; Phase III - 5;
Pre-registration - 2

``` r
#ANTIMICROBIALS INDICATION
AB_Indication <- Antibiotics %>% select(Product.name,Indications)
AB_Indication <- distinct(AB_Indication)
NT_Indication <- Non_traditional %>% select(Product.name,Indications)
NT_Indication <- distinct(NT_Indication)
#ANTIBIOTICS INDICATION
AB_Indication
```

    ##                                  Product.name
    ## 1                                    Afabicin
    ## 2                                    ARX-1796
    ## 3                                   Benapenem
    ## 4                                     BWC0977
    ## 5                                  Delpazolid
    ## 6                     Durlobactam + sulbactam
    ## 7                                    EBL-1003
    ## 8                   Enmetazobactam + cefepime
    ## 9                       ETX0282 + cefpodoxime
    ## 10                                Gepotidacin
    ## 11                                   KBP-7072
    ## 12                                      MRX-8
    ## 13                     Nacubactam + meropenem
    ## 14                              Nafithromycin
    ## 15                          QPX7728 + QPX2014
    ## 16                          QPX7728 + QPX2015
    ## 17                                    QPX9003
    ## 18                                     RG6006
    ## 19                              Solithromycin
    ## 20                                    SPR-206
    ## 21 Sulopenem, sulopenem etzadroxil/probenecid
    ## 22                   Taniborbactam + cefepime
    ## 23                                   TNP-2092
    ## 24                                   TNP-2198
    ## 25                                     TXA709
    ## 26                     VNRX-7145 + ceftibuten
    ## 27             XNW4107 +imipenem \n+ cilastin
    ## 28                      Zidebactam + cefepime
    ## 29                               Zoliflodacin
    ## 30                                 GSK3036656
    ## 31                                    BTZ-043
    ## 32                                 Delpazolid
    ## 33                                 GSK2556286
    ## 34                                 Macozinone
    ## 35                                 OPC-167832
    ## 36                                  Sutezolid
    ## 37                                   TBA-7371
    ## 38                                   TBAJ-587
    ## 39                                   TBAJ-876
    ## 40                                    TBI-166
    ## 41                                    TBI-223
    ## 42                                  Telacebec
    ## 43                                    CRS3123
    ## 44                                    DNV3837
    ## 45                               Ibezapolstat
    ## 46                                   MGB-BP-3
    ## 47                               Ridinilazole
    ##                                                                                     Indications
    ## 1                                                    ABSSI (caused by S. aureus), osteomyelitis
    ## 2                                                                      Gram-negative infections
    ## 3                                                                                          cUTI
    ## 4                                          Gram-negative and Gram-positive bacterial infections
    ## 5                                  Pulmonary tuberculosis, \nGram-positive bacterial infections
    ## 6                                                                         A. baumannii HAP, VAP
    ## 7                                                                          Bacterial infections
    ## 8                                                                                      cUTI, AP
    ## 9                 Active against ESBL, OXA-48 and KPC, but not\n MBL-producing Enterobacterales
    ## 10                                                  Uncomplicated urogenital gonorrhea and uUTI
    ## 11                Multi-drug resistant bacterial infections including\n (CABP, DFI, cUTI, cIAI)
    ## 12                                                                     Gram-negative infections
    ## 13                                                                         Bacterial infections
    ## 14                                                 CABP (caused by S. pneumoniae and S. aureus)
    ## 15                                                                     Gram-negative infections
    ## 16                                                                     Gram-negative infections
    ## 17                                                                     Gram-negative infections
    ## 18                                                                     Acinetobacter infections
    ## 19                                                                                S. pneumoniae
    ## 20                                                                     Acinetobacter infections
    ## 21                                                                              uUTI, cUTI, IAI
    ## 22                                                                                  cUTI and AP
    ## 23                            Gastrointestinal infections \n(caused by S. aureus and H. pylori)
    ## 24 Helicobacter pylori infection, Clostridioides difficile infection, \nand bacterial vaginosis
    ## 25                                                                                         MRSA
    ## 26                                                                     Gram-negative infections
    ## 27                                                                                    HABP/VABP
    ## 28                                                           Gram-negative bacterial infections
    ## 29                                                                 Uncomplicated N. gonorrhoeae
    ## 30                                                                       Pulmonary tuberculosis
    ## 31                                                                                 Tuberculosis
    ## 32                                 Pulmonary tuberculosis,\n Gram-positive bacterial infections
    ## 33                                                                                 Tuberculosis
    ## 34                                                                       Pulmonary tuberculosis
    ## 35                                                                       Pulmonary tuberculosis
    ## 36                                 Pulmonary tuberculosis, \nGram-positive bacterial infections
    ## 37                                                                                 Tuberculosis
    ## 38                                                                                 Tuberculosis
    ## 39                                                                                 Tuberculosis
    ## 40                                                                                 Tuberculosis
    ## 41                                                                                 Tuberculosis
    ## 42                                                                       Pulmonary tuberculosis
    ## 43                                                                      C. difficile infections
    ## 44                                                                      C. difficile infections
    ## 45                                                                      C. difficile infections
    ## 46                                                             C. difficile-associated diarrhea
    ## 47                                                                      C. difficile infections

``` r
#NON-TRADITIONALS INDICATION
NT_Indication
```

    ##               Product.name
    ## 1            Bacteriophage
    ## 2                  9MW1411
    ## 3                    AB103
    ## 4                    ALS-4
    ## 5                  AP-PA02
    ## 6                   AR-301
    ## 7                   AR-302
    ## 8                  BX004-A
    ## 9                    CAL02
    ## 10                  CF-301
    ## 11 Ftortiazinon + cefepime
    ## 12              GSK3882347
    ## 13                LBP-EC01
    ## 14                 LMN-101
    ## 15               LSVT-1701
    ## 16                  OligoG
    ## 17                   Phage
    ## 18                Rhu-pGSN
    ## 19               SVT-1C469
    ## 20                 TRL1068
    ## 21                  YPT-01
    ## 22              BVL-GSK098
    ## 23                   ART24
    ## 24                   BB128
    ## 25                   CP101
    ## 26                  DAV132
    ## 27                   IM-01
    ## 28                 LMN-201
    ## 29                   MET-2
    ## 30                 RBX2660
    ## 31                 RBX7455
    ## 32                 SER-109
    ## 33                 SYN-004
    ## 34                   VE303
    ##                                                                       Indications
    ## 1                                                 Gram positive and Gram negative
    ## 2                                                                       S. aureus
    ## 3  S. aureus necrotizing soft tissue infection,\nacute kidney injury, peritonitis
    ## 4                                                                       S. aureus
    ## 5                                       CF subjects with P. aeruginosa infections
    ## 6                                                              S. aureus HAP, VAP
    ## 7                                                              S. aureus HAP, VAP
    ## 8                                       CF subjects with P. aeruginosa infections
    ## 9                                                   S. pneumonia severe pneumonia
    ## 10                                                           S. aureus bacteremia
    ## 11                                                             P. aeruginosa cUTI
    ## 12                                                                     E.coli UTI
    ## 13                                                                        E. coli
    ## 14                                              E. coli and  C. jejuni infections
    ## 15                                                           S. aureus bacteremia
    ## 16                                                                Cystic fibrosis
    ## 17                                                                        E. coli
    ## 18                                                               Hospitalized CAP
    ## 19                                                                      H. pylori
    ## 20                           Gram-negative and Gram-positive bacterial infections
    ## 21                                      CF subjects with P. aeruginosa infections
    ## 22                                                                   Tuberculosis
    ## 23                                                        C. difficile infections
    ## 24                                              Recurring C. difficile infections
    ## 25                                              Recurring C. difficile infections
    ## 26                                                        C. difficile infections
    ## 27                                                        C. difficile infections
    ## 28                                                        C. difficile infections
    ## 29                                              Recurring C. difficile infections
    ## 30                                                                   C. difficile
    ## 31                                              Recurring C. difficile infections
    ## 32                                                        C. difficile infections
    ## 33                                                        C. difficile infections
    ## 34                                              Recurring C. difficile infections

``` r
##(ggplot(data = AB_Indication) + geom_bar(mapping = aes( fill = Product.name, y = Indications), color="black") + labs(title = "ANTIBIOTICS INDICATION", y = "Indications", fill= "Product")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"), legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=5))+theme(axis.text.x = element_text(angle = 90))

##(ggplot(data = NT_Indication) + geom_bar(mapping = aes( fill = Product.name, y = Indications), color="black") + labs(title = "NON-TRADITIONAL PRODUCTS INDICATION", y = "Indications", fill= "Product")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"),legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=5))+theme(axis.text.x = element_text(angle = 90))
```

An **antimicrobial indication** refers to specific condition or
infection for which an antimicrobial agent is recommended or prescribed.

The indication for antibiotics and non-traditional products is tabulated
individually. Plots were not visualized due to spatial inconvenience.
</br>

``` r
#PRIORITY PATHOGENS AND THEIR THREAT LEVELS
priority <- AMR_PRODUCTS %>% select(Pathogen.name,Pathogen.activity)
priority <- distinct(priority)
priority <- as.data.frame(priority)
ggplot(data = priority) + geom_bar(mapping = aes( fill = Pathogen.name, x = Pathogen.activity), color="black") + labs(title = "PRIORITY PATHOGENS AND THEIR THREAT LEVELS", x = "Priority level", fill= "Pathogen")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"),legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=5))+theme(axis.text.x = element_text(angle = 10))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
priority
```

    ##                       Pathogen.name           Pathogen.activity
    ## 1          Streptococcus pneumoniae    Other priority pathogens
    ## 2             Staphylococcus aureus    Other priority pathogens
    ## 3            Pseudomonas aeruginosa Critical priority pathogens
    ## 4          Other priority pathogens    Other priority pathogens
    ## 5             Neisseria gonnorhoeae    Other priority pathogens
    ## 6               Helicobacter pylori    Other priority pathogens
    ## 7  Gram-positive priority pathogens    Other priority pathogens
    ## 8              Enterococcus faecium    Other priority pathogens
    ## 9                  Enterobacterales Critical priority pathogens
    ## 10      Critical priority pathogens Critical priority pathogens
    ## 11               Campylobacter spp.    Other priority pathogens
    ## 12  All critical priority pathogens Critical priority pathogens
    ## 13          Acinetobacter baumannii Critical priority pathogens
    ## 14       Mycobacterium tuberculosis                         N/A
    ## 15         Clostridioides difficile                         N/A


The threat level of priority pathogens were visualized and tabulated.

**Critical priority pathogens** pose the most urgent threat due to their
resistant towards multiple antimicrobial products. Pathogens like
***Pseudomonas aeruginosa, Enterobacterales, Acinetobacter baumannii***
falls under this category. </br>

``` r
#ACTIVITY OF PRODUCTS AGAINST PRIORITY PATHOGEN
Activity <- AMR_PRODUCTS %>% select( Active.against.priority.pathogens., Product.type)
Activity <- lapply(Activity, function(x) ifelse(x == "N/A", NA, x))
Activity <- as.data.frame(Activity)
Activity <- na.omit(Activity)
ggplot(Activity, aes(x = Product.type, fill = Active.against.priority.pathogens. )) + geom_bar(position = "dodge", color="black")  + labs(title = "ANTIMICROBIAL PRODUCT ACTIVE AGAINST PATHOGENS", fill = "Active", x= "Product")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
#ANTIBIOTICS ACTIVE AGAINST PRIORITY PATHOGENS
AB_Activity <- Antibiotics %>% select(Product.name,Pathogen.name,Active.against.priority.pathogens.)
AB_Activity <- distinct(AB_Activity)
AB_Activity <- lapply(AB_Activity, function(x) ifelse(x == "N/A", NA, x))
AB_Activity <- as.data.frame(AB_Activity)
AB_Activity <- na.omit(AB_Activity)
ggplot(AB_Activity, aes(x =Active.against.priority.pathogens., fill = Product.name )) + geom_bar(position = "dodge", color="black") + facet_wrap(~ Pathogen.name) + labs(title = "ANTIBIOTICS ACTIVE AGAINST PRIORITY PATHOGENS", x = "Activity", fill= "Product")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"),legend.position = "bottom", legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=4))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-9-2.png)

``` r
#NON-TRADITIONALS ACTIVE AGAINST PRIORITY PATHOGENS
NT_Activity <- Non_traditional %>% select(Product.name,Pathogen.name,Active.against.priority.pathogens.)
NT_Activity <- distinct(NT_Activity)
NT_Activity <- lapply(NT_Activity, function(x) ifelse(x == "N/A", NA, x))
NT_Activity <- as.data.frame(NT_Activity)
NT_Activity <- na.omit(NT_Activity)
ggplot(NT_Activity, aes(x =Active.against.priority.pathogens., fill = Product.name )) + geom_bar(position = "dodge", color="black") + facet_wrap(~ Pathogen.name) + labs(title = "NON-TRADITIONAL PRODUCTS ACTIVE AGAINST PRIORITY PATHOGENS", x = "Activity", fill= "Product")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"),legend.position = "bottom",legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=4))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-9-3.png)

Antibiotics and non-traditional products that are active against
priority pathogens are visualized.

Antibiotics **ARF-1796,TXA709, QPX7728+QPX2014** are active against most
of the pathogens. **Afabicin** is inactive to be the most inactive.

Non-traditional products **TRL1068,phage,bacteriophage and BX004-A** are
found to be predominantly active against most of the pathogens. </br>

``` r
#ANTIBACTERIAL CLASS
AB_product_class <- Antibiotics %>% select(Product.name,Antibacterial.class)
AB_product_class <- distinct(AB_product_class)
AB_product_class <- as.data.frame(AB_product_class)
NT_product_class <- Non_traditional %>% select(Product.name,Antibacterial.class)
NT_product_class <- distinct(NT_product_class)
NT_product_class <- as.data.frame(NT_product_class)
#ANTIBIOTICS
ggplot(data = AB_product_class) + geom_bar(mapping = aes( fill = Product.name, y = Antibacterial.class), color="black") + labs(title = "ANTIBIOTICS - CLASS", y = "Class", fill= "Product")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"),legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=3.5))+theme(axis.text.x = element_text(angle = 90))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
AB_product_class
```

    ##                                  Product.name
    ## 1                                    Afabicin
    ## 2                                    ARX-1796
    ## 3                                   Benapenem
    ## 4                                     BWC0977
    ## 5                                  Delpazolid
    ## 6                     Durlobactam + sulbactam
    ## 7                                    EBL-1003
    ## 8                   Enmetazobactam + cefepime
    ## 9                       ETX0282 + cefpodoxime
    ## 10                                Gepotidacin
    ## 11                                   KBP-7072
    ## 12                                      MRX-8
    ## 13                     Nacubactam + meropenem
    ## 14                              Nafithromycin
    ## 15                          QPX7728 + QPX2014
    ## 16                          QPX7728 + QPX2015
    ## 17                                    QPX9003
    ## 18                                     RG6006
    ## 19                              Solithromycin
    ## 20                                    SPR-206
    ## 21 Sulopenem, sulopenem etzadroxil/probenecid
    ## 22                   Taniborbactam + cefepime
    ## 23                                   TNP-2092
    ## 24                                   TNP-2198
    ## 25                                     TXA709
    ## 26                     VNRX-7145 + ceftibuten
    ## 27             XNW4107 +imipenem \n+ cilastin
    ## 28                      Zidebactam + cefepime
    ## 29                               Zoliflodacin
    ## 30                                 GSK3036656
    ## 31                                    BTZ-043
    ## 32                                 GSK2556286
    ## 33                                 Macozinone
    ## 34                                 OPC-167832
    ## 35                                  Sutezolid
    ## 36                                   TBA-7371
    ## 37                                   TBAJ-587
    ## 38                                   TBAJ-876
    ## 39                                    TBI-166
    ## 40                                    TBI-223
    ## 41                                  Telacebec
    ## 42                                    CRS3123
    ## 43                                    DNV3837
    ## 44                               Ibezapolstat
    ## 45                                   MGB-BP-3
    ## 46                               Ridinilazole
    ##                                     Antibacterial.class
    ## 1                                        FabI inhibitor
    ## 2                                    DBO-BLI + ?-lactam
    ## 3                                            Carbapenem
    ## 4                                         Topoisomerase
    ## 5                                         Oxazolidinone
    ## 6     DBO-BLI /PBP2 binder + ?-lactam-BLI/PBP1,3 binder
    ## 7                                        Aminoglycoside
    ## 8                          ?-lactam BLI + cephalosporin
    ## 9                   DBO-BLI/PBP2 binder + cephalosporin
    ## 10      Topoisomerase Inhibitors (Triazaacenaphthylene)
    ## 11                                         Tetracycline
    ## 12                                            Polymyxin
    ## 13                  DBO-BLI/PBP2 binder + cephalosporin
    ## 14                                            Macrolide
    ## 15                           Boronate-BLI + undisclosed
    ## 16                           Boronate-BLI + undisclosed
    ## 17                                            Polymyxin
    ## 18                                  Macrocyclic peptide
    ## 19                                            Macrolide
    ## 20                                            Polymyxin
    ## 21                                     ?-Lactam (penem)
    ## 22              Boronate BLI + ?-lactam (cephalosporin)
    ## 23                       Rifamycin-quinolizinone hybrid
    ## 24                   rifamycin-nitroimidazole conjugate
    ## 25                    Diflurobenzamide (FtsZ inhibitor)
    ## 26                         Boronate-BLI + cephalosporin
    ## 27           BLI + carbapenem + \ndegradation inhibitor
    ## 28                 DBO-BLI/ PBP2 binder + cephalosporin
    ## 29     Topoisomerase Inhibitors (Spiropyrimidenetrione)
    ## 30                         Leu RS inhibitor (oxaborole)
    ## 31                    DprE1 inhibitor (benzothiazinone)
    ## 32                                          Undisclosed
    ## 33                    DprE1 inhibitor (Benzothiazinone)
    ## 34             DprE1 inhibitor (3,4-dihydrocarbostyril)
    ## 35                                        Oxazolidinone
    ## 36                          DprE1 inhibitor (azaindole)
    ## 37                                      Diarylquinoline
    ## 38                                      Diarylquinoline
    ## 39               Riminophenazine (clofazimine-analogue)
    ## 40                                        Oxazolidinone
    ## 41                                Imidazopyridine amide
    ## 42  Diaryldiamine (Methionyl-tRNA synthetase inhibitor)
    ## 43                       Oxazolidinone-quinolone hybrid
    ## 44 Substituted guanine (DNA polymerae IIIIC inhibitor))
    ## 45                 DNA minor groove binder (distamycin)
    ## 46                                    Bis-benzimidazole

``` r
#NON-TRADITIONALS
ggplot(data = NT_product_class) + geom_bar(mapping = aes( fill = Product.name, y = Antibacterial.class), color="black") + labs(title = "NON-TRADITIONAL - CLASS", y = "Class", fill= "Product")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"),legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=5))+theme(axis.text.x = element_text(angle = 90))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-10-2.png)

``` r
NT_product_class
```

    ##               Product.name
    ## 1            Bacteriophage
    ## 2                  9MW1411
    ## 3                    AB103
    ## 4                    ALS-4
    ## 5                  AP-PA02
    ## 6                   AR-301
    ## 7                   AR-302
    ## 8                  BX004-A
    ## 9                    CAL02
    ## 10                  CF-301
    ## 11 Ftortiazinon + cefepime
    ## 12              GSK3882347
    ## 13                LBP-EC01
    ## 14                 LMN-101
    ## 15               LSVT-1701
    ## 16                  OligoG
    ## 17                   Phage
    ## 18                Rhu-pGSN
    ## 19               SVT-1C469
    ## 20                 TRL1068
    ## 21                  YPT-01
    ## 22              BVL-GSK098
    ## 23                   ART24
    ## 24                   BB128
    ## 25                   CP101
    ## 26                  DAV132
    ## 27                   IM-01
    ## 28                 LMN-201
    ## 29                   MET-2
    ## 30                 RBX2660
    ## 31                 RBX7455
    ## 32                 SER-109
    ## 33                 SYN-004
    ## 34                   VE303
    ##                                                   Antibacterial.class
    ## 1                                                       Bacteriophage
    ## 2                                                 Monoclonal antibody
    ## 3                Antagonist of superantigen exotoxins and CD28 T-cell
    ## 4                             Staphyloxanthin biosynthesis inhibition
    ## 5                                                       Bacteriophage
    ## 6                                    anti-S. aureus IgM monoclonal Ab
    ## 7                                    anti-S. aureus IgG monoclonal Ab
    ## 8                                                       Bacteriophage
    ## 9          Broad spectrum anti-toxin liposomal agent and nanoparticle
    ## 10                                                    Phage endolysin
    ## 11                    Type III secretion system inhibition + cefepime
    ## 12                                                 Undisclosed (FimH)
    ## 13                                         CRISPR-Cas3 enhanced phage
    ## 14                            Monoclonal Ab like\nrecombinant protein
    ## 15                                                    Phage endolysin
    ## 16                        Alginate oligosaccharide (G-block) fragment
    ## 17                                                      Bacteriophage
    ## 18                          Recombinant human plasma gelsolin protein
    ## 19                                        Live biotherapeutic product
    ## 20                                                           Antibody
    ## 21                                                      Bacteriophage
    ## 22      Amido piperidine (inactivation of TetR-like repressor, EthR2)
    ## 23                                        Live biotherapeutic product
    ## 24                                        Live biotherapeutic product
    ## 25                                        Live biotherapeutic product
    ## 26     Antibiotic inactivator and protective colon-targeted adsorbent
    ## 27                                   anti-C. difficile polcyclonal Ab
    ## 28 Phage endolysin and three toxin-binding proteins (5D, E3, and 7F)s
    ## 29                                        Live biotherapeutic product
    ## 30                                        Live biotherapeutic product
    ## 31                                        Live biotherapeutic product
    ## 32                                        Live biotherapeutic product
    ## 33                                             Antibiotic inactivator
    ## 34                                        Live biotherapeutic product

Antibacterial class of antibiotics and non-traditional products are
visualized and tabulated.

Major **non-traditional** products lie under the class **Live
bio-therapeutic product**. </br>

``` r
#DEVELOPERS
Developer <- AMR_PRODUCTS %>% select(Product.name,Developer)
Developer <- distinct(Developer)
Developer <- as.data.frame(Developer)
Developer
```

    ##                                  Product.name
    ## 1                               Bacteriophage
    ## 2                                     9MW1411
    ## 3                                       AB103
    ## 4                                    Afabicin
    ## 5                                       ALS-4
    ## 6                                     AP-PA02
    ## 7                                      AR-301
    ## 8                                      AR-302
    ## 9                                    ARX-1796
    ## 10                                  Benapenem
    ## 11                                    BWC0977
    ## 12                                    BX004-A
    ## 13                                      CAL02
    ## 14                                     CF-301
    ## 15                                 Delpazolid
    ## 16                    Durlobactam + sulbactam
    ## 17                                   EBL-1003
    ## 18                  Enmetazobactam + cefepime
    ## 19                      ETX0282 + cefpodoxime
    ## 20                    Ftortiazinon + cefepime
    ## 21                                Gepotidacin
    ## 22                                 GSK3882347
    ## 23                                   KBP-7072
    ## 24                                   LBP-EC01
    ## 25                                    LMN-101
    ## 26                                  LSVT-1701
    ## 27                                      MRX-8
    ## 28                     Nacubactam + meropenem
    ## 29                              Nafithromycin
    ## 30                                     OligoG
    ## 31                                      Phage
    ## 32                          QPX7728 + QPX2014
    ## 33                          QPX7728 + QPX2015
    ## 34                                    QPX9003
    ## 35                                     RG6006
    ## 36                                   Rhu-pGSN
    ## 37                              Solithromycin
    ## 38                                    SPR-206
    ## 39 Sulopenem, sulopenem etzadroxil/probenecid
    ## 40                                  SVT-1C469
    ## 41                   Taniborbactam + cefepime
    ## 42                                   TNP-2092
    ## 43                                   TNP-2198
    ## 44                                    TRL1068
    ## 45                                     TXA709
    ## 46                     VNRX-7145 + ceftibuten
    ## 47             XNW4107 +imipenem \n+ cilastin
    ## 48                                     YPT-01
    ## 49                      Zidebactam + cefepime
    ## 50                               Zoliflodacin
    ## 51                                 GSK3036656
    ## 52                                    BTZ-043
    ## 53                                 BVL-GSK098
    ## 54                                 GSK2556286
    ## 55                                 Macozinone
    ## 56                                 OPC-167832
    ## 57                                  Sutezolid
    ## 58                                   TBA-7371
    ## 59                                   TBAJ-587
    ## 60                                   TBAJ-876
    ## 61                                    TBI-166
    ## 62                                    TBI-223
    ## 63                                  Telacebec
    ## 64                                      ART24
    ## 65                                      BB128
    ## 66                                      CP101
    ## 67                                    CRS3123
    ## 68                                     DAV132
    ## 69                                    DNV3837
    ## 70                               Ibezapolstat
    ## 71                                      IM-01
    ## 72                                    LMN-201
    ## 73                                      MET-2
    ## 74                                   MGB-BP-3
    ## 75                                    RBX2660
    ## 76                                    RBX7455
    ## 77                               Ridinilazole
    ## 78                                    SER-109
    ## 79                                    SYN-004
    ## 80                                      VE303
    ##                                                                                                  Developer
    ## 1                                                                    Tashkent Pediatric Medical\nInstitute
    ## 2                                                                                       Mabwell Bioscience
    ## 3                                                                                                 Atox Bio
    ## 4                                                                              Debiopharm International SA
    ## 5                                                                                            Aptorum Group
    ## 6                                                                                   Armata Pharmaceuticals
    ## 7                                                                             Aridis Pharmaceuticals, Inc.
    ## 8                                                                           Aridis\nAstraZeneca/ MedImmune
    ## 9                                                                            Pfizer/ Arixa Pharmaceuticals
    ## 10                                                               Sichuan Pharmaceutical Holdings Group Ltd
    ## 11                                                                                       Bugworks Research
    ## 12                                                                                                   BiomX
    ## 13                                                                   Eagle Pharmaceuticals\n(Combioxin SA)
    ## 14                                                                                              Contrafect
    ## 15                                                              LegoChem Biosciences Inc., HaiHe Biopharma
    ## 16                                                                               Entasis Therapeutics Inc.
    ## 17                                                                                                 Juvabis
    ## 18                                                                                    Allecra therapeutics
    ## 19                                                                               Entasis Therapeutics Inc.
    ## 20                                            Gamaleya Research Institute of Epidemiology and Microbiology
    ## 21                                                                                                     GSK
    ## 22                                                                                                     GSK
    ## 23                                                      KBP BioSciences Pharmaceutical Technical Co., Ltd.
    ## 24                                                                                        Locus Bioscience
    ## 25                                                                                        Lumen Bioscience
    ## 26                                                                   Roivant Sciences/iNtRON Biotechnology
    ## 27                                                                                                  MicuRx
    ## 28                                                                                             Meiji Seika
    ## 29                                                                                          Wockhardt Ltd.
    ## 30                                                                                           AlgiPharma AS
    ## 31                                                                            Adaptive Phage\nTherapeutics
    ## 32                                                                                          Qpex Biopharma
    ## 33                                                                                          Qpex Biopharma
    ## 34                                                                                          Qpex Biopharma
    ## 35                                                                                                   Roche
    ## 36                                                                                   BioAegis Therapeutics
    ## 37                                                                                Fujfilm Toyama\nChemical
    ## 38                                                                                      Spero Therapeutics
    ## 39                                                                                     Iterum Therapeutics
    ## 40                                                                                                Servatus
    ## 41                                                                    VenatoRx Pharmaceuticals, Inc/ GARDP
    ## 42                                                                                     Tennor Therapeutics
    ## 43                                                                                     TenNor Therapeutics
    ## 44                                                                                      Trellis Bioscience
    ## 45                                                                                   TAXIS Pharmaceuticals
    ## 46                                                                                                Venatorx
    ## 47                                                                                                Sinovent
    ## 48                                                                    Felix Biotechnology/ Yale University
    ## 49                                                                                          Wockhardt Ltd.
    ## 50                                                                        Entasis Therapeutics Inc./ GARDP
    ## 51                                                                                     GlaxoSmithKline PLC
    ## 52               University of Munich; Hans Kn\xf6ll Institute, Jena; German Center for Infection Research
    ## 53                                                                                          Bioversys/ GSK
    ## 54                                               GSK. TB Drug Accelarator/ Bill & Melinda Gates Foundation
    ## 55                                     Innovative Medicines for Tuberculosis Foundation and Nearmedic Plus
    ## 56                                                                         Otsuka Pharmaceutical Co., Ltd.
    ## 57                                                                                   TB Alliance/ Sequella
    ## 58 TB Alliance/ Bill & Melinda Gates Medical Research Institute/ Foundation for Neglected Disease Research
    ## 59                                                                                             TB Alliance
    ## 60                                                                                             TB Alliance
    ## 61         Institute of Materia Medica, Chinese Academy of Medical Sciences & Peking Union Medical College
    ## 62                                                                Institute of Materia Medica/ TB Alliance
    ## 63                                                                                              Qurient Co
    ## 64                                                                                    Artugen Therapeutics
    ## 65                                                                                               BiomeBank
    ## 66                                                                                       Finch Theraputics
    ## 67                                         Crestone/ National Institute of Allergy and Infectious Diseases
    ## 68                                                                                             Da Volterra
    ## 69                                                                                                 Deinove
    ## 70                                                                                   Acurx Pharmaceuticals
    ## 71                                                                                               ImmuniMed
    ## 72                                                                                        Lumen Bioscience
    ## 73                                                                                        NuBiyota/ Takeda
    ## 74                                                                                      MGB Biopharma Ltd.
    ## 75                                                                                 Ferring Pharmaceuticals
    ## 76                                                                                      Ferring (Rebiotix)
    ## 77                                                                                Summit Therapeutics Inc.
    ## 78                                                                                       Seres Theraputics
    ## 79                                                                                     Synthetic Biologics
    ## 80                                                                                     Vedanta Biosciences

The developers of the antimicrobial products are tabulated. </br>

``` r
#INNOVATIVENESS OF THE PRODUCTS
Innovativeness <- AMR_PRODUCTS %>% select(Product.name,Innovative.)
Innovativeness <- distinct(Innovativeness)
Innovativeness <- lapply(Innovativeness, function(x) ifelse(x == "N/A", NA, x))
Innovativeness <- as.data.frame(Innovativeness)
Innovativeness <- na.omit(Innovativeness)
ggplot(data = Innovativeness) + geom_bar(mapping = aes( fill = Product.name, x = Innovative.), color="black") + labs(title = "INNOVATIVE PRODUCT", X = "Innovativenss", fill= "Product")+ theme_classic()+  theme(plot.title  = element_text(face = "bold"),legend.key.size = unit(0.2,"cm"), legend.text = element_text(size=5))
```

![](STAGE2_DATA-VISUALIZATION-OF-ANTIMICROBIAL-PRODUCTS_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
Innovativeness
```

    ##                                  Product.name  Innovative.
    ## 4                                    Afabicin          Yes
    ## 9                                    ARX-1796           No
    ## 10                                  Benapenem           No
    ## 11                                    BWC0977 Inconclusive
    ## 15                                 Delpazolid           No
    ## 16                    Durlobactam + sulbactam           No
    ## 17                                   EBL-1003           No
    ## 18                  Enmetazobactam + cefepime           No
    ## 19                      ETX0282 + cefpodoxime           No
    ## 21                                Gepotidacin          Yes
    ## 23                                   KBP-7072           No
    ## 27                                      MRX-8           No
    ## 28                     Nacubactam + meropenem           No
    ## 29                              Nafithromycin           No
    ## 32                          QPX7728 + QPX2014 Inconclusive
    ## 33                          QPX7728 + QPX2015 Inconclusive
    ## 34                                    QPX9003 Inconclusive
    ## 35                                     RG6006 Inconclusive
    ## 37                              Solithromycin           No
    ## 38                                    SPR-206           No
    ## 39 Sulopenem, sulopenem etzadroxil/probenecid           No
    ## 41                   Taniborbactam + cefepime          Yes
    ## 42                                   TNP-2092           No
    ## 43                                   TNP-2198           No
    ## 45                                     TXA709          Yes
    ## 46                     VNRX-7145 + ceftibuten          Yes
    ## 47             XNW4107 +imipenem \n+ cilastin           No
    ## 49                      Zidebactam + cefepime           No
    ## 50                               Zoliflodacin          Yes
    ## 51                                 GSK3036656          Yes
    ## 52                                    BTZ-043          Yes
    ## 54                                 GSK2556286          Yes
    ## 55                                 Macozinone          Yes
    ## 56                                 OPC-167832          Yes
    ## 57                                  Sutezolid           No
    ## 58                                   TBA-7371          Yes
    ## 59                                   TBAJ-587           No
    ## 60                                   TBAJ-876           No
    ## 61                                    TBI-166           No
    ## 62                                    TBI-223           No
    ## 63                                  Telacebec          Yes
    ## 67                                    CRS3123          Yes
    ## 69                                    DNV3837 Inconclusive
    ## 70                               Ibezapolstat          Yes
    ## 74                                   MGB-BP-3          Yes
    ## 77                               Ridinilazole          Yes

Among the 80 products, 17 are found to be innovative.

**Innovative products** Afabicin, Gepotidacin, Taniborbactam + cefepime,
TXA709, VNRX-7145 + ceftibuten, Zoliflodacin, GSK3036656, BTZ-043,
GSK2556286, Macozinone, OPC-167832, Telacebec, CRS3123, Ibezapolstat,
MGB-BP-3, Ridinilazole.

**TXA709** - *Active against most of the priority pathogens.*

------------------------------------------------------------------------

 ***THE DATA WAS SEGMENTED INTO SUBSETS TO REMOVE DUPLICATES AND
OBTAIN MEANINGFUL RESULTS***

**Link for infographics:** *https://github.com/BLakshana/Hackbio_cancer_internship/blob/main/STAGE2_DATA-VISUALIZATION%20OF%20ANTIMICROBIAL%20PRODUCTS%20IN%20CLINICAL%20DEVELOPMENT%20(WHO-Nov%202021)/STAGE_2_INFOGRAPHICS_DATA%20VISUALIZATION%20OF%20ANTIMICROBIAL%20PRODUCTS.pdf*
