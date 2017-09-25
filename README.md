# ShinyR-DAM

### Authors 
Karol Cichewicz, Jay Hirsh, University of Virginia, Charlottesville, VA

### Short description
ShinyR-DAM is an application for analyzing Drosophila locomotor activity, sleep, and circadian periodicity recorded by the Drosophila Activity Monitor (DAM) system, developed and manufactured by TriKinetics (Waltham, MA). Our program operates in the cloud and can be accessed via this link: https://karolcichewicz.shinyapps.io/shinyr-dam/ , or deployed locally using RStudio. For optimization and grant writing purposes we track the usage of our app using google analytics java script included in this repository. We do not collect any information about the data processed by our program.  

### Testing
For testing we provide 5 monitor files in this repository. Each monitor file contains data of 32 flies, that represent a condition. Thre are 5 LD days: 2017-06-23 - 2017-06-27; and 5 DD days: 2017-06-28 - 2017-07-02 in this dataset. Light onset time was at 6 AM, and acquisition frequency was set to 1 min. 

### Abstract
We developed a web application ShinyR-DAM for analyzing Drosophila locomotor activity, sleep, and circadian rhythms recorded by the Drosophila Activity Monitor (DAM) system (TriKinetics, Waltham, MA). The DAM system measures locomotor activity as infrared beam break counts of flies walking in glass tubes. It allows long-term recording of behavior, making it particularly suitable for circadian biology studies. Comparing with the existing programs for DAM data analysis, ShinyR-DAM substantially decreases the complexity and time required to analyze the data, producing aesthetically pleasing plots, summary tables, and data files for further statistical analysis. Our program has an intuitive graphical user interface that enables novice users to quickly learn the analyses. It runs in a web browser and requires no installation on a local computer, nor programming skills. 

### License
ShinyR-DAM source code is provided under the GPLv3 free software license.
