An example LDA implementation for Scala

This is a demo version made in 2 hours so execution speed is a bit slow. However you can check against [my C++ implementation](https://github.com/polymorpher/aliaslda) to figure out how to improve the speed.

Designed to supplement AI course on BitTiger [https://www.bittiger.io/livecourses/YQCMuXwL7fhHuQT5K](https://www.bittiger.io/livecourses/YQCMuXwL7fhHuQT5K)

Requirements:
- Scala >2.11.8
- SBT 


To run, enter `sbt run` and select the demo you want to run

- NLPCore: an example of using simple NLP processing pipeline on two sentences

- SNAPReaderDemo: an example of reading data file line by line and parse each line as JSON.

- TextDemo: Read `src/main/resources/text/SanDiskUltra64GB.txt`, perform NLP preprocessing, and LDA. Result are displayed for every iteration

- BOWDemo: You need to specify dataset as parameter. Instead of running `sbt run`, run `sbt "runMain BOWDemo <dataset>"`, where `<dataset>` can be either `kos` or `nips`. Don't forget the double quotes!
 
 
Data files:
```$xslt
src/main/resources
  /bow
    /docword.kos.txt      // Daily Kos blog data, preprocessed, extracted from UCL bag of words dataset
    /docword.nips.txt     // NIPS paper abstract data, extracted from UCL bag of words dataset
    /vocab.kos.txt        // Vocabulary for Daily Kos
    /vocab.nips.txt       // Vocabulary for NIPS
  /text
    /SanDiskUltra64GB.txt // Amazon product reviews for SanDisk Ultra 64GB until 2014
```