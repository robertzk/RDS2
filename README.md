Better serialization for R [![Build Status](https://travis-ci.org/robertzk/RDS2.svg?branch=master)](https://travis-ci.org/robertzk/RDS2) [![Coverage Status](https://coveralls.io/repos/robertzk/RDS2/badge.svg?branch=master)](https://coveralls.io/r/robertzk/RDS2?branch=master) [![Documentation](https://img.shields.io/badge/rocco--docs-%E2%9C%93-blue.svg)](http://robertzk.github.io/RDS2/)
===========

A slightly improved serialization format over R's built-in [RDS](http://cran.r-project.org/web/packages/RDS/index.html)
that solves the problem of serializing external pointers (e.g., to 
non-native C structures).

# Installation

This package is not yet available from CRAN (as of May 15, 2015).
To install the latest development builds directly from GitHub, run this instead:

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("robertzk/RDS2")
```


