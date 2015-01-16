INSTALL
=======

As long as this package is not in CRAN, installation may ba a hassle.  Installation will depend on the operating system.  

It is probaly easiest if you have R version 2.15 or a more recent version installed. 

Operating systems that I like
-----------------------------

This would include most Linux and Unix-like systems

  - Download the package from github as a single [ROC.tar.gz](https://api.github.com/repos/davidavdav/ROC/tarball)
  - in a command line, type
 
 ```sh
 mv master ROC.tar.gz ## where `master' is the name of the download
 R --vanilla CMD INSTALL ROC.tar.gz
 ```
 
Operating systems that I really do not like
-------------------------------------------
 
This would be all sorts of versions of Windows
 
  - Download the package from github as a single [ROC.zip](https://api.github.com/repos/davidavdav/ROC/zipball)
  - open a `cmd` window
  - cd to the installation director of R, specifically to the place where R.exe is located
  - type

```dos
R --vanilla CMD INSTALL \\path\to\ROC.zip
```

Testing installation
--------------------

Start R, and type
```R
library(ROC)
example(plot.cond)
```
and hit a couple of times `<return>`.  You should see a number of DET plots that correspond to some of the example data sets. 
