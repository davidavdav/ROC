INSTALL
=======

As long as this package is not in CRAN, installation may ba a hassle.  Installation will depend on the operating system.  

It is probaly easiest if you have a version  2.15 of R installed or a more recent version. 

Operating systems that I like
-----------------------------

This would include most Linux and Unix-like systems

 - Download the package from github as a single [.tar.gz https://api.github.com/repos/davidavdav/ROC/tarball]
 - in a command line, type
 
 ```sh
 R --vanilla CMD INSTALL ROC.tar.gz
 ```
 
Operating systems that I really do not like
-------------------------------------------
 
This would be all sorts of versions of Windows
 
  - Download the package from github as a single .tar.gz
  - open a `cmd` window
  - cd to the installation director of R, specifically to the place where R.exe is located
  - type

```dos
R --vanilla CMD INSTALL \\path\to\ROC.tar.gz
```

Testing installation
--------------------

Start R, and type
```R
library(ROC)
example(plot.cond)
```
and hit a couple of times `<return>`.  You should see a couple of DET plots being drawn. 
