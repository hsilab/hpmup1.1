# ncpm_v1.0

How to install the N-CPM package?

1. Run Rstudio as an administrator

2. You must have the "devtool" package. If not, you can install it with the code below:
install.packages("devtools")

3. Run the code below:
devtools::install_github("jpark-tamu/ncpm_v1.0")

4. If Rstudio generates an error below,

Error: package or namespace load failed for 'ncpm' in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
namespace 'package name' 0.x.xx is already loaded, but >= 0.x.xx is required

Then, you need to remove the package and install a latest version with the code below:

remove.packages("package name")
install.packages("package name")

5. Once you successfully run devtools::install_github("jpark-tamu/ncpm_v1.0"), then, the N-CPM package will be installed.

6. Finally, you can run N-CPM with:
library(ncpm)
NCPMGUI()
