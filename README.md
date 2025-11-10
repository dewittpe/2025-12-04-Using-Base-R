# Using Base R instead of tidyverse or data.table 

Presentation to Denver R User Group 2025-12-04

The data manipulation tools within the tidyverse and for data.table are
extremely powerful and useful.  However, there are cases when you might be
better off using base R.

This presentation will first show a specific use case and how to build the
wanted result via tidyverse (dplyr and tidyr), with data.table, and then several
ways with base R functions.

The example will be somewhat trivial for the a data analysis.  But what if you
are writting a package and want it to be useful for all dialects and you have a
requirement that the package will not import any namespaces? This presentation
will address this situtation and show posible solutions.
