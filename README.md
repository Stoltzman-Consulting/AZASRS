# AZASRS  

#### Provides higher level abstraction for database queries and `asrsMethods` library.  


## Installation  

`devtools::install_github('AZASRS/AZASRS')`  

Ensure that you have received the `.Renviron` file and put it into your home folder. Locate your home folder via: `path.expand("~")`. This `.Renviron` file contains all of the environment variables that you need in order to connect to the database. It's important that you have your own `GITHUB_PAT` and this will not work if you do not have a blank line at the end of the file (strange requirement, but important).


## Important Functions

`library('AZASRS')`  

#### Pulling data  
These will begin with `get_` and **do not require** data to be passed.
  - Pull all NAV (contains meta data):  `get_pm_nav_daily()`  
  - Pull all Cash Flow (contains meta data):  `get_pm_cash_flow_daily()`  
  - Pull Fund Info: `get_pm_fund_info()`  
  - Pull benchmark index: `get_benchmark_daily_index()`  
    - Beware that this is a large amount of data and should be filtered

#### Building high level output  
These will begin with `build_` and **do not require** data to be passed, however, it will likely only be useful if you do so.  
  - Build all Private Market Metrics: `build_privm_metrics()`  
  - Build P2P IRR's: `build_privm_p2p_irr()`  
  - Build benchmark future value index factors: `build_benchmark_fv_index_factor()`  

#### Calculating metrics  
These will begin with `calc_` and require specific inputs. These can be seen on a case by case basis within the documentation.  
  - Calculate irr: `calc_irr()` -- requires data to be passed

#### Raw data (database tables)  
You can pull the raw data from the database tables if you need to. This should be a **last resort** because schema changes are possible and this would make breaking changes in your reports. These willall begin with `tbl_`. If you need to do joining, you should not ever use raw SQL but should instead use these `tbl_` functions.  
  - Getting Fund Info: `tbl_pm_fund_info()`

If you are joining these, you will need to utilize a single connection, available, for example:   

```
con = AZASRS_DATABASE_CONNECTION()
tbl_pm_fund_nav_daily(con) %>%
  left_join(tbl_pm_fund_info(con), by = 'pm_fund_info_id')
```

#### Return types  
You can return a `tibble` or a database object (shown below).

```
# Source:   lazy query [?? x 27]
# Database: Microsoft SQL Server 12.00.2000[dbo@azasrs/asrs]
```  

A `tibble` requires it to actually calculate from the database, be stored in R (RAM) and then be processed. If you utilize the database objects, you may do calculations before it reaches your local computer. The latter method will optimize performance by putting computing power in the database. Many functions utilize `return_tibble = TRUE` (or `FALSE`) which allows the user to decide.


#### Utilizing more functionality  
You can utilize most of the `dplyr` and `dbplyr` packages. This allows for the use of `mutate`, `filter`, etc. This will speed up your data analysis dramatically by reducing the number of rows and columns you are pulling.

```
get_pm_nav_daily(return_tibble = FALSE) %>%
  dplyr::filter(effective_date >= '2019-01-01')
```


## Examples

Examples provided in `examples` folder.  


## Notes  
  
  - Updating the database requires files to be uploaded into Azure Blob Storage. These must be created and updated utilizing the `data-pipeline` repository <https://github.com/AZASRS/data-pipeline>. 

