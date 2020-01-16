# AZASRS  

#### Provides higher level abstraction for database queries and `asrsMethods` library.  


## Installation  

`devtools::install_github('AZASRS/AZASRS', force = TRUE)`  

Ensure that you have received the `.Renviron` file and put it into your home folder. Locate your home folder via: `path.expand("~")`. This `.Renviron` file contains all of the environment variables that you need in order to connect to the database. It's important that you have your own `GITHUB_PAT` and this will not work if you do not have a blank line at the end of the file (strange requirement, but important).

----

## Contributions  
Please utilize the projects section: <https://github.com/AZASRS/AZASRS/projects/1> in order to give your ideas and start planning projects. We can keep track of things here and make sure that we are not duplicating efforts or repeating mistakes. We'll start to develop a process surrounding this that will allow us to keep our workflow integrated and our code nice and clean. Please note, that you should always make a branch to work on, commit your changes regularly with good notes, and make pull requests on a small set of features. Doing anything large-scale will result in merge conflicts and changes will take longer to complete. Consistently fetching new branches and merging into the branch you are working on is also a good practice. Upon finishing a feature in the branch you created, you will make a pull request and then we can talk through it and make changes if necessary. All tests must pass before you can merge changes. Running `testthat::test_dir('tests')` will check existing code. You will be required to write tests for any code you will be adding as well in order to ensure we can trust changes moving forward.

#### Workflow for a contribution to the library:  

  - Start by going to: <https://github.com/AZASRS/AZASRS/projects/1>  
  - On the far left, add a note under wishlist (make it short enough to read)  
  - If it is complicated, click the three dots `...` on your new note and click "Convert to issue". There you can make some more notes and add a use case. You can also assign users so that people know who this affects.  
  - Keep in mind, most people will not immediately know what the end result you're looking for is, so provide a detailed explanation of what it should do and how it should be calculated.  
  - If it is possible to add a screenshot of the end result (keep proprietary information in mind) that would be helpful  

  1. Pull the latest `development` branch to make sure it is up-to-date  
  2. Checkout your `development` branch  
  3. Create a new branch from `development` with a name that reflects the feature you're working on (i.e. `cash_adjusted_navs`). If you are creating a new function, please name the function the same as the file within the `R` directory.  
  4. Make code changes / add new files / etc. -- consider the outcome and create a test by running the command `usethis::use_test('my_new_function') ` in which `my_new_function` represents the new file/function you want to test. Ensure this test fails if it does not return the proper results.  
  5. Run all tests `testthat::test_dir('tests')` -- ensure they pass (the testthat results are above the "Results" section, and show something like:  [ OK: 19 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 0 ]) -- if all tests do not pass, figure out the problem and fix it before committing or making a pull request.  
  6. With each change, save, stage, commit often (in small batches, typically one file at a time) with informative commit messages. Small commits are extremely important, and incremental changes **must** be made in order to avoid merge conflicts.  
  7. Push changes to GitHub  
  8. Make pull request to merge your branch into `development`  
  9. Changes will be reviewed by at least 2 people and then merged if they satisfy all requirements  
  10. On a schedule, all changes should be merged into `development` and then the changes will be merged into `master`  
  11. This will be followed by a "new release" in which all parties should be notified and everyone should install the newest library with `devtools::install_github('AZASRS/AZASRS', force = TRUE)`  
  
#### Testing considerations:  
  1. Do not add any .csv or other data that could be seen as proprietary to GitHub  
  2. Never "hard code" any file locations  
  3. When needing to save data, utilize the `saveRDS_test()` function. Where you take the variable you want to save and utilize the function name you have created. For example, if I created a function called `my_favorite_function` that returned a tibble of values, I would then run the following in my console:
  ```
  my_favorite_function = my_favorite_function(argument1, argument2)
  saveRDS_test(my_favorite_function)
  ```
  
  4. This creates an .rds file (so ensure that there is no proprietary information, or very minimal by perhaps writing `%>% head(3)` to the end so that very little is available)  
  5. After running `usethis::use_test('my_favorite_function')` a file will be created: `./tests/testthat/test-my_favorite_function.R` that typically is opened up automatically and you can add your test there. A small example is included and please reach out if you need help understanding what to write. If you look at the other examples it may be easy to see what to test. These do not need to be extremely difficult tests but should cover the majority of what you are looking to achieve.  
  6. Run `testthat::test_dir('tests')` to see if your tests pass! You should also slightly modify it after to ensure it fails in order to know that you are testing something meaningful. But at the end of the day it needs to pass.


----

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

----

## Examples

Examples provided in `examples` folder.  


----

## Notes  
  
  - Updating the database requires files to be uploaded into Azure Blob Storage. These must be created and updated utilizing the `data-pipeline` repository <https://github.com/AZASRS/data-pipeline>. 

