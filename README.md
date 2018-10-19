# AZASRS  

#### Provides higher level abstraction for database queries and `asrsMethods` library.  


## Installation  

`devtools::install_github('AZASRS/AZASRS')`


## Examples

Examples provided in `examples` folder.


## Notes  

  - This package requires access to the P: drive. In the event you are connecting over VPN, ensure that you can access the P: drive via "My Computer" because simply connecting to VPN does not activate your connection immediately. After you see files appear, try to use the library again!  

  - If you intened to access tables directly, use the AZASRS library methods located in `R/database_globals.R` to ensure that your code does not break in the future.  
  
  - Any functions utilizing the library should only use **one** connection to the database for all of the joins to ensure `lazy loading` works. Without this, code queries will be extremely inefficient.  
  
  - Please reach out to scotts@azasrs.gov if you have any questions!

