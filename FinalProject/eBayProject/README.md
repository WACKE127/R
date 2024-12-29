The sole purpose of this Django project is to interact with PostgreSQL, the eBay API, and the user.

Populating the database with relevant information and displaying it so it's not necessary to use head() functions is the uniform goal.

Actual data analytics pipelines will be built seperate. Because of the way Django functions, it may be possible to integrate these
in the future. 

CURRENT PROBLEMS:
- eBay API not accepting api token. 
    - DIAGNOSE where exactly this issue is occuring.
        - Am I sending off the correct requests?
        - Is my token being imported properly?
        - Is my token being sent off to eBay's API properly?