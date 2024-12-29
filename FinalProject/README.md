### FINAL 

To practice relevant skills covered within the comprehensive totality of MLBA, design and implement an information pipeline that
summarizes relevant information for an eBay product niche using the eBay developer API. 

## Steps:

- 1. Datamine and preprocess existing eBay listings/seller information

- 2. Employ exploratory data analysis and dimension reduction

- 3. Use predictive modeling techniques including Classification, Regression, and Clustering to evaluate the market niche

- 4. Evaluate model performance

- 5. Use predictive modeling to evaluate ROI against product availability and using these metrics, determine the top 3 products in this niche for resale

- 6. Evaluate model performance

AI GENERATED RUBRIC:
API Data Collection & Preprocessing	20%
- Effective use of eBay API
- Quality of data cleaning
- Handling of missing/outlier data	
Exploratory Analysis & Visualization	20%
- Depth of EDA (visuals, stats)
- Quality of insights from visualizations	
Modeling & Analysis	25%
- Sound choice of methods
- Correct application/evaluation
- Clear interpretation of results	
Quantitative Investment Ranking	15%
- Well-justified scoring/metric
- Clarity in ranking logic
- Actionable top picks	
Interpretation, Reflection & Recommendations	15%
- Depth of insight
- Realistic, data-driven conclusions
- Ethical considerations	
Report Organization & Clarity	5%
- Readable structure
- Clear communication
- Reproducible code	

## Requirements: 

- Python Installation 
- R Installation
- Libraries in pyRequirements.txt
- API keys in APIs.env with the schema 
    eBay = APIKEYPLACEHOLDER
- PostgreSQL server
    sudo apt install -y postgresql-common
    sudo /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh
    sudo apt install postgresql-17
    sudo service postgresql start
    sudo apt-get install postgresql-client-17
    psql -U project -h 127.0.0.1 -d eBayProject -W
    sudo sed -i 's/^\s*local\s\+all\s\+all\s\+peer\s*$/local   all   all   md5/' /etc/postgresql/17/main/pg_hba.conf
    sudo systemctl restart postgresql

