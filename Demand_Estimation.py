import os
import requests
import numpy as np
import pandas as pd
import certifi
from bs4 import BeautifulSoup, SoupStrainer
from google_play_scraper import app
from google_play_scraper.exceptions import NotFoundError
import html
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import statsmodels.api as sm

# --------------------- CREATING WEBSCRAPER --------------------------- #

# Define the URL and send and HTTP GET request to the URL 

## Defining the URL of the webpage to scrape 
url = "https://sites.google.com/view/francescodecarolis/data/apps-dataset"
print(url)

## Sending an HTTP GET request to the URL 
response = requests.get(url)

## Checking if the response status code is 200 (if the code is 200, the request was successful)
if response.status_code == 200:
   print("yes")

# Decode the HTML content from the webpage and and parse it using the BeautifulSoup library

## Decoding the HTML content and unescape special characters -- i.e., convert escape sequences or special character representations 
### back to their original characters 
html_content = html.unescape(response.text)
print(html_content)

## Parsing the HTML content using the BeautifulSoup library 
soup = BeautifulSoup(html_content, "html.parser")
print(soup)

# Extract and process the data 

## Extracting column headers in the table's <thead> section
column_headers = soup.find('thead').find_all('th')

## Extracting column names from the column headers 
column_names = [header.get_text(strip=True) for header in column_headers]
print(column_names)

## Finding all rows -- <tr> elements -- in the table, skipping the first row since it contains the column names 
table_rows = soup.find_all('tr')[1:]
print(table_rows)

## Intializing empty list to store the data 
data = []

## Looping through each rows to extract the data 
for row in table_rows:
    td_elements = row.find_all('td')
    row_data = [td.get_text(strip=True) for td in td_elements]
    data.append(row_data)

## Creating pandas data frame using the Pandas library to create the necessary output, where column names are given by column_names, and entries by data
df = pd.DataFrame(data, columns = column_names)  
print(df)

# --------------------- EXTRACTING REMAINING DATA USING GOOGLE PLAY SCRAPER  --------------------------- #

# Set the SSL certificate path 

## Finding the path 
certifi_path = certifi.where()
print(certifi_path)

## Setting the path 
os.environ['SSL_CERT_FILE'] = ''
# Scrape requested information with google-play-scraper and append it to the existing data frame

## Initialing lists to store the information 
prices = []
genres = []
scores = []
reviews = []
offers_IAP = []
min_downloads = []
downloads = []

## Looping over each app 
for index, value in df.iloc[:500, 3].items():
    print(index)
    ### Extracting information for the current app using the app function from the google-play-scraper
    try:
        result = app(
            value,
            lang='en',
            country='us'
        )
        ### From the extracted information, getting the requested information and appending it to its corresponding list 
        prices.append(result.get('price'))
        genres.append(result.get('genre'))
        scores.append(result.get('score'))
        reviews.append(result.get('reviews'))
        offers_IAP.append(result.get('offersIAP'))
        downloads.append(result.get('installs'))
    ### Removing app from the data frame if the app no longer exists 
    except NotFoundError:
        df.drop(index, inplace=True)

## Appending the new columns to the original data frame 
df['Price'] = prices
df['Genre'] = genres
df['Average Score'] = scores
df['Number of Reviews'] = reviews
df['Offers IAP'] = offers_IAP
df['Number of Downloads'] = downloads

## Displaying the updated DataFrame
pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
print(df)

## Storing the data frame into an intermediary file to not have to scrape data again 
df.to_csv('data.csv', index=False) 

# --------------------- COMPUTING QUANTITY OF DOWNLOADS AND CREATING AN OUTSIDE GOOD --------------------------- #

# Compute "Estimated Number of Downloads": if 20*"Number of Reviews" > "Number of Downloads," 
##"Estimated Number of Downloads" = 20*"Number of Reviews", else, "Estimated Number of Downloads" = "Number of Downloads"

## Operationalizing the "Number of Downloads" columns 
### Removing thousands separators and the '+' 
df['Number of Downloads'] = df['Number of Downloads'].str.replace(',', '').str.replace('+','')

### Converting column from string to int64
df['Number of Downloads'] = df['Number of Downloads'].astype('int64')

## Creating the "Estimated Number of Downlaods" column according to conditions 
df['Estimated Number of Downloads'] = np.where(
    20 * df['Number of Reviews'] > df['Number of Downloads'],
    20 * df['Number of Reviews'],
    df['Number of Downloads']
)

## Dropping "Number of Downloads" column
df = df.drop("Number of Downloads", axis=1)

# Generate outside goods: create categories to group the genres in, if a genre cannot be categorized and it contains less than 10 apps within it, 
##set the category to "Outside Good"

## Finding out how many apps within each genre to ensure enough apps belong to the "Outside Good" category 
### and that only genres with less than 10 apps belong to the "Outside Good" category
genre_counts = df['Genre'].value_counts()
print(genre_counts)

## Categorizing genres by assessing the apps within each genre and reasoning
Games = ["Casual", "Simulation", "Adventure", "Trivia", "Role Playing", "Action", "Puzzle", "Arcade", "Card", "Board", "Racing", "Strategy"]
Education = ["Education", "Educational", "Books & Reference"]
Health = ["Health & Fitness", "Medical"]
Entertainment = ["Entertainment", "Sports"]
Multimedia = ["Photography", "Video Players & Editors", "Music & Audio", "Music"]
Utilities = ["Auto & Vehicles", "Tools", "Weather", "Business", "Finance", "Productivity"]
Navigation =["Maps & Navigation", "Travel & Local"]
Personalization = ["Personalization"]
OutsideGood = ["Communication", "Shopping", "Parenting", "Lifestyle", "Social"]

## Creating a list of of the genre categories
category_lists = {
    "Games": Games,
    "Education": Education,
    "Health": Health,
    "Entertainment": Entertainment,
    "Multimedia": Multimedia,
    "Utilities": Utilities,
    "Navigation": Navigation,
    "Personalization": Personalization,
    "Outside Good": OutsideGood
}

## Initializing category column 
df["Category"] = "Uncategorized"

## Mapping each app to one of the categories based on it's genre 
for category, genre_list in category_lists.items():
    df.loc[df["Genre"].isin(genre_list), "Category"] = category

# Create the final data frame to be used in the next assignment 

## Displaying the updated DataFrame
pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
print(df)

## Storing the final DataFrame into a file to produce the summary statistics table and scatter plot
### without having to scrape the data again 
df.to_csv('final_data.csv', index=False) 

# --------------------- COMPUTING SUMMARY STATISTICS AND CREATING PLOT --------------------------- #

# Create and download summary statistics table 

## Operationalizing data
df['Offers IAP'] = df['Offers IAP'].astype(int)
print(df)

## Creating a copy of the DataFrame with an additional "all categories" row 
### which does not discriminate across categories
df_copy = pd.concat([df, df.assign(Category='All Categories')])

## Initializing an empty summary statistics DataFrame
sumstats = pd.DataFrame()

## Looping over each variable to construct summary statistics table for each variable 
variables = ['Price', 'Offers IAP', 'Estimated Number of Downloads', 'Number of Reviews', 'Average Score']

for var in variables:
    summary_statistics = df_copy.groupby('Category')[var].agg(['mean', 'std', 'min', lambda x: x.quantile(0.25), 'median', lambda x: x.quantile(0.75), 'max']).reset_index()
    summary_statistics.columns = ['Category', 'Mean', 'SD', 'Min', '25%', 'Median', '75%', 'Max']
    count_observations = df_copy['Category'].value_counts().reset_index()
    count_observations.columns = ['Category', 'Number of Observations']
    result_df = pd.merge(summary_statistics, count_observations, on='Category')
    empty_row = pd.DataFrame({'Variable': [var], 'Category': [''], 'Number of Observations': [''], 'Mean': [''], 'SD': [''], 'Min': [''], '25%': [''], 'Median': [''], '75%': [''], 'Max': ['']})
    sumstats = pd.concat([sumstats, empty_row, result_df], ignore_index=True)

## Rounding entries to 2 decimals    
columns_to_process = ['Mean', 'SD', 'Min', '25%', 'Median', '75%', 'Max']
for column in columns_to_process:
    sumstats[column] = sumstats[column].apply(pd.to_numeric, errors='coerce')
    sumstats[column] = sumstats[column].astype(float).apply(lambda x: round(x, 2))  # Apply the round function correctly

## Displaying and saving summary statistics table
print(sumstats)
sumstats.to_csv('sumstats.csv', index=False)

# Create and save the scatter plot

## Creating a figure with a specified size
plt.figure(figsize=(10, 6))

## Choosing scatter plot data
scatter = plt.scatter(df['Estimated Number of Downloads'], df['Price'], c='purple', alpha=0.6, s=20, edgecolor='black', linewidth=0.5)

## Setting the x and y-axis labels
plt.xlabel('Quantity (Estimated Number of Downloads)', fontname='Times New Roman', fontsize=12)
plt.ylabel('Price (USD)', fontname='Times New Roman', fontsize=12)  # Increase the font size to 12 or your desired size

## Setting the background color
plt.gca().set_facecolor('white')

## Defining x-axis ticks and labels
x_ticks = range(0, int(df['Estimated Number of Downloads'].max()) + 1, 10000000)
x_labels = [f'{tick:,}' for tick in x_ticks]  # Format labels with commas
plt.xticks(x_ticks, x_labels, fontname='Times New Roman', fontsize=10, rotation=0)
intermediary_ticks = range(1000000, int(df['Estimated Number of Downloads'].max()) + 1, 1000000)
ax = plt.gca()
ax.set_xticks(intermediary_ticks, minor=True)
ax.xaxis.set_minor_formatter(ticker.NullFormatter())

## Defining y-axis ticks and labels
y_ticks = range(0, int(df['Price'].max()) + 1, 10)  # Ticks every 10 USD
y_labels = [f'{tick:.2f} USD' for tick in y_ticks]  # Format all labels with two decimal places
plt.yticks(y_ticks, y_labels, fontname='Times New Roman', fontsize=10)
intermediary_ticks_y = [tick for tick in range(1, int(df['Price'].max()) + 1) if tick % 10 != 0]
ax.yaxis.set_minor_locator(ticker.FixedLocator(intermediary_ticks_y))
ax.yaxis.set_minor_formatter(ticker.NullFormatter())
plt.gca().yaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: '{:.2f}'.format(x)))

## Adding grid lines
plt.grid(True, linestyle='-', alpha=0.3)
plt.grid(True, which='minor', linestyle='--', alpha=0.3)

## Removing the legend from the plot
plt.legend().set_visible(False)

## Saving plot
plt.savefig('scatter.pdf', format='pdf', dpi=300)

## Displaying the plot
plt.show()

# --------------------- PART 1: DEMAND ESTIMATION (preparation) --------------------------- #

df = pd.read_csv('df.csv')

# Compute normal and within-group market shares of each app and the outside option for each country

## Computing total downloads and total downloads of the "OUTSIDE" good for each country (market) in the data
df['total_sum'] = df.groupby('country')['new_est'].transform('sum')
df['total_sum_within_group'] = df.groupby(['country', 'nest'])['new_est'].transform('sum')
df['total_sum_outside'] = df[df['nest'] == 'OUTSIDE'].groupby('country')['new_est'].transform('sum')

## Computing normal and within-group market shares for each country-app combination. Note that the shares for the apps that 
### comprise the "OUTSIDE" good are the same for each country. 
df['market_share'] = df.apply(lambda row: row['total_sum_outside'] / row['total_sum'] if row['nest'] == 'OUTSIDE' else row['new_est'] / row['total_sum'], axis=1)
df['market_share_within_group'] = df.apply(lambda row: row['new_est'] / row['total_sum_within_group'], axis=1)

## Sanity check: Checking that, for each country (country-nest combination), the market share (within-group market share) of the outside good combined with the market shares of 
### other apps add to 1. 

### Total market share for each country
print("Total market share for each country:")
print(df[df['nest'] != 'OUTSIDE'].groupby('country')['market_share'].sum() + df[df['nest'] == 'OUTSIDE'].groupby('country')['market_share'].first())

### Total market share for each unique combination of country and nest
print("Total Market share within group for each unique country nest combination:")
print(df.groupby(['country','nest'])['market_share_within_group'].sum())


# Create data frames for each country with variables necessary for performing the regression

## Initializing empty array to store the country data frames 
list_country = []

## Loop over each country to obtain necessary dependent and independent variables
for country, country_df in df.groupby('country'):
    new_df = pd.DataFrame()
    new_df['App'] = country_df[country_df["nest"] != 'OUTSIDE']['app'].copy()
    new_df['Number of Downloads'] = country_df[country_df["nest"] != 'OUTSIDE']['new_est'].copy()
    new_df['Dependent Variable'] = np.log(country_df[country_df["nest"] != 'OUTSIDE']['market_share']) - np.log(country_df[country_df['nest'] == 'OUTSIDE']['market_share'].values[0])
    new_df['Market Share'] = country_df[country_df["nest"] != 'OUTSIDE']['market_share'].copy()
    new_df['Price'] = country_df[country_df["nest"] != 'OUTSIDE']['price'].copy()
    new_df['Number of Reviews'] = country_df[country_df["nest"] != 'OUTSIDE']['nofreviews'].copy()
    new_df['Average Score'] = country_df[country_df["nest"] != 'OUTSIDE']['averagescore'].copy()
    new_df['IAP'] = country_df[country_df["nest"] != 'OUTSIDE']['inapppurchases'].copy().astype(int)
    new_df['Within Group Market Share'] = np.log(country_df[country_df["nest"] != 'OUTSIDE']['market_share_within_group'].copy())
    new_df['Country'] = [country] * new_df.shape[0]
    new_df['Nest'] = country_df[country_df['nest'] != 'OUTSIDE']['nest'].copy()
    new_df['Developer'] = country_df[country_df['nest'] != 'OUTSIDE']['developer'].copy()
    new_df['Instrument'] = country_df.groupby('appgenre')['appgenre'].transform('count')
    list_country.append(new_df)

## Add country specific dummies

### Create boolean for each country 
combined_df = pd.concat(list_country, ignore_index=True)
combined_df = pd.concat([combined_df, pd.get_dummies(combined_df['Country'], prefix='Country')], axis=1)

### Operationalize boolean (note US excluded to prevent perfect collinearitt)
dummies = [
    'Country_Australia',
    'Country_Canada',
    'Country_France',
    'Country_Germany',
    'Country_Italy',
    'Country_Japan',
    'Country_Korea',
    'Country_Netherlands',
    'Country_Spain',
    'Country_Sweden',
    'Country_United Kingdom'
]

for dummy_col in dummies:
    combined_df[dummy_col] = combined_df[dummy_col].astype(int)

# Save new data frame excluding observations missing 

combined_df = combined_df.dropna()  # Corrected to assign back to combined_df
combined_df.to_csv('final.csv', index=False)

# --------------------- PART 1: DEMAND ESTIMATION (logit estimation) --------------------------- #

# Without instrument 

# Model
independent_vars = ['IAP', 'Number of Reviews', 'Average Score', 'Price'] + dummies
model = sm.OLS(combined_df['Dependent Variable'], combined_df[independent_vars])
results = model.fit()
summary = results.summary()

# Table
summary_df = pd.read_html(summary.tables[1].as_html(), header=0, index_col=0)[0]
result_df = pd.DataFrame(columns=['Regressor', 'Estimate', 'Std. Error', 'P-value', 'Confidence Interval'])

for idx, var in enumerate(independent_vars):
    result_df.loc[idx + 1, 'Regressor'] = var
    result_df.loc[idx + 1, 'Estimate'] = round(summary_df.loc[var, 'coef'], 3)
    result_df.loc[idx + 1, 'Std. Error'] = round(summary_df.loc[var, 'std err'], 3)
    result_df.loc[idx + 1, 'P-value'] = round(summary_df.loc[var, 'P>|t|'], 3)
    result_df.loc[idx + 1, 'Confidence Interval'] = f"({round(summary_df.loc[var, '[0.025'], 3)}, {round(summary_df.loc[var, '0.975]'], 3)})"

# Produce output

print(result_df)
result_df.to_csv('logit.csv', index = False)

# With instrument 

# Stage 1:
independent_vars = ['IAP', 'Number of Reviews', 'Average Score', 'Instrument'] + dummies
model = sm.OLS(combined_df['Price'], combined_df[independent_vars])
results = model.fit()
summary = results.summary()
combined_df['fitted_price'] = results.predict()

# Table
summary_df = pd.read_html(summary.tables[1].as_html(), header=0, index_col=0)[0]
result_df = pd.DataFrame(columns=['Regressor', 'Estimate', 'Std. Error', 'P-value', 'Confidence Interval'])

for idx, var in enumerate(independent_vars):
    result_df.loc[idx + 1, 'Regressor'] = var
    result_df.loc[idx + 1, 'Estimate'] = round(summary_df.loc[var, 'coef'], 3)
    result_df.loc[idx + 1, 'Std. Error'] = round(summary_df.loc[var, 'std err'], 3)
    result_df.loc[idx + 1, 'P-value'] = round(summary_df.loc[var, 'P>|t|'], 3)
    result_df.loc[idx + 1, 'Confidence Interval'] = f"({round(summary_df.loc[var, '[0.025'], 3)}, {round(summary_df.loc[var, '0.975]'], 3)})"

print(result_df)
result_df.to_csv('IV_logit_1.csv', index = False)
    
# Stage 2:
independent_vars = ['IAP', 'Number of Reviews', 'Average Score', 'fitted_price'] + dummies
model = sm.OLS(combined_df['Dependent Variable'], combined_df[independent_vars])
results = model.fit()
summary = results.summary()
    
# Table
summary = results.summary()
summary_logit_IV = summary
summary_df = pd.read_html(summary.tables[1].as_html(), header=0, index_col=0)[0]
result_df = pd.DataFrame(columns=['Regressor', 'Estimate', 'Std. Error', 'P-value', 'Confidence Interval'])

for idx, var in enumerate(independent_vars):
    result_df.loc[idx + 1, 'Regressor'] = var
    result_df.loc[idx + 1, 'Estimate'] = round(summary_df.loc[var, 'coef'],3)
    result_df.loc[idx + 1, 'Std. Error'] = round(summary_df.loc[var, 'std err'],3)
    result_df.loc[idx + 1, 'P-value'] = round(summary_df.loc[var, 'P>|t|'],3)
    result_df.loc[idx + 1, 'Confidence Interval'] = f"({round(summary_df.loc[var, '[0.025'],3)}, {summary_df.loc[var, '0.975]']})"

# Produce output

print(result_df)
result_df.to_csv('IV_logit_2.csv', index = False)

# --------------------- PART 1: DEMAND ESTIMATION (nested logit estimation) --------------------------- #

# Without instrument 

independent_vars = ['IAP', 'Number of Reviews', 'Average Score', 'Price', 'Within Group Market Share'] + dummies
model = sm.OLS(combined_df['Dependent Variable'], combined_df[independent_vars])
results = model.fit()
summary = results.summary()

# Table
summary_df = pd.read_html(summary.tables[1].as_html(), header=0, index_col=0)[0]
result_df = pd.DataFrame(columns=['Regressor', 'Estimate', 'Std. Error', 'P-value', 'Confidence Interval'])

for idx, var in enumerate(independent_vars):
    result_df.loc[idx + 1, 'Regressor'] = var
    result_df.loc[idx + 1, 'Estimate'] = round(summary_df.loc[var, 'coef'], 3)
    result_df.loc[idx + 1, 'Std. Error'] = round(summary_df.loc[var, 'std err'], 3)
    result_df.loc[idx + 1, 'P-value'] = round(summary_df.loc[var, 'P>|t|'], 3)
    result_df.loc[idx + 1, 'Confidence Interval'] = f"({round(summary_df.loc[var, '[0.025'], 3)}, {round(summary_df.loc[var, '0.975]'], 3)})"

# Produce output

print(result_df)
result_df.to_csv('nested_logit.csv', index = False)

# With instrument 

# Stage 1:
independent_vars = ['IAP', 'Number of Reviews', 'Average Score', 'Instrument', 'Within Group Market Share'] + dummies
model = sm.OLS(combined_df['Price'], combined_df[independent_vars])
results = model.fit()
summary = results.summary()
combined_df['fitted_price'] = results.predict()

# Table
summary_df = pd.read_html(summary.tables[1].as_html(), header=0, index_col=0)[0]
result_df = pd.DataFrame(columns=['Regressor', 'Estimate', 'Std. Error', 'P-value', 'Confidence Interval'])

for idx, var in enumerate(independent_vars):
    result_df.loc[idx + 1, 'Regressor'] = var
    result_df.loc[idx + 1, 'Estimate'] = round(summary_df.loc[var, 'coef'], 3)
    result_df.loc[idx + 1, 'Std. Error'] = round(summary_df.loc[var, 'std err'], 3)
    result_df.loc[idx + 1, 'P-value'] = round(summary_df.loc[var, 'P>|t|'], 3)
    result_df.loc[idx + 1, 'Confidence Interval'] = f"({round(summary_df.loc[var, '[0.025'], 3)}, {round(summary_df.loc[var, '0.975]'], 3)})"

print(result_df)
result_df.to_csv('IV_nested_logit_1.csv', index = False)
    
# Stage 2:
independent_vars = ['IAP', 'Number of Reviews', 'Average Score', 'fitted_price', 'Within Group Market Share']+dummies
model = sm.OLS(combined_df['Dependent Variable'], combined_df[independent_vars])
results = model.fit()
summary = results.summary()
    
# Table
summary = results.summary()
summary_df = pd.read_html(summary.tables[1].as_html(), header=0, index_col=0)[0]
result_df = pd.DataFrame(columns=['Regressor', 'Estimate', 'Std. Error', 'P-value', 'Confidence Interval'])

for idx, var in enumerate(independent_vars):
    result_df.loc[idx + 1, 'Regressor'] = var
    result_df.loc[idx + 1, 'Estimate'] = round(summary_df.loc[var, 'coef'],3)
    result_df.loc[idx + 1, 'Std. Error'] = round(summary_df.loc[var, 'std err'],3)
    result_df.loc[idx + 1, 'P-value'] = round(summary_df.loc[var, 'P>|t|'],3)
    result_df.loc[idx + 1, 'Confidence Interval'] = f"({round(summary_df.loc[var, '[0.025'],3)}, {summary_df.loc[var, '0.975]']})"

# Produce output

print(result_df)
result_df.to_csv('IV_nested_logit_2.csv', index = False)

# --------------------- PART 2: SUPPLY ANALYSIS (preparation) --------------------------- #

# Obtain coefficients for the logit model with IV 
summary_logit_IV = pd.read_html(summary_logit_IV.tables[1].as_html(), header=0, index_col=0)[0]

## Obtain US data frame since the analysis is for the US
US_df = combined_df[combined_df['Country'] == 'United States'].reset_index(drop=True)

## Obtain US market share of the outside good 
US_outside_market_share = df[(df['country'] == 'United States') & (df['nest'] == 'OUTSIDE')]['market_share'].values[0]

# --------------------- PART 2: SUPPLY ANALYSIS (mark up estimation) --------------------------- #

# Compute ownership matrix under single product firms, current ownership, and perfect collusion

## Single product firms: Every firm owns one app
omega_single_product_firm = np.identity(US_df.shape[0])

## Current structure: Multi-product firms setting prices for products jointly 
omega_current_ownership = np.zeros((US_df.shape[0], US_df.shape[0]))
for i in range(US_df.shape[0]):
    for j in range(US_df.shape[0]):
        if US_df.iloc[i]['Developer'] == US_df.iloc[j]['Developer']:
            omega_current_ownership[i][j] = 1  

## Monopoly: Joint profit-maximization of all the brands
omega_monopoly = np.ones(combined_df[combined_df['Country'] == 'United States'].shape[0])

# Compute own and cross-price elasticities
            
## Initializing elasticity matrix             
elasticity_matrix = np.zeros((US_df.shape[0], US_df.shape[0]), dtype=float)

## Looping over each app combination 
for i in range(US_df.shape[0]):
    for j in range(US_df.shape[0]):
        if i != j:
            elasticity_matrix[i][j] = (
                US_df['Market Share'][i]*
                US_df['Market Share'][j]*
                summary_logit_IV.loc['fitted_price', 'coef']
            )
        else:
            elasticity_matrix[i][j] = (
                -summary_logit_IV.loc['fitted_price', 'coef'] *
                US_df['Market Share'].reset_index(drop=True)[i] *
                (1 - US_df['Market Share'][i])
            )

# Compute and output the markup under each scenario

## Creting output 
markup_df = pd.DataFrame({
    'App': US_df['App'],  # App names from US_df
    'Mark Up Single Product Firm': np.dot(np.linalg.inv(omega_single_product_firm * elasticity_matrix), US_df['Market Share']),
    'Mark Up Current Ownership': np.dot(np.linalg.inv(omega_current_ownership * elasticity_matrix), US_df['Market Share']),
    'Mark Up Monopoly': np.dot(np.linalg.inv(omega_monopoly * elasticity_matrix), US_df['Market Share'])
})

marginal_cost_df = pd.DataFrame({
    'App': US_df['App'],  # App names from US_df
    'Marginal Cost Single Product Firm': US_df['Price'] - markup_df['Mark Up Single Product Firm'] ,
    'Marginal Cost Current Ownership': US_df['Price'] - markup_df['Mark Up Current Ownership'],
    'Marginal Cost Monopoly': US_df['Price'] - markup_df['Mark Up Monopoly']
})

## Rounding 
markup_df = round(markup_df,10)
marginal_cost_df = round(marginal_cost_df, 2)

## Printing output 

#print(markup_df)
print(markup_df['Mark Up Monopoly'])
print(marginal_cost_df)

markup_df.to_csv('Mark Up Matrix.csv', index = False)
marginal_cost_df.to_csv('Marginal Cost Matrix', index = False)

# --------------------- PART 2: SUPPLY ANALYSIS (merger simulation) --------------------------- #

# Preparation

## Initializing trackers 
best_profit = float('-inf')
best_developer = None
market_share = float('-inf')
revenue = float('-inf')

## Gathering unique developers 
unique_developers = US_df['Developer'].unique()

# Loop over each developer and compute profits 
for developer in unique_developers:
    if developer != 'Mojang':
        temp_df = US_df.copy()
        temp_df['Developer'] = temp_df['Developer'].replace(developer, 'Mojang')

        omega_current_ownership = np.zeros((temp_df.shape[0], temp_df.shape[0]))

        for i in range(temp_df.shape[0]):
            for j in range(temp_df.shape[0]):
                if temp_df.iloc[i]['Developer'] == temp_df.iloc[j]['Developer']:
                    omega_current_ownership[i][j] = 1

        markup = np.dot(np.linalg.inv(omega_current_ownership * elasticity_matrix), temp_df['Market Share'])
        mojang_indices = temp_df[temp_df['Developer'] == 'Mojang'].index
        markup = markup[mojang_indices,]
        profit_mojang = (markup*temp_df.loc[mojang_indices, 'Number of Downloads']).sum()
        market_share_mojang = temp_df.loc[mojang_indices, 'Market Share'].sum()
        revenue_mojang = (temp_df.loc[mojang_indices, 'Price']*temp_df.loc[mojang_indices, 'Number of Downloads']).sum()

        print(developer)

        if profit_mojang > best_profit:
            best_profit = profit_mojang
            best_developer = developer
            market_share = market_share_mojang
            revenue = revenue_mojang


print("Best Developer:", best_developer)

# Create output 

## After 
mojang_profit_after = best_profit
mojang_revenue_after = revenue
mojang_market_share_after = market_share 

## Before 
US_df['Revenue'] = US_df['Number of Downloads'] * US_df['Price']
mojang_revenue_before = US_df.loc[US_df['App'] == 'Minecraft', 'Revenue'].values[0]
mojang_profit_before = markup_df.loc[markup_df['App'] == 'Minecraft', 'Mark Up Current Ownership'].values[0] * US_df.loc[US_df['App'] == 'Minecraft', 'Number of Downloads'].values[0]
mojang_market_share_before = US_df.loc[US_df['App'] == 'Minecraft', 'Market Share'].values[0]       

## Table with before, after and difference in profits, revenues and market shares
before_after_data = {
    'Before': [mojang_profit_before, mojang_revenue_before, mojang_market_share_before],
    'After': [mojang_profit_after, mojang_revenue_after, mojang_market_share_after]
}

before_after_data['Difference'] = [after - before for before, after in zip(before_after_data['Before'], before_after_data['After'])]
before_after_table = pd.DataFrame(before_after_data, index=['Profit', 'Revenue', 'Market Share'])
before_after_table = round(before_after_table, 3)

## Outputing table
print(before_after_table)
before_after_table.to_csv("Merger Simulation Results", index = False)

# --------------------- END --------------------------- #






