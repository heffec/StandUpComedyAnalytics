import requests
import re
import pandas as pd

CSV_path = 'ComicsList.csv'
ENCODING = 'utf_8'

def getComics(page):
    url = "https://www.dead-frog.com/comedians/list/" + page
    response = requests.get(url)
    print(page)
    
    if response.status_code == 200:
        pattern = r"<h4><strong>(.*?)</strong></h4>"
        matches = re.findall(pattern, response.text)
        
        for match in matches:
            print(match)
            write_to_csv(match)
    else:
        print("Request failed with status code:", response.status_code)

import pandas as pd

def write_to_csv(data):
    # Read the existing CSV data into a DataFrame
    existing_df = pd.read_csv(CSV_path)
    # Create a DataFrame with the new comic data
    new_data = pd.DataFrame({'COMIC': [data]})
    # Append the new data to the existing DataFrame
    combined_df = existing_df.append(new_data, ignore_index=True)
    # Remove duplicates based on a specific column (e.g., 'COMIC')
    df = df.drop_duplicates(subset='COMIC')
    # Sort the DataFrame by the 'COMIC' column
    combined_df = combined_df.sort_values(by='COMIC')
    # Write the sorted DataFrame back to the CSV file
    combined_df.to_csv(CSV_path, index=False, encoding=ENCODING)

pageList = []
for i in range(48):
    if i == 0:
        pageList.append("")
    else:
        pageList.append("P" + str(i * 12))
for item in pageList:
    getComics(item)