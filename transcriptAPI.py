from youtube_transcript_api import YouTubeTranscriptApi
import csv

# READS THROUGH THE LIST OF COMICS, GETS THEIR YOUTUBE SPECIAL INFO
def getSpecials(id_list):
    # Define the path to your CSV file
    csv_file_path = 'ComicsList.csv'

    # Specify the target column name (e.g., 'SPECIAL')
    target_column_name = 'SPECIAL'

    # Open the CSV file for reading
    with open(csv_file_path, 'r', newline='') as csv_file:
        # Create a CSV reader
        csv_reader = csv.DictReader(csv_file)

        # Iterate through the rows in the CSV file
        for row in csv_reader:
            # Get the content of the 'SPECIAL' column
            specials_cell_content = row[target_column_name]
            # Check if the 'SPECIAL' cell is not empty
            if specials_cell_content:
                # Parse the pseudo-JSON content within the 'SPECIAL' cell
                # Extract the 'id' value using string manipulation
                id_start = specials_cell_content.find('id:')  # Find the position of 'id:'
                if id_start != -1:
                    id_start += len('id:')  # Move the starting position after 'id:'
                    id_end = specials_cell_content.find('}', id_start)  # Find the end of the 'id' value
                    if id_end != -1:
                        id_value = specials_cell_content[id_start:id_end]
                        id_list.append(id_value)
                
# PROCESS AND SAVE THE TRANSCRIPT TO A CSV WHERE THE FILENAME IS THE VIDEO ID          
def saveTranscriptToCSV(id):
    # Retrieve the available transcripts
    transcript_list = YouTubeTranscriptApi.list_transcripts(id)

    # Create a list to store the transcript data
    transcript_data = []

    # Iterate over all available transcripts
    for transcript in transcript_list:
        # Fetch the transcript data
        transcript_text = transcript.fetch()

        # Append each entry to the transcript_data list
        transcript_data.extend(transcript_text)

    # Define the CSV file name
    csv_file_name = id+'.csv'

    # Open the CSV file in write mode
    with open(csv_file_name, 'w', newline='', encoding='utf-8') as csv_file:
        # Create a CSV writer
        csv_writer = csv.writer(csv_file)

        # Write the header row
        csv_writer.writerow(['text', 'start', 'duration'])

        # Write data from the transcript data to the CSV
        for entry in transcript_data:
            csv_writer.writerow([entry['text'], entry['start'], entry['duration']])

    print(f'CSV data has been saved to {csv_file_name}')

id_list = []
getSpecials(id_list)
for id in id_list:
    saveTranscriptToCSV(id)