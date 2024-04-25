from googleapiclient.discovery import build
from youtube_transcript_api import YouTubeTranscriptApi
import pandas as pd

class YouTubeAPI:
    def __init__(self, key):
        self.api_key = key
        self.youtube = build('youtube', 'v3', developerKey=self.api_key)
        self.request = None  # You can initialize request here if needed

    def fetch_video_categories(self, part, regionCode):
        self.request = self.youtube.videoCategories().list(
            part='snippet',
            regionCode='US'
        )
        response = self.request.execute()
        return response
    
    def get_video_category(self, category, part='snippet', regionCode='US'):
        categories = self.fetch_video_categories(part, regionCode)
        found_category = [item for item in categories['items'] if item['snippet']['title'] == category]
        for cat in found_category:
            print(cat)
        return found_category

if __name__ == '__main__':
    # Create an instance of the YouTubeAPI class
    api = YouTubeAPI('AIzaSyCNm7MOIwt96xcDYANK3Mam4ZmGBxlsvzw')

    # Call the method to fetch video categories
    response = api.get_video_category("Comedy")