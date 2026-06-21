#Extracting congressional documents from the congress website to the folder

from selenium import webdriver
from selenium.webdriver.chrome.service import service as chromeservice
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
import re



#Extracting pdf files for page 1
from selenium import webdriver # Selenium is used for browser automation.
from selenium.webdriver.chrome.service import Service as ChromeService
from webdriver_manager.chrome import ChromeDriverManager  # ChromeDriverManager automatically downloads the right version of ChromeDriver.
from selenium.webdriver.common.by import By  # By is a locator strategy for finding elements (e.g., by tag, class, XPath).
import time  # time is used to pause between downloads.
import os
import urllib.parse  # os and urllib.parse help with file paths and URL handling.
import re


# Setting my download path
#download_dir = "/Users/agnesnamyalo/Desktop/LAB_MATERIAL"  #folder to put the downloaded files

download_dir =  "/Users/agnesnamyalo/Documents/PROJECT_WORK/CHAPTER_1/Congressional_documents/CONGRESS_DOCUMENTS/"

# function to wait for downloads to finish
def wait_for_downloads(download_path, timeout=30):
    seconds = 0
    while any(filename.endswith(".crdownload") for filename in os.listdir(download_path)):
        time.sleep(1)
        seconds += 1
        if seconds > timeout:
            break



options = webdriver.ChromeOptions()
options.add_experimental_option('prefs', {
    "download.default_directory": download_dir,
    "download.prompt_for_download": False,    #Ensuring that Chrome doesn't try to preview PDFs but downloads them directly.
    "plugins.always_open_pdf_externally": True
})

driver = webdriver.Chrome(
    service=ChromeService(ChromeDriverManager().install()),
    options=options
)

# URL to fetch
#This opens the Congress.gov search results page for the keyword "Endagered Species Act" in the Congressional Record, maximizes the browser, and waits up to 5 seconds for elements to load.

#URL = 'https://www.congress.gov/quick-search/congressional-record?wordsPhrases=%22endangered+species+act%22+%22ESA%22&congresses[0]=all&dateOperator=between&startDate=01%2F04%2F1995&endDate=12%2F30%2F2025&dateIsOption=yesterday&sectionHouse=on&sectionExtensionsOfRemarks=on&representative[0]=&senator[0]=&pageSize=250' #link with the first 250 pdfs with the first limited search word

#URL = 'https://www.congress.gov/quick-search/congressional-record?wordsPhrases=%22endangered+species%22&wordVariants=on&congresses[0]=all&dateOperator=between&startDate=01%2F02%2F2015&endDate=12%2F30%2F2025&dateIsOption=yesterday&sectionHouse=on&sectionExtensionsOfRemarks=on&representative[0]=&senator[0]=&pageSize=250'  #link with the first 250 pdfs with the endangered species search word

#URL = 'https://www.congress.gov/quick-search/congressional-record?wordsPhrases=%22endangered+species%22&wordVariants=on&congresses[0]=all&dateOperator=between&startDate=01%2F06%2F2015&endDate=12%2F26%2F2025&dateIsOption=yesterday&sectionHouse=on&sectionExtensionsOfRemarks=on&representative[0]=&senator[0]=&pageSize=250' #page one

#URL = 'https://www.congress.gov/quick-search/congressional-record?wordsPhrases=%22endangered+species%22&wordVariants=on&congresses[0]=all&dateOperator=between&startDate=01%2F06%2F2015&endDate=12%2F26%2F2025&dateIsOption=yesterday&sectionHouse=on&sectionExtensionsOfRemarks=on&representative[0]=&senator[0]=&pageSize=250&page=2' #page two

URL = 'https://www.congress.gov/quick-search/congressional-record?wordsPhrases=%22endangered+species%22&wordVariants=on&congresses%5B0%5D=all&dateOperator=between&startDate=01%2F06%2F2015&endDate=12%2F26%2F2025&dateIsOption=yesterday&sectionHouse=on&sectionExtensionsOfRemarks=on&representative%5B0%5D=&senator%5B0%5D=&pageSize=250&page=2' #page twoo

driver.get(URL)
driver.maximize_window()
driver.implicitly_wait(5)


pdf_links = driver.find_elements(By.XPATH, "//a[translate(substring(@href, string-length(@href)-3), 'PDF', 'pdf')='.pdf']")


base_url = driver.current_url
downloaded = 0



for link in pdf_links:
    href = link.get_attribute('href')
    if not href:
        continue
    # Resolve relative URLs
    href_abs = urllib.parse.urljoin(base_url, href)
    driver.get(href_abs)    #Navigating to the PDF link to trigger Chrome’s auto-download.
    downloaded += 1
    if downloaded >=250:   #Stops after downloading 250 files.
            break
    time.sleep(7) 

print(f"Downloaded {downloaded} PDFs to {download_dir}")  #Printing the number of PDFs downloaded and closes the browser.
driver.quit()

