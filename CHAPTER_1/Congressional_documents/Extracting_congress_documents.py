#Extracting congressional documents from the congress website to the folder

from selenium import webdriver
from selenium.webdriver.chrome.service import service as chromeservice
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
import re

