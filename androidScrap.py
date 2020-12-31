from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
import time

options = Options()
options.headless = True

browser = webdriver.Chrome(executable_path=ChromeDriverManager().install(), chrome_options=options)
review = open(r"/Users/yusufhancer/Desktop/android.csv","w")
link = r"https://play.google.com/store/apps/details?id=com.obilet.androidside&hl=tr&gl=US&showAllReviews=true"
browser.get(link)
browser.find_element_by_class_name("DPvwYc").click()
#time.sleep(1)
#browser.find_element_by_css_selector('[data-value="2"]').click()

date = list()
comment1 = list()
comment2 = list() 
rating =list()
commentcss = browser.find_elements_by_css_selector("span[jsname='bN97Pc']")
commentcss2= browser.find_elements_by_css_selector("span[jsname='fbQN7e']")
datexpath = browser.find_elements_by_xpath("(.//span[@class = 'p2TkOb'])")
ratingxpath = browser.find_elements_by_xpath("//div[@class='pf5lIe']")
for i in range(0,len(commentcss)):
    date.append(datexpath[i].get_attribute('innerHTML'))
    comment1.append(commentcss[i].get_attribute('innerHTML'))
    comment2.append(commentcss2[i].get_attribute('innerHTML'))
    rating.append(ratingxpath[i+1].get_attribute('innerHTML'))

for i in range(0,len(comment1)):
    if comment1[i].__contains__('<button class="LkLjZd ScJHi OzU4dc  " jsaction="click:TiglPc"'):
        comment1[i] = comment2[i]
    start = rating[i].find("zerinden ") + len("zerinden ")
    end = rating[i].find("ald")
    rating[i] = rating[i][start:end]

for i in range(0,len(date)):
    s = date[i] +";" + rating[i]+";"+comment1[i]
    print(s,file=review)
review.flush()
review.close()
browser.close()