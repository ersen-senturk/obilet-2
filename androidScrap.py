from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
import time

options = Options()
options.headless = True

browser = webdriver.Chrome(executable_path=ChromeDriverManager().install(), chrome_options=options)
review = open(r"/Users/yusufhancer/Desktop/android.txt","w")
ratingString = ["Rated ", "zerinden "]
links = ["https://play.google.com/store/apps/details?id=com.obilet.androidside&hl=en&gl=US&showAllReviews=true","https://play.google.com/store/apps/details?id=com.obilet.androidside&hl=tr&gl=US&showAllReviews=true"]
for MAINFOR in range(2):
    browser.get(links[MAINFOR])
    browser.maximize_window()
    rel=browser.find_element_by_class_name("DPvwYc")
    rel.click()
    time.sleep(0.5)      #sıralamanın değişmesini kontrol edebiliriz manual sayı girmekten daha iyi olabilir bu sayının aşılabileceği durumlar ortaya çıkabilir
    if MAINFOR == 0: #logodan konum alıp basıyoruz hl=en için
        logo=browser.find_element_by_class_name("xSyT2c")
        action = ActionChains(browser)
        action.move_to_element(logo).move_by_offset(0,190).click().perform()
    else:   ##akordiyondan konum alıp basıyoruz hl=tr için
        action = ActionChains(browser)
        action.move_to_element(rel).move_by_offset(0,35).click().perform()
    
    time.sleep(2)   #sıralamanın değişmesini kontrol edebiliriz manual sayı girmekten daha iyi olabilir bu sayının aşılabileceği durumlar ortaya çıkabilir
    date = list()
    comment1 = list()
    comment2 = list() 
    rating =list()
    commentcss = browser.find_elements_by_css_selector("span[jsname='bN97Pc']")
    commentcss2= browser.find_elements_by_css_selector("span[jsname='fbQN7e']")     #tamamını göster
    datexpath = browser.find_elements_by_xpath("(//div[@class='bAhLNe kx8XBd']//span[@class = 'p2TkOb'])")
    ratingxpath = browser.find_elements_by_xpath("//div[@class='pf5lIe']")

    for i in range(0,len(commentcss)):
        date.append(datexpath[i].get_attribute('innerHTML'))
        comment1.append(commentcss[i].get_attribute('innerHTML'))
        comment2.append(commentcss2[i].get_attribute('innerHTML'))
        rating.append(ratingxpath[i+1].get_attribute('innerHTML'))

    for i in range(0,len(comment1)):
        if comment1[i].__contains__('<button class="LkLjZd ScJHi OzU4dc  " jsaction="click:TiglPc"'):
            comment1[i] = comment2[i]           #tamamını göster olanlar ile olmayanları tek bir listede toplama
        start = rating[i].find(ratingString[MAINFOR]) + len(ratingString[MAINFOR])  #yalnızca sayıyı almak için substringing
        rating[i] = rating[i][start:start+1]

    for i in range(0,len(date)):
        s = date[i] +";&;" + rating[i]+";&;"+comment1[i]    #;&; yerine daha complex bir şeyde kullanılabilir ama sonrasında .R da da aynı şekilde güncellenmeli
        print(s,file=review)

review.flush()
review.close()
browser.close();
