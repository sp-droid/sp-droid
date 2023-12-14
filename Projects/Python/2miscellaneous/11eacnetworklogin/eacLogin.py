import json
from time import sleep
from pathlib import Path

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.chrome.service import Service as ChromeService
from webdriver_manager.chrome import ChromeDriverManager

jsonPath = Path(__file__).parent.resolve() / 'accountDetails.json'

# One time detail input
if Path(jsonPath).exists():
    with open(jsonPath, mode='r') as file:
        details = json.load(file)

    options = webdriver.ChromeOptions()
    options.add_argument('--ignore-certificate-errors')
    options.add_argument('--ignore-ssl-errors')
    driver = webdriver.Chrome(service=ChromeService(), options=options)

    sleep(0.1)
    driver.get(url='http://www.msftconnecttest.com/redirect')

    XPATHusername = '/html/body/div[1]/div[2]/div/div[1]/div/div[1]/form[1]/fieldset/div/div[1]/input'
    while True:
        sleep(1)
        try:
            print('trying')
            driver.find_element(By.XPATH, XPATHusername).send_keys(details['id'])
            break
        except:
            pass
    sleep(0.02)
    print('got it')

    XPATHpassword = '/html/body/div[1]/div[2]/div/div[1]/div/div[1]/form[1]/fieldset/div/div[3]/input'
    driver.find_element(By.XPATH, XPATHpassword).send_keys(details['password'])
    sleep(0.02)

    XPATHtc = '/html/body/div[1]/div[2]/div/div[1]/div/div[1]/form[1]/div[2]/fieldset/div/div/input'
    checkBox = driver.find_element(By.XPATH, XPATHtc)
    action = ActionChains(driver)
    action.move_to_element(checkBox).click().perform()
    sleep(0.02)

    XPATHsubmit = '/html/body/div[1]/div[2]/div/div[1]/div/div[1]/form[1]/div[3]/div/input'
    driver.find_element(By.XPATH, XPATHsubmit).click()
    sleep(1)

else:
    print('(One time only) please input your account id and password to access ESA Public. You must have internet access for the first time')
    details = {}
    details['id'] = input('Account id:\n')
    details['password'] = input('Account password:\n')

    if details['id'] == '' or details['password'] == '': raise ValueError("Fields can't be empty")

    with open(jsonPath, mode='w') as file:
        json.dump(details, file, indent=2)
        print(f'File saved to: {jsonPath}')
    print('If you want to change them, edit the new accountDetails.json file or delete it and restart')

    driver = webdriver.Chrome(service=ChromeService(ChromeDriverManager().install()))