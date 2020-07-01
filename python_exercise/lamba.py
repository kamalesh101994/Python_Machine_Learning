"""def mul_by_num(n):
    return lambda a:a*n
x=mul_by_num(5)
print(x(3))"""


"""def fibo (n):
    x=[]
    x = x[A[n-1]+A[n-2]]
    A.append(x)
    print(A[n])

A=[0,1]
fibo(1)"""

"""name = str(input("enter the website to be checked: "))
import urllib.request
with urllib.request.urlopen(name) as res:
    if res.getcode() == 200:
        print("the webpage is available")
    else:
        print("invalid webpage")"""

"""name = str("https://openweathermap.org/current")
import urllib.request
response = urllib.request.urlopen(name)
html = response.read()
html = str(html)
print(html)"""

'''import requests
api_token = 'b35975e18dc93725acb092f7272cc6b8'
headers = {'Content-Type': 'application/json',
           'Authorization': 'b35975e18dc93725acb092f7272cc6b8'}
URL = "http://api.openweathermap.org/data/2.5/weather?q=London"
api_url='{0}account'.format(URL)
print(URL)
response = requests.get(URL, headers=headers)
print(response.status_code)'''

'''import requests
import json
headers = {'Content-Type': 'application/json',
           'Authorization': 'b35975e18dc93725acb092f7272cc6b8'}
response = requests.get("http://api.openweathermap.org/data/2.5/weather?q=London&appid=b35975e18dc93725acb092f7272cc6b8")
print(response.status_code)
final = response.json()
print(final)
print("temperature :" , final['main']['temp'])
print("wind speed :" , final['wind']['speed'])
print(str(("description :",  final['weather']['description'])))
print("weather :", final['weather']['main'])'''


'''import requests
response=requests.get("http://api.openweathermap.org/data/2.5/weather?q=London&appid=b35975e18dc93725acb092f7272cc6b8")
with open('test.html', 'w') as f:
    f.write(response.text)
with open('test.html') as file:
    data = file.read()
    print(data)'''

'''f = open("textdoc1", "a")
f.write("Now the file has more content!")
f.close()
f = open("textdoc1","r")
print(f.read())'''



'''f = open("test1", "x")
f.write("this is the first line of the doc")
f.close()
f = open("test2", "r")
print(f.read())'''

'''FileExistsError: [Errno 17] File exists: 'test1'''
'''ValueError: invalid mode: 'W'''''
'''try:
    f = open("test1", "w")
    print(f.read())
except:
    ValueError: print ("permission issue")
finally:
    f.close()'''

'''def foo(i):
    l = [1,2,3]
    try:
        assert i >= 1
        return l[i]
    except TypeError,e:
        print "dealing with TypeError"
    except IndexError, e:
        print "dealing with IndexError"
    except:
        print
        "oh dear"
    finally:
       print "the end"'''

'''class NametooshortError(ValueError):
    pass
strr = str(input("enter your name:"))
if len(strr) < 10:
    raise NametooshortError(name)'''

























