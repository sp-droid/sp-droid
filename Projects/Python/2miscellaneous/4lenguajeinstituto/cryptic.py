dictionary = {
    'a':'1', 'b':'2', 'c':'3', 'd':'4', 'e':'5', 'f':'6', 'g':'7', 'h':'8', 'i':'9', 'j':'10',
    'k':'11', 'l':'12', 'm':'13', 'n':'14', 'ñ':'15', 'o':'16', 'p':'17', 'q':'18', 'r':'19',
    's':'20', 't':'21', 'u':'22', 'v':'23', 'w':'24', 'x':'25', 'y':'26', 'z':'27', '_':'-', '-':'---', '.':'--',
    '1':'a', '2':'f', '3':'x', '4':'g', '5':'h', '6':'m', '7':'s', '8':'b', '9':'K', '0':'l'}

invDict = {v: k for k, v in dictionary.items()}

def coder(phrase: str, dictionary: dict):

    if phrase=='': return phrase
    translation = ''
    for letter in phrase:
        if letter in dictionary:
            translation += dictionary[letter]+'_'
        elif letter.lower() in dictionary:
            translation += '.'+dictionary[letter.lower()]+'_'
        else:
            translation += letter+'_'

    return translation

def decoder(code: str, invDict: dict):

    if code=='': return code
    separated_code = code.split('_')
    phrase = ''
    for element in separated_code:
        element_ = element
        mayus = False
        for character in element:
            if character=='.':
                mayus = True
                element_=element.replace('.','')
                break
        if element_ in invDict:
            if mayus==True:
                phrase += invDict[element_].capitalize()
            else:
                phrase += invDict[element_]
        else: phrase += element_

    return phrase
    

# while(True):
#     var = input('Escribe A, B o cualquier otra cosa si quieres codificar, decodificar o salir'+'\n')
#     if var=="A":
#         phrase = input('Escribe la frase a codificar'+'\n')
#         result = coder(phrase, dictionary)
#         print(result)
#     elif var=="B":
#         code = input('Escribe la clave a decodificar'+'\n')
#         result = decoder(code, invDict)
#         print(result)
#     else:
#         break
var = input('Type A to code, type B to decode'+'\n')
if var=='A':
    route = 'code/'
    func = lambda phrase: coder(phrase, dictionary)
elif var=='B':
    route = 'decode/'
    func = lambda code: decoder(code, invDict)
else: raise ValueError(f'Wrong input: {var}')

with open(route+'input.txt', 'r') as file:
    contents = file.readlines()

with open(route+'output.txt', 'w') as file:
    for line in contents:
        result = func(line)
        if result == '_': continue
        file.write(result)