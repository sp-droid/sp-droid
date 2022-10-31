dictionary = {'a':'1', 'b':'2', 'c':'3', 'd':'4', 'e':'5', 'f':'6', 'g':'7', 'h':'8', 'i':'9', 'j':'10', 'k':'11', 'l':'12', 'm':'13', 'n':'14', 'ñ':'15',
'o':'16', 'p':'17', 'q':'18', 'r':'19', 's':'20', 't':'21', 'u':'22', 'v':'23', 'w':'24', 'x':'25', 'y':'26', 'z':'27', ' ':' '}

def coder(phrase,dictionary):

    translation = ""
    for letter in phrase:
        for key in dictionary:
            if key==letter:
                translation += dictionary[key]+"_"
                break
            if key.capitalize()==letter:
                translation += "."+dictionary[key]+"_"
                break

    return translation

def decoder(code,dictionary):

    separated_code = code.split("_")
    phrase = ""
    for element in separated_code:
        element_ = element
        mayus = False
        for character in element:
            if character==".":
                mayus = True
                element_=element.replace(".","")
                break
        for key in dictionary:
            if dictionary[key]==element_:
                if mayus==True:
                    phrase += key.capitalize()
                else:
                    phrase += key
                break

    return phrase

while(True):
    var = input('Escribe A, B o cualquier otra cosa si quieres codificar, decodificar o salir'+'\n')
    if var=="A":
        phrase = input('Escribe la frase a codificar'+'\n')
        result = coder(phrase, dictionary)
        print(result)
    elif var=="B":
        code = input('Escribe la clave a decodificar'+'\n')
        result = decoder(code, dictionary)
        print(result)
    else:
        break

