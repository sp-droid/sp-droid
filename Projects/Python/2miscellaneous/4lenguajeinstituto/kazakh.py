cyrillic_to_latin = {
    'А': 'A', 'а': 'a',
    'Ә': 'Ah', 'ә': 'ah',
    'Б': 'B', 'б': 'b',
    'Д': 'D', 'д': 'd',
    'Е': 'Ie', 'е': 'ie',
    'Ф': 'F', 'ф': 'f',
    'Г': 'G', 'г': 'g',
    'Ғ': 'Jj', 'ғ': 'jj',
    'Х': 'J', 'х': 'j',
    'Һ': 'J', 'һ': 'j',
    'И': 'I', 'и': 'i',
    'Й': 'I', 'й': 'i',
    'І': 'E', 'і': 'e',
    'Ж': 'Szh', 'ж': 'szh',
    'К': 'K', 'к': 'k',
    'Л': 'L', 'л': 'l',
    'М': 'M', 'м': 'm',
    'Н': 'N', 'н': 'n',
    'Ң': 'Ng', 'ң': 'ng',
    'О': 'Uo', 'о': 'uo',
    'Ө': 'Ue', 'ө': 'ue',
    'П': 'P', 'п': 'p',
    'Қ': 'Kh', 'қ': 'kh',
    'Р': 'R', 'р': 'r',
    'С': 'S', 'с': 's',
    'Ш': 'Sh', 'ш': 'sh',
    'Т': 'T', 'т': 't',
    'У': 'U', 'у': 'u',
    'Ұ': 'O', 'ұ': 'o',
    'Ү': 'Eh', 'ү': 'eh',
    'В': 'V', 'в': 'v',
    'Ы': 'Ah', 'ы': 'ah',
    'Э': 'Ts', 'э': 'ts',
}

latin_to_cyrillic = {v: k for k, v in cyrillic_to_latin.items()}

def translator(phrase: str, dictionary: dict):

    if phrase=='': return phrase
    translation = ''
    for letter in phrase:
        if letter in dictionary: translation += dictionary[letter]
        else: translation += letter

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
    
var = input('Type A for Cyrillic Kazakh to Latin, type B for the inverse'+'\n')
if var=='A':
    route = 'code/'
    func = lambda phrase: translator(phrase, cyrillic_to_latin)
elif var=='B':
    route = 'decode/'
    func = lambda code: translator(code, latin_to_cyrillic)
else: raise ValueError(f'Wrong input: {var}')

with open(route+'input.txt', 'r', encoding='utf-8') as file:
    contents = file.readlines()

with open(route+'output.txt', 'w', encoding='utf-8') as file:
    for line in contents:
        result = func(line)
        if result == '_': continue
        file.write(result)