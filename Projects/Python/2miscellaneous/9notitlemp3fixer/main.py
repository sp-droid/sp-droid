import os
from pathlib import Path

import eyed3
from bing import Bing
from tqdm import tqdm

tracks = os.listdir('tracks/')

for name in tqdm(tracks):
    author, releaseDate, title = name.split('.mp3')[0].split(' - ')
    
    bing = Bing(f'portrait {author} "{title}"', 1, Path(fr'images/').absolute(), 'off', 60, verbose=False)
    bing.run()

    audio = eyed3.load(fr'tracks/{name}')

    audio.initTag()
    audio.tag.title = title
    audio.tag.artist = author
    audio.tag.track_num = 0
    audio.tag.album = f'{releaseDate}-{author}'
    audio.tag.images.set(3, open('images/Image_1.jpg', 'rb').read(), 'image/jpeg')
    audio.tag.save(version=eyed3.id3.ID3_V2_3)


