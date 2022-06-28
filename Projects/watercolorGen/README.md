Hi, I've had the need to make a good quality watercolor map for a mod in Imperator Rome, one in which the sea floor elevation realistically resembles the land right next to it. So I made a generator that works for ck3 and Imperator. 

A frequent "problem" we modders usually have is that we go to great lengths to make an exquisite heightmap, specially if the map is based on some real part of the world and we can access satellite data, but then we have to hand paint or apply rough shaders to the watercolor map that defines the height below sea level. Sometimes if you are really talented, you can achieve a very good result, but other times (like in my case) one can't always wake up the inner Bob Ross.
 
The github page with instructions: https://github.com/sp-droid/myrepo/tree/main/Projects/watercolorGen
Here you can find 3 compressed splitted files, download all and extract the first one to get the program. To run it, open the watercolor.ipynb with a notebook editor like VSCode or JupyterLab and run cell by cell. Be sure to drop your heightmap in the input folder. For ck3, you have to downscale your heightmap by TWO and color all WATER to BLACK rgb(0,0,0). For imperator the downscale factor is FOUR.

The only parameters you really have to tune correctly are in the **last cell**:
equator = 0 #If 0 is the top, 1 the bottom, where is the equator in your map? The equation for m works with equator = 0, 0.5 or 1. Haven't done the math on the others, but you can ofc
temperate_center = 0.2 #If 0 is the equator, 0.5 the pole. You want this between Gibraltar and the Pirinees, or equivalent in your mod
artic_center = 0.36 #If 0 is the equator, 0.5 the pole. You want this around Helsinki in Finland, or equivalent in your mod
 
**Download the "flowmapGen.rar" (link above), unzip and you will find...**:
- Program folder
- main.ipynb notebook with the code, fine tuned for ck3
- **To open and run the notebook it's recommended to have either JupyterLab, Visual Studio Code or any code editor that can run notebooks**
- Be sure you have the following libraries installed for it to work, aside from **Python3: numpy, PIL, tqdm and joblib**
- An example of input, ck3's heightmap, downscaled by a factor of 4, white (255,255,255) under sea level, black (0,0,0) above + gaussian blur with radius=1, although the last thing is not necessary at all.
![normal_input2](https://user-images.githubusercontent.com/52839915/175094023-2860834a-66a6-487c-8b12-456b15b10aa1.png)
- Rivers can be done automatically too but I didn't need it for now, if u implement it be sure to open an issue/throw a pull request! (rivers near the coast will flow towards it properly tho)

I've attached an example output for ck3, ![image](https://user-images.githubusercontent.com/52839915/175093775-eafbe03c-b025-4ecf-8577-585f2a701617.png) along with ck3's real flowmap to compare ![image](https://user-images.githubusercontent.com/52839915/175093858-a5b5e927-4258-48a5-9421-95446e458170.png)
Then a basic flowmap using a normal map online generator ![image](https://user-images.githubusercontent.com/52839915/175093908-4bb28cbc-3684-4381-b11d-b3f404572cd4.png)
Lastly an example with a world map from imperator ![image](https://user-images.githubusercontent.com/52839915/175093934-d2353273-4e32-46cb-b122-710305fb6fee.png)

This was made in large part thanks to Carlberg & MattAlexi messages about flowmaps, they were useful to design the algorithm  
