
import requests 
import json 
import pandas as pd 

df = pd.read_csv('C:/Users/Alejandra/dssg_files/memphis/blockCoord.csv')
url = 'http://api.walkscore.com/score?format=json&lat={latitude}&lon={longitude}&wsapikey=90a5e9c4497d7bfe03a2ba27518990c8'
df["walkscore"] = ""
for i,row in df.iterrows():
    if row.isnull()["mylongitude"] or row.isnull()["mylatitude"]:
        print("skipping row {}".format(i))
        continue
    r = requests.get(url.format(longitude=row['mylongitude'],
                            latitude=row['mylatitude']))
   
    if not r.ok:
        print('Row  '+ i +'failed!')
        continue
        
    result = json.loads(r.text)
    df.ix[i, "walkscore"] = result["walkscore"] 
    
    if i%5==0:
        print("On location {i}: {score}".format(i=i, score=result["walkscore"]))
        
df.to_csv('C:/Users/Alejandra/dssg_files/memphis/blockCoord_walkscore.csv')