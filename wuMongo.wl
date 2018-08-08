(* ::Package:: *)

BeginPackage["wuMongo`"]


Needs["MongoLink`"]
myMongoConnect[]=MongoConnect["mongodb://10.24.4.107:27017"];
myMongoConnect::usage = "myMongoConnect[] is used to change the host and port, since all mongo db connection use this function to connect, chage this can chage every usage";

getMongoList[dbName_,collectionName_,fieldName_]:=Module[{client,db,coll},
client=myMongoConnect[];
db=client[dbName];
coll=db[collectionName];
MongoCollectionDistinct[coll,fieldName]
]
getMongoList::usage= "getMongoList[dbName,collectionName,fieldName] to list all the disctinct records of that field of that collection of that db";

getMongoOneData[dbName_,collectionName_,filter_List,fields_List,needReturn_,needID_:False]:=Module[{client,db,coll,dataWithID},
client=myMongoConnect[];
db=client[dbName];
coll=db[collectionName];
dataWithID=MongoCollectionFindOne[coll, Association[filter],Association[(Rule[#,needReturn]&/@(fields))]];
If[dataWithID===Null, Print["No record finded!"],
If[needID,dataWithID,Delete[dataWithID,"_id"]]
]]

getMongoOneData::usage = "getMongoOneData[dbName,collectionName,{filterRules},{fields},showFiedsOrElse,needID:False] from the db.collection find the item which matching the rule of filter like id->001, return the fields data,
if needReturn is false then this field will not selected, others will be returned, if neeedReturn is True, only return the selected one";

getMongoOneData[dbName_,collectionName_,filter_List,needID_:False]:=Module[{client,db,coll,dataWithID},
client=myMongoConnect[];
db=client[dbName];
coll=db[collectionName];
dataWithID=MongoCollectionFindOne[coll, Association[filter]];
If[dataWithID===Null, Print["No record finded!"],
	If[needID,dataWithID,Delete[dataWithID,"_id"]]
	]
]
getMongoOneData::usage = "getMongoOneData[dbName,collectionName,{filterRule},needID:False] from the db.collection find the item which matching the rule of filter like id->001, return the all the fields data";

modifyOneData[dbName_,collectionName_,filter_List,newValueRule_List]:=Module[{client,db,coll,ok},
client=myMongoConnect[];
db=client[dbName];
coll=db[collectionName];
ok=MongoCollectionUpdateOne[coll, Association[filter],
<|"$set"->Association[newValueRule]|>
];
Print[ok]
]
modifyOneData::usage = "modifyOneData[dbName,collectionName,filter,newValue] is modify the selected item with new value rule";



EndPackage[]
