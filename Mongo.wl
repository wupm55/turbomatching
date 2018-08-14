(* ::Package:: *)

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

Options[getMongoOneData]={"showID"->False};
getMongoOneData[dbName_,collectionName_,filter_List,fields_List,needReturn_:True,OptionsPattern[]]:=Module[{client,db,coll,dataWithID,needID},
client=myMongoConnect[];
db=client[dbName];
coll=db[collectionName];
needID=TrueQ[OptionValue["showID"]];
dataWithID=MongoCollectionFindOne[coll, Association[filter],Association[(Rule[#,needReturn]&/@(fields))]];
If[dataWithID===Null, Print["No record finded!"],
If[needID,dataWithID,Delete[dataWithID,"_id"]]
]]

getMongoOneData[dbName_,collectionName_,filter_List,OptionsPattern[]]:=Module[{client,db,coll,dataWithID,needID},
client=myMongoConnect[];
db=client[dbName];
coll=db[collectionName];
needID=TrueQ[OptionValue["showID"]];
dataWithID=MongoCollectionFindOne[coll, Association[filter]];
needID=TrueQ[OptionValue["showID"]];
If[dataWithID===Null, Print["No record finded!"],
If[needID,dataWithID,Delete[dataWithID,"_id"]]
]]

getMongoOneData::usage = "getMongoOneData[dbName,collectionName,{filterRules},{fields},showFiedsOrElse,needID:False] from the db.collection find the item which matching the rule of filter like id->001, return the fields data,
if needReturn is false then this field will not selected, others will be returned, if neeedReturn is True, only return the selected one";

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

findId[db_String,col_String,filterRule_List,idName_]:=Module[{client,coll,curs},
client=myMongoConnect[];
coll=client[db,col];
curs=MongoCollectionFind[coll,Association[filterRule],<|idName->True|>];
Delete[#,"_id"]&/@MongoCursorToArray[curs]//Values//Flatten
]

getMongoManyData[dbName_,collName_,filter_,needKeys_]:=Module[{client,coll,curs,all},
client=myMongoConnect[];
coll=client[dbName,collName];curs=MongoCollectionFind[coll,Association[filter],Association[Thread[needKeys->True]]];
all=MongoCursorToArray[curs];
 Delete[#,"_id"]&/@all//Dataset
]

deleteMasterDetail[dbname_,mastercoll_,detailcoll_,idName_,id_]:=Module[{coll,client},
client=myMongoConnect[];
coll=client[dbname,mastercoll];
MongoCollectionDeleteMany[coll,<|idName->id|>]//Print;
coll=client[dbname,detailcoll];
MongoCollectionDeleteMany[coll,<|idName->id|>]//Print;
]



