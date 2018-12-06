(* ::Package:: *)

Needs["MongoLink`"]
myMongoConnect[]=MongoConnect["mongodb://10.24.4.107:27017"];
myMongoConnect::usage = "myMongoConnect[] is used to change the host and port, since all mongo db connection use this function to connect, chage this can chage every usage";
client=myMongoConnect[];



mgUpdate[dbName_String,collectionName_String,filter_Rule,info:(_Rule|_List)]:=Module[{coll},
coll=client[dbName,collectionName];
MongoCollectionUpdateOne[coll,<|filter|>,<|"$set"-><|info|>|>]
]



mgList[dbName_,collectionName_,fieldName_,filter_]:=Module[{db,coll},
db=client[dbName];
coll=db[collectionName];
MongoCollectionDistinct[coll,fieldName,filter]
];

mgList[dbName_,collectionName_,fieldName_]:=Module[{db,coll},
db=client[dbName];
coll=db[collectionName];
MongoCollectionDistinct[coll,fieldName]
]
mgList::usage= "mgList[dbName,collectionName,fieldName] to list all the disctinct records of that field of that collection of that db";




Options[mgFindOne]={"showID"->False};
mgFindOne[dbName_,collectionName_,filter_List,fields_List,needReturn_:True,OptionsPattern[]]:=Module[{db,coll,dataWithID,needID},

db=client[dbName];
coll=db[collectionName];
needID=TrueQ[OptionValue["showID"]];
dataWithID=MongoCollectionFindOne[coll, Association[filter],Association[(Rule[#,needReturn]&/@(fields))]];
dataWithID=dataWithID/.x_String/;StringLength[x]>10000->PopupWindow["Click to Show",myDeserialize[x]];
If[dataWithID===Null, Print["No record finded!"],
If[needID,dataWithID,Delete[dataWithID,"_id"]]
]
]

mgFindOne[dbName_,collectionName_,filter_List,OptionsPattern[]]:=Module[{db,coll,dataWithID,needID},
db=client[dbName];
coll=db[collectionName];
needID=TrueQ[OptionValue["showID"]];
dataWithID=MongoCollectionFindOne[coll, Association[filter]];
dataWithID=dataWithID/.x_String/;StringLength[x]>10000->PopupWindow["Click to Show",myDeserialize[x]];
needID=TrueQ[OptionValue["showID"]];
If[dataWithID===Null, Print["No record finded!"],
If[needID,dataWithID,Delete[dataWithID,"_id"]]
]]

mgFindOne::usage = "mgFindOne[dbName,collectionName,{filterRules},{fields},showFiedsOrElse,needID:False] from the db.collection find the item which matching the rule of filter like id->001, return the fields data,
if needReturn is false then this field will not selected, others will be returned, if neeedReturn is True, only return the selected one";



UpdateOneData[dbName_,collectionName_,filter_List,newValueRule_List]:=Module[{db,coll,ok},
db=client[dbName];
coll=db[collectionName];
ok=MongoCollectionUpdateOne[coll, Association[filter],
<|"$set"->Association[newValueRule]|>
];
Print[ok]
]
UpdateOneData::usage = "UpdateOneData[dbName,collectionName,filter,newValue] is Update the selected item with new value rule";




findId[db_String,col_String,filterRule_List,idName_]:=Module[{coll,curs},
coll=client[db,col];
curs=MongoCollectionFind[coll,Association[filterRule],<|idName->True|>];
Delete[#,"_id"]&/@MongoCursorToArray[curs]//Values//Flatten
]

getMongoManyData[dbName_,collName_,filter_,needKeys_]:=Module[{coll,curs,all},
coll=client[dbName,collName];curs=MongoCollectionFind[coll,Association[filter],Association[Thread[needKeys->True]]];
all=MongoCursorToArray[curs];
 Delete[#,"_id"]&/@all//Dataset
]

deleteMasterDetail[dbname_,mastercoll_,detailcoll_,idName_,id_]:=Module[{coll,client},

coll=client[dbname,mastercoll];
MongoCollectionDeleteMany[coll,<|idName->id|>]//Print;
coll=client[dbname,detailcoll];
MongoCollectionDeleteMany[coll,<|idName->id|>]//Print;
]


associationToMongo[dbName_,collectionName_,association_]:=Module[{db,coll,ok},
db=client[dbName];
coll=db[collectionName];
ok=MongoCollectionInsert[coll,association]
]


findMap[filter_Rule|filter_List]:=Module[{coll,records,array},
coll=client["turbo","map"];
records=MongoCollectionFind[coll,<|filter|>,<|"Test_Number"->1,"_id"->0|>]//MongoCursorToArray;
Values/@records//Flatten
]


mySerialize[obj_]:=Compress[BinarySerialize[obj]];
myDeserialize[bin_String]/;StringLength[bin]>10000:=bin//Uncompress//BinaryDeserialize;


mgInsertInTurbo[collName_String,data:(_Rule|_List)]:=Module[{coll},
coll=client["turbo",collName];
MongoCollectionInsert[coll,data//Association]
];



mgInsert[dbName_String,collName_String,data:(_Rule|_List)]:=Module[{coll},
coll=client[dbName,collName];
MongoCollectionInsert[coll,data//Association]
];


mgFind[dbName_String,col_String,filter:(_Rule|_List)]:=Module[{coll,curs},
coll=client[dbName,col];
curs=MongoCollectionFind[coll,Association[filter],<|"_id"->0|>];
MongoCursorToArray[curs]
]
mgFind[dbName_String,col_String]:=Module[{coll,curs},
coll=client[dbName,col];
curs=MongoCollectionFind[coll];
MongoCursorToArray[curs]
]


mgFindInTurbo[col_String,filter:(_Rule|_List)]:=Module[{coll,curs},
coll=client["turbo",col];
curs=MongoCollectionFind[coll,{
 {Association[filter], <|"_id"->0|>}
}];
MongoCursorToArray[curs]
]

mgFindInTurbo[col_String,filter:(_Rule|_List)]:=Module[{coll,curs},
coll=client["turbo",col];
curs=MongoCollectionFind[coll,Association[filter],<|"_id"->0|>];
MongoCursorToArray[curs]
]



like[x_String]:=<|"$regex"->x,"$options"->"$i"|>;





mgUnset[dbName_String,collectionName_String,filter_Rule,info:_Rule]:=Module[{coll},
coll=client[dbName,collectionName];
MongoCollectionUpdateOne[coll,Association[filter],
Association["$unset"->Association[Keys[info]->1]]]
]







