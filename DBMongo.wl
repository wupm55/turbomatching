(* ::Package:: *)

<<MongoDBLink`
myMongoConnect[]:=OpenConnection["mongodb://10.24.4.107:27017"];
myMongoConnect::usage = "myMongoConnect[] is used to change the host and port, since all mongo db connection use this function to connect, chage this can chage every usage";

getMongoList[dbName_,collectionName_,fieldName_]:=Module[{conn,db,coll,list},
conn=myMongoConnect[];
db=GetDatabase[conn,dbName];
coll=GetCollection[db,collectionName];
list=FindDistinct[coll, fieldName];
  CloseConnection[conn];
list
]
getMongoList::usage= "getMongoList[dbName,collectionName,fieldName] to list all the disctinct records of that field of that collection of that db";

Default[getMongoOneData]=False;
getMongoOneData[dbName_,collectionName_,filter_List,fields_List,needReturn_,needID_:False]:=Module[{client,db,coll,allFieldsName,dataWithID,complement},
client=myMongoConnect[];
db=GetDatabase[client,dbName];
coll=GetCollection[db,collectionName];
allFieldsName=FindDocuments[coll,filter][[1]]//Keys;
complement=Complement[allFieldsName,fields];
dataWithID=Association[(FindDocuments[coll,filter,"Fields"->If[needReturn,fields,complement]][[1]])];
dataWithID=associateListInAssociation[dataWithID];
CloseConnection[client];
If[needID,dataWithID,Delete[dataWithID,"_id"]]
]
getMongoOneData[dbName_,collectionName_,filter_List,needID_.]:=Module[{client,db,coll,allFieldsName,dataWithID,complement},
client=myMongoConnect[];
db=GetDatabase[client,dbName];
coll=GetCollection[db,collectionName];
allFieldsName=FindDocuments[coll,filter][[1]]//Keys;
dataWithID=Association[(FindDocuments[coll,filter,"Fields"->allFieldsName][[1]])];
CloseConnection[client];
If[needID,dataWithID,Delete[dataWithID,"_id"]]
]
getMongoOneData::usage = "getMongoOneData[dbName,collectionName,filter_Rule,needID:False] from the db.collection find the item which matching the rule of filter like id->001, return the all the fields data. \n
getMongoOneData[dbName,collectionName,filter_Rule,fields,?needReturn,needID:False] from the db.collection find the item which matching the rule of filter like id->001, return the fields data,
if needReturn is false then this field will not selected, others will be returned, if neeedReturn is True, only return the selected one";



Default[getMongoManyData]=False;
getMongoManyData[dbName_,collectionName_,filter_List,fields_List,needReturn_:True,needID_.]:=Module[{client,db,coll,allFieldsName,dataWithID,dataNoID,complement},
client=myMongoConnect[];
db=GetDatabase[client,dbName];
coll=GetCollection[db,collectionName];
allFieldsName=FindDocuments[coll,filter][[1]]//Keys;
complement=Complement[allFieldsName,fields];
dataWithID=Dataset[Association/@FindDocuments[coll,filter,"Fields"->If[needReturn,fields,complement]]];
dataNoID=deleteColumns[dataWithID,{"_id"}];
CloseConnection[client];
If[needID,dataWithID,dataNoID]
]

modifyOneData[dbName_,collectionName_,filter_List,newValueRule_List]:=Module[{client,db,coll,allFieldsName,ok},
client=myMongoConnect[];
db=GetDatabase[client,dbName];
coll=GetCollection[db,collectionName];
ok=UpdateDocument[coll, filter, {"$set" -> newValueRule}];
ok
]
modifyOneData::usage = "modifyOneData[dbname,collection,{filterRule},{newValuesRule}] ";

insertMongoData[dbName_,collectionName_,dataset_]:=Module[{conn,db,coll,list,ok,doc},
conn=myMongoConnect[];
db=GetDatabase[conn,dbName];
coll=GetCollection[db,collectionName];
doc=dataset//Normal//Normal;
ok=InsertDocument[coll,  #] &/@ doc;
 CloseConnection[conn];
Print[ok]
]

