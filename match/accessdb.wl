(* ::Package:: *)

Needs["DatabaseLink`"];
access=OpenSQLConnection[JDBC["Microsoft Access(UCanAccess)","C:\\Program Files (x86)\\National Instruments\\LabVIEW 2018\\user.lib\\MapDatabase.mdb"]];
mgTilte2Access=<|"id"->"SolutionID","vh"->"VH","op"->"OP","speed"->"Speed","torque"->"Torque","be"->"be","veff"->"VolEff","\[Lambda]"->"airFuelRatio","dpAF"->"dpAirFilter","dpCooler"->"dpCooler","t2ac"->"T2ac","t3"->"T3","dpEx"->"dpExhaust","wg"->"wg","etaT"->"etaT","hpEGR"->"hp_egr","t2HPEGR"->"t2_hpegr","lpEGR"->"lp_egr","t1LPEGR"->"t1_lpegr","t1"->"T1","p0"->"p0"|>;
reverseKeytoValue[a_Association]:=Thread[Rule[Values[a],Keys[a]]];
accessTitle2mg={"SolutionID"->"id","VH"->"vh","OP"->"op","Speed"->"speed","Torque"->"torque","be"->"be","VolEff"->"veff","airFuelRatio"->"\[Lambda]","dpAirFilter"->"dpAF","dpCooler"->"dpCooler","T2ac"->"t2ac","T3"->"t3","dpExhaust"->"dpEx","wg"->"wg","etaT"->"etaT","hp_egr"->"hpEGR","t2_hpegr"->"t2HPEGR","lp_egr"->"lpEGR","t1_lpegr"->"t1LPEGR","T1"->"t1","p0"->"p0"}


solutionSummaryFileds=SQLColumnNames[access,"SolutionSummary"][[All,2]]
solutionInputFileds=SQLColumnNames[access,"SolutionInputs"][[All,2]];



getSolutionSummary[solutionID_,columns_:solutionSummaryFileds]:=Module[{data,datum},
data=SQLSelect[access,"SolutionSummary",columns,SQLStringMatchQ[SQLColumn["SolutionID"],solutionID]];
datum=data[[1]];
Association[Thread[columns->datum]]//Dataset
]



getSolutionInputs[solutionID_,columns_:solutionInputFileds]:=Module[{data,ds},
data=SQLSelect[access,"SolutionInputs",columns,SQLStringMatchQ[SQLColumn["SolutionID"],solutionID]];
ds=Table[Thread[columns->data[[i]]]//Association,{i,1,Length[data]}]//Dataset;
ds[All,mgTilte2Access]
]


solutionList:=SQLSelect[access,"SolutionSummary",{"SolutionID"}]//Flatten;


saveSolutionSummary[query_:{"SolutionID","Project","SolutionDesc","EngineVolume"}]:=Module[{data},
data=Input/@query;
SQLInsert[access,"SolutionSummary",query,toStringorNumber/@data]
]


askInputs[questions_List]:=Thread[Rule[questions,Table[Input[q],{q,questions}]]]


toStringorNumber[in_]:=If[StringQ[in],in,If[NumberQ[in],in,SymbolName[in]]];
listfy[x_]:= x/.Times->List


dbInsertDataset[conn_,table_String,dataset_Dataset]:=Module[{keys,data},
keys=Keys[Normal[dataset]][[1]];
data=Values[Normal[dataset]];
SQLInsert[conn,table,keys,data]
]


dbInsertInputs[id_String]:=Module[{input,data,values,mginput,temp,accessInput},

input=getEngTestData[id]//analizeEngineData//toInputs;
SQLInsert[access,"SolutionSummary",{"SolutionID","EngineVolume"},{id,input[1,"vh"]}];
mginput=datasetAddColumn[input,"id"->id];
temp=datasetChangeTitle[mginput,mgTilte2Access//Normal];
accessInput=datasetDeleteColumns[temp,{"VH"}];
dbInsertDataset[access,"SolutionInputs",accessInput]
]


associationValueToKey[a_Association]:=Module[{keys,vals},keys=Keys[a];vals=Values[a];Thread[Rule[vals,keys]]]
