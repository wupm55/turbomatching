(* ::Package:: *)

Needs["MongoLink`"]


(*datasets toolkit*)
datasetJoin[d1_,d2_]:=Table[Append[d1[[k]],d2[[k]]]//Normal,{k,1,Min[Length[d1],Length[d2]]}]//Dataset
datasetJoin::usage = "datasetJoin[d1,d2] is connect two dataset";

addNewColumn[da_Dataset,item_Rule]:=Table[Append[da[[i]]//Normal,item],{i,1,Length[da]}]//Dataset
addNewColumn::usage = "addNewColumn[dataset,item] add a new key->value to each row of the dataset";

datasetToExcel[data_]:=Module[{k,v,t,f},
k=Keys[data][[1]]//Normal;
v=Values[data]//Normal;
t=Prepend[v,k];
f=Export[SystemDialogInput["FileSave"],t];
Print["File saved in "<>f]
]
datasetToExcel::usage = "datasetToExcel[dataset] save in the excel file";

plotSets[ps_List,opts:OptionsPattern[]]:=Module[{style,data,d,lengend,label},
style=Flatten[Table[Table[ps[[i]]["style"],ps[[i]]["data"]//Length],{i,1,Length[ps]}],1];
data=Table[ps[[i]]["data"],{i,1,Length[ps]}];
d=Flatten[data,1];
label=Column[Table[Style[ps[[i]]["title"],ps[[i]]["style"]],{i,1,Length[ps]}]];
(*Labeled[ListLinePlot[d,PlotStyle->style,PlotRange->Full,LabelStyle->Bold,GridLines->Automatic,options],label,Right]*)
ListLinePlot[d,PlotStyle->style,PlotRange->All,PlotLegends->label,BaseStyle->{FontSize->12,Bold},GridLines->Automatic,Evaluate[FilterRules[{opts},Options[ListLinePlot]]]]
]
plotSets::usage = "plotSets[ps,style] Plot a list of ps, ps is formatted like <|data->{....},title->\"xxx\", style->Red|>,style is optional";
findListInAssocialtion[data_Association]:=List/@(Select[Table[{i->(Head[data[[i]]]===List)},{i,1,Length[data]}]//Association,#&]//Keys)
associateListInAssociation[data_]:=MapAt[Association/@#&,data,findListInAssocialtion[data]]

deleteColumns[a_,keysTobeDleted_List]:=
If[Length[Dimensions[a]]==1,Delete[a,Partition[keysTobeDleted,1]],
Table[Delete[a[[i]]//Normal,Partition[keysTobeDleted,1]],{i,1,Length[a]}]//Dataset]


compareDatasetbyID[ids_List,keys_List, group_:"speed"]:=Module[{datasets,ds,all,selected,n,selectedID},
datasets=analizeEngineData/@getEngineData/@ids;
compareDatasetbyID[datasets,ids,keys, group]
]

compareDatasetbyID[datasets_List,ids_List,keys_List, group_:"speed"]:=Module[{ds,all,selected,selectedID,n},
n=Length[ids];
selected=Table[datasets[[i]][All,keys],{i,1,n}];
ds=Table[addNewColumn[selected[[i]],"id"->ids[[i]]],{i,1,n}];
all=Normal/@ds//Flatten//Dataset;
selectedID=all[All,Append[keys,"id"]];
selectedID[GroupBy[group]]
]

compareTestsData[ids_List,x_,y_, group_:"speed"]:=Module[{data,all,dd,ds,n=Length[ids]},
compareDatasetbyID[ids,{x,y},group]
]
compareTest[ids_List,x_,y_]:=Module[{results,info},
results=analizeEngineData/@getEngineData/@ids;
info=getEngineInfo/@ids;
{Column[{plotData[results,x,y,ids],info}],compareTestsData[ids,x,y,"speed"]}
]

easyCompareTests:=Manipulate[compareTest[tests,x,y],{x,resultKeys},{{y,"torque"},DeleteCases[resultKeys,"speed"]},{tests,ListPicker[#1,testList]&,ControlPlacement->Left}
]


(*Plot toolkit*)
plotData[res_List,x_,y_,ids_List,opts:OptionsPattern[]]:=ListLinePlot[Table[res[[i]][All,{x,y}],{i,1,Length[res]}],FilterRules[{opts},Options[ListLinePlot]],ImageSize->Medium,PlotRange->Full,PlotLegends->ids,AxesLabel->{x,y},BaseStyle->{FontSize->12,Bold},GridLines->Automatic,PlotMarkers->Automatic,PlotLabel->(StringTemplate["Compare `1`-`2`" ][x,y])];
plotData[res_,x_,y_,id_,opts:OptionsPattern[]]:=ListLinePlot[res[All,{x,y}],AxesLabel->{x,y},FilterRules[{opts},Options[ListLinePlot]],PlotRange->Full,BaseStyle->{FontSize->12,Bold},GridLines->Automatic,ImageSize->Medium,PlotMarkers->Automatic,PlotLabel->(StringTemplate["`1`-`2` of `3` " ][x,y,id] )];
plotData::usage = "plotData[results,x,y,ids,style:] list plot {x,y} of the result dataset ,and use the id as the plot Legend, style is to modify the plot style. ";



(*mongo toolkit*)
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

findId[db_String,col_String,filterRule_List,idName_]:=Module[{client,coll,curs},
client=myMongoConnect[];
coll=client[db,col];
curs=MongoCollectionFind[coll,Association[filterRule],<|idName->True|>];
Delete[#,"_id"]&/@MongoCursorToArray[curs]//Values//Flatten
]



(*engine test*)
testList:=getMongoList["turbo","engineTest","testID"]
testList::usage = "testList list all test ids in the database";

getEngineInfo[id_]:=Module[{info},
info=getMongoOneData["turbo","engineTest",{"testID"->id},{"data"},False];
MapAt[Dataset,info,Key["turboConfig"]]//Dataset
]
getEngineInfo::usage = "getEngineInfo[id], get the test infomation about the id";

getEngineData[id_,filter_:True]:=Module[{data,vh,NO0Data,dataFilted},
data=getMongoOneData["turbo","engineTest",{"testID"->id},{"data","VH"},True];
vh=Rule["VH",data["VH"]];
data=addNewColumn[data["data"]//Dataset,vh];
NO0Data=data[Select[(#speed>0&&#mb>0&&#mL>0)&]];
NO0Data=NO0Data[SortBy["speed"]];
dataFilted=NO0Data[GroupBy["speed"]];
     dataFilted=Query[MaximalBy[#torque&]]/@dataFilted//Values//Flatten;
     If[filter,dataFilted,NO0Data]
]
getEngineData::usage = "getEngineData[id,filter:True] get the test data in dataset about the id ";

analizeEngineDataTemp[d_]:=Module[{R=287.04,t},
(* mL: air mass in kg/h ; mb: fuel mass in kg/h ; All pressure in kPa; *)
t["be"]:=#mb/#power*1000&;
t["af"]:=#mL/#mb&;
t["d2"]:=((#dp2ac+100)*1000)/(R*(#t2ac+273.15))&;
t["d1"]:=((#dpfilter+100)*1000)/(R*(#t1+273.15))&;
t["vred"]:=#mL/3600/(((#dpfilter+#p0)*1000)/(R*(#t1+273.15)))&;
t["ve"]:=(#mL/3.6)/(#speed/60*0.5*#VH*((#dp2ac+100)*1000)/(R*(#t2ac+273.15)))&;

t["p2p1"]:=(#dp2bc+#p0)/(#p0+#dpfilter)&;
t["t2th"]:=(#t1+273.15)*((#dp2bc+#p0)/(#p0+#dpfilter))^((1.4-1)/1.4)-273.15&;

t["p3"]=If[#p3==0,(#"p3s1"+#"p3s2")/2,#p3]+#p0&;
t["t3"]=If[#t3==0,(#"t3s1"+#"t3s2")/2,#t3]&; 
t["p4"]=(#p0+#p4)&;
t["p3p4"]=(If[#p3==0,(#"p3s1"+#"p3s2")/2,#p3]+#p0)/(#p0+#p4)&;
d[All,<|"speed"->"speed","torque"->"torque","power"->"power","be"->t["be"],"\[Lambda]"->(#mL/#mb/14.7&),"airFuelRatio"->t["af"],"veff"->t["ve"],"dpAF"->(-#dpfilter&),"dpCooler"->(#dp2bc-#dp2ac&),"dpEx"->"p4","p0"->"p0","p1"->(#p0+#dpfilter&),"p2bc"->(#dp2bc+#p0&),"p2ac"->(#dp2ac+#p0&),"p3"->t["p3"],"p4"->(#p4+#p0&),"t1"->"t1","t2bc"->"t2bc","t2ac"->"t2ac","t3"->t["t3"],"t4"->"t4","p2p1"->t["p2p1"],"vred"->t["vred"],"p3p4"->t["p3p4"],"mL"->"mL","mb"->"mb","mT"->(#mL+#mb&)|> ]
];
analizeEngineData[d_]:=Module[{etaC,etaT,mfp,Cpair=1.006,CpT=1.135,k=1.4,\[Gamma]=1.33,temp,temp2,powerC,powerCReal,powerT,etaTByPower},
(*cpair=1.006kJ/kgK ;  cpT=1.135kJ/kgK kAir=1.4; kExhaust=1.36   *)
temp=analizeEngineDataTemp[d];
etaC=(((#t1+273.15)*(#p2p1)^((k-1)/k)-273.15)-#t1)/(#t2bc-#t1)&;
etaT=(k* #mL (-1+#p2p1^((-1+k)/k)) (273.15+#t1) (-1+\[Gamma]))/((-1+k) #mT (1-(1/#p3p4)^((-1+\[Gamma])/\[Gamma])) (273.15+#t3) \[Gamma])&;
mfp=#mT/3600*Sqrt[#t3+273.15]/(#p3/100)&;
powerC=(#mL *Cpair*(#t1+273.15)*(#p2p1^((k-1)/k)-1)/3600)/((((#t1+273.15)*(#p2p1)^((k-1)/k)-273.15)-#t1)/(#t2bc-#t1))&;
powerCReal=#mL*Cpair*(#t2bc-#t1)/3600&;
powerT=#mT*CpT*(#t3-#t4)/3600&;
temp2=temp[All,<|"\[Eta]C"->etaC,"\[Eta]T"->etaT,"mfp"->mfp,"powerC"->powerC,"powerT"->powerT|>];
datasetJoin[temp,temp2]
]
analizeEngineData::usage = "analizeEngineData[id] return the analized engine data";

resultToInput[result_]:=datasetJoin[Table["op"->j//Association,{j,1,5}]//Dataset,result[All,{"speed","torque","be","\[Lambda]","veff","p0","dpAF","dpCooler","dpEx","t1","t2ac","t3"}]];
resultToInput::usage = "resultToInput[result] in to matching input format";
engineDataKeys=getEngineData[testList[[1]]][[1]]//Keys//Normal;
engineDataKeys::uasage = "engineDataKeys returns the titles of the imported engine test data" ;

resultKeys=(getEngineData[testList[[1]]]//analizeEngineData//Keys)[[1]]//Normal;
resultKeys::uasage = "resultKeys returns the titles of the imported analized test data" ;







easyCompareEngineTestData:=Manipulate[{plotData[analizeEngineData[getEngineData[#]]&/@{testid,testid2},x,y,{testid,testid2},ImageSize->Medium],getEngineInfo[#]&/@{testid,testid2}},{testid,testList,ControlType->PopupMenu},
{testid2,testList,ControlType->PopupMenu},Delimiter,{x,resultKeys},{{y,"torque"},DeleteCases[resultKeys,"speed"]},ControlPlacement->Left]//Quiet
easyCompareEngineTestData::usage = "easyCompareEngineTestData is to compare the engine test data";


easyPlotAll[testid_List,opts:OptionsPattern[]]:=Table[plotData[analizeEngineData[getEngineData[#]]&/@testid,"speed",y,testid,FilterRules[{opts},Options[ListLinePlot]]],
{y,DeleteCases[resultKeys,"speed"]}]
easyPlotAll[testid_String,opts:OptionsPattern[]]:=easyPlotAll[{testid},opts];
easyPlotAll::usage = "easyPlotAll[testids] used to compare test data in all parameters";

plotLugLines[testid_List,opts:OptionsPattern[]]:=Table[plotData[analizeEngineData[getEngineData[#]]&/@testid,"vred",y,testid,FilterRules[{opts},Options[ListLinePlot]]],
{y,{"p2p1","\[Eta]C"}}]
plotLugLines::usage = "plotLugLines[testids] is to plot a list of tests' luglines for comparision"

plotTLugLines[testid_List,opts:OptionsPattern[]]:=Table[plotData[analizeEngineData[getEngineData[#]]&/@testid,"p3p4",y,testid,FilterRules[{opts},Options[ListLinePlot]]],
{y,{"mfp","\[Eta]T"}}]





(*cmap plot toolkit*)
cmapList:=getMongoList["turbo","cmap","name"];
cmapList::usage = "cmapList list all cmap ids in the database"

getCMap[name_]:=Module[{map},
map=getMongoOneData["turbo","cmap",{"name"->name},{"CMapdata"},True]//Values//Flatten;
map//Dataset]
getCMap::usage = "getCMap[cmapid] return the compressor map data in dataset format "

Default[getCMapPlotData]=Magenta;

getCMapPlotData[id_,style_.]:=Module[{map,pressLine,etaLine,surgeLine,nred,uredC,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
map=getMapData[id];
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#piCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#etaCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
surgeLine={map[Select[#MP==1&]][SortBy["KL"]][All,{#Vred,#piCtt}&]//Normal};
nred=Round[map[Select[#MP==3&]][SortBy["KL"]][All,"nred"]//Normal];
uredC=map[Select[#MP==3&]][SortBy["KL"]][All,"uredC"]//Normal;
tip=StringTemplate["`1`[rpm]=`2`[m/s]"]@@@Transpose[{nred,uredC}];
pressLineSpeed=Tooltip@@@Transpose[{pressLine,tip}];
etaLineSpeed=Tooltip@@@Transpose[{etaLine,tip}];
d2=StringTemplate["d2: `` mm"][map[[1]]["d2"]];
<|"pr"-><|"data"->pressLineSpeed,"title"->id,"style"->style|>,
"eta"-><|"data"->etaLineSpeed,"title"->id,"style"->style|>,
"surge"-><|"data"->surgeLine,"title"->d2,"style"->{Dashed,style}|>
|>
]

getCMapPlotData[map_Dataset,id_String,style_.]:=Module[{pressLine,etaLine,surgeLine,nred,uredC,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#piCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#etaCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
surgeLine={map[Select[#MP==1&]][SortBy["KL"]][All,{#Vred,#piCtt}&]//Normal};
nred=Round[map[Select[#MP==3&]][SortBy["KL"]][All,"nred"]//Normal];
uredC=map[Select[#MP==3&]][SortBy["KL"]][All,"uredC"]//Normal;
tip=StringTemplate["`1`[rpm]=`2`[m/s]"]@@@Transpose[{nred,uredC}];
pressLineSpeed=Tooltip@@@Transpose[{pressLine,tip}];
etaLineSpeed=Tooltip@@@Transpose[{etaLine,tip}];
d2=StringTemplate["d2: `` mm"][map[[1]]["d2"]];
<|"pr"-><|"data"->pressLineSpeed,"title"->id,"style"->style|>,
"eta"-><|"data"->etaLineSpeed,"title"->id,"style"->style|>,
"surge"-><|"data"->surgeLine,"title"->d2,"style"->{Dashed,style}|>
|>
]


getCMapPlotData::usage = "getCMapPlotData[id,style.] is to return the cmap info in following format, used for plotCMap comand.<|pr-><|data->pressLine,title->id,style->style|>,
eta-><|data->etaLineSpeed,title->id,style->style|>,
surge-><|data->surgeLine,title->d2,style->{Dashed,style}|>|>";


(*plotCMaps[ids_List,showCont_:False]:=Module[{maps,n,styles},
n=Length[ids];
styles=Take[ColorData[1,"ColorList"],n];
maps=getCMapPlotData[#1,#2]&@@@Transpose[{ids,styles}];
If[showCont,
{Show[Show[Table[plotCMapContour[ids[[i]]],{i,1,n}]],
  plotSets[Flatten[Table[{maps[[i]]["pr"],maps[[i]]["surge"]},{i,1,n}],1],{AxesLabel->{"Vred","p2p1"},ImageSize->Medium,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI"}],ImageSize->Medium,PlotRange->All],
  plotSets[Table[maps[[i]]["eta"],{i,1,n}],{AxesLabel->{"Vred","\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"},ImageSize->Medium,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"}]
},
	{plotSets[Flatten[Table[{maps[[i]]["pr"],maps[[i]]["surge"]},{i,1,n}],1],{AxesLabel->{"Vred","p2p1"},
	ImageSize->Medium,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI"},PlotRange->{All,{1,4}}],
	plotSets[Table[maps[[i]]["eta"],{i,1,n}],{AxesLabel->{"Vred","\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"},ImageSize->Medium,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"}]
	}
]
]*)


(*plotCMaps[id_String,showCont_:False]:=plotCMaps[{id},showCont];
plotCMaps::usage = "plotCMaps[cmapids,showContour:True] to plot a list of compressor map for comparsion";
*)


plotLugLinesOnMaps[testids_List,mapids_List,showCont_:False]:=Module[{mapPlots,mapTPlots},
mapPlots=plotCMaps[mapids,showCont];
mapTPlots=plotTMaps[mapids];
{
{Show[Extract[mapPlots,{1,1,1,1}],Extract[plotLugLines[testids],{1,1}],PlotRange->All],Show[Extract[mapPlots,{1,1,2}],Extract[plotLugLines[testids],2],PlotRange->All]}//Row,
 {Show[Extract[mapTPlots,{1,1}],Extract[plotTLugLines[testids],{1,1}],PlotRange->All],Show[Extract[mapTPlots,2],Extract[plotTLugLines[testids],2],PlotRange->All]}//Row,
 Extract[mapPlots,2]}
]

plotLugLinesOnMaps[testid_String,mapid_String,showCont_:False]:=plotLugLinesOnMaps[{testid},{mapid},showCont]
plotLugLinesOnMaps::usage="plotLugLinesOnMaps[testids_List,mapids_List] to plot lugines on different maps"

plotCMapContour[id_]:=Module[{map,data},
map=getCMap[id];
data=map[All,{"Vred","piCts","etaCts"}]//Values;
ListContourPlot[data,MaxPlotPoints->7,FrameLabel->{"Vred [\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p2p1"},PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI",BaseStyle->{FontSize->12,Bold},
PerformanceGoal->"Speed",PlotTheme->"Business",PlotLegends->None,ImageSize->Medium,InterpolationOrder->3,GridLines->Automatic,ContourStyle->Directive[GrayLevel[0],Opacity[0.5`],Dashed]]
]
plotCMapContour[map_Dataset]:=Module[{data},
data=map[All,{"Vred","piCts","etaCts"}]//Values;
ListContourPlot[data,MaxPlotPoints->7,FrameLabel->{"Vred [\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p2p1"},PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI",BaseStyle->{FontSize->12,Bold},
PerformanceGoal->"Speed",PlotTheme->"Business",PlotLegends->None,ImageSize->Medium,InterpolationOrder->3,GridLines->Automatic,ContourStyle->Directive[GrayLevel[0],Opacity[0.5`],Dashed]]
]

easyTestOnMap=Manipulate[plotLugLinesOnMaps[tests,maps,showCont],{{showCont,True,"Show Contour"},{True,False},ControlPlacement->Top},{tests,ListPicker[#1,testList]&,ControlPlacement->Left},{maps,ListPicker[#1,mapList]&},ControlPlacement->Left];
easyTestOnMap::usage = "easyTestOnMap is to compare test results on different maps ";






mapList:=getMongoList["turbo","mapAdmin","Test_Number"]

getMapData[id_]:=Module[{client,coll,curs,all,rawMap,cmap},
client=myMongoConnect[];
coll=client["turbo","mapData"];
curs=MongoCollectionFind[coll,<|"Test_Number"->id|>,
<|"Line_Point_No"->True,"Line_No"->True,"n_cor_C"->True,"u_cor_C"->True,"u_cor_T"->True,"V_dot_Tcor_C"->True,"m_dot_Tcor_C"->True,"pi_st_C"->True,"pi_tt_C"->True,"eta_ad_tt_C"->True,
"eta_ad_st_C"->True,"d_Cdot"->True,"d_Tdot"->True,
"n_t_tv"->True,"pi_ts_T"->True,"pi_tt_T"->True,"MFP_Tdot"->True,"m_dot_T"->True,"eta_tot_ts_T"->True,"eta_tot_tt_T"->True
|>];
all=MongoCursorToArray[curs];
rawMap=Delete[#,"_id"]&/@all//Dataset;
cmap=rawMap[All,<|"MP"->"Line_Point_No","KL"->"Line_No","nred"->"n_t_tv","uredC"->"u_cor_C","Vred"->"V_dot_Tcor_C","m"->"m_dot_Tcor_C","piCtt"->"pi_tt_C",
"etaCtt"->"eta_ad_tt_C","piCts"->"pi_st_C","etaCts"->"eta_ad_st_C","d2"->"d_Cdot","d5"->"d_Tdot",
"uredT"->"u_cor_T","piTts"->"pi_ts_T","piTtt"->"pi_tt_T","MFP"->"MFP_Tdot","mT"->"m_dot_T","etaTts"->"eta_tot_ts_T","etaTtt"->"eta_tot_tt_T"

|>][SortBy[{"LP","MP"}]];
Sort[cmap,(#1["KL"]*100+#1["MP"]<#2["KL"]*100+#2["MP"])&]
]

getMapInfo[id_]:=Module[{client,coll,noNeeds,allInfo},
allInfo=getMongoOneData["turbo","mapAdmin",{"Test_Number"->id}]//Dataset;
noNeeds={"A_1","A_2","A_3","A_4","dp_2_st_high","dp_3_st_high","dp_oil_low","n_t_high","p_ref_C","Sample Rate (sec)","T_2_dev_high","T_2_tot_high","T_3_dev_high","T_3_tot_high","Test_BenchStr","Test_Customer","Test_Hardware","Test_MappingStr","Test_Operator","Test_Purpose","Test_SampleStr","Test_StartDate","Test_StartTime","T_oil_high","Turbo_Model","Turbo_SizeStr","T_W_high"};
deleteColumns[allInfo,noNeeds]
]


plotCMaps[ids_List,showCont_:False]:=Module[{maps,n,styles,mapdatas},
n=Length[ids];
styles=Take[ColorData[1,"ColorList"],n];
mapdatas=getMapData/@ids;
maps=getCMapPlotData[#1,#2,#3]&@@@Transpose[{mapdatas,ids,styles}];
If[showCont,
{
{Show[Show[Table[plotCMapContour[mapdatas[[i]]],{i,1,n}]],
  plotSets[Flatten[Table[{maps[[i]]["pr"],maps[[i]]["surge"]},{i,1,n}],1],{AxesLabel->{"Vred[\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p2p1"},ImageSize->Medium,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI"}],ImageSize->Medium,PlotRange->All],
  plotSets[Table[maps[[i]]["eta"],{i,1,n}],{AxesLabel->{"Vred","\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"},ImageSize->Medium,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"}]
}//Row,
getMapInfo/@ids
},
	{
	{plotSets[Flatten[Table[{maps[[i]]["pr"],maps[[i]]["surge"]},{i,1,n}],1],{AxesLabel->{"Vred[\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p2p1"},
	ImageSize->Medium,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI"},PlotRange->{All,{1,4}}],
	plotSets[Table[maps[[i]]["eta"],{i,1,n}],{AxesLabel->{"Vred[\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"},ImageSize->Medium,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"}]
	}//Row,getMapInfo/@ids
	}
]
]

deleteMap[id_]:=Module[{coll,client},
client=myMongoConnect[];
coll=client["turbo","mapData"];
MongoCollectionDeleteMany[coll,<|"Test_Number"->id|>]//Print;
coll=client["turbo","mapAdmin"];
MongoCollectionDeleteMany[coll,<|"Test_Number"->id|>]//Print;
]

plotCMaps[id_String,showCont_:False]:=plotCMaps[{id},showCont];

getTMapPlotData[map_Dataset,id_String,style_.]:=Module[{pressLine,etaLine,surgeLine,nred,uredT,pressLineSpeed,etaLineSpeed,tip,d5,pr,eta},
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#piTtt,#MFP}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#piTtt,#etaTtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
nred=Round[map[Select[#MP==3&]][SortBy["KL"]][All,"nred"]//Normal];
uredT=Round[map[Select[#MP==3&]][SortBy["KL"]][All,"uredT"]//Normal];
tip=StringTemplate["`1`[rpm]=`2`[m/s]"]@@@Transpose[{nred,uredT}];
pressLineSpeed=Tooltip@@@Transpose[{pressLine,tip}];
etaLineSpeed=Tooltip@@@Transpose[{etaLine,tip}];
d5=StringTemplate["|d5: `` mm"][map[[1]]["d5"]];
<|"pr"-><|"data"->pressLineSpeed,"title"->id<>d5,"style"->style|>,
"eta"-><|"data"->etaLineSpeed,"title"->id<>d5,"style"->style|>
|>

]

getTMapPlotData[id_String,style_.]:=Module[{map,pressLine,etaLine,surgeLine,nred,uredT,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
map=getMapData[id];
  getTMapPlotData[map,id,style]

]

plotTMaps[ids_List]:=Module[{maps,n,styles,mapdatas,mapinfo},
n=Length[ids];
styles=Take[ColorData[1,"ColorList"],n];
mapinfo=getMapInfo/@ids;
maps=getTMapPlotData[#1,#2]&@@@Transpose[{ids,styles}];

	{plotSets[Table[maps[[i]]["pr"],{i,1,n}],{AxesLabel->{"p3/p4","MFP"},
	ImageSize->Medium,PlotLabel->"Turbine Map PI-MFP"},PlotRange->All],
	plotSets[Table[maps[[i]]["eta"],{i,1,n}],{AxesLabel->{"p3/p4","\!\(\*SubscriptBox[\(\[Eta]\), \(T\)]\)"},ImageSize->Medium,PlotLabel->"Turbine Map PI-\!\(\*SubscriptBox[\(\[Eta]\), \(T\)]\)"}],
	mapinfo
	}

]

plotTMaps[id_String]:=plotTMaps[{id}]







(* ::InheritFromParent:: *)
(**)
