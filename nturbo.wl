(* ::Package:: *)

(*mongo toolkit*)

SetDirectory["D:\\wolf\\github\\Github"];


<<Mongo`



unit1KeyOfDataset[dataset_,key_,unit_]:=Association/@Thread[key->(dataset[Quantity[#,unit]&,key]//Normal)]//Dataset;
unitKeysOfDataset[dataset_,key_List,unit_List]:=datasetJoin[Table[unit1KeyOfDataset[dataset,key[[i]],unit[[i]]],{i,1,Length[key]}]];



(*datasets toolkit*)

findListInAssocialtion[data_Association]:=List/@(Select[Table[{i->(Head[data[[i]]]===List)},{i,1,Length[data]}]//Association,#&]//Keys);
findDatasetInAssocialtion[data_Association]:=List/@(Select[Table[{i->(Head[data[[i]]]===Dataset)},{i,1,Length[data]}]//Association,#&]//Keys);
associateListInAssociation[data_]:=MapAt[Association/@#&,data,findListInAssocialtion[data]]

flattenDataset[dataset_Dataset]:=Module[{info,pos,normalParts,subPartsKey,subParts,f},
info=dataset//Normal;
pos=findDatasetInAssocialtion[info];
normalParts=Delete[info,Extract[Keys[info],pos]];
subPartsKey=Extract[Keys[info],pos]//Normal;
subParts=Normal/@Extract[info,pos];
f=StringJoin[subPartsKey<>".",#]&;
subParts=KeyMap[f, subParts[[1]]];
Normal/@{normalParts,subParts}//Flatten//Association//Dataset
]
flattenDataset::usage = "flattenDataset[dataset] make the inside datasets into normal datasets"

datasetJoin[d1_,d2_]:=Table[Append[d1[[k]],d2[[k]]]//Normal,{k,1,Min[Length[d1],Length[d2]]}]//Dataset
datasetJoin::usage = "datasetJoin[d1,d2] is connect two dataset";
datasetJoin[data_List]:=Fold[datasetJoin,data[[1]],data[[2;;-1]]]

datasetConnect[datasets_List]:=Dataset[Normal/@datasets];
datasetConnect::usage = "datasetConnect[datasets] used to combine of datasets which has same stucture(titles)"

addNewColumn[da_Dataset,item_Rule]:=Table[Append[da[[i]]//Normal,item],{i,1,Length[da]}]//Dataset
addNewColumn::usage = "addNewColumn[dataset,item] add a new key->value to each row of the dataset";

datasetToExcel[data_]:=Module[{k,v,t,f},
k=Keys[data][[1]]//Normal;
v=Values[data]//Normal;
t=Prepend[v,k];
f=Export[SystemDialogInput["FileSave"],t];
Print["File saved in "<>f]
];
datasetToExcel::usage = "datasetToExcel[dataset] save in the excel file";

compareDatasets[datasets_List,ids_List,keys_List, group_]:=Module[{ds,all,selected,selectedID,n},
n=Length[ids];
selected=Table[datasets[[i]][All,keys],{i,1,n}];
ds=Table[addNewColumn[selected[[i]],"id"->ids[[i]]],{i,1,n}];
all=Normal/@ds//Flatten//Dataset;
selectedID=all[All,Append[keys,"id"]];
selectedID[GroupBy[group]]
];
compareDatasets::usage = "compareDatasets[datasets,ids,keys,group] is used to compare two datasets which has same structure, ans choose keys to comapre,ids is used identify which sets belongs to which id, group to group the data for easy compare"

deleteColumns[a_,keysTobeDleted_List]:=If[Length[Dimensions[a]]==1,Delete[a,Partition[keysTobeDleted,1]],
Table[Delete[a[[i]]//Normal,Partition[keysTobeDleted,1]],{i,1,Length[a]}]//Dataset]
deleteColumns::usage = "deleteColumns[dataset, keysTobeDelete] used delete the  columns of a Dataset or an Association, specified by the kes";





(* ::InheritFromParent:: *)
(**)


compareDatasetbyID[ids_List,keys_List, group_:"speed"]:=Module[{datasets,ds,all,selected,n,selectedID},
datasets=analizeEngineData/@getEngineData/@ids;
compareDatasets[datasets,ids,keys, group]
]

compareTestsData[ids_List,x_,y_, group_:"speed"]:=Module[{data,all,dd,ds,n=Length[ids]},
compareDatasetbyID[ids,{x,y},group]
]



compareTest[ids_List,x_,y_]:=Module[{results,info,pane},
results=analizeEngineData/@getEngineData/@ids;
info=datasetConnect[getEngineInfo/@ids];
pane={plotData[results,x,y,ids],compareTestsData[ids,x,y,"speed"],info}
]
compareTest[id_String,x_,y_]:=compareTest[{id},x,y];
compareTest::usage = "compareTest[testids,x,y,showData:False] to plot the test results of different test id and show the compared data" 

easyCompareTests:=Manipulate[{compareTest[tests,x,y],If[tests!={},Thread[findTestIdPos[tests]->tests],""]},{x,resultKeys,ControlPlacement->Left},{{y,"torque"},DeleteCases[resultKeys,"speed"],ControlPlacement->Left},
{tests,ListPicker[#1,testList]&,ControlPlacement->Top}]



(*Plot toolkit*)
plotData[res_List,x_,y_,ids_List,opts:OptionsPattern[]]:=ListLinePlot[Table[res[[i]][All,{x,y}],{i,1,Length[res]}],Evaluate[FilterRules[{opts},Options[ListLinePlot]]],ImageSize->Medium,PlotRange->Full,PlotLegends->Placed[ids,Below],AxesLabel->{x,y},BaseStyle->{FontSize->12,Bold},GridLines->Automatic,PlotMarkers->Automatic,PlotLabel->(StringTemplate["Compare `1`-`2`" ][x,y])];
plotData[res_,x_,y_,id_,opts:OptionsPattern[]]:=ListLinePlot[res[All,{x,y}],AxesLabel->{x,y},Evaluate[FilterRules[{opts},Options[ListLinePlot]]],ImageSize->Medium,PlotRange->Full,BaseStyle->{FontSize->12,Bold},GridLines->Automatic,PlotMarkers->Automatic,PlotLabel->(StringTemplate["`1`-`2` of `3` " ][x,y,id] )];
plotData::usage = "plotData[results,x,y,ids,style:] list plot {x,y} of the result dataset ,and use the id as the plot Legend, style is to modify the plot style. ";

plotSets[ps_List,opts:OptionsPattern[]]:=Module[{style,data,d,lengend,label},
style=Flatten[Table[Table[ps[[i]]["style"],ps[[i]]["data"]//Length],{i,1,Length[ps]}],1];
data=Table[ps[[i]]["data"],{i,1,Length[ps]}];
d=Flatten[data,1];
label=Column[Table[Style[ps[[i]]["title"],ps[[i]]["style"]],{i,1,Length[ps]}]];
ListLinePlot[d,PlotStyle->style,PlotRange->All,PlotLegends->Placed[label,Below],BaseStyle->{FontSize->12,Bold},GridLines->Automatic,ImageSize->Medium,Evaluate[FilterRules[{opts},Options[ListLinePlot]]]]
]

plotSets::usage = "plotSets[ps,style] Plot a list of ps, ps is formatted like <|data->{....},title->\"xxx\", style->Red|>,style is optional";





wuBarChart[data_List,legend_List,rowLabel_List,cLabel_List,y_,showrl_:False]:=BarChart[data,ChartLegends->Placed[legend,Below],AxesLabel->{None,y},ChartLabels->{rowLabel,{If[showrl,Placed[cLabel,Center,Rotate[#,90 Degree]&]]}},LabelingFunction->(Placed[Style[NumberForm[#,4],15,Bold],Above]&),ImageSize->Large,BaseStyle->{FontSize->12,Bold}];

barGroupedDataset[di_Dataset,y_,idTitle_String]:=Module[{data,rowLabel,idLegend},
rowLabel=di[Keys]//Normal;(*get the grouped values*)
data=Table[di[[i]][All,y]//Normal,{i,1,Length[di]}];
idLegend=DeleteDuplicates[Normal[Flatten[Values[di]][All,idTitle]]];
wuBarChart[data,idLegend,rowLabel,idLegend,y]
]



barTestData[ids_List,x_,y_,compareWhichSpeed_:""]:=Module[{di,pos,groups,i},
di=compareTestsData[ids,x,y];
groups=di[Keys]//Normal;
pos=Position[groups,#]&/@Flatten[{compareWhichSpeed}];
pos=Flatten[DeleteCases[pos,{}]]//Sort;
If[compareWhichSpeed=="",pos=All,];
If[pos=={},pos=All,];
di=Take[di,pos];
barGroupedDataset[di,y,"id"]
]



easyBarTests:=Manipulate[barTestData[tests,x,y,g],{x,resultKeys,ControlPlacement->Left},{{y,"torque"},DeleteCases[resultKeys,"speed"],ControlPlacement->Left},{g,InputField,ControlPlacement->Left},
{tests,ListPicker[#,testList]&,ControlPlacement->Top}]


(*engine test*)
testList:=getMongoList["turbo","engineTest","testID"]
testList::usage = "testList list all test ids in the database";
findTestIdPos[name_]:=Extract[Flatten[Position[testList,name]],1];
findTestIdPos[names_List]:=findTestIdPos/@names;
getEngineInfo[id_]:=Module[{info,dataset},
info=getMongoOneData["turbo","engineTest",{"testID"->id},{"data"},False];
dataset=MapAt[Dataset,info,Key["turboConfig"]]//Dataset;
flattenDataset[dataset]
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
engineDataKeys=Extract[getEngineData[testList[[1]]],1]//Keys//Normal;
engineDataKeys::uasage = "engineDataKeys returns the titles of the imported engine test data" ;

resultKeys=Extract[(getEngineData[testList[[1]]]//analizeEngineData//Keys),1]//Normal;
resultKeys::uasage = "resultKeys returns the titles of the imported analized test data" ;










easyPlotAll[testid_List,opts:OptionsPattern[]]:=Table[plotData[analizeEngineData[getEngineData[#]]&/@testid,"speed",y,testid,opts],
{y,DeleteCases[resultKeys,"speed"]}]
easyPlotAll[testid_String,opts:OptionsPattern[]]:=easyPlotAll[{testid},opts];
easyPlotAll::usage = "easyPlotAll[testids] used to compare test data in all parameters";

plotLugLines[testid_List,opts:OptionsPattern[]]:=Table[plotData[analizeEngineData[getEngineData[#]]&/@testid,"vred",y,testid,opts],
{y,{"p2p1","\[Eta]C"}}]
plotLugLines::usage = "plotLugLines[testids] is to plot a list of tests' luglines for comparision"

plotTLugLines[testid_List,opts:OptionsPattern[]]:=Table[plotData[analizeEngineData[getEngineData[#]]&/@testid,"p3p4",y,testid,opts],
{y,{"mfp","\[Eta]T"}}]








Default[getCMapPlotData]=Magenta;

getCMapPlotData[id_,style_.]:=Module[{map,pressLine,etaLine,surgeLine,nred,uredC,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
map=getMapData[id];
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#piCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#etaCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
surgeLine={map[Select[#MP==1&]][SortBy["KL"]][All,{#Vred,#piCtt}&]//Normal};
nred=Round[map[Select[#MP==3&]][SortBy["KL"]][All,"nred"]//Normal];
uredC=map[Select[#MP==3&]][SortBy["KL"]][All,"uredC"]//Normal;
tip=StringTemplate["`1`[rpm]=`2`[m/s]"]@@@Transpose[{nred,uredC}];
pressLine=DeleteCases[pressLine,{}];
etaLine=DeleteCases[etaLine,{}];
pressLineSpeed=Tooltip@@@Transpose[{pressLine,tip}];
etaLineSpeed=Tooltip@@@Transpose[{etaLine,tip}];
d2=StringTemplate["d2: `` mm"][map[[1]]["d2"]];
<|"pr"-><|"data"->pressLineSpeed,"title"->id,"style"->style|>,
"eta"-><|"data"->etaLineSpeed,"title"->id,"style"->style|>,
"surge"-><|"data"->surgeLine,"title"->d2,"style"->{Dashed,style}|>
|>
]

getCMapPlotData[map_Dataset,id_String,style_]:=Module[{pressLine,etaLine,surgeLine,nred,uredC,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#piCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#etaCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
surgeLine={map[Select[#MP==1&]][SortBy["KL"]][All,{#Vred,#piCtt}&]//Normal};
nred=Round[map[Select[#MP==3&]][SortBy["KL"]][All,"nred"]//Normal];
uredC=map[Select[#MP==3&]][SortBy["KL"]][All,"uredC"]//Normal;
pressLine=DeleteCases[pressLine,{}];
etaLine=DeleteCases[etaLine,{}];
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








plotLugLinesOnMaps[testids_List,mapids_List,showContour_,opt:OptionsPattern[]]:=Module[{mapinfo,testInfo,mapPlots,mapTPlots,testC,testT,c,t},
mapPlots=plotCMaps[mapids,showContour,opt][[1]];
mapinfo=plotCMaps[mapids,showContour,opt][[2]];
testInfo=datasetConnect[getEngineInfo/@testids];
mapTPlots=plotTMaps[mapids,opt][[1]];
testC=plotLugLines[testids];
testT=plotTLugLines[testids];
c={Show[Extract[mapPlots,{1,1}],Extract[plotLugLines[testids],{1,1}],PlotRange->All],Show[Extract[mapPlots,{1,2}],Extract[testC,2],PlotRange->All]};
t={Show[Extract[mapTPlots,{1,1}],Extract[plotTLugLines[testids],{1,1}],PlotRange->All],Show[Extract[mapTPlots,{1,2}],Extract[testT,2],PlotRange->All]};
{TabView[{"Compressor Side"->c,"Turbine Side"->t}],
 OpenerView[{testInfo,mapinfo}]
}//Column
 
]

plotLugLinesOnMaps[testid_String,mapid_String,showContour_]:=plotLugLinesOnMaps[{testid},{mapid},showContour]
plotLugLinesOnMaps::usage="plotLugLinesOnMaps[testids_List,mapids_List] to plot lugines on different maps"


plotCMapContour[map_Dataset]:=Module[{data},
data=map[All,{"Vred","piCts","etaCts"}]//Values;
ListContourPlot[data,MaxPlotPoints->7,FrameLabel->{"Vred [\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p2p1"},PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI",BaseStyle->{FontSize->12,Bold},
PerformanceGoal->"Speed",PlotTheme->"Business",PlotLegends->None,InterpolationOrder->3,GridLines->Automatic,ContourStyle->Directive[GrayLevel[0],Opacity[0.5`],Dashed]]
]

easyTestOnMap=Manipulate[plotLugLinesOnMaps[tests,maps,showContour],{{showContour,True,"Show Contour"},{True,False},ControlPlacement->Top},{tests,ListPicker[#1,testList]&,ControlPlacement->Left},{maps,ListPicker[#1,mapList]&},ControlPlacement->Left];
easyTestOnMap::usage = "easyTestOnMap is to compare test results on different maps ";





mapList:=getMongoList["turbo","map","Test_Number"];
cmapList=getMongoList["turbo","map","Test_Number",<|"$or"->{<|"usage"->"c"|>,<|"usage"->"ct"|>}|>];

tmapList=getMongoList["turbo","map","Test_Number",<|"$or"->{<|"usage"->"t"|>,<|"usage"->"ct"|>}|>];
getTMapidsInGroup[id_]:=Module[{mapinfo,group},
mapinfo=getMapInfo[id];
If[KeyExistsQ[mapinfo,"group"] && mapinfo["group"]!="",
getMongoManyData["turbo","map",{"group"->mapinfo["group"]},{"Test_Number"}]//Normal//Values//Flatten,
id]
]

getMapData[id_]:=Module[{titles,client,coll,curs,all,rawMap,map},
titles={"Line_Point_No","Line_No","n_cor_C","u_cor_C","u_cor_T","V_dot_Tcor_C","m_dot_Tcor_C","pi_st_C","pi_tt_C","eta_ad_tt_C","eta_ad_st_C","d_C","d_T","n_t_tv","pi_ts_T","pi_tt_T","MFP_T","m_dot_T","eta_tot_ts_T","eta_tot_tt_T"};
rawMap=getMongoManyData["turbo","map",{"Test_Number"->id},StringJoin["data.",#]&/@titles]//Values//Normal//Flatten//Dataset;
map=rawMap[All,<|"MP"->"Line_Point_No","KL"->"Line_No","nred"->"n_t_tv","uredC"->"u_cor_C","Vred"->"V_dot_Tcor_C","m"->"m_dot_Tcor_C","piCtt"->"pi_tt_C",
"etaCtt"->"eta_ad_tt_C","piCts"->"pi_st_C","etaCts"->"eta_ad_st_C","d2"->"d_C","d5"->"d_T",
"uredT"->"u_cor_T","piTts"->"pi_ts_T","piTtt"->"pi_tt_T","MFP"->"MFP_T","mT"->"m_dot_T","etaTts"->"eta_tot_ts_T","etaTtt"->"eta_tot_tt_T"
|>];
Sort[map,(#1["KL"]*100+#1["MP"]<#2["KL"]*100+#2["MP"])&]
]

getMapInfo[id_]:=Module[{client,coll,noNeeds,allInfo},
allInfo=getMongoOneData["turbo","map",{"Test_Number"->id}]//Dataset;
noNeeds={"data","Test_StartDate","Test_StartTime","T_oil_high","Turbo_Model","Turbo_SizeStr","T_W_high"};
deleteColumns[allInfo,noNeeds]
]



plotCMaps[ids_List,showContour_,opt:OptionsPattern[]]:=Module[{maps,n,styles,mapdatas,cmapLine,contour,cmapEta},
n=Length[ids];
styles=Take[ColorData[1,"ColorList"],n];
mapdatas=getMapData/@ids;
maps=getCMapPlotData[#1,#2,#3]&@@@Transpose[{mapdatas,ids,styles}];
cmapLine= plotSets[Flatten[Table[{maps[[i]]["pr"],maps[[i]]["surge"]},{i,1,n}],1], AxesLabel->{"Vred[\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p2p1"},PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI"];
contour=Show[Table[plotCMapContour[mapdatas[[i]]],{i,1,n}]];
cmapEta= plotSets[Table[maps[[i]]["eta"],{i,1,n}],AxesLabel->{"Vred","\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"},PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)",opt];

{{If[showContour,Show[contour,cmapLine,opt],cmapLine],cmapEta[[1]]}//Row,
	datasetConnect[getMapInfo/@ids]
	}

]

deleteMap[id_]:=deleteMasterDetail["turbo","map","map","Test_Number",id];

plotCMaps[id_String,showContour_,opt:OptionsPattern[]]:=plotCMaps[{id},showContour,opt];

getTMapPlotData[map_Dataset,id_String,style_:Magenta]:=Module[{pressLine,etaLine,surgeLine,nred,uredT,pressLineSpeed,etaLineSpeed,tip,d5,pr,eta},
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#piTtt,#MFP}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#piTtt,#etaTtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
nred=Round[map[Select[#MP==3&]][SortBy["KL"]][All,"nred"]//Normal];
uredT=Round[map[Select[#MP==3&]][SortBy["KL"]][All,"uredT"]//Normal];
pressLine=DeleteCases[pressLine,{}];
etaLine=DeleteCases[etaLine,{}];
tip=StringTemplate["`1`[rpm]=`2`[m/s]"]@@@Transpose[{nred,uredT}];
pressLineSpeed=Tooltip@@@Transpose[{pressLine,tip}];
etaLineSpeed=Tooltip@@@Transpose[{etaLine,tip}];
d5=StringTemplate["|d5: `` mm"][map[[1]]["d5"]];
<|"pr"-><|"data"->pressLineSpeed,"title"->id<>d5,"style"->style|>,
"eta"-><|"data"->etaLineSpeed,"title"->id<>d5,"style"->style|>
|>

]

getTMapPlotData[id_String,style_]:=Module[{map,pressLine,etaLine,surgeLine,nred,uredT,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
  map=getMapData[id];
  getTMapPlotData[map,id,style]
]

plotTMaps[id_List,opt:OptionsPattern[]]:=Module[{ids,maps,n,styles,mapdatas,mapinfo},
ids=Flatten[getTMapidsInGroup/@id]//DeleteDuplicates;
n=Length[ids];
styles=Take[ColorData[1,"ColorList"],n];
mapinfo=datasetConnect[getMapInfo/@ids];
maps=getTMapPlotData[#1,#2]&@@@Transpose[{ids,styles}];
	OpenerView[{{plotSets[Table[maps[[i]]["pr"],{i,1,n}],AxesLabel->{"p3/p4","MFP"},
						PlotLabel->"Turbine Map PI-MFP",PlotRange->All,opt],
	plotSets[Table[maps[[i]]["eta"],{i,1,n}],{AxesLabel->{"p3/p4","\!\(\*SubscriptBox[\(\[Eta]\), \(T\)]\)"},PlotLabel->"Turbine Map PI-\!\(\*SubscriptBox[\(\[Eta]\), \(T\)]\)",opt}][[1]]},
	{mapinfo}
	}]
]

plotTMaps[id_String,opt:OptionsPattern[]]:=plotTMaps[{id},opt]

plotMaps[id_String,showContour_,opt:OptionsPattern[]]:=plotMaps[{id},showContour,opt]

plotMaps[id_List,showContour_,opt:OptionsPattern[]]:=Module[{cmap,tmap},
cmap=plotCMaps[id,showContour,opt][[1]];
tmap=plotTMaps[id,opt][[1]];
OpenerView[{TabView[{"Compressor Map"->Extract[cmap,1],
"Turbine Map"->Extract[tmap,1]}],
Extract[tmap,2]}]
]
easyMaps:=Manipulate[Column[{id,plotMaps[id,showContour]}],{id,ListPicker[#,mapList]&},{{showContour,False,"show contour"},{True,False}},ControlPlacement->Left]





(* ::InheritFromParent:: *)
(**)
