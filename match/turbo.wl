(* ::Package:: *)

(*mongo toolkit*)

SetDirectory["E:\\Github"];


(* ::Subsection:: *)
(*Dataset operate*)


<<Mongo`
datasetJoin[d1_,d2_]:=Table[Append[d1[[k]],d2[[k]]]//Normal,{k,1,Min[Length[d1],Length[d2]]}]//Dataset
datasetJoin::usage = "datasetJoin[d1,d2] is connect two dataset";
datasetJoin[data_List]:=Fold[datasetJoin,data[[1]],data[[2;;-1]]]

datasetConnect[datasets_List]:=Dataset[Normal/@datasets];
datasetConnect::usage = "datasetConnect[datasets] used to combine of datasets which has same stucture(titles)"

datasetDeleteColumns[dataset_Dataset,columnNames_List]:=Module[{rowKeys,updatedKeys},
rowKeys=keytokey[x]//Association;
updatedKeys=Fold[Delete[#1,#2]&,rowKeys,columnNames];
dataset[All,updatedKeys]
]

datasetAddColumn[da_Dataset,item_Rule]:=Table[Append[da[[i]]//Normal,item],{i,1,Length[da]}]//Dataset
datasetAddColumn::usage = "datasetAddColumn[dataset,item] add a new key->value to each row of the dataset";
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

findListInAssocialtion[data_Association]:=List/@(Select[Table[{i->(Head[data[[i]]]===List)},{i,1,Length[data]}]//Association,#&]//Keys);
findDatasetInAssocialtion[data_Association]:=List/@(Select[Table[{i->(Head[data[[i]]]===Dataset)},{i,1,Length[data]}]//Association,#&]//Keys);
associateListInAssociation[data_]:=MapAt[Association/@#&,data,findListInAssocialtion[data]];

keytokey[i_]:=Module[{k},k=Keys[Extract[Normal[i],1]];Thread[k->k]];



findIdPos[name_,list_]:=Flatten[Position[list,name]];
findIdPos[names_List,list_List]:=findIdPos[#,list]&/@names;



(*datasets toolkit*)

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
ds=Table[datasetAddColumn[selected[[i]],"id"->ids[[i]]],{i,1,n}];
all=Normal/@ds//Flatten//Dataset;
selectedID=all[All,Append[keys,"id"]];
selectedID[GroupBy[group]]
];
compareDatasets::usage = "compareDatasets[datasets,ids,keys,group] is used to compare two datasets which has same structure, ans choose keys to comapre,ids is used identify which sets belongs to which id, group to group the data for easy compare"

deleteColumns[a_,keysTobeDleted_List]:=If[Length[Dimensions[a]]==1,Delete[a,Partition[keysTobeDleted,1]],
Table[Delete[a[[i]]//Normal,Partition[keysTobeDleted,1]],{i,1,Length[a]}]//Dataset]
deleteColumns::usage = "deleteColumns[dataset, keysTobeDelete] used delete the  columns of a Dataset or an Association, specified by the kes";

unit1KeyOfDataset[dataset_,key_,unit_]:=Association/@Thread[key->(dataset[Quantity[#,unit]&,key]//Normal)]//Dataset;
unitKeysOfDataset[dataset_,key_List,unit_List]:=datasetJoin[Table[unit1KeyOfDataset[dataset,key[[i]],unit[[i]]],{i,1,Length[key]}]];




(* ::InheritFromParent:: *)
(**)


compareDatasetbyID[ids_List,keys_List, group_:"speed"]:=Module[{datasets,ds,all,selected,n,selectedID},
datasets=analizeEngineData/@getEngTestData/@ids;
compareDatasets[datasets,ids,keys, group]
]

compareTestsData[ids_List,x_,y_, group_:"speed"]:=Module[{data,all,dd,ds,n=Length[ids]},
compareDatasetbyID[ids,{x,y},group]
]









Options[compareTest]={showData->True,showInfo->True};
compareTest[id:(_List|_String),x_,y_,opts___?OptionQ]:=Module[{ids,results,info,pane,showDataQ=showData/.Flatten[{opts}]/.showData->True,showInfoQ=showInfo/.Flatten[{opts}]/.showInfo->True},
ids=Flatten[{id}];
results=analizeEngineData/@getEngTestData/@ids;
info=datasetConnect[getEngTestInfo/@ids];
DeleteCases[
{plotData[results,x,y,ids,opts],
If[showDataQ,compareTestsData[ids,x,y,"speed"]],
If[showInfoQ,info]
},
Null]
//Column
]
(*compareTest[id_,x_,y_,opts___?OptionQ]:=compareTest[{id},x,y,opts];*)
compareTest::usage = "compareTest[testids,x,y,showData:False] to plot the test results of different test id and show the compared data" 

easyCompareTests:=Manipulate[{compareTest[tests,x,y],
If[tests!={},Thread[findIdPos[tests,testList]->tests]]
},
{x,resultKeys,ControlPlacement->Left},
{{y,"torque"},DeleteCases[resultKeys,"speed"],ControlPlacement->Left},
{tests,ListPicker[#1,testList]&,ControlPlacement->Top}
]



(*Plot toolkit*)
plotData[result:(_List|_Dataset),x_,y_,id:(_List|_String),opts___?OptionQ]:=
Module[{ids,res},
ids=Flatten[{id}];
res={result}//Flatten;
ListLinePlot[Table[res[[i]][All,{x,y}],{i,1,Length[res]}],
Evaluate[FilterRules[{opts},Options[ListLinePlot]]],ImageSize->Medium,PlotRange->Full,PlotLegends->Placed[ids,Below],
AxesLabel->{x,y},BaseStyle->{FontSize->12,Bold},GridLines->Automatic,PlotMarkers->Automatic,PlotLabel->(StringTemplate[" `1`-`2`" ][x,y])]
];

(*plotData[res_,x_,y_,id_,opts___?OptionQ]:=ListLinePlot[res[All,{x,y}],AxesLabel->{x,y},Evaluate[FilterRules[{opts},Options[ListLinePlot]]],ImageSize->Medium,PlotRange->Full,BaseStyle->{FontSize->12,Bold},GridLines->Automatic,PlotMarkers->Automatic,PlotLabel->(StringTemplate["`1`-`2` of `3` " ][x,y,id] )];
*)
plotData::usage = "plotData[results,x,y,ids,opts___?OptionQ] list plot {x,y} of the result dataset ,and use the id as the plot Legend, style is to Update the plot style. ";

plotSets[ps_List,opts___?OptionQ]:=Module[{style,data,d,lengend,label},
style=Flatten[Table[Table[ps[[i]]["style"],ps[[i]]["data"]//Length],{i,1,Length[ps]}],1];
data=Table[ps[[i]]["data"],{i,1,Length[ps]}];
d=Flatten[data,1];
label=Column[Table[Style[ps[[i]]["title"],ps[[i]]["style"]],{i,1,Length[ps]}]];
ListLinePlot[d,Evaluate[FilterRules[{opts}//Flatten,Options[ListLinePlot]]],PlotStyle->style,PlotRange->All,PlotLegends->Placed[label,Below],BaseStyle->{FontSize->12,Bold},GridLines->Automatic,ImageSize->Medium]
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
If[compareWhichSpeed=="",pos=All];
If[pos=={},pos=All];
di=Take[di,pos];
barGroupedDataset[di,y,"id"]
]
barTestData[ids_String,x_,y_,compareWhichSpeed_:""]:=barTestData[{ids},x,y,compareWhichSpeed]


easyBarTests:=Manipulate[barTestData[tests,x,y,g],{x,resultKeys,ControlPlacement->Left},{{y,"torque"},DeleteCases[resultKeys,"speed"],ControlPlacement->Left},{g,InputField,ControlPlacement->Left},
{tests,ListPicker[#,testList]&,ControlPlacement->Top}]


(*engine test*)
testList:=mgList["turbo","engineTest","testID"]
testList::usage = "testList list all test ids in the database";

getEngTestInfo[id_]:=Module[{info,dataset},
info=mgFindOne["turbo","engineTest",{"testID"->id},{"data"},False];
dataset=MapAt[Dataset,info,Key["turboConfig"]]//Dataset;
flattenDataset[dataset]
]
getEngTestInfo::usage = "getEngTestInfo[id], get the test infomation about the id";

getEngTestData[id_,filter_:True]:=Module[{data,vh,NO0Data,dataFilted},
data=mgFindOne["turbo","engineTest",{"testID"->id},{"data","VH"},True];
vh=Rule["VH",data["VH"]];
data=datasetAddColumn[data["data"]//Dataset,vh];
NO0Data=data[Select[(#speed>0&&#mb>0&&#mL>0)&]];
NO0Data=NO0Data[SortBy["speed"]];
dataFilted=NO0Data[GroupBy["speed"]];
     dataFilted=Query[MaximalBy[#torque&]]/@dataFilted//Values//Flatten;
     If[filter,dataFilted,NO0Data]
    
]
getEngTestData::usage = "getEngTestData[id,filter:True] get the test data in dataset about the id ";

\[Rho][p_,t_]:=((p)*1000)/(287*(t+273.15));

analizeEngineDataTemp[data_]:=Module[{R=287,t,d},
(* mL: air mass in kg/h ; mb: fuel mass in kg/h ; All pressure in kPa; *)
d=data[Select[#torque>0&],All];
t["be"]:=#mb/#power*1000&;
t["af"]:=#mL/#mb&;
t["d0"]:=\[Rho][#p0,#t0]&;
t["d2ac"]:=\[Rho][#dp2ac+#p0,#t2ac]&;
t["d1"]:=\[Rho][#dpfilter+#p0,#t1]&;
t["v1"]:=#mL/3600/\[Rho][#dpfilter+#p0,#t1]&;
t["ve"]:=(#mL/3.6)/(#speed/60*0.5*#VH*((#dp2ac+100)*1000)/(R*(#t2ac+273.15)))&;

t["p2p1"]:=(#dp2bc+#p0)/(#p0+#dpfilter)&;
t["t2th"]:=(#t1+273.15)*((#dp2bc+#p0)/(#p0+#dpfilter))^((1.4-1)/1.4)-273.15&;

t["p3"]=Max[(#"p3s1"+#"p3s2")/2+#p0,#p3+#p0]&;
t["t3"]=Max[(#"t3s1"+#"t3s2")/2,#t3]&; 
t["p4"]=(#p0+#p4)&;
t["p3p4"]=Max[(#"p3s1"+#"p3s2")/2+#p0,#p3+#p0]/(#p0+#p4)&;
d[All,<|"speed"->"speed","torque"->"torque","\[Rho]0"->t["d0"],"\[Rho]1"->t["d1"],"\[Rho]2ac"->t["d2ac"],"\[Rho]0"->t["d0"],"power"->"power","be"->t["be"],"\[Lambda]"->(#mL/#mb/14.7&),"airFuelRatio"->t["af"],"veff"->t["ve"],"dpAF"->(#dpfilter&),"dpCooler"->(#dp2bc-#dp2ac&),"dpEx"->"p4","p0"->"p0","p1"->(#p0+#dpfilter&),"p2bc"->(#p0+#dp2bc&),"p2ac"->(#p0+#dp2ac&),"p3"->t["p3"],"p4"->t["p4"],"t1"->"t1","t2bc"->"t2bc","t2ac"->"t2ac","t3"->t["t3"],"t4"->"t4","p2p1"->t["p2p1"],"v1"->t["v1"],"p3p4"->t["p3p4"],"mL"->"mL","mb"->"mb","mT"->(#mL+#mb&),"vh"->"VH"|> ]
];


analizeEngineData[d_]:=Module[{vred,etaC,etaT,mfp,Cpair=1.006,c5,CpT=1.135,k=1.4,\[Gamma]=1.33,t,temp,temp2,etaTotal,powerC,powerCReal,powerT,etaTByPower},
(*cpair=1.006kJ/kgK ;  cpT=1.135kJ/kgK kAir=1.4; kExhaust=1.36   *)
temp=analizeEngineDataTemp[d];

etaC=(((#t1+273.15)*(#p2p1)^((k-1)/k)-273.15)-#t1)/(#t2bc-#t1)&;
etaT=If[#p3p4>1.01,(k #mL (-1+#p2p1^((-1+k)/k)) (273.15` +#t1) (-1+\[Gamma]))/((-1+k) #mT (1-(1/#p3p4)^((-1+\[Gamma])/\[Gamma])) (273.15` +#t3) \[Gamma]),"p3=p4!"]&;
etaTotal=If[#p3p4>1.01,((((#t1+273.15`) #p2p1^((k-1)/k)-273.15`)-#t1) (k #mL (-1+#p2p1^((-1+k)/k)) (273.15` +#t1) (-1+\[Gamma])))/((#t2bc-#t1) ((-1+k) #mT (1-(1/#p3p4)^((-1+\[Gamma])/\[Gamma])) (273.15` +#t3) \[Gamma])),"p3=p4!"]&;
mfp=#mT/3600*Sqrt[#t3+273.15]/(#p3/100)&; (*kg Sqrt[K]/s Bar*)
powerC=(#mL *Cpair*(#t1+273.15)*(#p2p1^((k-1)/k)-1)/3600)/((((#t1+273.15)*(#p2p1)^((k-1)/k)-273.15)-#t1)/(#t2bc-#t1))&;
powerCReal=#mL*Cpair*(#t2bc-#t1)/3600&;
powerT=#mT*CpT*(#t3-#t4)/3600&;
c5=c[#t3,#p3p4,1136]&;
vred=#v1 Sqrt[298/(#t1+273.15)]&;
temp2=temp[All,Prepend[<|"p1(rel)"->(#p1-#p0&),
"p2bc(rel)"->(#p2bc-#p0&),"p2ac(rel)"->(#p2ac-#p0&),"p3(rel)"->(#p3-#p0&),
"p4(rel)"->(#p4-#p0&),"\[Eta]C"->etaC,"p3p4"->(#p3p4&),"\[Eta]T"->etaT,"\[Eta]total"->etaTotal,
"mfp"->mfp,

"powerC"->powerC,"powerT"->powerT,"c5"->c5,"vred"->vred|>,keytokey[temp]]]

]
analizeEngineData::usage = "analizeEngineData[id] return the analized engine data";

resultsToInputs[result_]:=datasetJoin[Table["op"->j//Association,{j,1,Length[result]}]//Dataset,result[All,{"speed","torque","be","\[Lambda]","veff","p0","dpAF","dpCooler","dpEx","t1","t2ac","t3","vh"}]];
resultsToInputs::usage = "resultsToInputs[result] in to matching input format";
engineDataKeys=Extract[getEngTestData[testList[[1]]],1]//Keys//Normal;
engineDataKeys::uasage = "engineDataKeys returns the titles of the imported engine test data" ;

resultKeys=Extract[(getEngTestData[testList[[1]]]//analizeEngineData//Keys),1];
resultKeys::uasage = "resultKeys returns the titles of the imported analized test data" ;













easyPlotAll[testid:(_List|_String),opts___?OptionQ]:=Table[plotData[analizeEngineData[getEngTestData[#]]&/@Flatten[{testid}],"speed",y,testid,opts],
{y,DeleteCases[resultKeys,"speed"]}];

easyPlotAll[testid:(_List|_String),maps:(_List|_String),opts___?OptionQ]:={
plotLugLinesOnMaps[testid,maps,opts],easyPlotAll[testid,opts]}//Flatten

easyPlotAll::usage = "easyPlotAll[testids] used to compare test data in all parameters";



plotCLugLines[testid:(_List|_String),opts___?OptionQ]:=Table[plotData[analizeEngineData[getEngTestData[#]]&/@Flatten[{testid}],"vred",y,testid,opts],
{y,{"p2p1","\[Eta]C"}}];

plotCLugLines::usage = "plotLugLines[testids] is to plot a list of tests' luglines for comparision"



plotTLugLines[testid:(_List|_String),opts___?OptionQ]:=Table[plotData[analizeEngineData[getEngTestData[#]]&/@Flatten[{testid}],"p3p4",y,testid,opts],
{y,{"mfp","\[Eta]T"}}];




Default[getCMapPlotData]=Magenta;
getCMapPlotData[id_,style_.]:=Module[{map,pressLine,etaLine,surgeLine,nred,uredC,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
map=getMapData[id];
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#piCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#etaCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
surgeLine={map[Select[#MP==1&]][SortBy["KL"]][All,{#Vred,#piCtt}&]//Normal};
nred=(First/@Values[map[GroupBy["KL"]]])[All,"nred"]//Normal;
uredC=(First/@Values[map[GroupBy["KL"]]])[All,"uredC"]//Normal;
tip=StringTemplate["`1`[rpm]=`2`[m/s]"]@@@Transpose[{nred,uredC}];
pressLine=DeleteCases[pressLine,{}];
etaLine=DeleteCases[etaLine,{}];
{pressLine,tip}
]

getCMapPlotData[map_Dataset,id_,style_]:=Module[{pressLine,etaLine,surgeLine,nred,uredC,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#piCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#etaCtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
surgeLine={map[Select[#MP==1&]][SortBy["KL"]][All,{#Vred,#piCtt}&]//Normal};
nred=(First/@Values[map[GroupBy["KL"]]])[All,"nred"]//Normal;
uredC=(First/@Values[map[GroupBy["KL"]]])[All,"uredC"]//Normal;
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








plotLugLinesOnMaps[testid:(_List|_String),mapid:(_List|_String),opts___?OptionQ]:=Module[{testids,mapids,mapinfo,testInfo,mapCPlots,mapTPlots,testC,testT,c,t},
testids=Flatten[{testid}];
mapids=Flatten[{mapid}];

mapCPlots=Extract[plotCMaps[mapids,opts],1];
mapinfo=getMapInfo[mapid];
testInfo=datasetConnect[getEngTestInfo/@testids];
mapTPlots=Extract[plotTMaps[mapids,opts],1];
testC=plotCLugLines[testids];
testT=plotTLugLines[testids];
c= {Show[Extract[mapCPlots,1],Extract[testC,{1,1}],FilterRules[{opts}//Flatten,Options[Show]],PlotRange->All]
,Show[Extract[mapCPlots,2],Extract[testC,2],FilterRules[{opts}//Flatten,Options[Show]],PlotRange->All]
	};
t={Show[Extract[mapTPlots,1],Extract[plotTLugLines[testids],{1,1}],FilterRules[{opts}//Flatten,Options[Show]],PlotRange->All],
	Show[Extract[mapTPlots,2],Extract[testT,2],FilterRules[{opts}//Flatten,Options[Show]],PlotRange->All]
	};
{c,
t,
testInfo,
mapinfo
}//Column


]

plotLugLinesOnMaps::usage="plotLugLinesOnMaps[testids_List,mapids_List,opts___?OptionQ] to plot lugines on different maps"



plotCMapContour[map_Dataset]:=Module[{data},
data=map[All,{"Vred","piCts","etaCts"}]//Values;
ListContourPlot[data,MaxPlotPoints->7,FrameLabel->{"Vred [\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p2p1"},PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI",BaseStyle->{FontSize->12,Bold},
PerformanceGoal->"Speed",PlotTheme->"Business",PlotLegends->None,InterpolationOrder->3,GridLines->Automatic,ContourStyle->Directive[GrayLevel[0],Opacity[0.5`],Dashed]]
]

easyTestOnMap=Manipulate[{plotLugLinesOnMaps[tests,maps,opts],"tests:"->If[tests!={},Thread[findIdPos[tests,testList]->tests]],
"maps:"->If[maps!={},Thread[findIdPos[maps,mapList]->maps]]
}
,{opts,{showContour->False},ControlType->InputField,ControlPlacement->Top},
{tests,ListPicker[#1,testList]&,ControlPlacement->Left},
{maps,ListPicker[#1,mapList]&},ControlPlacement->Left
];
easyTestOnMap::usage = "easyTestOnMap is to compare test results on different maps ";





mapList:=mgList["turbo","map","Test_Number"];
cmapList=mgList["turbo","map","Test_Number",<|"$or"->{<|"usage"->"c"|>,<|"usage"->"ct"|>}|>];

tmapList=mgList["turbo","map","Test_Number",<|"$or"->{<|"usage"->"t"|>,<|"usage"->"ct"|>}|>];
getTMapidsInGroup[id_]:=Module[{mapinfo,group},
mapinfo=getMapInfo[id];
If[KeyExistsQ[mapinfo//Normal,"group"] && mapinfo["group"]!="",
getMongoManyData["turbo","map",{"group"->mapinfo["group"]},{"Test_Number"}]//Normal//Values//Flatten,
id]
]

getMapData[id_]:=Module[{titles,client,coll,curs,all,rawMap,map},
titles={"Line_Point_No","Line_No","u_cor_C_tv","u_cor_T","V_dot_Tcor_C","m_dot_Tcor_C","pi_st_C","pi_tt_C","eta_ad_tt_C","eta_ad_st_C","d_C","d_T","n_t_tv","pi_ts_T","pi_tt_T","MFP_T","m_dot_T","eta_tot_ts_T","eta_tot_tt_T"};
rawMap=getMongoManyData["turbo","map",{"Test_Number"->id},StringJoin["data.",#]&/@titles]//Values//Normal//Flatten//Dataset;
map=rawMap[All,<|"MP"->"Line_Point_No","KL"->"Line_No","nred"->"n_t_tv","uredC"->"u_cor_C_tv","Vred"->"V_dot_Tcor_C","m"->"m_dot_Tcor_C","piCtt"->"pi_tt_C",
"etaCtt"->"eta_ad_tt_C","piCts"->"pi_st_C","etaCts"->"eta_ad_st_C","d2"->"d_C","d5"->"d_T",
"uredT"->"u_cor_T","piTts"->"pi_ts_T","piTtt"->"pi_tt_T","MFP"->"MFP_T","mT"->"m_dot_T","etaTts"->"eta_tot_ts_T","etaTtt"->"eta_tot_tt_T"
|>];
Sort[map,(#1["KL"]*100+#1["MP"]<#2["KL"]*100+#2["MP"])&]
]

getMapInfo[id_]:=Module[{client,coll,noNeeds,allInfo},
allInfo=mgFindOne["turbo","map",{"Test_Number"->id}]//Dataset;
noNeeds={"data","Test_StartDate","Test_StartTime","T_oil_high","Turbo_Model","Turbo_SizeStr","T_W_high"};
Association["quick access"->Button["show map",CreateDialog[plotMaps[id],WindowTitle->"map of "<>id]],
deleteColumns[allInfo,noNeeds]//Normal//Normal]//Dataset
]

getMapInfo[ids_List]:=datasetConnect[getMapInfo/@ids];





Options[plotCMaps]={showContour->False};
plotCMaps[id:(_List|_String),opts___?OptionQ]:=Module[{ids,showC,maps,n,styles,mapdatas,cmapLine,contour,cmapEta,cmapLineWithContour},
ids={id}//Flatten;
n=Length[ids];
styles=Take[ColorData[1,"ColorList"],n];
mapdatas=getMapData/@ids;
maps=getCMapPlotData[#1,#2,#3]&@@@Transpose[{mapdatas,ids,styles}];
cmapLine= plotSets[Flatten[Table[{maps[[i]]["pr"],maps[[i]]["surge"]},{i,1,n}],1], Evaluate[FilterRules[{opts},Options[ListLinePlot]]],AxesLabel->{"Vred[\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p2p1"},PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI"];
contour=Show[Table[plotCMapContour[mapdatas[[i]]],{i,1,n}]];
cmapEta= plotSets[Table[maps[[i]]["eta"],{i,1,n}],AxesLabel->{"Vred","\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"},PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)",opts];
showC=showContour/.Flatten[{opts}]/.Options[plotCMaps];
cmapLineWithContour=Show[contour,cmapLine,Evaluate[FilterRules[{opts},Options[ListLinePlot]]]];
	
	
	{If[TrueQ[showC],
	cmapLineWithContour,cmapLine],
	cmapEta[[1]]}
	//Column

]




deleteMap[id_]:=deleteMasterDetail["turbo","map","map","Test_Number",id];


getTMapPlotData[map_Dataset,id_,style_:Magenta]:=Module[{pressLine,etaLine,surgeLine,nred,uredT,pressLineSpeed,etaLineSpeed,tip,d5,pr,eta},
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#piTtt,#MFP}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#piTtt,#etaTtt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
nred=(First/@Values[map[GroupBy["KL"]]])[All,"nred"]//Normal;
uredT=(First/@Values[map[GroupBy["KL"]]])[All,"uredT"]//Normal;
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

getTMapPlotData[id_,style_]:=Module[{map,pressLine,etaLine,surgeLine,nred,uredT,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
  map=getMapData[id];
  getTMapPlotData[map,id,style]
]

plotTMaps[id:(_List|_String),opts___?OptionQ]:=Module[{ids,maps,n,styles,mapdatas,mapinfo},
ids=Flatten[{id}];
ids=Flatten[getTMapidsInGroup/@ids]//DeleteDuplicates;
n=Length[ids];
styles=Take[ColorData[1,"ColorList"],n];
mapinfo=datasetConnect[getMapInfo/@ids];
maps=getTMapPlotData[#1,#2]&@@@Transpose[{ids,styles}];
	{plotSets[Table[maps[[i]]["pr"],{i,1,n}],AxesLabel->{"p3/p4","MFP"},
						PlotLabel->"Turbine Map PI-MFP",PlotRange->All,opts],
	plotSets[Table[maps[[i]]["eta"],{i,1,n}],{AxesLabel->{"p3/p4","\!\(\*SubscriptBox[\(\[Eta]\), \(T\)]\)"},PlotLabel->"Turbine Map PI-\!\(\*SubscriptBox[\(\[Eta]\), \(T\)]\)",opts}][[1]]
	}//Column
]

plotMaps[id:(_List|_String),opts___?OptionQ]:=Module[{cmap,tmap},
{{cmap=plotCMaps[id,opts],
tmap=plotTMaps[id,opts]}//Row
,Button["show info",CreateDialog[getMapInfo@id]]}//Column

]

easyMaps:=Manipulate[Column[{Thread[findIdPos[id,mapList]->id],plotMaps[id,showContour->show]}],{id,ListPicker[#,mapList]&},{{show,False,"show contour"},{True,False}},ControlPlacement->Left]





(* ::InheritFromParent:: *)
(**)


testToInput[testdata_Dataset]:=testdata//analizeEngineData//resultsToInputs;
inputToMongo[id_,inputData_Dataset]:=Module[{as},
as=<|"_id"->id,"input"->inputData|>;
associationToMongo["turbo","engReq",as]
];


c[t3_,er_,Cp_:1136]:=Sqrt[2 Cp (t3+273.15)(1-(1/er)^((1.36-1)/1.36))]
u[n_,d5_]:=NumberForm[n*Pi d5 /60000 //N,4];



resultKeys


withUnitForDataset[dataset_]:=datasetChangeTitle[dataset,withUnit];
withChineseForDataset[dataset_]:=datasetChangeTitle[dataset,withChinese];


withUnit={"p3s2"->"p3s2[kPa]","p3s1"->"p3s1[kPa]","t3s1"->"t3s1[\[Degree]C]","t3s2"->"t3s2[\[Degree]C]","dp2bc"->"dp2bc[kPa]","dp2ac"->"dp2ac[kPa]","dpfilter"->"dpFilter[kPa]","t0"->"t0[\[Degree]C]","VH"->"VH[L]","\[Rho]0"->"\[Rho]0[kg/\!\(\*SuperscriptBox[\(m\), \(3\)]\)]","\[Rho]1"->"\[Rho]1[kg/\!\(\*SuperscriptBox[\(m\), \(3\)]\)]","\[Rho]2ac"->"\[Rho]2ac[kg/\!\(\*SuperscriptBox[\(m\), \(3\)]\)]","p1(rel)"->"p1(rel)[kPa]","p2bc(rel)"->"p2bc(rel)[kPa]","p2ac(rel)"->"p2ac(rel)[kPa]","p3(rel)"->"p3(rel)[kPa]","p4(rel)"->"p4(rel)[kPa]","speed"->"speed[rpm]","torque"->"torque[Nm]","power"->"power[kW]","be"->"be[g/kWh]","\[Lambda]"->"\[Lambda]","airFuelRatio"->"airFuelRatio","veff"->"veff","dpAF"->"dpAF[kPa]","dpCooler"->"dpCooler[kPa]","dpEx"->"dpEx[kPa]","p0"->"p0[kpa]","p1"->"p1[kPa]","p2bc"->"p2bc[kPa]","p2ac"->"p2ac[kPa]","p3"->"p3[kPa]","p4"->"p4[kPa]","t1"->"t1[\[Degree]C]","t2bc"->"t2bc[\[Degree]C]","t2ac"->"t2ac[\[Degree]C]","t3"->"t3[\[Degree]C]","t4"->"t4[\[Degree]C]","p2p1"->"p2p1","vred"->"vred[\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p3p4"->"p3p4","mL"->"mL[kg/h]","mb"->"mb[kg/h]","mT"->"mT[kg/h]","vh"->"vh[L]","\[Eta]C"->"\[Eta]C","\[Eta]T"->"\[Eta]T","\[Eta]total"->"\[Eta]total","mfp"->"mfp[\!\(\*FractionBox[\(kg \*SqrtBox[\(K\)]\), \(s\\\ Bar\)]\)]","powerC"->"powerC[kW]","powerT"->"powerT[kW]","c5"->"c[m/s]"};


withChinese={"FSN"->"\:70df\:5ea6","p3s1"->"\:6da1\:524d\:8868\:538b1[kPa]","p3s2"->"\:6da1\:524d\:8868\:538b2[kPa]","t3s1"->"\:6da1\:524d\:6e29\:5ea61[\[Degree]C]","t3s2"->"\:6da1\:524d\:6e29\:5ea62[\[Degree]C]","dp2bc"->"\:4e2d\:51b7\:524d\:8868\:538b[kPa]","dp2ac"->"\:4e2d\:51b7\:540e\:8868\:538b[kPa]","dpfilter"->"\:7a7a\:6ee4\:538b\:964d[kPa]","t0"->"\:73af\:5883\:6e29\:5ea6[\[Degree]C]","VH"->"\:53d1\:52a8\:673a\:6392\:91cf[L]","\[Rho]0"->"\:7a7a\:6c14\:5bc6\:5ea6[kg/\!\(\*SuperscriptBox[\(m\), \(3\)]\)]","\[Rho]1"->"\:538b\:524d\:5bc6\:5ea6[kg/\!\(\*SuperscriptBox[\(m\), \(3\)]\)]","\[Rho]2ac"->"\:4e2d\:51b7\:540e\:5bc6\:5ea6[kg/\!\(\*SuperscriptBox[\(m\), \(3\)]\)]","p1(rel)"->"\:538b\:524d\:538b\:529b(\:8868\:538b)[kPa]","p2bc(rel)"->"\:538b\:540e\:538b\:529b(\:8868\:538b\:ff09[kPa]","p2ac(rel)"->"\:4e2d\:51b7\:540e\:538b\:529b(\:8868\:538b)[kPa]","p3(rel)"->"\:6da1\:524d\:538b\:529b(\:8868\:538b)[kPa]","p4(rel)"->"\:6392\:6c14\:80cc\:538b(\:8868\:538b)[kPa]","speed"->"\:8f6c\:901f[rpm]","torque"->"\:626d\:77e9[Nm]","power"->"\:529f\:7387[kW]","be"->"\:6bd4\:6cb9\:8017[g/kWh]","\[Lambda]"->"\:8fc7\:91cf\:7a7a\:6c14\:7cfb\:6570","airFuelRatio"->"\:7a7a\:71c3\:6bd4","veff"->"\:5145\:6c14\:6548\:7387","dpAF"->"\:7a7a\:6ee4\:538b\:964d[kPa]","dpCooler"->"\:4e2d\:51b7\:538b\:964d[kPa]","dpEx"->"\:6392\:6c14\:80cc\:538bp4[kPa]","p0"->"\:73af\:5883\:538b\:529b[kPa]","p1"->"\:538b\:524d\:538b\:529b[kPa]","p2bc"->"\:538b\:540e\:538b\:529b[kPa]","p2ac"->"\:4e2d\:51b7\:540e\:538b\:529b[kPa]","p3"->"\:6da1\:524d\:538b\:529b[kPa]","p4"->"\:6da1\:540e\:538b\:529b[kPa]","t1"->"\:538b\:524d\:6e29\:5ea6[\[Degree]C]","t2bc"->"\:4e2d\:51b7\:524d\:6e29\:5ea6[\[Degree]C]","t2ac"->"\:4e2d\:51b7\:540e\:540e\:6e29\:5ea6[\[Degree]C]","t3"->"\:6da1\:524d\:6e29\:5ea6[\[Degree]C]","t4"->"\:6da1\:540e\:6e29\:5ea6[\[Degree]C]","p2p1"->"\:538b\:6bd4","vred"->"\:538b\:7aef\:6298\:5408\:6d41\:91cf[\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p3p4"->"\:81a8\:80c0\:6bd4","mL"->"\:7a7a\:6c14\:8d28\:91cf\:6d41\:91cf[kg/h]","mb"->"\:71c3\:6cb9\:8d28\:91cf\:6d41\:91cf[kg/h]","mT"->"\:5e9f\:6c14\:8d28\:91cf\:6d41\:91cf[kg/h]","vh"->"\:53d1\:52a8\:673a\:6392\:91cf[L]","\[Eta]C"->"\:538b\:7aef\:6548\:7387","\[Eta]T"->"\:6da1\:7aef\:6548\:7387","\[Eta]total"->"\:589e\:538b\:5668\:603b\:6548\:7387","mfp"->"\:6da1\:7aef\:6298\:5408\:6d41\:91cf[\!\(\*FractionBox[\(kg \*SqrtBox[\(K\)]\), \(s\\\ Bar\)]\)]","powerC"->"\:538b\:7aef\:529f\:7387[kW]","powerT"->"\:6da1\:7aef\:529f\:7387[kW]","c5"->"c(u/c)[m/s]"};


datasetChangeTitle[dataset_,oldTONewrules_List]:=Dataset[Association/@(Normal[Normal[dataset]]/.oldTONewrules)];


cwList:=mgList["turbo","cw","PN"];
twList:=mgList["turbo","tw","PN"];


getCWInfo[pn_]:=mgFindOne["turbo","cw",{"PN"->pn}]//Dataset
getCWInfo[pns_List]:=getCWInfo/@pns//datasetConnect;
getTWInfo[pn_]:=mgFindOne["turbo","tw",{"PN"->pn}]//Dataset
getTWInfo[pns_List]:=getTWInfo/@pns//datasetConnect;


(* ::Subsection:: *)
(*update association info*)


updateDialog[id_,key_,initVar_,action_]:=Module[{ret},
CreateDialog[{InputField[Dynamic[initVar]],{DefaultButton["save",MessageDialog[action[id,key->initVar]]],DefaultButton["exit",DialogReturn[]]}//Row},
WindowTitle->"Update "<>key];
]
Attributes[updateDialog]={HoldAll};

updateButton[id_,initValue_Rule,action_]:=Module[{key,val},
val=Values[initValue];
{val,Button["update" ,updateDialog[id,initValue//Keys,val,action]]
}
]
Attributes[updateButton]={HoldAll};

updateAssociation[asso_Association,idColumnName_,action_]:=Module[{ass,keys,values,rules,detail,n,valueCanUpdate,id},
ass=KeyDrop[asso,"quick access"];
keys=Keys[ass];
values=Values[ass];
n=Length[keys];
rules=Normal[ass];
id=ass[idColumnName];
valueCanUpdate=updateButton[id,#,action]&/@rules;
Thread[Rule[keys,valueCanUpdate]]//Association
]



cwUpdateInfo[pn_,info:(_Rule|_List)]:=mgUpdate["turbo","cw","PN"->pn,info];
twUpdateInfo[pn_,info:(_Rule|_List)]:=mgUpdate["turbo","tw","PN"->pn,info];
mapUpdateInfo[pn_,info:(_Rule|_List)]:=mgUpdate["turbo","map","Test_Number"->pn,info];
engTestUpdateInfo[pn_,info:(_Rule|_List)]:=mgUpdate["turbo","engineTest","testID"->pn,info];
turboUpdateInfo[pn_,info:(_Rule|_List)]:=mgUpdate["turbo","turbos","id"->pn,info];
engReqUpdateInfo[pn_,info:(_Rule|_List)]:=mgUpdate["turbo","engReq","id"->pn,info];


btnMapInfo[id_]:=Button[id,MessageDialog[getMapInfo[id]]]





updateCW[data_Dataset]:=updateAssociation[data//Normal,"PN",cwUpdateInfo]//Dataset;
updateTW[data_Dataset]:=updateAssociation[data//Normal,"PN",twUpdateInfo]//Dataset;
updateMap[data_Dataset]:=updateAssociation[data//Normal,"Test_Number",mapUpdateInfo]//Dataset;
updateEngTest[data_Dataset]:=updateAssociation[data//Normal,"testID",engTestUpdateInfo]//Dataset;
updateTurbo[data_Dataset]:=updateAssociation[data//Normal,"id",turboUpdateInfo]//Dataset;
updateEngReq[data_Dataset]:=updateAssociation[data//Normal,"id",engReqUpdateInfo]//Dataset;


(* ::Subsection:: *)
(*To add a new info to association*)


easyImport:=Import[SystemDialogInput["FileOpen"]];


dialogNewInfo[id_,key_,val_,action_]:=Module[{},
CreateDialog[{"Name:",InputField[Dynamic[key]],"Value:",InputField[Dynamic[val]],

{Button["Add info",MessageDialog[action[id,key->val]]],
	    Button["Exit",DialogReturn[]]
}//Row
},
WindowTitle->"add new information"];
]
Attributes[dialogNewInfo]={HoldAll};


addInfoButton[id_,action_]:=Module[{key="",val=""},

Button["add new info" ,dialogNewInfo[id,key,val,action]]
]


addInfoAssication[asso_Association,idColumnName_String,action_]:=Module[{ass,keys,values,rules,detail,n,newValue,id},
ass=KeyDrop[asso,"quick access"];
keys=Keys[ass];
values=Values[ass];
n=Length[keys];
rules=Normal[ass];
id=ass[idColumnName];
newValue=addInfoButton[id,action];
Prepend[ass//Normal,"new info"->newValue]//Association
]


(* ::Item:: *)
(*\:5177\:4f53\:7684\:5b9e\:73b0\:65b9\:6cd5*)


addCWInfo[as_Dataset]:=addInfoAssication[as//Normal,"PN",cwUpdateInfo]//Dataset;
addTWInfo[as_Dataset]:=addInfoAssication[as//Normal,"PN",twUpdateInfo]//Dataset;
addMapInfo[as_Dataset]:=addInfoAssication[as//Normal,"Test_Number",mapUpdateInfo]//Dataset;
addEngTestInfo[as_Dataset]:=addInfoAssication[as//Normal,"testID",engTestUpdateInfo]//Dataset;
addTurboInfo[as_Dataset]:=addInfoAssication[as//Normal,"id",turboUpdateInfo]//Dataset;
addEngReqInfo[as_Dataset]:=addInfoAssication[as//Normal,"id",engReqUpdateInfo]//Dataset;


(* ::Item:: *)
(**)


(* ::Subsection:: *)
(*To unset an association*)


(* ::Text:: *)
(*\:901a\:7528\:7684\:5b9e\:73b0\:65b9\:6cd5*)


dialogUnset[id_,initValue_Rule,action_]:=Module[{ret},
CreateDialog[{InputField[initValue//Values],
{
Button["Delete",DialogReturn[MessageDialog[action[id,initValue]]]],
	    Button["Exit",DialogReturn[]]
}//Row
},
WindowTitle->"Delete" <>(initValue//Keys)<> " of :"<> id];
]



btnUnset[id_,initValue_Rule,action_]:=Module[{key,val},
{initValue//Values,Button["Delete",dialogUnset[id,initValue,action]]}
]


unsetAssociation[asso_Association,idColumnName_String,action_]:=Module[{ass,keys,values,rules,detail,valueCanUpdate,id},
ass=KeyDrop[asso,"quick access"];
keys=Keys[ass];
values=Values[ass];
rules=Normal[ass];
id=ass[idColumnName];
valueCanUpdate=btnUnset[id,#,action]&/@rules;
Thread[Rule[keys,valueCanUpdate]]//Association
]






(* ::Text:: *)
(*\:5177\:4f53\:7684\:5b9e\:73b0*)


(* ::Input:: *)
(*\:5220\:9664MongoDB\:4e2dcwinfo\:7684\:4e00\:9879\:503c*)


cwUnsetInfo[id_,initValue_Rule]:=mgUnset["turbo","cw","PN"->id,initValue];
twUnsetInfo[id_,initValue_Rule]:=mgUnset["turbo","tw","PN"->id,initValue];
mapUnsetInfo[id_,initValue_Rule]:=mgUnset["turbo","map","Test_Number"->id,initValue];
engTestUnsetInfo[id_,initValue_Rule]:=mgUnset["turbo","engineTest","testID"->id,initValue];
turboUnsetInfo[id_,initValue_Rule]:=mgUnset["turbo","turbos","id"->id,initValue];
engReqUnsetInfo[id_,initValue_Rule]:=mgUnset["turbo","engReq","id"->id,initValue];


(* ::Input:: *)
(*\:5728associaltion\:7684\:6570\:636e\:4e0a\:6dfb\:52a0\:4e00\:4e2a\:5220\:9664\:6309\:94ae\:ff0c\:70b9\:51fb\:6309\:94ae\:5f39\:51fa\:5bf9\:8bdd\:6846\:ff0c\:786e\:8ba4\:5220\:9664\:8be5field*)


unsetCW[data_Dataset]:=unsetAssociation[data//Normal,"PN",cwUnsetInfo]//Dataset;
unsetTW[data_Dataset]:=unsetAssociation[data//Normal,"PN",twUnsetInfo]//Dataset;
unsetMap[data_Dataset]:=unsetAssociation[data//Normal,"Test_Number",mapUnsetInfo]//Dataset;
unsetEngTest[data_Dataset]:=unsetAssociation[data//Normal,"testID",engTestUnsetInfo]//Dataset;
unsetTurbo[data_Dataset]:=unsetAssociation[data//Normal,"id",turboUnsetInfo]//Dataset;
unsetEngReq[data_Dataset]:=unsetAssociation[data//Normal,"id",engReqUnsetInfo]//Dataset;



newTurbo[id_,ELT_String,Eng_String]:=mgInsertInTurbo["turbos",{"_id"->id,"id"->id,"ELT"->ELT,"Eng"->Eng}];
turboList:=mgList["turbo","turbos","id"];
getTurboInfo[id_]:=mgFindOne["turbo","turbos",{"id"->id}]//Dataset;
turboList:=mgList["turbo","turbos","id"];


newEngRequirement[id_,volume_?NumberQ]:=mgInsertInTurbo["engReq",{"_id"->id,"id"->id,"volume"->volume}];
engReqList:=mgList["turbo","engReq","id"];
getEngReqInfo[id_]:=mgFindOne["turbo","engReq",{"id"->id}]//Dataset;
getEngReqData[id_]:=mgFindOne["turbo","engReq",{"id"->id}]["data"]//Dataset;
addEngReqData[id_,data_Dataset]:=engReqUpdateInfo[id,Normal[data]]


(* ::Text:: *)
(* *)


(* ::Input:: *)
(**)



