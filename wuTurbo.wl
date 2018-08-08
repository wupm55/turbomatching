(* ::Package:: *)

BeginPackage["wuTurbo`"]
<<wuToolKit`
<<wuDBMongo`


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
d[All,<|"speed"->"speed","torque"->"torque","power"->"power","be"->t["be"],"\[Lambda]"->(#mL/#mb/14.7&),"airFuelRatio"->t["af"],"veff"->t["ve"],"dpAF"->(-#dpfilter&),"dpCooler"->(#dp2bc-#dp2ac&),"dpEx"->"p4","p0"->"p0","p1"->(#p0+#dpfilter&),"p2bc"->(#dp2bc+#p0&),"p2ac"->(#dp2ac+#p0&),"p3"->t["p3"],"p4"->(#p4+#p0&),"t0"->"t0","t1"->"t1","t2bc"->"t2bc","t2ac"->"t2ac","t3"->t["t3"],"t4"->"t4","p2p1"->t["p2p1"],"vred"->t["vred"],"p3p4"->t["p3p4"],"mL"->"mL","mb"->"mb","mT"->(#mL+#mb&),"FSN"->"FSN"|> ]
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



plotDatas[res_List,x_,y_,ids_List,opts:OptionsPattern[]]:=ListLinePlot[Table[res[[i]][All,{x,y}],{i,1,Length[res]}],FilterRules[{opts},Options[ListLinePlot]],ImageSize->Medium,PlotRange->Full,PlotLegends->ids,AxesLabel->{x,y},BaseStyle->{FontSize->12,Bold},GridLines->Automatic,PlotMarkers->Automatic,PlotLabel->(StringTemplate["Compare `1`-`2`" ][x,y])];
plotDatas[res_,x_,y_,id_,opts:OptionsPattern[]]:=ListLinePlot[res[All,{x,y}],AxesLabel->{x,y},FilterRules[{opts},Options[ListLinePlot]],PlotRange->Full,BaseStyle->{FontSize->12,Bold},GridLines->Automatic,ImageSize->Large,PlotMarkers->Automatic,PlotLabel->(StringTemplate["`1`-`2` of `3` " ][x,y,id] )];
plotDatas::usage = "plotDatas[results,x,y,ids,style:] list plot {x,y} of the result dataset ,and use the id as the plot Legend, style is to modify the plot style. ";


easyCompareEngineTestData:=Manipulate[{plotDatas[analizeEngineData[getEngineData[#]]&/@{testid,testid2},x,y,{testid,testid2},ImageSize->Large],getEngineInfo[#]&/@{testid,testid2}},{testid,testList,ControlType->PopupMenu},
{testid2,testList,ControlType->PopupMenu},Delimiter,{x,resultKeys},{{y,"torque"},DeleteCases[resultKeys,"speed"]},ControlPlacement->Left]//Quiet
easyCompareEngineTestData::usage = "easyCompareEngineTestData is to compare the engine test data";


easyPlotAll[testid_List,opts:OptionsPattern[]]:=Table[plotDatas[analizeEngineData[getEngineData[#]]&/@testid,"speed",y,testid,FilterRules[{opts},Options[ListLinePlot]]],
{y,DeleteCases[resultKeys,"speed"]}]
easyPlotAll[testid_String,opts:OptionsPattern[]]:=easyPlotAll[{testid},opts];
easyPlotAll::usage = "easyPlotAll[testids] used to compare test data in all parameters";

plotLugLines[testid_List,opts:OptionsPattern[]]:=Table[plotDatas[analizeEngineData[getEngineData[#]]&/@testid,"vred",y,testid,FilterRules[{opts},Options[ListLinePlot]]],
{y,{"p2p1","\[Eta]C"}}]
plotLugLines::usage = "plotLugLines[testids] is to plot a list of tests' luglines for comparision"




cmapList:=getMongoList["turbo","cmap","name"];
cmapList::usage = "cmapList list all cmap ids in the database"

getCMap[name_]:=Module[{map},
map=getMongoOneData["turbo","cmap",{"name"->name},{"CMapdata"},True]//Values//Flatten;
map//Dataset]
getCMap::usage = "getCMap[cmapid] return the compressor map data in dataset format "


getCMapPlotData[id_,style_.]:=Module[{map,pressLine,etaLine,surgeLine,nred,Ured,pressLineSpeed,etaLineSpeed,tip,d2,surge,pr,eta},
map=getCMap[id];
pressLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#PItt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
etaLine=Table[map[Select[#KL==i&]][SortBy["MP"]][All,{#Vred,#etatt}&]//Normal,{i,Min[map[All,"KL"]],Max[map[All,"KL"]]}];
surgeLine={map[Select[#MP==1&]][SortBy["KL"]][All,{#Vred,#PItt}&]//Normal};
nred=Round[map[Select[#MP==3&]][SortBy["KL"]][All,"nred"]//Normal];
Ured=map[Select[#MP==3&]][SortBy["KL"]][All,"Ured"]//Normal;
tip=StringTemplate["`1`[rpm]=`2`[m/s]"]@@@Transpose[{nred,Ured}];
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


plotCMaps[ids_List,showCont_:False]:=Module[{maps,n,styles},
n=Length[ids];
styles=Take[ColorData[1,"ColorList"],n];
maps=getCMapPlotData[#1,#2]&@@@Transpose[{ids,styles}];
If[showCont,
{Show[Show[Table[plotCMapContour[ids[[i]]],{i,1,n}]],
  plotSets[Flatten[Table[{maps[[i]]["pr"],maps[[i]]["surge"]},{i,1,n}],1],{AxesLabel->{"Vred","p2p1"},ImageSize->Large,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI"}],ImageSize->Large,PlotRange->All],
  plotSets[Table[maps[[i]]["eta"],{i,1,n}],{AxesLabel->{"Vred","\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"},ImageSize->Large,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"}]
},
	{plotSets[Flatten[Table[{maps[[i]]["pr"],maps[[i]]["surge"]},{i,1,n}],1],{AxesLabel->{"Vred","p2p1"},
	ImageSize->Large,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI"},PlotRange->{All,{1,4}}],
	plotSets[Table[maps[[i]]["eta"],{i,1,n}],{AxesLabel->{"Vred","\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"},ImageSize->Large,PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-\!\(\*SubscriptBox[\(\[Eta]\), \(c\)]\)"}]
	}
]
]


plotCMaps[id_String,showCont_:False]:=plotCMaps[{id},showCont];
plotCMaps::usage = "plotCMaps[cmapids,showContour:True] to plot a list of compressor map for comparsion";


plotLugLinesOnMaps[testids_List,mapids_List,showCont_:False]:=
{Show[Extract[plotCMaps[mapids,showCont],1],Extract[plotLugLines[testids],1]],Show[Extract[plotCMaps[mapids,showCont],2],Extract[plotLugLines[testids],2]]};

plotLugLinesOnMaps[testid_String,mapid_String,showCont_:False]:=plotLugLinesOnMaps[{testid},{mapid},showCont]
plotLugLinesOnMaps::usage="plotLugLinesOnMaps[testids_List,mapids_List] to plot lugines on different maps"

plotCMapContour[id_]:=Module[{map,data},
map=getCMap[id];
data=map[All,{"Vred","PIts","etats"}]//Values;
ListContourPlot[data,MaxPlotPoints->Automatic,FrameLabel->{"Vred [\!\(\*SuperscriptBox[\(m\), \(3\)]\)/s]","p2p1"},PlotLabel->"Compressor Map \!\(\*SubscriptBox[\(V\), \(red\)]\)-PI",BaseStyle->{FontSize->12,Bold},
PerformanceGoal->"Speed",PlotTheme->"Business",PlotLegends->None,ImageSize->Large,InterpolationOrder->3,GridLines->Automatic,ContourStyle->Directive[GrayLevel[0],Opacity[0.5`],Dashed]]
]

easyTestOnMap=Manipulate[plotLugLinesOnMaps[tests,maps,showCont],{{showCont,True,"Show Contour"},{True,False},ControlPlacement->Top},{tests,ListPicker[#1,testList]&,ControlPlacement->Left},{maps,ListPicker[#1,cmapList]&},ControlPlacement->Left];
easyTestOnMap::usage = "easyTestOnMap is to compare test results on different maps ";


importMapAdmin[file_]:=Module[{rawData,clearData},
rawData=SemanticImport[file];
clearData=deleteDatasetColumns[rawData,{"Test_BenchStr"}];
replaceAllDatasetTitle[clearData,"."->""]
]
importMapData[file_]:=Module[{rawData,clearData},
rawData=SemanticImport[file];
replaceAllDatasetTitle[rawData,"."->""]
]


insertMapAdmin[file_]:=insertMongoData["turbo","mapAdmin",importMapAdmin[file]]



EndPackage[]






(* ::InheritFromParent:: *)
(**)
