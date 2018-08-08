(* ::Package:: *)

BeginPackage["wuToolKit`"]



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

deleteDatasetColumns[dataset_,columnNames_]:=dataset[All,Complement[dataset[[1]]//Keys//Normal,columnNames]]

replaceAllDatasetTitle[dataset_,replaceRule_]:=Module[{titles,newNames},
titles=dataset[[1]]//Keys//Normal;
newNames=Association[Thread[Rule[StringReplace[titles,replaceRule],titles]]];
dataset[All,newNames]
]


findListInAssocialtion[data_Association]:=List/@(Select[Table[{i->(Head[data[[i]]]===List)},{i,1,Length[data]}]//Association,#&]//Keys)
associateListInAssociation[data_]:=MapAt[Association/@#&,data,findListInAssocialtion[data]]

EndPackage[]

