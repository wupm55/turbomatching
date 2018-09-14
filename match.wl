(* ::Package:: *)

SetDirectory["E:\\Github"];
<<nturbo`


turboConstant=<|"Lst"->14.7,"\!\(\*SubscriptBox[\(T\), \(ref\)]\)"->25,"\[Kappa]"->1.4,"\!\(\*SubscriptBox[\(R\), \(L\)]\)"->287,"Hu"->42000,"\[Kappa]T"->1.34,"CpAir"->Quantity[1.007,"Joules"/"Grams"/"Kelvins"],"CpT"->Quantity[1.136,"Joules"/"Grams"/"Kelvins"]|>;
enginePower[n_,t_]:=Quantity[QuantityMagnitude[n]*QuantityMagnitude[t]*2*\[Pi]/60000,"kW"]//N;
fuelDemand[Pe_,be_]:=UnitConvert[Pe*be,"Grams"/"Seconds"];
airDemand[mb_,\[Lambda]_,Lst_]:=mb*\[Lambda]*Lst;
density[p_,T_]:=
Quantity[QuantityMagnitude[p,"Pascals"]/287/QuantityMagnitude[T,"Kelvins"] ,"kg/m^3"]//N;
density::usage="density[p_,T_] get the density of air,p in bar, T in degC, density in g/l, R=287J/kg/K";
density[dp_,t_,unit_:"kPa"]:=((dp+100)*1000)/(287*(t+273.15));
density::usage="density[dp_,t_,unit_:kpa] diecty get the density from guage pressure";

pressure[d_,T_]:=Quantity[QuantityMagnitude[d,"Kilograms"/"Meters"^3]*Quantity[287,1/"Kelvins"]*UnitConvert[T,"Kelvins"]/100000,"Bars"];
expansionRate[PT_,t3_,mT_,etaT_,k_:turboConstant["\[Kappa]T"],CpT_:QuantityMagnitude[turboConstant["CpT"]]]:= 1/(1-PT/(CpT*(t3)*etaT*mT))^(k/(k-1));
DKW[mT_,t3_,p3_]:=mT*Sqrt[t3]/p3;
compressPower[t1_,p2p1_,mL_,etaV_,Cp_]:=Quantity[QuantityMagnitude[t1,"Kelvins"]*((p2p1)^((1.4-1)/1.4)-1)*QuantityMagnitude[mL]*Cp/etaV/1000000,"Kilowatts"];
torque[Pe_,n_]:=Quantity[QuantityMagnitude[Pe]*1000/(2*\[Pi]*QuantityMagnitude[n]/60),"Newtons"*"Meters"];
volumentricEfficiency[n_,VH_,\[Rho]2_,mL_]:=Module[{VL,mth},
	VL:=0.5*n*VH/60; 
	mth:=\[Rho]2*VL;
	mL/mth
];
volumentricEfficiency::usage="volumentricEfficiency[speed,VH,density,mL_gPerSec], the input without unit";
compressAir[t1_,p2p1_,eta_]:=Module[{air,k=turboConstant["\[Kappa]"]},
air["t2th"]:=(t1+273.15)*p2p1^((k-1)/k);
air["t2"]:=Quantity[(air["t2th"]-(t1+273.15))/eta+t1,"DegreesCelsius"];
air
];
turboBoundary[speed_,torque_,\[Lambda]_,be_,veff_,p0_,dpFilter_,dpCooler_,dp4_,t1_,t2ac_,t3_,Vh_]:=Module[{opi},
opi["t1"]=Quantity[t1,"DegreesCelsius"];
opi["p0"]=Quantity[p0,"kPa"];
opi["Vh"]=Quantity[Vh,"Liters"];
opi["speed"]=Quantity[speed,1/"Minutes"];
opi["torque"]=Quantity[torque,"Newtons"*"Meters"];
opi["\[Lambda]"]=\[Lambda];
opi["be"]=Quantity[be,"Grams"/"Kilowatts"/"Hours"];
opi["veff"]:=veff;
opi["dpFilter"]=Quantity[dpFilter,"Kilopascals"];
opi["dpCooler"]=Quantity[dpCooler,"Kilopascals"];
opi["dp4"]=Quantity[dp4,"Kilopascals"];
opi["t2ac"]=Quantity[t2ac,"DegreesCelsius"];
opi["t3"]=Quantity[t3,"DegreesCelsius"];
<|"Vh"->opi["Vh"],"speed"->opi["speed"],"torque"->opi["torque"],"\[Lambda]"->opi["\[Lambda]"],"veff"->veff,"be"->opi["be"],"p0"->opi["p0"],"dpFilter"->opi["dpFilter"],"dpCooler"->opi["dpCooler"],"dp4"->opi["dp4"],"t1"->opi["t1"],"t2ac"->opi["t2ac"],
"t3"->opi["t3"]|>
];
turboBoundary::usage="turboBoundary[speed,torque,\[Lambda],be,veff,p0,dpFilter,dpCooler,dp4,t1,t2ac,t3,Vh] define an operation point input object,dp unit is kPa";

simpleMatching[op_,etaV_,etaT_,wgRate_]:=Module[{opi=op,t,k=turboConstant["\[Kappa]"],kt=turboConstant["\[Kappa]T"],Cp=turboConstant["CpAir"],CpT=turboConstant["CpT"]},
AppendTo[opi,"Pe"->enginePower[opi["speed"],opi["torque"]]];
AppendTo[opi,"mb"->fuelDemand[opi["Pe"],opi["be"]]];
AppendTo[opi,"mL"->airDemand[opi["mb"],opi["\[Lambda]"],turboConstant["Lst"]]];
AppendTo[opi,"p0"->opi["p0"]];
AppendTo[opi,"p1"->opi["p0"]-opi["dpFilter"]];
AppendTo[opi,"\[Rho]1"->density[opi["p1"],opi["t1"]]];
AppendTo[opi,"V1"->opi["mL"]/opi["\[Rho]1"]];
AppendTo[opi,"t1"->opi["t1"]];
AppendTo[opi,"Vred"->opi["V1"]*Sqrt[298/(QuantityMagnitude[opi["t1"],"Kelvins"])]//N];
AppendTo[opi,"\[Rho]2"->2*opi["mL"]/(UnitConvert[opi["speed"],1/"Seconds"]*opi["Vh"]*opi["veff"])];
AppendTo[opi,"t2ac"->opi["t2ac"]];
AppendTo[opi,"p2ac"->pressure[opi["\[Rho]2"],opi["t2ac"]]];
AppendTo[opi,"p2bc"->opi["p2ac"]+opi["dpCooler"]];
AppendTo[opi,"p2p1"->opi["p2bc"]/opi["p1"]];
AppendTo[opi,"etaV"->etaV];
AppendTo[opi,"t2th"->Quantity[QuantityMagnitude[opi["t1"],"Kelvins"]*(opi["p2p1"])^((k-1)/k)-273.15,"DegreesCelsius"]];
AppendTo[opi,"t2"->(opi["t2th"]-opi["t1"])/etaV+opi["t1"]];
AppendTo[opi,"t2bc"->opi["t2"]];
AppendTo[opi,"\[Rho]2bc"->density[opi["p2bc"],opi["t2"]]];
AppendTo[opi,"t2ac"->opi["t2ac"]];
AppendTo[opi,"\[Rho]2ac"->density[opi["p2ac"],opi["t2ac"]]];
AppendTo[opi,"Pv"->UnitConvert[Cp*(opi["t2bc"]-opi["t1"])*opi["mL"],"kW"]];
AppendTo[opi,"etaT"->etaT];
AppendTo[opi,"mAbg"->opi["mL"]+opi["mb"]];
AppendTo[opi,"mWG"->opi["mAbg"]*wgRate];
AppendTo[opi,"mT"->opi["mAbg"]-opi["mWG"]];
AppendTo[opi,"PT"->opi["Pv"]];
AppendTo[opi,"t3"->opi["t3"]];
AppendTo[opi,"etaT"->etaT];
AppendTo[opi,"etaV"->etaV];
AppendTo[opi,"p3p4"->expansionRate[QuantityMagnitude[opi["PT"],"Kilowatts"],QuantityMagnitude[opi["t3"],"Kelvins"],QuantityMagnitude[opi["mT"],"Kilograms"/"Seconds"],etaT]];
AppendTo[opi,"t4th"->Quantity[QuantityMagnitude[opi["t3"],"Kelvins"]/(opi["p3p4"])^((kt-1)/kt)-273.15,"DegreesCelsius"]];
AppendTo[opi,"t4"->opi["t3"]-etaT*(opi["t3"]-opi["t4th"])];
AppendTo[opi,"PTcheck"->CpT*(opi["t3"]-opi["t4"])*opi["mT"]];
AppendTo[opi,"p4"->opi["dp4"]+opi["p0"]];
AppendTo[opi,"p3"->opi["p3p4"]*opi["p4"]];
t["mT"]=QuantityMagnitude[opi["mT"],"Kilograms"/"Seconds"];
t["t3"]=QuantityMagnitude[opi["t3"],"Kelvins"];
t["p3"]=QuantityMagnitude[opi["p3"],"Bars"];
AppendTo[opi,"mfp"->DKW[t["mT"],t["t3"],t["p3"]]]

];
simpleMatching::usage="simpleMatching[turboBoundary,etaV,etaT,wgRate] to calcualte the turbocharger status for one point";



x[data_]:=data/.{x_,y_,z_}->x;
y[data_]:=data/.{x_,y_,z_}->y;
z[data_]:=data/.{x_,y_,z_}->z;
xz[data_]:=data/.{x_,y_,z_}->{x,z};
xy[data_]:=data/.{x_,y_,z_}->{x,y};
yz[data_]:=data/.{x_,y_,z_}->{y,z};





mapTpi2mfp[mapid_]:=Module[{mapdata,md,mdg,di,mins,max,,maxs,inters,pieces,n},
mapdata=getMapData[mapid];
md=mapdata[All,{"KL","piTtt","MFP","etaTtt"}];
mdg=md[GroupBy["KL"]];
n=Length[mdg];
di=Table[(mdg[[i]][1;;-2])[All,{"piTtt","MFP"}]//Values//Normal,{i,1,n}];
mins=Table[mdg[[i]][Min,"piTtt"],{i,1,n}];
maxs=Table[mdg[[i]][Max,"piTtt"],{i,1,n}];
max=Max[maxs];
AppendTo[mins,max];
inters=Table[Interpolation[di[[i]],InterpolationOrder->1],{i,1,n}];
pieces=Table[{inters[[i]][pi],mins[[i]]<pi<mins[[i+1]]},{i,1,n}];
Piecewise[pieces]
]


mapTpi2eta[mapid_]:=Module[{mapdata,md,mdg,di,mins,max,maxs,inters,pieces,n},
mapdata=getMapData[mapid];
md=mapdata[All,{"KL","piTtt","MFP","etaTtt"}];
mdg=md[GroupBy["KL"]];
n=Length[mdg];
di=Table[(mdg[[i]][1;;-2])[All,{"piTtt","etaTtt"}]//Values//Normal,{i,1,n}];
mins=Table[mdg[[i]][Min,"piTtt"],{i,1,n}];
maxs=Table[mdg[[i]][Max,"piTtt"],{i,1,n}];
max=Max[maxs];
AppendTo[mins,max];
inters=Table[Interpolation[di[[i]],InterpolationOrder->1],{i,1,n}];
pieces=Table[{inters[[i]][pi],mins[[i]]<pi<mins[[i+1]]},{i,1,n}];
Piecewise[pieces,0.5]
]


mapTmfp2pi[mapid_]:=Module[{mapdata,md,mdg,di,mins,maxs,max,inters,pieces,n},
mapdata=getMapData[mapid];
md=mapdata[All,{"KL","piTtt","MFP","etaTtt"}];
mdg=md[GroupBy["KL"]];
n=Length[mdg];
di=Table[(mdg[[i]][1;;-2])[All,{"MFP","piTtt"}]//Values//Normal,{i,1,n}];
mins=Table[mdg[[i]][Min,"MFP"],{i,1,n}];
maxs=Table[mdg[[i]][Max,"MFP"],{i,1,n}];
max=Max[maxs];
AppendTo[mins,max];
inters=Table[Interpolation[di[[i]],InterpolationOrder->1],{i,1,n}];
pieces=Table[{inters[[i]][mfp],mins[[i]]<mfp<mins[[i+1]]},{i,1,n}];
Piecewise[pieces,4.2]
]





getEtaVFunction[mapid_]:=Module[{mapdata,md,mdg,n,g,x2z,x2y,x2zi,x2yi,xy2zi,},
mapdata=getMapData[mapid];
md=mapdata[All,{"KL","Vred","piCtt","etaCtt"}];
mdg=md[GroupBy["KL"]];
n=Length[md];
g=Normal/*Values/@DeleteMissing[Table[mdg[[i]][All,{"Vred","piCtt","etaCtt"}],{i,1,n}]];
n=Length[g];
x2z=Table[Interpolation[xz[g[[i]]],Method->"Spline",InterpolationOrder->2],{i,1,n}];
x2y=Table[Interpolation[xy[g[[i]]],Method->"Spline",InterpolationOrder->2],{i,1,n}];
<|"x2z"->x2z,"x2y"->x2y,"n"->n|>
]


interpolateEtaC[vred_,pi_,etaCFunction_]:=Module[{x2zi,x2yi,xy2zi,x2y,x2z,n},
x2y=etaCFunction["x2y"];
x2z=etaCFunction["x2z"];
n=etaCFunction["n"];
x2zi=Table[(x2z[[i]])[vred],{i,1,n}];
 x2yi=Table[x2y[[i]][vred],{i,1,n}];
    xy2zi=Interpolation[Transpose[{x2yi,x2zi}],Method->"Spline",InterpolationOrder->2];
    xy2zi[pi]
]


compressorRequirement[tb_Association]:=Module[{opi=tb,t,k=turboConstant["\[Kappa]"],kt=turboConstant["\[Kappa]T"],Cp=turboConstant["CpAir"],CpT=turboConstant["CpT"]},

AppendTo[opi,"Pe"->enginePower[opi["speed"],opi["torque"]]];
AppendTo[opi,"mb"->fuelDemand[opi["Pe"],opi["be"]]];
AppendTo[opi,"mL"->airDemand[opi["mb"],opi["\[Lambda]"],turboConstant["Lst"]]];
AppendTo[opi,"p0"->opi["p0"]];
AppendTo[opi,"p1"->opi["p0"]-opi["dpFilter"]];
AppendTo[opi,"\[Rho]1"->density[opi["p1"],opi["t1"]]];
AppendTo[opi,"V1"->opi["mL"]/opi["\[Rho]1"]];
AppendTo[opi,"t1"->opi["t1"]];
AppendTo[opi,"Vred"->opi["V1"]*Sqrt[298/(QuantityMagnitude[opi["t1"],"Kelvins"])]//N];
AppendTo[opi,"\[Rho]2ac"->2*opi["mL"]/(UnitConvert[opi["speed"],1/"Seconds"]*opi["Vh"]*opi["veff"])];
AppendTo[opi,"t2ac"->opi["t2ac"]];
AppendTo[opi,"p2ac"->UnitConvert[pressure[opi["\[Rho]2ac"],opi["t2ac"]],"kPa"]];
AppendTo[opi,"p2bc"->opi["p2ac"]+opi["dpCooler"]];
AppendTo[opi,"p2p1"->opi["p2bc"]/opi["p1"]]//KeySort
]

compressorRequirement[inputData_List]:=Module[{opi},
opi=turboBoundary@@@inputData;
compressorRequirement/@opi
]


getCMapPlot[mapid_]:=Module[{cmap,cmap1,cmap2},
cmap=plotCMaps[mapid,False];
cmap1=Extract[cmap,{1,1,1}];
cmap2=Extract[cmap,{1,1,2}];
{cmap1,cmap2}
]
getTMapPlot[mapid_]:=Module[{tmap,tmap1,tmap2},
tmap=plotTMaps[mapid];
tmap1=Extract[tmap,{1,1,1}];
tmap2=Extract[tmap,{1,1,2}];
{tmap1,tmap2}
]
fitCompressorMap[tb_,cmap_]:=Module[{lugline,lugeta,cmap1,cmap2},
lugline=Table[{tb[[i]]["Vred"],tb[[i]]["p2p1"]},{i,1,Length[tb]}];
lugeta=Table[{tb[[i]]["Vred"],tb[[i]]["etaV"]},{i,1,Length[tb]}];
cmap1=Extract[cmap,{1}];
cmap2=Extract[cmap,{2}];
{Show[cmap1,ListLinePlot[lugline,PlotStyle->{Black,Thick},PlotMarkers->{Automatic, 10}],PlotRange->All,ImageSize->400],
Show[cmap2,ListLinePlot[lugeta,PlotStyle->{Black,Thick},PlotMarkers->{Automatic, 10}],PlotRange->All,ImageSize->400]
}//Column
]
fitTurbineMap[tb_,tmap_]:=Module[{lugline,lugeta,tmap1,tmap2},
lugline=Table[{tb[[i]]["p3p4"],tb[[i]]["mfp"]},{i,1,Length[tb]}];
lugeta=Table[{tb[[i]]["p3p4"],tb[[i]]["etaT"]},{i,1,Length[tb]}];
tmap1=Extract[tmap,{1}];
tmap2=Extract[tmap,{2}];
{Show[tmap1,
ListLinePlot[lugline,PlotStyle->{Black,Thick},PlotMarkers->{Automatic, 10}],PlotRange->All,ImageSize->400],
Show[tmap2,
ListLinePlot[lugeta,PlotStyle->{Black,Thick},PlotMarkers->{Automatic, 10}],PlotRange->All,ImageSize->400]
}//Column
]
 readMapEtaVs[cr_,mapFun_]:=Table[interpolateEtaC[QuantityMagnitude[(cr[[i]])["Vred"]],(cr[[i]])["p2p1"],mapFun],
{i,1,Length[cr]}]

selectCMap[cmapid_]:=<|"cmap"->getCMapPlot[cmapid],
"etacFuncunction"->getEtaVFunction[cmapid]|>;
selectTMap[tmapid_]:=<|"tmap"->getTMapPlot[tmapid]|>;






turbineRequirement[tb_Association,etaV_]:=Module[{opi=tb,t,k=turboConstant["\[Kappa]"],kt=turboConstant["\[Kappa]T"],Cp=QuantityMagnitude[turboConstant["CpAir"]],CpT=QuantityMagnitude[turboConstant["CpT"]]},
AppendTo[opi,"etaV"->etaV];
AppendTo[opi,"t2th"->Quantity[QuantityMagnitude[opi["t1"],"Kelvins"]*(opi["p2p1"])^((k-1)/k)-273.15,"DegreesCelsius"]];
AppendTo[opi,"t2"->(opi["t2th"]-opi["t1"])/etaV+opi["t1"]];

AppendTo[opi,"t2bc"->opi["t2"]];
AppendTo[opi,"\[Rho]2bc"->density[opi["p2bc"],opi["t2"]]];
AppendTo[opi,"t2ac"->opi["t2ac"]];
AppendTo[opi,"\[Rho]2ac"->density[opi["p2ac"],opi["t2ac"]]];
AppendTo[opi,"Pv"->UnitConvert[turboConstant["CpAir"]*(UnitConvert[opi["t2bc"],"Kelvins"]-UnitConvert[opi["t1"],"Kelvins"])*opi["mL"],"kW"]];
AppendTo[opi,"PC"->
UnitConvert[UnitConvert[opi["t1"],"Kelvins"]*(opi["p2p1"]^((k-1)/k)-1)*opi["mL"]*turboConstant["CpAir"]/opi["etaV"],"kW"]];
AppendTo[opi,"mAbg"->opi["mL"]+opi["mb"]];
AppendTo[opi,"PT"->opi["Pv"]];
AppendTo[opi,"t3"->opi["t3"]]//KeySort
]

turbineRequirement[cr_List,cmap_Association]:=Module[
{etacs,ecf,tr,cmapPlot},
ecf=cmap["etacFuncunction"];
etacs=readMapEtaVs[cr,ecf];
cmapPlot=cmap["cmap"];
tr=turbineRequirement@@@Transpose[{cr,etacs}];
<|"data"->tr,
"compressor"->fitCompressorMap[tr,cmapPlot]|>
]
(*turbineRequirement[cr_List,cmap_Association,etacs_List]:=Module[
{tr,cmapPlot},
cmapPlot=cmap["cmap"];
tr=turbineRequirement@@@Transpose[{cr,etacs}]
<|"data"->tr,
"compressor"->fitCompressorMap[tr,cmapPlot]|>
]*)

turbineTuning[tb_,wg_,etaT_]:=Module[{opi=tb,t,k=turboConstant["\[Kappa]"],kt=turboConstant["\[Kappa]T"],Cp=QuantityMagnitude[turboConstant["CpAir"]],CpT=QuantityMagnitude[turboConstant["CpT"]]},
AppendTo[opi,"etaT"->etaT];
AppendTo[opi,"mWG"->opi["mAbg"]*wg];
AppendTo[opi,"mT"->opi["mAbg"]-opi["mWG"]];
AppendTo[opi,"p3p4"->expansionRate[QuantityMagnitude[opi["PT"],"Kilowatts"],QuantityMagnitude[opi["t3"],"Kelvins"],QuantityMagnitude[opi["mT"],"Kilograms"/"Seconds"],etaT]];
AppendTo[opi,"t4th"->Quantity[QuantityMagnitude[opi["t3"],"Kelvins"]/(opi["p3p4"])^((kt-1)/kt)-273.15,"DegreesCelsius"]];
AppendTo[opi,"t4"->opi["t3"]-etaT*(opi["t3"]-opi["t4th"])];
AppendTo[opi,"PTcheck"->CpT*(opi["t3"]-opi["t4"])*opi["mT"]];
AppendTo[opi,"p4"->opi["dp4"]+opi["p0"]];
AppendTo[opi,"p3"->opi["p3p4"]*opi["p4"]];
t["mT"]=QuantityMagnitude[opi["mT"],"Kilograms"/"Seconds"];
t["t3"]=QuantityMagnitude[opi["t3"],"Kelvins"];
t["p3"]=QuantityMagnitude[opi["p3"],"Bars"];
AppendTo[opi,"mfp"->DKW[t["mT"],t["t3"],t["p3"]]]
]

turbineTuning[tb_,wgs_,etats_,tmapPlot_]:=Module[{results},
results=turbineTuning@@@Transpose[{tb,wgs,etats}];
<|"turbine"->fitTurbineMap[results,tmapPlot],"data"->Dataset[results]|>
]



getInput[id_]:=getMongoOneData["turbo","matching",{"_id"->id}]


tunner[n_,min_,max_]:=Table[Module[{x},{i,Slider[Dynamic[x],{min,max}],Dynamic[x]}],{i,1,n}];
tunn[t_]:=Table[t[[i]][[3]],{i,1,Length[t]}];
