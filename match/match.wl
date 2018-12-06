(* ::Package:: *)

SetDirectory["E:\\Github"];



(* ::Text:: *)
(*\:57fa\:672c\:7269\:7406\:516c\:5f0f*)



turboConstant=<|"Lst"->14.7,"\!\(\*SubscriptBox[\(T\), \(ref\)]\)"->25,"\[Kappa]"->1.4,"\!\(\*SubscriptBox[\(R\), \(L\)]\)"->287,"Hu"->42000,"\[Kappa]T"->1.34,"CpAir"->Quantity[1.007,"Joules"/"Grams"/"Kelvins"],"CpT"->Quantity[1.1366,"Joules"/"Grams"/"Kelvins"]|>;

enginePower[n_?NumberQ,t_?NumberQ]:=n*t*2*\[Pi]/60000//N;
enginePower[n_Quantity,t_Quantity]:=Quantity[QuantityMagnitude[n]*QuantityMagnitude[t]*2*\[Pi]/60000,"kW"]//N;

fuelDemand[Pe_?NumberQ,be_?NumberQ]:=Pe*be/3600/1000//N;
fuelDemand[Pe_Quantity,be_Quantity]:=UnitConvert[Pe*be,"Grams"/"Seconds"];

airDemand[mb_?NumberQ,\[Lambda]_?NumberQ,Lst_?NumberQ]:=mb*\[Lambda]*Lst;
airDemand[mb_Quantity,\[Lambda]_,Lst_]:=mb*\[Lambda]*Lst;

density[p_?NumberQ,t_?NumberQ]:=(p*1000)/(287*(t+273.15));
density[p_Quantity,T_Quantity]:=
Quantity[QuantityMagnitude[p,"Pascals"]/287/QuantityMagnitude[T,"Kelvins"] ,"kg/m^3"]//N;
density::usage="density[p_Quantity,T_Quantity] get the density of air,p in bar, T in degC, density in g/l, R=287J/kg/K";
density::usage="density[dp_?NumberQ,t_?NumberQ] directy get the density from guage pressure";

pressure[d_?NumberQ,t_?NumberQ]:=d*287*(t+273.15)/1000;
pressure[d_Quantity,T_Quantity]:=Quantity[QuantityMagnitude[d,"Kilograms"/"Meters"^3]*Quantity[287,1/"Kelvins"]*UnitConvert[T,"Kelvins"]/100000,"Bars"];

expansionRate[PT_?NumberQ,t3_?NumberQ,mT_?NumberQ,etaT_?NumberQ,k_:turboConstant["\[Kappa]T"],CpT_:QuantityMagnitude[turboConstant["CpT"]]]:=(1-PT/(CpT etaT mT (t3+273.15)))^(-(k/(-1+k)));
expansionRate[PT_Quantity,t3_Quantity,mT_Quantity,etaT_,k_:turboConstant["\[Kappa]T"],CpT_:QuantityMagnitude[turboConstant["CpT"]]]:= 1/(1-PT/(CpT*(t3)*etaT*mT))^(k/(k-1));

DKW[mT_?NumberQ,t3_?NumberQ,p3_?NumberQ]:=(mT Sqrt[t3+273.15])/(p3*0.01);(*unit: (kgSqrt[T])/sbar*)

compressPower[t1_,p2p1_,mL_,etaV_,Cp_]:=Quantity[QuantityMagnitude[t1,"Kelvins"]*((p2p1)^((1.4-1)/1.4)-1)*QuantityMagnitude[mL]*Cp/etaV/1000000,"Kilowatts"];

torque[Pe_?NumberQ,n_?NumberQ]:=Pe*1000/(2*\[Pi]*n/60)//N;
torque[Pe_Quantity,n_Quantity]:=Quantity[QuantityMagnitude[Pe]*1000/(2*\[Pi]*QuantityMagnitude[n]/60),"Newtons"*"Meters"];

volumentricEfficiency[n_?NumberQ,VH_?NumberQ,\[Rho]2_?NumberQ,mL_?NumberQ]:=Module[{VL,mth},
	VL:=0.5*n*VH/60; 
	mth:=\[Rho]2*VL;
	mL/mth
];
volumentricEfficiency::usage="volumentricEfficiency[speed,VH,density,mL_gPerSec], the input without unit";


compressAir[t1_?NumberQ,p2p1_?NumberQ,eta_?NumberQ]:=Module[{air,k=turboConstant["\[Kappa]"]},
air["t2th"]:=(t1+273.15)*p2p1^((k-1)/k);
air["t2"]:=Quantity[(air["t2th"]-(t1+273.15))/eta+t1,"DegreesCelsius"];
air
];


(* ::Text:: *)
(*\:5339\:914dstep1 \:8ba1\:7b97\:53d1\:52a8\:673a\:9700\:6c42*)


calCompressorDemandPoint[op_Association]:=Module[{opi=op,t,k=turboConstant["\[Kappa]"],kt=turboConstant["\[Kappa]T"],Cp=turboConstant["CpAir"],CpT=turboConstant["CpT"]},
AppendTo[opi,"Pe"->enginePower[opi["speed"],opi["torque"]]];
AppendTo[opi,"mb"->fuelDemand[opi["Pe"],opi["be"]]];
AppendTo[opi,"mL"->airDemand[opi["mb"],opi["\[Lambda]"],turboConstant["Lst"]]];
AppendTo[opi,"p0"->opi["p0"]];
AppendTo[opi,"p1"->opi["p0"]+opi["dpAF"]];
AppendTo[opi,"\[Rho]1"->density[opi["p1"],opi["t1"]]];
AppendTo[opi,"v1"->opi["mL"]/opi["\[Rho]1"]];
AppendTo[opi,"t1"->opi["t1"]];
AppendTo[opi,"vred"->opi["v1"]*Sqrt[298/(opi["t1"]+273.15)]//N];
AppendTo[opi,"\[Rho]2ac"->2*opi["mL"]*1000/(opi["speed"]/60*opi["vh"]*opi["veff"])];
AppendTo[opi,"t2ac"->opi["t2ac"]];
AppendTo[opi,"p2ac"->pressure[opi["\[Rho]2ac"],opi["t2ac"]]];
AppendTo[opi,"p2bc"->opi["p2ac"]-opi["dpCooler"]];
AppendTo[opi,"p2p1"->opi["p2bc"]/opi["p1"]];
AppendTo[opi,"t2th"->(opi["t1"]+273.15)*(opi["p2p1"])^((k-1)/k)-273.15];
AppendTo[opi,"t2ac"->opi["t2ac"]];
AppendTo[opi,"\[Rho]2ac"->density[opi["p2ac"],opi["t2ac"]]]
];

matchStep1EngDemand[data_Dataset]:=Module[{list,res},
list=data//Normal;
res=calCompressorDemandPoint/@list;
Dataset[res]
]


(* ::Text:: *)
(*\:5339\:914dstep2 fit \:538b\:7aefmap*)


getCMapFuction[id_]:=Module[{vpe,list,data,fc},
vpe=getMapData[id][All,{"Vred","piCtt","etaCts"}];
list=vpe//Values//Normal;
data=list/.{x_,y_,z_}->{x,y}->z;
fc=Predict[data]
]\:ff1b

getTMapFuction[id_]:=Module[{pme,list,data,fc},
pme=getMapData[id][All,{"piTtt","MFP","etaTtt"}];
list=pme//Values//Normal;
data=list/.{x_,y_,z_}->{x,y}->z;
fc=Predict[data]
]


plotCLuglineVP[dataset_Dataset,id_:"",opts___?OptionQ]:=plotData[dataset,"vred","p2p1",id,PlotRange->All,opts];
plotCLuglineVE[dataset_Dataset,id_:"",opts___?OptionQ]:=plotData[dataset,"vred","etac",id,PlotRange->All,opts];


matchStep2FitCompressor[engDem_Dataset,mapid_String]:=Module[{maps,mapvp,mapve,plot,fc,lgvp,lgve,etac,e,matchData},
fc=getCMapFuction[mapid];
lgvp=engDem[All,{"vred","p2p1"}]//Values//Normal;
etac=fc/@lgvp;
e="etac"->#&/@etac;
matchData=Append[#2,#1]&@@@Transpose[{e,Normal[engDem]}]//Dataset;
maps=plotCMaps[mapid];
mapvp=Extract[maps,{1,1}];
mapve=Extract[maps,{1,2}];
lgvp=plotCLuglineVP[matchData];
lgve=plotCLuglineVE[matchData];
<|"plot"->{Show[mapvp,lgvp,PlotRange->All],Show[mapve,lgve,PlotRange->All]},"data"->matchData|>
]

matchStep2FitCompressorManually[engDem_Dataset,etaC_List,mapid_String]:=Module[{maps,mapvp,mapve,plot,fc,lgvp,lgve,etac,e,matchData},
etac=etaC;
e="etac"->#&/@etac;
matchData=Append[#2,#1]&@@@Transpose[{e,Normal[engDem]}]//Dataset;
maps=plotCMaps[mapid];
mapvp=Extract[maps,{1,1}];
mapve=Extract[maps,{1,2}];
lgvp=plotCLuglineVP[matchData];
lgve=plotCLuglineVE[matchData];
<|"plot"->{Show[mapvp,lgvp,PlotRange->All],Show[mapve,lgve,PlotRange->All]},"data"->matchData|>
]




(* ::Text:: *)
(*\:5339\:914d \:8ba1\:7b97\:6da1\:7aef\:9700\:6c42*)


calTurbineDemandPoint[op_Association]:=Module[{opi=op,k=turboConstant["\[Kappa]"],kt=turboConstant["\[Kappa]T"],Cp=turboConstant["CpAir"],CpT=turboConstant["CpT"]},AppendTo[opi,"\[Rho]2bc"->density[opi["p2bc"],opi["t2bc"]]];

AppendTo[opi,"t2bc"->(opi["t2th"]-opi["t1"])/opi["etac"]+opi["t1"]];
AppendTo[opi,"\[Rho]2bc"->density[opi["p2bc"],opi["t2bc"]]];
AppendTo[opi,"t2ac"->opi["t2ac"]];
AppendTo[opi,"\[Rho]2ac"->density[opi["p2ac"],opi["t2ac"]]];
AppendTo[opi,"PC"->QuantityMagnitude[turboConstant["CpAir"]]*(opi["t2bc"]-opi["t1"])*opi["mL"]];
AppendTo[opi,"mAbg"->opi["mL"]+opi["mb"]];
AppendTo[opi,"PT"->opi["PC"]];
AppendTo[opi,"t3"->opi["t3"]];
AppendTo[opi,"mAbg"->opi["mL"]+opi["mb"]];
AppendTo[opi,"PT"->opi["PC"]];
AppendTo[opi,"t3"->opi["t3"]];
AppendTo[opi,"p4"->opi["dpEx"]+opi["p0"]]
]
calTurbineDemand[data_Dataset]:=Module[{list,res},
list=Normal[data];
res=calTurbineDemandPoint/@list;
Dataset[res]
]
calTurbineDemand3[fitONCMAP_Association]:=Module[{data,list,res},
data=fitONCMAP["data"];
list=Normal[data];
res=calTurbineDemandPoint/@list;
Dataset[res]
]


(* ::Text:: *)
(*\:5339\:914dstep3 \:6da1\:7aef\:5339\:914d\:8ba1\:7b97*)


turbineTuningPoint[tb_Association,etaT_,wg_]:=Module[{opi=tb,t,k=turboConstant["\[Kappa]"],kt=turboConstant["\[Kappa]T"],Cp=QuantityMagnitude[turboConstant["CpAir"]],CpT=QuantityMagnitude[turboConstant["CpT"]]},
AppendTo[opi,"etat"->etaT];
AppendTo[opi,"mWG"->opi["mAbg"]*wg];
AppendTo[opi,"mT"->opi["mAbg"]-opi["mWG"]];
AppendTo[opi,"p3p4"->expansionRate[opi["PT"],opi["t3"],opi["mT"],opi["etat"]]];
AppendTo[opi,"t4th"->(opi["t3"]/opi["p3p4"]^((kt-1)/kt)-273.15)];
AppendTo[opi,"t4"->opi["t3"]-etaT*(opi["t3"]-opi["t4th"])];
AppendTo[opi,"PTcheck"->CpT*(opi["t3"]-opi["t4"])*opi["mT"]];
AppendTo[opi,"p4"->opi["p0"]-opi["dpEx"]];
AppendTo[opi,"p3"->opi["p3p4"]*opi["p4"]];
AppendTo[opi,"mfp"->DKW[opi["mT"],opi["t3"],opi["p3"]]]
]


matchStep3FitTurbine[fitONCMAP_Association,mapid_String]:=Module[{data,list,res,table,lgpm,lgpe,maps,mappe,mappm},
	data=calTurbineDemand3[fitONCMAP];	
	list=Normal[data];
	res=turbineTuningPoint[#,0.65,0]&/@list;
	table=res//Dataset;
		lgpm=plotData[table,"p3p4","mfp",""];
	lgpe=plotData[table,"p3p4","etat",""];
	maps=plotTMaps[mapid];
	mappm=Extract[maps,{1,1}];
	mappe=Extract[maps,{1,2}];
	<|"plot"->{Show[mappm,lgpm,PlotRange->All],Show[mappe,lgpe,PlotRange->All]},"data"->table|>
]

matchStep3FitTurbineManually[fitONCMAP_Association,etat_,wg_,mapid_String]:=Module[{data,list,res,table,lgpm,lgpe,maps,mappm,mappe},
	data=calTurbineDemand3[fitONCMAP];
	list=Normal[data];
	res=turbineTuningPoint[#1,#2,#3]&@@@Transpose[{list,etat,wg}];
	table=res//Dataset;
	lgpm=plotData[table,"p3p4","mfp",""];
	lgpe=plotData[table,"p3p4","etat",""];
	maps=plotTMaps[mapid];
	mappm=Extract[maps,{1,1}];
	mappe=Extract[maps,{1,2}];
	<|"plot"->{Show[mappm,lgpm,PlotRange->All],Show[mappe,lgpe,PlotRange->All]},"data"->table|>
]



