(* ::Package:: *)

addtodoItem[anything:(_Rule|_List)]:=Module[{},
mgInsert["todo","things",{anything,"create date"->Now,"id"->Hash[Now]}]
]


todoList:=Module[{list},
mgFind["todo","things"]//Dataset
]


todoUpdateInfo[pn_,info:(_Rule|_List)]:=mgUpdate["todo","things","id"->pn,info];
Attributes[todoUpdateInfo]={HoldAll}


todoUnsetInfo[id_,initValue_Rule]:=mgUnset["todo","things","id"->id,initValue];


unsetTodo[data_Dataset]:=unsetAssociation[data//Normal,"id",todoUnsetInfo]//Dataset;


addTodo[as_Dataset]:=addInfoAssication[as//Normal,"id",todoUpdateInfo]//Dataset;


updateTodo[data_Dataset]:=updateAssociation[data//Normal,"id",todoUpdateInfo]//Dataset;


btnToDataset[data_Dataset,caption_String,action_]:=Module[{list,newList,index ,ret},
list=data//Normal;
index=Range[Length[list]];
ret=Prepend[#1,caption->Button[caption, action[Dataset[#1]]]]&/@list;
ret//Dataset
]



editTodo[data_Dataset]:=btnToDataset[data,"edit",editQuestionDialog];


editQuestionDialog[data_]:=Module[{action},
CreateDialog[{"you want how to edit:", data,
{Button["update", editDialog[data,updatetodo]],
Button["delete 1 field", editDialog[data,unsettodo]],
Button["add 1 field", editDialog[data,addtodo]]}//Row,
Button["exit",DialogReturn[]]}//Column]
]


editDialog[data_,action_]:=Module[{list},

CreateDialog[{
action[data],
Button["exit",DialogReturn[]]
}]
]
Attributes[editDialog]={HoldAll};
