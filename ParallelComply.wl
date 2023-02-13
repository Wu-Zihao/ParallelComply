(* ::Package:: *)

ParallelComply`packagePath=DirectoryName[$InputFileName]
ParallelComply`workingPath=$Failed


Print["ParallelComply: This is an interface to run mathmatica functions in parallel."]
Print["For more information, run ParallelComplyUserGuide[]."]


ParallelComplyUserGuide[]:=Module[{userGuide},
	userGuide=
	"
0.You must have the following packages on your computer:
parallel
	";
	Print[userGuide]
]


Options[CreateParallelComplyWorkingFolder]={OverWrite->False};
CreateParallelComplyWorkingFolder[path_,OptionsPattern[]]:=Module[{},
	If[FileExistsQ[path],
		If[DirectoryQ[path],
			If[OptionValue[OverWrite],
				Run["rm -rf "<>path];
				Print[path<>" has been overwritten."]
				,
				Print["Folder "<>path<>" already exists. Their might be some existing data there."];
			]
			,
			Print[path<>" is an existing file! Failed."];
			Return[$Failed]
		]
	];
	Run["mkdir "<>path];
	Run["cp -f "<>ParallelComply`packagePath<>"*.* "<>path];
	Run["rm -f "<>path<>"ParallelComply.wl"];
	ParallelComply`workingPath=path;
	Print["The working path of ParallelComply has been set to "<>path];
	Return[path]
]
ChangeParallelComplyWorkingFolder[path_]:=Module[{lackedFiles},
	If[FileExistsQ[path],
		If[DirectoryQ[path],
			lackedFiles=LackedParallelComplyFiles[path];
			If[lackedFiles==={},
				ParallelComply`workingPath=path;
				Print["The working path of ParallelComply has been set to "<>path];
				(*Print["Please make sure this folder is an ParallelComply working folder and the dependent files are complete!"]*)
				,
				Print["Lacking following files at "<>path<>"\n",lackedFiles];
				Return[$Failed]
			]
			,
			Print[path<>" is an existing file! Failed."];
			Return[$Failed]
		]
		,
		Print[path<>" dose not exist!"];
		Return[$Failed]
	];
]


AutoCreateSubFolder[]:=Module[{i,path,subFolder},
	If[ParallelComply`workingPath===$Failed,
		Print["No working folder specified! Run CreateParallelComplyWorkingFolder first!"];
		Return[$Failed]
		,
		path=ParallelComply`workingPath
	];
	For[i=1,True,i++,
		subFolder=path<>"parallel_mission_"<>ToString[i]<>"/";
		If[!FileExistsQ[subFolder],
			Run["mkdir "<>subFolder];
			Break[];
		]
	];
	Return[subFolder]
]


ParallelComplyBufferClear[]:=Module[{i,path,subFolders,subFolder},
	If[ParallelComply`workingPath===$Failed,
		Print["No working folder specified! Run CreateParallelComplyWorkingFolder first!"];
		Return[$Failed]
		,
		path=ParallelComply`workingPath
	];
	subFolders=FileNames[All,path];
	For[i=1,i<=Length[subFolders],i++,
		subFolder=subFolders[[i]];
		If[!DirectoryQ[subFolder],Continue[];];
		Run["rm -rf "<>subFolder]
	];
	Print["ParallelComply buffer cleared in "<>path];
	Return[path]
]


ClearAll[ParallelComply]
Options[ParallelComply]={ReadFiles->{}}
ParallelComply[input_,operation_,OptionsPattern[]]:=Module[{path,subFolder,dimensions,i,script,tasks,task,tasksFlatten,timer,dir,result},
	If[ParallelComply`workingPath===$Failed,
		Print["No working folder specified! Run CreateParallelComplyWorkingFolder first!"];
		Return[$Failed]
		,
		path=ParallelComply`workingPath
	];
	timer=AbsoluteTime[];
	Print["Initializing parallel tasks..."];
	subFolder=AutoCreateSubFolder[];
	Run["cp "<>path<>"ComplyOperation.wl "<>subFolder<>"ComplyOperation.wl"];
	DumpSave[subFolder<>"operation.mx",operation];
	Export[subFolder<>"operation_identifier.txt",operation//InputForm//ToString];
	Export[subFolder<>"read_files.txt",OptionValue[ReadFiles]//InputForm//ToString];
	Run["mkdir "<>subFolder<>"inputs/"];
	Run["mkdir "<>subFolder<>"outputs/"];
	dimensions=Dimensions[input];
	tasks=Table@@Join[{
		task@@(i/@Range[Length[dimensions]])
	},Table[{i[j],dimensions[[j]]},{j,Length[dimensions]}]];
	tasksFlatten=Flatten[tasks];
	Table[
		Export[subFolder<>"inputs/input_"<>StringRiffle[ToString/@(tasksFlatten[[i]]/.task->List),"_"]<>".txt",input[[(tasksFlatten[[i]])/.task->Sequence]]//InputForm//ToString]
	,{i,Length[tasksFlatten]}];
	script=StringRiffle[Table[
		"math -script ComplyOperation.wl "<>subFolder<>"inputs/input_"<>StringRiffle[ToString/@(tasksFlatten[[i]]/.task->List),"_"]<>".txt\n"
	,{i,Length[tasksFlatten]}],""];
	Export[subFolder<>"script.txt",script];

	Print["\tdone. Time used: ",AbsoluteTime[]-timer," s."];
	timer=AbsoluteTime[];
	Print["Complying operation in parallel..."];
	dir=Directory[];
	SetDirectory[subFolder];
	Run["cat script.txt | parallel"];
	SetDirectory[dir];
	Print["\tdone. Time used: ",AbsoluteTime[]-timer," s."];
	timer=AbsoluteTime[];
	Print["Reading results..."];
	result=SparseArray[{},dimensions]//Normal;
	Table[
		result[[(tasksFlatten[[i]])/.task->Sequence]]=
		Get[subFolder<>"outputs/output_"<>StringRiffle[ToString/@(tasksFlatten[[i]]/.task->List),"_"]<>".txt"]
	,{i,Length[tasksFlatten]}];
	Print["\tdone. Time used: ",AbsoluteTime[]-timer," s."];
	Return[result];
]


(* ::Section:: *)
(*Usually used commands*)


ParallelFactor=ParallelComply[#,Factor]&;
ParallelTogether=ParallelComply[#,Together]&;
ParallelSimplify=ParallelComply[#,Simplify]&;
ParallelFullSimplify=ParallelComply[#,FullSimplify]&;
