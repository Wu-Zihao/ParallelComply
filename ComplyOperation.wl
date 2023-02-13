(* ::Package:: *)

workingPath=DirectoryName[$InputFileName]


Get[workingPath<>"operation.mx"]


Get/@Get[workingPath<>"read_files.txt"]


Operation=Get[workingPath<>"operation_identifier.txt"]


inputFilePath=$CommandLine[[-1]]


input=Get[inputFilePath]


output=Operation[input]


preOutputFilePath=FileNameSplit[inputFilePath]
preOutputFilePath[[-2]]=StringReplace[preOutputFilePath[[-2]],"input"->"output"]
preOutputFilePath[[-1]]=StringReplace[preOutputFilePath[[-1]],"input"->"output"]


outputFilePath=FileNameJoin[preOutputFilePath]


Export[outputFilePath,output//InputForm//ToString]
