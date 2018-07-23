(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`Utils`",
				{"BlackHoleAnalysis`OverloadedSymbols`",
					"GeneralUtilities`"}];


def;
reDef;
memDef;
testDef;
Aborting;

TestOptions;
TestMetaData;
MakeKeysStrings;

GetSymbol;
GetFormat;
AddTag;
DropTag;

DefineFormats;
GetNumInfo;
(*PlotDTNumInfo;*)

(*ChangeIndepVar;*)
(*JoinDataTables;
CutDataTable;
NumberToDT;
*)
(*SimpsonsRule;
TrapezoidRule;
LRectangleRule;
*)
MakePDString;
(*GetWithPackedDTs;*)

ModeCategory;
OutputDirectory;
SetOutputDirectory;
OutputMessage;
ReadOutput;
ReadError;
ReadSystem;

StoreSourceCode;

NumberToString;

ComplexQ;
If[$VersionNumber<10,BooleanQ];
RealPosQ;
RealNatQ;
RealQ;

(*ResamplePeriodicDataTable;

(* SimulationTools *)

FilterNaNs(*::usage = "FilterNaNs[d] replaces any NaN (Not a Number) values in the DataTable d with Missing[], which is Mathematica's notation for missing data."*);
NaNQ(*::usage = "NaNQ[x] returns True if x is a NaN (Not a Number) value and False if it is not.  Mathematica deals strangely with NaN values imported from other programs.  This function was developed for use with the h5mma package for reading HDF5 data."*);
RunSubprocess;

(* Experimental *)
MapMonitored;
TailFile;
ShowIt;
MapSuccessive;*)


(*NDSolveStep::usage="NDSolveStep[eqns,ys,ICs,{x,xI,xF}] numerically integrates the differential equations eqns with independent variable x and \
dependent variables ys from xI to xF. Initial conditions ICs are taken to be at xI and the final values of ys are returned at xF.";*)
(*NDSolveDT::usage="NDSolveDT[eqns,ys,ICs,{x,pts}] numerically integrates the differential equations eqns with independent variable x and \
dependent variables ys from the first point in the list pts to the last point in that list. Initial conditions ICs are taken to be at the \
first point in pts and a list of DataTables is returned with the values of ys at each of the points in pts.";*)


Begin["`Private`"];


Options[GetSymbol]={"OverStr"->Null,"FormatSymbol"->True};
Options[AddTag]={"TagPosition"->"Up"};
Options[DropTag]={"TagPosition"->"Up"};
(*Options[CutDataTable]={"MiddlePoint"->"Both"};*)
Options[OutputMessage]={"Style"->"Standard"};
(*Options[NDSolveStep]=Options[NDSolve];*)
(*Options[NDSolveDT]=Join[Options[NDSolveStep],{"Interpolate"->True}];*)


Unprotect[reDef];
ClearAll[reDef];
SetAttributes[reDef,HoldAll];
reDef/:SetDelayed[reDef[f_[args___]],rhs_]:=
	(f[x___]:=(Print["Invalid call to "<> ToString[f]<>"[]. Unexpected parameters: ",{x}];Abort[]);
	f[args]:=Block[{$CurrentFunction=f},rhs])
Protect[reDef];


Unprotect[def];
ClearAll[def];
SetAttributes[def,HoldAll];
def/:SetDelayed[def[f_[args___]],rhs_]:=
	(Clear[f];
	f[x___]:=(Print["Invalid call to "<> ToString[f]<>"[]. Unexpected parameters: ",{x}];Abort[]);
	f[args]:=Block[{$CurrentFunction=f},rhs])
Protect[def];


Unprotect[testDef];
ClearAll[testDef];
SetAttributes[testDef,HoldAll];
testDef/:SetDelayed[testDef[f_[args___]],rhs_]:=
	(f[x___]:=(Print["Invalid call to "<> ToString[f]<>"[]. Unexpected parameters: ",{x}];Abort[]);
	f[args]:=Block[{},rhs])
Protect[testDef];


Unprotect[memDef];
ClearAll[memDef];
SetAttributes[memDef,HoldAll];
memDef/:SetDelayed[memDef[fn_[args___]],rhs_]:=
	(Clear[fn];
	fn[x___]:=(Print["Invalid call to "<> ToString[fn]<>"[]. Unexpected parameters: ",{x}];Abort[]);
	fn[args2:PatternSequence[args]]:=fn[args2]=Block[{$CurrentFunction=fn},rhs])
Protect[memDef];


def@
DefineFormats[syms_Association]:=
Module[{GC,FC,EC},

	GC=GetSymbol[syms,"GCoefficient"];
	FC=GetSymbol[syms,"FCoefficient"];
	EC=GetSymbol[syms,"ECoefficient"];

	Format[GC[0][m_,n_][fn_]]:=ToString[GC[MakePDString[fn,{m,n}]],StandardForm];
	Format[GC[1][m_,n_][fn_]]:=ToString[FC[MakePDString[fn,{m,n}]],StandardForm];
	Format[GC[k_?(#>=2&)][m_,n_][fn_]]:=ToString[Subscript[EC,k][MakePDString[fn,{m,n}]],StandardForm];
]


def@
MakeKeysStrings[syms_List]:=
Module[{},
	Last@StringSplit[ToString[#[[1]]],"`"]->#[[2]]&/@syms
]


testDef@
testValue[optsTests_List,opt_Rule]:=
testValue[optsTests,opt]=
Module[{key,value,test},
	
	value=opt[[2]];
	key=opt[[1]];
	test=key/.optsTests;

	test[value]
]


testDef@
TestOptions[optsTests_List,optsGiven_List,funcName_:$CurrentFunction]:=
Module[{failList,testList,badTestList,
		optsDefault,optsTestsStr,optsGivenFlatStr,
		optsDefaultStr,defaultUsed,optsUsed},
	
	(* First test the default options *)
	optsDefault = Options[funcName];
	optsDefaultStr = MakeKeysStrings@optsDefault;
	optsTestsStr = MakeKeysStrings@optsTests;

	(* Test that the default options and tests lists are compatible *)
	compLists[optsTestsStr,optsDefaultStr,"Tests for Options","Default Options",funcName];

	testList = testValue[optsTestsStr, #] & /@ optsDefaultStr;
	failList = Flatten@Position[testList, False];
	badTestList = DeleteCases[testList,True|False];

	If[Length@badTestList=!=0,	
		Print["Options test "<> ToString@#<> " failed to evaluate to True or False."]&/@badTestList;
		Print["Aborting in "<>ToString[funcName]<>"[]."];
		Abort[];
	];

	If[Length@failList=!=0,
		Print[StringForm["Default option '`1`' did not satisfy test `2`", 
				optsDefaultStr[[#]], (optsDefaultStr[[#]]/.optsTestsStr)[[1]]]] & /@ failList;
		Print["Aborting in "<>ToString[funcName]<>"[]."];
		Abort[];
	];

	(* Now test the passed options *)
	optsGivenFlatStr = MakeKeysStrings@Flatten@optsGiven;
	defaultUsed=#->(#/.optsDefaultStr)&/@Complement[optsDefaultStr[[All,1]],optsGivenFlatStr[[All,1]]];
	optsUsed=Union[defaultUsed,optsGivenFlatStr];
	
	(* Test that the used options and tests lists are compatible *)
	compLists[optsTestsStr,optsUsed,"Tests for Options","Passed Options",funcName];
	
	testList = testValue[optsTestsStr, #] & /@ optsUsed;
	failList = Flatten@Position[testList, False];
	
	If[Length@failList=!=0,
		Print[StringForm["Passed option '`1`' did not satisfy test `2`", optsUsed[[#]], (optsUsed[[#]]/.optsTestsStr)[[1]]]] & /@ failList;
		Print["Aborting in "<>ToString[funcName]<>"[]."];
		Abort[];
	];
];


testDef@
compLists[l1_List,l2_List,str1Name_String,str2Name_String,funcName_:$CurrentFunction]:=
Module[{dups,comp},
	
	If[!(Sort@l2[[All,1]]===Sort@l1[[All,1]]===Union[l1[[All,1]],l2[[All,1]]]),

		dups=(Flatten@Position[l1[[All,1]],#])&/@Union@l1[[All,1]];
		If[Length@#=!=1,
			Print["Multiple values for element " <> ToString@l1[[#]]<>" in "<>str1Name];
			Print["Aborting in " <> ToString[funcName]];
			Abort[]
		]&/@dups;
	
		dups=(Flatten@Position[l2[[All,1]],#])&/@Union@l2[[All,1]];
		If[Length@#=!=1,
			Print["Multiple values for element " <> ToString@l2[[#]]<>" in "<>str2Name];
			Print["Aborting in " <> ToString[funcName]];
			Abort[]
		]&/@dups;
	
		comp=Complement[l1[[All,1]],l2[[All,1]]];
		If[Length@comp=!=0,
			Print["The following elements from "<>str1Name<> " were not found in "<>str2Name];
			Print@comp;
			Print["Aborting in " <> ToString[funcName]];
			Abort[]
		];
	
		comp=Complement[l2[[All,1]],l1[[All,1]]];
		If[Length@comp=!=0,
			Print["The following elements from "<>str2Name<> " were not found in "<>str1Name];
			Print@comp;
			Print["Aborting in " <> ToString[funcName]];
			Abort[]
		]

	]
]


testDef@
TestMetaData[optsTests_List,optsGiven_List,funcName_:$CurrentFunction]:=
Module[{failList,testList,optsGivenFlat,
		optsTestsTrunc,optsComp,
		optsTestsStr,optsGivenFlatStr},
			
	(* Test the passed meta data options *)
	optsTestsStr = MakeKeysStrings@optsTests;
	optsGivenFlat = Flatten@optsGiven;
	optsGivenFlatStr = MakeKeysStrings@optsGivenFlat;
	
	(* Check that only avaiable meta data options were passed *)
	optsComp=Complement[optsGivenFlatStr[[All,1]],optsTestsStr[[All,1]]];

	If[Length@optsComp=!=0,
		Print["The following unsupported meta data options were passed to "<>ToString[funcName]<>"[]:"];
		Print[optsComp];
		Abort[];
	];

	(* Check that all required meta data options were passed *)
	optsComp=Complement[optsTestsStr[[All,1]],optsGivenFlatStr[[All,1]]];

	If[Length@optsComp=!=0,
		Print["The following required meta data options were not passed to "<>ToString[funcName]<>"[]:"];
		Print[optsComp];
		Abort[];
	];
	(* Finally, run tests on the options that were passed *)
	optsTestsTrunc=(#[[1]]->(#[[1]]/.optsTestsStr)&)/@optsGivenFlatStr;
	testList=testValue[optsTestsTrunc, #] & /@ optsGivenFlatStr;
	failList = Flatten@Position[testList, False];
	
	If[Length@failList=!=0,
		Print[StringForm["Meta data option '`1`' did not satisfy test `2`", optsGivenFlatStr[[#]] ,optsTestsTrunc[[#,2]]]] & /@ failList;
		Print["Aborting in "<>ToString[funcName]<>"[]."];
		Abort[];
	];
];


testDef@
Aborting[fn_Symbol:$CurrentFunction]:=
Module[{},
	Print["Aborting in ", fn, "[]."];
	Abort[]
]


testDef@
Aborting[syms_Association,fn_Symbol:$CurrentFunction]:=
Module[{},
	OutputMessage[syms,"Aborting in "<>ToString[fn],1,Style->"Error"];
	Aborting[fn]
]


(*def@
ChangeIndepVar[dt_DataTable,transfFunc_]:=
Module[{dtList,sol},

	dtList=ToList@dt;
	sol={transfFunc[#[[1]]],#[[2]]}&/@dtList;

	ToDataTable@sol
]*)


(*def@
NumberToDT[num_?NumericQ,coords_List]:=ToDataTable@Table[{pt,num},{pt,coords}];*)


(*def@
JoinDataTables[dt1_DataTable,dt2_DataTable]:=
Module[{l1,l2,l},
	l1=ToList@dt1;
	l2=ToList@dt2;

	l=Which[
		l1[[1,1]]>l2[[-1,1]],
		{l2,l1},
		l1[[-1,1]]<l2[[1,1]],
		{l1,l2},
		True,
		Print["Cannot Join overlapping DataTables ", dt1, dt2];
		Aborting[]
	];

	ToDataTable@Join[Sequence@@l]
]

reDef@
JoinDataTables[dt1_DataTable,dt2_DataTable,dt3_DataTable..]:=JoinDataTables[JoinDataTables[dt1,dt2],dt3]*)


(*DataTable/:Most[d_DataTable]:=Drop[d,-1]
DataTable/:Rest[d_DataTable]:=Drop[d,1]*)


(*DataTable/:Power[a_,dt_DataTable]/;NumericQ[a]:=ToDataTable[ToListOfCoordinates[dt],Power[a,ToListOfData[dt]]];*)


(*DataTable/:Times[a___,dt_DataTable,b___]/;DeleteDuplicates[Join[NumericQ/@{a,b},{True}]]=={True}:=ToDataTable[ToListOfCoordinates[dt],Times[a,ToListOfData[dt],b]];*)


(*DataTable/:Plus[a___,dt_DataTable,b___]/;DeleteDuplicates[Join[NumericQ/@{a,b},{True}]]=={True}:=ToDataTable[ToListOfCoordinates[dt],Plus[a,ToListOfData[dt],b]];*)


(*DataTable/:Power[dt_DataTable,a_]/;NumericQ[a]:=ToDataTable[ToListOfCoordinates[dt],Power[ToListOfData[dt],a]];*)


(*def@
CutDataTable[dt_DataTable,xp_?NumericQ,opts:OptionsPattern[]]:=
Module[{pt,dtP,dtM,l1,mp,optionsRules,ptP,ptM,xpAct},

	optionsRules = {"MiddlePoint" -> Function[x,x==="Left"||x==="Right"||x==="Both"||x==="Neither"]};

	TestOptions[optionsRules,{opts}];

	mp=OptionValue[MiddlePoint];

	l1=ToListOfCoordinates@dt;

	If[xp>=Last@l1||xp<=First@l1,
		Print["Cannot cut DataTable at point ", xp, " It is outside coordinate range ",{ First@l1,Last@l1}];
		Aborting[]
	];
	xpAct=First@Nearest[l1,xp];
	pt=First@First@Position[l1,xpAct];
	
	{ptM,ptP}=Switch[mp,
					"Left",
					{pt,pt+1},
					"Right",
					{pt-1,pt},
					"Both",
					{pt,pt},
					"Neither",
					{pt-1,pt+1}
				];

	dtM=dt[[1;;ptM]];
	dtP=dt[[ptP;;-1]];
	{dtP,dtM}
]*)


(*def@
ResamplePeriodicDataTable[d_DataTable,newChis_List]:=
Module[{list,deltaChi,num,l2,l,dOfChi,coords,data,prec},
	l=ToList[d];
	prec=Precision[l[[All,1]]];
	deltaChi=l[[2,1]];
	data=Join[l[[All,2]],Reverse@Take[l[[All,2]],{2,-2}]];
	coords=Join[l[[All,1]],2 l[[-1,1]]-Reverse@Take[l[[All,1]],{2,-2}]];
	list=Thread[{coords,data}];
	num=Length@list;
	
	dOfChi[chi_]:=
	Which[(l2=Select[list,Abs[#[[1]]-chi]<10^(-.95prec)&])=={},
		1/(2 num) Sin[(\[Pi] chi)/deltaChi]Sum[list[[n+1,2]](-1)^n ((-1)^(num+1) Tan[\[Pi] (chi-n deltaChi)/(2 num deltaChi)]+Cot[\[Pi] (chi-n deltaChi)/(2 num deltaChi)]),{n,0,num-1}],
		Length[l2]==1,
		l2[[1,2]],
		True,
		Print["Multiple original chi points found near new point ", chi];
		Aborting[]
	];

	ToDataTable[newChis,dOfChi/@newChis]
]*)


(*def@
SimpsonsRule[dt_DataTable]:=
Module[{nCells,factors,coords},
	nCells=Length[dt]-1;
	coords=ToListOfCoordinates[dt];
	factors=ToDataTable[coords,Flatten@Join[{1,4},ConstantArray[{2,4},(nCells-2)/2],{1}]];
	(Last[coords]-First[coords])/(3 nCells) Total[factors dt]
]*)


(*def@
TrapezoidRule[dt_DataTable]:=
Module[{nCells,factors,coords},
	nCells=Length[dt]-1;
	coords=ToListOfCoordinates[dt];
	factors=ToDataTable[coords,Flatten@Join[{1},ConstantArray[{2},nCells-1],{1}]];
	(Last[coords]-First[coords])/(2nCells) Total[factors dt]
]*)


(*def@
LRectangleRule[dt_DataTable]:=
Module[{nCells,coords},
	nCells=Length[dt]-1;
	coords=ToListOfCoordinates[dt];
	(Last[coords]-First[coords])/nCells Total[Most@dt]
]*)


extractSymbol[syms_Association,baseStr_String,funcName_:$CurrentFunction]:=
extractSymbol[syms,baseStr,funcName]=syms[baseStr]


memDef@
getTagFormat[head_["Tags"->tags_]/;MatchQ[tags,{HoldPattern[_String->{__String}]..}]]:=
Module[{upSyms,downSyms,tagPos,wrongTags,strSup,strSub},
	tagPos=Cases[tags,(fn_->_):>fn,2];
	wrongTags=DeleteCases[tagPos,_?(#==="Up"||#==="Down"&)];
	
	If[wrongTags=!={},
		Print["The following tag positions are invalid."];
		Print[wrongTags];
		Aborting[]
	];

	If[Count[tagPos,"Up"]>1,
		Print["Multiple \"Up\" strings given."];
		Print[tags];
		Aborting[]
	];
	
	If[Count[tagPos,"Down"]>1,
		Print["Multiple \"Down\" strings given."];
		Print[tags];
		Aborting[]
	];
	
	If[MemberQ[tagPos,"Up"],
		upSyms="Up"/.tags;
		strSup=StringJoin[Most[upSyms]/.x_String:>x<>",",Last[upSyms]]
	];
	
	If[MemberQ[tagPos,"Down"],
		downSyms="Down"/.tags;
		strSub=StringJoin[Most[downSyms]/.x_String:>x<>",",Last[downSyms]];
	];
	
	Which[
		tagPos==={"Up"},
		ToString[Superscript[head,strSup],StandardForm],
		tagPos==={"Down"},
		ToString[Subscript[head,strSub],StandardForm],
		tagPos==={"Down","Up"},
		ToString[Subsuperscript[head,strSub,strSup],StandardForm],
(*		ToString[\!\(
\*SubsuperscriptBox[\(head\), \(strSub\), \(strSup\)], StandardForm\)],*)
		True,
		Print["Tags must be given in form \"Down\"->{...},\"Up\"->{...}, not \"Up\"->{...},\"Down\"->{...}"];
		Aborting[]
	]
]


memDef@
getIndexFormat[head_["Indices"->inds_]/;MatchQ[inds,{HoldPattern[_String->{__String}]..}]]:=
Module[{upSyms,downSyms,indsPos,wrongTags,strSup,strSub},
	indsPos=Cases[inds,(fn_->_):>fn,2];
	wrongTags=DeleteCases[indsPos,_?(#==="Up"||#==="Down"&)];
	
	If[wrongTags=!={},
		Print["The following index positions are invalid."];
		Print[wrongTags];
		Aborting[]
	];

	If[Count[indsPos,"Up"]>1,
		Print["Multiple \"Up\" strings given."];
		Print[inds];
		Aborting[]
	];

	If[Count[indsPos,"Down"]>1,
		Print["Multiple \"Down\" strings given."];
		Print[inds];
		Aborting[]
	];

	If[MemberQ[indsPos,"Up"],
		upSyms="Up"/.inds;
		strSup=StringJoin[upSyms]
	];

	If[MemberQ[indsPos,"Down"],
		downSyms="Down"/.inds;
		strSub=StringJoin[downSyms]
	];

	Which[
		indsPos==={"Up"},
		ToString[DisplayForm[SuperscriptBox[head,strSup]],StandardForm],
		indsPos==={"Down"},
		ToString[DisplayForm[SubscriptBox[head,strSub]],StandardForm],
		indsPos==={"Down","Up"},
		ToString[DisplayForm[SuperscriptBox[SubscriptBox[head,strSub],strSup]],StandardForm],
		indsPos==={"Up","Down"},
		ToString[DisplayForm[SubscriptBox[SuperscriptBox[head,strSup],strSub]],StandardForm],
		True,
		Print["Error in Which[] call in getIndexFormat[]"];
		Aborting[]
	]
]


def@
GetFormat[syms_Association,headSym_Symbol]:=ToString[headSym,StandardForm]

reDef@
GetFormat[syms_Association,headSym_Symbol["OverStr"->overStr_]]:=
Module[{overSym},
	overSym=extractSymbol[syms,overStr];
	ToString[Overscript[headSym,overSym],StandardForm]
]

reDef@
GetFormat[syms_Association,head_[label_/;(label==="Tags"||label==="Indices")->subSyms_]]:=
Module[{ud,notUD,upSyms,downSyms,strSup,strSub,
newSymsU,newSymsUD},

	ud=Cases[subSyms,(fn_->_):>fn,2];
	notUD=DeleteCases[ud,_?(#==="Up"||#==="Down"&)];
	
	If[notUD=!={},
		Print["The following index positions are invalid."];
		Print[notUD];
		Aborting[]
	];

	If[Count[ud,"Up"]>1,
		Print["Multiple \"Up\" strings given."];
		Print[ud];
		Aborting[]
	];

	If[Count[ud,"Down"]>1,
		Print["Multiple \"Down\" strings given."];
		Print[ud];
		Aborting[]
	];

	If[MemberQ[ud,"Up"],
		upSyms=extractSymbol[syms,#]&/@("Up"/.subSyms);
		strSup=ToString/@upSyms;
		newSymsU=subSyms/.HoldPattern["Up"->__]:>"Up"->strSup,
		newSymsU=subSyms
	];

	If[MemberQ[ud,"Down"],
		downSyms=extractSymbol[syms,#]&/@("Down"/.subSyms);
		strSub=ToString/@downSyms;
		newSymsUD=newSymsU/.HoldPattern["Down"->__]:>"Down"->strSub,
		newSymsUD=newSymsU
	];

	If[label==="Tags",
		getTagFormat[GetFormat[syms,head][label->newSymsUD]],
		getIndexFormat[GetFormat[syms,head][label->newSymsUD]]
	]
]


def@
AddTag[syms_Association,head_["Tags"->subSyms_],tagStr_String,opts:OptionsPattern[]]:=
Module[{tagPos,optionsRules,realHead,
		tagSymsUp,tagSymsDown,tagLabels,
		tagSymsUpNew,tagSymsDownNew,fullSymbol},

	optionsRules = {"TagPosition"->Function[x, x==="Up" || x==="Down"]};
	TestOptions[optionsRules,{opts},AddTag];

	tagPos=OptionValue[TagPosition];
	
	tagLabels=Cases[subSyms,(fn_->_):>fn,2];
	
	tagSymsUp=If[MemberQ[tagLabels,"Up"],"Up"/.subSyms,{}];
	tagSymsDown=If[MemberQ[tagLabels,"Down"],"Down"/.subSyms,{}];
	
	tagSymsUpNew=If[tagPos==="Up",Join[tagSymsUp,{tagStr}],tagSymsUp];
	tagSymsDownNew=If[tagPos==="Down",Join[tagSymsDown,{tagStr}],tagSymsDown];
	
	fullSymbol=
		Which[tagSymsUpNew==={},
			head["Tags"->{"Down"->tagSymsDownNew}],
			tagSymsDownNew==={},
			head["Tags"->{"Up"->tagSymsUpNew}],
			True,
			head["Tags"->{"Down"->tagSymsDownNew,"Up"->tagSymsUpNew}]
		];

	realHead=head//.x_[y__]:>x;
	Format[fullSymbol]=GetFormat[syms,fullSymbol];

	fullSymbol
]

reDef@
AddTag[syms_Association,head_,tagStr_String,opts:OptionsPattern[]]:=
Module[{tagPos,optionsRules,fullSymbol,realHead},

	optionsRules = {"TagPosition"->Function[x, x==="Up" || x==="Down"]};
	TestOptions[optionsRules,{opts},AddTag];
	
	tagPos=OptionValue[TagPosition];
	
	fullSymbol=head["Tags"->{tagPos->{tagStr}}];
	realHead=head//.x_[y__]:>x;
	Format[fullSymbol]=GetFormat[syms,fullSymbol];
	
	fullSymbol
]


def@
DropTag[syms_Association,head_["Tags"->tagList_],tag_String,opts:OptionsPattern[]]:=
Module[{tlMod,optionsRules,tagPos,fullSymbol,tagSym,tlMod2,realHead},

	optionsRules = {"TagPosition"->Function[x, x==="Up" || x==="Down"]};
	TestOptions[optionsRules,{opts}];
	
	tagPos=OptionValue[TagPosition];
	
	If[Cases[tagList,(tagPos->{___,tag})->tag,2]==={},
		tagSym=extractSymbol[syms,tag];
		Print["Attempting to drop tag ", tagSym, " that doesn't exist from symbol ",head["Tags"->tagList]];
		Print["DropTag[] can only drop the last tag and the correct \"Up\"/\"Down\" position must be specified."];
		Aborting[]
	];
	
	If[tagPos==="Up",
		tlMod=tagList/.HoldPattern["Up" ->{x___,tag}]:>"Up"->{x};
		tlMod2=DeleteCases[tlMod,HoldPattern["Up" ->{}]]
		,
		tlMod=tagList/.HoldPattern["Down" ->{x__,tag}]:>"Down"->{x};
		tlMod2=DeleteCases[tlMod,HoldPattern["Down" ->{}]]
	];
		
	fullSymbol=
		If[tlMod2==={},
			head,
			head["Tags"->tlMod2]
		];

	realHead=head//.x_[y__]:>x;
	Format[fullSymbol]=GetFormat[syms,fullSymbol];
	
	fullSymbol
]


testDef@
GetSymbol[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{optionsRules,overStr,symbol,str,fullSymbol,format,head},

	optionsRules = {"OverStr"->Function[x,StringQ[x]||x===Null],
					"FormatSymbol"->Function[x,(x === True || x === False)]};
	TestOptions[optionsRules,{opts},GetSymbol];
	
	overStr=OptionValue[OverStr];
	format=OptionValue[FormatSymbol];

	str=(expr//.fn_[___]:>fn);
	
	If[Length@Cases[{expr},str,Infinity,Heads->True]=!=1,
		Print["Symbols cannot include tags or indices that are the same as the symbol Head." ];
		Print[str<>" violates this rule."];
		Aborting[]
	];

	symbol=extractSymbol[syms,str];
	
	If[StringQ[expr]&&overStr===Null,
		Return@symbol
	];

	fullSymbol=
		If[overStr===Null,
			expr/.str->symbol,
			expr/.str->symbol["OverStr"->overStr]
		];

	If[format,
		head=fullSymbol//.x_[y__]:>x;
		Format[fullSymbol]=GetFormat[syms,fullSymbol];
		fullSymbol,
		InputForm[fullSymbol]
	]
]


def@
GetNumInfo[num_?NumericQ]:=
Module[{},
	Print["Number = ",num//FullForm];
	Print["Precision = ",Precision[num], If[Precision[num]===MachinePrecision," = "<>ToString@FullForm@N[Precision[num]],""]];
	Print["Accuracy = ",Accuracy[num]];
	Print["RealExponent = ",RealExponent[num]//FullForm];
	Print["RealDigits = ",RealDigits[num][[1]]//Length];
]


GetWithPackedDTs[file_String]:=Get[file]/.dt_DataTable:>ToPackedArray[dt]


(*PlotDTNumInfo[dt_DataTable]:=
ListLinePlot[{Precision/@dt,Accuracy/@dt,RealExponent/@dt},PlotRange->All,PlotLegends->{"Precision","Accuracy","RealExponent"}]*)


def@
MakePDString[sym_,{m_,n_}]:=
Module[{},

	Which[
	{m,n}=={0,0},
		ToExpression@ToBoxes@sym,
	{m,n}=={1,0},
		"\!\(\*SubscriptBox[\(\[PartialD]\), \(t\)]\)"<>ToExpression@ToBoxes@sym,
	{m,n}=={0,1},
		"\!\(\*SubscriptBox[\(\[PartialD]\), \(r\)]\)"<>ToExpression@ToBoxes@sym,
	{m,n}=={1,1},
		\!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*SubscriptBox[\\(\\[PartialD]\\), \\(t\\)]\\)\\!\\(\\*SubscriptBox[\\(\\[PartialD]\\), \\(r\\)]\\)\>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)<>ToExpression@ToBoxes@sym,
	m>1&&n==0,
		ToString@StringForm["\!\(\*SubsuperscriptBox[\(\[PartialD]\), \(t\), \(`1`\)]\)",m]<>ToExpression@ToBoxes@sym,
	m==0&&n>1,
		ToString@StringForm["\!\(\*SubsuperscriptBox[\(\[PartialD]\), \(r\), \(`1`\)]\)",n]<>ToExpression@ToBoxes@sym,
	m>1&&n==1,
		ToString@StringForm["\!\(\*SubsuperscriptBox[\(\[PartialD]\), \(t\), \(`1`\)]\)",m]<>"\!\(\*SubscriptBox[\(\[PartialD]\), \(r\)]\)"<>ToExpression@ToBoxes@sym,
	m==1&&n>1,
		"\!\(\*SubscriptBox[\(\[PartialD]\), \(t\)]\)"<>ToString@StringForm["\!\(\*SubsuperscriptBox[\(\[PartialD]\), \(r\), \(`1`\)]\)",n]<>ToExpression@ToBoxes@sym,
	m>1&&n>1,
		ToString@StringForm["\!\(\*SubsuperscriptBox[\(\[PartialD]\), \(t\), \(`1`\)]\)\!\(\*SubsuperscriptBox[\(\[PartialD]\), \(r\), \(`2`\)]\)",m,n]<>ToExpression@ToBoxes@sym
	]
]


def@ModeCategory[lN_Integer/;lN>=0]:=Switch[lN,0,"Monopole",1,"Dipole",_,"Radiative"]


def@
OutputDirectory[syms_Association]:=GetSymbol[syms,"OutputDirectory"]


def@
SetOutputDirectory[syms_Association,dir_String]:=If[Length@Cases[syms,HoldPattern["OutputDirectory"->_]]==0,Join[syms,Association["OutputDirectory"->dir]],syms/.("OutputDirectory"->_):>("OutputDirectory"->dir)]


def@
OutputMessage[syms_Association,msg_String,level_Integer,opts:OptionsPattern[]]:=
Module[{fileNameBase,fileNames,outDir,minLevel=1,optionsRules,
		maxLevel=3,levelUsed,msgLevel,msgNoReturn,returnStr,style},

	Print[msg]
(*	optionsRules = {"Style" -> Function[x,MemberQ[{"Standard","Error","System"},x]||MatchQ[x,_List]]};
	TestOptions[optionsRules,{opts}];
	style=OptionValue[Style];

	If[MatchQ[style,_List],
		OutputMessage[syms,msg,level,Style->#]&/@style,

		fileNameBase=
		Switch[style,
			"Standard","Output_",
			"Error","Error_",
			"System","System_"
		];

		levelUsed=
		Which[level>maxLevel,
			OutputMessage[syms,"Attemping to output at level "<> ToString[level]<> " which exceeds maxLevel = "<> ToString[maxLevel],2,Style->"Error"];
			OutputMessage[syms,"Decreasing level to "<> ToString[maxLevel],2,Style->"Error"];
			maxLevel,
			level<minLevel,
			OutputMessage[syms,"Attemping to output at level "<> ToString[level]<> " which is less than minLevel = "<> ToString[minLevel],2,Style->"Error"];
			OutputMessage[syms,"Increasing level to "<> ToString[minLevel],2,Style->"Error"];
			minLevel,
			True,
			level
		];
	
		outDir=OutputDirectory[syms];
		If[Not[DirectoryQ[outDir]],CreateDirectory[outDir]];
		fileNames=FileNameJoin[{outDir,fileNameBase<>ToString[#]<>".txt"}]&/@Range[levelUsed,maxLevel];
		{returnStr,msgNoReturn}=StringReplace[msg,x:"\n"...~~rest__:>{x,rest}][[1]];
		msgLevel="Level "<>ToString[levelUsed]<>": "<>msgNoReturn;
		msgToFile[#,msgLevel,returnStr]&/@fileNames
	]*)
]


def@
ReadOutput[syms_,n_Integer:1]:=ReadList[FileNameJoin[{OutputDirectory[syms],"Output_"<>ToString[n]<>".txt"}],String]

reDef@
ReadOutput[syms_,n_Integer,takeNum_]:=Take[ReadOutput[syms,n],takeNum]


def@
ReadError[syms_,n_Integer:1]:=ReadList[FileNameJoin[{OutputDirectory[syms],"Error_"<>ToString[n]<>".txt"}],String]

reDef@
ReadError[syms_,n_Integer,takeNum_]:=Take[ReadError[syms,n],takeNum]


def@
ReadSystem[syms_,n_Integer:1]:=ReadList[FileNameJoin[{OutputDirectory[syms],"System_"<>ToString[n]<>".txt"}],String]

reDef@
ReadSystem[syms_,n_Integer,takeNum_]:=Take[ReadSystem[syms,n],takeNum]


def@
msgToFile[file_String,msg_String,returnStr_]:=
Module[{stream,kernelMsg},
	kernelMsg=returnStr<>"Kernel "<>ToString[$KernelID]<>": "<>msg<>"\n";
	stream=OpenAppend[file];
	WriteString[stream,kernelMsg];
	Close[stream]
]


def@
NumberToString[num_/;RealQ[num]]:=
Module[{digits,spanI,spanF,exp,sign,digitsAdj,numN},

	numN=If[Cases[num,_Log,Infinity]=!={},N[num],num];
	{digits,exp}=RealDigits[numN];
	sign=If[Negative[numN],"-",""];
	spanF=If[Length[digits]>=5,5,Length[digits]];
	spanI=If[Length[digits]==1,1,2];
	digitsAdj=PadRight[digits,exp+1];

	If[exp<=3&&exp>=0,

		If[MatchQ[numN,_Real],
			sign<>StringJoin[ToString/@digitsAdj[[1;;exp]]]<>If[exp+1<=Length[digits],"."<>StringJoin[ToString/@digits[[exp+1;;spanF]]],""],
			ToString[numN]
		],
	
		sign<>ToString[digits[[1]]]<>"."<>If[Length[digits]>1,StringJoin[ToString/@digits[[spanI;;spanF]]],""]<>"*10^"<>ToString[exp-1]
	]
]


NumberToString[num_/;ComplexQ[num]]:=NumberToString[Re[num]]<>If[Positive[Im[num]],"+",""]<>NumberToString[Im[num]]<>"*I"


def@
StoreSourceCode[syms_Association]:=
Module[{mFiles,sourceDir,schwDir,kernelDir,sourceMFiles,oldDir,gitStatus,stream,gitCommit,sourceMDir},
	oldDir=Directory[];
	schwDir=FileNameJoin[{$UserBaseDirectory,"Applications","BlackHoleAnalysis"}];
	kernelDir=FileNameJoin[{schwDir,"Kernel"}];
	sourceDir=FileNameJoin[{OutputDirectory[syms],"Source","BlackHoleAnalysis"}];
	sourceMDir=FileNameJoin[{sourceDir,"MFiles"}];

	mFiles=FileNames["*.m",{schwDir}];
	sourceMFiles=FileNameJoin[{sourceMDir,FileNameTake[#]}]&/@mFiles;
	
	If[Not[DirectoryQ[sourceDir]],CreateDirectory[sourceDir]];
	If[Not[DirectoryQ[sourceMDir]],CreateDirectory[sourceMDir]];
	CopyDirectory[kernelDir,FileNameJoin[{sourceDir,"Kernel"}]];
	MapThread[CopyFile,{mFiles,sourceMFiles}];
	SetDirectory[schwDir];
	gitStatus=ReadList["!git status ",Record];
	gitCommit=ReadList["!git log --pretty=oneline | head -1",Record];
	
	stream=OpenWrite[FileNameJoin[{sourceDir,"gitStatus.txt"}]];
	WriteString[stream,"Current commit:\n"];
	WriteString[stream,gitCommit<>"\n\n"];
	WriteString[stream,"git status:\n"];
	WriteString[stream,#<>"\n"]&/@gitStatus;
	Close[stream];

	SetDirectory[oldDir];
]


def@
RealQ[num_]:=NumericQ[num]&&Element[num,Reals]


def@
ComplexQ[num_]:=NumericQ[num]&&Not[RealQ[num]]


def@
RealPosQ[num_]:=NumericQ[num]&& Element[num,Reals]&&num>0


def@
RealNatQ[num_]:=NumericQ[num]&& Element[num,Reals]&&(num>=0)


(*def@
NDSolveStep[eqns_List,depVars_List,ICs_List,{indepVar_,xI_,xF_},opts:OptionsPattern[]]:=
Block[{interpFns},
	interpFns=NDSolveValue[{Sequence@@eqns,Sequence@@MapThread[#1[xI]==#2&,{depVars,ICs}]},depVars,{indepVar,xI,xF},opts];
	SetPrecision[#[xF]&/@interpFns,OptionValue[WorkingPrecision]]
]
reDef@
NDSolveStep[eqn_,depVar_,IC_,{indepVar_,xI_,xF_},opts:OptionsPattern[]]:=NDSolveStep[{eqn},{depVar},{IC},{indepVar,xI,xF},opts]*)


(*def@
NDSolveDT[eqns_List,depVars_List,ICs_List,{indepVar_,pts_},opts:OptionsPattern[]]:=
Module[{ndStep,data,interpFns,xI},
	
	If[OptionValue["Interpolate"],
		xI=First@pts;
		interpFns=NDSolveValue[{Sequence@@eqns,Sequence@@MapThread[#1[xI]==#2&,{depVars,ICs}]},depVars,{indepVar,First@pts,Last@pts},FilterRules[{opts},Except["Interpolate"]]];
		ToDataTable[pts,Table[#[xi],{xi,pts}]]&/@interpFns,

		ndStep[{xITemp_,xFTemp_},ICsTemp_]:={xFTemp,NDSolveStep[eqns,depVars,ICsTemp,{indepVar,xITemp,xFTemp},FilterRules[{opts},Except["Interpolate"]]]};
		data=FoldList[ndStep[{#1[[1]],#2},#1[[2]]]&,{First@pts,ICs},Rest@pts][[All,2]];
		ToDataTable[pts,#]&/@Thread[data]
	]
]
reDef@
NDSolveDT[eqn_,depVar_,IC_,{indepVar_,pts_},opts:OptionsPattern[]]:=NDSolveDT[{eqn},{depVar},{IC},{indepVar,pts},opts]*)


(* SimulationTools *)


(* Copyright 2010-2012 Ian Hinder and Barry Wardell

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

(**********************************************************)
(* FilterNaNs                                             *)
(**********************************************************)

SyntaxInformation[FilterNaNs] =
 {"ArgumentsPattern" -> {_}};

SetAttributes[FilterNaNs, {Listable}];

FilterNaNs[d_] :=
 If[NaNQ[d], Missing[], d];


(**********************************************************)
(* NaNQ                                                   *)
(**********************************************************)

SyntaxInformation[NaNQ] =
 {"ArgumentsPattern" -> {_}};

NaNQ[x_] :=
 Round[x] == -2147483648;

If[$VersionNumber < 9.,
   CreateTemporary[] :=
   Module[
     {name},
     name = FileNameJoin[{$TemporaryDirectory, IntegerString[RandomInteger[10^50], 16]}];
     Close[OpenWrite[name]];
     name]];

RunSubprocess[{cmd_, args___}] :=
  Module[
    {stdoutFile, stderrFile, cmdString, retCode, stdout, stderr},
    stdoutFile = CreateTemporary[];
    stderrFile = CreateTemporary[];
    (* TODO: handle quoting *)
    cmdString = StringJoin[Riffle[{cmd, args}, " "]];
    (* Run executes the cmdString using a shell *)
    retCode = Run[cmdString <> ">"<>stdoutFile<>" 2>"<>stderrFile];
    stdout = ReadList[stdoutFile, String, NullRecords -> True];
    stderr = ReadList[stderrFile, String, NullRecords -> True];
    DeleteFile[stdoutFile];
    DeleteFile[stderrFile];
    {retCode, stdout, stderr}];

MapMonitored[f_, args_List] :=
 Module[{x = 0},
  Monitor[MapIndexed[(x = #2[[1]]; f[#1]) &, args], 
   ProgressIndicator[x/Length[args]]]];

MapSuccessive[f_, l_List] :=
 MapThread[f, Drop[#, -1] & /@ {l, RotateLeft[l]}]


TailFile[filename_String, n_Integer] :=
 Module[{size, handle, lines},
  size = FileByteCount[filename];
  handle = OpenRead[filename];
  SetStreamPosition[handle, Max[size - n, 0]];
  lines = ReadList[handle, String];
  Close[handle];
  StringJoin[Riffle[lines, "\n"]]];

(* http://mathematica.stackexchange.com/questions/2230/mathematica-debuggability *)
SetAttributes[ShowIt, HoldAll];
ShowIt[code_] := 
   Module[{y}, 
      Print[ToString[Unevaluated[code]], " = ", y = code]; 
      y]


End[];

EndPackage[];
