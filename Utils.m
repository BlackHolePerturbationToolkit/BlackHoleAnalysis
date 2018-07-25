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
MakePDString;

OutputMessage;


Begin["`Private`"];


Options[GetSymbol]={"OverStr"->Null,"FormatSymbol"->True};
Options[AddTag]={"TagPosition"->"Up"};
Options[DropTag]={"TagPosition"->"Up"};


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


def@
OutputMessage[syms_Association,msg_String,level_Integer,opts:OptionsPattern[]]:=
Module[{fileNameBase,fileNames,outDir,minLevel=1,optionsRules,
		maxLevel=3,levelUsed,msgLevel,msgNoReturn,returnStr,style},

	Print[msg]
]


End[];

EndPackage[];
