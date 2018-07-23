(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`AssociationNested`",{"BlackHoleAnalysis`Utils`"}];


AssociationNested;
NormalNested;
NestedKeyExistsQ;
JoinNested;
KeysNested;
(*JoinDatasets;*)
SetValueNested;


Begin["`Private`"];


def@
AssociationNested[l:{HoldPattern[_->_]...}]:=AssociationNested[l,1];

reDef@
AssociationNested[l:{HoldPattern[_->_]...},level_Integer/;level>=1]:=Association[If[MatchQ[#,_->{HoldPattern[_->_]...}&&level>1],#[[1]]->AssociationNested[#[[2]],level-1],#]&/@l]

reDef@
AssociationNested[l:{HoldPattern[_->_]...},Infinity]:=Association[If[MatchQ[#,_->{HoldPattern[_->_]...}],#[[1]]->AssociationNested[#[[2]],Infinity],#]&/@l]


def@
NestedKeyExistsQ[a_,keys:HoldPattern[_]..]:=
Module[{temp},
	If[MatchQ[a,_Association],
		If[Length[{keys}]==1,
			KeyExistsQ[a,First@{keys}],
			If[MatchQ[temp=a[First@{keys}],_Association],NestedKeyExistsQ[temp,Sequence@@Rest@{keys}],False]
		]
		,
		Message[NestedKeyExistsQ::argx,a]
	]
]

NestedKeyExistsQ::argx="The argument `1` is not a valid Association.";


def@
NormalNested[a_Association]:=NormalNested[a,1];

reDef@
NormalNested[a_Association,Infinity]:=
Module[{rules},
	rules=NormalNested[a];
	rules//.aa_Association:>Normal[aa]
]

reDef@
NormalNested[a_Association,depth_Integer/;depth>=1]:=
Module[{rules},
	rules=Normal[a];
	If[depth==1,
		rules,
		rules/.aa_Association:>NormalNested[aa,depth-1]
	]
]


def@
SetValueNested[a_Association,keyValues:HoldPattern[_]..]:=
If[NestedKeyExistsQ[a,Sequence@@Most@{keyValues}],
	replaceValue[a,keyValues],
	newValue[a,keyValues]
]


def@
newValue[a_Association,key_,value_]:=
If[KeyExistsQ[a,key],
	Print["Key ",key, " already in list of keys ", Keys@a];
	Abort[],
	Append[a,key->value]
]

reDef@
newValue[a_Association,key_,key2_,value:HoldPattern[_]..]:=
If[KeyExistsQ[a,key],
	If[MatchQ[a[key],_Association],
	Append[a,key->newValue[a[key],key2,value]],
	Append[a,key->nestedAssoc[key2,value]],
	Print[a[key] ," not an Association. Key ", key2, " cannot be looked up in it."];
	Abort[]
	],
	Append[a,key->nestedAssoc[key2,value]]
]


def@
nestedAssoc[key_,value_]:=Association[{key->value}]

reDef@
nestedAssoc[key_,key2_,value:HoldPattern[_]..]:=Association[{key->nestedAssoc[key2,value]}]


def@
replaceValue[a_Association,key_,value_]:=
If[KeyExistsQ[a,key],
	Append[a,key->value],
	Print["Key ",key, " not found in list of keys ", Keys@a];
	Abort[]
]

reDef@
replaceValue[a_Association,key_,key2_,value:HoldPattern[_]..]:=
If[KeyExistsQ[a,key],
	If[MatchQ[a[key],_Association],
	Append[a,key->replaceValue[a[key],key2,value]],
	Print[a[key] ," not an Association. Key ", key2, " cannot be looked up in it."];
	Abort[]
	],
	Print["Key ",key, " not found in list of keys ", Keys@a];
	Abort[]
]


def@
JoinNested[a_Association,keys:HoldPattern[_]...,a2_Association]:=
Module[{aNew},
	aNew=a;
	Do[
		aNew=
			If[NestedKeyExistsQ[aNew,keys,k],
				If[MatchQ[aNew[keys,k],_Association]&&MatchQ[a2[k],_Association],
					SetValueNested[aNew,keys,k,JoinNested[aNew[k],keys,a2[k]]],
					Print["Associations with the same key sequence: ", Sequence@@(ToString[#]<>", "&/@{k}), "cannot be joined."];
					Abort[]
				],
				SetValueNested[aNew,keys,k,a2[k]]],
		{k,Keys[a2]}
	];
	aNew
]


reDef@
JoinNested[a:{_Association,_Association..}]:=Fold[JoinNested,First@a,Rest@a]


reDef@
JoinNested[{a_Association}]:=a


(*def@
JoinDatasets[ds1_Dataset,ds2_Dataset]:=Dataset[JoinNested[Normal[ds1],Normal[ds2]]]*)


(*reDef@
JoinDatasets[l1:{_Dataset,_Dataset..}]:=Fold[JoinDatasets,First@l1,Rest@l1]*)


def@
KeysNested[a_Association]:=KeysNested[a,1]

reDef@
KeysNested[a_Association,depth_/;depth>=1]:=subKeys[a,#,depth]&/@Keys[a]

reDef@
KeysNested[a_Association,Infinity]:=subKeys[a,#,Infinity]&/@Keys[a]


def@
subKeys[a_Association,key_,Infinity]:=
Module[{value},
	value=a[key];
	If[MatchQ[value,_Association],
		key->(subKeys[value,#,Infinity]&/@Keys[value]),
		key
	]
]

reDef@
subKeys[a_Association,key_,depth_/;depth>=1]:=
Module[{value},
	value=a[key];
	If[MatchQ[value,_Association]&&depth>1,
		key->(subKeys[value,#,depth-1]&/@Keys[value]),
		key
	]
]


End[];

EndPackage[];
