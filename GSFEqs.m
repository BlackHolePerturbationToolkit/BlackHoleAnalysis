(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`GSFEqs`",
				{"BlackHoleAnalysis`Utils`",
					"BlackHoleAnalysis`Harmonics`",
					"BlackHoleAnalysis`Coordinates`",
					"BlackHoleAnalysis`Symbols`",
					"BlackHoleAnalysis`Fields`",
					"BlackHoleAnalysis`Labels`",
					"BlackHoleAnalysis`ValidityTests`",
					"BlackHoleAnalysis`AnalyticTools`",
					"BlackHoleAnalysis`AssociationNested`"}];


(*******************************************************************)
(* Public functions *)
(*******************************************************************)
GSFComponent;
GSFComponentExpanded;
GSFComponentRules;
GSFt;
GSFr;
GSFtExpanded;
GSFrExpanded;
GSFtRules;
GSFrRules;
HRet;
HRetRules;

RPComponentRules;
RPtRules;
RPrRules;
RPphiRules;

HRPComponentRules;


Begin["`Private`"];


Options[GSFt]={"Parity"->"Even","Mode"->"Radiative"};
Options[GSFr]=Options[GSFt];
Options[GSFComponent]=Join[Options[GSFt],{"Component"->"t"}];
Options[GSFComponentExpanded]= {"Component"->"t"};
Options[GSFComponentRules]= {"Component"->"t"};
Options[RPComponentRules]={"Component"->"t"};


memDef@
HRet[syms_Association]:=
Module[{rules1,rules2,hret,hret1,th},

	th=ThetaSymbol[syms];

	rules1=Join[mpRules[syms],shRules[syms]];
	rules2=Join[coordRules[syms],constantRules[syms]];
	
	hret=Get[FileNameJoin[{"BlackHoleAnalysis","GSF","HRet"}]];
	hret1=StringReplace[hret,rules1];

	ToExpression@StringReplace[hret1,rules2]/.th->\[Pi]/2
]


def@
HRetRules[syms_Association,opts:OptionsPattern[]]:=
Module[{hRetRules,rules},


	hRetRules=Get[FileNameJoin[{"BlackHoleAnalysis","GSF","HRetRules"}]];
	rules=Join[mpRulesLM[syms],shRules[syms],coordRules[syms],constantRules[syms]];	
	
	ToExpression@StringReplace[hRetRules,rules]
]


def@
GSFComponent[syms_Association,opts:OptionsPattern[]]:=
Module[{gsfComp,rules1,rules2,gsfComp1,th,optionsRules,parity,mode,input,
		labels,getMPFns,symbols,mps,derivsT,derivsR,t,rp,collect,shs,comp},

	optionsRules = {"Component"->Function[x,MemberQ[{"t","r"},x]],
					"Parity"->Function[x,MemberQ[{"Even","Odd"},x]],
					"Mode"->Function[x,MemberQ[{"Radiative","Dipole","Monopole"},x]]};
	TestOptions[optionsRules,{opts}];

	parity=OptionValue[Parity];
	mode=OptionValue[Mode];
	comp=OptionValue[Component];

	th=ThetaSymbol[syms];
	t=TSymbol[syms];
	rp=RpSymbol[syms];

	gsfComp=Get[
			If[comp==="t",
				If[parity==="Even",
					FileNameJoin[{"BlackHoleAnalysis","GSF","GSFtEven_Unexpanded"}],
					FileNameJoin[{"BlackHoleAnalysis","GSF","GSFtOdd_Unexpanded"}]
					],
				If[parity==="Even",
					FileNameJoin[{"BlackHoleAnalysis","GSF","GSFrEven_Unexpanded"}],
					FileNameJoin[{"BlackHoleAnalysis","GSF","GSFrOdd_Unexpanded"}]
					]
				]
			];


	rules1=Join[mpRules[syms],shRules[syms]];
	rules2=Join[coordRules[syms],constantRules[syms]];	
	gsfComp1=StringReplace[gsfComp,rules1];

	labels= MetricPerturbationLables[Parity->parity,Mode->mode];
	getMPFns=AmplitudeFunction/@labels;
	symbols=#[syms,Gauge->Null,ReturnSymbol->True]&/@getMPFns;
	mps=#[t,rp[t]]&/@symbols;
	derivsT=Derivative[1,0][#][t,rp[t]]&/@symbols;
	derivsR=Derivative[0,1][#][t,rp[t]]&/@symbols;
	collect=Join[mps,derivsT,derivsR];
	shs={YSymbol[syms][t],YThetaSymbol[syms][t]};

	input=ToExpression@StringReplace[gsfComp1,rules2]/.th->\[Pi]/2;

	Collect[Total[Coefficient[input,#]#&/@collect],shs,Collect[#,collect,Simplify]&]
]


def@
GSFComponentRules[syms_Association,opts:OptionsPattern[]]:=
Module[{gsfComp,rules,optionsRules,comp},

	optionsRules = {"Component"->Function[x,MemberQ[{"t","r"},x]]};
	TestOptions[optionsRules,{opts}];

	comp=OptionValue[Component];

	gsfComp=Get[
				If[comp==="t",
					FileNameJoin[{"BlackHoleAnalysis","GSF","rSqGSFtRules"}],
					FileNameJoin[{"BlackHoleAnalysis","GSF","rSqGSFrRules"}]
				]
			];

	rules=Join[mpRulesLM[syms],shRules[syms],coordRules[syms],constantRules[syms]];	
	
	ToExpression@StringReplace[gsfComp,rules]
]


def@
GSFComponentExpanded[syms_Association,opts:OptionsPattern[]]:=
Module[{gsfComp,rules,optionsRules,t,rp,comp},

	optionsRules = {"Component"->Function[x,MemberQ[{"t","r"},x]]};
	TestOptions[optionsRules,{opts}];

	comp=OptionValue[Component];

	t=TSymbol[syms];
	rp=RpSymbol[syms];

	gsfComp=Get[
			If[comp==="t",
					FileNameJoin[{"BlackHoleAnalysis","GSF","rSqGSFt_Expanded"}],
					FileNameJoin[{"BlackHoleAnalysis","GSF","rSqGSFr_Expanded"}]
				]
			];

	rules=Join[mpRulesLM[syms],shRules[syms],coordRules[syms],constantRules[syms]];	
	
	ToExpression@StringReplace[gsfComp,rules]/rp[t]^2
]


memDef@
GSFt[syms_Association,opts:OptionsPattern[]]:=GSFComponent[syms,opts,Component->"t"]


memDef@
GSFr[syms_Association,opts:OptionsPattern[]]:=GSFComponent[syms,opts,Component->"r"]


memDef@
GSFtExpanded[syms_Association]:=GSFComponentExpanded[syms,Component->"t"]


memDef@
GSFrExpanded[syms_Association]:=GSFComponentExpanded[syms,Component->"r"]


memDef@
GSFtRules[syms_Association]:=GSFComponentRules[syms,Component->"t"]


memDef@
GSFrRules[syms_Association]:=GSFComponentRules[syms,Component->"r"]


def@
RPComponentRules[syms_Association,opts:OptionsPattern[]]:=
Module[{RPComp,rules,optionsRules,comp},

	optionsRules = {"Component"->Function[x,MemberQ[{"t","r","phi"},x]]};
	TestOptions[optionsRules,{opts}];

	comp=OptionValue[Component];

	RPComp=Get[
				Switch[comp,
				"t",
					FileNameJoin[{"BlackHoleAnalysis","GSF","Flt"}],
				"r",
					FileNameJoin[{"BlackHoleAnalysis","GSF","Flr"}],
				"phi",
					FileNameJoin[{"BlackHoleAnalysis","GSF","Flphi"}]
				]
			];

	rules=Join[coordRulesRP[syms],constantRules[syms]];	
	
	ToExpression@StringReplace[RPComp,rules]
]


memDef@
HRPComponentRules[syms_Association]:=
Module[{RPComp,rules},

	RPComp=Get[FileNameJoin[{"BlackHoleAnalysis","GSF","Hl"}]];

	rules=Join[coordRulesRP[syms],constantRules[syms]];	
	
	AssociationNested[ToExpression@StringReplace[RPComp,rules]]
]


memDef@
RPtRules[syms_Association,opts:OptionsPattern[]]:=RPComponentRules[syms,opts,Component->"t"]


memDef@
RPrRules[syms_Association,opts:OptionsPattern[]]:=RPComponentRules[syms,opts,Component->"r"]


memDef@
RPphiRules[syms_Association,opts:OptionsPattern[]]:=RPComponentRules[syms,opts,Component->"phi"]


def@
coordRulesRP[syms_Association]:=
Module[{rp,t,th},

	rp=ToString[RpSymbol[syms],InputForm];
	t=ToString[TSymbol[syms],InputForm];
	th=ToString[ThetaSymbol[syms],InputForm];
	
	{"t"->t,"rp"->rp}
]


def@
mpRules[syms_Association]:=
Module[{htt,htr,hrr,KK,jt,jr,GG,ht,hr,h2},

	htt=ToString[HttAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	htr=ToString[HtrAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	hrr=ToString[HrrAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	KK=ToString[KAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	jt=ToString[JtAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	jr=ToString[JrAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	GG=ToString[GAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	ht=ToString[HtAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	hr=ToString[HrAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	h2=ToString[H2Amplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	
	{"Subscript[htt, \[ScriptL], \[ScriptM]]"->htt,
	"Subscript[htr, \[ScriptL], \[ScriptM]]"->htr,
	"Subscript[hrr, \[ScriptL], \[ScriptM]]"->hrr,
	"Subscript[\[ScriptCapitalK], \[ScriptL], \[ScriptM]]"->KK,
	"Subscript[jt, \[ScriptL], \[ScriptM]]"->jt,
	"Subscript[jr, \[ScriptL], \[ScriptM]]"->jr,
	"Subscript[\[ScriptCapitalG], \[ScriptL], \[ScriptM]]"->GG,
	"Subscript[ht, \[ScriptL], \[ScriptM]]"->ht,
	"Subscript[hr, \[ScriptL], \[ScriptM]]"->hr,
	"Subscript[h2, \[ScriptL], \[ScriptM]]"->h2}
]


def@
mpRulesLM[syms_Association]:=
Module[{htt,htr,hrr,KK,jt,jr,GG,ht,hr,h2},

	htt=ToString[HttAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	htr=ToString[HtrAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	hrr=ToString[HrrAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	KK=ToString[KAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	jt=ToString[JtAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	jr=ToString[JrAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	GG=ToString[GAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	ht=ToString[HtAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	hr=ToString[HrAmplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	h2=ToString[H2Amplitude[syms,Gauge->Null,ReturnSymbol->True],InputForm];
	
	{"Subscript[htt, \[ScriptL], \[ScriptM]]"->htt<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[htr, \[ScriptL], \[ScriptM]]"->htr<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[hrr, \[ScriptL], \[ScriptM]]"->hrr<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[\[ScriptCapitalK], \[ScriptL], \[ScriptM]]"->KK<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[jt, \[ScriptL], \[ScriptM]]"->jt<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[jr, \[ScriptL], \[ScriptM]]"->jr<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[\[ScriptCapitalG], \[ScriptL], \[ScriptM]]"->GG<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[ht, \[ScriptL], \[ScriptM]]"->ht<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[hr, \[ScriptL], \[ScriptM]]"->hr<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[h2, \[ScriptL], \[ScriptM]]"->h2<>"[\[ScriptL], \[ScriptM]]",
	"Subscript[htt, "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>htt<>"["<>n<>" + \[ScriptL], \[ScriptM]]",
	"Subscript[htr, "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>htr<>"["<>n<>" + \[ScriptL], \[ScriptM]]",
	"Subscript[hrr, "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>hrr<>"["<>n<>" + \[ScriptL], \[ScriptM]]",
	"Subscript[jt, "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>jt<>"["<>n<>" + \[ScriptL], \[ScriptM]]",
	"Subscript[jr, "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>jr<>"["<>n<>" + \[ScriptL], \[ScriptM]]",
	"Subscript[\[ScriptCapitalK], "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>KK<>"["<>n<>" + \[ScriptL], \[ScriptM]]",
	"Subscript[\[ScriptCapitalG], "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>GG<>"["<>n<>" + \[ScriptL], \[ScriptM]]",
	"Subscript[ht, "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>ht<>"["<>n<>" + \[ScriptL], \[ScriptM]]",
	"Subscript[hr, "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>hr<>"["<>n<>" + \[ScriptL], \[ScriptM]]",
	"Subscript[h2, "~~n__~~" + \[ScriptL], \[ScriptM]]"/;MemberQ[(ToString/@Range[-3,3]),n]:>h2<>"["<>n<>" + \[ScriptL], \[ScriptM]]"}
]


def@
coordRules[syms_Association]:=
Module[{rp,t,th},

	rp=ToString[RpSymbol[syms],InputForm];
	t=ToString[TSymbol[syms],InputForm];
	th=ToString[ThetaSymbol[syms],InputForm];
	
	{"t[]"->t,"rp"->rp,"\[Theta][]"->th}
]


def@
constantRules[syms_Association]:=
Module[{En,JJ,l,m,M,mu},

	En=ToString[SpecificEnergySymbol[syms],InputForm];
	JJ=ToString[SpecificAngularMomentumSymbol[syms],InputForm];
	l=ToString[LSymbol[syms],InputForm];
	m=ToString[MSymbol[syms],InputForm];
	M=ToString[BlackHoleMassSymbol[syms],InputForm];
	mu=ToString[ParticleMassSymbol[syms],InputForm];
	
	{"M"->M,"\[ScriptCapitalE]"->En,"\[ScriptCapitalJ]"->JJ,"\[ScriptL]"->l,"\[ScriptM]"->m}
]


def@
shRules[syms_Association]:=
Module[{Y,YTheta,YPhi,YThetaTheta,YThetaPhi,YPhiPhi,
		XTheta,XPhi,XThetaTheta,XThetaPhi,XPhiPhi,t},

	t=TSymbol[syms];
	Y=ToString[YSymbol[syms][t],InputForm];
	YTheta=ToString[YThetaSymbol[syms][t],InputForm];
	YPhi=ToString[YPhiSymbol[syms][t],InputForm];
	YThetaTheta=ToString[YThetaThetaSymbol[syms][t],InputForm];
	YThetaPhi=ToString[YThetaPhiSymbol[syms][t],InputForm];
	YPhiPhi=ToString[YPhiPhiSymbol[syms][t],InputForm];
	XTheta=ToString[XThetaSymbol[syms][t],InputForm];
	XPhi=ToString[XPhiSymbol[syms][t],InputForm];
	XThetaTheta=ToString[XThetaThetaSymbol[syms][t],InputForm];
	XThetaPhi=ToString[XThetaPhiSymbol[syms][t],InputForm];
	XPhiPhi=ToString[XPhiPhiSymbol[syms][t],InputForm];
	
	{"Subscript[\[ScriptY], \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->Y,
	"Subscript[yth, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->YTheta,
	"Subscript[yph, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->YPhi,
	"Subscript[ythth, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->YThetaTheta,
	"Subscript[ythph, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->YThetaPhi,
	"Subscript[yphph, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->YPhiPhi,
	"Subscript[xth, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->XTheta,
	"Subscript[xph, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->XPhi,
	"Subscript[xthth, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->XThetaTheta,
	"Subscript[xthph, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->XThetaPhi,
	"Subscript[xphph, \[ScriptL], \[ScriptM]][\[Theta][], \[Phi][]]"->XPhiPhi,
	"Derivative[1, 0][Subscript[\[ScriptY], \[ScriptL], \[ScriptM]]][\[Theta][], \[Phi][]]"->YTheta}
]


End[];

EndPackage[];
