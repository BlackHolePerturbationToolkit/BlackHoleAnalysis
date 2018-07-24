(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`Sources`",
				{"BlackHoleAnalysis`Utils`",
					"BlackHoleAnalysis`Coordinates`",
					"BlackHoleAnalysis`ValidityTests`",
					"BlackHoleAnalysis`Labels`",
					"BlackHoleAnalysis`Symbols`"}];


(*******************************************************************)
(* Public functions *)
(*******************************************************************)
MetricPerturbationSourceFunction::usage="MetricPerturbationSourceFunction[label] returns the function for returning a metric perturbation source term associated with label.";

QttSource::usage="QttSource[] returns the metric perturbation amplitude source term Qtt in symbolic form.";
QtrSource::usage="QtrSource[] returns the metric perturbation amplitude source term Qtr in symbolic form.";
QrrSource::usage="QrrSource[] returns the metric perturbation amplitude source term Qrr in symbolic form.";
QtSource::usage="QtSource[] returns the metric perturbation amplitude source term Qt in symbolic form.";
QrSource::usage="QrSource[] returns the metric perturbation amplitude source term Qr in symbolic form.";
QFlatSource::usage="QFlatSource[] returns the metric perturbation amplitude source term QFlat in symbolic form.";
QSharpSource::usage="QSharpSource[] returns the metric perturbation amplitude source term QSharp in symbolic form.";
PtSource::usage="PtSource[] returns the metric perturbation amplitude source term Pt in symbolic form.";
PrSource::usage="PrSource[] returns the metric perturbation amplitude source term Pr in symbolic form.";
PSource::usage="PSource[] returns the metric perturbation amplitude source term P in symbolic form.";

MasterEquationSource::usage="MasterEquationSource[] returns the source term of the master equation in symbolic form.";

GSource::usage="GSource[] returns the source G-tilde term of the master equation source term in symbolic form.";
FSource::usage="FSource[] returns the source F-tilde term of the master equation source term in symbolic form.";


Begin["`Private`"];


Options[getMPSourceTerm]=Join[Options[QSharpSymbol],{"SourceExpansion"->"Full","ReturnSymbol"->False}];

Options[QttSource]=DeleteDuplicates@Join[Options[QttSymbol],Options[getMPSourceTerm]];
DocumentationBuilder`OptionDescriptions["QttSource"] = 
Join[DocumentationBuilder`OptionDescriptions["QttSymbol"], 
{
    "SourceExpansion"->"States to what extent source terms should be expanded, (\"Full\", \"Partial\", or \"None\")",
	"ReturnSymbol"->"Boolean stating whether to only return the symbol of the source term (with no arguments)"
}];
Options[QtrSource]=Options[QttSource];
DocumentationBuilder`OptionDescriptions["QtrSource"] = DocumentationBuilder`OptionDescriptions["QttSource"];
Options[QrrSource]=Options[QttSource];
DocumentationBuilder`OptionDescriptions["QrrSource"] = DocumentationBuilder`OptionDescriptions["QttSource"];
Options[QtSource]=DeleteDuplicates@Join[Options[QtSymbol],Options[getMPSourceTerm]];
DocumentationBuilder`OptionDescriptions["QtSource"]=Join[DocumentationBuilder`OptionDescriptions["QtSymbol"], FilterRules[DocumentationBuilder`OptionDescriptions["QttSource"],{"SourceExpansion","ReturnSymbol"}]];
Options[QrSource]=Options[QtSource];
DocumentationBuilder`OptionDescriptions["QrSource"]=DocumentationBuilder`OptionDescriptions["QtSource"];
Options[QSharpSource]=Options[getMPSourceTerm];
DocumentationBuilder`OptionDescriptions["QSharpSource"]=Join[DocumentationBuilder`OptionDescriptions["QSharpSymbol"], FilterRules[DocumentationBuilder`OptionDescriptions["QttSource"],{"SourceExpansion","ReturnSymbol"}]];
Options[QFlatSource]=Options[getMPSourceTerm];
DocumentationBuilder`OptionDescriptions["QFlatSource"]=DocumentationBuilder`OptionDescriptions["QSharpSource"];
Options[PtSource]=Options[QtSource];
DocumentationBuilder`OptionDescriptions["PtSource"]=DocumentationBuilder`OptionDescriptions["QtSource"];
Options[PrSource]=Options[QtSource];
DocumentationBuilder`OptionDescriptions["PrSource"]=DocumentationBuilder`OptionDescriptions["QtSource"];
Options[PSource]=Options[getMPSourceTerm];
DocumentationBuilder`OptionDescriptions["PSource"]=DocumentationBuilder`OptionDescriptions["QSharpSource"];

Options[GSource]=Join[Options[MasterFunctionSymbol],{"SourceExpansion"->"Partial"}];
DocumentationBuilder`OptionDescriptions["GSource"]=
Join[FilterRules[DocumentationBuilder`OptionDescriptions["QttSource"],{"SourceExpansion"}],
DocumentationBuilder`OptionDescriptions["MasterFunctionSymbol"]];
Options[FSource]=Options[GSource];
DocumentationBuilder`OptionDescriptions["FSource"]=DocumentationBuilder`OptionDescriptions["GSource"];
Options[MasterEquationSource] = Join[Options[GSource],{"GFForm"->True}];
DocumentationBuilder`OptionDescriptions["MasterEquationSource"]=
Join[DocumentationBuilder`OptionDescriptions["GSource"],{"GFForm"->"Boolean stating whether source term should be written in terms of G and F coefficents of the Dirac delta distribution and its first derivative"}];


indicesQ[x_]:=MemberQ[{"Up","Down","UpDown","DownUp"},x];
indexQ[x_]:=MemberQ[{"Up","Down"},x];
sourceExpQ[x_]:=MemberQ[{"None","Partial","Full"},x];


sourceTestsIndices={"Capital" -> BooleanQ, "SourceExpansion" -> sourceExpQ, "ReturnSymbol"->BooleanQ, "Indices" -> indicesQ};


zmMax=7;
cpmMax=6;
jtMax=5;
zmVars=Join[{"ZM"},"ZM"<>ToString[#]&/@Range[0,zmMax]];
cpmVars=Join[{"CPM"},"CPM"<>ToString[#]&/@Range[0,cpmMax]];
jtVars=Join[{"JT"},"JT"<>ToString[#]&/@Range[0,jtMax]];
masterVars=Join[{Default},zmVars,cpmVars,jtVars];


def@
QttSource[syms_Association,opts:OptionsPattern[]]:=
Module[{qttUpper,qtt,t,rp,M,En,mu,YBar,sym,cap,inds,se,f},

	TestOptions[sourceTestsIndices,{opts}];

	inds=OptionValue[Indices];
	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];

	sym=QttSymbol[syms,FilterRules[{opts},Options[QttSymbol]]];
	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	mu=ParticleMassSymbol[syms];
	YBar=YSymbol[syms,Conjugate->True];
	f = SchwarzschildF;
	
	qtt[t_]:=
		If[se==="Full",

			qttUpper[tt_]:=8\[Pi] mu En/(f[rp[tt],M]rp[tt]^2) YBar[tt];
	
			Which[inds==="Down",
				qttUpper[t] f[rp[t],M]^2,
				inds==="UpDown"||inds==="DownUp",
				-qttUpper[t] f[rp[t],M],
				inds==="Up",
				qttUpper[t]
			],
			
			QttSymbol[syms,Indices->inds,Capital->False][t]
		];

	getMPSourceTerm[syms,qtt,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
QttSource[opts:OptionsPattern[]]:=QttSource[DefaultSymbols[],opts]


def@
QtrSource[syms_Association,opts:OptionsPattern[]]:=
Module[{qtrUpper,qtr,t,rp,M,En,mu,YBar,sym,cap,inds,se,f},
       	
	TestOptions[sourceTestsIndices,{opts}];

	inds=OptionValue[Indices];
	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];
	sym=QtrSymbol[syms,FilterRules[{opts},Options[QtrSymbol]]];

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	mu=ParticleMassSymbol[syms];
	YBar=YSymbol[syms,Conjugate->True];
	f = SchwarzschildF;
	
	qtr[t_]:=
		If[se==="Full",

			qtrUpper[tt_]:=(8\[Pi] mu)/rp[tt]^2 (rp'[tt] En)/f[rp[tt],M] YBar[tt];

			Which[inds==="Down",
				-qtrUpper[t],
				inds==="UpDown",
				qtrUpper[t] / f[rp[t],M],
				inds==="DownUp",
				-qtrUpper[t] f[rp[t],M],
				inds==="Up",
				qtrUpper[t]
			],
		
			QtrSymbol[syms,Indices->inds,Capital->False][t]
		];

	getMPSourceTerm[syms,qtr,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
QtrSource[opts:OptionsPattern[]]:=QtrSource[DefaultSymbols[],opts]


def@
QrrSource[syms_Association,opts:OptionsPattern[]]:=
Module[{qrrUpper,qrr,t,rp,M,En,mu,YBar,USqVal,sym,cap,inds,se,f},
       	
	TestOptions[sourceTestsIndices,{opts}];

	inds=OptionValue[Indices];
	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];

	sym=QrrSymbol[syms,FilterRules[{opts},Options[QrrSymbol]]];
	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	mu=ParticleMassSymbol[syms];
	YBar=YSymbol[syms,Conjugate->True];
	USqVal=USquared[syms];
	f = SchwarzschildF;
	
	qrr[tt_]:=
		If[se==="Full",

			qrrUpper=8 \[Pi] mu  f[rp[tt],M]/(En rp[tt]^2) (En^2-USqVal)YBar[tt];

			Which[inds==="Down",
				qrrUpper/(f[rp[tt],M]^2),
				inds==="UpDown"||inds==="DownUp",
				qrrUpper/f[rp[tt],M],
				inds==="Up",
				qrrUpper
			],
		
			QrrSymbol[syms,Indices->inds,Capital->False][tt]
		] /. {t->tt};

	getMPSourceTerm[syms,qrr,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
QrrSource[opts:OptionsPattern[]]:=QrrSource[DefaultSymbols[],opts]


def@
QtSource[syms_Association,opts:OptionsPattern[]]:=
Module[{qtUpper,qt,t,rp,M,mu,YphiBar,JJ,la,sym,cap,inds,se,f},
       	
	TestOptions[sourceTestsIndices,{opts}];

	inds=OptionValue[Indices];
	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];

	sym=QtSymbol[syms,FilterRules[{opts},Options[QtSymbol]]];
	t=TSymbol[syms];
	rp=RpSymbol[syms];	
	M=BlackHoleMassSymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	mu=ParticleMassSymbol[syms];
	la=LambdaOfL[syms];
	YphiBar=YPhiSymbol[syms,Conjugate->True];
	f = SchwarzschildF;
		
	qt[t_]:=
		If[se==="Full",

			qtUpper[tt_]:=(8 \[Pi] mu)/(la+1) JJ/rp[tt]^2 YphiBar[tt];

			Which[inds==="Down",
				-f[rp[t],M] qtUpper[t],
				inds==="Up",
				qtUpper[t]
			],
		
			QtSymbol[syms,Indices->inds,Capital->False][t]
		];

	getMPSourceTerm[syms,qt,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
QtSource[opts:OptionsPattern[]]:=QtSource[DefaultSymbols[],opts]


def@
QrSource[syms_Association,opts:OptionsPattern[]]:=
Module[{qrUpper,qr,t,rp,M,mu,YphiBar,JJ,la,sym,cap,inds,se,f},
       	
	TestOptions[sourceTestsIndices,{opts}];

	inds=OptionValue[Indices];
	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];

	sym=QrSymbol[syms,FilterRules[{opts},Options[QrSymbol]]];
	t=TSymbol[syms];
	rp=RpSymbol[syms];	
	M=BlackHoleMassSymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	mu=ParticleMassSymbol[syms];
	la=LambdaOfL[syms];
	YphiBar=YPhiSymbol[syms,Conjugate->True];
	f = SchwarzschildF;
			
	qr[t_]:=
		If[se==="Full",

			qrUpper[tt_]:=(8 \[Pi] mu)/(la+1) JJ/rp[tt]^2 rp'[tt]  YphiBar[tt];

			Which[inds==="Down",
				qrUpper[t]/f[rp[t],M],
				inds==="Up",
				qrUpper[t]
			],
		
			QrSymbol[syms,Indices->inds,Capital->False][t]
		];

	getMPSourceTerm[syms,qr,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
QrSource[opts:OptionsPattern[]]:=QrSource[DefaultSymbols[],opts]


def@
QSharpSource[syms_Association,opts:OptionsPattern[]]:=
Module[{qSharp,t,rp,M,mu,YphiphiBar,En,JJ,la,sym,cap,se,f},
       	
	TestOptions[sourceTestsIndices,{opts}];

	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];

	sym=QSharpSymbol[syms,FilterRules[{opts},Options[QSharpSymbol]]];
	t=TSymbol[syms];
	rp=RpSymbol[syms];	
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	mu=ParticleMassSymbol[syms];
	la=LambdaOfL[syms];
	YphiphiBar=YPhiPhiSymbol[syms,Conjugate->True];
	f = SchwarzschildF;
	
	qSharp[t_]:=
		If[se==="Full",
			
			(8\[Pi] mu)/(la(la+1)) JJ^2/En f[rp[t],M]/rp[t]^2 YphiphiBar[t],
		
			QSharpSymbol[syms,Capital->False][t]
		];

	getMPSourceTerm[syms,qSharp,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
QSharpSource[opts:OptionsPattern[]]:=QSharpSource[DefaultSymbols[],opts]


def@
QFlatSource[syms_Association,opts:OptionsPattern[]]:=
Module[{qFlat,t,rp,M,mu,YBar,En,JJ,sym,cap,se,f},
       	
	TestOptions[sourceTestsIndices,{opts}];

	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];

	sym=QFlatSymbol[syms,FilterRules[{opts},Options[QFlatSymbol]]];
	t=TSymbol[syms];
	rp=RpSymbol[syms];	
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	mu=ParticleMassSymbol[syms];
	YBar=YSymbol[syms,Conjugate->True];
	f = SchwarzschildF;
	
	qFlat[t_]:=
		If[se==="Full",
			
			8\[Pi] mu JJ^2/En f[rp[t],M]/rp[t]^4 YBar[t],
		
			QFlatSymbol[syms,Capital->False][t]
		];

	getMPSourceTerm[syms,qFlat,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
QFlatSource[opts:OptionsPattern[]]:=QFlatSource[DefaultSymbols[],opts]


def@
PtSource[syms_Association,opts:OptionsPattern[]]:=
Module[{ptUpper,pt,t,rp,M,mu,XphiBar,JJ,la,sym,cap,inds,se,f},
       	
	TestOptions[sourceTestsIndices,{opts}];

	inds=OptionValue[Indices];
	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];

	sym=PtSymbol[syms,FilterRules[{opts},Options[PtSymbol]]];
	t=TSymbol[syms];
	rp=RpSymbol[syms];	
	M=BlackHoleMassSymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	mu=ParticleMassSymbol[syms];
	la=LambdaOfL[syms];
	XphiBar=XPhiSymbol[syms,Conjugate->True];
	f = SchwarzschildF;
	
	pt[t_]:=
		If[se==="Full",

			ptUpper[tt_]:=(8 \[Pi] mu)/(la+1) JJ/rp[tt]^2 XphiBar[tt];

			Which[inds==="Down",
				-f[rp[t],M] ptUpper[t],
				inds==="Up",
				ptUpper[t]
			],
		
			PtSymbol[syms,Indices->inds,Capital->False][t]
		];

	getMPSourceTerm[syms,pt,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
PtSource[opts:OptionsPattern[]]:=PtSource[DefaultSymbols[],opts]


def@
PrSource[syms_Association,opts:OptionsPattern[]]:=
Module[{prUpper,pr,t,rp,M,mu,XphiBar,JJ,la,sym,cap,inds,se,f},

	TestOptions[sourceTestsIndices,{opts}];

	inds=OptionValue[Indices];
	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];

	sym=PrSymbol[syms,FilterRules[{opts},Options[PrSymbol]]];
	t=TSymbol[syms];
	rp=RpSymbol[syms];	
	M=BlackHoleMassSymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	mu=ParticleMassSymbol[syms];
	la=LambdaOfL[syms];
	XphiBar=XPhiSymbol[syms,Conjugate->True];
	f = SchwarzschildF;
	
	pr[t_]:=
		If[se==="Full",

			prUpper[tt_]:=(8 \[Pi] mu)/(la+1) JJ/rp[tt]^2 D[rp[tt],tt] XphiBar[tt];

			Which[inds==="Down",
				prUpper[t]/f[rp[t],M],
				inds==="Up",
				prUpper[t]
			],
		
			PrSymbol[syms,Indices->inds,Capital->False][t]
		];

	getMPSourceTerm[syms,pr,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
PrSource[opts:OptionsPattern[]]:=PrSource[DefaultSymbols[],opts]


def@
PSource[syms_Association,opts:OptionsPattern[]]:=
Module[{p,t,rp,M,mu,XphiphiBar,En,JJ,la,sym,cap,se,f},
       	
	TestOptions[sourceTestsIndices,{opts}];

	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];

	sym=PSymbol[syms,FilterRules[{opts},Options[PSymbol]]];
	t=TSymbol[syms];
	rp=RpSymbol[syms];	
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	mu=ParticleMassSymbol[syms];
	la=LambdaOfL[syms];
	XphiphiBar=XPhiPhiSymbol[syms,Conjugate->True];
	f = SchwarzschildF;
	
	p[t_]:=
		If[se==="Full",
			
			(4\[Pi] mu)/(la(la+1)) JJ^2/En f[rp[t],M]/rp[t]^2 XphiphiBar[t],
		
			PSymbol[syms,Capital->False][t]
		];

	getMPSourceTerm[syms,p,sym,FilterRules[{opts},Options[getMPSourceTerm]]]
]
reDef@
PSource[opts:OptionsPattern[]]:=PSource[DefaultSymbols[],opts]


def@
getMPSourceTerm[syms_Association,sourceLower_,sourceSym_,opts:OptionsPattern[]]:=
Module[{t,r,rp,DD,cap,se,returnValue,sym},
       	
	TestOptions[sourceTestsIndices,{opts}];

	se=OptionValue[SourceExpansion];
	cap=OptionValue[Capital];
	sym=OptionValue[ReturnSymbol];

	If[sym,Return@sourceSym];

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];

	Which[se==="Full"||se==="Partial",

		If[cap,
			DD=DiracDeltaSymbol[syms];
			
			returnValue[t_,r_]:=sourceLower[t] DD[r-rp[t]],
			returnValue[t_]:=sourceLower[t]
		],

		se==="None",

		If[cap,
			DD=DiracDeltaSymbol[syms];
			
			returnValue[t_,r_]:=sourceSym[t,r],
			returnValue[t_]:=sourceSym[t]
		]
	];
		
	If[cap, returnValue[t,r], returnValue[t]]
]


def@
MasterEquationSource[syms_Association,opts:OptionsPattern[]] :=
Module[{t, r, rp, DD, optionsRules,gTil,var,EE,JJ,
		fTil,la,M,se,parity,gf,parOpt,varOpt,
        Qtt, Qrr, Qr, QSharp, QFlat,Qtr,Qt,
        Pt, Pr, P, f},

	optionsRules = {"Parity" -> Function[x,MemberQ[{Default,"Even","Odd"},x]],
					"Variable" -> Function[x,MemberQ[masterVars,x]],
					"SourceExpansion" -> Function[x, x==="None" || x==="Partial" || x==="Full"],
					"GFForm"->BooleanQ};

	TestOptions[optionsRules,{opts}];
	se=OptionValue[SourceExpansion];
	gf=OptionValue[GFForm];
	varOpt=OptionValue["Variable"];
	parOpt=OptionValue["Parity"];

	{parity,var}=ParityAndVariable[parOpt,varOpt];

	t=TSymbol[syms];
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	la=LambdaOfL[syms];
	DD=DiracDeltaSymbol[syms];
	f = SchwarzschildF;
	
	If[gf,

		gTil=GSource[syms,FilterRules[{opts},Options[GSource]]];
		fTil=FSource[syms,FilterRules[{opts},Options[FSource]]];

		gTil DD[r-rp[t]] + fTil DD'[r-rp[t]],

		Switch[var,
			"ZM"|"ZM0",
			
			Qtt[tt_,rr_] := QttSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qrr[tt_,rr_] := QrrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qr[tt_,rr_] := QrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			QSharp[tt_,rr_] := QSharpSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};
			QFlat[tt_,rr_] := QFlatSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};

			1/((la+1)CapitalLambda[r,M,la]) (r^2 f[r,M](f[r,M]^2 D[Qtt[t,r]/f[r,M]^2,r]-D[f[r,M]^2 Qrr[t,r],r])+
				r(CapitalLambda[r,M,la]-f[r,M])f[r,M]^2 Qrr[t,r]+
				r f[r,M]^2 QFlat[t,r]-f[r,M]^2/(r CapitalLambda[r,M,la]) (la(la-1)r^2+(4la-9)M r+15M^2) Qtt[t,r]/f[r,M]^2)+
				2 f[r,M]/CapitalLambda[r,M,la] f[r,M]Qr[t,r]-f[r,M]/r QSharp[t,r],

			"CPM"|"CPM0",

			Pr[tt_,rr_] := PrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Pt[tt_,rr_] := PtSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};

			(r f[r,M])/la (1/f[r,M] D[f[r,M]Pr[t,r],t]-f[r,M]D[Pt[t,r]/f[r,M],r]-(2M)/r^2 Pt[t,r]/f[r,M]),

			"JT"|"JT0",
			
			Qtt[tt_,rr_] := QttSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qrr[tt_,rr_] := QrrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qr[tt_,rr_] := QrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			QSharp[tt_,rr_] := QSharpSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};
			QFlat[tt_,rr_] := QFlatSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};

			(2 f[r,M](r-3M+r la)f[rp[t],M])/r Qr[t,r]
			+(2M+r( la-1))f[rp[t],M]^2 Qrr[t,r]
			+f[r,M]^2/f[rp[t],M]^2 (2M+r( 1-la))Qtt[t,r]
			+r f[r,M]^2 QFlat[t,r]
			- f[r,M]/r^3 (-18 M^2+6M r+r^2 la(la+1))QSharp[t,r]
			-r^2 f[r,M] f[rp[t],M]^2 D[Qrr[t,r],r]
			+(r^2 f[r,M]^3)/f[rp[t],M]^2 D[Qtt[t,r],r]
			+(3M f[r,M]^2)/r D[QSharp[t,r],r]
			,

			"CPM1",

			Pr[tt_,rr_] := PrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			P[tt_,rr_] := PSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};

			2(f[r,M]/r (f[r,M] (D[P[t,r],r]-Pr[t,r]) - 2/r (1-(3M)/r)P[t,r])),

			"CPM2",

			Pr[tt_,rr_] := PrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			P[tt_,rr_] := PSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};

			
			2((2*M*(2*M - r)*Pr[t, r]*Derivative[1][rp][t])/(r^2*rp[t]^2) + (P[t, r]*(120*JJ^2*M^3*r^4*(JJ^2 + r^2)^2 - 120*JJ^2*M^2*r^4*(JJ^2 + r^2)^2*rp[t] + 12*M*(3*JJ^2 + 4*M^2)*r^4*(JJ^2 + r^2)^2*rp[t]^2 + 
    3*(-JJ^2 + 4*(-3 + EE^2)*M^2)*r^4*(JJ^2 + r^2)^2*rp[t]^3 - 2*(-3 + 2*EE^2)*M*r^4*(JJ^2 + r^2)^2*rp[t]^4 + 
    2*EE^2*(M^2*(6*JJ^4 + 28*JJ^2*r^2 + 6*r^4) + r^2*(JJ^4*(1 + la) + JJ^2*(5 + 2*la)*r^2 + la*r^4) - 2*M*r*(JJ^4*(3 + la) + JJ^2*(13 + 2*la)*r^2 + (2 + la)*r^4))*rp[t]^7)*Derivative[1][rp][t])/
  ((2*M - r)*r^2*(JJ^2 + r^2)^3*rp[t]^7) + (2*EE^2*(JJ^2*r - r^3 + M*(-3*JJ^2 + r^2))*Derivative[1][rp][t]*Derivative[0, 1][P][t, r])/(r*(JJ^2 + r^2)^2) + 
 (EE^2*(-2*M + r)*Derivative[1][rp][t]*Derivative[0, 2][P][t, r])/(JJ^2 + r^2) + 
 (2*(2*M - r)*(-3*M + r - (r^6*(2*M - rp[t])*(10*JJ^2*M^2 - 7*JJ^2*M*rp[t] + (JJ^2 + 6*M^2)*rp[t]^2 + (-3 + 2*EE^2)*M*rp[t]^3))/((-2*M + r)^2*(JJ^2 + r^2)*rp[t]^6))*Derivative[1, 0][P][t, r])/r^4 - 
 ((2*M - r)*(2*M - rp[t])*Derivative[1, 0][Pr][t, r])/(r^2*rp[t]) + ((-2*M + r)^2*Derivative[1, 1][P][t, r])/r^3 + (EE^2*r^2*Derivative[1][rp][t]*Derivative[2, 0][P][t, r])/((2*M - r)*(JJ^2 + r^2))),

			"ZM1",
			
			Qtt[tt_,rr_] := QttSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qtr[tt_,rr_] := QtrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qrr[tt_,rr_] := QrrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qt[tt_,rr_] := QtSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qr[tt_,rr_] := QrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			QSharp[tt_,rr_] := QSharpSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};
			QFlat[tt_,rr_] := QFlatSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};

			-((4 M (-2 M+r)^2 rp[t] rp'[t])/((3 M+la r) (2 M-rp[t])^3))Qtt[t,r]+(2 la (-2 M+r)^2)/(3 M+la r)^2 Qtr[t,r]-(4 M r^2 (2 M-rp[t]) rp'[t])/((3 M+la r) rp[t]^3) Qrr[t,r]
			+(2 (3+2 la) M (-2 M+r)^2 rp[t])/(r^2 (3 M+la r)^2 (2 M-rp[t])) Qt[t,r]+(2 M (-2 M+r) rp'[t])/((3 M+la r) rp[t]^2) Qr[t,r]+(2 M-r)^3/(r^2 (3 M+la r) (1-(2 M)/rp[t])) D[Qt[t,r],r]
			+((2 M-r) (2 M-rp[t]))/((3 M+la r) rp[t]) D[Qr[t,r],t]-(-2 M+r)^2/((3 M+la r) (1-(2 M)/rp[t])^2) D[Qtt[t,r],t]+(r-(2 M r)/rp[t])^2/(3 M+la r) D[Qrr[t,r],t]+(2 M-r)/r^2 D[QSharp[t,r],t],

			"ZM2",

			Qtt[tt_,rr_] := QttSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qrr[tt_,rr_] := QrrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qt[tt_,rr_] := QtSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			Qr[tt_,rr_] := QrSource[syms,Capital->True,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
			QSharp[tt_,rr_] := QSharpSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};
			QFlat[tt_,rr_] := QFlatSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};

			(2 (2 M-r) (63 M^3+2 (-9+23 la) M^2 r+la (-17+7 la) M r^2-3 la^2 r^3) (2 M-rp[t]) Qr[t,r])/(r^4 (3 M+la r)^2 rp[t])
			+(2 (2 M-r) (M+la r) (-2 M+rp[t])^2 Qrr[t,r])/(r^2 (3 M+la r) rp[t]^2)
			-(2 M (-2 M+r)^2 (6 M^2+4 la M r-la r^2) rp[t]^2 Qtt[t,r])/(r^4 (3 M+la r)^2 (-2 M+rp[t])^2)
			-((2 M-r) (33 M^3+3 (-3+7 la) M^2 r+2 (-4+la) la M r^2-la^2 r^3) QFlat[t,r])/(r^3 (3 M+la r)^2)
			+1/(r^6 (3 M+la r)^2 (JJ^2+r^2)^3) (2 M-r) (324 M^4 (JJ^2+r^2)^3+9 M^3 r (JJ^6 (-20+33 la)+3 JJ^4 (-20+33 la) r^2+JJ^2 (-60-8 EE^2+99 la) r^4+(-20+8 EE^2+33 la) r^6)+la^2 r^4 (-3 JJ^6 (-1+la)+JJ^4 (9-9 la+2 EE^2 (1+la)) r^2+JJ^2 (9-9 la+2 EE^2 (5+2 la)) r^4+(3+(-3+2 EE^2) la) r^6)+la M r^3 (JJ^6 (15-44 la+6 la^2)+JJ^4 (45+4 (-33+EE^2) la+18 la^2) r^2+3 JJ^2 (15+12 EE^2-44 la+6 la^2) r^4+(15+12 EE^2 (-1+la)-44 la+6 la^2) r^6)+M^2 r^2 (JJ^6 (18-159 la+85 la^2)+3 JJ^4 (18+(-159+2 EE^2) la+85 la^2) r^2+3 JJ^2 (18-159 la+85 la^2-6 EE^2 (-3+2 la)) r^4+(18-159 la+85 la^2+18 EE^2 (-1+3 la)) r^6)) QSharp[t,r]
			-(2 M (2 M-r) (45 M^3+(-15+31 la) M^2 r+la (-13+4 la) M r^2-2 la^2 r^3) Qt[t,r] rp'[t])/(r^3 (3 M+la r)^2 (-2 M+rp[t])^2)
			+(M (2 M-r) (9 M^2+(3+7 la) M r-la r^2) (2 M-rp[t]) D[Qr[t,r],r])/(r^3 (3 M+la r)^2 rp[t])
			-(M (2 M-r) (-2 M+rp[t])^2 D[Qrr[t,r],r])/(r (3 M+la r) rp[t]^2)+(M (2 M-r)^3 D[Qtt[t,r],r])/(r^3 (3 M+la r) (1-(2 M)/rp[t])^2)
			-1/(r^5 (3 M+la r) (JJ^2+r^2)^2) (2 M-r) (60 M^3 (JJ^2+r^2)^2+la r^3 (3 JJ^4+2 (3+EE^2) JJ^2 r^2+(3-2 EE^2) r^4)+2 M r^2 (3 JJ^4 (1-3 la)+JJ^2 (6-EE^2 (-3+la)-18 la) r^2+3 (1+EE^2 (-1+la)-3 la) r^4)+6 M^2 r (JJ^4 (-7+4 la)-JJ^2 (14+EE^2-8 la) r^2+(-7+3 EE^2+4 la) r^4)) D[QSharp[t,r],r]
			+((-2 M+r)^2 (-JJ^2 r+(-1+EE^2) r^3+2 M (JJ^2+r^2)) D[QSharp[t,r],r,r])/(r^4 (JJ^2+r^2))
			-((2 M-r) (45 M^3+(-15+31 la) M^2 r+la (-13+4 la) M r^2-2 la^2 r^3) rp[t] D[Qt[t,r],t])/(r^3 (3 M+la r)^2 (2 M-rp[t]))
			-(EE^2 r D[QSharp[t,r],t,t])/(JJ^2+r^2),

			"ZM3"|"ZM4"|"ZM5"|"ZM6"|"ZM7"|"JT1"|"JT2"|"JT3"|"JT4"|"JT5"|"CPM3"|"CPM4"|"CPM5"|"CPM6",
	
			Print[var, " variable only available in GFForm"];
			Aborting[];
		]
	]
]
reDef@
MasterEquationSource[opts:OptionsPattern[]]:=MasterEquationSource[DefaultSymbols[],opts]


def@
GSource[syms_Association,opts:OptionsPattern[]] := 
Module[{t,parity,var,parOpt,varOpt,
		Gtt,Grr,Gr,GFlat,GSharp,Gtr,
		Gtt1,Grr1,GSharp1,G1,G2,G3,
		Gtt2,Grr2,GSharp2,GSharp3,
		qrr,qtt,qtr,qt,qr,qFlat,qSharp,
		Gt,Gt2,Gr1,Gr2,pt,pr,p,
		G,optionsRules,se},

	optionsRules = {"Parity" -> Function[x,MemberQ[{Default,"Even","Odd"},x]],
					"Variable" -> Function[x,MemberQ[masterVars,x]],
					"SourceExpansion" -> Function[x, x==="Partial" || x==="Full"]};

	TestOptions[optionsRules,{opts}];
	se=OptionValue[SourceExpansion];
	varOpt=OptionValue["Variable"];
	parOpt=OptionValue["Parity"];

	{parity,var}=ParityAndVariable[parOpt,varOpt];
	
	t = TSymbol[syms];

	Switch[var,
		"ZM"|"ZM0",
			
		qtt = QttSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qrr = QrrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qr = QrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qSharp = QSharpSource[syms,Capital->False,SourceExpansion->se];
		qFlat = QFlatSource[syms,Capital->False,SourceExpansion->se];

		Gtt=getGttEven[syms];
		Grr=getGrrEven[syms];
		Gr=getGrEven[syms];
		GSharp=getGSharpEven[syms];
		GFlat=getGFlatEven[syms];

		Gtt qtt + Grr qrr + Gr qr + GFlat qFlat + GSharp qSharp,

		"CPM"|"CPM0",

		pr = PrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		pt = PtSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
	
		Gt=getGtOdd[syms];
		Gr1=getGr1Odd[syms];
		Gr2=getGr2Odd[syms];

		Gt pt + Gr1 pr + Gr2 D[pr,t],

		"JT"|"JT0",
			
		qtt = QttSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qrr = QrrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qr = QrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qSharp = QSharpSource[syms,Capital->False,SourceExpansion->se];
		qFlat = QFlatSource[syms,Capital->False,SourceExpansion->se];

		Gtt=getGttEvenRW[syms];
		Grr=getGrrEvenRW[syms];
		Gr=getGrEvenRW[syms];
		GSharp=getGSharpEvenRW[syms];
		GFlat=getGFlatEvenRW[syms];

		Gtt qtt + Grr qrr + Gr qr + GFlat qFlat + GSharp qSharp,
	
		"CPM1",
		
		pr = PrSource[syms,Capital->False,Indices->"Down",SourceExpansion->se];
		p = PSource[syms,Capital->False,SourceExpansion->se];

		Gr=getGrRW[syms];
		G=getGRW[syms];

		2(Gr pr + G p),

		"CPM2",

		p = PSource[syms,Capital->False,SourceExpansion->se];
		pr = PrSource[syms,Capital->False,SourceExpansion->se,Indices->"Up"];
		
		Gr1=getGr1RWDot[syms];
		Gr2=getGr2RWDot[syms];
		G1=getG1RWDot[syms];
		G2=getG2RWDot[syms];
		G3=getG3RWDot[syms];

		2(Gr1 pr + Gr2 D[pr,t] + G1 p + G2 D[p,t] + G3 D[p,t,t]),

		"CPM3",

		gSourceCPM3[syms],

		"CPM4",

		gSourceCPM4[syms],

		"CPM5",

		gSourceCPM5[syms],

		"CPM6",

		gSourceCPM6[syms],

		"ZM1",

		qtt = QttSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qtr = QtrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qrr = QrrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qt = QtSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qr = QrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qSharp= QSharpSource[syms,Capital->False,SourceExpansion->se];

		Gtt1    = getGtt1EvenZ[syms];
		Gtr     = getGtrEvenZ[syms];
		Grr1    = getGrr1EvenZ[syms];
		Gt      = getGtEvenZ[syms];
		Gr1     = getGr1EvenZ[syms];
		GSharp1 = getGSharp1EvenZ[syms];

		Gtt2    = getGtt2EvenZ[syms];
		Grr2    = getGrr2EvenZ[syms];
		Gr2     = getGr2EvenZ[syms];
		GSharp2 = getGSharp2EvenZ[syms];

		qtt Gtt1 + qtr Gtr + qrr Grr1 + qt Gt + qr Gr1 + qSharp GSharp1
		+ D[qtt,t] Gtt2 + D[qrr,t] Grr2 + D[qr,t] Gr2 + D[qSharp,t] GSharp2,

		"ZM2",

		qtt = QttSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qrr = QrrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qt = QtSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qr = QrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qFlat= QFlatSource[syms,Capital->False,SourceExpansion->se];
		qSharp= QSharpSource[syms,Capital->False,SourceExpansion->se];

		Gtt1    = getGtt1EvenZDot[syms];
		Grr     = getGrr1EvenZDot[syms];
		Gt      = getGt1EvenZDot[syms];
		Gr      = getGr1EvenZDot[syms];
		GFlat   = getGFlat1EvenZDot[syms];
		GSharp1 = getGSharp1EvenZDot[syms];

		Gt2     = getGt2EvenZDot[syms];
		GSharp2 = getGSharp2EvenZDot[syms];

		GSharp3 = getGSharp3EvenZDot[syms];

		qtt Gtt1 + qrr Grr + qt Gt + qr Gr + qFlat GFlat + qSharp GSharp1
		+ D[qt,t] Gt2 + D[qSharp,t] GSharp2 + D[qSharp,t,t] GSharp3,

		"ZM3",

		gSourceZM3[syms],

		"ZM4",

		gSourceZM4[syms],

		"ZM5",

		gSourceZM5[syms],

		"ZM6",
		
		gSourceZM6[syms],

		"ZM7",

		gSourceZM7[syms],

		"JT1",

		gSourceJT1[syms],

		"JT2",

		gSourceJT2[syms],

		"JT3",

		gSourceJT3[syms],

		"JT4",

		gSourceJT4[syms],

		"JT5",

		gSourceJT5[syms]
	]
]
reDef@
GSource[opts:OptionsPattern[]]:=GSource[DefaultSymbols[],opts]


def@
FSource[syms_Association,opts:OptionsPattern[]] := 
Module[{Ftt,Frr,FSharp,parity,var,parOpt,varOpt,
		qrr,qtt,qSharp,qt,qr,FSharp1,FSharp2,
		Ft,Fr,F,t,F1,F2,
		pt,pr,p,
		optionsRules,se},

	optionsRules = {"Parity" -> Function[x,MemberQ[{Default,"Even","Odd"},x]],
					"Variable" -> Function[x,MemberQ[masterVars,x]],
					"SourceExpansion" -> Function[x, x==="Partial" || x==="Full"]};

	TestOptions[optionsRules,{opts}];
	se=OptionValue[SourceExpansion];
	varOpt=OptionValue["Variable"];
	parOpt=OptionValue["Parity"];
	t=TSymbol[syms];

	{parity,var}=ParityAndVariable[parOpt,varOpt];

	Switch[var,
		"ZM"|"ZM0",
			
		qtt = QttSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qrr = QrrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];

		Ftt=getFttEven[syms];
		Frr=getFrrEven[syms];
		
		Ftt qtt + Frr qrr,

		"CPM"|"CPM0",

		pr = PrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		pt = PtSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		
		Ft=getFtOdd[syms];
		Fr=getFrOdd[syms];

		Ft pt + Fr pr,

		"JT"|"JT0",
			
		qtt = QttSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qrr = QrrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qSharp= QSharpSource[syms,Capital->False,SourceExpansion->se];

		Ftt=getFttEvenRW[syms];
		Frr=getFrrEvenRW[syms];
		FSharp=getFSharpEvenRW[syms];
		
		Ftt qtt + Frr qrr+FSharp qSharp,

		"CPM1",

		p = PSource[syms,Capital->False,SourceExpansion->se];
		F = getFRW[syms];

		2 F p,

		"CPM2",

		p = PSource[syms,Capital->False,SourceExpansion->se];
		pr = PrSource[syms,Capital->False,SourceExpansion->se,Indices->"Up"];
		
		Fr=getFr1RWDot[syms];
		F1=getF1RWDot[syms];
		F2=getF2RWDot[syms];

		2(Fr pr + F1 p + F2 D[p,t]),

		"CPM3",

		fSourceCPM3[syms],

		"CPM4",

		fSourceCPM4[syms],

		"CPM5",

		fSourceCPM5[syms],

		"CPM6",

		fSourceCPM6[syms],

		"ZM1",
		qtt = QttSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qrr = QrrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qt = QtSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qr = QrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qSharp= QSharpSource[syms,Capital->False,SourceExpansion->se];

		Ftt=getFttEvenZ[syms];
		Frr=getFrrEvenZ[syms];
		Ft=getFtEvenZ[syms];
		Fr=getFrEvenZ[syms];
		FSharp=getFSharpEvenZ[syms];

		qtt Ftt + qrr Frr + qt Ft + qr Fr + qSharp FSharp,

		"ZM2",

		qtt = QttSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qrr = QrrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qt  = QtSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qr  = QrSource[syms,Capital->False,Indices->"Up",SourceExpansion->se];
		qSharp= QSharpSource[syms,Capital->False,SourceExpansion->se];

		Ftt     = getFtt1EvenZDot[syms];
		Frr     = getFrr1EvenZDot[syms];
		Ft      = getFt1EvenZDot[syms];
		Fr      = getFr1EvenZDot[syms];
		FSharp1 = getFSharp1EvenZDot[syms];
		FSharp2 = getFSharp2EvenZDot[syms];

		qtt Ftt + qrr Frr + qt Ft + qr Fr + qSharp FSharp1 + D[qSharp,t] FSharp2,

		"ZM3",

		fSourceZM3[syms],

		"ZM4",

		fSourceZM4[syms],

		"ZM5",

		fSourceZM5[syms],

		"ZM6",

		fSourceZM6[syms],

		"ZM7",

		fSourceZM7[syms],

		"JT1",

		fSourceJT1[syms],

		"JT2",

		fSourceJT2[syms],
	
		"JT3",

		fSourceJT3[syms],

		"JT4",

		fSourceJT4[syms],

		"JT5",

		fSourceJT5[syms]
	]
]
reDef@
FSource[opts:OptionsPattern[]]:=FSource[DefaultSymbols[],opts]


def@
getGttEven[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	-(f[rp[t],M]^2/((la+1)rp[t]CapitalLambda[rp[t],M,la]^2))(la(la+1)rp[t]^2+6 la M rp[t]+15M^2)
];


def@
getGrrEven[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	1/((la+1)rp[t]CapitalLambda[rp[t],M,la]^2) ((la+1)(la rp[t]+6 M)rp[t]+3M^2)
];


def@
getGrEven[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	(2 f[rp[t],M])/CapitalLambda[rp[t],M,la]
];


def@
getGSharpEven[syms_Association]:=
Module[{t,rp,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	-(f[rp[t],M]/rp[t])
];


def@
getGFlatEven[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	(rp[t] f[rp[t],M]^2)/((la+1)CapitalLambda[rp[t],M,la])
];


def@
getFttEven[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	(rp[t]^2 f[rp[t],M]^3)/((la+1)CapitalLambda[rp[t],M,la])
];


def@
getFrrEven[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	-((rp[t]^2 f[rp[t],M])/((la+1)CapitalLambda[rp[t],M,la]))
];


def@
getGtOdd[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	-(f[rp[t],M]/la)
];


def@
getGr1Odd[syms_Association]:=
Module[{t,rp,la},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	la=LambdaOfL[syms];

	rp'[t]/la
];


def@
getGr2Odd[syms_Association]:=
Module[{t,rp,la},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	la=LambdaOfL[syms];
	
	rp[t]/la
];


def@
getFtOdd[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	(rp[t]f[rp[t],M]^2)/la
]


def@
getFrOdd[syms_Association]:=
Module[{t,rp,la},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	la=LambdaOfL[syms];
	
	-((rp[t]rp'[t])/la)
]


def@
getGttEvenRW[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	-(la+1)rp[t]f[rp[t],M]^2
];


def@
getGrrEvenRW[syms_Association]:=
Module[{t,rp,la},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	la=LambdaOfL[syms];
	
	(la+1)rp[t]
];


def@
getGrEvenRW[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	(2 f[rp[t],M])/rp[t] (-3M+(1+la)rp[t])
];


def@
getGSharpEvenRW[syms_Association]:=
Module[{t,rp,la,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	f = SchwarzschildF;
	
	(-(f[rp[t],M]/rp[t]^2)(3M +la(la+1)rp[t]))
];


def@
getGFlatEvenRW[syms_Association]:=
Module[{t,rp,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	rp[t]f[rp[t],M]^2
];


def@
getFttEvenRW[syms_Association]:=
Module[{t,rp,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	rp[t]^2 f[rp[t],M]^3
];


def@
getFrrEvenRW[syms_Association]:=
Module[{t,rp,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	-rp[t]^2f[rp[t],M]
];


def@
getFSharpEvenRW[syms_Association]:=
Module[{t,rp,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	(3M)/rp[t] f[rp[t],M]^2
];


def@
getGrRW[syms_Association]:=
Module[{t,rp,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	-(f[rp[t],M]^2/rp[t])
]


(* This is the coefficient of p[t] in the GSource for the RW source term *)
def@
getGRW[syms_Association]:=
Module[{t,rp,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	-(f[rp[t],M]/rp[t]^2)
]


(* This is the coefficient of p[t] in the FSource for the RW source term *)
def@
getFRW[syms_Association]:=
Module[{t,rp,M,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	f[rp[t],M]^2/rp[t]
]


def@
getGr1RWDot[syms_Association]:=
Module[{t,rp,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	((-4 M+rp[t]) Derivative[1][rp][t])/rp[t]^3
]


def@
getGr2RWDot[syms_Association]:=
Module[{t,rp,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	(2 M - rp[t])/rp[t]^2
]


def@
getG1RWDot[syms_Association]:=
Module[{t,rp,M,EE,J,la},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	la=LambdaOfL[syms];

	((16*J^6*M^3 - 26*J^6*M^2*rp[t] + (13*J^6*M + 64*J^4*M^3)*rp[t]^2 - 
   2*J^4*(J^2 + (47 + 2*EE^2)*M^2)*rp[t]^3 + J^2*M*(J^2*(43 - 4*EE^2*(1 + la)) + 96*M^2)*
    rp[t]^4 - 2*J^2*(-(J^2*(-3 + EE^2*(1 + la))) + (67 + 12*EE^2)*M^2)*rp[t]^5 + 
   M*(J^2*(59 + EE^2*(12 - 8*la)) + 48*M^2)*rp[t]^6 + 
   2*(J^2*(-4 + EE^2*(-1 + 2*la)) + (-33 + 6*EE^2)*M^2)*rp[t]^7 + 
   (29 - 4*EE^2*(4 + la))*M*rp[t]^8 + 2*(-2 + EE^2*(2 + la))*rp[t]^9)*rp'[t])/
 ((2*M - rp[t])*rp[t]^5*(J^2 + rp[t]^2)^3)
]


def@
getG2RWDot[syms_Association]:=
Module[{t,rp,M,EE,J},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];

	-(((2*M - rp[t])*(2*J^4*M - J^4*rp[t] + 8*J^2*M*rp[t]^2 - 2*(2 + EE^2)*J^2*rp[t]^3 + 
    6*M*rp[t]^4 + (-3 + 2*EE^2)*rp[t]^5))/(rp[t]^4*(J^2 + rp[t]^2)^2))
]


def@
getG3RWDot[syms_Association]:=
Module[{t,rp,M,EE,J},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];

	(EE^2*rp[t]^2*rp'[t])/((2*M - rp[t])*(J^2 + rp[t]^2))
]


def@
getFr1RWDot[syms_Association]:=
Module[{t,rp,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	((-2*M + rp[t])*rp'[t])/rp[t]^2
]


def@
getF1RWDot[syms_Association]:=
Module[{t,rp,M,EE,J},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];

	((2*J^4*M^2 - 3*J^4*M*rp[t] + (J^4 + 8*J^2*M^2)*rp[t]^2 - 2*(5 + 4*EE^2)*J^2*M*rp[t]^3 + 
   ((3 + 2*EE^2)*J^2 + 6*M^2)*rp[t]^4 - 7*M*rp[t]^5 - 2*(-1 + EE^2)*rp[t]^6)*rp'[t])/(rp[t]^4*(J^2 + rp[t]^2)^2)
]


def@
getF2RWDot[syms_Association]:=
Module[{t,rp,M,EE,J},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];

	(-4*J^2*M^2 + 4*J^2*M*rp[t] - (J^2 + 4*M^2)*rp[t]^2 - 4*(-1 + EE^2)*M*rp[t]^3 + 
  (-1 + 2*EE^2)*rp[t]^4)/(rp[t]^3*(J^2 + rp[t]^2))
]


def@
getGtt1EvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	((2 M-rp[t]) (2 (3+la) M+la rp[t]) rp'[t])/(3 M+la rp[t])^2
];


def@
getGtrEvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	-((2 la (-2 M+rp[t])^2)/(3 M+la rp[t])^2)
];


def@
getGrr1EvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	(rp[t] (6 M+la rp[t]) rp'[t])/(3 M+la rp[t])^2
];


def@
getGtEvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	-((M (-2 M+rp[t])^2 (12 M+(-3+2 la) rp[t]))/(rp[t]^3 (3 M+la rp[t])^2))
];


def@
getGr1EvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	((3+2 la) M rp'[t])/(3 M+la rp[t])^2
];


def@
getGSharp1EvenZ[syms_Association]:=
Module[{t,rp,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	
	((-4 M+rp[t]) rp'[t])/rp[t]^3
];


def@
getGtt2EvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	-((-2 M+rp[t])^2/(3 M+la rp[t]))
];


def@
getGrr2EvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	rp[t]^2/(3 M+la rp[t])
];


def@
getGr2EvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	(-2 M+rp[t])/(3 M+la rp[t])
];


def@
getGSharp2EvenZ[syms_Association]:=
Module[{t,rp,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	
	(2 M-rp[t])/rp[t]^2
];


def@
getFttEvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	((-2 M+rp[t])^2 rp'[t])/(3 M+la rp[t])
];


def@
getFrrEvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	-((rp[t]^2 rp'[t])/(3 M+la rp[t]))
];


def@
getFtEvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	-((2 M-rp[t])^3/(rp[t]^2 (3 M+la rp[t])))
];


def@
getFrEvenZ[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	
	((2 M-rp[t]) rp'[t])/(3 M+la rp[t])
];


def@
getFSharpEvenZ[syms_Association]:=
Module[{t,rp,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	
	((-2 M+rp[t]) rp'[t])/rp[t]^2
];


def@
getGtt1EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	((-2 M+rp[t])^2 (6 M^3+la M rp[t]^2))/(rp[t]^4 (3 M+la rp[t])^2)
];


def@
getGrr1EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	(6 M^3+6 (-1+2 la) M^2 rp[t]+la (-7+4 la) M rp[t]^2-2 la^2 rp[t]^3)/(rp[t]^2 (3 M+la rp[t])^2)
];


def@
getGt1EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	((810 M^5+6 (-75+137 la) M^4 rp[t]+(45-471 la+272 la^2) M^3 rp[t]^2+3 la (15-57 la+8 la^2) M^2 rp[t]^3-4 la^2 (-5+4 la) M rp[t]^4+2 la^3 rp[t]^5) rp'[t])/(rp[t]^4 (3 M+la rp[t])^3)
];


def@
getGr1EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	(-918 M^5+6 (102-163 la) M^4 rp[t]-3 (33-239 la+108 la^2) M^3 rp[t]^2+la (-129+265 la-28 la^2) M^2 rp[t]^3+2 la^2 (-27+13 la) M rp[t]^4-6 la^3 rp[t]^5)/(rp[t]^4 (3 M+la rp[t])^3)
];


def@
getGFlat1EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	-(((2 M-rp[t]) (33 M^3+3 (-3+7 la) M^2 rp[t]+2 (-4+la) la M rp[t]^2-la^2 rp[t]^3))/(rp[t]^3 (3 M+la rp[t])^2))
];


def@
getGSharp1EvenZDot[syms_Association]:=
Module[{t,rp,la,M,EE,JJ},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	EE=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	
	1/(rp[t]^6 (3 M+la rp[t])^2 (JJ^2+rp[t]^2)^3) (468 JJ^6 M^5+18 JJ^6 (-26+21 la) M^4 rp[t]+JJ^4 M^3 (JJ^2 (135-363 la+110 la^2)+1404 M^2) rp[t]^2+JJ^4 M^2 (JJ^2 (-9+99 la-101 la^2+12 la^3)+18 (-84+10 EE^2+63 la) M^2) rp[t]^3+JJ^2 M (JJ^4 la (-6+23 la-12 la^2)+3 JJ^2 (171-387 la+110 la^2+4 EE^2 (-6+11 la)) M^2+972 M^4) rp[t]^4+JJ^2 (3 JJ^4 la^3+JJ^2 (-54-9 (-41+6 EE^2) la+7 (-45+4 EE^2) la^2+36 la^3) M^2+18 (-54+28 EE^2+47 la) M^4) rp[t]^5+M (JJ^4 la (-36+(81-8 EE^2) la+4 (-9+EE^2) la^2)+3 JJ^2 (99-267 la+94 la^2+12 EE^2 (-9+10 la)) M^2+36 M^4) rp[t]^6+(-JJ^4 la^2 (3-9 la+2 EE^2 (1+la))+3 JJ^2 (-9+75 la-85 la^2+12 la^3+2 EE^2 (9-38 la+12 la^2)) M^2+18 (4+2 EE^2+5 la) M^4) rp[t]^7+M (JJ^2 la (-18+57 la-36 la^2+4 EE^2 (9-9 la+2 la^2))+(-81-3 la+62 la^2+36 EE^2 (1+la)) M^2) rp[t]^8+(JJ^2 la^2 (EE^2 (2-4 la)+9 la)+(18-45 la-41 la^2+12 la^3+6 EE^2 (-3+3 la+2 la^2)) M^2) rp[t]^9+la (12-la-12 la^2+4 EE^2 (-3+la+la^2)) M rp[t]^10+la^2 (3 (1+la)-2 EE^2 (2+la)) rp[t]^11)
];


def@
getGt2EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	-(((2 M-rp[t]) (45 M^3+(-15+31 la) M^2 rp[t]+la (-13+4 la) M rp[t]^2-2 la^2 rp[t]^3))/(rp[t]^3 (3 M+la rp[t])^2))

	];


def@
getGSharp2EvenZDot[syms_Association]:=
Module[{t,rp,EE,JJ},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	
	(2 EE^2 (-JJ^2+rp[t]^2) rp'[t])/(JJ^2+rp[t]^2)^2

	];


def@
getGSharp3EvenZDot[syms_Association]:=
Module[{t,rp,EE,JJ},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	
	-((EE^2 rp[t])/(JJ^2+rp[t]^2))

	];


def@
getFtt1EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	(M (2 M-rp[t])^3)/(rp[t]^3 (3 M+la rp[t]))
];


def@
getFrr1EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	(M (-2 M+rp[t]))/(rp[t] (3 M+la rp[t]))
];


def@
getFt1EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	((2 M-rp[t]) (45 M^3+(-15+31 la) M^2 rp[t]+la (-13+4 la) M rp[t]^2-2 la^2 rp[t]^3) rp'[t])/(rp[t]^3 (3 M+la rp[t])^2)
];


def@
getFr1EvenZDot[syms_Association]:=
Module[{t,rp,la,M},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];

	-((M (2 M-rp[t]) (9 M^2+(3+7 la) M rp[t]-la rp[t]^2))/(rp[t]^3 (3 M+la rp[t])^2))
];


def@
getFSharp1EvenZDot[syms_Association]:=
Module[{t,rp,la,M,EE,JJ},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	EE=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	
	1/(rp[t]^5 (3 M+la rp[t]) (JJ^2+rp[t]^2)^2) (2 M-rp[t]) (30 JJ^4 M^3+3 JJ^4 (-7+2 la) M^2 rp[t]+3 JJ^2 M (-JJ^2 (-1+la)+8 M^2) rp[t]^2+6 (-1+4 EE^2) JJ^2 M^2 rp[t]^3+M (JJ^2 (-3+6 la+EE^2 (-6+8 la))-6 M^2) rp[t]^4-((3+2 EE^2) JJ^2 la+3 (-5+2 la) M^2) rp[t]^5+3 (-2+2 EE^2+3 la) M rp[t]^6+(-3+2 EE^2) la rp[t]^7)
];


def@
getFSharp2EvenZDot[syms_Association]:=
Module[{t,rp,EE,JJ},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	
	(2 EE^2 rp[t] rp'[t])/(JJ^2+rp[t]^2)

];


Clear[gSourceZM3]
gSourceZM3[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];

	fp=SchwarzschildF[rp[t],M];

	(8*fp^5*J*mu*Pi*YPhiBar*(3672*J^8*M^5 + 36*J^8*(-119 + 76*la)*M^4*rp[t] + 4*J^6*M^3*(J^2*(402 - 805*la + 132*la^2) + 3564*M^2)*rp[t]^2 + 
    2*J^6*M^2*(J^2*(-96 + 609*la - 314*la^2 + 2*la^3) + 48*(-172 + 21*En^2 + 111*la)*M^2)*rp[t]^3 + 
    2*J^4*M*(J^4*la*(-73 + 120*la - 2*la^2) + 3*J^2*(1022 - 2074*la + 346*la^2 + En^2*(-226 + 237*la))*M^2 + 10368*M^4)*rp[t]^4 + 
    J^4*(J^4*(-29 + la)*la^2 + 2*J^2*(3*En^2*(36 - 162*la + 41*la^2) + 2*(-180 + 1163*la - 612*la^2 + 5*la^3))*M^2 + 72*(-331 + 93*En^2 + 216*la)*M^4)*rp[t]^5 + 
    J^2*M*(-(J^4*la*(547 - 923*la + 20*la^2 + 2*En^2*(-79 + 91*la + 4*la^2))) + 6*J^2*(1458 + 24*En^4 - 3002*la + 510*la^2 + En^2*(-744 + 789*la))*M^2 + 13392*M^4)*rp[t]^6 + 
    J^2*(J^4*la^2*(-109 + 5*la + En^2*(34 + 4*la)) + 6*J^2*(2*En^4*(-3 + 7*la) + En^2*(114 - 533*la + 139*la^2) + 2*(-84 + 554*la - 298*la^2 + 3*la^3))*M^2 + 288*(-53 + 27*En^2 + 35*la)*M^4)*
     rp[t]^7 + M*(-(J^4*la*(765 - 8*En^4*(-3 + la) - 1329*la + 36*la^2 + En^2*(-498 + 602*la + 20*la^2))) + 2*J^2*(2766 + 288*En^4 - 5786*la + 1002*la^2 + 9*En^2*(-294 + 305*la))*M^2 + 3240*M^4)*
     rp[t]^8 + (J^4*la^2*(9*(-17 + la) - 4*En^4*(2 + la) + 2*En^2*(53 + 5*la)) + 2*J^2*(18*En^4*(-3 + 10*la) + 3*En^2*(138 - 628*la + 163*la^2) + 2*(-156 + 1053*la - 580*la^2 + 7*la^3))*M^2 + 
      12*(-305 + 258*En^2 + 204*la)*M^4)*rp[t]^9 + M*(-(J^2*la*(473 + En^4*(72 - 48*la) - 849*la + 28*la^2 + 2*En^2*(-297 + 353*la + 8*la^2))) + 
      6*(218 + 120*En^4 - 464*la + 82*la^2 + En^2*(-364 + 363*la))*M^2)*rp[t]^10 + (J^2*la^2*(-95 + 7*la - 4*En^4*(5 + 2*la) + 2*En^2*(61 + 4*la)) + 
      2*(-72 + 499*la - 282*la^2 + 4*la^3 + 18*En^4*(-6 + 13*la) + 3*En^2*(60 - 257*la + 65*la^2))*M^2)*rp[t]^11 - la*(109 - 72*En^4*(-2 + la) - 203*la + 8*la^2 + En^2*(-254 + 286*la + 4*la^2))*M*
     rp[t]^12 - 2*(-1 + En^2)*la^2*(-11 + la + 2*En^2*(7 + la))*rp[t]^13))/(CapLa^2*En^2*(1 + la)*USq^3*rp[t]^18) - 
 (8*fp^3*mu*Pi*YBar*(-72*J^6*M^4 + 6*J^6*(15 - 4*la)*M^3*rp[t] + 2*J^4*M^2*(J^2*(-12 + 20*la + la^2) + 90*M^2)*rp[t]^2 + J^4*M*(J^2*(-13 + la)*la - 6*(8 + 21*En^2 - 33*la)*M^2)*rp[t]^3 + 
    (-(J^6*la^2) + J^4*(-12 + En^2*(36 - 78*la) - 97*la + 52*la^2)*M^2 + 576*J^2*M^4)*rp[t]^4 - 2*J^2*M*(J^2*la*(-1 + 17*la + En^2*(-11 + 5*la)) + 3*(61 + 24*En^2 - 78*la)*M^2)*rp[t]^5 + 
    2*((2 + En^2)*J^4*la^2 + J^2*(24 - 157*la + 49*la^2 - 6*En^2*(-6 + 7*la))*M^2 + 162*M^4)*rp[t]^6 + M*(J^2*(43 + En^2*(44 - 8*la) - 71*la)*la + 6*(-38 + 21*En^2 + 41*la)*M^2)*rp[t]^7 + 
    ((11 + 4*En^2)*J^2*la^2 + 3*(12 - 59*la + 16*la^2 + 6*En^2*(-2 + 5*la))*M^2)*rp[t]^8 + 2*la*(14 - 18*la + En^2*(-13 + 9*la))*M*rp[t]^9 - 6*(-1 + En^2)*la^2*rp[t]^10)*Derivative[1][rp][t])/
  (CapLa^2*En*USq^2*rp[t]^14) + 
 YPhiPhiBar*(((8*I)*fp^6*J^3*m*mu*Pi*(-4860*J^8*M^5 + 12*J^8*(486 - 298*la + 3*m^2)*M^4*rp[t] - 3*J^6*M^3*(J^2*(753 - 1432*la + 232*la^2 + (12 - 8*la)*m^2) + 7224*M^2)*rp[t]^2 + 
      J^6*M^2*(J^2*(279 - 1662*la + 832*la^2 - 12*la^3 + (9 - 24*la + 4*la^2)*m^2) - 18*(-1454 + 171*En^2 + 880*la - 6*m^2)*M^2)*rp[t]^3 - 
      2*J^4*M*(J^4*la*(-3*(34 - 53*la + 2*la^2) + (-3 + 2*la)*m^2) - 3*J^2*(-1701 + 3192*la - 508*la^2 + 6*(-3 + 2*la)*m^2 + En^2*(357 - 364*la + 6*m^2))*M^2 + 18360*M^4)*rp[t]^4 + 
      J^4*(J^4*la^2*(38 - 3*la + m^2) + J^2*(1269 - 7464*la + 3668*la^2 - 48*la^3 + 3*(9 - 24*la + 4*la^2)*m^2 + 6*En^2*(-57 + 253*la - 67*la^2 + (-3 + 4*la)*m^2))*M^2 - 
        18*(-2490 + 715*En^2 + 1480*la - 6*m^2)*M^4)*rp[t]^5 - 2*J^2*M*(J^4*la*(-462 + 707*la - 24*la^2 + (-9 + 6*la)*m^2 - 2*En^2*(-60 + 71*la + (-3 + la)*m^2)) + 
        6*J^2*(1476 + 27*En^4 - 2712*la + 422*la^2 + (9 - 6*la)*m^2 + En^2*(-756 + 752*la - 6*m^2))*M^2 + 13932*M^4)*rp[t]^6 + 
      J^2*(J^4*la^2*(-2*En^2*(23 + m^2) + 3*(57 - 4*la + m^2)) - 3*J^2*(4*En^4*(-6 + 17*la) + 3*(-249 + 1428*la - 684*la^2 + 8*la^3) + (-9 + 24*la - 4*la^2)*m^2 + 
          En^2*(480 - 2122*la + 542*la^2 - 4*(-3 + 4*la)*m^2))*M^2 - 6*(-5742 + 3327*En^2 + 3344*la - 6*m^2)*M^4)*rp[t]^7 - 
      2*M*(J^4*la*(-810 + 1203*la - 36*la^2 + 2*En^4*(-12 + 7*la) + (-9 + 6*la)*m^2 + En^2*(504 - 581*la - 4*(-3 + la)*m^2)) + 
        3*J^2*(2307 + 264*En^4 - 4136*la + 628*la^2 + (6 - 4*la)*m^2 + En^2*(-2529 + 2300*la - 6*m^2))*M^2 + 3978*M^4)*rp[t]^8 + 
      (J^4*la^2*(4*En^4*(3 + la) - 4*En^2*(47 + m^2) + 3*(99 - 6*la + m^2)) + J^2*(1791 - 9960*la + 4636*la^2 - 48*la^3 - 24*En^4*(-12 + 43*la) + (9 - 24*la + 4*la^2)*m^2 + 
          6*En^2*(-453 + 1747*la - 405*la^2 + (-3 + 4*la)*m^2))*M^2 - 18*(-554 + 565*En^2 + 316*la)*M^4)*rp[t]^9 + 
      M*(2*J^2*la*(642 + En^4*(96 - 80*la) - 921*la + 24*la^2 + (3 - 2*la)*m^2 + 2*En^2*(-468 + 464*la + (-3 + la)*m^2)) - 3*(1359 + 996*En^4 - 2376*la + 352*la^2 + 4*En^2*(-687 + 580*la))*M^2)*
       rp[t]^10 + (J^2*la^2*(233 - 12*la + 8*En^4*(5 + la) + m^2 - 2*En^2*(167 + m^2)) - 2*(-270 + 1455*la - 658*la^2 + 6*la^3 + 90*En^4*(-6 + 11*la) + 9*En^2*(90 - 313*la + 67*la^2))*M^2)*
       rp[t]^11 + 2*la*(192 - 266*la + 6*la^2 - 18*En^4*(-20 + 9*la) + En^2*(-552 + 489*la))*M*rp[t]^12 + la^2*(69 - 192*En^2 - 3*la + 4*En^4*(31 + la))*rp[t]^13))/
    (CapLa^2*En^2*la*(1 + la)*USq^4*rp[t]^20) + (8*fp^6*J^2*mu*Pi*(-7938*J^10*M^5 + 3*J^10*(2757 - 2252*la + 174*m^2)*M^4*rp[t] - 
      3*J^8*M^3*(J^2*(900 - 2294*la + 588*la^2 - 3*(-49 + 40*la)*m^2) + 14070*M^2)*rp[t]^2 + J^8*M^2*(2*J^2*(135 - 1083*la + 857*la^2 - 63*la^3 + (45 - 153*la + 31*la^2)*m^2) - 
        9*(-4887 + 304*En^2 + 3956*la - 252*m^2)*M^2)*rp[t]^3 + J^6*M*(J^4*la*(204 - 497*la + 105*la^2 + (63 - 53*la)*m^2) + 
        6*J^2*(-2391 + 6055*la - 1526*la^2 + 4*(-81 + 65*la)*m^2 + En^2*(237 - 371*la + 48*m^2))*M^2 - 91260*M^4)*rp[t]^4 + 
      J^6*(J^4*la^2*(40 - 21*la + 11*m^2) + J^2*(1431 - 11454*la + 8930*la^2 - 630*la^3 + (405 - 1344*la + 268*la^2)*m^2 + 6*En^2*(-27 + 180*la - 92*la^2 + (-15 + 32*la)*m^2))*M^2 - 
        18*(-5297 + 748*En^2 + 4228*la - 204*m^2)*M^4)*rp[t]^5 + J^4*M*(J^4*la*(5*(216 - 521*la + 105*la^2) + (282 - 232*la)*m^2 + 2*En^2*(-57 + 122*la - 15*la^2 + 2*(-15 + 8*la)*m^2)) + 
        6*J^2*(-5193 + 6*En^4 + 12998*la - 3200*la^2 + (-531 + 420*la)*m^2 + 2*En^2*(588 - 905*la + 102*m^2))*M^2 - 100764*M^4)*rp[t]^6 + 
      J^4*(J^4*la^2*(212 - 105*la + 49*m^2 - 2*En^2*(13 - 3*la + 5*m^2)) + 3*J^2*(1035 - 8236*la + 12*En^4*la + 6280*la^2 - 420*la^3 + 3*(75 - 244*la + 48*la^2)*m^2 + 
          2*En^2*(-135 + 897*la - 440*la^2 + (-75 + 136*la)*m^2))*M^2 - 6*(-17691 + 4596*En^2 + 13804*la - 438*m^2)*M^4)*rp[t]^7 - 
      2*J^2*M*(J^4*la*(-1170 + 2774*la - 6*En^4*la - 525*la^2 + 9*(-26 + 21*la)*m^2 + En^2*(285 - 610*la + 66*la^2 + (150 - 68*la)*m^2)) - 
        3*J^2*(-5841 + 30*En^4 + 14278*la - 3400*la^2 + 12*(-32 + 25*la)*m^2 + En^2*(2406 - 3620*la + 264*m^2))*M^2 + 28449*M^4)*rp[t]^8 + 
      J^2*(J^4*la^2*(459 - 210*la + 4*En^4*(1 + la) + 81*m^2 - 10*En^2*(13 - 3*la + 5*m^2)) + J^2*(3537 - 27492*la + 180*En^4*la + 20224*la^2 - 1260*la^3 + 11*(45 - 144*la + 28*la^2)*m^2 + 
          6*En^2*(-3*(81 - 609*la + 280*la^2) + (-105 + 176*la)*m^2))*M^2 - 9*(-6759 + 3432*En^2 + 5108*la - 78*m^2)*M^4)*rp[t]^9 + 
      M*(2*J^4*la*(1326 - 3028*la + 30*En^4*la + 525*la^2 + (171 - 136*la)*m^2 + En^2*(-525 + 1214*la - 108*la^2 + (-210 + 88*la)*m^2)) + 
        3*J^2*(-6834 + 84*En^4 + 16094*la - 3668*la^2 + (-207 + 160*la)*m^2 + 36*En^2*(43*(4 - 5*la) + 6*m^2))*M^2 - 13122*M^4)*rp[t]^10 + 
      (J^4*la^2*(517 - 210*la + 20*En^4*(1 + la) + 59*m^2 + En^2*(-242 + 54*la - 70*m^2)) + J^2*(2133 - 15846*la + 11078*la^2 - 630*la^3 + 36*En^4*(-18 + 7*la) + (135 - 426*la + 82*la^2)*m^2 + 
          6*En^2*(-405 + 2259*la - 824*la^2 + 9*(-5 + 8*la)*m^2))*M^2 - 9*(-1591 + 1560*En^2 + 1156*la)*M^4)*rp[t]^11 - 
      M*(J^2*la*(-1584 + En^4*(432 - 84*la) + 3403*la - 525*la^2 + (-93 + 73*la)*m^2 - 6*En^2*(-285 + 458*la - 26*la^2 + 6*(-5 + 2*la)*m^2)) + 
        6*(828 + 558*En^4 - 1859*la + 402*la^2 + 3*En^2*(-543 + 563*la))*M^2)*rp[t]^12 + (J^2*la^2*(305 - 105*la + 4*En^4*(-11 + 7*la) + 16*m^2 - 6*En^2*(57 - 7*la + 5*m^2)) - 
        6*(-90 + 629*la - 413*la^2 + 21*la^3 + 6*En^4*(-30 + 61*la) + En^2*(270 - 1149*la + 332*la^2))*M^2)*rp[t]^13 + 
      la*(396 + En^4*(720 - 348*la) - 787*la + 105*la^2 - 6*En^2*(186 - 216*la + 7*la^2))*M*rp[t]^14 + 3*la^2*(25 + 4*En^2*(-17 + la) - 7*la + 4*En^4*(11 + la))*rp[t]^15)*Derivative[1][rp][t])/
    (CapLa^2*En*la*(1 + la)*USq^5*rp[t]^21))
]


Clear[fSourceZM3]
fSourceZM3[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];
	
	fp=SchwarzschildF[rp[t],M];

	(-8*fp^5*J*mu*Pi*YPhiBar*(72*J^6*M^3 + 2*J^6*(-31 + 15*la)*M^2*rp[t] + J^4*M*(J^2*(13 - 27*la) + 216*M^2)*rp[t]^2 + 6*J^4*(J^2*la + (-31 + 13*En^2 + 15*la)*M^2)*rp[t]^3 + 
    3*J^2*M*(J^2*(13 + 10*En^2*(-1 + la) - 27*la) + 72*M^2)*rp[t]^4 + 6*J^2*((3 - 2*En^2)*J^2*la + (-31 + 32*En^2 + 15*la)*M^2)*rp[t]^5 + 
    3*M*(J^2*(13 + 4*En^4 - 27*la + En^2*(-26 + 24*la)) + 24*M^2)*rp[t]^6 + 2*((9 - 15*En^2 + 2*En^4)*J^2*la + (-31 + 57*En^2 + 15*la)*M^2)*rp[t]^7 + 
    (13 + 36*En^4 - 27*la + 6*En^2*(-8 + 7*la))*M*rp[t]^8 + 6*(1 - 3*En^2 + 2*En^4)*la*rp[t]^9))/(CapLa*En^2*(1 + la)*USq^2*rp[t]^13) + 
 (8*fp^3*mu*Pi*YBar*(3*J^4*M^2 + J^4*(-1 + 2*la)*M*rp[t] + (-(J^4*la) + 12*J^2*M^2)*rp[t]^2 + J^2*(-5 - 6*En^2 + 6*la)*M*rp[t]^3 + (-((3 + 2*En^2)*J^2*la) + 9*M^2)*rp[t]^4 + 
    2*(-2 + 3*En^2 + 2*la)*M*rp[t]^5 + 2*(-1 + En^2)*la*rp[t]^6)*Derivative[1][rp][t])/(CapLa*En*USq*rp[t]^9) + 
 YPhiPhiBar*(((8*I)*fp^6*J^3*m*mu*Pi*(78*J^6*M^3 + 3*J^6*(-23 + 10*la)*M^2*rp[t] + 3*J^4*M*(J^2*(5 - 9*la) + 90*M^2)*rp[t]^2 + 3*J^4*(2*J^2*la + (-81 + 38*En^2 + 34*la)*M^2)*rp[t]^3 + 
      3*J^2*M*(J^2*(18 - 31*la + 2*En^2*(-8 + 7*la)) + 102*M^2)*rp[t]^4 + 3*J^2*((7 - 6*En^2)*J^2*la + (-93 + 124*En^2 + 38*la)*M^2)*rp[t]^5 + 
      3*M*(J^2*(21 + 8*En^4 - 35*la + En^2*(-56 + 44*la)) + 38*M^2)*rp[t]^6 + (4*(6 - 15*En^2 + 2*En^4)*J^2*la + 3*(-35 + 86*En^2 + 14*la)*M^2)*rp[t]^7 + 
      3*(8 + 32*En^4 - 13*la + 10*En^2*(-4 + 3*la))*M*rp[t]^8 + (9 - 42*En^2 + 32*En^4)*la*rp[t]^9))/(CapLa*En^2*la*(1 + la)*USq^3*rp[t]^15) + 
   (8*fp^6*J^2*mu*Pi*(147*J^8*M^3 - 3*J^8*(36 - 25*la + 2*m^2)*M^2*rp[t] + J^6*M*(J^2*(18 - 53*la + 6*la^2 + (3 - 2*la)*m^2) + 648*M^2)*rp[t]^2 + 
      J^6*(J^2*la*(8 - 3*la + m^2) + 6*(-80 + 22*En^2 + 54*la - 3*m^2)*M^2)*rp[t]^3 + J^4*M*(J^2*(81 - 232*la + 24*la^2 + (9 - 6*la)*m^2 - 6*En^2*(6 - 11*la + 2*m^2)) + 1098*M^2)*rp[t]^4 + 
      J^4*(J^2*la*(3*(12 - 4*la + m^2) - 2*En^2*(7 - 3*la + 2*m^2)) + 6*(-138 + 86*En^2 + 89*la - 3*m^2)*M^2)*rp[t]^5 + 
      3*J^2*M*(J^2*(2*(24 - 65*la + 6*la^2) + (3 - 2*la)*m^2 + En^2*(-48 + 82*la - 8*m^2)) + 280*M^2)*rp[t]^6 + 
      J^2*(J^2*la*(3*(21 - 6*la + m^2) - 2*En^2*(29 - 9*la + 4*m^2)) + 6*(-108 + 142*En^2 + 66*la - m^2)*M^2)*rp[t]^7 + 
      M*(J^2*(117 - 296*la + 24*la^2 + (3 - 2*la)*m^2 - 6*En^2*(48 - 61*la + 2*m^2)) + 243*M^2)*rp[t]^8 + (J^2*la*(50 - 12*la + m^2 - 2*En^2*(55 - 9*la + 2*m^2)) + 3*(-64 + 156*En^2 + 37*la)*M^2)*
       rp[t]^9 + (36 + 144*En^4 - 85*la + 6*la^2 + 6*En^2*(-30 + 31*la))*M*rp[t]^10 + 3*(5 + 16*En^4 + 2*En^2*(-11 + la) - la)*la*rp[t]^11)*Derivative[1][rp][t])/
    (CapLa*En*la*(1 + la)*USq^4*rp[t]^16))
]


Clear[gSourceZM4]
gSourceZM4[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];
	
	fp=SchwarzschildF[rp[t],M];

	 (-8*fp^6*mu*Pi*YBar*(12546*J^10*M^5 + 6*J^10*(-2514 + 1471*la)*M^4*rp[t] + 2*J^8*M^3*(J^2*(2922 - 5312*la + 807*la^2) + 25785*M^2)*rp[t]^2 + 
    J^8*M^2*(J^2*(-720 + 4119*la - 1935*la^2 + 20*la^3) + 6*(-10492 + 1119*En^2 + 6060*la)*M^2)*rp[t]^3 + 
    J^6*M*(J^4*la*(-507 + 744*la - 20*la^2) + J^2*(24768 - 44395*la + 6720*la^2 + 6*En^2*(-784 + 769*la))*M^2 + 76356*M^4)*rp[t]^4 + 
    J^6*(5*J^4*(-18 + la)*la^2 + J^2*(-3096 + 17461*la - 8140*la^2 + 108*la^3 + En^2*(756 - 3240*la + 806*la^2))*M^2 + 24*(-3994 + 1059*En^2 + 2250*la)*M^4)*rp[t]^5 + 
    2*J^4*M*(J^4*la*(-1089 + En^2*(261 - 286*la) + 1579*la - 54*la^2) + 6*J^2*(3239 + 42*En^4 - 5640*la + 845*la^2 + 2*En^2*(-763 + 730*la))*M^2 + 21474*M^4)*rp[t]^6 + 
    J^4*(J^4*la^2*(-384 + 94*En^2 + 27*la) + J^2*(-5022 + 27379*la - 12522*la^2 + 220*la^3 + 108*En^4*(-1 + 3*la) + 2*En^2*(1485 - 6318*la + 1552*la^2))*M^2 + 12*(-4788 + 2823*En^2 + 2555*la)*M^4)*
     rp[t]^7 + 2*J^2*M*(J^4*la*(-1761 + 2477*la - 110*la^2 + 12*En^4*(-3 + 2*la) + 6*En^2*(171 - 186*la + 2*la^2)) + J^2*(12408 + 1224*En^4 - 20375*la + 3000*la^2 + 6*En^2*(-2250 + 1943*la))*M^2 + 
      189*M^4)*rp[t]^8 + J^2*(-(J^4*la^2*(614 + 12*En^2*(-30 + la) - 55*la + 4*En^4*(4 + la))) + J^2*(-3420 + 17481*la - 7716*la^2 + 212*la^3 + 108*En^4*(-4 + 15*la) + 
        12*En^2*(420 - 1544*la + 347*la^2))*M^2 + 6*(-622 + 1860*En^2 + 105*la)*M^4)*rp[t]^9 + 2*M*(J^4*la*(-1194 + 1587*la - 106*la^2 + 12*En^4*(-12 + 11*la) + 2*En^2*(861 - 810*la + 16*la^2)) + 
      J^2*(2340*En^4 + 12*En^2*(-445 + 322*la) + 5*(288 - 292*la + 39*la^2))*M^2 - 2619*M^4)*rp[t]^10 - 
    (J^4*la^2*(408 - 53*la + 4*En^4*(13 + la) + En^2*(-582 + 32*la)) + J^2*(558 - 2074*la + 751*la^2 - 96*la^3 - 108*En^4*(-17 + 29*la) - 18*En^2*(133 - 406*la + 80*la^2))*M^2 + 
      6*(-956 + 669*En^2 + 594*la)*M^4)*rp[t]^11 - M*(J^2*la*(387 - 398*la + 96*la^2 - 24*En^4*(-51 + 22*la) - 4*En^2*(405 - 318*la + 14*la^2)) + 
      3*(664 + 240*En^4 - 1307*la + 192*la^2 + En^2*(-904 + 894*la))*M^2)*rp[t]^12 - 2*(-2*(-1 + En^2)*J^2*(15 + En^2*(-50 + la) - 6*la)*la^2 + 
      (-108 + 685*la - 324*la^2 - 8*la^3 + 18*En^4*(-6 + 13*la) + 3*En^2*(72 - 304*la + 71*la^2))*M^2)*rp[t]^13 - 2*(-1 + En^2)*la*(75 + 36*En^2*(-2 + la) - 118*la - 8*la^2)*M*rp[t]^14 + 
    4*(-1 + En^2)^2*la^2*(7 + la)*rp[t]^15))/(CapLa^2*En*USq^4*rp[t]^20) - 
 (16*fp^6*J*mu*Pi*YPhiBar*(17766*J^10*M^5 + 3*J^10*(-6084 + 4115*la)*M^4*rp[t] + 3*J^8*M^3*(J^2*(1985 - 4234*la + 715*la^2) + 30954*M^2)*rp[t]^2 + 
    J^8*M^2*(-2*J^2*(306 - 2080*la + 1107*la^2 + 6*la^3) + 9*(-10544 + 544*En^2 + 7241*la)*M^2)*rp[t]^3 + 
    J^6*M*(16*J^4*la*(-27 + 46*la + la^2) + 3*J^2*(10231 - 22186*la + 3883*la^2 + 4*En^2*(-219 + 268*la))*M^2 + 196020*M^4)*rp[t]^4 + 
    J^6*(-5*J^4*la^2*(16 + la) + J^2*(-3114 + 21541*la - 11862*la^2 + 12*la^3 + 6*En^2*(54 - 291*la + 82*la^2))*M^2 + 18*(-11084 + 1324*En^2 + 7723*la)*M^4)*rp[t]^5 + 
    J^4*M*(J^4*la*(En^2*(222 - 298*la - 28*la^2) + 3*(-733 + 1287*la + 8*la^2)) - 6*J^2*(-10666 + 6*En^4 + En^2*(2142 - 2638*la) + 23528*la - 4263*la^2)*M^2 + 209412*M^4)*rp[t]^6 + 
    J^4*(3*J^4*la^2*(-136 - 5*la + 4*En^2*(4 + la)) + J^2*(-6390 + 45179*la - 36*En^4*la - 25772*la^2 + 196*la^3 + 12*En^2*(135 - 714*la + 208*la^2))*M^2 + 6*(-35496 + 7764*En^2 + 25009*la)*M^4)*
     rp[t]^7 + 2*J^2*M*(-(J^4*la*(2259 - 4112*la + 6*En^4*la + 46*la^2 + En^2*(-555 + 730*la + 58*la^2))) - 3*J^2*(-11338 + 30*En^4 + En^2*(4158 - 5270*la) + 25340*la - 4731*la^2)*M^2 + 56691*M^4)*
     rp[t]^8 + J^2*(-(J^4*la^2*(4*En^4*(1 + la) + 3*(280 + la) - 4*En^2*(59 + 14*la))) + J^2*(-6714 + 48323*la - 180*En^4*la - 28428*la^2 + 396*la^3 + 24*En^2*(120 - 706*la + 221*la^2))*M^2 + 
      9*(-12860 + 5160*En^2 + 9113*la)*M^4)*rp[t]^9 + M*(-2*J^4*la*(2376 - 4469*la + 30*En^4*la + 130*la^2 + En^2*(-999 + 1492*la + 70*la^2)) - 
      3*J^2*(-12355 + 84*En^4 + 27766*la - 5303*la^2 - 12*En^2*(-751 + 891*la))*M^2 + 24894*M^4)*rp[t]^10 + 
    (J^4*la^2*(-884 + 31*la - 20*En^4*(1 + la) + En^2*(424 + 76*la)) + J^2*(-3654 + 26473*la - 15918*la^2 + 312*la^3 - 36*En^4*(-18 + 7*la) + 12*En^2*(291 - 1552*la + 472*la^2))*M^2 + 
      9*(-2848 + 2088*En^2 + 2017*la)*M^4)*rp[t]^11 + M*(-2*J^2*la*(6*En^4*(-36 + 7*la) + En^2*(-1209 + 1678*la + 22*la^2) + 3*(431 - 828*la + 38*la^2)) + 
      3*(2765 + 1116*En^4 - 6190*la + 1199*la^2 + 36*En^2*(-114 + 121*la))*M^2)*rp[t]^12 + 2*(2*J^2*la^2*(-120 + En^4*(11 - 7*la) + 9*la + En^2*(119 + 8*la)) + 
      (-414 + 2974*la - 1807*la^2 + 44*la^3 + 18*En^4*(-30 + 61*la) + 3*En^2*(318 - 1417*la + 394*la^2))*M^2)*rp[t]^13 + 
    la*(-585 + 1129*la - 68*la^2 + 12*En^4*(-60 + 29*la) + 2*En^2*(654 - 767*la + 4*la^2))*M*rp[t]^14 - 12*(-1 + En^2)*la^2*(-9 + la + En^2*(11 + la))*rp[t]^15)*Derivative[1][rp][t])/
  (CapLa^2*(1 + la)*USq^5*rp[t]^21) + 
 YPhiPhiBar*((8*fp^9*J^2*mu*Pi*(170100*J^14*M^6 - 9*J^14*(26203 - 16188*la + 2662*m^2)*M^5*rp[t] + 3*J^12*M^4*(J^2*(38739 - 65592*la + 13238*la^2 + (9585 - 5608*la)*m^2 + 36*m^4) + 437616*M^2)*
       rp[t]^2 + 3*J^12*M^3*(2*J^2*(-3942 + 15608*la - 8577*la^2 + 541*la^3 + (-1857 + 3367*la - 509*la^2)*m^2 + 6*(-3 + 2*la)*m^4) + (-607611 + 32184*En^2 + 372252*la - 55908*m^2)*M^2)*rp[t]^3 + 
      J^10*M^2*(J^4*(1620 - 18144*la + 23065*la^2 - 3855*la^3 - 12*la^4 + (1377 - 7821*la + 3659*la^2 - 28*la^3)*m^2 + 3*(9 - 24*la + 4*la^2)*m^4) - 
        18*J^2*(-49973 + 84032*la - 16727*la^2 + 3*(-3767 + 2172*la)*m^2 - 30*m^4 + En^2*(4711 - 4451*la + 728*m^2))*M^2 + 4407588*M^4)*rp[t]^4 + 
      J^10*M*(J^4*la*(1152 - 4063*la + 1480*la^2 + 12*la^3 + (963 - 1408*la + 28*la^2)*m^2 - 6*(-3 + 2*la)*m^4) + 
        3*J^2*(-61083 + 240640*la - 130586*la^2 + 8046*la^3 + (-26586 + 47448*la - 7036*la^2)*m^2 + 60*(-3 + 2*la)*m^4 + En^2*(7386 - 22604*la + 6940*la^2 - 4*(-765 + 754*la)*m^2 + 24*m^4))*M^2 + 
        27*(-227081 + 25344*En^2 + 137684*la - 18270*m^2)*M^4)*rp[t]^5 + J^8*(J^6*la^2*(216 - 182*la - 3*la^2 + (171 - 7*la)*m^2 + 3*m^4) + 
        J^4*(12555 - 140256*la + 176389*la^2 - 28753*la^3 - 72*la^4 - 6*(-1665 + 9300*la - 4261*la^2 + 28*la^3)*m^2 + 15*(9 - 24*la + 4*la^2)*m^4 + 
          2*En^2*(-810 + 8412*la - 8378*la^2 + 709*la^3 + (-738 + 3168*la - 800*la^2)*m^2 + 6*(-3 + 4*la)*m^4))*M^2 + 9*J^2*(336717 + 1008*En^4 - 561488*la + 109794*la^2 + (67425 - 38160*la)*m^2 + 
          120*m^4 + En^2*(-67198 + 62892*la - 9776*m^2))*M^4 + 8400240*M^6)*rp[t]^6 + 
      J^8*M*(J^4*la*(8928 - 31237*la + 11066*la^2 + 72*la^3 + 3*(2325 - 3324*la + 56*la^2)*m^2 + (90 - 60*la)*m^4 + 2*En^2*(-567 + 1937*la - 498*la^2 - 16*la^3 + (-507 + 566*la)*m^2 + 
            4*(-3 + la)*m^4)) - 3*J^2*(206037 - 806964*la + 430910*la^2 - 25698*la^3 + 30*(2691 - 4697*la + 681*la^2)*m^2 - 120*(-3 + 2*la)*m^4 + 12*En^4*(93 - 188*la + 28*m^2) - 
          4*En^2*(2*(6618 - 20107*la + 6103*la^2) + (5235 - 5038*la)*m^2 + 24*m^4))*M^2 + 45*(-260461 + 45768*En^2 + 155684*la - 17224*m^2)*M^4)*rp[t]^7 + 
      J^6*(J^6*la^2*(-2*En^2*(117 - 85*la - 8*la^2 + 92*m^2 + 2*m^4) + 3*(558 - 454*la - 6*la^2 + (411 - 14*la)*m^2 + 5*m^4)) + 
        J^4*(12*En^4*(18 - 198*la + 121*la^2 + (18 - 54*la)*m^2) + 2*En^2*(-5805 + 60162*la - 59387*la^2 + 5002*la^3 - 8*(639 - 2700*la + 661*la^2)*m^2 + 24*(-3 + 4*la)*m^4) + 
          3*(14130 - 157350*la + 195249*la^2 - 30737*la^3 - 60*la^4 - 5*(-2070 + 11247*la - 5019*la^2 + 28*la^3)*m^2 + 10*(9 - 24*la + 4*la^2)*m^4))*M^2 + 
        6*J^2*(10728*En^4 - 3*En^2*(101838 - 94397*la + 13832*m^2) + 5*(193497 - 319116*la + 60889*la^2 + (32274 - 17912*la)*m^2 + 36*m^4))*M^4 + 9919260*M^6)*rp[t]^8 + 
      J^6*M*(-2*J^4*la*(12*En^4*(-6 + 23*la + (-6 + 4*la)*m^2) + En^2*(4059 - 13765*la + 3526*la^2 + 92*la^3 + (3513 - 3810*la)*m^2 - 16*(-3 + la)*m^4) + 
          3*(-3*(1675 - 5807*la + 1980*la^2 + 10*la^3) - 5*(720 - 997*la + 14*la^2)*m^2 + 10*(-3 + 2*la)*m^4)) - 
        6*J^2*(6*En^4*(675 - 1364*la + 220*m^2) - En^2*(81387 - 243750*la + 72946*la^2 + (31260 - 28344*la)*m^2 + 72*m^4) - 
          5*(-39438 + 153650*la - 80247*la^2 + 4567*la^3 - 2*(6567 - 11192*la + 1586*la^2)*m^2 + 12*(-3 + 2*la)*m^4))*M^2 + 45*(-310117 + 75744*En^2 + 180996*la - 15186*m^2)*M^4)*rp[t]^9 + 
      J^4*(J^6*la^2*(5661 - 4404*la - 45*la^2 - 15*(-253 + 7*la)*m^2 + 30*m^4 + 4*En^4*(-3*(-4 + la + la^2) + 2*(4 + la)*m^2) - 2*En^2*(831 - 592*la - 46*la^2 + 628*m^2 + 8*m^4)) + 
        J^4*(-12*En^4*(-126 + 1440*la - 925*la^2 + 18*(-7 + 24*la)*m^2) + 6*En^2*(-6255 + 61635*la - 59963*la^2 + 4953*la^3 + (-5526 + 21360*la - 4904*la^2)*m^2 + 12*(-3 + 4*la)*m^4) + 
          5*(16092 - 180378*la + 220051*la^2 - 32937*la^3 - 48*la^4 - 4*(-2592 + 13662*la - 5933*la^2 + 28*la^3)*m^2 + 6*(9 - 24*la + 4*la^2)*m^4))*M^2 + 
        9*J^2*(20400*En^4 - 4*En^2*(82821 - 77754*la + 9724*m^2) + 5*(154975 - 249896*la + 45934*la^2 - 3*(-6419 + 3496*la)*m^2 + 12*m^4))*M^4 + 7311600*M^6)*rp[t]^10 + 
      J^4*M*(-2*J^4*la*(12*En^4*(-42 + 163*la - 10*la^2 + (-42 + 34*la)*m^2) - 2*En^2*(-6498 + 21177*la - 5322*la^2 - 110*la^3 + (-5661 + 5570*la)*m^2 + 12*(-3 + la)*m^4) + 
          5*(-5742 + 19826*la - 6401*la^2 - 24*la^3 + (-3591 + 4808*la - 56*la^2)*m^2 + 6*(-3 + 2*la)*m^4)) - 
        6*J^2*(12*En^4*(1209 - 2002*la + 402*m^2) - 4*En^2*(31152 - 99580*la + 29726*la^2 + (11565 - 9914*la)*m^2 + 12*m^4) + 
          5*(47799 - 182684*la + 91883*la^2 - 4875*la^3 + (11979 - 19953*la + 2767*la^2)*m^2 - 6*(-3 + 2*la)*m^4))*M^2 + 27*(-387521 + 134280*En^2 + 217876*la - 11844*m^2)*M^4)*rp[t]^11 + 
      (2*J^8*la^2*(2*En^4*(-3*(-26 + 7*la + 5*la^2) + (50 + 8*la)*m^2) + En^2*(-2637 + 1806*la + 110*la^2 - 1982*m^2 - 12*m^4) + 5*(1083 - 796*la - 6*la^2 + (627 - 14*la)*m^2 + 3*m^4)) + 
        J^6*(24*En^4*(594 - 2607*la + 1439*la^2 + (369 - 798*la)*m^2) + 2*En^2*(-23085 + 288240*la - 294746*la^2 + 23596*la^3 + (-26442 + 94320*la - 20408*la^2)*m^2 + 24*(-3 + 4*la)*m^4) + 
          5*(19602 - 217476*la + 255835*la^2 - 35353*la^3 - 36*la^4 + (9693 - 49635*la + 21009*la^2 - 84*la^3)*m^2 + 3*(9 - 24*la + 4*la^2)*m^4))*M^2 + 
        18*J^4*(296919 + 7944*En^4 - 459592*la + 80013*la^2 - 3*(-7615 + 4076*la)*m^2 + 6*m^4 + En^2*(-181463 + 161277*la - 13312*m^2))*M^4 + 3114828*J^2*M^6)*rp[t]^12 + 
      J^2*M*(J^4*la*(24*En^4*(387 - 596*la + 50*la^2 + (246 - 130*la)*m^2) + 4*En^2*(-8172 + 33933*la - 8638*la^2 - 140*la^3 + (-8979 + 8110*la)*m^2 + 8*(-3 + la)*m^4) + 
          5*(14040 - 47071*la + 13832*la^2 + 36*la^3 + (6687 - 8676*la + 84*la^2)*m^2 - 6*(-3 + 2*la)*m^4)) + 3*J^2*(-377379 + 1152*En^6 + 1377096*la - 652214*la^2 + 31242*la^3 - 
          6*(9639 - 15724*la + 2138*la^2)*m^2 + 12*(-3 + 2*la)*m^4 - 24*En^4*(-549 - 1846*la + 530*m^2) + En^2*(280374 - 855380*la + 235652*la^2 + (65940 - 54088*la)*m^2 + 24*m^4))*M^2 + 
        9*(-508177 + 269088*En^2 + 271924*la - 6886*m^2)*M^4)*rp[t]^13 + (J^6*la^2*(24*En^4*(84 - 14*la - 5*la^2 + (43 + 2*la)*m^2) + 15*(884 - 578*la - 3*la^2 + (387 - 7*la)*m^2 + m^4) - 
          2*En^2*(3573 - 3004*la - 140*la^2 + 3098*m^2 + 8*m^4)) + J^4*(288*En^6*(-30 + 7*la) + 72*En^4*(-420 + 215*la + 553*la^2 + (195 - 352*la)*m^2) + 
          2*En^2*(-25245 + 321450*la - 303344*la^2 + 21019*la^3 - 4*(4977 - 16740*la + 3458*la^2)*m^2 + 6*(-3 + 4*la)*m^4) + 
          3*(26955 - 282828*la + 310665*la^2 - 37985*la^3 - 24*la^4 + (7974 - 39792*la + 16458*la^2 - 56*la^3)*m^2 + (9 - 24*la + 4*la^2)*m^4))*M^2 + 
        3*J^2*(806133 + 103392*En^4 - 1178112*la + 191282*la^2 + (26901 - 14176*la)*m^2 - 6*En^2*(133623 - 103094*la + 3536*m^2))*M^4 + 587088*M^6)*rp[t]^14 + 
      M*(J^4*la*(192*En^6*(-30 + la) - 24*En^4*(843 + 200*la - 90*la^2 + 6*(-65 + 29*la)*m^2) + 3*(3*(6428 - 19837*la + 4990*la^2 + 8*la^3) + 7*(783 - 988*la + 8*la^2)*m^2 + (6 - 4*la)*m^4) + 
          2*En^2*(-18261 + 73189*la - 15754*la^2 - 200*la^3 + 3*(-4489 + 3810*la)*m^2 + 4*(-3 + la)*m^4)) - 3*J^2*(179433 + 20736*En^6 - 610180*la + 266706*la^2 - 11126*la^3 + 
          2*(5763 - 9229*la + 1233*la^2)*m^2 + 12*En^4*(2865 - 6608*la + 448*m^2) + 4*En^2*(-60948 + 150546*la - 34742*la^2 + (-4515 + 3582*la)*m^2))*M^2 + 9*(-98803 + 81432*En^2 + 50012*la)*M^4)*
       rp[t]^15 + (-(J^4*la^2*(96*En^6*(11 + la) - 8*En^4*(-3*(104 + 26*la + 5*la^2) + (199 + 4*la)*m^2) - 3*(3612 - 1894*la - 6*la^2 + (947 - 14*la)*m^2 + m^4) + 
           En^2*(7944 - 5642*la - 200*la^2 + 4604*m^2 + 4*m^4))) - J^2*(-41580 + 396666*la - 395629*la^2 + 40833*la^3 + 12*la^4 + 288*En^6*(-105 + 146*la) + 
          (-4860 + 23715*la - 9611*la^2 + 28*la^3)*m^2 + 12*En^4*(630 + 6708*la - 4457*la^2 + 6*(-90 + 149*la)*m^2) + 6*En^2*(10710 - 89154*la + 65113*la^2 - 3310*la^3 + 
            2*(945 - 3048*la + 608*la^2)*m^2))*M^2 + 18*(27221 + 16416*En^4 - 37284*la + 5597*la^2 + En^2*(-44852 + 29923*la))*M^4)*rp[t]^16 + 
      2*M*(J^2*la*(14769 - 40196*la + 8108*la^2 + 6*la^3 - 96*En^6*(-105 + 38*la) + (1665 - 2051*la + 14*la^2)*m^2 - 12*En^4*(189 + 829*la - 70*la^2 + 2*(-90 + 37*la)*m^2) + 
          En^2*(-22581 + 54993*la - 7614*la^2 - 76*la^3 + (-3825 + 3106*la)*m^2)) + 3*(-19251 + 6336*En^6 + 60482*la - 24165*la^2 + 849*la^3 + 6*En^4*(-5799 + 5744*la) + 
          3*En^2*(15903 - 32366*la + 6186*la^2))*M^2)*rp[t]^17 + (J^2*la^2*(5439 - 2072*la - 3*la^2 - 96*En^6*(-33 + 2*la) + (573 - 7*la)*m^2 - 4*En^2*(2160 - 702*la - 19*la^2 + 325*m^2) + 
          En^4*(-12*(-4 + 43*la + 5*la^2) + 8*(91 + la)*m^2)) + (9720 - 83610*la + 74977*la^2 - 6271*la^3 + 864*En^6*(-15 + 29*la) + 108*En^4*(330 - 1330*la + 357*la^2) + 
          6*En^2*(-5400 + 33699*la - 19287*la^2 + 647*la^3))*M^2)*rp[t]^18 + 2*la*(3420 - 8153*la + 1255*la^2 + 288*En^6*(-15 + 7*la) + 60*En^4*(201 - 217*la + 4*la^2) - 
        4*En^2*(2790 - 4786*la + 380*la^2 + 3*la^3))*M*rp[t]^19 - 12*(-1 + En^2)*la^2*(103 - 27*la + 8*En^4*(16 + la) + En^2*(-230 + 21*la + la^2))*rp[t]^20))/
    (CapLa^2*En*la*(1 + la)*USq^7*rp[t]^27) + ((16*I)*fp^7*J^3*m*mu*Pi*(35721*J^10*M^5 - 6*J^10*(6228 - 4439*la + 156*m^2)*M^4*rp[t] + 
      3*J^8*M^3*(J^2*(4095 - 9194*la + 1859*la^2 - 4*(-66 + 53*la)*m^2) + 70755*M^2)*rp[t]^2 + 2*J^8*M^2*(J^2*(-621 + 4461*la - 2813*la^2 + 102*la^3 - 27*(3 - 10*la + 2*la^2)*m^2) + 
        3*(-37290 + 1776*En^2 + 26195*la - 732*m^2)*M^2)*rp[t]^3 + J^6*M*(J^4*la*(-3*(293 - 582*la + 56*la^2) + (-111 + 92*la)*m^2) - 
        3*J^2*(-24720 + 54766*la - 10791*la^2 + 8*(-159 + 124*la)*m^2 + 2*En^2*(945 - 1282*la + 54*m^2))*M^2 + 521550*M^4)*rp[t]^4 + 
      J^6*(J^4*la^2*(-161 + 33*la - 19*m^2) - 6*J^2*(1260 - 8953*la + 5513*la^2 - 180*la^3 + 3*(45 - 144*la + 28*la^2)*m^2 + 2*En^2*(-54 + 333*la - 128*la^2 + 9*(-1 + 2*la)*m^2))*M^2 + 
        36*(-15467 + 1680*En^2 + 10631*la - 210*m^2)*M^4)*rp[t]^5 + J^4*M*(2*J^4*la*(-2676 + 5216*la - 450*la^2 + 4*(-69 + 55*la)*m^2 + En^2*(222 - 391*la + 14*la^2 - 18*(-2 + la)*m^2)) - 
        3*J^2*(-62349 + 24*En^4 + 135308*la - 25738*la^2 + 24*(-93 + 71*la)*m^2 + 12*En^2*(917 - 1210*la + 45*m^2))*M^2 + 667710*M^4)*rp[t]^6 + 
      2*J^4*(J^4*la^2*(-490 + 90*la - 47*m^2 + En^2*(46 - 2*la + 6*m^2)) + J^2*(-9639 + 67461*la - 36*En^4*la - 40144*la^2 + 1140*la^3 - 27*(27 - 84*la + 16*la^2)*m^2 + 
          6*En^2*(324 - 1947*la + 718*la^2 + (54 - 90*la)*m^2))*M^2 + 6*(-60903 + 12042*En^2 + 40369*la - 474*m^2)*M^4)*rp[t]^7 + 
      J^2*M*(J^4*la*(-13662 + 25885*la - 24*En^4*la - 1920*la^2 + (-990 + 768*la)*m^2 + 12*En^2*(222 - 382*la + 12*la^2 + (36 - 15*la)*m^2)) - 
        3*J^2*(-84537 + 144*En^4 + 175748*la - 31778*la^2 + 8*(-213 + 160*la)*m^2 + 12*En^2*(2193 - 2864*la + 63*m^2))*M^2 + 441225*M^4)*rp[t]^8 - 
      2*J^2*(J^4*la^2*(4*En^4*(1 + la) - 4*En^2*(68 - 4*la + 9*m^2) + 3*(416 - 65*la + 28*m^2)) + J^2*(13662 - 90699*la + 216*En^4*la + 50948*la^2 - 1200*la^3 + 27*(21 - 64*la + 12*la^2)*m^2 + 
          6*En^2*(-666 + 4665*la - 1652*la^2 + 9*(-9 + 14*la)*m^2))*M^2 - 3*(-83226 + 33672*En^2 + 52703*la - 264*m^2)*M^4)*rp[t]^9 + 
      M*(J^4*la*(-144*En^4*la - 3*(6428 - 11381*la + 680*la^2) + 8*(-96 + 73*la)*m^2 + 12*En^2*(468 - 900*la + 22*la^2 + (54 - 21*la)*m^2)) - 
        3*J^2*(-60432 + 936*En^4 + 118666*la - 20211*la^2 + 4*(-120 + 89*la)*m^2 + 4*En^2*(11115 - 11738*la + 81*m^2))*M^2 + 118521*M^4)*rp[t]^10 + 
      2*(-(J^4*la^2*(1744 - 210*la + 24*En^4*(1 + la) + 65*m^2 - 18*En^2*(32 - 2*la + 3*m^2))) - 3*J^2*(3474 - 21382*la + 11185*la^2 - 210*la^3 + 12*En^4*(-60 + 29*la) + 6*(9 - 27*la + 5*la^2)*m^2 + 
          6*En^2*(-552 + 2567*la - 714*la^2 + 6*(-2 + 3*la)*m^2))*M^2 + 9*(-7720 + 5964*En^2 + 4665*la)*M^4)*rp[t]^11 + 
      M*(J^2*la*(-14595 + En^4*(2880 - 456*la) + 23629*la - 1080*la^2 + (-219 + 164*la)*m^2 - 4*En^2*(-3414 + 4184*la - 52*la^2 + 9*(-8 + 3*la)*m^2)) + 
        3*(17619 + 7824*En^4 - 32654*la + 5239*la^2 + 6*En^2*(-4543 + 4090*la))*M^2)*rp[t]^12 + (J^2*la^2*(-2603 + En^4*(408 - 72*la) + 225*la - 37*m^2 + 16*En^2*(155 - 4*la + 3*m^2)) + 
        2*(-3240 + 18510*la - 9025*la^2 + 132*la^3 + 864*En^4*(-5 + 9*la) + 180*En^2*(42 - 155*la + 36*la^2))*M^2)*rp[t]^13 + 
      la*(-4500 + 6685*la - 228*la^2 + 48*En^4*(-120 + 53*la) + 6*En^2*(1710 - 1623*la + 10*la^2))*M*rp[t]^14 - 4*la^2*(198 + 5*En^2*(-89 + la) - 12*la + 8*En^4*(31 + la))*rp[t]^15)*
     Derivative[1][rp][t])/(CapLa^2*la*(1 + la)*USq^6*rp[t]^23))
]


Clear[fSourceZM4]
fSourceZM4[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];
	
	fp=SchwarzschildF[rp[t],M];

	(8*fp^6*mu*Pi*YBar*(207*J^8*M^3 + 4*J^8*(-47 + 18*la)*M^2*rp[t] + 6*J^6*M*(J^2*(7 - 11*la) + 90*M^2)*rp[t]^2 + J^6*(15*J^2*la + 2*(-259 + 108*En^2 + 90*la)*M^2)*rp[t]^3 + 
    J^4*M*(J^2*(123 - 174*la + En^2*(-90 + 76*la)) + 270*M^2)*rp[t]^4 + 2*J^4*((21 - 16*En^2)*J^2*la + 3*(-53 + 96*En^2 + 12*la)*M^2)*rp[t]^5 + 
    2*J^2*M*(J^2*(18*En^4 - 45*(-1 + la) + En^2*(-135 + 98*la)) - 126*M^2)*rp[t]^6 + J^2*((27 - 92*En^2 + 12*En^4)*J^2*la + 2*(83 + 96*En^2 - 54*la)*M^2)*rp[t]^7 - 
    3*M*(J^2*(7 - 48*En^4 + En^2*(38 - 20*la) - 26*la) + 63*M^2)*rp[t]^8 - 2*(6*(1 + 3*En^2 - 4*En^4)*J^2*la + (-77 + 84*En^2 + 36*la)*M^2)*rp[t]^9 - 6*(-1 + En^2)*(-5 + 6*En^2 + 10*la)*M*rp[t]^10 - 
    12*(-1 + En^2)^2*la*rp[t]^11))/(CapLa*En*USq^3*rp[t]^15) + (16*fp^6*J*mu*Pi*YPhiBar*(525*J^8*M^3 + 9*J^8*(-42 + 23*la)*M^2*rp[t] + J^6*M*(J^2*(63 - 150*la + 4*la^2) + 2196*M^2)*rp[t]^2 + 
    J^6*(J^2*(25 - 2*la)*la + 6*(-263 + 40*En^2 + 146*la)*M^2)*rp[t]^3 + J^4*M*(J^2*(261 - 632*la + 20*la^2 + En^2*(-66 + 84*la)) + 3474*M^2)*rp[t]^4 + 
    2*J^4*(J^2*(52 - 12*En^2 - 5*la)*la + 3*(-417 + 152*En^2 + 233*la)*M^2)*rp[t]^5 + 6*J^2*M*(J^2*(69 - 168*la + 6*la^2 + En^2*(-41 + 56*la)) + 410*M^2)*rp[t]^6 + 
    J^2*(J^2*(165 + 4*En^2*(-23 + la) - 18*la)*la + 6*(-297 + 220*En^2 + 166*la)*M^2)*rp[t]^7 + M*(J^2*(297 - 720*la + 28*la^2 + 6*En^2*(-67 + 82*la)) + 657*M^2)*rp[t]^8 + 
    (2*J^2*la*(59 - 7*la + En^2*(-74 + 4*la)) + 3*(-160 + 216*En^2 + 89*la)*M^2)*rp[t]^9 + (81 + 144*En^4 - 194*la + 8*la^2 + 6*En^2*(-37 + 40*la))*M*rp[t]^10 + 
    4*(-1 + En^2)*la*(-8 + 12*En^2 + la)*rp[t]^11)*Derivative[1][rp][t])/(CapLa*(1 + la)*USq^4*rp[t]^16) + 
 YPhiPhiBar*((-8*fp^9*J^2*mu*Pi*(4725*J^12*M^4 - 3*J^12*(1723 - 823*la + 156*m^2)*M^3*rp[t] + 6*J^10*M^2*(J^2*(294 - 429*la + 44*la^2 + (69 - 28*la)*m^2) + 5238*M^2)*rp[t]^2 + 
      J^10*M*(2*J^2*(-90 + 407*la - 117*la^2 + 15*(-3 + 5*la)*m^2) + 9*(402*En^2 - 5*(767 - 360*la + 60*m^2))*M^2)*rp[t]^3 + 
      3*J^8*(J^4*la*(-24 + 17*la - 11*m^2) + J^2*(3945 - 5669*la + 556*la^2 + (810 - 320*la)*m^2 - 2*En^2*(367 - 307*la + 72*m^2))*M^2 + 29535*M^4)*rp[t]^4 + 
      J^8*M*(J^2*(2*En^2*(135 - 511*la + 98*la^2 + (90 - 76*la)*m^2) + 3*(-405 + 1809*la - 496*la^2 + 10*(-18 + 29*la)*m^2)) + 9*(-10860 + 2438*En^2 + 4985*la - 680*m^2)*M^2)*rp[t]^5 + 
      J^6*(J^4*la*(-486 + 327*la - 195*m^2 + 2*En^2*(51 - 40*la + 32*m^2)) + 3*J^2*(180*En^4 + En^2*(-4526 + 3698*la - 816*m^2) + 5*(2244 - 3167*la + 292*la^2 + (372 - 144*la)*m^2))*M^2 + 
        137160*M^4)*rp[t]^6 + J^6*M*(J^2*(-36*En^4*(2 - 7*la + 2*m^2) + 2*En^2*(855 - 3145*la + 578*la^2 + (540 - 424*la)*m^2) + 15*(-231 + 1022*la - 262*la^2 + 12*(-7 + 11*la)*m^2)) + 
        6*(8958*En^2 - 5*(5099 - 2256*la + 228*m^2))*M^2)*rp[t]^7 + (J^8*la*(-24*En^4*(1 - la + m^2) - 15*(93 - 58*la + 30*m^2) + En^2*(642 - 488*la + 376*m^2)) + 
        6*J^6*(540*En^4 + En^2*(-5472 + 4486*la - 792*m^2) + 5*(1782 - 2423*la + 204*la^2 + (210 - 80*la)*m^2))*M^2 + 123255*J^4*M^4)*rp[t]^8 + 
      J^4*M*(J^2*(-72*En^4*(7 - 22*la + 7*m^2) - 6*En^2*(-645 + 2560*la - 444*la^2 + 8*(-45 + 34*la)*m^2) + 5*(-1125 + 4792*la - 1104*la^2 + 12*(-24 + 37*la)*m^2)) + 
        9*(-15565 + 8428*En^2 + 6535*la - 420*m^2)*M^2)*rp[t]^9 + 3*(J^6*la*(-56*En^4*(1 - la + m^2) - 5*(151 - 82*la + 34*m^2) + En^2*(506 - 384*la + 248*m^2)) + 
        2*J^4*(8385 + 720*En^4 - 10750*la + 800*la^2 + (585 - 220*la)*m^2 + En^2*(-8188 + 5994*la - 648*m^2))*M^2 + 20220*J^2*M^4)*rp[t]^10 + 
      J^2*M*(J^2*(En^4*(648 + 2616*la - 792*m^2) + 15*(-369 + 1464*la - 290*la^2 + (-54 + 82*la)*m^2) + En^2*(6570 - 21920*la + 3016*la^2 - 8*(-225 + 166*la)*m^2)) + 
        9*(-7851 + 6682*En^2 + 3096*la - 92*m^2)*M^2)*rp[t]^11 + (J^4*la*(-24*En^4*(-7 - 15*la + 11*m^2) - 15*(147 - 65*la + 19*m^2) + 2*En^2*(1281 - 664*la + 308*m^2)) + 
        3*J^2*(8763 + 3720*En^4 - 10453*la + 668*la^2 + (258 - 96*la)*m^2 - 2*En^2*(7177 - 4391*la + 192*m^2))*M^2 + 12663*M^4)*rp[t]^12 + 
      M*(-(J^2*(1440*En^6 + 24*En^4*(105 - 202*la + 15*m^2) + 3*(1020 - 3703*la + 608*la^2 + (60 - 90*la)*m^2) + 2*En^2*(-3510 + 8905*la - 842*la^2 + 2*(-135 + 98*la)*m^2))) + 
        15*(-1010 + 1314*En^2 + 373*la)*M^2)*rp[t]^13 + (J^2*la*(-1197 - 480*En^6 + 411*la - 63*m^2 - 24*En^4*(39 - 13*la + 5*m^2) + 4*En^2*(657 - 188*la + 46*m^2)) + 
        3*(1950 + 3180*En^4 - 2159*la + 116*la^2 + 18*En^2*(-285 + 149*la))*M^2)*rp[t]^14 + 2*(-360 + 720*En^6 + 1196*la - 159*la^2 + 18*En^4*(-100 + 99*la) + 3*En^2*(480 - 997*la + 62*la^2))*M*
       rp[t]^15 + 12*(-1 + En^2)*(23 + 40*En^4 + 8*En^2*(-8 + la) - 6*la)*la*rp[t]^16))/(CapLa*En*la*(1 + la)*USq^6*rp[t]^22) - 
   ((16*I)*fp^7*J^3*m*mu*Pi*(861*J^8*M^3 - 3*J^8*(214 - 117*la + 4*m^2)*M^2*rp[t] + J^6*M*(J^2*(3*(37 - 86*la + 4*la^2) + (6 - 4*la)*m^2) + 4164*M^2)*rp[t]^2 + 
      J^6*(J^2*la*(43 - 6*la + 2*m^2) + 12*(-263 + 40*En^2 + 139*la - 3*m^2)*M^2)*rp[t]^3 + 2*J^4*M*(J^2*(279 - 626*la + 24*la^2 + (9 - 6*la)*m^2 - 6*En^2*(11 - 16*la + m^2)) + 3933*M^2)*rp[t]^4 + 
      2*J^4*(J^2*la*(3*(36 - 4*la + m^2) - 2*En^2*(12 - 2*la + m^2)) + 9*(120*En^2 + 171*(-2 + la) - 2*m^2)*M^2)*rp[t]^5 + 
      3*J^2*M*(J^2*(381 - 796*la + 24*la^2 + (6 - 4*la)*m^2 - 8*En^2*(26 - 35*la + m^2)) + 2228*M^2)*rp[t]^6 + 
      J^2*(J^2*la*(435 - 36*la + 6*m^2 - 8*En^2*(29 - 3*la + m^2)) + 12*(-451 + 360*En^2 + 213*la - m^2)*M^2)*rp[t]^7 + 
      M*(-2*J^2*(-6*(88 - 171*la + 4*la^2) + (-3 + 2*la)*m^2 + 6*En^2*(131 - 132*la + m^2)) + 2121*M^2)*rp[t]^8 + 
      (2*J^2*la*(197 - 12*la + m^2 - 2*En^2*(140 - 6*la + m^2)) + 15*(-118 + 176*En^2 + 53*la)*M^2)*rp[t]^9 + 2*(180 + 360*En^4 - 329*la + 6*la^2 + 36*En^2*(-15 + 13*la))*M*rp[t]^10 + 
      2*(66 + 120*En^4 + 4*En^2*(-47 + la) - 3*la)*la*rp[t]^11)*Derivative[1][rp][t])/(CapLa*la*(1 + la)*USq^5*rp[t]^18))
]


Clear[gSourceZM5]
gSourceZM5[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];
	
	fp=SchwarzschildF[rp[t],M];
	
	(8*fp^9*J*mu*Pi*YPhiBar*(1058400*J^16*M^7 + 144*J^16*(-13848 + 5303*la)*M^6*rp[t] + 12*J^14*M^5*(J^2*(122061 - 119891*la + 12164*la^2) + 698544*M^2)*rp[t]^2 + 
    2*J^14*M^4*(J^2*(-260751 + 528293*la - 136766*la^2 + 1354*la^3) + 36*(-218699 + 19695*En^2 + 84429*la)*M^2)*rp[t]^3 + 
    2*J^12*M^3*(J^4*(44673 - 188040*la + 99773*la^2 - 2242*la^3 + 4*la^4) + 3*J^2*(3*En^2*(-110146 + 53751*la) + 2*(960350 - 950945*la + 98149*la^2))*M^2 + 14521248*M^4)*rp[t]^4 + 
    J^12*M^2*(J^4*(-5832 + 64409*la - 70442*la^2 + 2729*la^3 - 12*la^4) + 2*J^2*(-2042379 + 4171823*la - 1097888*la^2 + 12646*la^3 + En^2*(498972 - 678690*la + 80967*la^2))*M^2 + 
      504*(-107901 + 20784*En^2 + 42019*la)*M^4)*rp[t]^5 + J^10*M*(3*J^6*la*(-1401 + 3981*la - 240*la^2 + 2*la^3) + 
      J^4*(695928 - 2953709*la + 1591727*la^2 - 41748*la^3 + 80*la^4 - 6*En^2*(35346 - 114471*la + 38255*la^2 + 348*la^3))*M^2 + 
      6*J^2*(6607432 + 81360*En^4 - 6600574*la + 693714*la^2 + 3*En^2*(-812508 + 399823*la))*M^4 + 57492288*M^6)*rp[t]^6 + 
    J^10*(-(J^6*la^2*(770 - 69*la + la^2)) + J^4*(-45108 + 502452*la - 557477*la^2 + 25299*la^3 - 120*la^4 - 4*En^2*(-3951 + 36782*la - 29533*la^2 - 670*la^3 + 12*la^4))*M^2 + 
      6*J^2*(-2330561 + 4803415*la - 1286530*la^2 + 17014*la^3 + 6*En^4*(-12249 + 8879*la) + En^2*(1224436 - 1677705*la + 205135*la^2))*M^4 + 72*(-1490115 + 464307*En^2 + 585749*la)*M^6)*rp[t]^7 + 
    J^8*M*(J^6*la*(-32511 + 93587*la - 6634*la^2 + 60*la^3 + 2*En^2*(5547 - 12997*la - 547*la^2 + 24*la^3)) + 
      J^4*(2367630 - 10142447*la + 5558751*la^2 - 168180*la^3 + 344*la^4 + 12*En^4*(10230 - 24287*la + 4002*la^2) - 6*En^2*(259482 - 845620*la + 288401*la^2 + 1534*la^3))*M^2 + 
      6*J^2*(562032*En^4 + 69*En^2*(-112586 + 55933*la) + 2*(6490074 - 6545965*la + 701293*la^2))*M^4 + 71124480*M^6)*rp[t]^8 + 
    J^8*(2*J^6*la^2*(En^2*(1020 + 69*la - 6*la^2) - 5*(596 - 63*la + la^2)) - J^4*(152244 - 1712419*la + 1930512*la^2 - 101629*la^3 + 516*la^4 + 4*En^4*(2556 - 20610*la + 11554*la^2 + 581*la^3) + 
        2*En^2*(-57798 + 540829*la - 440963*la^2 - 6471*la^3 + 176*la^4))*M^2 + 2*J^2*(-13660899 + 16200*En^6 + 28437739*la - 7761844*la^2 + 116606*la^3 + 18*En^4*(-84983 + 61758*la) + 
        3*En^2*(3893982 - 5381524*la + 676433*la^2))*M^4 + 360*(-367343 + 166416*En^2 + 145869*la)*M^6)*rp[t]^9 + 
    J^6*M*(J^6*la*(-109743 + 320591*la - 26536*la^2 + 258*la^3 - 8*En^4*(879 - 1784*la - 216*la^2 + 3*la^3) + 2*En^2*(40557 - 95967*la - 2857*la^2 + 176*la^3)) + 
      J^4*(4593804 - 19886409*la + 11102159*la^2 - 383860*la^3 + 832*la^4 + 72*En^6*(-182 + 277*la) + 48*En^4*(17805 - 42305*la + 7127*la^2) - 6*En^2*(822774 - 2700980*la + 943175*la^2 + 996*la^3))*
       M^2 + 6*J^2*(1647072*En^4 + 3*En^2*(-4635256 + 2327939*la) + 10*(1592522 - 1623245*la + 177469*la^2))*M^4 + 56306880*M^6)*rp[t]^10 + 
    J^6*(J^6*la^2*(-20134 + 2503*la - 43*la^2 + En^2*(14900 + 772*la - 88*la^2) + 12*En^4*(-115 - 24*la + la^2)) + 
      J^4*(-292680 + 3329458*la - 3821095*la^2 + 231607*la^3 - 1248*la^4 + 24*En^6*(42 - 347*la + 104*la^2) - 4*En^4*(17793 - 143742*la + 81493*la^2 + 3482*la^3) - 
        4*En^2*(-91341 + 859059*la - 713405*la^2 - 3775*la^3 + 268*la^4))*M^2 + 2*J^2*(115128*En^6 + 90*En^4*(-50108 + 36489*la) + 3*En^2*(6952682 - 9711515*la + 1258683*la^2) + 
        5*(-3331989 + 7013663*la - 1953566*la^2 + 33046*la^3))*M^4 + 72*(-1448361 + 908205*En^2 + 581471*la)*M^6)*rp[t]^11 + 
    J^4*M*(J^6*la*(-211023 + 627003*la - 60318*la^2 + 624*la^3 - 8*En^6*(-84 + 187*la + 49*la^2) - 8*En^4*(6120 - 12449*la - 1329*la^2 + 24*la^3) + 
        4*En^2*(64059 - 153296*la - 2272*la^2 + 268*la^3)) + J^4*(72*En^6*(-1330 + 1979*la) + 12*En^4*(212478 - 500729*la + 86530*la^2) + 6*En^2*(-1462116 + 4851547*la - 1742049*la^2 + 6498*la^3) + 
        5*(1111566 - 4869911*la + 2774351*la^2 - 108780*la^3 + 248*la^4))*M^2 + 6*J^2*(2639808*En^4 + 3*En^2*(-5061454 + 2569061*la) + 2*(6247186 - 6441811*la + 719559*la^2))*M^4 + 27857088*M^6)*
     rp[t]^12 + J^4*(4*J^6*la^2*(-9688 + 1416*la - 26*la^2 + En^6*(50 + 26*la) + En^2*(11756 + 379*la - 67*la^2) + En^4*(-2380 - 451*la + 24*la^2)) + 
      J^4*(24*En^6*(294 - 2555*la + 758*la^2) - 12*En^4*(18351 - 142785*la + 81287*la^2 + 2813*la^3) - 5*(70056 - 807639*la + 945710*la^2 - 65607*la^3 + 372*la^4) - 
        2*En^2*(-322038 + 3063254*la - 2606427*la^2 + 15331*la^3 + 880*la^4))*M^2 + 6*J^2*(-4327747 + 112032*En^6 + 9223791*la - 2625832*la^2 + 49638*la^3 + 12*En^4*(-199777 + 148358*la) + 
        En^2*(7583648 - 10707190*la + 1431847*la^2))*M^4 + 72*(-713529 + 606480*En^2 + 289879*la)*M^6)*rp[t]^13 + 
    J^2*M*(J^6*la*(-252645 + 765685*la - 85340*la^2 + 930*la^3 - 8*En^6*(-588 + 1345*la + 307*la^2) - 8*En^4*(18861 - 36921*la - 3470*la^2 + 73*la^3) + 
        4*En^2*(112956 - 275705*la + 1224*la^2 + 440*la^3)) + J^4*(4291776 - 19063735*la + 11104581*la^2 - 490524*la^3 + 1168*la^4 + 144*En^6*(-2290 + 2853*la) + 
        96*En^4*(41538 - 100919*la + 18399*la^2) + 6*En^2*(-1588374 + 5332543*la - 1972261*la^2 + 17452*la^3))*M^2 + 
      6*J^2*(6121080 + 2557872*En^4 - 6392650*la + 730438*la^2 + 39*En^2*(-261004 + 133505*la))*M^4 + 7874496*M^6)*rp[t]^14 + 
    J^2*(J^6*la^2*(En^2*(82924 + 344*la - 440*la^2) - 8*En^6*(-161 - 75*la + 2*la^2) - 5*(9290 - 1599*la + 31*la^2) + 4*En^4*(-7242 - 1304*la + 73*la^2)) + 
      J^4*(-267012 + 3127664*la - 3746547*la^2 + 296009*la^3 - 1752*la^4 + 48*En^6*(999 - 4281*la + 1034*la^2) - 4*En^4*(81711 - 674070*la + 405214*la^2 + 8900*la^3) - 
        4*En^2*(-173196 + 1671958*la - 1462339*la^2 + 27152*la^3 + 420*la^4))*M^2 + 2*J^2*(-6312729 + 387936*En^6 + 13644023*la - 3976298*la^2 + 83446*la^3 + 18*En^4*(-391757 + 293313*la) + 
        3*En^2*(5100936 - 7252975*la + 997517*la^2))*M^4 + 72*(-200777 + 230193*En^2 + 82623*la)*M^6)*rp[t]^15 + 
    M*(J^6*la*(-192693 + 597793*la - 77030*la^2 + 876*la^3 - 48*En^6*(-657 + 669*la + 155*la^2) - 8*En^4*(28107 - 58808*la - 4082*la^2 + 112*la^3) + 
        2*En^2*(243243 - 609205*la + 17237*la^2 + 840*la^3)) + J^4*(2064738 + 6912*En^8 - 9318909*la + 5562517*la^2 - 275228*la^3 + 680*la^4 + 144*En^6*(-1258 + 3347*la) + 
        12*En^4*(326154 - 806057*la + 154166*la^2) + 6*En^2*(-1070202 + 3617738*la - 1373579*la^2 + 19110*la^3))*M^2 + 
      6*J^2*(1462608*En^4 + 3*En^2*(-1299402 + 666089*la) + 2*(855926 - 906527*la + 106087*la^2))*M^4 + 973728*M^6)*rp[t]^16 + 
    (-2*J^6*la^2*(17740 - 3607*la + 73*la^2 + En^4*(21778 + 3464*la - 224*la^2) + 8*En^6*(-416 - 151*la + 4*la^2) + 3*En^2*(-14886 + 529*la + 70*la^2)) + 
      J^4*(-126468 + 1510157*la - 1856252*la^2 + 166319*la^3 - 1020*la^4 + 576*En^8*(-30 + 7*la) + 48*En^6*(-519 - 2179*la + 1268*la^2) - 4*En^4*(77085 - 671070*la + 424336*la^2 + 2171*la^3) - 
        2*En^2*(-232740 + 2265813*la - 2029513*la^2 + 64351*la^3 + 464*la^4))*M^2 + 2*J^2*(-1750473 + 509976*En^6 + 3843545*la - 1148660*la^2 + 26602*la^3 + 18*En^4*(-235591 + 170742*la) + 
        3*En^2*(1974094 - 2805440*la + 393947*la^2))*M^4 + 72*(38352*En^2 + 61*(-405 + 169*la))*M^6)*rp[t]^17 + 
    M*(J^4*la*(-91341 + 384*En^8*(-30 + la) + 291357*la - 43344*la^2 + 510*la^3 - 16*En^6*(1047 + 953*la + 563*la^2) - 8*En^4*(26730 - 60188*la - 1476*la^2 + 93*la^3) + 
        En^2*(327138 - 837014*la + 45902*la^2 + 928*la^3)) + J^2*(565500 - 124416*En^8 - 2600147*la + 1594301*la^2 - 87900*la^3 + 224*la^4 + 72*En^6*(-5782 + 9521*la) + 
        48*En^4*(52647 - 123149*la + 23587*la^2) + 6*En^2*(-419346 + 1411558*la - 545317*la^2 + 10068*la^3))*M^2 + 6*(209218 + 378144*En^4 - 225040*la + 27006*la^2 + En^2*(-658992 + 336099*la))*M^4)*
     rp[t]^18 + (-(J^4*la^2*(192*En^8*(11 + la) + En^4*(41440 + 3208*la - 372*la^2) + 16*En^6*(34 - 215*la + 6*la^2) + 5*(3370 - 813*la + 17*la^2) + 4*En^2*(-14996 + 1277*la + 58*la^2))) + 
      J^2*(-33984 + 415434*la - 525857*la^2 + 53229*la^3 - 336*la^4 - 576*En^8*(-105 + 146*la) + 24*En^6*(456 - 11675*la + 4740*la^2) + 12*En^4*(-18540 + 146100*la - 90111*la^2 + 842*la^3) - 
        4*En^2*(-46260 + 446165*la - 404483*la^2 + 17709*la^3 + 68*la^4))*M^2 + 6*(119736*En^6 + 6*En^4*(-65582 + 44579*la) + En^2*(339994 - 478189*la + 67881*la^2) + 
        4*(-17657 + 39467*la - 12117*la^2 + 308*la^3))*M^4)*rp[t]^19 + 
    M*(J^2*la*(-24573 + 81041*la - 13906*la^2 + 168*la^3 - 384*En^8*(-105 + 38*la) - 8*En^6*(-1038 + 6295*la + 445*la^2) - 8*En^4*(19257 - 40095*la + 763*la^2 + 40*la^3) + 
        4*En^2*(32490 - 83451*la + 6695*la^2 + 68*la^3)) + (67464 + 76032*En^8 - 317009*la + 200199*la^2 - 12240*la^3 + 32*la^4 + 72*En^6*(-6698 + 6831*la) + 60*En^4*(13050 - 27619*la + 5070*la^2) + 
        6*En^2*(-74040 + 244647*la - 94979*la^2 + 2102*la^3))*M^2)*rp[t]^20 + 
    2*(-2*(-1 + En^2)*J^2*la^2*(-1136 + 327*la - 7*la^2 + 48*En^6*(-33 + 2*la) + En^2*(4793 - 481*la - 24*la^2) + 2*En^4*(-1199 - 141*la + 8*la^2)) + 
      (-1980 + 24914*la - 32610*la^2 + 3716*la^3 - 24*la^4 + 864*En^8*(-15 + 29*la) + 108*En^6*(360 - 1519*la + 402*la^2) + 6*En^4*(-6810 + 45359*la - 25763*la^2 + 445*la^3) + 
        En^2*(16920 - 158032*la + 142617*la^2 - 7621*la^3 - 16*la^4))*M^2)*rp[t]^21 + 4*(-1 + En^2)*la*(717 - 2462*la + 487*la^2 - 6*la^3 + 288*En^6*(-15 + 7*la) + 18*En^4*(490 - 693*la + la^2) - 
      2*En^2*(2604 - 6222*la + 505*la^2 + 7*la^3))*M*rp[t]^22 - 4*(-1 + En^2)^2*la^2*(133 - 46*la + la^2 + 48*En^4*(16 + la) + En^2*(-806 + 98*la + 4*la^2))*rp[t]^23))/
  (CapLa^2*En^2*(1 + la)*USq^7*rp[t]^30) + (8*fp^7*mu*Pi*YBar*(145800*J^14*M^6 + 6*J^14*(-38031 + 16892*la)*M^5*rp[t] + 6*J^12*M^4*(J^2*(21714 - 26516*la + 2977*la^2) + 138630*M^2)*rp[t]^2 + 
    J^12*M^3*(J^2*(-31938 + 91129*la - 28109*la^2 + 72*la^3) + 6*(-218454 + 40341*En^2 + 96083*la)*M^2)*rp[t]^3 + 
    J^10*M^2*(J^4*(2808 - 22414*la + 16135*la^2 - 104*la^3) + 3*J^2*(251156 - 303797*la + 33684*la^2 + En^2*(-84684 + 55466*la))*M^2 + 1914480*M^4)*rp[t]^4 + 
    J^10*M*(J^4*la*(1977 - 3973*la + 50*la^2) + 6*J^2*(-30990 + 87622*la - 26711*la^2 + 56*la^3 + En^2*(14010 - 29153*la + 4823*la^2))*M^2 + 18*(-169567 + 74952*En^2 + 73298*la)*M^4)*rp[t]^5 + 
    2*J^8*(J^6*(175 - 4*la)*la^2 + J^4*(8226 - 65101*la + 46356*la^2 - 254*la^3 + En^2*(-4302 + 29000*la - 15247*la^2 + 12*la^3))*M^2 + 
      3*J^2*(295830 + 8736*En^4 - 351949*la + 38099*la^2 + 6*En^2*(-39804 + 25835*la))*M^4 + 1096254*M^6)*rp[t]^6 + 
    J^8*M*(J^4*la*(11559 - 23001*la + 256*la^2 + 18*En^2*(-331 + 565*la)) + J^2*(-443052 + 1233171*la - 367735*la^2 + 352*la^3 + 36*En^4*(-787 + 974*la) + 18*En^2*(26673 - 54934*la + 9056*la^2))*
       M^2 + 6*(-596088 + 505161*En^2 + 248681*la)*M^4)*rp[t]^7 + J^6*(-(J^6*la^2*(-2040 + 43*la + 2*En^2*(530 + 3*la))) + 
      J^4*(39636 - 309117*la + 215771*la^2 - 672*la^3 + 12*En^4*(276 - 1583*la + 482*la^2) + 12*En^2*(-4149 + 27647*la - 14445*la^2 + 40*la^3))*M^2 + 
      3*J^2*(708960 + 96480*En^4 - 816155*la + 83800*la^2 + 6*En^2*(-182158 + 116369*la))*M^4 + 1156104*M^6)*rp[t]^8 + 
    2*J^6*M*(J^4*la*(13881 - 27139*la + 204*la^2 - 6*En^4*(-187 + 273*la + 10*la^2) - 6*En^2*(2874 - 4859*la + 24*la^2)) - 
      J^2*(271284 + 108*En^6 + En^4*(81054 - 97272*la) - 732508*la + 208474*la^2 + 400*la^3 - 3*En^2*(186576 - 377907*la + 61643*la^2))*M^2 + 3*(-334761 + 565680*En^2 + 126032*la)*M^4)*rp[t]^9 + 
    2*J^4*(2*J^6*la^2*(1219 + 6*En^2*(-255 + la) - 20*la + 2*En^4*(55 + 7*la)) + J^4*(-108*En^6*la + 12*En^4*(828 - 4542*la + 1351*la^2) + 2*(12393 - 94001*la + 63003*la^2 + 170*la^3) + 
        En^2*(-59022 + 387441*la - 199817*la^2 + 968*la^3))*M^2 + 3*J^2*(210758 + 108576*En^4 - 222346*la + 19223*la^2 + 4*En^2*(-158232 + 97801*la))*M^4 + 21582*M^6)*rp[t]^10 + 
    J^4*M*(-2*J^4*la*(-17271 + 32613*la + 36*En^6*la + 40*la^2 + 6*En^4*(-1122 + 1563*la + 44*la^2) + En^2*(40926 - 68238*la + 716*la^2)) - 
      3*J^2*(432*En^6 - 96*En^4*(-1289 + 1537*la) - 8*En^2*(56166 - 109445*la + 17344*la^2) + 3*(37762 - 94639*la + 23507*la^2 + 280*la^3))*M^2 + 18*(-12202 + 98277*En^2 - 521*la)*M^4)*rp[t]^11 + 
    (2*J^8*la^2*(3008 - 15*la - 12*En^6*(1 + la) + 6*En^4*(215 + 23*la) + 2*En^2*(-3617 + 58*la)) + J^6*(-1296*En^6*la + 24*En^4*(1725 - 10535*la + 3154*la^2) + 
        5*(6516 - 46364*la + 27913*la^2 + 584*la^3) + 4*En^2*(-37215 + 233147*la - 115962*la^2 + 764*la^3))*M^2 + 3*J^4*(72036 + 268896*En^4 - 32843*la - 5708*la^2 + 18*En^2*(-39002 + 22587*la))*
       M^4 - 214272*J^2*M^6)*rp[t]^12 + J^2*M*(-(J^4*la*(-22461 + 38993*la + 432*En^6*la + 1030*la^2 + 8*En^4*(-3567 + 5512*la + 64*la^2) + 4*En^2*(25758 - 40987*la + 620*la^2))) - 
      2*J^2*(38142 + 5184*En^6 - 62298*la - 1675*la^2 + 1384*la^3 - 72*En^4*(-3823 + 3817*la) - 9*En^2*(44520 - 80645*la + 11991*la^2))*M^2 + 18*(15239 + 10344*En^2 - 9238*la)*M^4)*rp[t]^13 - 
    2*(J^6*la^2*(-1921 + En^2*(9022 - 238*la) - 50*la + 72*En^6*(1 + la) - 16*En^4*(169 + 10*la)) + J^4*(-4428 + 24135*la - 6244*la^2 - 1698*la^3 + 72*En^6*(-111 + 52*la) - 
        24*En^4*(1770 - 7811*la + 1981*la^2) - 3*En^2*(-16020 + 92008*la - 42507*la^2 + 332*la^3))*M^2 - 3*J^2*(-19998 + 56736*En^4 + 35851*la - 6059*la^2 + 30*En^2*(-1636 + 691*la))*M^4 + 
      32670*M^6)*rp[t]^14 + M*(J^4*la*(5883 - 6821*la - 1320*la^2 - 96*En^6*(-111 + 16*la) + 48*En^4*(1207 - 1352*la + 3*la^2) - 54*En^2*(1225 - 1781*la + 32*la^2)) + 
      J^2*(19920 + 87696*En^6 - 95363*la + 47439*la^2 - 1424*la^3 + 108*En^4*(-2591 + 2142*la) + 6*En^2*(23397 - 33066*la + 3488*la^2))*M^2 - 18*(-5124 + 6255*En^2 + 2699*la)*M^4)*rp[t]^15 + 
    (J^4*la^2*(940 + 157*la + 8*En^4*(1270 + la) - 48*En^6*(-33 + 4*la) + 6*En^2*(-1900 + 61*la)) + J^2*(-864 + 16175*la - 21199*la^2 + 1792*la^3 + 1296*En^6*(-26 + 45*la) + 
        12*En^4*(4590 - 15795*la + 3338*la^2) + 4*En^2*(-5130 + 23760*la - 8335*la^2 + 76*la^3))*M^2 - 3*(15544 + 17280*En^4 - 22875*la + 3296*la^2 + 6*En^2*(-6334 + 4393*la))*M^4)*rp[t]^16 - 
    2*M*(-2*J^2*la*(-186 + 907*la - 181*la^2 + 36*En^6*(-156 + 67*la) - 4*En^2*(870 - 993*la + 19*la^2) + En^4*(9297 - 8085*la + 78*la^2)) + 
      3*(1116*En^6 + 6*En^4*(-923 + 976*la) + En^2*(6066 - 13347*la + 2407*la^2) + 4*(-411 + 1444*la - 579*la^2 + 12*la^3))*M^2)*rp[t]^17 - 
    2*(2*(-1 + En^2)*J^2*la^2*(-43 + 23*la + 12*En^4*(79 + la) + 6*En^2*(-103 + 7*la)) + (360 - 3650*la + 3470*la^2 - 184*la^3 + 36*En^6*(-30 + 61*la) + 12*En^4*(210 - 940*la + 249*la^2) + 
        En^2*(-1800 + 12769*la - 7321*la^2 + 56*la^3))*M^2)*rp[t]^18 - 4*(-1 + En^2)*la*(-132 + 358*la - 38*la^2 + 6*En^4*(-60 + 29*la) + En^2*(498 - 811*la - 16*la^2))*M*rp[t]^19 + 
    4*(-1 + En^2)^2*la^2*(5*(-5 + la) + 6*En^2*(11 + la))*rp[t]^20)*Derivative[1][rp][t])/(CapLa^2*En*USq^6*rp[t]^26) + 
 YPhiPhiBar*(((-8*I)*fp^10*J^3*m*mu*Pi*(1984500*J^16*M^7 - 108*J^16*(34843 - 13970*la + 481*m^2)*M^6*rp[t] + 3*J^14*M^5*(J^2*(925209 - 948088*la + 109704*la^2 - 4*(-7365 + 3004*la)*m^2 + 24*m^4) + 
        5692248*M^2)*rp[t]^2 + J^14*M^4*(J^2*(-988551 + 2081850*la - 610072*la^2 + 15276*la^3 + (-55503 + 61404*la - 6380*la^2)*m^2 + 12*(-9 + 4*la)*m^4) + 
        18*(-1801774 + 172935*En^2 + 718888*la - 22210*m^2)*M^2)*rp[t]^3 + 2*J^12*M^3*(J^4*(84141 - 366915*la + 218316*la^2 - 12954*la^3 + 28*la^4 + (7596 - 19302*la + 5430*la^2 - 20*la^3)*m^2 + 
          (27 - 36*la + 4*la^2)*m^4) - 3*J^2*(-3993663 + 4073436*la - 467112*la^2 + 2*(-57009 + 23072*la)*m^2 - 72*m^4 + 3*En^2*(240587 - 130080*la + 6394*m^2))*M^2 + 32401836*M^4)*rp[t]^4 + 
      J^12*M^2*(-(J^4*(10782 - 123144*la + 149231*la^2 - 16203*la^3 + 84*la^4 + (1521 - 10575*la + 6815*la^2 - 60*la^3)*m^2 + 3*(3 - 12*la + 4*la^2)*m^4)) + 
        J^2*(-8549541 + 17926908*la - 5208416*la^2 + 126336*la^3 + (-432477 + 474324*la - 48820*la^2)*m^2 + 72*(-9 + 4*la)*m^4 + 
          6*En^2*(357795 - 537503*la + 84531*la^2 - 3*(-7693 + 4408*la)*m^2 + 48*m^4))*M^2 + 18*(-6852226 + 1409847*En^2 + 2717736*la - 73414*m^2)*M^4)*rp[t]^5 + 
      J^10*M*(3*J^6*la*(-2576 + 7985*la - 1470*la^2 + 14*la^3 + (-353 + 620*la - 10*la^2)*m^2 + 2*(-1 + la)*m^4) + J^4*(1458225 - 6333846*la + 3738628*la^2 - 214688*la^3 + 456*la^4 + 
          (119286 - 300180*la + 83580*la^2 - 280*la^3)*m^2 + 12*(27 - 36*la + 4*la^2)*m^4 + 6*En^2*(-73533 + 262880*la - 113806*la^2 + 3784*la^3 - 2*(4485 - 7959*la + 1165*la^2)*m^2 + 
            16*(-3 + 2*la)*m^4))*M^2 + 6*J^2*(15223539 + 196290*En^4 - 15438340*la + 1751552*la^2 + (379338 - 152216*la)*m^2 + 180*m^4 - 12*En^2*(492827 - 264163*la + 11829*m^2))*M^4 + 
        141873768*M^6)*rp[t]^6 - J^10*(J^6*la^2*(1404 - 438*la + 7*la^2 - 5*(-37 + la)*m^2 + m^4) + J^4*(3*(31209 - 355228*la + 427412*la^2 - 44856*la^3 + 228*la^4) + 
          (12051 - 82869*la + 52805*la^2 - 420*la^3)*m^2 + 18*(3 - 12*la + 4*la^2)*m^4 - 2*En^2*(15363 - 159027*la + 161826*la^2 - 13510*la^3 - 8*la^4 + (3339 - 18555*la + 8405*la^2 - 40*la^3)*m^2 + 
            4*(9 - 24*la + 4*la^2)*m^4))*M^2 + J^2*(32672691 - 68133564*la + 19592032*la^2 - 457584*la^3 + (1449819 - 1574700*la + 160460*la^2)*m^2 - 180*(-9 + 4*la)*m^4 + 
          108*En^4*(9641 - 8045*la + 432*m^2) - 6*En^2*(2948187 - 4391407*la + 681243*la^2 + (173298 - 97536*la)*m^2 + 240*m^4))*M^4 - 18*(-15047470 + 5026659*En^2 + 5923848*la - 134930*m^2)*M^6)*
       rp[t]^7 + 2*J^8*M*(J^6*la*(-33540 + 103357*la - 18360*la^2 + 171*la^3 + (-4188 + 7265*la - 105*la^2)*m^2 + 18*(-1 + la)*m^4 + 
          En^2*(10764 - 31184*la + 5191*la^2 + 8*la^3 + (2298 - 3255*la + 40*la^2)*m^2 - 8*(-3 + 2*la)*m^4)) + J^4*(2794023 - 12074157*la + 7057670*la^2 - 389656*la^3 + 812*la^4 + 
          (201762 - 502038*la + 138230*la^2 - 420*la^3)*m^2 + 15*(27 - 36*la + 4*la^2)*m^4 + 6*En^4*(22863 - 63028*la + 15213*la^2 - 6*(-457 + 442*la)*m^2 + 12*m^4) + 
          3*En^2*(-609687 + 2161530*la - 923643*la^2 + 29540*la^3 - 2*(34251 - 59578*la + 8530*la^2)*m^2 + 80*(-3 + 2*la)*m^4))*M^2 + 
        3*J^2*(33543483 + 1515528*En^4 - 33766188*la + 3781992*la^2 + (701970 - 279200*la)*m^2 + 240*m^4 - 9*En^2*(2356871 - 1250104*la + 49170*m^2))*M^4 + 98163360*M^6)*rp[t]^8 + 
      J^8*(-(J^6*la^2*(12184 - 3658*la + 57*la^2 - 5*(-292 + 7*la)*m^2 + 6*m^4 + En^2*(-3932 + 1274*la + 4*la^2 + 20*(-40 + la)*m^2 - 8*m^4))) + 
        J^4*(-359865 + 4077492*la - 4862620*la^2 + 489732*la^3 - 2436*la^4 + 3*(-13740 + 93233*la - 58665*la^2 + 420*la^3)*m^2 - 45*(3 - 12*la + 4*la^2)*m^4 + 
          4*En^4*(-5049 + 48750*la - 38783*la^2 + 1583*la^3 - 6*(222 - 935*la + 230*la^2)*m^2 + 6*(-3 + 4*la)*m^4) + 2*En^2*(128250 - 1316877*la + 1323771*la^2 - 106330*la^3 - 40*la^4 + 
            (26010 - 141342*la + 62470*la^2 - 240*la^3)*m^2 + 20*(9 - 24*la + 4*la^2)*m^4))*M^2 + J^2*(-72257481 + 87480*En^6 + 149594796*la - 42474928*la^2 + 947904*la^3 - 
          5*(540909 - 581556*la + 58660*la^2)*m^2 + 240*(-9 + 4*la)*m^4 - 72*En^4*(113075 - 93029*la + 4704*m^2) + 18*En^2*(3548160 - 5231553*la + 797701*la^2 - 5*(-36739 + 20200*la)*m^2 + 160*m^4))*
         M^4 + 90*(-4183118 + 2048871*En^2 + 1631168*la - 29754*m^2)*M^6)*rp[t]^9 + 
      J^6*M*(J^6*la*(-257712 + 788237*la - 134040*la^2 + 1218*la^3 + (-28587 + 48880*la - 630*la^2)*m^2 + 90*(-1 + la)*m^4 + 4*En^4*(-3474 + 9729*la - 1086*la^2 - 22*la^3 + (-906 + 980*la)*m^2 + 
            4*(-3 + la)*m^4) - 20*En^2*(-8982 + 25734*la - 4123*la^2 - 4*la^3 - 4*(447 - 616*la + 6*la^2)*m^2 + 4*(-3 + 2*la)*m^4)) + 
        J^4*(12408273 - 53252958*la + 30748324*la^2 - 1618272*la^3 + 3304*la^4 - 10*(-76005 + 186882*la - 50870*la^2 + 140*la^3)*m^2 + 40*(27 - 36*la + 4*la^2)*m^4 - 
          144*En^6*(227 - 420*la + 19*m^2) + 24*En^4*(90579 - 246217*la + 58454*la^2 + (10227 - 9594*la)*m^2 + 24*m^4) + 6*En^2*(-2217843 + 7785072*la - 3272908*la^2 + 99216*la^3 - 
            10*(22335 - 37753*la + 5263*la^2)*m^2 + 160*(-3 + 2*la)*m^4))*M^2 + 12*J^2*(2511909*En^4 - 15*En^2*(1452013 - 759626*la + 24476*m^2) + 
          5*(4686102 - 4671022*la + 515044*la^2 + (77931 - 30724*la)*m^2 + 18*m^4))*M^4 + 176088600*M^6)*rp[t]^10 - 
      J^6*(J^6*la^2*(46795 - 13402*la + 203*la^2 - 35*(-142 + 3*la)*m^2 + 15*m^4 + 4*En^4*(678 - 176*la - 11*la^2 + 160*m^2 + 2*m^4) - 20*En^2*(1638 - 511*la - la^2 + (310 - 6*la)*m^2 + 2*m^4)) + 
        J^4*(2*(401265 - 4518486*la + 5327030*la^2 - 509916*la^3 + 2478*la^4) - 5*(-15714 + 105117*la - 65285*la^2 + 420*la^3)*m^2 + 60*(3 - 12*la + 4*la^2)*m^4 + 
          24*En^6*(-84 + 922*la - 461*la^2 + (-24 + 74*la)*m^2) - 8*En^4*(-20223 + 193047*la - 151091*la^2 + 6101*la^3 - 3*(1698 - 6959*la + 1650*la^2)*m^2 + 12*(-3 + 4*la)*m^4) - 
          2*En^2*(470880 - 4784166*la + 4737621*la^2 - 360750*la^3 - 72*la^4 - 5*(-17532 + 91833*la - 39307*la^2 + 120*la^3)*m^2 + 40*(9 - 24*la + 4*la^2)*m^4))*M^2 + 
        J^2*(-698760*En^6 + 36*En^4*(761759 - 615845*la + 29232*m^2) - 30*En^2*(4403574 - 6414599*la + 955691*la^2 - 36*(-5183 + 2784*la)*m^2 + 96*m^4) - 
          5*(-20306961 + 41617200*la - 11630176*la^2 + 245640*la^3 + (-605385 + 644292*la - 64340*la^2)*m^2 + 36*(-9 + 4*la)*m^4))*M^4 - 18*(-18877338 + 13070085*En^2 + 7272440*la - 98326*m^2)*M^6)*
       rp[t]^11 + 2*J^4*M*(J^6*la*(-287220 + 869789*la - 140040*la^2 + 1239*la^3 - 5*(5436 - 9155*la + 105*la^2)*m^2 + 60*(-1 + la)*m^4 - 8*En^6*(-84 + 280*la + 15*la^2 + (-24 + 17*la)*m^2) + 
          4*En^4*(-13914 + 38347*la - 4262*la^2 - 64*la^3 + (-3468 + 3610*la)*m^2 + 8*(-3 + la)*m^4) + En^2*(329580 - 931515*la + 141609*la^2 + 72*la^3 + (60060 - 79505*la + 600*la^2)*m^2 - 
            80*(-3 + 2*la)*m^4)) + J^4*(-216*En^6*(624 - 1133*la + 56*m^2) + 6*En^4*(627276 - 1658590*la + 385529*la^2 + (68076 - 59352*la)*m^2 + 72*m^4) + 
          15*En^2*(-922152 + 3210590*la - 1321425*la^2 + 37084*la^3 - 4*(19389 - 31846*la + 4326*la^2)*m^2 + 32*(-3 + 2*la)*m^4) + 
          5*(1755441 - 7455819*la + 4236646*la^2 - 210220*la^3 + 420*la^4 + (85896 - 208698*la + 56170*la^2 - 140*la^3)*m^2 + 3*(27 - 36*la + 4*la^2)*m^4))*M^2 + 
        3*J^2*(42607677 + 9212832*En^4 - 41922004*la + 4535672*la^2 + (518550 - 202688*la)*m^2 + 72*m^4 - 15*En^2*(3762017 - 1921104*la + 45294*m^2))*M^4 + 50016852*M^6)*rp[t]^12 + 
      J^4*(J^6*la^2*(-104255 + 28122*la - 413*la^2 + 25*(-377 + 7*la)*m^2 - 20*m^4 + 16*En^6*(25 + 3*la - 2*la^2 + (5 + la)*m^2) - 8*En^4*(2691 - 692*la - 32*la^2 + 605*m^2 + 4*m^4) + 
          2*En^2*(-3*(-20015 + 5941*la + 6*la^2) - 5*(-2071 + 30*la)*m^2 + 40*m^4)) - J^4*(24*En^6*(-672 + 7628*la - 3871*la^2 + 8*(-24 + 83*la)*m^2) - 
          4*En^4*(-150444 + 1336209*la - 1017311*la^2 + 39725*la^3 - 12*(3141 - 11523*la + 2530*la^2)*m^2 + 36*(-3 + 4*la)*m^4) - 
          20*En^2*(97632 - 993945*la + 968004*la^2 - 68165*la^3 - 4*la^4 + (15732 - 79434*la + 32954*la^2 - 80*la^3)*m^2 + 4*(9 - 24*la + 4*la^2)*m^4) + 
          5*(6*(38148 - 425058*la + 493223*la^2 - 44295*la^3 + 210*la^4) + (17973 - 118521*la + 72665*la^2 - 420*la^3)*m^2 + 9*(3 - 12*la + 4*la^2)*m^4))*M^2 + 
        J^2*(-93143151 + 2391120*En^6 + 188209140*la - 51564352*la^2 + 1019136*la^3 + (-2030103 + 2139324*la - 211580*la^2)*m^2 + 72*(-9 + 4*la)*m^4 - 144*En^4*(343759 - 281735*la + 11052*m^2) + 
          30*En^2*(5807055 - 8244217*la + 1184149*la^2 + (175821 - 92472*la)*m^2 + 48*m^4))*M^4 + 18*(-10813414 + 10494645*En^2 + 4104408*la - 36050*m^2)*M^6)*rp[t]^13 + 
      J^2*M*(J^6*la*(-128*En^6*(-84 + 283*la + 3*la^2 + 4*(-6 + 5*la)*m^2) + 8*En^4*(-51336 + 132728*la - 14395*la^2 - 155*la^3 + 6*(-2127 + 1970*la)*m^2 + 12*(-3 + la)*m^4) + 
          15*(5*(-10912 + 32545*la - 4884*la^2 + 42*la^3) - 5*(827 - 1372*la + 14*la^2)*m^2 + 6*(-1 + la)*m^4) - 40*En^2*(-34254 + 96323*la - 13564*la^2 - 2*la^3 + (-5370 + 6836*la - 40*la^2)*m^2 + 
            4*(-3 + 2*la)*m^4)) + J^4*(16281747 - 68079330*la + 37888812*la^2 - 1749088*la^3 + 3416*la^4 + (581562 - 1396764*la + 371860*la^2 - 840*la^3)*m^2 + 12*(27 - 36*la + 4*la^2)*m^4 - 
          288*En^6*(4313 - 5894*la + 385*m^2) + 48*En^4*(264441 - 751405*la + 174864*la^2 - 3*(-9077 + 7454*la)*m^2 + 12*m^4) + 
          30*En^2*(-1241451 + 4207200*la - 1666486*la^2 + 41576*la^3 - 2*(37371 - 59837*la + 7943*la^2)*m^2 + 16*(-3 + 2*la)*m^4))*M^2 + 
        6*J^2*(24652653 + 11319318*En^4 - 23860396*la + 2524512*la^2 + (191358 - 74184*la)*m^2 + 12*m^4 - 36*En^2*(1291526 - 635335*la + 9207*m^2))*M^4 + 32900472*M^6)*rp[t]^14 + 
      J^2*(J^6*la^2*(32*En^6*(93 + 8*la - 5*la^2 + 2*(9 + la)*m^2) - 5*(5*(5934 - 1478*la + 21*la^2) - 5*(-429 + 7*la)*m^2 + 3*m^4) + 20*En^2*(12503 - 3476*la - la^2 + (1842 - 20*la)*m^2 + 4*m^4) - 
          4*En^4*(19608 - 4895*la - 155*la^2 + 4380*m^2 + 12*m^4)) + J^4*(-1076607 + 11784204*la - 13377236*la^2 + 1109208*la^3 - 5124*la^4 + 3*(-20517 + 133445*la - 80805*la^2 + 420*la^3)*m^2 - 
          18*(3 - 12*la + 4*la^2)*m^4 + 48*En^6*(4506 - 17660*la + 6971*la^2 + (786 - 1534*la)*m^2) + 16*En^4*(-48249 + 571074*la - 461771*la^2 + 17135*la^3 - 3*(5439 - 18367*la + 3790*la^2)*m^2 + 
            6*(-3 + 4*la)*m^4) + 10*En^2*(268947 - 2666571*la + 2495604*la^2 - 154570*la^3 + 8*la^4 + (31203 - 152625*la + 61607*la^2 - 120*la^3)*m^2 + 4*(9 - 24*la + 4*la^2)*m^4))*M^2 + 
        3*J^2*(-18188667 + 562320*En^6 + 36076396*la - 9650432*la^2 + 176272*la^3 - 3*(83873 - 87556*la + 8580*la^2)*m^2 + 4*(-9 + 4*la)*m^4 - 12*En^4*(1785409 - 1363505*la + 31872*m^2) + 
          6*En^2*(8244765 - 11196111*la + 1526347*la^2 + (145238 - 75008*la)*m^2 + 16*m^4))*M^4 + 18*(-3593226 + 4871697*En^2 + 1340376*la - 5654*m^2)*M^6)*rp[t]^15 + 
      2*M*(J^6*la*(-384204 + 1119475*la - 153480*la^2 + 1281*la^3 + (-21192 + 34655*la - 315*la^2)*m^2 + 18*(-1 + la)*m^4 + 16*En^6*(4416 - 5225*la + 63*la^2 + (786 - 379*la)*m^2) + 
          8*En^4*(-33732 + 115901*la - 12910*la^2 - 100*la^3 + 2*(-5502 + 4675*la)*m^2 + 4*(-3 + la)*m^4) - 5*En^2*(-188964 + 510714*la - 62389*la^2 + 8*la^3 + (-21234 + 26141*la - 120*la^2)*m^2 + 
            8*(-3 + 2*la)*m^4)) + J^4*(4845825 + 47520*En^8 - 19828623*la + 10753154*la^2 - 455064*la^3 + 868*la^4 - 2*(-54549 + 129585*la - 34145*la^2 + 70*la^3)*m^2 + (27 - 36*la + 4*la^2)*m^4 - 
          144*En^6*(-3697 - 4685*la + 556*m^2) + 6*En^4*(1483071 - 3850640*la + 815679*la^2 + (81726 - 64332*la)*m^2 + 12*m^4) + 
          3*En^2*(-5534523 + 17738550*la - 6618901*la^2 + 139644*la^3 - 2*(94395 - 147914*la + 19250*la^2)*m^2 + 16*(-3 + 2*la)*m^4))*M^2 + 
        3*J^2*(8295369 + 8695512*En^4 - 7873060*la + 812152*la^2 - 6*(-5033 + 1936*la)*m^2 - 3*En^2*(7437563 - 3494312*la + 22186*m^2))*M^4 + 2395494*M^6)*rp[t]^16 + 
      (J^6*la^2*(-138934 + 31118*la - 427*la^2 + 5*(-1462 + 21*la)*m^2 - 6*m^4 + 32*En^6*(874 - la - 10*la^2 + (134 + 3*la)*m^2) - 16*En^4*(6888 - 2315*la - 50*la^2 + 1870*m^2 + 2*m^4) + 
          10*En^2*(34380 - 8143*la + 2*la^2 + (3626 - 30*la)*m^2 + 4*m^4)) + J^4*(-654777 + 6980844*la - 7701356*la^2 + 579108*la^3 - 2604*la^4 + 8640*En^8*(-15 + 7*la) + 
          (-23346 + 149889*la - 89705*la^2 + 420*la^3)*m^2 - 3*(3 - 12*la + 4*la^2)*m^4 - 144*En^6*(4416 - 4398*la - 2275*la^2 + 148*(-3 + 5*la)*m^2) + 
          4*En^4*(-307449 + 3174924*la - 2280661*la^2 + 65545*la^3 - 6*(8592 - 27491*la + 5430*la^2)*m^2 + 6*(-3 + 4*la)*m^4) + 
          2*En^2*(3*(427986 - 3929979*la + 3423937*la^2 - 174970*la^3 + 24*la^4) + (80694 - 384510*la + 151670*la^2 - 240*la^3)*m^2 + 4*(9 - 24*la + 4*la^2)*m^4))*M^2 + 
        3*J^2*(-6217377 + 2099880*En^6 + 12056268*la - 3137424*la^2 + 52288*la^3 + (-39993 + 41380*la - 4020*la^2)*m^2 - 24*En^4*(767767 - 512045*la + 4392*m^2) + 
          En^2*(24792804 - 31836034*la + 4079786*la^2 + (177366 - 90192*la)*m^2))*M^4 + 18*(-529520 + 992529*En^2 + 193748*la)*M^6)*rp[t]^17 + 
      M*(J^4*la*(-465840 + 8640*En^8*(-10 + la) + 1314079*la - 160920*la^2 + 1302*la^3 + (-16041 + 25880*la - 210*la^2)*m^2 + 6*(-1 + la)*m^4 + 
          192*En^6*(-2208 + 252*la + 31*la^2 + (222 - 92*la)*m^2) + 4*En^4*(-216522 + 620681*la - 51130*la^2 - 290*la^3 + (-34674 + 27860*la)*m^2 + 4*(-3 + la)*m^4) - 
          4*En^2*(3*(-149754 + 368644*la - 35801*la^2 + 12*la^3) - 8*(3423 - 4100*la + 15*la^2)*m^2 + 4*(-3 + 2*la)*m^4)) + 
        J^2*(3381291 - 1036800*En^8 - 13468074*la + 7085100*la^2 - 270752*la^3 + 504*la^4 + (34974 - 82236*la + 21460*la^2 - 40*la^3)*m^2 - 144*En^6*(19447 - 30712*la + 491*m^2) + 
          24*En^4*(766671 - 1617389*la + 290922*la^2 + (11583 - 8850*la)*m^2) - 6*En^2*(2944497 - 8784560*la + 3049688*la^2 - 52000*la^3 + (39042 - 60086*la + 7690*la^2)*m^2))*M^2 + 
        9*(826917 + 1922964*En^4 - 767792*la + 77048*la^2 + 4*En^2*(-784549 + 351646*la))*M^4)*rp[t]^18 + 
      (-(J^4*la^2*(83879 - 16398*la + 217*la^2 + 960*En^8*(16 + la) - 5*(-552 + 7*la)*m^2 + m^4 - 64*En^6*(-958 - 18*la - 5*la^2 + (112 + la)*m^2) + 
           4*En^4*(42258 - 9610*la - 145*la^2 + 5860*m^2 + 2*m^4) - 4*En^2*(80778 - 14271*la + 9*la^2 + (4660 - 30*la)*m^2 + 2*m^4))) + 
        J^2*(-5760*En^8*(-93 + 121*la) - 12*(19575 - 201791*la + 215115*la^2 - 14406*la^3 + 63*la^4) + (-3780 + 23979*la - 14195*la^2 + 60*la^3)*m^2 - 
          24*En^6*(-2880 + 82206*la - 34493*la^2 + 18*(-70 + 109*la)*m^2) - 8*En^4*(232875 - 1601913*la + 905551*la^2 - 16429*la^3 + 3*(2520 - 7779*la + 1490*la^2)*m^2) - 
          2*En^2*(-746550 + 6206352*la - 4951461*la^2 + 197530*la^3 - 40*la^4 + (-17010 + 79347*la - 30705*la^2 + 40*la^3)*m^2))*M^2 + 
        6*(-473376 + 1065420*En^6 + 894841*la - 225948*la^2 + 3394*la^3 + 6*En^4*(-560629 + 333603*la) + En^2*(2733600 - 3318529*la + 399533*la^2))*M^4)*rp[t]^19 + 
      2*M*(J^2*la*(-83196 - 59520*En^8*(-3 + la) + 225491*la - 24120*la^2 + 189*la^3 + (-1296 + 2065*la - 15*la^2)*m^2 + 24*En^6*(1140 - 8006*la + 115*la^2 + (420 - 163*la)*m^2) + 
          4*En^4*(-160326 + 293639*la - 13198*la^2 - 56*la^3 + 6*(-846 + 655*la)*m^2) + 5*En^2*(103716 - 227407*la + 16379*la^2 - 8*la^3 + (2304 - 2699*la + 8*la^2)*m^2)) + 
        (401760*En^8 + 72*En^6*(-33822 + 30151*la) + 6*En^4*(639054 - 1160574*la + 181787*la^2) + 3*En^2*(-688158 + 1915482*la - 619163*la^2 + 8276*la^3) + 
          2*(131787 - 509157*la + 259031*la^2 - 8813*la^3 + 16*la^4))*M^2)*rp[t]^20 + 
      (J^2*la^2*(-29795 - 1920*En^8*(-30 + la) + 4942*la - 63*la^2 + 5*(-89 + la)*m^2 - 8*En^4*(28725 - 2578*la - 28*la^2 + 855*m^2) + 10*En^2*(18365 - 2213*la + 2*la^2 + (391 - 2*la)*m^2) + 
          16*En^6*(1151 - 89*la - 10*la^2 + (211 + la)*m^2)) + (-37800 + 375132*la - 385015*la^2 + 22587*la^3 - 96*la^4 + 14400*En^8*(-21 + 37*la) + 24*En^6*(37800 - 137208*la + 31375*la^2) + 
          12*En^4*(-78750 + 438111*la - 207871*la^2 + 2249*la^3) + 4*En^2*(94500 - 718068*la + 526653*la^2 - 15875*la^3 + 4*la^4))*M^2)*rp[t]^21 - 
      2*la*(13320 - 34539*la + 3165*la^2 - 24*la^3 - 3360*En^8*(-30 + 13*la) - 96*En^6*(3180 - 2959*la + 9*la^2) + 4*En^2*(-32580 + 64410*la - 3331*la^2 + 2*la^3) + 
        4*En^4*(80370 - 115855*la + 2777*la^2 + 9*la^3))*M*rp[t]^22 - 4*(-1 + En^2)*la^2*(-1185 + 163*la - 2*la^2 + 240*En^6*(36 + la) + 8*En^4*(-2205 + 46*la + la^2) - 
        En^2*(-10200 + 751*la + la^2))*rp[t]^23))/(CapLa^2*En^2*la*(1 + la)*USq^8*rp[t]^32) - 
   (8*fp^10*J^2*mu*Pi*(2579850*J^18*M^7 - 27*J^18*(165929 - 82692*la + 11750*m^2)*M^6*rp[t] + 3*J^16*M^5*(J^2*(996528 - 1267718*la + 207300*la^2 + (163959 - 75968*la)*m^2 + 720*m^4) + 8067438*M^2)*
       rp[t]^2 + 3*J^16*M^4*(2*J^2*(-157968 + 412852*la - 170601*la^2 + 9441*la^3 + (-46353 + 58819*la - 7277*la^2)*m^2 + (-486 + 244*la)*m^4) + 
        3*(-4669435 + 338400*En^2 + 2316828*la - 307516*m^2)*M^2)*rp[t]^3 + 
      J^14*M^3*(J^4*(140400 - 761082*la + 636697*la^2 - 86621*la^3 + 776*la^4 + (67464 - 199065*la + 67053*la^2 - 920*la^3)*m^2 + 4*(324 - 495*la + 62*la^2)*m^4) - 
        6*J^2*(-4672842 + 5922211*la - 961326*la^2 + 4*(-179517 + 82633*la)*m^2 - 2688*m^4 + En^2*(623085 - 432171*la + 82212*m^2))*M^2 + 101463192*M^4)*rp[t]^4 + 
      J^14*M^2*(-(J^4*(7560 - 108060*la + 183338*la^2 - 48143*la^3 + 1032*la^4 + (5859 - 48084*la + 37349*la^2 - 1240*la^3)*m^2 + 21*(9 - 42*la + 16*la^2)*m^4)) + 
        3*J^2*(-2961678 + 7717660*la - 3167282*la^2 + 173114*la^3 + (-815697 + 1028000*la - 125932*la^2)*m^2 + 28*(-261 + 130*la)*m^4 + 
          2*En^2*(266142 - 515396*la + 117912*la^2 + (86259 - 58496*la)*m^2 + 960*m^4))*M^2 + 36*(-4894405 + 758034*En^2 + 2415784*la - 295237*m^2)*M^4)*rp[t]^5 + 
      J^12*M*(J^6*la*(-5472 + 23593*la - 11429*la^2 + 450*la^3 + (-4146 + 8849*la - 550*la^2)*m^2 + 3*(-43 + 50*la)*m^4) + 
        3*J^4*(438408 - 2372038*la + 1972085*la^2 - 264771*la^3 + 2392*la^4 + (198879 - 582654*la + 194308*la^2 - 2520*la^3)*m^2 + 28*(117 - 177*la + 22*la^2)*m^4 + 
          2*En^2*(-45486 + 212031*la - 134314*la^2 + 9969*la^3 + (-28455 + 61078*la - 11172*la^2)*m^2 + 24*(-34 + 27*la)*m^4))*M^2 + 
        36*J^2*(3265207 + 19575*En^4 - 4119596*la + 662914*la^2 + (461908 - 210970*la)*m^2 + 1428*m^4 - 3*En^2*(311324 - 214281*la + 38672*m^2))*M^4 + 249827544*M^6)*rp[t]^6 - 
      J^12*(2*J^6*la^2*(504 - 483*la + 32*la^2 + (370 - 40*la)*m^2 + 11*m^4) + J^4*(70740 - 1010652*la + 1705788*la^2 - 441505*la^3 + 9540*la^4 + (52119 - 424578*la + 326517*la^2 - 10220*la^3)*m^2 + 
          63*(23 - 106*la + 40*la^2)*m^4 - 2*En^2*(7425 - 103287*la + 155495*la^2 - 30835*la^3 + 76*la^4 + (8649 - 59958*la + 34521*la^2 - 700*la^3)*m^2 + 8*(63 - 207*la + 41*la^2)*m^4))*M^2 + 
        3*J^2*(12414126 - 32233116*la + 13120324*la^2 - 706440*la^3 + (3166293 - 3957248*la + 479332*la^2)*m^2 - 252*(-93 + 46*la)*m^4 + 12*En^4*(14009 - 15813*la + 3002*m^2) - 
          18*En^2*(267035 - 513425*la + 116256*la^2 + (82019 - 54840*la)*m^2 + 752*m^4))*M^4 - 36*(-12056205 + 3010938*En^2 + 5913040*la - 651963*m^2)*M^6)*rp[t]^7 + 
      J^10*M*(J^6*la*(-51192 + 219930*la - 104800*la^2 + 4158*la^3 + (-36849 + 77892*la - 4550*la^2)*m^2 + 21*(-47 + 54*la)*m^4 - 
          2*En^2*(-5202 + 22933*la - 10045*la^2 + 40*la^3 + (-5994 + 11059*la - 580*la^2)*m^2 + (-342 + 280*la)*m^4)) + 
        2*J^4*(2754945 - 14870043*la + 12271207*la^2 - 1621394*la^3 + 14756*la^4 + (1165617 - 3384543*la + 1115582*la^2 - 13580*la^3)*m^2 + 294*(54 - 81*la + 10*la^2)*m^4 + 
          18*En^4*(2805 - 10843*la + 3967*la^2 + (1619 - 2088*la)*m^2 + 40*m^4) + 9*En^2*(-137505 + 636812*la - 399382*la^2 + 29258*la^3 + (-82173 + 173786*la - 31128*la^2)*m^2 + 
            16*(-123 + 95*la)*m^4))*M^2 + 12*J^2*(24135579 + 511893*En^4 - 30282784*la + 4821938*la^2 + (3079056 - 1393426*la)*m^2 + 7560*m^4 - 9*En^2*(1242136 - 847179*la + 143426*m^2))*M^4 + 
        398917116*M^6)*rp[t]^8 + J^10*(J^6*la^2*(-9432 + 8854*la - 591*la^2 + (-6570 + 665*la)*m^2 - 168*m^4 + 2*En^2*(972 - 1024*la + la^2 - 5*(-214 + 23*la)*m^2 + 58*m^4)) + 
        J^4*(-296055 + 4226328*la - 7088639*la^2 + 1803092*la^3 - 39228*la^4 + (-205200 + 1655808*la - 1258177*la^2 + 36820*la^3)*m^2 - 63*(75 - 342*la + 128*la^2)*m^4 + 
          12*En^4*(-405 + 6180*la - 7738*la^2 + 731*la^3 - 2*(276 - 1670*la + 577*la^2)*m^2 + (-42 + 80*la)*m^4) + 6*En^2*(22545 - 311859*la + 465037*la^2 - 90941*la^3 + 276*la^4 + 
            (25335 - 172938*la + 97499*la^2 - 1780*la^3)*m^2 + 12*(105 - 332*la + 64*la^2)*m^4))*M^2 + 3*J^2*(-30588210 + 4320*En^6 + 79067444*la - 31864668*la^2 + 1684296*la^3 + 
          (-7086657 + 8767216*la - 1048444*la^2)*m^2 + 140*(-297 + 146*la)*m^4 - 36*En^4*(41225 - 45923*la + 8432*m^2) + 6*En^2*(3212697 - 6123189*la + 1369312*la^2 + (925461 - 607504*la)*m^2 + 
            6480*m^4))*M^4 + 18*(-38539249 + 13892772*En^2 + 18751628*la - 1810300*m^2)*M^6)*rp[t]^9 + 
      J^8*M*(-(J^6*la*(214200 - 916238*la + 428096*la^2 - 17094*la^3 + (144918 - 302647*la + 16450*la^2)*m^2 - 63*(-51 + 58*la)*m^4 - 
           4*En^4*(-828 + 4216*la - 1197*la^2 - 70*la^3 + (-1122 + 1851*la - 20*la^2)*m^2 + (-84 + 40*la)*m^4) + 6*En^2*(-15795 + 69012*la - 29771*la^2 + 164*la^3 + 
             (-17562 + 31751*la - 1500*la^2)*m^2 + 12*(-71 + 56*la)*m^4))) + J^4*(13571658 - 73030938*la + 59722226*la^2 - 7736836*la^3 + 70840*la^4 + 
          (5263011 - 15108954*la + 4912548*la^2 - 55720*la^3)*m^2 + 140*(405 - 603*la + 74*la^2)*m^4 + 216*En^6*(-13 + 33*la + 2*m^2) + 
          36*En^4*(25047 - 95795*la + 34555*la^2 - 4*(-3510 + 4399*la)*m^2 + 312*m^4) + 6*En^2*(-1664082 + 7641591*la - 4734926*la^2 + 341114*la^3 - 3*(313899 - 651394*la + 113644*la^2)*m^2 + 
            120*(-144 + 109*la)*m^4))*M^2 + 6*J^2*(77214954 + 3948426*En^4 - 96197894*la + 15118528*la^2 + (8612655 - 3856700*la)*m^2 + 15960*m^4 - 6*En^2*(8637668 - 5830727*la + 908720*m^2))*M^4 + 
        429525180*M^6)*rp[t]^10 + J^8*(J^6*la^2*(-39474 + 36166*la - 2429*la^2 + 15*(-1720 + 161*la)*m^2 - 546*m^4 - 4*En^4*(186 - 167*la - 32*la^2 + 220*m^2 + 14*m^4) + 
          6*En^2*(2946 - 3047*la + 13*la^2 - 5*(-626 + 61*la)*m^2 + 144*m^4)) + J^4*(-728595 + 10388928*la - 17291825*la^2 + 4304956*la^3 - 94164*la^4 + 
          (-468180 + 3729414*la - 2792749*la^2 + 75740*la^3)*m^2 - 105*(81 - 366*la + 136*la^2)*m^4 + 144*En^6*la*(2*(-6 + la) + 3*m^2) + 
          12*En^4*(-3645 + 55272*la - 68320*la^2 + 6475*la^3 + (-4968 + 29046*la - 9704*la^2)*m^2 + 6*(-63 + 104*la)*m^4) + 6*En^2*(91530 - 1256034*la + 1850999*la^2 - 355787*la^3 + 1284*la^4 + 
            (98280 - 659418*la + 361927*la^2 - 5780*la^3)*m^2 + 20*(189 - 582*la + 110*la^2)*m^4))*M^2 + 3*J^2*(-48952446 + 43200*En^6 + 125808284*la - 50070744*la^2 + 2585788*la^3 - 
          5*(1999773 - 2444540*la + 288216*la^2)*m^2 + 980*(-45 + 22*la)*m^4 - 36*En^4*(162313 - 176967*la + 31302*m^2) + 2*En^2*(22469271 - 42405985*la + 9341840*la^2 + (5990415 - 3828520*la)*m^2 + 
            29280*m^4))*M^4 + 18*(-41582401 + 20493300*En^2 + 20020588*la - 1616890*m^2)*M^6)*rp[t]^11 + 
      J^6*M*(-(J^6*la*(527040 - 2242484*la + 1022788*la^2 - 41034*la^3 + (330105 - 678694*la + 33950*la^2)*m^2 - 105*(-55 + 62*la)*m^4 + 72*En^6*la*(8 + 7*la - 2*m^2) - 
           12*En^4*(-2484 + 12508*la - 3587*la^2 - 182*la^3 + (-3366 + 5372*la - 60*la^2)*m^2 + 4*(-63 + 26*la)*m^4) + 6*En^2*(-64125 + 276800*la - 117266*la^2 + 836*la^3 + 
             (-68190 + 120243*la - 4940*la^2)*m^2 + 10*(-255 + 196*la)*m^4))) + 6*J^4*(3619242 - 19401379*la + 15684520*la^2 - 1981625*la^3 + 18228*la^4 - 
          5*(-250212 + 708420*la - 226797*la^2 + 2380*la^3)*m^2 + 70*(144 - 213*la + 26*la^2)*m^4 + 324*En^6*(-13 + 38*la + 2*m^2) + 
          6*En^4*(100188 - 376765*la + 133215*la^2 + (52471 - 65172*la)*m^2 + 848*m^4) + En^2*(-3913623 + 17767008*la - 10854730*la^2 + 764534*la^3 - 5*(417525 - 839254*la + 141384*la^2)*m^2 + 
            480*(-55 + 41*la)*m^4))*M^2 + 36*J^2*(13913999 + 1451103*En^4 - 17166629*la + 2653000*la^2 + (1292822 - 572194*la)*m^2 + 1680*m^4 - 15*En^2*(852085 - 569158*la + 78190*m^2))*M^4 + 
        312890904*M^6)*rp[t]^12 + J^6*(J^6*la^2*(-97155 + 86454*la - 5831*la^2 + (-58645 + 5005*la)*m^2 - 980*m^4 + 48*En^6*(1 + la)*(1 - la + m^2) - 
          12*En^4*(548 - 501*la - 86*la^2 - 10*(-65 + la)*m^2 + 42*m^4) + 6*En^2*(11925 - 12063*la + 97*la^2 - 25*(-485 + 41*la)*m^2 + 430*m^4)) + 
        J^4*(-1163970 + 16585032*la - 27341271*la^2 + 6623066*la^3 - 145404*la^4 + 5*(-135387 + 1060860*la - 780951*la^2 + 19460*la^3)*m^2 - 315*(29 - 130*la + 48*la^2)*m^4 + 
          432*En^6*la*(-36 + 11*la + 9*m^2) + 12*En^4*(-13500 + 220689*la - 267780*la^2 + 25643*la^3 - 2*(8526 - 54433*la + 17785*la^2)*m^2 + 4*(-273 + 424*la)*m^4) + 
          2*En^2*(659340 - 8844324*la + 12846307*la^2 - 2414423*la^3 + 9980*la^4 - 5*(-135468 + 873918*la - 460737*la^2 + 6220*la^3)*m^2 + 40*(441 - 1332*la + 248*la^2)*m^4))*M^2 + 
        3*J^2*(-53024766 + 220320*En^6 + 135125724*la - 52899152*la^2 + 2651052*la^3 + (-9096891 + 10971328*la - 1273924*la^2)*m^2 + 756*(-37 + 18*la)*m^4 - 
          12*En^4*(1127869 - 1173153*la + 212704*m^2) + 30*En^2*(2217261 - 4163305*la + 900608*la^2 + (529479 - 327520*la)*m^2 + 1632*m^4))*M^4 + 
        36*(-15207045 + 10082310*En^2 + 7217392*la - 453219*m^2)*M^6)*rp[t]^13 + 
      J^4*M*(-(J^6*la*(842040 - 3561618*la + 1575458*la^2 - 63378*la^3 + 5*(95250 - 192111*la + 8750*la^2)*m^2 - 105*(-59 + 66*la)*m^4 + 72*En^6*la*(67 + 58*la - 18*m^2) - 
           4*En^4*(-3*(9366 - 49087*la + 14401*la^2 + 578*la^3) + (-35112 + 59995*la - 600*la^2)*m^2 + 8*(-273 + 106*la)*m^4) + 2*En^2*(-460530 + 1941656*la - 804125*la^2 + 6956*la^3 - 
             5*(93870 - 157783*la + 5380*la^2)*m^2 + 40*(-297 + 224*la)*m^4))) + J^4*(2*(11774430 - 62725131*la + 49919936*la^2 - 6103063*la^3 + 56308*la^4) + 
          (6915699 - 19271550*la + 6066068*la^2 - 58520*la^3)*m^2 + 84*(459 - 675*la + 82*la^2)*m^4 + 72*En^6*(330 + 5489*la + 876*m^2) + 
          36*En^4*(267030 - 871829*la + 296339*la^2 - 8*(-16483 + 18311*la)*m^2 + 1072*m^4) + 30*En^2*(-1151082 + 5251749*la - 3168130*la^2 + 216044*la^3 - 3*(191199 - 368846*la + 59668*la^2)*m^2 + 
            24*(-186 + 137*la)*m^4))*M^2 + 12*J^2*(30670551 + 5529591*En^4 - 37308008*la + 5639978*la^2 + (2194134 - 959066*la)*m^2 + 1764*m^4 - 9*En^2*(4223940 - 2765547*la + 305504*m^2))*M^4 + 
        149216472*M^6)*rp[t]^14 + J^4*(J^6*la^2*(-155325 + 133346*la - 9009*la^2 + 25*(-3375 + 259*la)*m^2 - 1050*m^4 + 432*En^6*(1 + la)*(1 - la + m^2) - 
          8*En^4*(3048 - 2895*la - 420*la^2 + (3385 - 75*la)*m^2 + 182*m^4) + 2*En^2*(85215 - 83582*la + 983*la^2 + (82975 - 5675*la)*m^2 + 2000*m^4)) + 
        J^4*(-1261170 + 17933040*la - 29160373*la^2 + 6809878*la^3 - 149772*la^4 + (-633879 + 4871694*la - 3518519*la^2 + 79940*la^3)*m^2 - 63*(93 - 414*la + 152*la^2)*m^4 + 
          144*En^6*(-810 + 246*la + 263*la^2 + 6*(-75 + 53*la)*m^2) + 12*En^4*(-62100 + 579453*la - 620866*la^2 + 59315*la^3 - 4*(13017 - 67951*la + 19580*la^2)*m^2 + 4*(-357 + 536*la)*m^4) + 
          30*En^2*(63099 - 868677*la + 1260401*la^2 - 230137*la^3 + 1052*la^4 + (65223 - 398238*la + 199909*la^2 - 2220*la^3)*m^2 + 24*(42 - 125*la + 23*la^2)*m^4))*M^2 + 
        3*J^2*(-39159738 + 1301760*En^6 + 98439700*la - 37678012*la^2 + 1815144*la^3 + (-5203527 + 6185360*la - 706812*la^2)*m^2 + 28*(-351 + 170*la)*m^4 - 36*En^4*(443357 - 506109*la + 94202*m^2) + 
          6*En^2*(11034099 - 20446097*la + 4282944*la^2 + (2128923 - 1272472*la)*m^2 + 3600*m^4))*M^4 + 36*(-7303645 + 6616734*En^2 + 3399304*la - 145657*m^2)*M^6)*rp[t]^15 + 
      J^2*M*(-(J^6*la*(912672 - 3825178*la + 1622884*la^2 - 65310*la^3 + (444771 - 877544*la + 36050*la^2)*m^2 - 441*(-9 + 10*la)*m^4 - 24*En^6*(-3120 + 443*la - 551*la^2 + 36*(-50 + 11*la)*m^2) + 
           30*En^2*(-44136 + 191114*la - 77764*la^2 + 772*la^3 + (-45042 + 71091*la - 1940*la^2)*m^2 + 6*(-113 + 84*la)*m^4) + 4*En^4*(124362 - 374993*la + 104415*la^2 + 2930*la^3 + 
             4*(26727 - 36460*la + 250*la^2)*m^2 - 8*(-357 + 134*la)*m^4))) - 2*J^4*(-8743437 + 47520*En^8 + 46002777*la - 35788025*la^2 + 4185754*la^3 - 38668*la^4 + 
          (-2005461 + 5492841*la - 1698342*la^2 + 14980*la^3)*m^2 - 14*(486 - 711*la + 86*la^2)*m^4 + 72*En^6*(24015 - 18098*la + 1818*m^2) - 
          18*En^4*(3*(84935 - 355487*la + 131687*la^2) + (196881 - 193072*la)*m^2 + 648*m^4) - 3*En^2*(-5679441 + 26011596*la - 15279494*la^2 + 983994*la^3 - 
            3*(798315 - 1474846*la + 228904*la^2)*m^2 + 48*(-207 + 151*la)*m^4))*M^2 + 12*J^2*(14857761 + 4975479*En^4 - 17705468*la + 2600222*la^2 + (711924 - 307190*la)*m^2 + 264*m^4 - 
          3*En^2*(8514052 - 5332823*la + 398146*m^2))*M^4 + 42398154*M^6)*rp[t]^16 + 
      J^2*(J^6*la^2*(-168498 + 137666*la - 9289*la^2 + 1785*(-44 + 3*la)*m^2 - 672*m^4 + 96*En^6*(-122 + 18*la - 15*la^2 + (-62 + 13*la)*m^2) + 
          30*En^2*(8204 - 8222*la + 123*la^2 + (7900 - 415*la)*m^2 + 114*m^4) - 8*En^4*(11667 - 7115*la - 725*la^2 + (9740 - 150*la)*m^2 + 238*m^4)) + 
        J^4*(-941355 + 13258392*la - 21086653*la^2 + 4680020*la^3 - 102900*la^4 - 8640*En^8*(-15 + 7*la) + (-374274 + 2816088*la - 1992947*la^2 + 41020*la^3)*m^2 - 21*(99 - 438*la + 160*la^2)*m^4 - 
          96*En^6*(-9765 + 23439*la - 4583*la^2 + 3*(-75 + 589*la)*m^2) + 12*En^4*(-31725 + 582462*la - 810034*la^2 + 84745*la^3 - 2*(47646 - 201288*la + 50635*la^2)*m^2 + 18*(-49 + 72*la)*m^4) + 
          6*En^2*(294705 - 4294899*la + 6167655*la^2 - 1063055*la^3 + 5212*la^4 + (286983 - 1654206*la + 789697*la^2 - 7100*la^3)*m^2 + 4*(567 - 1668*la + 304*la^2)*m^4))*M^2 - 
        3*J^2*(19175286 + 793440*En^6 - 47162236*la + 17502724*la^2 - 800360*la^3 + (1708083 - 2000048*la + 224852*la^2)*m^2 + (1476 - 712*la)*m^4 + 36*En^4*(378751 - 453137*la + 64752*m^2) - 
          2*En^2*(22892385 - 40440005*la + 7961984*la^2 + (2851941 - 1649840*la)*m^2 + 1968*m^4))*M^4 + 9*(-8392315 + 10709592*En^2 + 3807100*la - 82102*m^2)*M^6)*rp[t]^17 + 
      M*(-(J^6*la*(681192 + 8640*En^8*(-10 + la) - 2800150*la + 1118120*la^2 - 44898*la^3 + (261834 - 504497*la + 18550*la^2)*m^2 - 21*(-67 + 74*la)*m^4 + 
           48*En^6*(-12930 + 7729*la + 222*la^2 + (-300 + 538*la)*m^2) - 12*En^4*(-20376 + 142294*la - 52985*la^2 - 950*la^3 + (-64734 + 70315*la - 300*la^2)*m^2 + 12*(-49 + 18*la)*m^4) + 
           6*En^2*(-207999 + 943556*la - 365705*la^2 + 3980*la^3 + (-197358 + 292113*la - 6260*la^2)*m^2 + 4*(-381 + 280*la)*m^4))) + 
        3*J^4*(429120*En^8 + 2*(1447155 - 7437221*la + 5597229*la^2 - 616478*la^3 + 5692*la^4) + (445263 - 1197882*la + 363668*la^2 - 2920*la^3)*m^2 + 12*(57 - 83*la + 10*la^2)*m^4 - 
          24*En^6*(-43725 + 18611*la + 9714*m^2) + 12*En^4*(149715 - 930037*la + 349081*la^2 - 4*(-36892 + 32979*la)*m^2 + 152*m^4) + 
          2*En^2*(-4070070 + 17710821*la - 9718266*la^2 + 563414*la^3 + (-1107879 + 1965778*la - 293484*la^2)*m^2 + 24*(-76 + 55*la)*m^4))*M^2 + 
        9*J^2*(5770372 + 4752852*En^4 - 6684522*la + 946004*la^2 + (135073 - 57528*la)*m^2 - 36*En^2*(400092 - 233013*la + 8192*m^2))*M^4 + 5477706*M^6)*rp[t]^18 + 
      (J^6*la^2*(-125772 + 95154*la - 6391*la^2 + 960*En^8*(16 + la) + 5*(-9214 + 553*la)*m^2 - 238*m^4 + 96*En^6*(1082 + 17*la - 25*la^2 + (42 + 17*la)*m^2) - 
          12*En^4*(4582 - 7745*la - 480*la^2 - 20*(-567 + 5*la)*m^2 + 98*m^4) + 6*En^2*(39164 - 39563*la + 687*la^2 - 5*(-6868 + 271*la)*m^2 + 256*m^4)) + 
        J^4*(-476415 + 6538128*la - 10032579*la^2 + 2073052*la^3 - 45468*la^4 + 2880*En^8*(-267 + 299*la) + (-126990 + 934698*la - 647847*la^2 + 12020*la^3)*m^2 - 63*(5 - 22*la + 8*la^2)*m^4 - 
          48*En^6*(2*(6435 - 21759*la + 928*la^2) + (-4950 + 9651*la)*m^2) + 12*En^4*(37395 + 383058*la - 722004*la^2 + 73001*la^3 + (-81060 + 300014*la - 68264*la^2)*m^2 + (-210 + 304*la)*m^4) + 
          2*En^2*(660420 - 9149952*la + 12184391*la^2 - 1854883*la^3 + 9508*la^4 + (417510 - 2284794*la + 1041447*la^2 - 7540*la^3)*m^2 + 4*(315 - 918*la + 166*la^2)*m^4))*M^2 - 
        3*J^2*(5688570 + 285120*En^6 - 13549644*la + 4827478*la^2 - 206226*la^3 + (245937 - 283646*la + 31374*la^2)*m^2 + 12*En^4*(1003061 - 917871*la + 52742*m^2) + 
          6*En^2*(-3433887 + 5542769*la - 990512*la^2 + (-180471 + 101384*la)*m^2))*M^4 + 27*(-366921 + 673608*En^2 + 161252*la)*M^6)*rp[t]^19 + 
      M*(J^4*la*(960*En^8*(-534 + 151*la) + 2*(-172080 + 680304*la - 248438*la^2 + 9927*la^3) + (-88563 + 166542*la - 5450*la^2)*m^2 + 3*(-71 + 78*la)*m^4 + 
          24*En^6*(-17700 + 12598*la + 549*la^2 + (6600 - 3154*la)*m^2) + 4*En^4*(73800 + 331374*la - 146127*la^2 - 1590*la^3 - 2*(82185 - 77438*la + 210*la^2)*m^2 + 4*(-105 + 38*la)*m^4) - 
          2*En^2*(-468855 + 1957936*la - 651178*la^2 + 7492*la^3 + (-285966 + 399703*la - 6700*la^2)*m^2 + (-846 + 616*la)*m^4)) + 
        J^2*(2642328 - 1978560*En^8 - 13073868*la + 9398711*la^2 - 955381*la^3 + 8800*la^4 + (195156 - 515679*la + 153733*la^2 - 1120*la^3)*m^2 - 72*En^6*(-46221 + 2950*la + 5250*m^2) + 
          36*En^4*(232842 - 763051*la + 211821*la^2 + (42501 - 35668*la)*m^2) - 6*En^2*(2016117 - 7759536*la + 3784046*la^2 - 185074*la^3 + 3*(72309 - 123854*la + 17864*la^2)*m^2))*M^2 + 
        18*(385868 + 857658*En^4 - 431213*la + 58326*la^2 + 3*En^2*(-479137 + 255713*la))*M^4)*rp[t]^20 + 
      (J^4*la^2*(-63399 + 960*En^8*(-88 + la) + 42466*la - 2829*la^2 + 5*(-3105 + 163*la)*m^2 - 36*m^4 + 48*En^6*(-1409 - 114*la - 45*la^2 + (571 + 21*la)*m^2) - 
          4*En^4*(-3*(2720 + 7729*la + 274*la^2) - 50*(-565 + 3*la)*m^2 + 70*m^4) + 2*En^2*(88023 - 72361*la + 1369*la^2 - 5*(-9883 + 293*la)*m^2 + 142*m^4)) + 
        J^2*(-150660 + 1970028*la - 2867075*la^2 + 537071*la^3 - 11724*la^4 - 77760*En^8*(-11 + 17*la) + 4*(-4725 + 34026*la - 23101*la^2 + 385*la^3)*m^2 - 
          144*En^6*(7830 - 14622*la - 949*la^2 + 5*(-210 + 349*la)*m^2) - 36*En^4*(9990 - 173823*la + 171408*la^2 - 11515*la^3 + (8400 - 28694*la + 6098*la^2)*m^2) - 
          6*En^2*(-130410 + 1476894*la - 1682859*la^2 + 206487*la^3 - 1084*la^4 + (-28350 + 148518*la - 65037*la^2 + 380*la^3)*m^2))*M^2 + 
        6*(-390438 + 814320*En^6 + 891692*la - 301967*la^2 + 11829*la^3 + 162*En^4*(-16529 + 11577*la) + 3*En^2*(740337 - 1072955*la + 170520*la^2))*M^4)*rp[t]^21 + 
      M*(-(J^2*la*(108360 - 401729*la + 129233*la^2 - 5124*la^3 + 31680*En^8*(-18 + 7*la) + 4*(3285 - 6031*la + 175*la^2)*m^2 + 24*En^6*(31380 - 11483*la - 970*la^2 + 10*(-420 + 173*la)*m^2) + 
           4*En^4*(64890 - 341401*la + 73221*la^2 + 466*la^3 + (50940 - 44041*la + 80*la^2)*m^2) - 6*En^2*(91800 - 299680*la + 74061*la^2 - 876*la^3 + (19350 - 25783*la + 340*la^2)*m^2))) + 
        3*(125424 + 169920*En^8 - 589408*la + 399755*la^2 - 36635*la^3 + 336*la^4 + 72*En^6*(-15392 + 15765*la) + 12*En^4*(148848 - 319459*la + 64089*la^2) + 
          2*En^2*(-486648 + 1625388*la - 688260*la^2 + 26659*la^3))*M^2)*rp[t]^22 + 
      (J^2*la^2*(-19845 - 960*En^8*(-98 + la) + 11104*la - 731*la^2 + 15*(-153 + 7*la)*m^2 + 6*En^2*(16765 - 8468*la + 167*la^2 + (3325 - 75*la)*m^2) + 
          8*En^4*(-6930 + 6244*la + 124*la^2 + 15*(-289 + la)*m^2) + 48*En^6*(-2485 - 186*la - 21*la^2 + 5*(71 + la)*m^2)) + 
        (-22680 + 277092*la - 376411*la^2 + 62001*la^3 - 1344*la^4 + 25920*En^8*(-7 + 13*la) + 144*En^6*(3780 - 15852*la + 4391*la^2) + 36*En^4*(-15750 + 104339*la - 62862*la^2 + 2291*la^3) + 
          12*En^2*(18900 - 173964*la + 165677*la^2 - 15123*la^3 + 80*la^4))*M^2)*rp[t]^23 - 6*la*(2700 - 9188*la + 2498*la^2 - 98*la^3 - 480*En^8*(-42 + 19*la) - 
        12*En^6*(5100 - 5731*la + 125*la^2) + 6*En^4*(10770 - 19541*la + 1697*la^2 + 6*la^3) + 3*En^2*(-8760 + 22269*la - 3695*la^2 + 44*la^3))*M*rp[t]^24 - 
      12*(-1 + En^2)*la^2*(-245 + 108*la - 7*la^2 + 80*En^6*(22 + la) + 8*En^4*(-450 + 47*la + 2*la^2) + En^2*(2090 - 544*la + 6*la^2))*rp[t]^25)*Derivative[1][rp][t])/
    (CapLa^2*En*la*(1 + la)*USq^9*rp[t]^33))
]


Clear[fSourceZM5]
fSourceZM5[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];
	
	fp=SchwarzschildF[rp[t],M];

	(-8*fp^9*J*mu*Pi*YPhiBar*(14112*J^14*M^5 + 6*J^14*(-3671 + 945*la)*M^4*rp[t] + J^12*M^3*(J^2*(12521 - 8855*la + 160*la^2) + 98784*M^2)*rp[t]^2 + 
    2*J^12*M^2*(J^2*(-1523 + 2515*la - 110*la^2) + 3*(-25641 + 6033*En^2 + 6671*la)*M^2)*rp[t]^3 + 
    J^10*M*(4*J^4*(66 - 305*la + 25*la^2) + J^2*(87203 - 62309*la + 1240*la^2 + 30*En^2*(-1317 + 445*la))*M^2 + 296352*M^4)*rp[t]^4 + 
    J^10*(-15*J^4*(-7 + la)*la + 2*J^2*(-10565 + 17621*la - 850*la^2 + En^2*(6822 - 7320*la + 60*la^2))*M^2 + 18*(-25581 + 12816*En^2 + 6731*la)*M^4)*rp[t]^5 + 
    J^8*M*(J^4*(1821 - 8497*la + 770*la^2 - 2*En^2*(729 - 2545*la + 50*la^2)) + 3*J^2*(86723 + 6108*En^4 - 62661*la + 1368*la^2 + En^2*(-83882 + 28768*la))*M^2 + 493920*M^4)*rp[t]^6 + 
    J^8*(5*J^4*la*(145 - 23*la + 2*En^2*(-55 + 2*la)) + 6*J^2*(2*En^4*(-949 + 525*la) + En^2*(14465 - 15709*la + 200*la^2) - 4*(2615 - 4410*la + 234*la^2))*M^2 + 
      30*(-25517 + 20577*En^2 + 6795*la)*M^4)*rp[t]^7 + J^6*M*(J^4*(-4*En^4*(-387 + 995*la + 10*la^2) + 27*(199 - 939*la + 94*la^2) - 4*En^2*(2313 - 8144*la + 250*la^2)) + 
      J^2*(107100*En^4 + 6*En^2*(-112142 + 39133*la) + 5*(86207 - 63041*la + 1504*la^2))*M^2 + 493920*M^4)*rp[t]^8 + 
    2*J^6*(J^4*la*(63*(17 - 3*la) + 10*En^4*(28 + la) + 5*En^2*(-349 + 20*la)) + J^2*(972*En^6 + 6*En^4*(-5609 + 3111*la) - 10*(5173 - 8831*la + 514*la^2) + 3*En^2*(38572 - 42577*la + 756*la^2))*
       M^2 + 15*(-25449 + 29712*En^2 + 6863*la)*M^4)*rp[t]^9 + J^4*M*(2*J^4*(12*En^6*(-14 + 25*la) + En^2*(-12249 + 43838*la - 1918*la^2) - 2*En^4*(-2331 + 5909*la + 10*la^2) + 
        5*(879 - 4201*la + 464*la^2)) + J^2*(253800*En^4 + 12*En^2*(-81059 + 28736*la) + 5*(85655 - 63449*la + 1648*la^2))*M^2 + 296352*M^4)*rp[t]^10 + 
    2*J^4*(J^4*la*(1755 - 345*la - 8*En^6*(7 + la) + 30*En^4*(56 + la) + En^2*(-4633 + 392*la)) + J^2*(-51115 + 5832*En^6 + 88435*la - 5630*la^2 + 12*En^4*(-6602 + 3803*la) + 
        6*En^2*(27874 - 31219*la + 712*la^2))*M^2 + 9*(-25377 + 40785*En^2 + 6935*la)*M^4)*rp[t]^11 + 
    J^2*M*(2*J^4*(24*En^6*(-49 + 74*la) + 6*En^4*(1767 - 4754*la + 64*la^2) + 5*(861 - 4175*la + 508*la^2) - 3*En^2*(5877 - 21334*la + 1224*la^2)) + 
      3*J^2*(85067 + 107400*En^4 - 63885*la + 1800*la^2 + En^2*(-268274 + 96166*la))*M^2 + 98784*M^4)*rp[t]^12 + 
    J^2*(-(J^4*la*(-3445 + 755*la + 112*En^6*(7 + la) + 28*En^4*(-277 + 8*la) - 6*En^2*(-2229 + 256*la))) + 6*J^2*(-10091 + 3456*En^6 + 17715*la - 1230*la^2 + 4*En^4*(-8564 + 4981*la) + 
        2*En^2*(23176 - 26171*la + 718*la^2))*M^2 + 6*(-25301 + 54528*En^2 + 7011*la)*M^4)*rp[t]^13 + 
    M*(J^4*(48*En^6*(-33 + 145*la) + 9*(561 - 2765*la + 370*la^2) + 4*En^4*(7047 - 18962*la + 536*la^2) - 2*En^2*(14724 - 53683*la + 3758*la^2)) + 
      J^2*(84443 + 219780*En^4 - 64349*la + 1960*la^2 + 6*En^2*(-60293 + 21712*la))*M^2 + 14112*M^4)*rp[t]^14 + 
    (-(J^4*la*(48*En^6*(13 + la) + 45*(-45 + 11*la) + 4*En^4*(-2597 + 208*la) - 4*En^2*(-2794 + 401*la))) + 2*J^2*(17208*En^6 + 30*En^4*(-2467 + 1385*la) - 2*(4975 - 8873*la + 670*la^2) + 
        3*En^2*(21067 - 23773*la + 744*la^2))*M^2 + 6*(-3603 + 10263*En^2 + 1013*la)*M^4)*rp[t]^15 + 
    M*(J^2*(1641 - 2880*En^8 - 8237*la + 1210*la^2 + 48*En^6*(-157 + 258*la) + 4*En^4*(5580 - 13799*la + 506*la^2) - 2*En^2*(6795 - 24542*la + 1972*la^2)) + 
      (11969 + 62604*En^4 - 9263*la + 304*la^2 + 6*En^2*(-11488 + 4127*la))*M^2)*rp[t]^16 + 2*(-2*(-1 + En^2)*J^2*la*(165 + 240*En^6 + En^4*(916 - 44*la) - 45*la + En^2*(-1121 + 169*la)) + 
      (11700*En^6 + 6*En^4*(-3751 + 1983*la) - 4*(350 - 635*la + 52*la^2) + 3*En^2*(4082 - 4561*la + 156*la^2))*M^2)*rp[t]^17 + 
    4*(-1 + En^2)*(-57 + 720*En^6 + 292*la - 47*la^2 + 6*En^4*(-212 + 349*la) + 3*En^2*(206 - 697*la + 54*la^2))*M*rp[t]^18 + 4*(-1 + En^2)^2*la*(23 + 240*En^4 - 7*la + 16*En^2*(-13 + 2*la))*
     rp[t]^19))/(CapLa*En^2*(1 + la)*USq^6*rp[t]^25) + (8*fp^7*mu*Pi*YBar*(-1437*J^12*M^4 + 3*J^12*(601 - 166*la)*M^3*rp[t] + J^10*M^2*(J^2*(-734 + 633*la + 4*la^2) - 6468*M^2)*rp[t]^2 - 
    J^10*M*(2*J^2*(-48 + 131*la + 2*la^2) + 3*(-2757 + 1894*En^2 + 726*la)*M^2)*rp[t]^3 + J^8*(J^4*la*(35 + la) + J^2*(-3435 + 2833*la + 32*la^2 - 42*En^2*(-101 + 49*la))*M^2 - 10485*M^4)*rp[t]^4 - 
    J^8*M*(J^2*(-459 + 1202*la + 32*la^2 + 2*En^2*(369 - 772*la + 8*la^2)) + 6*(-2323 + 4101*En^2 + 550*la)*M^2)*rp[t]^5 + 
    J^6*(J^4*la*(165 + 8*la + En^2*(-270 + 8*la)) - 2*J^2*(3009 + 1134*En^4 - 2261*la - 52*la^2 + 495*En^2*(-19 + 9*la))*M^2 - 5928*M^4)*rp[t]^6 - 
    J^6*M*(J^2*(-837 + 2022*la + 104*la^2 + 24*En^4*(-26 + 33*la) + 18*En^2*(187 - 380*la + 4*la^2)) + 6*(-1499 + 6534*En^2 + 226*la)*M^2)*rp[t]^7 + 
    (J^8*la*(293 - 4*En^4*(-55 + la) + 26*la + 6*En^2*(-205 + 6*la)) - 2*J^6*(2174 + 4788*En^4 - 1191*la - 88*la^2 + 6*En^2*(-2614 + 1173*la))*M^2 + 2001*J^4*M^4)*rp[t]^8 - 
    J^4*M*(J^2*(-669 + 1290*la + 176*la^2 + 48*En^4*(-58 + 71*la) + 2*En^2*(2979 - 5644*la + 52*la^2)) + 3*(327 + 8068*En^2 - 474*la)*M^2)*rp[t]^9 + 
    (J^6*la*(219 + 44*la - 8*En^4*(-125 + 3*la) + En^2*(-2150 + 52*la)) - J^4*(276 + 16560*En^4 + 1151*la - 164*la^2 + 12*En^2*(-1756 + 707*la))*M^2 + 3564*J^2*M^4)*rp[t]^10 + 
    J^2*M*(J^2*(135 + En^4*(6120 - 5808*la) + 172*la - 164*la^2 - 10*En^2*(441 - 740*la + 4*la^2)) - 3*(1243 + 270*En^2 - 538*la)*M^2)*rp[t]^11 + 
    (J^4*la*(24 + 41*la - 8*En^4*(-267 + 4*la) + 2*En^2*(-777 + 10*la)) + J^2*(1191 - 7416*En^4 - 90*En^2*(-23 + la) - 1727*la + 80*la^2)*M^2 + 1089*M^4)*rp[t]^12 + 
    2*M*(-2*J^2*(27 + 648*En^6 - 141*la + 20*la^2 + 12*En^4*(-70 + 53*la) - 6*En^2*(-30 + 24*la + la^2)) + 3*(-204 + 519*En^2 + 76*la)*M^2)*rp[t]^13 + 
    2*(-2*(-1 + En^2)*J^2*la*(-13 + 216*En^4 + 2*En^2*(-35 + la) + 5*la) + (218 + 918*En^4 - 258*la + 8*la^2 + 3*En^2*(-367 + 195*la))*M^2)*rp[t]^14 + 
    8*(-1 + En^2)*(6 + 36*En^4 - 23*la + 2*la^2 + En^2*(-39 + 81*la))*M*rp[t]^15 + 4*(-1 + En^2)^2*la*(-5 + 24*En^2 + la)*rp[t]^16)*Derivative[1][rp][t])/(CapLa*En*USq^5*rp[t]^21) + 
 YPhiPhiBar*(((8*I)*fp^10*J^3*m*mu*Pi*(23814*J^14*M^5 - 3*J^14*(12539 - 3354*la + 152*m^2)*M^4*rp[t] + J^12*M^3*(J^2*(21627 - 15743*la + 480*la^2 + (624 - 160*la)*m^2) + 182070*M^2)*rp[t]^2 + 
      J^12*M^2*(2*J^2*(-2655 + 4462*la - 330*la^2 + (-141 + 110*la)*m^2) + 3*(-96197 + 24102*En^2 + 25458*la - 992*m^2)*M^2)*rp[t]^3 + 
      J^10*M*(2*J^4*(231 - 1072*la + 150*la^2 + (21 - 50*la)*m^2) + J^2*(166584 - 120001*la + 3480*la^2 - 8*(-513 + 130*la)*m^2 - 6*En^2*(13200 - 5043*la + 344*m^2))*M^2 + 602046*M^4)*rp[t]^4 + 
      3*J^10*(5*J^4*la*(12 - 3*la + m^2) + J^2*(-13699 + 22791*la - 1600*la^2 + 48*(-13 + 10*la)*m^2 + En^2*(9066 - 10826*la + 520*la^2 + (608 - 240*la)*m^2))*M^2 + 
        (-319523 + 170868*En^2 + 83474*la - 2680*m^2)*M^4)*rp[t]^5 + J^8*M*(J^4*(3*(1199 - 5511*la + 730*la^2 + (94 - 220*la)*m^2) + 2*En^2*(-1413 + 5401*la - 690*la^2 + 2*(-99 + 160*la)*m^2)) + 
        J^2*(556227 + 40608*En^4 - 395543*la + 10800*la^2 - 40*(-279 + 70*la)*m^2 - 12*En^2*(47182 - 17709*la + 1040*m^2))*M^2 + 1117710*M^4)*rp[t]^6 + 
      J^8*(10*J^4*la*(140 - 33*la + 10*m^2 - 2*En^2*(53 - 15*la + 7*m^2)) - 3*J^2*(46029 - 75608*la + 4980*la^2 + (1710 - 1300*la)*m^2 + En^4*(8252 - 5568*la + 464*m^2) + 
          4*En^2*(-16373 + 19209*la - 860*la^2 + 20*(-47 + 18*la)*m^2))*M^2 + 15*(-119377 + 103386*En^2 + 30690*la - 768*m^2)*M^4)*rp[t]^7 + 
      J^6*M*(J^4*(12180 - 55299*la + 6840*la^2 - 60*(-13 + 30*la)*m^2 + En^4*(3096 - 9772*la + 880*la^2 - 96*(-6 + 5*la)*m^2) + 4*En^2*(-5175 + 19423*la - 2310*la^2 + 70*(-9 + 14*la)*m^2)) + 
        J^2*(268344*En^4 - 30*En^2*(57644 - 21165*la + 976*m^2) + 5*(209376 - 146413*la + 3720*la^2 + (3216 - 800*la)*m^2))*M^2 + 1258290*M^4)*rp[t]^8 + 
      J^6*(5*J^4*la*(947 - 207*la + 55*m^2 + 8*En^4*(28 - 9*la + 5*m^2) - 8*En^2*(194 - 51*la + 22*m^2)) + J^2*(4968*En^6 - 12*En^4*(13965 - 9150*la + 704*m^2) - 
          30*En^2*(-20218 + 23247*la - 940*la^2 + 112*(-8 + 3*la)*m^2) + 5*(-52470 + 84677*la - 5160*la^2 + 16*(-93 + 70*la)*m^2))*M^2 + 15*(-135477 + 173592*En^2 + 34166*la - 616*m^2)*M^4)*
       rp[t]^9 + J^4*M*(-2*J^4*(12*En^6*(28 - 81*la + 8*m^2) + 10*En^2*(3231 - 11938*la + 1275*la^2 + (306 - 464*la)*m^2) + 2*En^4*(-5436 + 16505*la - 1400*la^2 + 72*(-13 + 10*la)*m^2) + 
          5*(-2343 + 10438*la - 1185*la^2 + 2*(-57 + 130*la)*m^2)) + J^2*(723456*En^4 - 120*En^2*(24670 - 8723*la + 280*m^2) + 5*(239997 - 164369*la + 3840*la^2 - 32*(-81 + 20*la)*m^2))*M^2 + 
        858114*M^4)*rp[t]^10 + J^4*(4*J^4*la*(-8*En^6*(7 - 3*la + 2*m^2) + 40*En^4*(49 - 15*la + 8*m^2) + 10*(227 - 45*la + 10*m^2) - 5*En^2*(1217 - 285*la + 106*m^2)) + 
        6*J^2*(5616*En^6 - 4*En^4*(18725 - 12240*la + 708*m^2) + 20*En^2*(8882 - 9787*la + 340*la^2 + (260 - 96*la)*m^2) + 5*(-2*(5077 - 8007*la + 445*la^2) + 3*(-67 + 50*la)*m^2))*M^2 + 
        3*(-466439 + 849450*En^2 + 115094*la - 1312*m^2)*M^4)*rp[t]^11 + J^2*M*(-2*J^4*(48*En^6*(56 - 141*la + 16*m^2) + 40*En^2*(1476 - 5179*la + 465*la^2 + (90 - 134*la)*m^2) + 
          4*En^4*(-6912 + 22265*la - 1700*la^2 + 36*(-27 + 20*la)*m^2) + 15*(-923 + 4005*la - 410*la^2 + (-31 + 70*la)*m^2)) + 
        J^2*(1167984*En^4 + 11*(76032 - 50833*la + 1080*la^2) - 8*(-693 + 170*la)*m^2 - 30*En^2*(99304 - 33393*la + 632*m^2))*M^2 + 327690*M^4)*rp[t]^12 + 
      J^2*(J^4*la*(-256*En^6*(7 - 3*la + 2*m^2) + 240*En^4*(86 - 25*la + 11*m^2) - 80*En^2*(554 - 105*la + 31*m^2) + 5*(2134 - 375*la + 65*m^2)) + 
        3*J^2*(-71895 + 16560*En^6 + 110333*la - 5520*la^2 + 32*(-27 + 20*la)*m^2 - 8*En^4*(32995 - 19030*la + 592*m^2) - 10*En^2*(-37169 + 38559*la - 1100*la^2 + 8*(-74 + 27*la)*m^2))*M^2 + 
        3*(-180057 + 453012*En^2 + 43398*la - 232*m^2)*M^4)*rp[t]^13 + M*(J^4*(20037 - 84189*la + 7650*la^2 + (402 - 900*la)*m^2 - 48*En^6*(-178 - 473*la + 52*m^2) + 
          8*En^4*(14598 - 37895*la + 2000*la^2 + (828 - 600*la)*m^2) + 10*En^2*(-13113 + 42529*la - 3030*la^2 + (-414 + 608*la)*m^2)) + 
        3*J^2*(109083 + 345312*En^4 - 71047*la + 1360*la^2 + (328 - 80*la)*m^2 - 4*En^2*(136550 - 43509*la + 352*m^2))*M^2 + 53946*M^4)*rp[t]^14 + 
      (2*J^4*la*(3830 - 585*la + 70*m^2 - 32*En^6*(-37 - 27*la + 13*m^2) + 80*En^4*(268 - 45*la + 14*m^2) - 10*En^2*(2431 - 345*la + 71*m^2)) + 
        J^2*(-85887 + 179424*En^6 + 127928*la - 5700*la^2 + (-462 + 340*la)*m^2 - 12*En^4*(65175 - 32320*la + 356*m^2) - 12*En^2*(-53299 + 51849*la - 1180*la^2 + 4*(-83 + 30*la)*m^2))*M^2 + 
        3*(-29983 + 101358*En^2 + 7054*la)*M^4)*rp[t]^15 + M*(J^2*(8172 - 17280*En^8 - 33143*la + 2640*la^2 - 8*(-9 + 20*la)*m^2 - 96*En^6*(522 - 691*la + 12*m^2) - 
          4*En^4*(-34794 + 71795*la - 2300*la^2 + 72*(-7 + 5*la)*m^2) + 4*En^2*(-19989 + 59753*la - 3270*la^2 + (-234 + 340*la)*m^2)) + 
        3*(18424 + 121224*En^4 - 11681*la + 200*la^2 + 2*En^2*(-62924 + 19063*la))*M^2)*rp[t]^16 + 
      (J^2*la*(-5760*En^8 - 384*En^6*(46 - 4*la + m^2) + 5*(619 - 81*la + 5*m^2) - 40*En^2*(730 - 75*la + 8*m^2) + 40*En^4*(1240 - 105*la + 17*m^2)) + 
        3*(-4924 + 52824*En^6 + 7113*la - 280*la^2 + En^4*(-98932 + 43928*la) + En^2*(51032 - 46826*la + 840*la^2))*M^2)*rp[t]^17 + 
      2*(11520*En^8 + 36*En^6*(-880 + 767*la) + 3*(240 - 939*la + 65*la^2) - 6*En^2*(1680 - 4681*la + 195*la^2) + 2*En^4*(14760 - 26473*la + 520*la^2))*M*rp[t]^18 + 
      60*(-1 + En^2)*la*(-9 + 128*En^6 + 8*En^4*(-29 + la) - 8*En^2*(-14 + la) + la)*rp[t]^19))/(CapLa*En^2*la*(1 + la)*USq^7*rp[t]^27) + 
   (8*fp^10*J^2*mu*Pi*(33075*J^16*M^5 - 3*J^16*(15688 - 5853*la + 1138*m^2)*M^4*rp[t] + 3*J^14*M^3*(J^2*(7968 - 8029*la + 678*la^2 + (1397 - 430*la)*m^2 + 4*m^4) + 93168*M^2)*rp[t]^2 + 
      J^14*M^2*(J^2*(-5040 + 11630*la - 2481*la^2 + 28*la^3 + (-1668 + 1585*la - 20*la^2)*m^2 + 4*(-3 + la)*m^4) + 6*(-66368 + 13500*En^2 + 24522*la - 4379*m^2)*M^2)*rp[t]^3 + 
      J^12*M*(J^4*(360 - 2276*la + 978*la^2 - 28*la^3 + (213 - 630*la + 20*la^2)*m^2 + (3 - 4*la)*m^4) - 3*J^2*(-67510 + 67440*la - 5568*la^2 + (-10827 + 3290*la)*m^2 - 24*m^4 + 
          2*En^2*(12679 - 7089*la + 1894*m^2))*M^2 + 1040436*M^4)*rp[t]^4 + J^12*(J^4*la*(144 - 123*la + 7*la^2 - 5*(-16 + la)*m^2 + m^4) + 
        J^2*(-42768 + 97977*la - 20416*la^2 + 228*la^3 + (-13038 + 12215*la - 140*la^2)*m^2 + 24*(-3 + la)*m^4 + 6*En^2*(3582 - 6250*la + 829*la^2 + (1414 - 710*la)*m^2 + 12*m^4))*M^2 + 
        18*(-82484 + 35448*En^2 + 30128*la - 4835*m^2)*M^4)*rp[t]^5 + J^10*M*(J^4*(3060 - 19247*la + 8068*la^2 - 228*la^3 + (1683 - 4900*la + 140*la^2)*m^2 - 6*(-3 + 4*la)*m^4 + 
          2*En^2*(-855 + 4810*la - 1832*la^2 + 24*la^3 - 2*(369 - 790*la + 20*la^2)*m^2 + 6*(-3 + 2*la)*m^4)) + 3*J^2*(9900*En^4 + 32*(7880 - 7791*la + 626*la^2) - 15*(-2411 + 722*la)*m^2 + 60*m^4 - 
          6*En^2*(33489 - 18441*la + 4580*m^2))*M^2 + 2232432*M^4)*rp[t]^6 + 
      J^10*(J^4*la*(1224 - 1018*la + 57*la^2 - 35*(-18 + la)*m^2 + 6*m^4 - 2*En^2*(324 - 313*la + 12*la^2 + (270 - 20*la)*m^2 + 6*m^4)) + 
        J^2*(-160056 + 363523*la - 73624*la^2 + 812*la^3 - 15*(2934 - 2703*la + 28*la^2)*m^2 + 60*(-3 + la)*m^4 - 36*En^4*(376 - 411*la + 126*m^2) + 
          6*En^2*(28590 - 49184*la + 6335*la^2 - 60*(-174 + 85*la)*m^2 + 60*m^4))*M^2 + 6*(-532168 + 364428*En^2 + 191666*la - 26755*m^2)*M^4)*rp[t]^7 + 
      J^8*M*(J^4*(11475 - 71742*la + 29182*la^2 - 812*la^3 + 60*(96 - 274*la + 7*la^2)*m^2 + (45 - 60*la)*m^4 + 12*En^4*(90 - 496*la + 133*la^2 - 4*(-26 + 35*la)*m^2 + 4*m^4) + 
          30*En^2*(3*(-153 + 850*la - 314*la^2 + 4*la^3) - 4*(93 - 193*la + 4*la^2)*m^2 + (-6 + 4*la)*m^4)) + 3*J^2*(543618 + 74952*En^4 - 530696*la + 41232*la^2 + (67315 - 19850*la)*m^2 + 80*m^4 - 
          6*En^2*(115541 - 62491*la + 14150*m^2))*M^2 + 3028050*M^4)*rp[t]^8 + 
      J^8*(J^4*la*(4590 - 3697*la + 203*la^2 - 15*(-143 + 7*la)*m^2 + 15*m^4 - 30*En^2*(174 - 163*la + 6*la^2 - 8*(-17 + la)*m^2 + 2*m^4) + 
          4*En^4*(96 - 107*la + 3*la^2 - 10*(-11 + la)*m^2 + 4*m^4)) + J^2*(-345888 + 1080*En^6 + 777421*la - 151944*la^2 + 1652*la^3 + (-82860 + 74975*la - 700*la^2)*m^2 + 80*(-3 + la)*m^4 - 
          108*En^4*(973 - 1032*la + 298*m^2) + 18*En^2*(33174 - 56114*la + 6967*la^2 + (11050 - 5190*la)*m^2 + 40*m^4))*M^2 + 30*(-144908 + 141432*En^2 + 51253*la - 5927*m^2)*M^4)*rp[t]^9 + 
      J^6*M*(J^4*(24840 - 154321*la + 360*En^6*la + 60432*la^2 - 1652*la^3 + 10*(1101 - 3075*la + 70*la^2)*m^2 + (60 - 80*la)*m^4 + 12*En^4*(720 - 3849*la + 992*la^2 + (776 - 980*la)*m^2 + 16*m^4) + 
          6*En^2*(-8100 + 44189*la - 15736*la^2 + 192*la^3 - 10*(615 - 1212*la + 20*la^2)*m^2 + 20*(-3 + 2*la)*m^4)) + 
        3*J^2*(245556*En^4 + 10*(74330 - 71329*la + 5310*la^2) + (75255 - 21850*la)*m^2 + 60*m^4 - 10*En^2*(135127 - 71735*la + 14056*m^2))*M^2 + 2666160*M^4)*rp[t]^10 + 
      J^6*(J^4*la*(9945 - 7692*la + 413*la^2 - 25*(-163 + 7*la)*m^2 + 20*m^4 + 8*En^4*(387 - 414*la + 11*la^2 - 5*(-83 + 5*la)*m^2 + 8*m^4) - 
          12*En^2*(1530 - 1387*la + 48*la^2 + (1115 - 50*la)*m^2 + 10*m^4)) + J^2*(9720*En^6 - 36*En^4*(10123 - 10115*la + 2916*m^2) + 
          30*En^2*(38826 - 65068*la + 7699*la^2 - 8*(-1412 + 637*la)*m^2 + 24*m^4) + 5*(5*(-19008 + 42075*la - 7850*la^2 + 84*la^3) + (-18744 + 16655*la - 140*la^2)*m^2 + 12*(-3 + la)*m^4))*M^2 + 
        18*(-213968 + 285180*En^2 + 73898*la - 6563*m^2)*M^4)*rp[t]^11 + J^4*M*(J^4*(3240*En^6*la + 12*En^4*(2970 - 13313*la + 3245*la^2 - 156*(-19 + 20*la)*m^2 + 24*m^4) + 
          10*En^2*(-9342 + 51703*la - 17642*la^2 + 204*la^3 - 8*(819 - 1531*la + 20*la^2)*m^2 + 12*(-3 + 2*la)*m^4) - 5*(-6858 + 42128*la - 15670*la^2 + 420*la^3 + (-2529 + 6910*la - 140*la^2)*m^2 + 
            3*(-3 + 4*la)*m^4)) + 3*J^2*(663578 + 411504*En^4 - 621664*la + 43808*la^2 + (50457 - 14430*la)*m^2 + 24*m^4 - 30*En^2*(55139 - 28261*la + 4342*m^2))*M^2 + 1491444*M^4)*rp[t]^12 + 
      J^4*(J^4*la*(5*(2748 - 2005*la + 105*la^2 + (930 - 35*la)*m^2 + 3*m^4) - 20*En^2*(1779 - 1589*la + 51*la^2 - 8*(-147 + 5*la)*m^2 + 6*m^4) + 
          4*En^4*(3066 - 2955*la + 65*la^2 - 20*(-156 + 5*la)*m^2 + 24*m^4)) + J^2*(-428400 + 67248*En^6 + 925835*la - 162416*la^2 + 1708*la^3 + (-63558 + 55485*la - 420*la^2)*m^2 + 
          24*(-3 + la)*m^4 - 72*En^4*(7811 - 8660*la + 2322*m^2) + 30*En^2*(48066 - 78254*la + 8531*la^2 - 6*(-1791 + 779*la)*m^2 + 12*m^4))*M^2 + 
        6*(-362468 + 656280*En^2 + 121412*la - 7259*m^2)*M^4)*rp[t]^13 + J^2*M*(J^4*(31320 - 188001*la + 65108*la^2 - 1708*la^3 + 3*(2901 - 7760*la + 140*la^2)*m^2 - 6*(-3 + 4*la)*m^4 - 
          144*En^6*(150 - 179*la + 60*m^2) + 24*En^4*(1440 - 10941*la + 2860*la^2 + (2638 - 2440*la)*m^2 + 8*m^4) + 30*En^2*(-3861 + 21176*la - 6616*la^2 + 72*la^3 + 
            (-2154 + 3844*la - 40*la^2)*m^2 + (-6 + 4*la)*m^4)) + 3*J^2*(462996*En^4 + 8*(47415 - 42995*la + 2826*la^2) + (18767 - 5290*la)*m^2 + 4*m^4 - 6*En^2*(217643 - 104483*la + 10628*m^2))*
         M^2 + 485136*M^4)*rp[t]^14 + J^2*(J^4*la*(12534 - 8378*la + 427*la^2 - 15*(-212 + 7*la)*m^2 + 6*m^4 - 480*En^6*(14 - 3*la + 6*m^2) - 
          30*En^2*(1488 - 1219*la + 36*la^2 + (766 - 20*la)*m^2 + 2*m^4) + 16*En^4*(786 - 1420*la + 25*la^2 - 5*(-274 + 5*la)*m^2 + 4*m^4)) + 
        J^2*(-249048 - 30096*En^6 + 519497*la - 84096*la^2 + 868*la^3 + (-23898 + 20515*la - 140*la^2)*m^2 + 4*(-3 + la)*m^4 - 36*En^4*(18078 - 18865*la + 3486*m^2) + 
          18*En^2*(65934 - 99560*la + 9463*la^2 + (8968 - 3780*la)*m^2 + 4*m^4))*M^2 + 6*(-119432 + 296004*En^2 + 38534*la - 1145*m^2)*M^4)*rp[t]^15 + 
      M*(J^4*(18675 + 17280*En^8 - 107702*la + 33858*la^2 - 868*la^3 + 2*(1659 - 4350*la + 70*la^2)*m^2 + (3 - 4*la)*m^4 - 48*En^6*(-1050 - 41*la + 360*m^2) + 
          12*En^4*(2970 - 25218*la + 5615*la^2 + (4244 - 3620*la)*m^2 + 4*m^4) + 6*En^2*(-17055 + 84920*la - 22354*la^2 + 228*la^3 + (-5556 + 9540*la - 80*la^2)*m^2 + (-6 + 4*la)*m^4)) + 
        3*J^2*(358344*En^4 + 2*(63575 - 55316*la + 3336*la^2) + (2985 - 830*la)*m^2 - 2*En^2*(308077 - 134803*la + 6430*m^2))*M^2 + 70227*M^4)*rp[t]^16 + 
      (J^4*la*(7434 + 5760*En^8 - 4383*la + 217*la^2 - 5*(-241 + 7*la)*m^2 + m^4 - 1440*En^6*(-3*(4 + la) + 4*m^2) - 6*En^2*(6558 - 4211*la + 114*la^2 - 40*(-49 + la)*m^2 + 2*m^4) + 
          4*En^4*(3636 - 5985*la + 85*la^2 + (4370 - 50*la)*m^2 + 4*m^4)) + J^2*(-85536 + 53784*En^6 + 170223*la - 24904*la^2 + 252*la^3 - 5*(768 - 649*la + 4*la^2)*m^2 - 
          36*En^4*(17443 - 13216*la + 990*m^2) + 6*En^2*(99990 - 134554*la + 10495*la^2 + (5530 - 2270*la)*m^2))*M^2 + 9*(-11712 + 39792*En^2 + 3619*la)*M^4)*rp[t]^17 + 
      M*(J^2*(6660 - 46080*En^8 - 36287*la + 10072*la^2 - 252*la^3 + 10*(54 - 139*la + 2*la^2)*m^2 + En^6*(24480 + 31848*la - 8640*m^2) - 
          12*En^4*(-6120 + 21773*la - 2888*la^2 + 60*(-21 + 17*la)*m^2) - 10*En^2*(5850 - 24731*la + 5032*la^2 - 48*la^3 + 2*(351 - 584*la + 4*la^2)*m^2)) + 
        3*(19108 + 126348*En^4 - 15841*la + 862*la^2 + 6*En^2*(-21811 + 8611*la))*M^2)*rp[t]^18 + 
      (J^2*la*(2625 - 15360*En^8 - 1312*la + 63*la^2 - 5*(-39 + la)*m^2 - 480*En^6*(-16 - 9*la + 6*m^2) - 8*En^4*(-3405 + 1626*la - 19*la^2 + 5*(-129 + la)*m^2) + 
          20*En^2*(-4*(276 - 121*la + 3*la^2) + (-123 + 2*la)*m^2)) + (-13248 + 142488*En^6 + 24931*la - 3229*la^2 + 32*la^3 + 36*En^4*(-7407 + 4241*la) + 6*En^2*(22902 - 27212*la + 1661*la^2))*M^2)*
       rp[t]^19 + 2*(540 + 8640*En^8 - 2748*la + 656*la^2 - 16*la^3 + 36*En^6*(-660 + 733*la) + 6*En^4*(3690 - 8477*la + 607*la^2) + 3*En^2*(-2520 + 9049*la - 1346*la^2 + 12*la^3))*M*rp[t]^20 + 
      4*(-1 + En^2)*la*(-105 + 1440*En^6 + 43*la - 2*la^2 + 120*En^4*(-22 + 3*la) + En^2*(1290 - 353*la + 7*la^2))*rp[t]^21)*Derivative[1][rp][t])/(CapLa*En*la*(1 + la)*USq^8*rp[t]^28))
]


Clear[gSourceZM6]
gSourceZM6[syms_Association]:=
Module[{mu,M,J,la,YBar,YPhiBar,YPhiPhiBar,En,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	
(-8*mu*Pi*YBar*(-2*M + rp[t])^2*(12761658*J^18*M^7 + 
    18*J^18*(-1345400 + 494313*la)*M^6*rp[t] + 
    6*J^16*M^5*(J^2*(2983008 - 2815370*la + 266585*la^2) + 17550207*M^2)*
     rp[t]^2 + J^16*M^4*(J^2*(-6399876 + 12490279*la - 3030391*la^2 + 
        14836*la^3) + 18*(-11132102 + 757629*En^2 + 4086000*la)*M^2)*
     rp[t]^3 + J^14*M^3*(J^4*(1097532 - 4468437*la + 2235054*la^2 - 
        24684*la^3 + 56*la^4) + 3*J^2*(49507622 - 46659789*la + 
        4430736*la^2 + 6*En^2*(-1058318 + 519627*la))*M^2 + 379579392*M^4)*
     rp[t]^4 + J^14*M^2*(J^4*(-71280 + 766734*la - 797160*la^2 + 15097*la^3 - 
        84*la^4) + J^2*(-53270580 + 103770023*la - 25226204*la^2 + 
        140300*la^3 + 6*En^2*(1587130 - 2179432*la + 271025*la^2))*M^2 + 
      108*(-6712672 + 982542*En^2 + 2459269*la)*M^4)*rp[t]^5 + 
    2*J^12*M*(J^6*la*(-24915 + 68143*la - 2001*la^2 + 21*la^3) + 
      J^4*(4582476 - 18611789*la + 9318142*la^2 - 116800*la^3 + 288*la^4 + 
        En^2*(-994428 + 3272565*la - 1138832*la^2 + 912*la^3))*M^2 + 
      3*J^2*(89908661 + 665496*En^4 - 84538364*la + 8044604*la^2 + 
        348*En^2*(-71467 + 34933*la))*M^4 + 389371968*M^6)*rp[t]^6 + 
    J^12*(-7*J^6*la^2*(1260 - 55*la + la^2) - 
      J^4*(597240 - 6404823*la + 6657191*la^2 - 142915*la^3 + 864*la^4 + 
        2*En^2*(-71460 + 684816*la - 572197*la^2 + 622*la^3 + 28*la^4))*M^2 + 
      J^2*(-194282466 + 377365429*la - 91844620*la^2 + 574396*la^3 + 
        36*En^4*(-99094 + 74663*la) + 12*En^2*(6261982 - 8550863*la + 
          1063766*la^2))*M^4 + 36*(-41527034 + 9923532*En^2 + 15163585*la)*
       M^6)*rp[t]^7 + J^10*M*(J^6*la*(-417435 + 1139737*la - 37865*la^2 + 
        432*la^3 + En^2*(98730 - 241262*la + 86*la^2 + 56*la^3)) + 
      J^4*(33571188 - 135870431*la + 68029688*la^2 - 958972*la^3 + 
        2568*la^4 + 12*En^4*(80094 - 200467*la + 37422*la^2) + 
        8*En^2*(-1977543 + 6463401*la - 2245252*la^2 + 4587*la^3))*M^2 + 
      6*J^2*(186410737 + 4969260*En^4 - 174595880*la + 16627364*la^2 + 
        264*En^2*(-319781 + 155426*la))*M^4 + 989065188*M^6)*rp[t]^8 + 
    J^10*(J^6*la^2*(-73850 + 3635*la - 72*la^2 + 
        En^2*(17640 + 40*la - 14*la^2)) - J^4*(2197800 - 23469105*la + 
        24363482*la^2 - 588239*la^3 + 3852*la^4 + 
        12*En^4*(6180 - 54209*la + 34082*la^2 + 465*la^3) + 
        4*En^2*(-286695 + 2725053*la - 2267013*la^2 + 8849*la^3 + 80*la^4))*
       M^2 + J^2*(-405135840 + 233496*En^6 + 783354641*la - 190606772*la^2 + 
        1327644*la^3 + 36*En^4*(-751881 + 559025*la) + 
        24*En^2*(10724845 - 14542934*la + 1808841*la^2))*M^4 + 
      72*(-26570182 + 9387861*En^2 + 9645352*la)*M^6)*rp[t]^9 + 
    2*J^8*M*(J^6*la*(-767886 + 2090613*la - 78104*la^2 + 963*la^3 - 
        4*En^4*(6318 - 14311*la - 555*la^2 + 7*la^3) + 
        2*En^2*(198015 - 480296*la + 2471*la^2 + 80*la^3)) + 
      J^4*(35216460 - 141781589*la + 70884909*la^2 - 1113724*la^3 + 
        3256*la^4 + 36*En^6*(-1248 + 2099*la) + 
        12*En^4*(309117 - 762022*la + 141231*la^2) + 
        En^2*(-27372906 + 88685631*la - 30728240*la^2 + 105012*la^3))*M^2 + 
      3*J^2*(240462629 + 15819912*En^4 - 223730388*la + 21269998*la^2 + 
        12*En^2*(-13452995 + 6489343*la))*M^4 + 392831154*M^6)*rp[t]^10 + 
    J^8*(J^6*la^2*(-271528 + 15016*la - 321*la^2 + 
        4*En^4*(-2379 - 200*la + 7*la^2) - 10*En^2*(-14121 + 68*la + 
          8*la^2)) + J^4*(-4640490 + 49255356*la - 50983893*la^2 + 
        1373611*la^3 - 9768*la^4 + 48*En^6*(120 - 1222*la + 477*la^2) - 
        12*En^4*(48465 - 418884*la + 260102*la^2 + 2683*la^3) + 
        En^2*(4016790 - 37743216*la + 31220830*la^2 - 220224*la^3 - 
          568*la^4))*M^2 + J^2*(-527124318 + 1857168*En^6 + 1011561931*la - 
        245400010*la^2 + 1884852*la^3 + 108*En^4*(-814558 + 594913*la) + 
        12*En^2*(41469808 - 55768037*la + 6925486*la^2))*M^4 + 
      36*(-42717300 + 21620205*En^2 + 15342389*la)*M^6)*rp[t]^11 + 
    J^6*M*(J^6*la*(-3241797 + 8785421*la - 366847*la^2 + 4884*la^3 - 
        8*En^6*(-480 + 1330*la + 189*la^2) - 4*En^4*(99138 - 220642*la - 
          6775*la^2 + 96*la^3) + En^2*(2773266 - 6658530*la + 70166*la^2 + 
          568*la^3)) + J^4*(92493072 - 369189443*la + 183753040*la^2 - 
        3185372*la^3 + 10344*la^4 + 288*En^6*(-2576 + 4201*la) + 
        36*En^4*(690354 - 1653267*la + 303110*la^2) + 
        4*En^2*(-26742105 + 85919028*la - 29670052*la^2 + 147186*la^3))*M^2 + 
      6*J^2*(195770263 + 27559548*En^4 - 180015685*la + 17002300*la^2 + 
        90*En^2*(-2101622 + 999293*la))*M^4 + 366487632*M^6)*rp[t]^12 + 
    J^6*(-2*J^6*la^2*(286336 - 17737*la + 407*la^2 + 
        En^4*(36948 + 2598*la - 96*la^2) + 4*En^6*(-132 - 51*la + la^2) + 
        En^2*(-246666 + 3140*la + 71*la^2)) + 
      J^4*(-6154920 + 64693350*la - 66544132*la^2 + 1980425*la^3 - 
        15516*la^4 + 48*En^6*(960 - 10136*la + 3903*la^2) - 
        12*En^4*(172620 - 1401888*la + 848924*la^2 + 6141*la^3) + 
        12*En^2*(659220 - 6155520*la + 5067007*la^2 - 54509*la^3 + 8*la^4))*
       M^2 + J^2*(-435082476 + 6333768*En^6 + 824044799*la - 198270356*la^2 + 
        1661796*la^3 + 36*En^4*(-4274297 + 3129197*la) + 
        60*En^2*(9906154 - 13102506*la + 1613651*la^2))*M^4 + 
      36*(-20367736 + 15005742*En^2 + 7157825*la)*M^6)*rp[t]^13 + 
    2*J^4*M*(J^6*la*(-2148855 + 5774443*la - 266911*la^2 + 3879*la^3 - 
        32*En^6*(-480 + 1348*la + 147*la^2) - 6*En^2*(-455580 + 1088495*la - 
          18905*la^2 + 8*la^3) - 4*En^4*(175806 - 367563*la - 8968*la^2 + 
          125*la^3)) + J^4*(38748816 - 152391416*la + 75074567*la^2 - 
        1418672*la^3 + 5328*la^4 + 36*En^6*(-45010 + 57067*la) + 
        12*En^4*(1772661 - 4365487*la + 810181*la^2) + 
        5*En^2*(-13048848 + 41138553*la - 14042600*la^2 + 91644*la^3))*M^2 + 
      3*J^2*(95507111 + 30194568*En^4 - 85816732*la + 7962828*la^2 + 
        12*En^2*(-11264597 + 5212183*la))*M^4 + 38567016*M^6)*rp[t]^14 + 
    J^4*(J^6*la^2*(-758164 + 52173*la - 1293*la^2 - 
        64*En^6*(-122 - 41*la + la^2) + 12*En^2*(80945 - 1958*la + 2*la^2) + 
        4*En^4*(-64680 - 4081*la + 125*la^2)) + 
      J^4*(-5243940 + 54187359*la - 55030525*la^2 + 1784497*la^3 - 
        15984*la^4 + 48*En^6*(10335 - 43720*la + 13112*la^2) - 
        12*En^4*(264810 - 2415991*la + 1513744*la^2 + 3847*la^3) + 
        10*En^2*(986598 - 9025578*la + 7315717*la^2 - 106856*la^3 + 
          172*la^4))*M^2 + J^2*(-217480110 + 6467040*En^6 + 401800543*la - 
        94769644*la^2 + 858260*la^3 + 36*En^4*(-4924914 + 3450245*la) + 
        12*En^2*(36630670 - 46890201*la + 5650290*la^2))*M^4 + 
      36*(-4563646 + 5493576*En^2 + 1499987*la)*M^6)*rp[t]^15 + 
    J^2*M*(J^6*la*(-3657885 + 9673371*la - 487387*la^2 + 7992*la^3 - 
        8*En^6*(-40674 + 43948*la + 3897*la^2) - 
        10*En^2*(-682419 + 1599197*la - 39653*la^2 + 172*la^3) - 
        4*En^4*(544662 - 1278466*la - 16589*la^2 + 308*la^3)) + 
      J^4*(39771252 + 183168*En^8 - 152207033*la + 73310784*la^2 - 
        1488740*la^3 + 7096*la^4 + 576*En^6*(341 + 7408*la) + 
        12*En^4*(4343970 - 10124965*la + 1816834*la^2) + 
        8*En^2*(-12577032 + 38064255*la - 12628568*la^2 + 99573*la^3))*M^2 + 
      6*J^2*(22728451 + 19515492*En^4 - 19143684*la + 1667644*la^2 + 
        24*En^2*(-2169099 + 954011*la))*M^4 - 4908654*M^6)*rp[t]^16 + 
    J^2*(J^6*la^2*(-643730 + 48375*la - 1332*la^2 + 
        56*En^4*(-7278 - 352*la + 11*la^2) - 8*En^6*(-7910 - 1479*la + 
          21*la^2) + 10*En^2*(120876 - 4568*la + 43*la^2)) + 
      J^4*(-2771280 + 27768813*la - 27478026*la^2 + 952685*la^3 - 
        10644*la^4 + 4032*En^8*(-60 + 29*la) + 
        96*En^6*(-10470 + 1678*la + 7007*la^2) + 
        12*En^4*(-349560 + 2979501*la - 1778298*la^2 + 5389*la^3) + 
        4*En^2*(2007585 - 17398653*la + 13574371*la^2 - 242111*la^3 + 
          656*la^4))*M^2 + J^2*(-54902136 + 14974632*En^6 + 95195939*la - 
        21082700*la^2 + 205876*la^3 + 36*En^4*(-3531819 + 2233651*la) + 
        24*En^2*(7470961 - 9021924*la + 1037853*la^2))*M^4 + 
      18*(276672 + 1020924*En^2 - 202025*la)*M^6)*rp[t]^17 - 
    2*M*(J^6*la*(964500 - 2473691*la + 132630*la^2 - 2661*la^3 - 
        768*En^8*(-105 + 11*la) + 32*En^6*(10461 - 115*la + 611*la^2) + 
        4*En^4*(361722 - 791557*la + 3305*la^2 + 81*la^3) + 
        2*En^2*(-1386615 + 3079478*la - 94929*la^2 + 656*la^3)) + 
      J^4*(-5326164 + 978048*En^8 + En^6*(3749616 - 5069700*la) + 
        19126733*la - 8645059*la^2 + 185196*la^3 - 1464*la^4 - 
        12*En^4*(1812051 - 3631464*la + 592321*la^2) - 
        3*En^2*(-7325694 + 20668143*la - 6485376*la^2 + 57956*la^3))*M^2 - 
      3*J^2*(-9313 + 4743000*En^4 + 644470*la - 118351*la^2 + 
        12*En^2*(-486217 + 175657*la))*M^4 + 1928151*M^6)*rp[t]^18 - 
    (J^6*la^2*(337960 - 26910*la + 887*la^2 + 192*En^8*(149 + 9*la) + 
        En^2*(-974970 + 47320*la - 656*la^2) + 
        En^4*(530460 + 1272*la - 324*la^2) + 32*En^6*(2945 - 544*la + 
          6*la^2)) + J^4*(789210 - 7404030*la + 6863387*la^2 - 246049*la^3 + 
        4392*la^4 + 576*En^8*(-1785 + 2281*la) - 
        48*En^6*(10140 - 106142*la + 36085*la^2) - 
        12*En^4*(-379665 + 2479926*la - 1276982*la^2 + 8319*la^3) - 
        2*En^2*(1915155 - 15159036*la + 11031941*la^2 - 220088*la^3 + 
          940*la^4))*M^2 + J^2*(1475790 - 12308112*En^6 + 322694*la - 
        797753*la^2 + 6136*la^3 - 36*En^4*(-985342 + 541169*la) - 
        12*En^2*(1978660 - 2003923*la + 189962*la^2))*M^4 + 
      18*(-387102 + 353151*En^2 + 152962*la)*M^6)*rp[t]^19 + 
    M*(J^4*la*(-546747 + 1308871*la - 71193*la^2 + 2196*la^3 - 
        384*En^8*(-1785 + 583*la) - 8*En^6*(-42612 + 112082*la + 1727*la^2) + 
        4*En^4*(-778446 + 1302918*la - 18319*la^2 + 8*la^3) - 
        2*En^2*(-1316835 + 2666447*la - 90617*la^2 + 940*la^3)) + 
      2*J^2*(278550 + 827712*En^8 - 464344*la - 67451*la^2 + 2196*la^3 + 
        336*la^4 + 144*En^6*(-33646 + 28855*la) + 
        6*En^4*(1191018 - 2016847*la + 286398*la^2) + 
        6*En^2*(-567891 + 1356276*la - 355660*la^2 + 3802*la^3))*M^2 - 
      3*(1622332 + 1242648*En^4 - 1657519*la + 174024*la^2 + 
        6*En^2*(-495090 + 247033*la))*M^4)*rp[t]^20 - 
    2*((-1 + En^2)*J^4*la^2*(-47434 + 1632*En^6*(-34 + la) + 3758*la - 
        183*la^2 + 4*En^4*(-22912 - 521*la + 11*la^2) + 
        2*En^2*(90598 - 4131*la + 26*la^2)) + 
      J^2*(30240 - 185343*la + 68104*la^2 - 673*la^3 + 504*la^4 - 
        288*En^8*(-1110 + 1909*la) - 24*En^6*(38640 - 135964*la + 
          29603*la^2) - 6*En^4*(-154320 + 809714*la - 351464*la^2 + 
          2945*la^3) - 2*En^2*(174240 - 1164774*la + 711835*la^2 - 
          15561*la^3 + 168*la^4))*M^2 + 3*(-271412 + 151596*En^6 + 
        578319*la - 156520*la^2 + 1568*la^3 + 6*En^4*(-104973 + 71057*la) + 
        En^2*(749654 - 1037876*la + 135303*la^2))*M^4)*rp[t]^21 - 
    2*M*(-2*(-1 + En^2)*J^2*la*(192*En^6*(-555 + 236*la) + 
        16*En^4*(12813 - 14525*la + 46*la^2) + 
        3*(3406 - 4993*la + 131*la^2 - 42*la^3) + 
        6*En^2*(-18086 + 30971*la - 1086*la^2 + 7*la^3)) + 
      (128844 + 38016*En^8 - 579511*la + 325845*la^2 - 7704*la^3 - 32*la^4 + 
        36*En^6*(-8254 + 8503*la) + 12*En^4*(50757 - 107933*la + 
          18741*la^2) + 6*En^2*(-79800 + 261509*la - 94476*la^2 + 698*la^3))*
       M^2)*rp[t]^22 - 4*(-1 + En^2)*((-1 + En^2)*J^2*la^2*
       (1678 - 71*la + 21*la^2 + 48*En^4*(377 + 7*la) + 
        8*En^2*(-2099 + 106*la)) + (3780 - 45720*la + 53997*la^2 - 
        2320*la^3 - 24*la^4 + 432*En^6*(-15 + 29*la) + 
        36*En^4*(465 - 2432*la + 718*la^2) + 3*En^2*(-4680 + 40305*la - 
          29444*la^2 + 61*la^3))*M^2)*rp[t]^23 - 
    4*(-1 + En^2)^2*la*(-2670 + 8426*la - 607*la^2 - 12*la^3 + 
      288*En^4*(-15 + 7*la) - 18*En^2*(-390 + 736*la + 13*la^2))*M*rp[t]^24 + 
    8*(-1 + En^2)^3*la^2*(-242 + 29*la + la^2 + 24*En^2*(16 + la))*rp[t]^25))/
  (En*rp[t]^16*(3*M + la*rp[t])^2*(J^2 + rp[t]^2)^8) + 
 (8*J^2*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*(149971500*J^22*M^8 - 
    567*J^22*(549209 - 228956*la + 45978*m^2)*M^7*rp[t] + 
    9*J^20*M^6*(J^2*(28856221 - 29426424*la + 4049754*la^2 + 
        (5491355 - 2094320*la)*m^2 + 39564*m^4) + 194764248*M^2)*rp[t]^2 + 
    3*J^20*M^5*(-2*J^2*(18390975 - 36039879*la + 12041939*la^2 - 
        561921*la^3 + (6075201 - 5935903*la + 621681*la^2)*m^2 - 
        12*(-8417 + 3389*la)*m^4 + 60*m^6) + 
      9*(-134909919 + 5811600*En^2 + 55993444*la - 10750356*m^2)*M^2)*
     rp[t]^3 + J^18*M^4*(J^4*(24859278 - 89513382*la + 56892033*la^2 - 
        6322887*la^3 + 37748*la^4 + (12984003 - 26152587*la + 6972091*la^2 - 
          110140*la^3)*m^2 + (380727 - 415632*la + 42404*la^2)*m^4 - 
        60*(-9 + 4*la)*m^6) - 18*J^2*(-168911539 + 171541852*la - 
        23459971*la^2 + (-30672769 + 11633084*la)*m^2 - 199398*m^4 + 
        3*En^2*(4563043 - 2471957*la + 526796*m^2))*M^2 + 9362914740*M^4)*
     rp[t]^4 + J^18*M^3*(-(J^4*(2757240 - 19509372*la + 22458421*la^2 - 
         4610050*la^3 + 62924*la^4 + 72*la^5 + (2211219 - 9260157*la + 
           5047050*la^2 - 186700*la^3 + 200*la^4)*m^2 + 
         (104247 - 261306*la + 72196*la^2 - 184*la^3)*m^4 + 
         10*(27 - 36*la + 4*la^2)*m^6)) + 6*J^2*(-215494284 + 420712419*la - 
        139707659*la^2 + 6466943*la^3 + (-68121090 + 66180260*la - 
          6868846*la^2)*m^2 + 4*(-256314 + 102323*la)*m^4 - 480*m^6 + 
        6*En^2*(4048280 - 5694205*la + 1014259*la^2 + (1101023 - 567230*la)*
           m^2 + 12954*m^4))*M^2 + 27*(-721290619 + 64584864*En^2 + 
        297872852*la - 54287130*m^2)*M^4)*rp[t]^5 + 
    J^16*M^2*(J^6*(113400 - 2070540*la + 4584658*la^2 - 1622556*la^3 + 
        38649*la^4 + 108*la^5 + (141795 - 1563984*la + 1752907*la^2 - 
          116735*la^3 + 300*la^4)*m^2 - 3*(-3480 + 23862*la - 15111*la^2 + 
          92*la^3)*m^4 + 15*(3 - 12*la + 4*la^2)*m^6) + 
      J^4*(291552102 - 1046356080*la + 661063683*la^2 - 72826329*la^3 + 
        451432*la^4 + (146202867 - 292758252*la + 77352662*la^2 - 
          1172480*la^3)*m^2 + (3895803 - 4212288*la + 425476*la^2)*m^4 - 
        480*(-9 + 4*la)*m^6 - 6*En^2*(6630897 - 19657606*la + 9023314*la^2 - 
          524219*la^3 + (3283305 - 4717132*la + 668944*la^2)*m^2 + 
          (93714 - 53052*la)*m^4 + 120*m^6))*M^2 + 
      9*J^2*(1807960903 + 5344200*En^4 - 1827577616*la + 248139958*la^2 + 
        (311013843 - 117194744*la)*m^2 + 1787904*m^4 - 
        6*En^2*(50861209 - 27385746*la + 5618276*m^2))*M^4 + 30200918040*M^6)*
     rp[t]^6 + J^16*M*(-2*J^6*la*(-39960 + 220647*la - 136535*la^2 + 
        5165*la^3 + 27*la^4 + (-49545 + 144076*la - 15890*la^2 + 75*la^3)*
         m^2 + (-3585 + 6194*la - 69*la^2)*m^4 + 15*(-1 + la)*m^6) - 
      J^4*(32367438 - 228394494*la + 261420959*la^2 - 53140348*la^3 + 
        752916*la^4 + 728*la^5 + (25019253 - 104137473*la + 56260694*la^2 - 
          1995660*la^3 + 2040*la^4)*m^2 + (1076733 - 2669634*la + 
          729484*la^2 - 1656*la^3)*m^4 + 80*(27 - 36*la + 4*la^2)*m^6 + 
        2*En^2*(-2425383 + 15541818*la - 14879105*la^2 + 2169998*la^3 - 
          6456*la^4 + (-2032623 + 6982461*la - 2744146*la^2 + 55500*la^3)*
           m^2 - 12*(9129 - 15998*la + 2299*la^2)*m^4 + 120*(-3 + 2*la)*m^6))*
       M^2 - 6*J^2*(1154506794 - 2244376641*la + 740056959*la^2 - 
        33926883*la^3 + (346896933 - 334771601*la + 34388337*la^2)*m^2 - 
        64*(-72366 + 28627*la)*m^4 + 1680*m^6 + 
        36*En^4*(236566 - 183619*la + 39437*m^2) - 
        6*En^2*(45268704 - 63301078*la + 11188018*la^2 + 
          (11822681 - 6037058*la)*m^2 + 126952*m^4))*M^4 + 
      27*(-2329175873 + 326113920*En^2 + 956399548*la - 163901424*m^2)*M^6)*
     rp[t]^7 + J^14*(J^8*la^2*(14400 - 17332*la + 1009*la^2 + 9*la^3 + 
        5*(3508 - 632*la + 5*la^2)*m^2 + (1235 - 23*la)*m^4 + 5*m^6) + 
      J^6*(1332450 - 24280542*la + 53480375*la^2 - 18717605*la^3 + 
        462606*la^4 + 1092*la^5 + (1613475 - 17681799*la + 19647838*la^2 - 
          1253720*la^3 + 3060*la^4)*m^2 - 3*(-36345 + 246098*la - 
          153939*la^2 + 828*la^3)*m^4 + 120*(3 - 12*la + 4*la^2)*m^6 - 
        2*En^2*(99225 - 1807497*la + 3683273*la^2 - 1071751*la^3 + 
          6890*la^4 + 124*la^5 - 2*(-71235 + 713037*la - 664741*la^2 + 
            33070*la^3 + 10*la^4)*m^2 - 3*(-4545 + 24933*la - 11086*la^2 + 
            36*la^3)*m^4 + 10*(9 - 24*la + 4*la^2)*m^6))*M^2 + 
      J^4*(1563746094 - 5590972560*la + 3508181847*la^2 - 382466949*la^3 + 
        2456444*la^4 + (748244844 - 1487980965*la + 389119159*la^2 - 
          5620900*la^3)*m^2 + 4*(4438773 - 4749408*la + 474776*la^2)*m^4 - 
        1680*(-9 + 4*la)*m^6 + 36*En^4*(3*(168204 - 378641*la + 95629*la^2) + 
          (210173 - 167550*la)*m^2 + 4152*m^4) - 
        6*En^2*(74398443 - 219344951*la + 99907281*la^2 - 5755378*la^3 + 
          (35530773 - 50568924*la + 7083200*la^2)*m^2 + (931932 - 518816*la)*
           m^4 + 840*m^6))*M^4 + 18*J^2*(2922590457 + 28567548*En^4 - 
        2938602184*la + 395655141*la^2 + (471777348 - 176413792*la)*m^2 + 
        2336208*m^4 + En^2*(-773131290 + 413382435*la - 81113472*m^2))*M^6 + 
      65457299448*M^8)*rp[t]^8 + 
    J^14*M*(J^6*la*(939060 - 5162169*la + 3152299*la^2 - 123635*la^3 - 
        546*la^4 + (1127325 - 3250951*la + 343215*la^2 - 1530*la^3)*m^2 + 
        2*(37395 - 63721*la + 621*la^2)*m^4 - 240*(-1 + la)*m^6 + 
        2*En^2*(-69030 + 390429*la - 221513*la^2 + 2343*la^3 + 124*la^4 - 
          (98415 - 264671*la + 25445*la^2 + 20*la^3)*m^2 - 
          4*(2325 - 3233*la + 27*la^2)*m^4 + 20*(-3 + 2*la)*m^6)) - 
      J^4*(173810097 - 1222556580*la + 1390252444*la^2 - 279383160*la^3 + 
        4100976*la^4 + 3280*la^5 + 4*(32194881 - 133042914*la + 
          71144937*la^2 - 2402300*la^3 + 2340*la^4)*m^2 - 
        4*(-1239813 + 3036654*la - 820124*la^2 + 1656*la^3)*m^4 + 
        280*(27 - 36*la + 4*la^2)*m^6 + 12*En^4*(201951 - 1166562*la + 
          849809*la^2 - 63408*la^3 + (166989 - 442730*la + 96624*la^2)*m^2 + 
          (8838 - 8440*la)*m^4 + 24*m^6) + 2*En^2*(-27302805 + 174107010*la - 
          165420301*la^2 + 23898742*la^3 - 83208*la^4 + 
          (-22194009 + 75473139*la - 29292182*la^2 + 567780*la^3)*m^2 - 
          24*(46176 - 79387*la + 11186*la^2)*m^4 + 840*(-3 + 2*la)*m^6))*
       M^2 + 6*J^2*(-3737335077 + 621000*En^6 + 7230048381*la - 
        2364612927*la^2 + 107133277*la^3 - 8*(132266913 - 126627191*la + 
          12852535*la^2)*m^2 + 112*(-108903 + 42686*la)*m^4 - 3360*m^6 - 
        36*En^4*(2548430 - 1959901*la + 408666*m^2) + 
        6*En^2*(230241172 - 319775645*la + 56013091*la^2 + 
          (57336332 - 28981496*la)*m^2 + 547792*m^4))*M^4 + 
      54*(-2527448105 + 493621152*En^2 + 1030965564*la - 163975266*m^2)*M^6)*
     rp[t]^9 + J^12*(J^8*la^2*(169200 - 200198*la + 12066*la^2 + 91*la^3 + 
        5*(39894 - 6871*la + 51*la^2)*m^2 + (12860 - 207*la)*m^4 + 40*m^6 - 
        2*En^2*(12720 - 15828*la + 256*la^2 + 31*la^3 - 
          5*(-3518 + 625*la + la^2)*m^2 + (1600 - 27*la)*m^4 + 10*m^6)) + 
      J^6*(7164045 - 130233420*la + 285131782*la^2 - 98513759*la^3 + 
        2521923*la^4 + 4920*la^5 + 3*(2786910 - 30309338*la + 33341557*la^2 - 
          2022575*la^3 + 4680*la^4)*m^2 + (508725 - 3394776*la + 
          2095308*la^2 - 9936*la^3)*m^4 + 420*(3 - 12*la + 4*la^2)*m^6 - 
        12*En^4*(-7560 + 148104*la - 275428*la^2 + 56317*la^3 + 703*la^4 + 
          2*(-6159 + 57931*la - 42065*la^2 + 953*la^3)*m^2 - 
          4*(360 - 1497*la + 362*la^2)*m^4 + 4*(-3 + 4*la)*m^6) - 
        2*En^2*(1119825 - 20328840*la + 41130996*la^2 - 11841780*la^3 + 
          90250*la^4 + 1252*la^5 - 2*(-785925 + 7780431*la - 7161136*la^2 + 
            341830*la^3 + 70*la^4)*m^2 + (140805 - 755472*la + 328484*la^2 - 
            864*la^3)*m^4 + 70*(9 - 24*la + 4*la^2)*m^6))*M^2 + 
      J^4*(5069161548 - 18045180414*la + 11233852725*la^2 - 1209424011*la^3 + 
        8028136*la^4 - 4*(-574090605 + 1132167732*la - 292509770*la^2 + 
          3993680*la^3)*m^2 + 28*(1685151 - 1784016*la + 176512*la^2)*m^4 - 
        3360*(-9 + 4*la)*m^6 - 216*En^6*(9496 - 13149*la + 2402*m^2) + 
        36*En^4*(5479827 - 12225071*la + 3055903*la^2 - 
          4*(-552363 + 433757*la)*m^2 + 40744*m^4) - 
        6*En^2*(379920792 - 1112900317*la + 502362811*la^2 - 28646411*la^3 + 
          8*(21729450 - 30595741*la + 4223786*la^2)*m^2 - 
          308*(-13299 + 7252*la)*m^4 + 2520*m^6))*M^4 + 
      18*J^2*(6351626457 + 137881728*En^4 - 6347322584*la + 846178498*la^2 + 
        (949717755 - 351858864*la)*m^2 + 3918012*m^4 - 
        12*En^2*(195861911 - 103866590*la + 19344976*m^2))*M^6 + 
      100301964336*M^8)*rp[t]^10 + 
    J^12*M*(J^6*la*(5049000 - 27612951*la + 16608501*la^2 - 674413*la^3 - 
        2460*la^4 + (5840385 - 16677557*la + 1671275*la^2 - 7020*la^3)*m^2 + 
        (348390 - 584488*la + 4968*la^2)*m^4 - 840*(-1 + la)*m^6 - 
        4*En^4*(-15525 + 97785*la - 46173*la^2 - 1696*la^3 + 38*la^4 - 
          2*(12594 - 32685*la + 1938*la^2 + 29*la^3)*m^2 + 
          (-2922 + 3098*la)*m^4 + 8*(-3 + la)*m^6) + 
        2*En^2*(-778905 + 4380072*la - 2455468*la^2 + 31159*la^3 + 
          1252*la^4 - (1085625 - 2881123*la + 266195*la^2 + 140*la^3)*m^2 - 
          6*(15995 - 21676*la + 144*la^2)*m^4 + 140*(-3 + 2*la)*m^6)) + 
      J^4*(-564297057 + 3954315168*la - 4463092650*la^2 + 884748870*la^3 - 
        13422344*la^4 - 8640*la^5 - 4*(99495351 - 407593935*la + 
          215326410*la^2 - 6858460*la^3 + 6360*la^4)*m^2 + 
        28*(-475821 + 1150938*la - 307228*la^2 + 552*la^3)*m^4 - 
        560*(27 - 36*la + 4*la^2)*m^6 + 72*En^6*(3897 - 20833*la + 
          8896*la^2 + (2721 - 4870*la)*m^2 + 96*m^4) - 
        12*En^4*(2210067 - 12663108*la + 9129607*la^2 - 677556*la^3 + 
          (1784037 - 4653166*la + 998748*la^2)*m^2 + (89106 - 82664*la)*m^4 + 
          144*m^6) - 4*En^2*(-69992640 + 443771103*la - 417893119*la^2 + 
          59695936*la^3 - 238620*la^4 + 4*(-13714299 + 46094901*la - 
            17630513*la^2 + 323520*la^3)*m^2 - 84*(29673 - 49731*la + 
            6848*la^2)*m^4 + 1260*(-3 + 2*la)*m^6))*M^2 + 
      12*J^2*(-4067270811 + 3330072*En^6 + 7824337731*la - 2534528031*la^2 + 
        113165069*la^3 - 7*(153204411 - 145229977*la + 14536087*la^2)*m^2 + 
        28*(-368049 + 142973*la)*m^4 - 2100*m^6 - 
        36*En^4*(6199715 - 4719566*la + 947037*m^2) + 
        12*En^2*(175812104 - 242187295*la + 41977856*la^2 - 
          7*(-5912451 + 2953006*la)*m^2 + 336546*m^4))*M^4 + 
      378*(-554144485 + 142044912*En^2 + 224276908*la - 32473092*m^2)*M^6)*
     rp[t]^11 + J^10*(J^8*la^2*(909720 - 1055812*la + 65820*la^2 + 410*la^3 + 
        5*(206589 - 33710*la + 234*la^2)*m^2 + (59785 - 828*la)*m^4 + 
        140*m^6 + 4*En^4*(3120 - 3783*la - 302*la^2 + 19*la^3 + 
          (4744 - 611*la - 29*la^2)*m^2 + 508*m^4 + 4*m^6) - 
        2*En^2*(143310 - 176004*la + 3418*la^2 + 313*la^3 - 
          5*(-38744 + 6631*la + 7*la^2)*m^2 + (16465 - 216*la)*m^4 + 
          70*m^6)) + J^6*(24*En^6*(-270 + 8154*la - 13857*la^2 + 812*la^3 + 
          (-480 + 5458*la - 2524*la^2)*m^2 + 4*(-15 + 47*la)*m^4) - 
        12*En^4*(-83160 + 1620588*la - 2983536*la^2 + 606625*la^3 + 
          6243*la^4 + 2*(-66879 + 618942*la - 441338*la^2 + 9942*la^3)*m^2 - 
          4*(3735 - 15075*la + 3526*la^2)*m^4 + 24*(-3 + 4*la)*m^6) - 
        2*En^2*(5760720 - 104132448*la + 208918861*la^2 - 59388527*la^3 + 
          525128*la^4 + 5648*la^5 + (7871850 - 76868334*la + 69704876*la^2 - 
            3154120*la^3 - 400*la^4)*m^2 - 7*(-93285 + 484356*la - 
            204812*la^2 + 432*la^3)*m^4 + 210*(9 - 24*la + 4*la^2)*m^6) + 
        3*(2*(3883005 - 70378911*la + 153019376*la^2 - 52074371*la^3 + 
            1377727*la^4 + 2160*la^5) + (8681295 - 93565846*la + 
            101688440*la^2 - 5805360*la^3 + 12720*la^4)*m^2 - 
          7*(-65955 + 433544*la - 264012*la^2 + 1104*la^3)*m^4 + 
          280*(3 - 12*la + 4*la^2)*m^6))*M^2 - 
      2*J^4*(-5525982270 + 3240*En^8 + 19572388866*la - 12073043829*la^2 + 
        1279837947*la^3 - 8754052*la^4 + 7*(-335144979 + 653962641*la - 
          166537229*la^2 + 2128340*la^3)*m^2 - 
        7*(5745843 - 6020208*la + 589756*la^2)*m^4 + 2100*(-9 + 4*la)*m^6 + 
        216*En^6*(51856 - 70933*la + 13079*m^2) - 
        72*En^4*(6722202 - 14852681*la + 3668194*la^2 + 
          (2606294 - 2008857*la)*m^2 + 44778*m^4) + 
        6*En^2*(583176243 - 1694739088*la + 756854510*la^2 - 42629020*la^3 + 
          28*(9046431 - 12588937*la + 1707602*la^2)*m^2 - 
          42*(-122071 + 65208*la)*m^4 + 2100*m^6))*M^4 + 
      36*J^2*(198502560*En^4 - 21*En^2*(113307027 - 59508083*la + 
          10436420*m^2) + 7*(697404285 - 691966628*la + 91148297*la^2 + 
          (94750833 - 34716580*la)*m^2 + 312138*m^4))*M^6 + 111231007272*M^8)*
     rp[t]^12 + J^10*M*(J^6*la*(16420050 - 89259225*la + 52756314*la^2 - 
        2213672*la^3 - 6480*la^4 - 5*(-3637977 + 10266854*la - 965612*la^2 + 
          3816*la^3)*m^2 + 14*(67635 - 111688*la + 828*la^2)*m^4 - 
        1680*(-1 + la)*m^6 + 8*En^6*(-4*(135 - 1446*la + 282*la^2 + 
            89*la^3) + (-960 + 3125*la + 202*la^2)*m^2 + 
          8*(-15 + 11*la)*m^4) - 4*En^4*
         (3*(-56925 + 355323*la - 166847*la^2 - 5186*la^3 + 122*la^4) - 
          (273588 - 696089*la + 41458*la^2 + 456*la^3)*m^2 + 
          6*(-5057 + 5165*la)*m^4 + 48*(-3 + la)*m^6) + 
        2*En^2*(-4006305 + 22376178*la - 12364397*la^2 + 183758*la^3 + 
          5648*la^4 - 5*(1086939 - 2839392*la + 249296*la^2 + 80*la^3)*m^2 - 
          14*(31725 - 41504*la + 216*la^2)*m^4 + 420*(-3 + 2*la)*m^6)) - 
      J^4*(1232854803 - 8599789944*la + 6480*En^8*la + 9622240206*la^2 - 
        1876209660*la^3 + 29328616*la^4 + 14640*la^5 + 
        14*(58627962 - 237431241*la + 123573138*la^2 - 3671980*la^3 + 
          3240*la^4)*m^2 - 14*(-1639683 + 3918114*la - 1034164*la^2 + 
          1656*la^3)*m^4 + 700*(27 - 36*la + 4*la^2)*m^6 - 
        72*En^6*(43227 - 228695*la + 97246*la^2 + (30571 - 53442*la)*m^2 + 
          1136*m^4) + 12*En^4*(10958751 - 62134836*la + 44261908*la^2 - 
          3254664*la^3 + 3*(2885397 - 7317870*la + 1538668*la^2)*m^2 - 
          6*(-69093 + 60404*la)*m^4 + 360*m^6) + 
        4*En^2*(-215829144 + 1359467817*la - 1266425318*la^2 + 
          178477952*la^3 - 806976*la^4 + 28*(-5760594 + 19160343*la - 
            7204621*la^2 + 122880*la^3)*m^2 - 84*(76026 - 124217*la + 
            16726*la^2)*m^4 + 2100*(-3 + 2*la)*m^6))*M^2 + 
      12*J^2*(15820920*En^6 - 36*En^4*(17994537 - 13541626*la + 
          2560145*m^2) + 42*En^2*(102395576 - 139559989*la + 23892081*la^2 + 
          (22621631 - 11105390*la)*m^2 + 146470*m^4) - 
        7*(894550536 - 1710064737*la + 547605993*la^2 - 23999061*la^3 + 
          2*(107937525 - 101081944*la + 9955047*la^2)*m^2 + 
          (1653936 - 637012*la)*m^4 + 240*m^6))*M^4 + 
      378*(-615862421 + 199302720*En^2 + 246828364*la - 31577934*m^2)*M^6)*
     rp[t]^13 + J^8*(J^8*la^2*(2958255 - 3358698*la + 216280*la^2 + 
        1080*la^3 + 10*(321537 - 49082*la + 318*la^2)*m^2 - 
        7*(-23165 + 276*la)*m^4 + 280*m^6 - 8*En^6*(180 - 146*la - 93*la^2 + 
          3*la^3 + (264 + 54*la - 10*la^2)*m^2 + 4*(6 + la)*m^4) + 
        12*En^4*(11350 - 13708*la - 961*la^2 + 61*la^3 - 
          2*(-8523 + 1106*la + 38*la^2)*m^2 + 1744*m^4 + 8*m^6) - 
        2*En^2*(736170 - 890124*la + 20252*la^2 + 1412*la^3 - 
          10*(-96831 + 15823*la + 10*la^2)*m^2 + (75915 - 756*la)*m^4 + 
          210*m^6)) + J^6*(-2160*En^8*la^2 + 24*En^6*(-2970 + 90504*la - 
          152709*la^2 + 10474*la^3 + (-5280 + 61478*la - 28340*la^2)*m^2 + 
          4*(-165 + 562*la)*m^4) - 12*En^4*(-427140 + 8041632*la - 
          14618457*la^2 + 2940452*la^3 + 24504*la^4 + 
          (-690030 + 6003804*la - 4161700*la^2 + 91548*la^3)*m^2 - 
          4*(18945 - 69933*la + 15374*la^2)*m^4 + 60*(-3 + 4*la)*m^6) - 
        2*En^2*(17724420 - 320793111*la + 637136678*la^2 - 178394716*la^3 + 
          1804024*la^4 + 14992*la^5 - 14*(-1662705 + 16138527*la - 
            14412666*la^2 + 606580*la^3 + 40*la^4)*m^2 - 
          21*(-82125 + 412704*la - 169828*la^2 + 288*la^3)*m^4 + 
          350*(9 - 24*la + 4*la^2)*m^6) + 3*(17020395 - 307059858*la + 
          662204972*la^2 - 221329068*la^3 + 6033406*la^4 + 7320*la^5 + 
          14*(1293105 - 13763275*la + 14729147*la^2 - 781295*la^3 + 
            1620*la^4)*m^2 - 7*(-115125 + 745756*la - 448278*la^2 + 
            1656*la^3)*m^4 + 350*(3 - 12*la + 4*la^2)*m^6))*M^2 - 
      2*J^4*(35640*En^8 + 216*En^6*(255996 - 339381*la + 65677*m^2) - 
        72*En^4*(3*(6492795 - 14333383*la + 3490180*la^2) + 
          (7045243 - 5426305*la)*m^2 + 107430*m^4) + 
        42*En^2*(171519738 - 491828402*la + 216757680*la^2 - 12024661*la^3 + 
          (70262691 - 95987904*la + 12707720*la^2)*m^2 - 
          20*(-56736 + 29743*la)*m^4 + 300*m^6) + 
        7*(-1217149632 + 4288407804*la - 2616910305*la^2 + 272051955*la^3 - 
          1910648*la^4 + (-477191871 + 918615612*la - 229965070*la^2 + 
            2721760*la^3)*m^2 + (-6509943 + 6753888*la - 655436*la^2)*m^4 + 
          240*(-9 + 4*la)*m^6))*M^4 + 18*J^2*(763905960*En^4 - 
        42*En^2*(159598109 - 83093850*la + 13519404*m^2) + 
        7*(1553389689 - 1527635792*la + 198195450*la^2 + 
          (185955897 - 67258472*la)*m^2 + 462864*m^4))*M^6 + 89680212720*M^8)*
     rp[t]^14 + J^8*M*(-(J^6*la*(-35978040 + 194046645*la - 112368156*la^2 + 
         4857808*la^3 + 10980*la^4 + 720*En^8*la*(1 + la) + 
         7*(-5416755 + 15046244*la - 1308140*la^2 + 4860*la^3)*m^2 - 
         14*(117855 - 191672*la + 1242*la^2)*m^4 + 2100*(-1 + la)*m^6 - 
         8*En^6*(-5940 + 62859*la - 13923*la^2 - 3286*la^3 + 
           (-10560 + 34615*la + 1262*la^2)*m^2 + 8*(-165 + 136*la)*m^4) + 
         4*En^4*(-871965 + 5288250*la - 2452092*la^2 - 64664*la^3 + 
           1552*la^4 - (1404900 - 3371815*la + 198862*la^2 + 1568*la^3)*m^2 + 
           18*(-8525 + 7929*la)*m^4 + 120*(-3 + la)*m^6) - 
         2*En^2*(-12347055 + 68682939*la - 37309558*la^2 + 642100*la^3 + 
           14992*la^4 - 35*(459711 - 1188880*la + 97408*la^2 + 16*la^3)*m^2 - 
           14*(83595 - 105668*la + 432*la^2)*m^4 + 700*(-3 + 2*la)*m^6))) - 
      J^4*(71280*En^8*la - 72*En^6*(271437 - 1142421*la + 471506*la^2 + 
          (196637 - 270318*la)*m^2 + 7360*m^4) + 
        12*En^4*(30089997 - 180061668*la + 127466812*la^2 - 9271976*la^3 + 
          (22853553 - 59441494*la + 12419980*la^2)*m^2 + 
          (1045530 - 867560*la)*m^4 + 480*m^6) + 
        28*En^2*(-64609785 + 398531127*la - 365445409*la^2 + 50653570*la^3 - 
          254904*la^4 + (-45493497 + 148416627*la - 54406562*la^2 + 
            838500*la^3)*m^2 - 60*(24054 - 38413*la + 5069*la^2)*m^4 + 
          300*(-3 + 2*la)*m^6) + 7*(271914759 - 1889105364*la + 
          2093260350*la^2 - 399864960*la^3 + 6416456*la^4 + 2352*la^5 + 
          2*(84542427 - 337156029*la + 172300014*la^2 - 4717580*la^3 + 
            3960*la^4)*m^2 + (3752946 - 8865108*la + 2315128*la^2 - 
            3312*la^3)*m^4 + 80*(27 - 36*la + 4*la^2)*m^6))*M^2 + 
      12*J^2*(40358520*En^6 - 36*En^4*(35866529 - 25900301*la + 
          4676490*m^2) + 42*En^2*(2*(72420812 - 97937266*la + 
            16558307*la^2) + (29871257 - 14310290*la)*m^2 + 141316*m^4) - 
        7*(998046882 - 1893468519*la + 597582417*la^2 - 25551901*la^3 + 
          (214124475 - 197639395*la + 19112579*la^2)*m^2 - 
          16*(-77169 + 29483*la)*m^4 + 120*m^6))*M^4 + 
      378*(-498331231 + 199049856*En^2 + 197105508*la - 21250440*m^2)*M^6)*
     rp[t]^15 + J^6*(J^8*la^2*(6479460 - 7170642*la + 475670*la^2 + 
        1830*la^3 + 35*(191313 - 26816*la + 162*la^2)*m^2 - 
        7*(-40285 + 414*la)*m^4 + 350*m^6 - 8*En^6*(1890 - 1561*la - 
          893*la^2 + 28*la^3 + (2744 + 474*la - 70*la^2)*m^2 + 
          4*(61 + 6*la)*m^4) + 4*En^4*(172500 - 205491*la - 12919*la^2 + 
          776*la^3 + (259794 - 33640*la - 784*la^2)*m^2 + 26184*m^4 + 
          60*m^6) - 2*En^2*(2266905 - 2695773*la + 71788*la^2 + 3748*la^3 - 
          70*(-40949 + 6311*la + 2*la^2)*m^2 - 7*(-28475 + 216*la)*m^4 + 
          350*m^6)) + J^6*(-23760*En^8*la^2 + 24*En^6*(-82890 + 574032*la - 
          779527*la^2 + 55542*la^3 - 2*(43440 - 198257*la + 73422*la^2)*m^2 + 
          20*(-354 + 733*la)*m^4) - 12*En^4*(-733320 + 22289184*la - 
          42188475*la^2 + 8431620*la^3 + 52528*la^4 + 
          2*(-816495 + 7965866*la - 5636762*la^2 + 119230*la^3)*m^2 - 
          20*(10260 - 35187*la + 7330*la^2)*m^4 + 80*(-3 + 4*la)*m^6) - 
        14*En^2*(5530275 - 95688972*la + 185668420*la^2 - 50964758*la^3 + 
          579812*la^4 + 3704*la^5 + (6679350 - 63646086*la + 55370464*la^2 - 
            2095360*la^3 - 40*la^4)*m^2 - 5*(-80055 + 390918*la - 
            157016*la^2 + 216*la^3)*m^4 + 50*(9 - 24*la + 4*la^2)*m^6) + 
        21*(2*(1879875 - 33813315*la + 72326459*la^2 - 23654659*la^3 + 
            661750*la^4 + 588*la^5) + (3789345 - 39613480*la + 
            41564756*la^2 - 2018560*la^3 + 3960*la^4)*m^2 + 
          (133365 - 852116*la + 505998*la^2 - 1656*la^3)*m^4 + 
          40*(3 - 12*la + 4*la^2)*m^6))*M^2 + 
      2*J^4*(1093176*En^8 - 216*En^6*(380756 - 872273*la + 122155*m^2) + 
        36*En^4*(83431356 - 169862225*la + 39731735*la^2 + 
          (26362605 - 19735920*la)*m^2 + 298660*m^4) - 
        42*En^2*(243937326 - 694076365*la + 302485717*la^2 - 16484090*la^3 + 
          (95012475 - 126127688*la + 16162696*la^2)*m^2 - 
          118*(-9417 + 4856*la)*m^4 + 180*m^6) - 
        7*(-1359473490 + 4762844676*la - 2867595825*la^2 + 290510043*la^3 - 
          2086372*la^4 + (-479532258 + 907946403*la - 222861413*la^2 + 
            2416220*la^3)*m^2 + (-4898502 + 5035392*la - 484384*la^2)*m^4 + 
          120*(-9 + 4*la)*m^6))*M^4 + 36*J^2*(4415361717 + 485511660*En^4 - 
        4288679624*la + 545259397*la^2 - 6*(-73762727 + 26295296*la)*m^2 + 
        769776*m^4 - 21*En^2*(159737336 - 82463539*la + 11973904*m^2))*M^6 + 
      51811246620*M^8)*rp[t]^16 + 
    J^6*M*(-(J^6*la*(7920*En^8*la*(1 + la) - 8*En^6*(-158220 + 401373*la - 
           80109*la^2 - 14666*la^3 + (-171060 + 221297*la + 2770*la^2)*m^2 + 
           40*(-354 + 181*la)*m^4) + 4*En^4*(-1581255 + 14747346*la - 
           7035108*la^2 - 149744*la^3 + 3808*la^4 - 
           (3349920 - 9013341*la + 539610*la^2 + 3080*la^3)*m^2 + 
           10*(-41439 + 35719*la)*m^4 + 160*(-3 + la)*m^6) + 
         7*(-7945830 + 42590931*la - 24098562*la^2 + 1068806*la^3 + 
           1764*la^4 + (-7929345 + 21551464*la - 1701070*la^2 + 5940*la^3)*
            m^2 + (-272610 + 437084*la - 2484*la^2)*m^4 + 
           240*(-1 + la)*m^6) - 14*En^2*(-3840525 + 20334399*la - 
           10730636*la^2 + 210686*la^3 + 3704*la^4 - 
           (4622175 - 11655626*la + 854350*la^2 + 40*la^3)*m^2 - 
           30*(9035 - 11084*la + 36*la^2)*m^4 + 100*(-3 + 2*la)*m^6))) - 
      J^4*(432*En^8*(17184 - 3301*la + 2400*m^2) - 
        72*En^6*(-938697 - 1833771*la + 1217654*la^2 + (126175 - 516090*la)*
           m^2 + 20000*m^4) + 12*En^4*(77410557 - 379555488*la + 
          248190302*la^2 - 17507560*la^3 + 5*(8709177 - 22186318*la + 
            4458436*la^2)*m^2 + (1509330 - 1203880*la)*m^4 + 360*m^6) + 
        28*En^2*(-92803347 + 565962369*la - 513721433*la^2 + 70026338*la^3 - 
          384792*la^4 + (-63441387 + 199837029*la - 70602214*la^2 + 
            950700*la^3)*m^2 - 12*(119916 - 187767*la + 24346*la^2)*m^4 + 
          180*(-3 + 2*la)*m^6) + 7*(303572583 - 2103694488*la + 
          2304313428*la^2 - 428444088*la^3 + 7026064*la^4 + 1728*la^5 + 
          2*(86353587 - 337727502*la + 168908856*la^2 - 4207240*la^3 + 
            3360*la^4)*m^2 - 4*(-712611 + 1665378*la - 430628*la^2 + 
            552*la^3)*m^4 + 40*(27 - 36*la + 4*la^2)*m^6))*M^2 + 
      12*J^2*(-5693124357 + 120400128*En^6 + 10682232453*la - 
        3304988127*la^2 + 136609429*la^3 - 4*(258005481 - 234252257*la + 
          22206419*la^2)*m^2 + 8*(-516639 + 195908*la)*m^4 - 240*m^6 - 
        36*En^4*(46004743 - 33011307*la + 5997831*m^2) + 
        42*En^2*(144853796 - 195244177*la + 32525237*la^2 + 
          (27110514 - 12598612*la)*m^2 + 84288*m^4))*M^4 + 
      27*(-4056516433 + 1998148800*En^2 + 1574818108*la - 132400794*m^2)*M^6)*
     rp[t]^17 + J^4*(J^8*la^2*(-8*En^6*(31590 - 12875*la - 4583*la^2 + 
          112*la^3 + (33400 + 1430*la - 210*la^2)*m^2 + 20*(121 + 3*la)*
           m^4) + 7*(1430865 - 1543566*la + 105004*la^2 + 294*la^3 + 
          10*(139782 - 17579*la + 99*la^2)*m^2 + (46505 - 414*la)*m^4 + 
          40*m^6) - 14*En^2*(699225 - 779790*la + 24116*la^2 + 926*la^3 - 
          10*(-82205 + 11296*la + la^2)*m^2 - 5*(-9205 + 54*la)*m^4 + 
          50*m^6) + 4*En^4*(345540 - 581265*la - 32641*la^2 + 1904*la^3 - 
          10*(-63167 + 9670*la + 154*la^2)*m^2 + 70220*m^4 + 80*m^6)) + 
      J^6*(-432*En^8*(-8400 + 11896*la - 547*la^2 + 120*(-21 + 13*la)*m^2) + 
        24*En^6*(1021410 - 1761636*la - 1390601*la^2 + 149490*la^3 + 
          (134160 + 276070*la - 297300*la^2)*m^2 + 40*(-573 + 998*la)*m^4) - 
        12*En^4*(-3953880 + 55758156*la - 86303301*la^2 + 16120166*la^3 + 
          61614*la^4 + 30*(-100953 + 1013216*la - 693548*la^2 + 12720*la^3)*
           m^2 - 20*(15570 - 50703*la + 10142*la^2)*m^4 + 
          60*(-3 + 4*la)*m^6) - 14*En^2*(8178975 - 137193255*la + 
          263075474*la^2 - 71143612*la^3 + 891364*la^4 + 4360*la^5 + 
          (9720810 - 88477002*la + 73704676*la^2 - 2403920*la^3 + 40*la^4)*
           m^2 + (408105 - 1945392*la + 765364*la^2 - 864*la^3)*m^4 + 
          30*(9 - 24*la + 4*la^2)*m^6) + 21*(4180275 - 75413286*la + 
          160020940*la^2 - 50891114*la^3 + 1453870*la^4 + 864*la^5 + 
          2*(1976925 - 20177977*la + 20666979*la^2 - 904985*la^3 + 1680*la^4)*
           m^2 + (102435 - 646264*la + 379452*la^2 - 1104*la^3)*m^4 + 
          20*(3 - 12*la + 4*la^2)*m^6))*M^2 - 
      2*J^4*(-7774400142 + 46066968*En^8 + 27002630970*la - 
        15952915581*la^2 + 1558813587*la^3 - 11396072*la^4 + 
        2*(-1173401181 + 2179605300*la - 523471466*la^2 + 5145200*la^3)*m^2 - 
        2*(8259273 - 8417808*la + 803176*la^2)*m^4 + 240*(-9 + 4*la)*m^6 + 
        216*En^6*(2911408 - 2452682*la + 258035*m^2) - 
        36*En^4*(109472061 - 218729909*la + 50951571*la^2 + 
          (36016754 - 25100270*la)*m^2 + 240396*m^4) + 
        42*En^2*(242200347 - 693270119*la + 299317699*la^2 - 15898697*la^3 + 
          4*(22242933 - 28451189*la + 3506290*la^2)*m^2 + 
          (671358 - 341304*la)*m^4 + 60*m^6))*M^4 + 
      9*J^2*(10342686849 + 1655482176*En^4 - 9860203288*la + 
        1219141762*la^2 + (796910991 - 279690224*la)*m^2 + 850716*m^4 - 
        24*En^2*(403465815 - 204616030*la + 24280376*m^2))*M^6 + 
      20544497208*M^8)*rp[t]^18 + 
    J^4*M*(J^6*la*(-432*En^8*(-5500 + 2487*la + 131*la^2 + 
          240*(-7 + la)*m^2) + 8*En^6*(2005560 - 658461*la - 163959*la^2 - 
          37406*la^3 + 5*(54564 + 42991*la + 398*la^2)*m^2 + 
          80*(-573 + 248*la)*m^4) - 4*En^4*
         (3*(-2713005 + 11497440*la - 4512178*la^2 - 66024*la^3 + 
            1988*la^4) - 5*(1255632 - 3413267*la + 178966*la^2 + 756*la^3)*
           m^2 + 30*(-20921 + 17097*la)*m^4 + 120*(-3 + la)*m^6) - 
        7*(-8839800 + 47337213*la - 26044542*la^2 + 1178434*la^3 + 
          1296*la^4 + (-8260125 + 21831998*la - 1535270*la^2 + 5040*la^3)*
           m^2 - 2*(104535 - 165428*la + 828*la^2)*m^4 + 120*(-1 + la)*m^6) + 
        14*En^2*(-5651865 + 29088549*la - 15146362*la^2 + 331366*la^3 + 
          4360*la^4 + (-6721245 + 16049778*la - 994610*la^2 + 40*la^3)*m^2 + 
          (-275850 + 330056*la - 864*la^2)*m^4 + 60*(-3 + 2*la)*m^6)) + 
      J^4*(-1735914267 + 2073600*En^10 + 11981208240*la - 12905240424*la^2 + 
        2308761060*la^3 - 38497376*la^4 - 5280*la^5 - 
        4*(215430921 - 823444119*la + 401972202*la^2 - 8999500*la^3 + 
          6840*la^4)*m^2 + 4*(-2423763 + 5608854*la - 1437124*la^2 + 
          1656*la^3)*m^4 - 80*(27 - 36*la + 4*la^2)*m^6 + 
        144*En^8*(848808 - 439051*la + 53520*m^2) + 
        72*En^6*(5466147 - 12104427*la + 3090202*la^2 - 85*(-3463 + 12778*la)*
           m^2 + 25760*m^4) - 12*En^4*(109440891 - 498782532*la + 
          322653314*la^2 - 22931888*la^3 + (65681919 - 150315690*la + 
            27743764*la^2)*m^2 + (1249398 - 967800*la)*m^4 + 144*m^6) - 
        28*En^2*(-90116280 + 563128815*la - 511657564*la^2 + 68297248*la^3 - 
          401112*la^4 + 4*(-15493104 + 46527264*la - 15701627*la^2 + 
            178980*la^3)*m^2 - 12*(73563 - 113281*la + 14468*la^2)*m^4 + 
          60*(-3 + 2*la)*m^6))*M^2 + 6*J^2*(238544784*En^6 + 
        3*(-2240207169 + 4126911297*la - 1240873669*la^2 + 48904359*la^3) + 
        (-941958789 + 840131123*la - 77987565*la^2)*m^2 + 
        4*(-574359 + 216283*la)*m^4 - 60*m^6 - 
        72*En^4*(37436265 - 28442358*la + 5069323*m^2) + 
        24*En^2*(366945008 - 488816305*la + 79155204*la^2 + 
          (56479913 - 25390874*la)*m^2 + 99478*m^4))*M^4 + 
      27*(-1625871275 + 1011639504*En^2 + 614940148*la - 35123412*m^2)*M^6)*
     rp[t]^19 + 
    J^2*(J^8*la^2*(2880*En^8*(144 - 7*la - la^2 + 2*(22 + la)*m^2) - 
        8*En^6*(-317070 - 15865*la - 13193*la^2 + 252*la^3 - 
          10*(3064 - 193*la + 35*la^2)*m^2 + 40*(193 + 2*la)*m^4) + 
        12*En^4*(498020 - 366595*la - 15981*la^2 + 994*la^3 - 
          30*(-13157 + 1871*la + 21*la^2)*m^2 + 35300*m^4 + 20*m^6) + 
        7*(2*(796770 - 838827*la + 58138*la^2 + 108*la^3) + 
          5*(290457 - 31988*la + 168*la^2)*m^2 + (35605 - 276*la)*m^4 + 
          20*m^6) - 14*En^2*(1021710 - 1114149*la + 39040*la^2 + 1090*la^3 + 
          10*(118887 - 13400*la + la^2)*m^2 + (46725 - 216*la)*m^4 + 
          30*m^6)) + J^6*(103680*En^10*(-21 + 13*la) + 
        144*En^8*(-267120 + 564072*la - 81381*la^2 + 40*(-525 + 904*la)*
           m^2) + 24*En^6*(-1897830 + 10979724*la - 6774061*la^2 + 
          313270*la^3 + (386760 + 657350*la - 612500*la^2)*m^2 + 
          20*(-1599 + 2573*la)*m^4) - 12*En^4*(-7182000 + 78149604*la - 
          114328263*la^2 + 21674542*la^3 + 29358*la^4 + 
          (-5593338 + 45507012*la - 27542140*la^2 + 383556*la^3)*m^2 - 
          4*(66735 - 209577*la + 40682*la^2)*m^4 + 24*(-3 + 4*la)*m^6) - 
        14*En^2*(7472250 - 133876980*la + 262645588*la^2 - 70334510*la^3 + 
          946280*la^4 + 3536*la^5 + 2*(5051925 - 43010283*la + 
            33860358*la^2 - 915140*la^3 + 40*la^4)*m^2 - 
          9*(-28335 + 132404*la - 51188*la^2 + 48*la^3)*m^4 + 
          10*(9 - 24*la + 4*la^2)*m^6) + 3*(23721390 - 430641450*la + 
          902858864*la^2 - 275642728*la^3 + 7995116*la^4 + 2640*la^5 + 
          (20239185 - 200727418*la + 199987672*la^2 - 7784400*la^3 + 
            13680*la^4)*m^2 + (351975 - 2195192*la + 1275636*la^2 - 
            3312*la^3)*m^4 + 40*(3 - 12*la + 4*la^2)*m^6))*M^2 + 
      J^4*(9254896758 + 286310160*En^8 - 31591187334*la + 18130660485*la^2 - 
        1681397811*la^3 + 12452036*la^4 + (2178299115 - 3962849943*la + 
          930026575*la^2 - 8211820*la^3)*m^2 + 
        (9244503 - 9348048*la + 885236*la^2)*m^4 - 60*(-9 + 4*la)*m^6 - 
        432*En^6*(2989600 - 2528815*la + 526981*m^2) + 
        144*En^4*(39662298 - 91127649*la + 22320092*la^2 + 
          (16423748 - 10508887*la)*m^2 + 52086*m^4) - 
        12*En^2*(1218493101 - 3494503276*la + 1474514478*la^2 - 
          74540828*la^3 + 4*(95928060 - 117830601*la + 13929226*la^2)*m^2 + 
          (1602306 - 804688*la)*m^4 + 60*m^6))*M^4 + 
      18*J^2*(2099513927 + 532886688*En^4 - 1946816524*la + 231732303*la^2 + 
        (106981945 - 36945644*la)*m^2 + 52062*m^4 - 
        3*En^2*(835088219 - 406102945*la + 33095964*m^2))*M^6 + 
      5051966436*M^8)*rp[t]^20 + 
    J^2*M*(J^6*la*(50253750 + 207360*En^10*(-7 + la) - 269044881*la + 
        141954876*la^2 - 6508388*la^3 - 3960*la^4 + 
        (42186135 - 107909722*la + 6645100*la^2 - 20520*la^3)*m^2 + 
        2*(358695 - 560944*la + 2484*la^2)*m^4 - 240*(-1 + la)*m^6 + 
        48*En^8*(-530700 + 282371*la - 7625*la^2 + 400*(-105 + 47*la)*m^2) + 
        8*En^6*(-3865320 + 5606877*la - 294105*la^2 - 57130*la^3 + 
          (764520 + 469615*la - 1570*la^2)*m^2 + 40*(-1599 + 641*la)*m^4) - 
        4*En^4*(-14431365 + 47915880*la - 18759198*la^2 - 132076*la^3 + 
          6188*la^4 - (11546388 - 24865835*la + 926486*la^2 + 2968*la^3)*
           m^2 + 18*(-29849 + 23501*la)*m^4 + 48*(-3 + la)*m^6) + 
        14*En^2*(-5164065 + 28693101*la - 15229124*la^2 + 360032*la^3 + 
          3536*la^4 + (-6964875 + 15418984*la - 767560*la^2 + 80*la^3)*m^2 - 
          2*(86055 - 100864*la + 216*la^2)*m^4 + 20*(-3 + 2*la)*m^6)) - 
      J^4*(1041459417 + 30412800*En^10 - 7083511308*la + 7413922245*la^2 - 
        1251649170*la^3 + 21104044*la^4 + 840*la^5 + 
        (408508569 - 1522828845*la + 724418730*la^2 - 14426540*la^3 + 
          10440*la^4)*m^2 + (2734263 - 6270714*la + 1593284*la^2 - 1656*la^3)*
         m^4 + 10*(27 - 36*la + 4*la^2)*m^6 - 
        144*En^8*(-1918176 + 1286665*la + 66000*m^2) - 
        72*En^6*(6869265 - 12655381*la + 3503514*la^2 + 
          (1690961 - 2165670*la)*m^2 + 15856*m^4) + 
        12*En^4*(58555917 - 376940988*la + 279800132*la^2 - 20795096*la^3 + 
          (67252197 - 135788366*la + 22706188*la^2)*m^2 + 
          (552906 - 418984*la)*m^4 + 24*m^6) + 
        4*En^2*(-436764420 + 2834421603*la - 2550490934*la^2 + 
          324675896*la^3 - 1993440*la^4 + 4*(-69962499 + 199496706*la - 
            64107343*la^2 + 604020*la^3)*m^2 - 12*(177846 - 270037*la + 
            34046*la^2)*m^4 + 60*(-3 + 2*la)*m^6))*M^2 + 
      6*J^2*(-2771820492 + 45185040*En^6 + 4955933727*la - 1430813207*la^2 + 
        52751563*la^3 + (-256564194 + 224646908*la - 20412158*la^2)*m^2 + 
        4*(-70686 + 26447*la)*m^4 - 72*En^4*(23801755 - 18115266*la + 
          2460027*m^2) + 6*En^2*(779342296 - 992735885*la + 151631839*la^2 + 
          (79083623 - 34410158*la)*m^2 + 58114*m^4))*M^4 + 
      27*(-406259311 + 331274208*En^2 + 148477636*la - 4209570*m^2)*M^6)*
     rp[t]^21 + (-(J^8*la^2*(-9081045 + 9217362*la - 645520*la^2 - 660*la^3 + 
         11520*En^10*(22 + la) - 10*(739197 - 69770*la + 342*la^2)*m^2 + 
         (-121985 + 828*la)*m^4 - 40*m^6 + 960*En^8*(4394 - 35*la + 15*la^2 + 
           (326 - 24*la)*m^2) + 40*En^6*(131526 + 497*la - 4393*la^2 + 
           70*la^3 + (-20192 + 202*la - 70*la^2)*m^2 + 4*(536 + 3*la)*m^4) + 
         14*En^2*(939480 - 1144752*la + 43724*la^2 + 884*la^3 + 
           10*(122191 - 10519*la + 2*la^2)*m^2 + (29095 - 108*la)*m^4 + 
           10*m^6) - 4*En^4*(2535420 - 1570113*la - 38507*la^2 + 3094*la^3 - 
           2*(-1045767 + 90658*la + 742*la^2)*m^2 + 90408*m^4 + 24*m^6))) + 
      J^6*(-34560*En^10*(-567 + 586*la) + 144*En^8*(448560 - 1265784*la + 
          193675*la^2 + 40*(-987 + 1118*la)*m^2) + 
        24*En^6*(-3487050 + 13635120*la - 7615231*la^2 + 547902*la^3 - 
          14*(31740 - 248087*la + 82146*la^2)*m^2 + 4*(-5145 + 7922*la)*
           m^4) - 12*En^4*(-1659420 + 44225208*la - 93630411*la^2 + 
          20453460*la^3 - 15232*la^4 + (-7051938 + 46153844*la - 
            24278076*la^2 + 236644*la^3)*m^2 - 36*(3365 - 10295*la + 
            1954*la^2)*m^4 + 4*(-3 + 4*la)*m^6) - 
        2*En^2*(31020570 - 654418953*la + 1319033666*la^2 - 340222612*la^3 + 
          4786168*la^4 + 13648*la^5 + (48719250 - 386402694*la + 
            286451396*la^2 - 6239720*la^3 + 400*la^4)*m^2 + 
          (625995 - 2877312*la + 1096124*la^2 - 864*la^3)*m^4 + 
          10*(9 - 24*la + 4*la^2)*m^6) + 3*(14281785 - 257513202*la + 
          525743182*la^2 - 150387992*la^3 + 4400479*la^4 + 420*la^5 + 
          (9866445 - 94831166*la + 91722925*la^2 - 3135785*la^3 + 5220*la^4)*
           m^2 + (100185 - 618358*la + 355959*la^2 - 828*la^3)*m^4 + 
          5*(3 - 12*la + 4*la^2)*m^6))*M^2 + 
      3*J^4*(1297920510 + 2400912*En^8 - 4287370504*la + 2354984341*la^2 - 
        202543823*la^3 + 1511768*la^4 + (201257937 - 358352228*la + 
          82144562*la^2 - 646720*la^3)*m^2 + 
        (381609 - 383104*la + 36028*la^2)*m^4 - 
        144*En^6*(-923196 - 764675*la + 510159*m^2) + 
        48*En^4*(22900677 - 58056221*la + 13928874*la^2 + 
          (8512929 - 5059627*la)*m^2 + 9418*m^4) + 
        2*En^2*(-1333783731 + 3648715126*la - 1445631430*la^2 + 
          66271295*la^3 + (-277794501 + 328105652*la - 37248832*la^2)*m^2 + 
          (-472494 + 234812*la)*m^4))*M^4 + 
      9*J^2*(1070265419 + 536927400*En^4 - 955484880*la + 108262462*la^2 + 
        (25958639 - 8820760*la)*m^2 - 6*En^2*(285229593 - 129217682*la + 
          5013780*m^2))*M^6 + 583935480*M^8)*rp[t]^22 + 
    M*(J^6*la*(30321000 - 159347865*la + 78073614*la^2 - 3599422*la^3 - 
        630*la^4 - 23040*En^10*(-567 + 146*la) - 
        5*(-4101897 + 10129516*la - 538672*la^2 + 1566*la^3)*m^2 + 
        2*(101970 - 157766*la + 621*la^2)*m^4 - 30*(-1 + la)*m^6 + 
        48*En^8*(900960 - 608677*la - 14205*la^2 + 80*(-987 + 293*la)*m^2) + 
        8*En^6*(-6910380 + 7009977*la - 714873*la^2 - 52706*la^3 + 
          (-915720 + 1878485*la - 3638*la^2)*m^2 + 8*(-5145 + 1976*la)*m^4) - 
        4*En^4*(-3164535 + 31363614*la - 18702156*la^2 - 8008*la^3 + 
          4256*la^4 - (14435988 - 24543189*la + 585738*la^2 + 1456*la^3)*
           m^2 + 2*(-121791 + 93355*la)*m^4 + 8*(-3 + la)*m^6) + 
        2*En^2*(-21698505 + 142112793*la - 75353122*la^2 + 1862068*la^3 + 
          13648*la^4 + 5*(-6689619 + 13686536*la - 529816*la^2 + 80*la^3)*
           m^2 - 6*(70325 - 81036*la + 144*la^2)*m^4 + 20*(-3 + 2*la)*m^6)) + 
      J^4*(-449834013 + 80179200*En^10 + 2947899654*la - 2940735511*la^2 + 
        455111860*la^3 - 7714516*la^4 + 360*la^5 + 
        (-115731153 + 420477969*la - 194832342*la^2 + 3423020*la^3 - 
          2360*la^4)*m^2 + (-341037 + 775746*la - 195596*la^2 + 184*la^3)*
         m^4 - 144*En^8*(449616 - 9731*la + 57360*m^2) + 
        72*En^6*(-4890561 + 2628553*la + 1823158*la^2 + 
          (2184751 - 2072282*la)*m^2 + 3776*m^4) + 
        12*En^4*(-17300151 + 225018396*la - 177845548*la^2 + 12448984*la^3 + 
          (-38159211 + 69839890*la - 10727684*la^2)*m^2 + 
          2*(-50799 + 37852*la)*m^4) + 2*En^2*(494227683 - 3061841244*la + 
          2574594761*la^2 - 293330534*la^3 + 1843512*la^4 + 
          (211548699 - 574580433*la + 176192866*la^2 - 1353180*la^3)*m^2 + 
          12*(53019 - 79548*la + 9919*la^2)*m^4))*M^2 + 
      6*J^2*(-724704834 + 82549584*En^6 + 1241121549*la - 339416243*la^2 + 
        11428983*la^3 + (-31579809 + 27147349*la - 2415045*la^2)*m^2 - 
        36*En^4*(27358864 - 17346587*la + 1019535*m^2) + 
        6*En^2*(281726704 - 329365106*la + 45761514*la^2 + 
          (12282449 - 5186130*la)*m^2))*M^4 + 
      27*(-47952765 + 52596480*En^2 + 16817932*la)*M^6)*rp[t]^23 + 
    (J^6*la^2*(5490180 - 5123298*la + 359105*la^2 + 105*la^3 + 
        3840*En^10*(569 + 2*la) + 5*(715989 - 56984*la + 261*la^2)*m^2 + 
        (34630 - 207*la)*m^4 + 5*m^6 + 1920*En^8*(3710 + 84*la - 15*la^2 + 
          (-311 + 18*la)*m^2) - 8*En^6*(1141650 - 42451*la - 21543*la^2 + 
          308*la^3 - 2*(-89672 + 113*la + 105*la^2)*m^2 + 
          4*(1721 + 6*la)*m^4) + 4*En^4*(629700 - 1700889*la - 11333*la^2 + 
          2128*la^3 - 2*(-1265339 + 59128*la + 364*la^2)*m^2 + 40912*m^4 + 
          4*m^6) - 2*En^2*(4049445 - 5843139*la + 232852*la^2 + 3412*la^3 + 
          10*(581821 - 36863*la + 10*la^2)*m^2 + (71215 - 216*la)*m^4 + 
          10*m^6)) + J^4*(19209960 - 331246764*la + 640898851*la^2 - 
        165302229*la^3 + 4846518*la^4 - 540*la^5 + 172800*En^10*
         (-231 + 310*la) + (8625960 - 80315265*la + 75385294*la^2 - 
          2243240*la^3 + 3540*la^4)*m^2 - 3*(-12600 + 77042*la - 43971*la^2 + 
          92*la^3)*m^4 - 144*En^8*(3*(-73920 + 93688*la + 2493*la^2) + 
          40*(-357 + 944*la)*m^2) + 24*En^6*(2349270 - 9610752*la - 
          538925*la^2 + 623074*la^3 - 2*(553140 - 2213379*la + 534970*la^2)*
           m^2 + 12*(-420 + 629*la)*m^4) + 12*En^4*(-2661120 - 15851352*la + 
          58491537*la^2 - 12831892*la^3 + 28536*la^4 + 
          (4575690 - 25998444*la + 12252380*la^2 - 82068*la^3)*m^2 + 
          12*(1890 - 5671*la + 1058*la^2)*m^4) + 
        2*En^2*(-17837820 + 368057151*la - 691196083*la^2 + 156857231*la^3 - 
          2249834*la^4 - 4892*la^5 - 2*(9759960 - 72663354*la + 
            50937547*la^2 - 881810*la^3 + 70*la^4)*m^2 + 
          (-94500 + 428439*la - 161198*la^2 + 108*la^3)*m^4))*M^2 - 
      3*J^2*(-351117810 + 88790544*En^8 + 1102332248*la - 569801049*la^2 + 
        44136411*la^3 - 330308*la^4 + (-25206912 + 43938507*la - 
          9841153*la^2 + 68700*la^3)*m^2 + 144*En^6*(137628 - 984619*la + 
          172857*m^2) + 12*En^4*(-67277460 + 125840771*la - 24333875*la^2 + 
          (-7420097 + 4168930*la)*m^2) + 2*En^2*(523495377 - 1281104283*la + 
          453619025*la^2 - 17618674*la^3 + (44465289 - 50704116*la + 
            5547920*la^2)*m^2))*M^4 + 18*(64839629 + 71181900*En^4 - 
        55266632*la + 5901249*la^2 + 3*En^2*(-47792458 + 19876053*la))*M^6)*
     rp[t]^24 + M*(-(J^4*la*(-13593690 + 67092510*la - 28886431*la^2 + 
         1328283*la^3 - 270*la^4 - 345600*En^10*(-77 + 26*la) + 
         (-5959560 + 14206473*la - 646055*la^2 + 1770*la^3)*m^2 - 
         6*(4270 - 6543*la + 23*la^2)*m^4 + 48*En^8*(-438240 + 122087*la + 
           6495*la^2 + 80*(-357 + 227*la)*m^2) - 
         24*En^6*(1607220 - 1366683*la - 359581*la^2 - 9482*la^3 + 
           (-745020 + 762425*la - 766*la^2)*m^2 + 8*(-420 + 157*la)*m^4) + 
         4*En^4*(3*(1778265 + 5011650*la - 4177004*la^2 + 16832*la^3 + 
             624*la^4) - (9311640 - 13568735*la + 207262*la^2 + 408*la^3)*
            m^2 + 6*(-7595 + 5707*la)*m^4) + 2*En^2*(12626055 - 78236694*la + 
           35691083*la^2 - 893615*la^3 - 4892*la^4 - 
           5*(-2669652 + 5093319*la - 151421*la^2 + 28*la^3)*m^2 + 
           2*(31815 - 36146*la + 54*la^2)*m^4))) - 
      2*J^2*(63886509 + 26818560*En^10 - 392770458*la + 365292712*la^2 - 
        49936760*la^3 + 845984*la^4 - 104*la^5 + 
        (7404237 - 26234502*la + 11847118*la^2 - 182560*la^3 + 120*la^4)*
         m^2 + 216*En^8*(-618496 + 404569*la + 20880*m^2) + 
        36*En^6*(2473635 + 1445929*la - 1627794*la^2 + (-830757 + 698398*la)*
           m^2) + 6*En^4*(29335032 - 152798478*la + 86308537*la^2 - 
          4401416*la^3 + 2*(4428729 - 7566972*la + 1089590*la^2)*m^2) + 
        3*En^2*(-74056995 + 389286216*la - 284108447*la^2 + 26454250*la^3 - 
          167032*la^4 + (-11717271 + 30512421*la - 8974466*la^2 + 55900*la^3)*
           m^2))*M^2 + 18*(-30272003 + 30074472*En^6 + 49146979*la - 
        12584033*la^2 + 376707*la^3 + 12*En^4*(-8449948 + 4358357*la) + 
        2*En^2*(50683636 - 53459973*la + 6612727*la^2))*M^4)*rp[t]^25 + 
    (J^4*la^2*(2456685 - 1920592*la + 133370*la^2 - 45*la^3 + 
        19200*En^10*(-229 + 2*la) + 5*(207226 - 13767*la + 59*la^2)*m^2 + 
        (4345 - 23*la)*m^4 + 1920*En^8*(1762 + 42*la - 15*la^2 + 
          (131 + 12*la)*m^2) + 12*En^4*(-261500 - 420297*la + 1927*la^2 + 
          312*la^3 - 2*(-267279 + 7150*la + 34*la^2)*m^2 + 2548*m^4) - 
        8*En^6*(-804330 - 136481*la - 12173*la^2 + 168*la^3 + 
          (386624 - 446*la - 70*la^2)*m^2 + 4*(421 + la)*m^4) + 
        En^2*(-4740510 + 5747916*la - 229576*la^2 - 2446*la^3 - 
          10*(460956 - 21359*la + 7*la^2)*m^2 + 6*(-3575 + 9*la)*m^4)) - 
      J^2*(-5896800 + 92991870*la - 165296510*la^2 + 36595847*la^3 - 
        1067751*la^4 + 312*la^5 + 34560*En^10*(-651 + 1034*la) + 
        (-1134000 + 10241856*la - 9337699*la^2 + 240425*la^3 - 360*la^4)*
         m^2 + 432*En^8*(144480 - 411584*la + 63799*la^2 + 
          40*(-210 + 347*la)*m^2) + 24*En^6*(-1982610 + 4523892*la + 
          2094451*la^2 - 374502*la^3 + 6*(81900 - 279219*la + 59354*la^2)*
           m^2) + 12*En^4*(-665280 + 21910164*la - 34079475*la^2 + 
          4753445*la^3 - 14757*la^4 + 6*(-192150 + 1000847*la - 436729*la^2 + 
            2045*la^3)*m^2) + 2*En^2*(10773000 - 161208999*la + 
          247847590*la^2 - 43393016*la^3 + 620874*la^4 + 1028*la^5 + 
          4*(850500 - 6011433*la + 4015997*la^2 - 55105*la^3 + 5*la^4)*m^2))*
       M^2 + 3*(46026216 + 35706960*En^8 - 135551338*la + 65040619*la^2 - 
        4392341*la^3 + 32792*la^4 + 72*En^6*(-2621768 + 1786655*la) + 
        12*En^4*(26242587 - 36712095*la + 5480675*la^2) + 
        6*En^2*(-34646154 + 74450243*la - 22997153*la^2 + 707357*la^3))*M^4)*
     rp[t]^26 + M*(J^2*la*(4155840 - 18335778*la + 6468289*la^2 - 
        294237*la^3 + 156*la^4 - 69120*En^10*(-217 + 86*la) + 
        (781200 - 1800681*la + 69635*la^2 - 180*la^3)*m^2 - 
        48*En^8*(870300 - 604771*la - 4135*la^2 + 480*(-105 + 43*la)*m^2) - 
        24*En^6*(-1324440 + 498573*la + 253903*la^2 + 2702*la^3 + 
          (329700 - 284419*la + 170*la^2)*m^2) + 
        2*En^2*(-7538400 + 32199873*la - 10166502*la^2 + 251287*la^3 + 
          1028*la^4 + (-2318400 + 4177447*la - 95575*la^2 + 20*la^3)*m^2) + 
        4*En^4*(1477440 - 14294991*la + 4944453*la^2 - 32056*la^3 - 
          478*la^4 + (2337300 - 3097291*la + 31510*la^2 + 50*la^3)*m^2)) + 
      2*(-8923284 + 3939840*En^10 + 50607216*la - 43170371*la^2 + 
        5008659*la^3 - 84316*la^4 + 16*la^5 + 648*En^8*(-57048 + 56959*la) + 
        72*En^6*(1324620 - 2762324*la + 526223*la^2) + 
        18*En^4*(-5807034 + 18690216*la - 7438469*la^2 + 230580*la^3) + 
        6*En^2*(8517420 - 37508868*la + 23162747*la^2 - 1622344*la^3 + 
          10132*la^4))*M^2)*rp[t]^27 + 
    2*((-1 + En^2)*J^2*la^2*(-372690 + 218365*la - 14872*la^2 + 13*la^3 + 
        1920*En^8*(653 + 2*la) - 15*(4510 - 249*la + la^2)*m^2 + 
        480*En^6*(-4616 - 83*la - 15*la^2 + 6*(71 + la)*m^2) + 
        4*En^4*(86070 + 113875*la + 1803*la^2 - 52*la^3 + 
          10*(-11676 + 85*la + la^2)*m^2) - 10*En^2*(-99096 + 63398*la - 
          1819*la^2 - 27*la^3 + (-33030 + 991*la + la^2)*m^2)) + 
      (453600 - 6404832*la + 10256900*la^2 - 1853407*la^3 + 53457*la^4 - 
        24*la^5 + 17280*En^10*(-84 + 151*la) + 
        72*En^8*(85680 - 349728*la + 92929*la^2) + 
        72*En^6*(-142380 + 914823*la - 524437*la^2 + 14945*la^3) + 
        6*En^4*(1375920 - 12172536*la + 10915261*la^2 - 779161*la^3 + 
          2761*la^4) - En^2*(3175200 - 36143928*la + 44680975*la^2 - 
          5445023*la^3 + 76368*la^4 + 96*la^5))*M^2)*rp[t]^28 + 
    6*(-1 + En^2)*la*(-105840 + 408287*la - 110595*la^2 + 4938*la^3 - 
      4*la^4 + 3840*En^8*(-84 + 37*la) + 24*En^6*(44100 - 56221*la + 
        1007*la^2) - 4*En^4*(313740 - 664231*la + 60751*la^2 + 302*la^3) - 
      2*En^2*(-312480 + 929143*la - 163681*la^2 + 2770*la^3 + 18*la^4))*M*
     rp[t]^29 - 4*(-1 + En^2)^2*la^2*(-28170 + 11395*la - 754*la^2 + la^3 + 
      2880*En^6*(29 + la) + 720*En^4*(-266 + 29*la + la^2) + 
      2*En^2*(68130 - 17255*la + 302*la^2 + 7*la^3))*rp[t]^30))/
  (En*la*(1 + la)*rp[t]^17*(3*M + la*rp[t])^2*(J^2 + rp[t]^2)^11) + 
 (((-16*I)*J^3*m*mu*Pi*YPhiPhiBar*(2*M - rp[t])*(33456969*J^18*M^7 - 
      54*J^18*(1076656 - 472365*la + 26136*m^2)*M^6*rp[t] + 
      3*J^16*M^5*(J^2*(12951357 - 14642864*la + 1899459*la^2 + 
          (729408 - 329532*la)*m^2 + 2052*m^4) + 110955555*M^2)*rp[t]^2 + 
      6*J^16*M^4*(J^2*(-2058504 + 4837693*la - 1600337*la^2 + 50606*la^3 + 
          (-206211 + 255270*la - 30160*la^2)*m^2 + 2*(-693 + 346*la)*m^4) + 
        3*(-32214608 + 1500066*En^2 + 14056875*la - 721176*m^2)*M^2)*
       rp[t]^3 + J^14*M^3*(J^4*(1837944 - 9092901*la + 6167686*la^2 - 
          466350*la^3 + 2020*la^4 + (300105 - 864693*la + 278840*la^2 - 
            2540*la^3)*m^2 + (3699 - 5616*la + 700*la^2)*m^4) - 
        3*J^2*(-129522933 + 145670452*la - 18721335*la^2 + 
          24*(-281503 + 126000*la)*m^2 - 15804*m^4 + 
          12*En^2*(923593 - 563913*la + 35895*m^2))*M^2 + 1486514916*M^4)*
       rp[t]^4 + J^14*M^2*(-(J^4*(99765 - 1326936*la + 1860181*la^2 - 
           260802*la^3 + 2676*la^4 + (26055 - 209166*la + 156130*la^2 - 
             3420*la^3)*m^2 + 6*(90 - 417*la + 158*la^2)*m^4)) + 
        3*J^2*(-41296695 + 96561532*la - 31660598*la^2 + 975052*la^3 - 
          4*(962625 - 1179724*la + 137620*la^2)*m^2 + 4*(-5391 + 2662*la)*
           m^4 + 2*En^2*(2377593 - 4112590*la + 741474*la^2 - 
            8*(-28311 + 18734*la)*m^2 + 1356*m^4))*M^2 + 
        72*(-36073192 + 3585024*En^2 + 15640155*la - 731892*m^2)*M^4)*
       rp[t]^5 + J^12*M*(J^6*la*(-70074 + 257335*la - 62460*la^2 + 
          1161*la^3 - 5*(3615 - 7459*la + 303*la^2)*m^2 + 
          (-366 + 423*la)*m^4) + J^4*(18497835 - 91072752*la + 
          61260313*la^2 - 4509546*la^3 + 19120*la^4 + 
          (2827602 - 8058528*la + 2564360*la^2 - 21440*la^3)*m^2 + 
          (29133 - 43632*la + 5380*la^2)*m^4 + 6*En^2*(-408978 + 1734919*la - 
            877028*la^2 + 35870*la^3 - 2*(37437 - 78589*la + 13671*la^2)*
             m^2 + 6*(-193 + 152*la)*m^4))*M^2 + 
        12*J^2*(145511883 + 411075*En^4 - 162639464*la + 20675631*la^2 + 
          (6914730 - 3061176*la)*m^2 + 12933*m^4 - 
          3*En^2*(8893473 - 5379264*la + 322891*m^2))*M^4 + 3919578660*M^6)*
       rp[t]^6 - 2*J^12*(J^6*la^2*(6264 - 2673*la + 82*la^2 + 
          (1580 - 110*la)*m^2 + 31*m^4) + 
        J^4*(3*(167985 - 2223967*la + 3093865*la^2 - 422117*la^3 + 
            4232*la^4) + (124065 - 984066*la + 724450*la^2 - 14520*la^3)*
           m^2 + 3*(720 - 3279*la + 1226*la^2)*m^4 - 
          En^2*(67500 - 875157*la + 1070973*la^2 - 111318*la^3 + 160*la^4 - 
            2*(-11385 + 77574*la - 42683*la^2 + 540*la^3)*m^2 + 
            20*(36 - 117*la + 23*la^2)*m^4))*M^2 + 
        6*J^2*(46562907 - 108224162*la + 35115904*la^2 - 1047824*la^3 + 
          (3979401 - 4818208*la + 553940*la^2)*m^2 + (17793 - 8706*la)*m^4 + 
          6*En^4*(49397 - 49869*la + 3086*m^2) - 
          En^2*(6*(1923612 - 3295029*la + 586643*la^2) + 
            (1034061 - 672388*la)*m^2 + 5028*m^4))*M^4 - 
        36*(-95467373 + 15271794*En^2 + 41074419*la - 1712130*m^2)*M^6)*
       rp[t]^7 + J^10*M*(J^6*la*(-707913 + 2582135*la - 609543*la^2 + 
          11052*la^3 - 10*(17202 - 34987*la + 1296*la^2)*m^2 + 
          3*(-974 + 1107*la)*m^4 + En^2*(93198 - 343146*la + 72582*la^2 - 
            176*la^3 + 4*(7800 - 13877*la + 450*la^2)*m^2 + 
            (972 - 788*la)*m^4)) + 
        J^4*(4*(20941578 - 102510579*la + 68278073*la^2 - 4867464*la^3 + 
            20124*la^4) - 6*(-1970289 + 5540442*la - 1735900*la^2 + 
            13160*la^3)*m^2 + 3*(32373 - 47952*la + 5860*la^2)*m^4 + 
          12*En^4*(59583 - 211631*la + 61680*la^2 + (10089 - 12670*la)*m^2 + 
            132*m^4) + 12*En^2*(-2003928 + 8414446*la - 4200226*la^2 + 
            167636*la^3 + (-347946 + 716329*la - 121787*la^2)*m^2 + 
            (-4434 + 3376*la)*m^4))*M^2 + 12*J^2*(386644896 + 3892563*En^4 - 
          428938876*la + 53822935*la^2 + (16341594 - 7141464*la)*m^2 + 
          23355*m^4 - 3*En^2*(38206735 - 22856548*la + 1277351*m^2))*M^4 + 
        6745436190*M^6)*rp[t]^8 + 
      J^10*(J^6*la^2*(-126546 + 52467*la - 1568*la^2 + 20*(-1502 + 95*la)*
           m^2 - 494*m^4 + 2*En^2*(8518 - 3671*la + 4*la^2 - 
            20*(-137 + 9*la)*m^2 + 82*m^4)) + 
        J^4*(-3*(1528515 - 20123158*la + 27741342*la^2 - 3663524*la^3 + 
            35728*la^4) + (-1051245 + 8214756*la - 5948140*la^2 + 
            107520*la^3)*m^2 - 18*(810 - 3639*la + 1346*la^2)*m^4 + 
          12*En^4*(-2862 + 41556*la - 42707*la^2 + 1792*la^3 - 
            4*(285 - 1715*la + 562*la^2)*m^2 + 8*(-6 + 11*la)*m^4) + 
          4*En^2*(334260 - 4286997*la + 5179509*la^2 - 526002*la^3 + 
            830*la^4 + (108090 - 720390*la + 386816*la^2 - 4320*la^3)*m^2 + 
            20*(144 - 447*la + 85*la^2)*m^4))*M^2 + 
        6*J^2*(-248539215 + 12636*En^6 + 573529244*la - 183777588*la^2 + 
          5275752*la^3 - 2*(9518349 - 11356524*la + 1284260*la^2)*m^2 + 
          10*(-6471 + 3142*la)*m^4 - 6*En^4*(952549 - 945042*la + 
            56656*m^2) + 4*En^2*(25036914 - 42388634*la + 7435707*la^2 + 
            (2084001 - 1325656*la)*m^2 + 7485*m^4))*M^4 + 
        36*(-330177692 + 76038432*En^2 + 140698073*la - 5040840*m^2)*M^6)*
       rp[t]^9 + J^8*M*(-(J^6*la*(3220524 - 11652478*la + 2660814*la^2 - 
           46812*la^3 + 5*(145629 - 291010*la + 9660*la^2)*m^2 + 
           (9846 - 11043*la)*m^4 - 4*En^4*(-5808 + 25080*la - 2808*la^2 - 
             151*la^3 + (-2304 + 3689*la - 6*la^2)*m^2 + (-96 + 44*la)*m^4) + 
           4*En^2*(-230802 + 838556*la - 173650*la^2 + 509*la^3 + 
             (-74070 + 128369*la - 3690*la^2)*m^2 + 2*(-969 + 751*la)*
              m^4))) + J^4*(2*(112328721 - 546079422*la + 359446838*la^2 - 
            24633792*la^3 + 98896*la^4) + (28689282 - 79314912*la + 
            24403720*la^2 - 165760*la^3)*m^2 + 5*(35613 - 52272*la + 
            6340*la^2)*m^4 + 72*En^6*(-224 + 621*la + 8*m^2) + 
          12*En^4*(585318 - 2043311*la + 585186*la^2 + (96030 - 116404*la)*
             m^2 + 1140*m^4) + 12*En^2*(-8795082 + 36464960*la - 
            17930922*la^2 + 694480*la^3 + (-1432209 + 2880005*la - 
              475911*la^2)*m^2 + 5*(-2697 + 2008*la)*m^4))*M^2 + 
        6*J^2*(1344153879 + 32837382*En^4 - 1477304608*la + 182415181*la^2 - 
          60*(-811267 + 349429*la)*m^2 + 50310*m^4 - 
          6*En^2*(96003901 - 56725888*la + 2892115*m^2))*M^4 + 
        7890042654*M^6)*rp[t]^10 + 
      2*J^8*(J^6*la^2*(-4*En^4*(620 - 181*la - 34*la^2 + (218 + 3*la)*m^2 + 
            8*m^4) - 3*(95933 - 38438*la + 1112*la^2 + (21160 - 1190*la)*
             m^2 + 277*m^4) + 2*En^2*(42074 - 17803*la + 47*la^2 + 
            (12980 - 765*la)*m^2 + 326*m^4)) + 
        J^4*(-6*(1030725 - 13477832*la + 18379885*la^2 - 2331673*la^3 + 
            22008*la^4) + 10*(-129870 + 994641*la - 705805*la^2 + 11340*la^3)*
           m^2 - 15*(900 - 3999*la + 1466*la^2)*m^4 + 
          36*En^6*la*(-140 + 59*la + 8*m^2) + 12*En^4*(-14310 + 204444*la - 
            206199*la^2 + 8912*la^3 - 4*(1425 - 8182*la + 2583*la^2)*m^2 + 
            20*(-12 + 19*la)*m^4) + En^2*(2972700 - 37612371*la + 
            44739620*la^2 - 4417104*la^3 + 7548*la^4 + 
            (909450 - 5923092*la + 3086066*la^2 - 29160*la^3)*m^2 + 
            100*(180 - 543*la + 101*la^2)*m^4))*M^2 + 
        6*J^2*(-217185006 + 69228*En^6 + 496770179*la - 156722401*la^2 + 
          4285502*la^3 - 10*(1437522 - 1686539*la + 187330*la^2)*m^2 + 
          95*(-369 + 178*la)*m^4 - 6*En^4*(2059218 - 1993741*la + 
            114390*m^2) + En^2*(127278744 - 212575986*la + 36649346*la^2 + 
            (9708435 - 5978300*la)*m^2 + 23160*m^4))*M^4 + 
        18*(-388912414 + 121924110*En^2 + 163607005*la - 4771284*m^2)*M^6)*
       rp[t]^11 + J^6*M*(-(J^6*la*(8686035 - 31126829*la + 6823254*la^2 - 
           115752*la^3 + 10*(179637 - 350851*la + 10248*la^2)*m^2 + 
           (18210 - 20205*la)*m^4 + 24*En^6*la*(125 + 81*la - 8*m^2) - 
           4*En^4*(-58080 + 245373*la - 28932*la^2 - 1237*la^3 - 
             2*(11520 - 17611*la + 66*la^2)*m^2 + 20*(-48 + 19*la)*m^4) + 
           2*En^2*(-2053365 + 7335750*la - 1480496*la^2 + 5070*la^3 - 
             2*(311940 - 524647*la + 12690*la^2)*m^2 + 10*(-1209 + 911*la)*
              m^4))) + J^4*(394766811 - 1904199654*la + 1235193536*la^2 - 
          80513004*la^3 + 312536*la^4 - 10*(-4407912 + 11945475*la - 
            3602140*la^2 + 21700*la^3)*m^2 + 5*(38853 - 56592*la + 6820*la^2)*
           m^4 + 144*En^6*(-1120 + 3483*la + 40*m^2) + 
          36*En^4*(858459 - 2946133*la + 823940*la^2 - 5*(-25977 + 31310*la)*
             m^2 + 1080*m^4) + 12*En^2*(-22735032 + 92571722*la - 
            44730282*la^2 + 1666404*la^3 - 5*(692010 - 1336581*la + 
              212407*la^2)*m^2 + 20*(-1059 + 776*la)*m^4))*M^2 + 
        6*J^2*(1595557161 + 80335350*En^4 - 1730933852*la + 209353217*la^2 + 
          (46653564 - 19787616*la)*m^2 + 32346*m^4 - 
          30*En^2*(31010945 - 18116790*la + 799289*m^2))*M^4 + 
        6299507700*M^6)*rp[t]^12 + 
      J^6*(J^6*la^2*(-1552230 + 596649*la - 16576*la^2 + 20*(-15625 + 763*la)*
           m^2 - 3070*m^4 + 32*En^6*(1 + la)*(7 - 3*la + 2*m^2) - 
          16*En^4*(3037 - 941*la - 143*la^2 + (1072 - 3*la)*m^2 + 40*m^4) + 
          2*En^2*(373010 - 154137*la + 648*la^2 - 40*(-2726 + 135*la)*m^2 + 
            2030*m^4)) + J^4*(-3*(7278525 - 94627552*la + 127388216*la^2 - 
            15349568*la^3 + 139496*la^4) + 5*(-815805 + 6097152*la - 
            4228160*la^2 + 59640*la^3)*m^2 - 30*(990 - 4359*la + 1586*la^2)*
           m^4 + 144*En^6*la*(-700 + 379*la + 40*m^2) + 
          12*En^4*(-114210 + 1800864*la - 1779213*la^2 + 79520*la^3 - 
            60*(696 - 4441*la + 1380*la^2)*m^2 + 720*(-2 + 3*la)*m^4) + 
          4*En^2*(3977100 - 48561030*la + 56599583*la^2 - 5392014*la^3 + 
            9694*la^4 - 30*(-38490 + 237759*la - 118181*la^2 + 900*la^3)*
             m^2 + 600*(24 - 71*la + 13*la^2)*m^4))*M^2 + 
        12*J^2*(-259993752 + 346554*En^6 + 587065084*la - 181412501*la^2 + 
          4657978*la^3 + (-13987443 + 16117700*la - 1757180*la^2)*m^2 + 
          3*(-7551 + 3622*la)*m^4 - 15*En^4*(2150355 - 1953738*la + 
            116576*m^2) + 5*En^2*(41277633 - 68507510*la + 11588512*la^2 - 
            8*(-346785 + 205627*la)*m^2 + 3954*m^4))*M^4 + 
        72*(-156897292 + 66129024*En^2 + 64843855*la - 1414644*m^2)*M^6)*
       rp[t]^13 + J^4*M*(-(J^6*la*(15340590 - 54447541*la + 11326878*la^2 - 
           184086*la^3 + 25*(112605 - 214064*la + 5418*la^2)*m^2 + 
           (20010 - 22005*la)*m^4 + 48*En^6*la*(583 + 363*la - 40*m^2) + 
           4*En^4*(236160 - 1065894*la + 132728*la^2 + 4139*la^3 + 
             15*(5700 - 9505*la + 38*la^2)*m^2 - 360*(-8 + 3*la)*m^4) + 
           4*En^2*(-2738865 + 9446096*la - 1845906*la^2 + 6985*la^3 - 
             15*(52730 - 83513*la + 1590*la^2)*m^2 + 420*(-23 + 17*la)*
              m^4))) + J^4*(476778339 - 2272049556*la + 1444179286*la^2 - 
          88088100*la^3 + 329280*la^4 - 6*(-7283025 + 19316208*la - 
            5703140*la^2 + 30240*la^3)*m^2 + 3*(42093 - 60912*la + 7300*la^2)*
           m^4 + 216*En^6*(4312 + 11817*la + 920*m^2) + 
          60*En^4*(1605588 - 4609867*la + 1217958*la^2 - 28*(-8091 + 8498*la)*
             m^2 + 840*m^4) + 60*En^2*(-7334886 + 30021731*la - 
            14318902*la^2 + 505326*la^3 + (-1033347 + 1901507*la - 
              289257*la^2)*m^2 + (-3657 + 2648*la)*m^4))*M^2 + 
        12*J^2*(651669507 + 55638225*En^4 - 693627128*la + 81652195*la^2 - 
          6*(-2335133 + 975080*la)*m^2 + 5751*m^4 - 
          3*En^2*(170812243 - 97528768*la + 3320585*m^2))*M^4 + 
        3319946676*M^6)*rp[t]^14 + 
      2*J^4*(J^6*la^2*(-1371240 + 500631*la - 13244*la^2 + 
          50*(-4883 + 203*la)*m^2 - 1685*m^4 + 160*En^6*(1 + la)*
           (7 - 3*la + 2*m^2) - 4*En^4*(24480 - 8189*la - 974*la^2 - 
            75*(-106 + la)*m^2 + 240*m^4) + 2*En^2*(494580 - 197156*la + 
            1069*la^2 - 75*(-1832 + 69*la)*m^2 + 1620*m^4)) + 
        J^4*(-3*(4430565 - 57049651*la + 75355726*la^2 - 8464052*la^3 + 
            73696*la^4) + (-2070765 + 15069318*la - 10198390*la^2 + 
            125160*la^3)*m^2 - 9*(1080 - 4719*la + 1706*la^2)*m^4 + 
          36*En^6*(-15120 + 9624*la + 4081*la^2 + 120*(-21 + 16*la)*m^2) + 
          240*En^4*(-17037 + 138798*la - 116128*la^2 + 5256*la^3 + 
            (-3975 + 19256*la - 5162*la^2)*m^2 + (-48 + 70*la)*m^4) + 
          5*En^2*(2483460 - 31413147*la + 36628330*la^2 - 3338292*la^3 + 
            6124*la^4 - 2*(-366975 + 2121666*la - 998761*la^2 + 5940*la^3)*
             m^2 + 140*(36 - 105*la + 19*la^2)*m^4))*M^2 + 
        6*J^2*(-215511915 + 3081384*En^6 + 476683658*la - 143179452*la^2 + 
          3385848*la^3 - 3*(2843885 - 3217768*la + 344340*la^2)*m^2 + 
          (-8091 + 3862*la)*m^4 - 30*En^4*(1342929 - 1370611*la + 
            84994*m^2) + En^2*(230463864 - 375228130*la + 61279302*la^2 + 
            (11910819 - 6806396*la)*m^2 + 8868*m^4))*M^4 + 
        36*(-83936987 + 48515034*En^2 + 33883615*la - 479502*m^2)*M^6)*
       rp[t]^15 + J^2*M*(-(J^6*la*(18689679 - 65319260*la + 12616020*la^2 - 
           195216*la^3 + 10*(285120 - 526537*la + 11424*la^2)*m^2 - 
           9*(-1454 + 1587*la)*m^4 - 24*En^6*(-29340 + 5477*la - 2575*la^2 + 
             360*(-14 + 3*la)*m^2) - 20*En^4*(-271644 + 640919*la - 
             75956*la^2 - 1485*la^3 - 4*(16200 - 20137*la + 54*la^2)*m^2 + 
             8*(-96 + 35*la)*m^4) + 10*En^2*(-1715037 + 6129884*la - 
             1173350*la^2 + 4654*la^3 - 2*(250560 - 369199*la + 5310*la^2)*
              m^2 + (-3378 + 2462*la)*m^4))) + 
        J^4*(-1036800*En^8 + 2*(201132315 - 937083966*la + 578108494*la^2 - 
            32245128*la^3 + 115640*la^4) + (27163242 - 70490268*la + 
            20378360*la^2 - 94640*la^3)*m^2 + (45333 - 65232*la + 7780*la^2)*
           m^4 - 288*En^6*(122720 - 84969*la + 2650*m^2) + 
          60*En^4*(1450185 - 5908225*la + 1752168*la^2 + (373851 - 345050*la)*
             m^2 + 516*m^4) + 12*En^2*(-41098944 + 167160722*la - 
            77258534*la^2 + 2469180*la^3 + (-4628982 + 8130659*la - 
              1186041*la^2)*m^2 + 14*(-591 + 424*la)*m^4))*M^2 + 
        12*J^2*(355084218 + 59390865*En^4 - 368041084*la + 41862851*la^2 - 
          6*(-801565 + 329636*la)*m^2 + 873*m^4 - 
          3*En^2*(129948693 - 70668244*la + 1519621*m^2))*M^4 + 
        1048048065*M^6)*rp[t]^16 + 
      J^2*(J^6*la^2*(480*En^6*(-235 + 25*la - 7*la^2 + (-38 + 4*la)*m^2) - 
          80*En^4*(12293 - 2553*la - 177*la^2 + (2858 - 18*la)*m^2 + 
            32*m^4) + 10*En^2*(311354 - 129749*la + 796*la^2 + 
            (86420 - 2340*la)*m^2 + 566*m^4) - 3*(1114146 - 376487*la + 
            9408*la^2 - 20*(-8216 + 287*la)*m^2 + 734*m^4)) + 
        J^4*(-51840*En^8*(-21 + 13*la) - 3*(7640145 - 95967814*la + 
            122835290*la^2 - 12501884*la^3 + 103824*la^4) + 
          (-2636955 + 18682332*la - 12339700*la^2 + 131040*la^3)*m^2 - 
          6*(1170 - 5079*la + 1826*la^2)*m^4 - 96*En^6*(-104580 + 241770*la - 
            42307*la^2 + 180*(-7 + 29*la)*m^2) + 
          60*En^4*(-28674 + 1044924*la - 1258665*la^2 + 67904*la^3 - 
            4*(16167 - 63181*la + 14778*la^2)*m^2 + 8*(-30 + 43*la)*m^4) + 
          4*En^2*(6682500 - 88176381*la + 100924099*la^2 - 8340654*la^3 + 
            15210*la^4 - 2*(-870705 + 4733517*la - 2116634*la^2 + 9720*la^3)*
             m^2 + 20*(288 - 831*la + 149*la^2)*m^4))*M^2 - 
        6*J^2*(240352869 + 6308604*En^6 - 515496164*la + 149142628*la^2 - 
          3172808*la^3 + (5945982 - 6609416*la + 694680*la^2)*m^2 + 
          (2466 - 1172*la)*m^4 + 6*En^4*(14273619 - 14533478*la + 
            620112*m^2) - 4*En^2*(91915716 - 141078662*la + 21570589*la^2 - 
            3*(-935471 + 517328*la)*m^2 + 819*m^4))*M^4 + 
        18*(-108065512 + 87723456*En^2 + 42380831*la - 284040*m^2)*M^6)*
       rp[t]^17 + M*(-(J^6*la*(16112088 + 103680*En^8*(-7 + la) - 
           54484900*la + 9419658*la^2 - 138012*la^3 + 
           5*(362139 - 649774*la + 12012*la^2)*m^2 + (4722 - 5121*la)*m^4 + 
           480*En^6*(-13848 + 8063*la + 145*la^2 + 2*(-84 + 83*la)*m^2) - 
           20*En^4*(-52944 + 680144*la - 134440*la^2 - 1553*la^3 + 
             (-130968 + 129931*la - 210*la^2)*m^2 + 4*(-120 + 43*la)*m^4) + 
           4*En^2*(-4656072 + 17161964*la - 3017798*la^2 + 12039*la^3 + 
             (-1184850 + 1634167*la - 17550*la^2)*m^2 + (-3858 + 2782*la)*
              m^4))) + J^4*(12493440*En^8 + 4*(57812373 - 259504089*la + 
            153552883*la^2 - 7611684*la^3 + 26104*la^4) + 
          (9640134 - 24496992*la + 6939800*la^2 - 28160*la^3)*m^2 + 
          (6939 - 9936*la + 1180*la^2)*m^4 - 72*En^6*(-657544 + 336733*la + 
            29200*m^2) + 36*En^4*(2032926 - 10519047*la + 3032090*la^2 + 
            (493098 - 417980*la)*m^2 + 204*m^4) + 
          12*En^2*(2*(-17515569 + 65993778*la - 28236943*la^2 + 
              756388*la^3) + (-2262531 + 3818159*la - 536837*la^2)*m^2 + 
            (-1539 + 1096*la)*m^4))*M^2 + 3*J^2*(468324189 + 203688612*En^4 - 
          469433744*la + 51258715*la^2 - 12*(-240298 + 97393*la)*m^2 - 
          12*En^2*(62034895 - 31452288*la + 293721*m^2))*M^4 + 150529185*M^6)*
       rp[t]^18 + 2*(J^6*la^2*(-1438923 + 427656*la - 10024*la^2 + 
          2880*En^8*(22 + la) + 10*(-15602 + 455*la)*m^2 - 397*m^4 + 
          320*En^6*(1739 + 26*la - 9*la^2 + (25 + 4*la)*m^2) - 
          20*En^4*(7600 - 10307*la - 374*la^2 + (11242 - 39*la)*m^2 + 
            40*m^4) + 2*En^2*(852754 - 346983*la + 2217*la^2 - 
            5*(-40604 + 783*la)*m^2 + 646*m^4)) + 
        J^4*(-6876360 + 82320942*la - 100440198*la^2 + 8936106*la^3 - 
          70512*la^4 + 5760*En^8*(-651 + 724*la) + 
          2*(-239355 + 1653159*la - 1066835*la^2 + 9780*la^3)*m^2 - 
          3*(180 - 777*la + 278*la^2)*m^4 - 12*En^6*(448560 - 1316628*la + 
            143209*la^2 + 120*(-273 + 485*la)*m^2) + 
          12*En^4*(252234 + 2317968*la - 3343731*la^2 + 161296*la^3 - 
            12*(11940 - 41510*la + 8877*la^2)*m^2 + 12*(-12 + 17*la)*m^4) + 
          En^2*(12691620 - 149248389*la + 155144188*la^2 - 10454064*la^3 + 
            18580*la^4 - 2*(-892215 + 4611750*la - 1973791*la^2 + 7020*la^3)*
             m^2 + 20*(108 - 309*la + 55*la^2)*m^4))*M^2 + 
        3*J^2*(-81749040 + 80856*En^6 + 168453349*la - 46566773*la^2 + 
          869062*la^3 + (-904053 + 988238*la - 102120*la^2)*m^2 - 
          12*En^4*(7849012 - 6029031*la + 87378*m^2) + 
          En^2*(4*(47295924 - 66311639*la + 9261667*la^2) - 
            54*(-41191 + 22156*la)*m^2))*M^4 + 
        9*(-15875924 + 18160914*En^2 + 6027091*la)*M^6)*rp[t]^19 + 
      M*(J^4*la*(-9645669 + 30778433*la - 4541274*la^2 + 62712*la^3 + 
          1920*En^8*(-2604 + 727*la) - 10*(65577 - 114505*la + 1800*la^2)*
           m^2 + (-726 + 783*la)*m^4 - 24*En^6*(303480 - 206893*la - 
            705*la^2 + 240*(-91 + 40*la)*m^2) + 4*En^4*(979992 + 3242415*la - 
            690188*la^2 - 4751*la^3 - 6*(96384 - 84569*la + 86*la^2)*m^2 + 
            12*(-48 + 17*la)*m^4) + En^2*(17702118 - 56752484*la + 
            7788240*la^2 - 30356*la^3 + 4*(605220 - 790969*la + 6390*la^2)*
             m^2 - 4*(-723 + 517*la)*m^4)) + J^2*(82006047 - 22464000*En^8 - 
          350131629*la + 196670906*la^2 - 8405814*la^3 + 27492*la^4 - 
          3*(-496989 + 1238439*la - 344240*la^2 + 1220*la^3)*m^2 - 
          144*En^6*(-215064 - 15203*la + 7956*m^2) + 
          108*En^4*(1419387 - 3697189*la + 776604*la^2 + (48785 - 39166*la)*
             m^2) - 12*En^2*(20166912 - 66897270*la + 25590862*la^2 - 
            529420*la^3 + (461934 - 754395*la + 102841*la^2)*m^2))*M^2 + 
        9*(23585051 + 26585388*En^4 - 22764732*la + 2376309*la^2 + 
          12*En^2*(-4536922 + 2137851*la))*M^4)*rp[t]^20 + 
      (J^4*la^2*(-1715538 + 3840*En^8*(-216 + la) + 418623*la - 9152*la^2 + 
          20*(-5633 + 137*la)*m^2 - 122*m^4 + 160*En^6*(-7375 - 70*la - 
            33*la^2 + 2*(278 + 5*la)*m^2) - 16*En^4*(-29985 - 29443*la - 
            577*la^2 + (24534 - 51*la)*m^2 + 24*m^4) + 
          En^2*(3201916 - 930862*la + 5888*la^2 - 80*(-5159 + 72*la)*m^2 + 
            484*m^4)) + J^2*(-5178600 + 57977364*la - 66504795*la^2 + 
          4982046*la^3 - 37236*la^4 - 28800*En^8*(-357 + 521*la) + 
          10*(-15120 + 102021*la - 64435*la^2 + 510*la^3)*m^2 - 
          48*En^6*(259560 - 415032*la - 25481*la^2 + 48*(-210 + 331*la)*
             m^2) - 12*En^4*(867510 - 8963808*la + 6354803*la^2 - 
            164384*la^3 + 4*(22680 - 73749*la + 14896*la^2)*m^2) - 
          4*En^2*(-4441500 + 42374328*la - 37903465*la^2 + 1869522*la^3 - 
            3194*la^4 + 6*(-31500 + 156501*la - 64633*la^2 + 180*la^3)*m^2))*
         M^2 + 3*(-25598529 + 23969880*En^6 + 50421396*la - 13254214*la^2 + 
          211948*la^3 + 132*En^4*(-669691 + 418562*la) + 
          6*En^2*(14929271 - 19112754*la + 2430634*la^2))*M^4)*rp[t]^21 + 
      M*(-(J^2*la*(3616020 - 10676194*la + 1281294*la^2 - 16617*la^3 + 
           19200*En^8*(-357 + 131*la) + 5*(20664 - 35197*la + 471*la^2)*m^2 + 
           48*En^6*(172800 - 59077*la - 1477*la^2 + 4*(-1680 + 659*la)*m^2) + 
           4*En^4*(1805760 - 5068258*la + 373192*la^2 + 1577*la^3 + 
             3*(60900 - 49819*la + 34*la^2)*m^2) - 
           4*En^2*(3069765 - 7782360*la + 715798*la^2 - 2675*la^3 + 
             3*(42630 - 53419*la + 330*la^2)*m^2))) + 
        (13458636 + 7551360*En^8 - 54336924*la + 28816601*la^2 - 
          1033314*la^3 + 3216*la^4 + 72*En^6*(-733248 + 677909*la) + 
          36*En^4*(2649444 - 5072023*la + 837510*la^2) + 
          6*En^2*(-10599354 + 31231059*la - 10684144*la^2 + 161638*la^3))*
         M^2)*rp[t]^22 + 2*(-2*J^2*la^2*(2400*En^8*(-118 + la) + 
          3*(53225 - 9997*la + 203*la^2) + (4425 - 90*la)*m^2 + 
          En^4*(334968 - 68734*la - 772*la^2 - 42*(-734 + la)*m^2) - 
          48*En^6*(-6883 - 156*la - 13*la^2 + 2*(141 + la)*m^2) + 
          En^2*(-541920 + 88666*la - 539*la^2 + 15*(-1448 + 15*la)*m^2)) + 
        (-453600 + 4721148*la - 5061479*la^2 + 309237*la^3 - 2184*la^4 + 
          17280*En^8*(-84 + 145*la) + 36*En^6*(131040 - 495432*la + 
            117751*la^2) + 72*En^4*(-76860 + 452842*la - 227089*la^2 + 
            2880*la^3) + 3*En^2*(907200 - 7331940*la + 5705627*la^2 - 
            194146*la^3 + 316*la^4))*M^2)*rp[t]^23 + 
      la*(5760*En^8*(-336 + 143*la) + 24*En^6*(264180 - 256517*la + 
          1279*la^2) + En^2*(3729600 - 7849934*la + 457294*la^2 - 
          1620*la^3) - 4*En^4*(1876140 - 2867569*la + 82028*la^2 + 
          219*la^3) + 3*(-210000 + 570597*la - 53671*la^2 + 652*la^3))*M*
       rp[t]^24 - 48*(-1 + En^2)*la^2*(-2300 + 319*la - 6*la^2 + 
        160*En^6*(43 + la) + En^2*(11175 - 899*la + la^2) + 
        10*En^4*(-1575 + 43*la + la^2))*rp[t]^25))/
    (la*(1 + la)*rp[t]^14*(3*M + la*rp[t])^2*(J^2 + rp[t]^2)^10) + 
   (16*J*mu*Pi*YPhiBar*(2*M - rp[t])*(14272902*J^18*M^7 + 
      675*J^18*(-37080 + 14347*la)*M^6*rp[t] + 
      3*J^16*M^5*(J^2*(5668161 - 5679850*la + 531567*la^2) + 43830090*M^2)*
       rp[t]^2 + 3*J^16*M^4*(-2*J^2*(926302 - 1938375*la + 470942*la^2 + 
          3466*la^3) + 3*(-25563380 + 1168656*En^2 + 9986553*la)*M^2)*
       rp[t]^3 + J^14*M^3*(2*J^4*(432963 - 1912654*la + 974650*la^2 + 
          17163*la^3 - 130*la^4) + 3*J^2*(51963501 - 52531714*la + 
          5049903*la^2 + 12*En^2*(-369152 + 190273*la))*M^2 + 539644680*M^4)*
       rp[t]^4 + J^14*M^2*(J^4*(-50760 + 601056*la - 651113*la^2 - 
          20522*la^3 + 372*la^4) + 6*J^2*(-8464815 + 17854565*la - 
          4440352*la^2 - 18434*la^3 + 2*En^2*(496935 - 727669*la + 
            83778*la^2))*M^2 + 36*(-26168066 + 2563602*En^2 + 10328763*la)*
         M^4)*rp[t]^5 + J^12*M*(J^6*la*(-35625 + 104515*la + 5203*la^2 - 
          177*la^3) - 3*J^4*(-2627703 + 11689493*la - 6073564*la^2 - 
          65842*la^3 + 660*la^4 + 2*En^2*(184695 - 661097*la + 221214*la^2 + 
            7526*la^3))*M^2 + 36*J^2*(17680785 + 48975*En^4 - 18045846*la + 
          1784168*la^2 + En^2*(-3243271 + 1682379*la))*M^4 + 1295544456*M^6)*
       rp[t]^6 + J^12*(14*J^6*la^2*(-455 - 33*la + 2*la^2) + 
        J^4*(-460080 + 5480502*la - 6027341*la^2 - 126416*la^3 + 2892*la^4 + 
          En^2*(70020 - 749220*la + 632408*la^2 + 48948*la^3 - 320*la^4))*
         M^2 + 6*J^2*(-34443051 + 73285343*la - 18684850*la^2 - 14976*la^3 + 
          24*En^4*(-9326 + 7687*la) + 2*En^2*(4371897 - 6429068*la + 
            761016*la^2))*M^4 + 108*(-20890978 + 3309894*En^2 + 8337937*la)*
         M^6)*rp[t]^7 + J^10*M*(J^6*la*(-322920 + 957457*la + 34006*la^2 - 
          1407*la^3 + 2*En^2*(24150 - 63525*la - 8309*la^2 + 136*la^3)) + 
        J^4*(31942809 - 143207154*la + 75992779*la^2 + 274216*la^3 - 
          5988*la^4 + 12*En^4*(25068 - 71647*la + 12162*la^2) - 
          6*En^2*(1627617 - 5836588*la + 1991038*la^2 + 56826*la^3))*M^2 + 
        12*J^2*(126655377 + 1267533*En^4 - 130627946*la + 13301462*la^2 + 
          3*En^2*(-12583105 + 6573057*la))*M^4 + 2006286084*M^6)*rp[t]^8 + 
      J^10*(J^6*la^2*(-57750 - 3177*la + 228*la^2 + 
          En^2*(8820 + 1714*la - 56*la^2)) - 2*J^4*(927585 - 11123922*la + 
          12440368*la^2 + 117124*la^3 - 4554*la^4 + 
          En^4*(9090 - 98844*la + 63906*la^2 + 6540*la^3) + 
          6*En^2*(-51555 + 551084*la - 470009*la^2 - 31497*la^3 + 250*la^4))*
         M^2 + 6*J^2*(-81933831 + 3888*En^6 + 176028189*la - 46091614*la^2 + 
          124456*la^3 + 36*En^4*(-54224 + 44495*la) + 
          12*En^2*(2832305 - 4183966*la + 510439*la^2))*M^4 + 
        18*(-193639196 + 44437284*En^2 + 78215315*la)*M^6)*rp[t]^9 + 
      J^8*M*(J^6*la*(-1302219 + 3907494*la + 75984*la^2 - 4617*la^3 + 
          4*En^4*(-3072 + 8594*la + 1932*la^2 + la^3) + 
          2*En^2*(213435 - 562253*la - 65654*la^2 + 1290*la^3)) + 
        J^4*(75633075 - 342070326*la + 185761117*la^2 - 770440*la^3 - 
          8060*la^4 + 72*En^6*(-67 + 191*la) + 36*En^4*(73773 - 208990*la + 
            36050*la^2) - 6*En^2*(6341349 - 22779904*la + 7938292*la^2 + 
            178998*la^3))*M^2 + 6*J^2*(390072819 + 9606762*En^4 - 
          406952998*la + 42740779*la^2 + 6*En^2*(-28208899 + 14856307*la))*
         M^4 + 2080136052*M^6)*rp[t]^10 + 
      J^8*(-(J^6*la^2*(232926 + 8091*la - 780*la^2 + 
           4*En^4*(648 + 245*la + 2*la^2) + 180*En^2*(-432 - 77*la + 
             3*la^2))) + 2*J^4*(720*En^6*(-2 + la)*la - 
          6*En^4*(13635 - 145572*la + 93725*la^2 + 8652*la^3) + 
          9*(-242685 + 2932772*la - 3342086*la^2 + 11192*la^3 + 758*la^4) - 
          2*En^2*(-604260 + 6453801*la - 5568458*la^2 - 307901*la^3 + 
            3000*la^4))*M^2 + 6*J^2*(-125641143 + 38880*En^6 + 272893021*la - 
          73527734*la^2 + 467364*la^3 + 36*En^4*(-208735 + 169862*la) + 
          2*En^2*(38188899 - 56698676*la + 7155784*la^2))*M^4 + 
        90*(-40058176 + 12607236*En^2 + 16388967*la)*M^6)*rp[t]^11 + 
      J^6*M*(-(J^6*la*(3066624 - 9328865*la - 10496*la^2 + 7599*la^3 + 
           120*En^6*la*(8 + 5*la) + 12*En^4*(9216 - 25063*la - 5249*la^2 + 
             13*la^3) - 6*En^2*(278093 - 734220*la - 73690*la^2 + 
             1756*la^3))) + 3*J^4*(38449773 - 175693846*la + 97878085*la^2 - 
          1221812*la^3 - 244*la^4 + 216*En^6*(-67 + 218*la) + 
          12*En^4*(287259 - 809239*la + 141314*la^2) - 
          2*En^2*(14311857 - 51423562*la + 18361602*la^2 + 291738*la^3))*
         M^2 + 18*J^2*(134046681 + 6922494*En^4 - 141614010*la + 
          15360637*la^2 + 10*En^2*(-8013553 + 4267455*la))*M^4 + 
        1445540040*M^6)*rp[t]^12 + 
      J^6*(2*J^6*la^2*(-274317 - 2989*la + 698*la^2 - 
          18*En^4*(634 + 229*la) + 8*En^6*(7 + 8*la + la^2) - 
          3*En^2*(-50518 - 8005*la + 378*la^2)) + 
        J^4*(-6616620 + 80719833*la - 93993010*la^2 + 1649900*la^3 + 
          5676*la^4 + 1296*En^6*la*(-20 + 13*la) - 
          12*En^4*(50850 - 570627*la + 367457*la^2 + 29594*la^3) + 
          En^2*(5511960 - 58356372*la + 50956000*la^2 + 2137740*la^3 - 
            26104*la^4))*M^2 + 6*J^2*(-128932437 + 187920*En^6 + 
          283512175*la - 78759462*la^2 + 799484*la^3 + 
          60*En^4*(-279769 + 221093*la) + 20*En^2*(5427015 - 8132023*la + 
            1068699*la^2))*M^4 + 36*(-69448470 + 29249094*En^2 + 28797827*la)*
         M^6)*rp[t]^13 + J^4*M*(-(J^6*la*(4645845 - 14369558*la + 
           289090*la^2 + 5127*la^3 + 216*En^6*la*(37 + 22*la) + 
           En^2*(-3803160 + 9990522*la + 816872*la^2 - 23452*la^3) + 
           4*En^4*(103968 - 295077*la - 54314*la^2 + 427*la^3))) + 
        J^4*(117613431 - 544000068*la + 311732345*la^2 - 6674764*la^3 + 
          13500*la^4 + 72*En^6*(-66 + 10481*la) + 
          24*En^4*(1040358 - 2704144*la + 466149*la^2) - 
          30*En^2*(4067175 - 14709160*la + 5427156*la^2 + 44378*la^3))*M^2 + 
        12*J^2*(138998739 + 13515039*En^4 - 148831306*la + 16687262*la^2 + 
          3*En^2*(-37270997 + 20108793*la))*M^4 + 650133000*M^6)*rp[t]^14 + 
      J^4*(2*J^6*la^2*(-415719 + 7494*la + 618*la^2 + 
          En^2*(344076 + 47356*la - 2600*la^2) + 72*En^6*(7 + 8*la + la^2) + 
          4*En^4*(-10563 - 3341*la + 67*la^2)) + 
        J^4*(-6686820 + 82573581*la - 98587558*la^2 + 3292328*la^3 - 
          13068*la^4 + 144*En^6*(-1200 - 356*la + 883*la^2) - 
          12*En^4*(156930 - 1366637*la + 808667*la^2 + 59284*la^3) - 
          60*En^2*(-130719 + 1385750*la - 1237305*la^2 - 31529*la^3 + 
            552*la^4))*M^2 + 18*J^2*(-29569475 + 286080*En^6 + 65909135*la - 
          18907874*la^2 + 265472*la^3 + 120*En^4*(-59981 + 48471*la) + 
          En^2*(33656650 - 51093176*la + 7029072*la^2))*M^4 + 
        36*(-31189466 + 17534982*En^2 + 13110763*la)*M^6)*rp[t]^15 + 
      J^2*M*(-(J^6*la*(4696920 - 14832405*la + 668292*la^2 - 2943*la^3 + 
           72*En^6*(1560 + 631*la + 133*la^2) + 4*En^4*(317292 - 682780*la - 
             119081*la^2 + 1403*la^3) - 20*En^2*(270576 - 716143*la - 
             41107*la^2 + 1524*la^3))) + J^4*(80340039 - 95040*En^8 - 
          376849974*la + 222679457*la^2 - 6872984*la^3 + 20660*la^4 + 
          144*En^6*(-26767 + 21562*la) + 36*En^4*(864258 - 2321859*la + 
            424394*la^2) + 6*En^2*(-18831435 + 69092912*la - 26573794*la^2 + 
            17314*la^3))*M^2 + 12*J^2*(62267925 + 11558511*En^4 - 
          67598578*la + 7835384*la^2 + En^2*(-67563753 + 36816777*la))*M^4 + 
        171979686*M^6)*rp[t]^16 + 
      J^2*(J^6*la^2*(96*En^6*(-167 - 29*la + 3*la^2) + 
          27*(-31150 + 1689*la + 4*la^2) - 30*En^2*(-32562 - 3593*la + 
            232*la^2) + 8*En^4*(-30012 - 8455*la + 272*la^2)) - 
        2*J^4*(4320*En^8*(-15 + 7*la) - 48*En^6*(9630 - 25956*la + 
            3803*la^2) + 6*En^4*(185715 - 1688628*la + 1042213*la^2 + 
            65606*la^3) + 3*(752895 - 9439797*la + 11597156*la^2 - 
            590444*la^3 + 4038*la^4) + 2*En^2*(-1791045 + 19358550*la - 
            17921827*la^2 - 83497*la^3 + 6090*la^4))*M^2 + 
        6*J^2*(-39570741 + 9360*En^6 + 89462423*la - 26523130*la^2 + 
          476024*la^3 + 36*En^4*(-507094 + 426813*la) + 
          4*En^2*(15355611 - 23531714*la + 3381383*la^2))*M^4 + 
        9*(-32991776 + 25088088*En^2 + 14052009*la)*M^6)*rp[t]^17 + 
      M*(J^6*la*(-3175065 - 8640*En^8*(-10 + la) + 10287382*la - 
          764504*la^2 + 8733*la^3 - 48*En^6*(-12750 + 8621*la + 1078*la^2) - 
          12*En^4*(124656 - 273620*la - 49688*la^2 + 719*la^3) + 
          6*En^2*(825265 - 2253085*la - 49578*la^2 + 3842*la^3)) + 
        3*J^4*(11860623 + 429120*En^8 - 56484638*la + 34471293*la^2 - 
          1401528*la^3 + 4964*la^4 - 24*En^6*(-26289 + 8039*la) + 
          12*En^4*(675213 - 2011270*la + 410910*la^2) + 
          En^2*(-22971774 + 85197256*la - 34132296*la^2 + 336508*la^3))*M^2 + 
        9*J^2*(21929451 + 8888916*En^4 - 24124638*la + 2887841*la^2 + 
          4*En^2*(-8205025 + 4469577*la))*M^4 + 20419614*M^6)*rp[t]^18 + 
      (J^6*la^2*(-569010 + 57053*la - 892*la^2 + 960*En^8*(16 + la) + 
          En^2*(894000 + 55524*la - 5436*la^2) + 32*En^6*(3301 + 272*la + 
            la^2) + 12*En^4*(-23672 - 8535*la + 302*la^2)) + 
        2*J^4*(-988065 + 12606717*la - 15980158*la^2 + 1116416*la^3 - 
          9246*la^4 + 1440*En^8*(-267 + 299*la) - 
          192*En^6*(1125 - 3969*la + 1012*la^2) - 
          6*En^4*(102555 - 1339506*la + 968603*la^2 + 30164*la^3) + 
          En^2*(2164500 - 23831634*la + 22909744*la^2 - 439218*la^3 - 
            4720*la^4))*M^2 + 6*J^2*(-10423671 + 282528*En^6 + 23889104*la - 
          7314644*la^2 + 159206*la^3 + 12*En^4*(-954097 + 765710*la) + 
          En^2*(22877610 - 34871512*la + 5165328*la^2))*M^4 + 
        9*(-3922572 + 4113768*En^2 + 1690505*la)*M^6)*rp[t]^19 + 
      M*(J^4*la*(-1390104 + 4641439*la - 501296*la^2 + 7323*la^3 + 
          960*En^8*(-534 + 151*la) - 24*En^6*(12540 - 13782*la + 3679*la^2) - 
          4*En^4*(210024 - 671709*la - 80213*la^2 + 1717*la^3) + 
          En^2*(3000750 - 8501016*la + 211652*la^2 + 9320*la^3)) + 
        J^2*(9321075 - 1978560*En^8 - 45042426*la + 28386577*la^2 - 
          1432970*la^3 + 5520*la^4 + 72*En^6*(27369 + 13250*la) + 
          12*En^4*(1439373 - 3925747*la + 819162*la^2) + 
          6*En^2*(-4394019 + 16095994*la - 6608222*la^2 + 119282*la^3))*M^2 + 
        9*(2609911 + 2562996*En^4 - 2904422*la + 358201*la^2 + 
          4*En^2*(-1386919 + 744192*la))*M^4)*rp[t]^20 + 
      (-2*J^4*la^2*(-480*En^8*(-88 + la) + En^4*(85164 + 32810*la - 
            1504*la^2) + 24*En^6*(919 - 592*la + 9*la^2) + 
          3*(41561 - 6584*la + 150*la^2) + 5*En^2*(-54174 + 1043*la + 
            230*la^2)) + J^2*(-511920 + 6644661*la - 8695853*la^2 + 
          779326*la^3 - 7056*la^4 - 77760*En^8*(-11 + 17*la) + 
          144*En^6*(-6540 + 9716*la + 457*la^2) + 
          12*En^4*(-92640 + 984655*la - 695743*la^2 + 3042*la^3) - 
          12*En^2*(-142500 + 1534667*la - 1497646*la^2 + 62093*la^3 + 
            110*la^4))*M^2 + 12*(-620384 + 489240*En^6 + 1437948*la - 
          453617*la^2 + 11507*la^3 + 18*En^4*(-105195 + 75137*la) + 
          3*En^2*(670813 - 996047*la + 148968*la^2))*M^4)*rp[t]^21 + 
      M*(J^2*la*(-31680*En^8*(-18 + 7*la) - 24*En^6*(26220 - 10423*la + 
            1646*la^2) - 4*En^4*(191856 - 527549*la + 1054*la^2 + 681*la^3) + 
          2*En^2*(593334 - 1681055*la + 122440*la^2 + 738*la^3) + 
          3*(-120138 + 414105*la - 60163*la^2 + 972*la^3)) + 
        (1107684 + 509760*En^8 - 5413613*la + 3514335*la^2 - 210586*la^3 + 
          848*la^4 + 72*En^6*(-52924 + 55703*la) + 
          72*En^4*(98553 - 220270*la + 42545*la^2) + 
          6*En^2*(-817122 + 2863627*la - 1173222*la^2 + 28192*la^3))*M^2)*
       rp[t]^22 + (-24*(-1 + En^2)*J^2*la^2*(-2696 + 40*En^6*(-98 + la) + 
          618*la - 16*la^2 + En^2*(6181 - 422*la - 33*la^2) + 
          2*En^4*(67 - 284*la + 9*la^2)) + (-60480 + 794763*la - 
          1070885*la^2 + 116920*la^3 - 1104*la^4 + 25920*En^8*(-7 + 13*la) + 
          144*En^6*(4080 - 17976*la + 4915*la^2) + 
          12*En^4*(-57600 + 408669*la - 245069*la^2 + 4852*la^3) + 
          4*En^2*(86400 - 861723*la + 822785*la^2 - 47320*la^3 + 24*la^4))*
         M^2)*rp[t]^23 + 6*(-1 + En^2)*la*(7100 - 25183*la + 4635*la^2 - 
        78*la^3 + 480*En^6*(-42 + 19*la) + En^4*(45840 - 66884*la + 
          412*la^2) - 2*En^2*(16380 - 40692*la + 3389*la^2 + 35*la^3))*M*
       rp[t]^24 - 32*(-1 + En^2)^2*la^2*(239 - 74*la + 2*la^2 + 
        30*En^4*(22 + la) + En^2*(-848 + 101*la + 4*la^2))*rp[t]^25))/
    ((1 + la)*rp[t]^14*(3*M + la*rp[t])^2*(J^2 + rp[t]^2)^9))*
  Derivative[1][rp][t]
]


Clear[fSourceZM6]
fSourceZM6[syms_Association]:=
Module[{mu,M,J,la,YBar,YPhiBar,YPhiPhiBar,En,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];

(-8*mu*Pi*YBar*(-2*M + rp[t])^3*(-179991*J^16*M^5 + 
    12*J^16*(23757 - 5501*la)*M^4*rp[t] - 
    4*J^14*M^3*(J^2*(41168 - 26192*la + 185*la^2) + 320409*M^2)*rp[t]^2 - 
    3*J^14*M^2*(J^2*(-13588 + 20213*la - 340*la^2) + 
      6*(-113363 + 16878*En^2 + 26134*la)*M^2)*rp[t]^3 - 
    J^12*M*(3*J^4*(1200 - 5013*la + 155*la^2) + 
      2*J^2*(592736 - 375089*la + 2780*la^2 + 6*En^2*(-27742 + 9099*la))*
       M^2 + 3877344*M^4)*rp[t]^4 - 2*J^12*(-35*J^4*(-19 + la)*la + 
      J^2*(En^2*(57498 - 59944*la + 480*la^2) - 
        3*(49228 - 72749*la + 1280*la^2))*M^2 + 
      9*(-345397 + 111990*En^2 + 79056*la)*M^4)*rp[t]^5 - 
    2*J^10*M*(J^4*(En^2*(-6090 + 20756*la - 420*la^2) + 
        3*(4380 - 18149*la + 585*la^2)) + J^2*(1820119 + 66096*En^4 - 
        1142347*la + 8734*la^2 + 30*En^2*(-37202 + 12117*la))*M^2 + 
      3202002*M^4)*rp[t]^6 + 
    J^10*(5*J^4*la*(-1939 + En^2*(882 - 36*la) + 106*la) + 
      J^2*(-84*En^4*(-964 + 551*la) - 20*En^2*(39039 - 40289*la + 366*la^2) + 
        3*(304978 - 446423*la + 8084*la^2))*M^2 - 
      6*(-1729401 + 933906*En^2 + 390958*la)*M^4)*rp[t]^7 - 
    J^8*M*(J^4*(82215 - 336885*la + 11151*la^2 + 
        12*En^4*(875 - 2369*la + 21*la^2) - 20*En^2*(4200 - 14107*la + 
          321*la^2)) + 2*J^2*(3073004 + 414396*En^4 - 1902437*la + 
        14544*la^2 + 6*En^2*(-523748 + 169081*la))*M^2 + 6102630*M^4)*
     rp[t]^8 - 2*J^8*(J^4*la*(15137 - 848*la - 6*En^4*(-309 + 8*la) + 
        15*En^2*(-1013 + 46*la)) + 
      J^2*(6516*En^6 + 6*En^4*(-43533 + 24278*la) + 
        2*En^2*(557571 - 569105*la + 5778*la^2) - 
        3*(260815 - 375944*la + 6792*la^2))*M^2 + 
      15*(-335247 + 278754*En^2 + 74072*la)*M^4)*rp[t]^9 - 
    2*J^6*M*(2*J^4*(35685 - 143655*la + 4734*la^2 + 6*En^6*(-80 + 181*la) + 
        En^2*(-60930 + 202009*la - 5133*la^2) + 
        9*En^4*(1960 - 5101*la + 56*la^2)) + 
      J^2*(1050876*En^4 + 30*En^2*(-159756 + 50563*la) + 
        5*(606827 - 366635*la + 2622*la^2))*M^2 + 1569798*M^4)*rp[t]^10 + 
    J^6*(2*J^4*la*(-26197 + 320*En^6 + 1458*la + 48*En^4*(-259 + 8*la) - 
        102*En^2*(-433 + 22*la)) + J^2*(-87336*En^6 - 
        36*En^4*(-37221 + 20785*la) - 20*En^2*(174651 - 173770*la + 
          1872*la^2) + 15*(105152 - 147619*la + 2492*la^2))*M^2 - 
      6*(-892419 + 1152990*En^2 + 187262*la)*M^4)*rp[t]^11 - 
    J^4*M*(J^4*(24*En^6*(-640 + 1213*la) + 12*En^4*(14880 - 39806*la + 
          601*la^2) - 40*En^2*(9879 - 31636*la + 846*la^2) + 
        15*(9822 - 38391*la + 1181*la^2)) + 
      2*J^2*(1673936 + 1486296*En^4 - 960659*la + 5100*la^2 + 
        30*En^2*(-136838 + 41729*la))*M^2 + 557496*M^4)*rp[t]^12 - 
    2*J^4*(J^4*la*(26885 - 2560*En^6 + En^4*(32028 - 1488*la) - 1395*la + 
        30*En^2*(-2383 + 126*la)) + J^2*(-451902 + 76968*En^6 + 602187*la - 
        7680*la^2 + 12*En^4*(-85249 + 44290*la) + 
        10*En^2*(156195 - 148310*la + 1572*la^2))*M^2 + 
      9*(-60823 + 153058*En^2 + 10024*la)*M^4)*rp[t]^13 + 
    2*J^2*M*(-(J^4*(44010 - 162927*la + 3855*la^2 + 24*En^6*(20 + 1141*la) + 
         12*En^4*(12980 - 30443*la + 484*la^2) - 
         10*En^2*(18723 - 56296*la + 1452*la^2))) + 
      J^2*(-387301 - 1094472*En^4 + 182209*la + 910*la^2 - 
        6*En^2*(-289802 + 82159*la))*M^2 + 90378*M^4)*rp[t]^14 - 
    J^2*(J^4*la*(31775 - 1290*la + 32*En^6*(-17 + 18*la) - 
        48*En^4*(-2317 + 106*la) + 30*En^2*(-4483 + 222*la)) + 
      J^2*(396432*En^6 + 12*En^4*(-140346 + 64775*la) + 
        3*(-77986 + 87833*la + 580*la^2) + 4*En^2*(354909 - 310337*la + 
          2790*la^2))*M^2 + 6*(37749 + 26310*En^2 - 12782*la)*M^4)*rp[t]^15 + 
    M*(J^4*(-25335 + 31680*En^8 + 80979*la + 375*la^2 - 
        48*En^6*(-2380 + 2889*la) - 36*En^4*(8485 - 16519*la + 221*la^2) + 
        4*En^2*(46350 - 126283*la + 2685*la^2)) + 
      2*J^2*(45436 - 258156*En^4 - 49513*la + 1496*la^2 - 
        30*En^2*(-5056 + 811*la))*M^2 + 72981*M^4)*rp[t]^16 + 
    2*((-1 + En^2)*J^4*la*(4445 + 5280*En^6 + En^4*(25136 - 544*la) - 10*la + 
        10*En^2*(-2839 + 128*la)) - 3*J^2*(1781 + 47820*En^6 - 7006*la + 
        648*la^2 + 6*En^4*(-12891 + 4994*la) - 
        10*En^2*(-2799 + 1651*la + 10*la^2))*M^2 + 
      3*(-18249 + 25074*En^2 + 4762*la)*M^4)*rp[t]^17 + 
    2*M*(-6*(-1 + En^2)*J^2*(-30 + 3840*En^6 - 490*la + 137*la^2 + 
        En^4*(-6080 + 8194*la) + En^2*(2320 - 5185*la + 112*la^2)) + 
      (29651 + 52596*En^4 - 21443*la + 376*la^2 + 6*En^2*(-13632 + 4679*la))*
       M^2)*rp[t]^18 + 4*(-1 + En^2)*(-4*(-1 + En^2)*J^2*la*
       (-1 + 960*En^4 + 14*la + 14*En^2*(-43 + 2*la)) + 
      3*(1130 + 2490*En^4 - 1933*la + 84*la^2 + En^2*(-3563 + 3147*la))*M^2)*
     rp[t]^19 + 12*(-1 + En^2)^2*(90 + 240*En^4 - 440*la + 37*la^2 + 
      En^2*(-320 + 862*la))*M*rp[t]^20 + 32*(-1 + En^2)^3*la*
     (-13 + 30*En^2 + 2*la)*rp[t]^21))/(En*rp[t]^15*(3*M + la*rp[t])*
   (J^2 + rp[t]^2)^7) - (8*J^2*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^3*
   (2499525*J^20*M^6 - 63*J^20*(70203 - 21151*la + 5844*m^2)*M^5*rp[t] + 
    6*J^18*M^4*(J^2*(500081 - 380170*la + 27166*la^2 + 
        (96993 - 23704*la)*m^2 + 570*m^4) + 4438026*M^2)*rp[t]^2 + 
    J^18*M^3*(2*J^2*(-481293 + 741699*la - 127902*la^2 + 1250*la^3 + 
        (-167607 + 111853*la - 1990*la^2)*m^2 + 10*(-234 + 59*la)*m^4) + 
      27*(-1747473 + 138850*En^2 + 522256*la - 136924*m^2)*M^2)*rp[t]^3 + 
    J^16*M^2*(J^4*(142920 - 449824*la + 146127*la^2 - 3420*la^3 + 
        3*(27543 - 42631*la + 1820*la^2)*m^2 - 45*(-47 + 36*la)*m^4) - 
      3*J^2*(-10686130 + 8063009*la - 567260*la^2 + 2*(-978687 + 236836*la)*
         m^2 - 9960*m^4 + 6*En^2*(266302 - 109099*la + 34376*m^2))*M^2 + 
      128407059*M^4)*rp[t]^4 + 
    J^16*M*(3*J^4*(-2520 + 20556*la - 11889*la^2 + 515*la^3 - 
        3*(805 - 3462*la + 275*la^2)*m^2 + 35*(-3 + 7*la)*m^4) + 
      2*J^2*(10*(-515142 + 788466*la - 133782*la^2 + 1321*la^3) + 
        (-1700838 + 1123607*la - 18830*la^2)*m^2 + 10*(-2061 + 514*la)*m^4 + 
        6*En^2*(177443 - 199797*la + 19527*la^2 + (56414 - 19788*la)*m^2 + 
          640*m^4))*M^2 + 9*(-25319482 + 4183746*En^2 + 7498019*la - 
        1847088*m^2)*M^4)*rp[t]^5 + 
    J^14*(-10*J^6*la*(288 - 309*la + 23*la^2 + (268 - 37*la)*m^2 + 11*m^4) + 
      J^4*(1532826 - 4795900*la + 1531665*la^2 - 36180*la^3 + 
        3*(281412 - 431009*la + 17300*la^2)*m^2 - 90*(-209 + 158*la)*m^4 + 
        2*En^2*(-188001 + 500020*la - 126982*la^2 + 1440*la^3 + 
          (-116391 + 128528*la - 3600*la^2)*m^2 + 30*(-113 + 44*la)*m^4))*
       M^2 + 3*J^2*(51701742 + 535680*En^4 - 38679669*la + 2672000*la^2 + 
        (8852736 - 2117968*la)*m^2 + 37800*m^4 - 
        6*En^2*(2686442 - 1088777*la + 327408*m^2))*M^4 + 369586800*M^6)*
     rp[t]^6 + J^14*M*(J^4*(3*(-27090 + 219954*la - 124925*la^2 + 5455*la^3 + 
          (-24885 + 105839*la - 7885*la^2)*m^2 + 35*(-27 + 62*la)*m^4) + 
        En^2*(20790 - 161094*la + 86476*la^2 - 2520*la^3 + 
          (24360 - 86894*la + 6360*la^2)*m^2 - 30*(-49 + 78*la)*m^4)) - 
      2*J^2*(24973569 - 37930122*la + 6315210*la^2 - 62840*la^3 + 
        (7744842 - 5055808*la + 79000*la^2)*m^2 - 70*(-1125 + 278*la)*m^4 + 
        36*En^4*(17599 - 11278*la + 3797*m^2) - 
        6*En^2*(1799329 - 2004875*la + 192483*la^2 - 28*(-19379 + 6693*la)*
           m^2 + 5320*m^4))*M^2 + 36*(-18251581 + 4717326*En^2 + 5348064*la - 
        1221108*m^2)*M^4)*rp[t]^7 + 
    J^12*(5*J^6*la*(-6192 + 6513*la - 488*la^2 + 178*(-31 + 4*la)*m^2 - 
        197*m^4 + 6*En^2*(256 - 299*la + 18*la^2 + (294 - 46*la)*m^2 + 
          17*m^4)) + J^4*(12*En^4*(23307 - 49667*la + 7571*la^2 + 
          (13871 - 8644*la)*m^2 + 340*m^4) + 2*En^2*(-1918098 + 5050619*la - 
          1258508*la^2 + 14580*la^3 - 8*(141666 - 153848*la + 4005*la^2)*
           m^2 + 210*(-137 + 52*la)*m^4) + 3*(2482719 - 7716090*la + 
          2416110*la^2 - 57440*la^3 + 2*(645939 - 977048*la + 36440*la^2)*
           m^2 - 210*(-115 + 86*la)*m^4))*M^2 + 
      12*J^2*(37344117 + 1281636*En^4 - 27665975*la + 1869880*la^2 - 
        28*(-210522 + 49691*la)*m^2 + 20370*m^4 - 
        6*En^2*(3043987 - 1218603*la + 346612*m^2))*M^4 + 704436642*M^6)*
     rp[t]^8 + J^12*M*(J^4*(12*En^4*(-1260 + 9839*la - 4556*la^2 + 71*la^3 + 
          (-1750 + 5143*la - 274*la^2)*m^2 + 4*(-35 + 29*la)*m^4) + 
        3*(-131985 + 1065999*la - 592930*la^2 + 26020*la^3 + 
          (-115395 + 484366*la - 33380*la^2)*m^2 + 245*(-15 + 34*la)*m^4) - 
        2*En^2*(-106785 + 820086*la - 431407*la^2 + 12810*la^3 - 
          2*(60270 - 210943*la + 14340*la^2)*m^2 + 105*(-61 + 94*la)*m^4)) + 
      2*J^2*(-72319203 + 92880*En^6 + 108871902*la - 17723520*la^2 + 
        177160*la^3 - 14*(1486107 - 956128*la + 13780*la^2)*m^2 + 
        70*(-2439 + 598*la)*m^4 - 36*En^4*(170477 - 107428*la + 34759*m^2) + 
        6*En^2*(8202641 - 9031004*la + 849132*la^2 - 84*(-27653 + 9381*la)*
           m^2 + 18480*m^4))*M^2 + 126*(-9959997 + 3591804*En^2 + 
        2882363*la - 597612*m^2)*M^4)*rp[t]^9 + 
    J^10*(-(J^6*la*(24*En^4*(225 - 287*la + 14*la^2 + (309 - 56*la)*m^2 + 
           24*m^4) - 30*En^2*(2630 - 3009*la + 184*la^2 + 
           (2908 - 422*la)*m^2 + 147*m^4) + 5*(30168 - 31023*la + 2332*la^2 + 
           (25562 - 3032*la)*m^2 + 763*m^4))) - 
      2*J^4*(36*En^6*(784 - 1193*la + 392*m^2) - 
        6*En^4*(229197 - 479932*la + 71438*la^2 + (130077 - 78868*la)*m^2 + 
          2880*m^4) + En^2*(8802954 - 22918678*la + 5585932*la^2 - 
          65760*la^3 + 28*(175491 - 187148*la + 4410*la^2)*m^2 - 
          630*(-161 + 60*la)*m^4) + 3*(-3604584 + 11116499*la - 
          3400770*la^2 + 81080*la^3 - 7*(250533 - 372976*la + 12760*la^2)*
           m^2 + 105*(-251 + 186*la)*m^4))*M^2 + 
      84*J^2*(10211700 + 779220*En^4 - 7479983*la + 492170*la^2 + 
        (1454787 - 338116*la)*m^2 + 3900*m^4 - 
        6*En^2*(1165935 - 459995*la + 122692*m^2))*M^4 + 931822920*M^6)*
     rp[t]^10 + J^10*M*
     (J^4*(48*En^6*(45 - 467*la + 164*la^2 + (80 - 216*la)*m^2 + 10*m^4) + 
        3*(-384615 + 3086014*la - 1675340*la^2 + 73580*la^3 - 
          7*(45375 - 187082*la + 11740*la^2)*m^2 + 245*(-33 + 74*la)*m^4) + 
        En^2*(986580 - 7509510*la + 3857656*la^2 - 116160*la^3 + 
          28*(37890 - 130289*la + 7980*la^2)*m^2 - 630*(-73 + 110*la)*m^4) + 
        12*En^4*(-12600 + 96769*la - 43707*la^2 + 706*la^3 + 
          (-17010 + 48165*la - 2414*la^2)*m^2 + 4*(-315 + 244*la)*m^4)) + 
      2*J^2*(886896*En^6 - 36*En^4*(732979 - 454124*la + 138057*m^2) + 
        42*En^2*(3168073 - 3434396*la + 314964*la^2 + (836092 - 276324*la)*
           m^2 + 5000*m^4) + 7*(-19819377 + 29546064*la - 4679460*la^2 + 
          46820*la^3 + (-5188536 + 3281614*la - 43180*la^2)*m^2 + 
          50*(-657 + 160*la)*m^4))*M^2 + 126*(-13212031 + 6238902*En^2 + 
        3764624*la - 686772*m^2)*M^4)*rp[t]^11 + 
    J^8*(J^6*la*(40*En^6*(18 - 27*la + la^2 - 8*(-4 + la)*m^2 + 4*m^4) - 
        12*En^4*(4500 - 5639*la + 283*la^2 + (5998 - 1032*la)*m^2 + 
          428*m^4) + 30*En^2*(12169 - 13571*la + 840*la^2 - 
          14*(-916 + 119*la)*m^2 + 525*m^4) - 
        5*(87867 - 88065*la + 6608*la^2 + (70210 - 7504*la)*m^2 + 
          1673*m^4)) + J^4*(-72*En^6*(7840 - 11483*la + 3752*m^2) + 
        36*En^4*(329759 - 684848*la + 99226*la^2 + (173417 - 103948*la)*m^2 + 
          3100*m^4) - 21*(-1979721 + 6059398*la - 1802490*la^2 + 42920*la^3 - 
          2*(443142 - 647069*la + 20060*la^2)*m^2 + 300*(-34 + 25*la)*m^4) + 
        28*En^2*(-1722573 + 4399753*la - 1044322*la^2 + 12360*la^3 - 
          4*(225966 - 233828*la + 4815*la^2)*m^2 + 75*(-185 + 68*la)*m^4))*
       M^2 + 42*J^2*(27169812 + 3936024*En^4 - 19621461*la + 1247396*la^2 + 
        (3376074 - 771472*la)*m^2 + 6660*m^4 - 
        6*En^2*(4067348 - 1583339*la + 388080*m^2))*M^4 + 869856246*M^6)*
     rp[t]^12 + J^8*M*(J^4*(48*En^6*(495 - 4745*la + 1608*la^2 + 
          (880 - 2096*la)*m^2 + 110*m^4) + 21*(-105750 + 845011*la - 
          446050*la^2 + 19510*la^3 + (-81675 + 329158*la - 18530*la^2)*m^2 + 
          175*(-9 + 20*la)*m^4) - 14*En^2*(-198450 + 1461759*la - 
          728536*la^2 + 21960*la^3 + (-201300 + 666986*la - 35160*la^2)*m^2 + 
          75*(-85 + 126*la)*m^4) + 12*En^4*(-51660 + 418321*la - 
          184149*la^2 + 3130*la^3 + (-66990 + 193027*la - 8730*la^2)*m^2 + 
          20*(-210 + 157*la)*m^4)) + 2*J^2*(3441744*En^6 - 
        36*En^4*(141*(13641 - 8044*la) + 332035*m^2) + 
        42*En^2*(5547173 - 5951818*la + 530766*la^2 - 140*(-9656 + 3087*la)*
           m^2 + 5600*m^4) + 7*(-26437293 + 38938338*la - 5951592*la^2 + 
          59380*la^3 + (-6092226 + 3780554*la - 45020*la^2)*m^2 + 
          10*(-2817 + 682*la)*m^4))*M^2 + 126*(-12389056 + 7400130*En^2 + 
        3458277*la - 528648*m^2)*M^4)*rp[t]^13 + 
    J^6*(5*J^6*la*(88*En^6*(18 - 27*la + la^2 - 8*(-4 + la)*m^2 + 4*m^4) + 
        210*En^2*(973 - 1039*la + 64*la^2 + (972 - 106*la)*m^2 + 29*m^4) - 
        12*En^4*(3792 - 4777*la + 257*la^2 + (4798 - 768*la)*m^2 + 284*m^4) - 
        7*(24165 - 23589*la + 1756*la^2 + (17990 - 1700*la)*m^2 + 325*m^4)) - 
      2*J^4*(36*En^6*(22868 - 44517*la + 11200*m^2) - 
        6*En^4*(2835537 - 5308644*la + 730006*la^2 + (1317025 - 739220*la)*
           m^2 + 15200*m^4) - 14*En^2*(-3023640 + 7679597*la - 1778228*la^2 + 
          20880*la^3 - 5*(301191 - 299288*la + 5220*la^2)*m^2 + 
          1425*(-11 + 4*la)*m^4) + 21*(-1322754 + 4014201*la - 1151367*la^2 + 
          27260*la^3 + (-528237 + 754579*la - 20980*la^2)*m^2 + 
          15*(-293 + 214*la)*m^4))*M^2 + 42*J^2*(25603260 + 6117480*En^4 - 
        18131201*la + 1101040*la^2 - 8*(-328227 + 73681*la)*m^2 + 3540*m^4 - 
        6*En^2*(4842268 - 1854081*la + 398104*m^2))*M^4 + 568363824*M^6)*
     rp[t]^14 + J^6*M*(J^4*(48*En^6*(-3825 - 14431*la + 6044*la^2 - 
          40*(-5 + 163*la)*m^2 + 340*m^4) + 
        21*(2*(-70560 + 562905*la - 286537*la^2 + 12415*la^3) + 
          (-99345 + 389948*la - 19450*la^2)*m^2 + 35*(-39 + 86*la)*m^4) + 
        12*En^4*(-197820 + 1166801*la - 462343*la^2 + 7930*la^3 - 
          5*(37282 - 96629*la + 3406*la^2)*m^2 + 40*(-175 + 128*la)*m^4) - 
        14*En^2*(-347625 + 2565831*la - 1257704*la^2 + 37320*la^3 - 
          20*(17535 - 55061*la + 2400*la^2)*m^2 + 75*(-97 + 142*la)*m^4)) + 
      2*J^2*(11818224*En^6 - 180*En^4*(595797 - 354344*la + 101971*m^2) + 
        42*En^2*(6606859 - 7024966*la + 601686*la^2 + (1421412 - 438204*la)*
           m^2 + 3720*m^4) + 7*(5*(-5008581 + 7246386*la - 1055052*la^2 + 
            10456*la^3) + (-4799034 + 2918776*la - 31240*la^2)*m^2 + 
          10*(-1503 + 362*la)*m^4))*M^2 + 252*(-4077819 + 3055002*En^2 + 
        1107008*la - 131196*m^2)*M^4)*rp[t]^15 + 
    J^4*(5*J^6*la*(8*En^6*(-1278 - 771*la + 61*la^2 - 8*(-19 + 40*la)*m^2 + 
          136*m^4) + 42*En^2*(8500 - 9147*la + 548*la^2 + 
          (8420 - 730*la)*m^2 + 165*m^4) - 7*(32310 - 30537*la + 2240*la^2 + 
          (21778 - 1792*la)*m^2 + 281*m^4) - 12*En^4*(13848 - 12415*la + 
          667*la^2 - 2*(-6673 + 764*la)*m^2 + 472*m^4)) + 
      J^4*(-730944*En^8 - 72*En^6*(177452 - 139983*la + 35280*m^2) + 
        60*En^4*(862287 - 1667070*la + 231044*la^2 + (436329 - 222652*la)*
           m^2 + 2700*m^4) + 28*En^2*(-3566709 + 9123670*la - 2041438*la^2 + 
          23460*la^3 - 4*(409266 - 388928*la + 5625*la^2)*m^2 + 
          45*(-233 + 84*la)*m^4) - 21*(-2518131 + 7532290*la - 2051640*la^2 + 
          48080*la^3 - 2*(422883 - 590156*la + 14600*la^2)*m^2 + 
          30*(-157 + 114*la)*m^4))*M^2 + 12*J^2*(59516643 + 21945420*En^4 - 
        40976551*la + 2339200*la^2 + (4610484 - 1016492*la)*m^2 + 3750*m^4 - 
        42*En^2*(2020539 - 748609*la + 128276*m^2))*M^4 + 249693705*M^6)*
     rp[t]^16 + J^4*M*(J^4*(576*En^8*(840 - 493*la + 210*m^2) + 
        48*En^6*(43965 - 95341*la + 15440*la^2 - 80*(-109 + 244*la)*m^2 + 
          460*m^4) + 21*(-134595 + 1066457*la - 514000*la^2 + 21940*la^3 + 
          (-81255 + 310102*la - 13580*la^2)*m^2 + 35*(-21 + 46*la)*m^4) - 
        14*En^2*(-391635 + 3044331*la - 1468834*la^2 + 42180*la^3 - 
          2*(200070 - 592363*la + 20820*la^2)*m^2 + 45*(-109 + 158*la)*m^4) + 
        60*En^4*(-56700 + 361511*la - 152061*la^2 + 2488*la^3 + 
          (-70938 + 157121*la - 3902*la^2)*m^2 + 4*(-315 + 227*la)*m^4)) + 
      2*J^2*(-117687249 + 12827376*En^6 + 165522042*la - 22520160*la^2 + 
        220840*la^3 - 2*(8536671 - 5087404*la + 48700*la^2)*m^2 + 
        10*(-3195 + 766*la)*m^4 - 36*En^4*(2942679 - 1828268*la + 
          476277*m^2) + 42*En^2*(5569277 - 5756020*la + 458244*la^2 + 
          (938756 - 279252*la)*m^2 + 1360*m^4))*M^2 + 
      9*(-50741509 + 48282984*En^2 + 13278461*la - 1065132*m^2)*M^4)*
     rp[t]^17 + (5*J^8*la*(1152*En^8*(27 - 3*la + 7*m^2) + 
        42*En^2*(9668 - 10957*la + 624*la^2 + (9532 - 638*la)*m^2 + 
          111*m^4) - 7*(30906 - 27639*la + 1984*la^2 - 2*(-8857 + 628*la)*
           m^2 + 151*m^4) + 8*En^6*(5*(3510 - 501*la + 35*la^2) + 
          (3776 - 560*la)*m^2 + 184*m^4) - 12*En^4*(19416 - 21857*la + 
          1069*la^2 + (25070 - 1776*la)*m^2 + 424*m^4)) + 
      2*J^6*(2259648*En^8 - 36*En^6*(144288 - 171627*la + 70280*m^2) + 
        6*En^4*(3705567 - 8493812*la + 1198026*la^2 - 3*(-728477 + 340468*la)*
           m^2 + 6240*m^4) + 14*En^2*(-3018897 + 7599431*la - 1577222*la^2 + 
          17520*la^3 - 2*(558291 - 508148*la + 6030*la^2)*m^2 + 
          15*(-257 + 92*la)*m^4) - 3*(-5996052 + 17442785*la - 4404930*la^2 + 
          101720*la^3 + (-1529289 + 2084548*la - 45640*la^2)*m^2 + 
          15*(-335 + 242*la)*m^4))*M^2 + 6*J^4*(53667999 + 32953032*En^4 - 
        35536800*la + 1868710*la^2 + (2702157 - 585296*la)*m^2 + 990*m^4 - 
        12*En^2*(8193047 - 2853593*la + 330132*m^2))*M^4 + 66716892*J^2*M^6)*
     rp[t]^18 + J^2*M*(J^4*(-241920*En^10 + 1152*En^8*(-2100 + 1226*la + 
          35*m^2) + 48*En^6*(23985 - 87549*la + 27740*la^2 - 
          40*(-814 + 929*la)*m^2 + 290*m^4) + 3*(-651735 + 5031674*la - 
          2223980*la^2 + 93020*la^3 + (-300195 + 1113866*la - 42580*la^2)*
           m^2 + 35*(-45 + 98*la)*m^4) - 14*En^2*(-322380 + 2574543*la - 
          1157276*la^2 + 31680*la^3 + (-285420 + 799918*la - 22440*la^2)*
           m^2 + 15*(-121 + 174*la)*m^4) + 12*En^4*(-109620 + 1674819*la - 
          826161*la^2 + 12326*la^3 + (-397110 + 773743*la - 13194*la^2)*m^2 + 
          4*(-735 + 524*la)*m^4)) + 2*J^2*(-54071928 + 6603984*En^6 + 
        72909837*la - 9041670*la^2 + 87410*la^3 + 
        (-5067873 + 2960617*la - 25270*la^2)*m^2 + 10*(-423 + 101*la)*m^4 - 
        36*En^4*(2280497 - 1324868*la + 242819*m^2) + 
        6*En^2*(23383381 - 22550804*la + 1579932*la^2 + (2471372 - 711444*la)*
           m^2 + 1480*m^4))*M^2 + 9*(-13774963 + 16977366*En^2 + 3444592*la - 
        137268*m^2)*M^4)*rp[t]^19 + 
    (J^6*la*(-80640*En^10 + 1920*En^8*(-418 - 15*la + 7*m^2) + 
        210*En^2*(8095 - 8887*la + 472*la^2 + (6744 - 346*la)*m^2 + 41*m^4) + 
        40*En^6*(8442 - 7305*la + 275*la^2 - 8*(-1682 + 65*la)*m^2 + 
          116*m^4) - 5*(149823 - 120843*la + 8432*la^2 + (65062 - 3952*la)*
           m^2 + 323*m^4) - 12*En^4*(39840 - 128869*la + 5393*la^2 - 
          22*(-6299 + 276*la)*m^2 + 988*m^4)) + 
      J^4*(-1813248*En^8 - 72*En^6*(-60784 - 120801*la + 63112*m^2) + 
        12*En^4*(2924847 - 6425512*la + 785858*la^2 + (1176137 - 513388*la)*
           m^2 + 1180*m^4) - 3*(-5656173 + 15697758*la - 3559965*la^2 + 
          80660*la^3 + (-922437 + 1228939*la - 23740*la^2)*m^2 + 
          15*(-89 + 64*la)*m^4) + 4*En^2*(-13341357 + 30929449*la - 
          5522266*la^2 + 58680*la^3 - 4*(756366 - 662348*la + 6435*la^2)*
           m^2 + 15*(-281 + 100*la)*m^4))*M^2 + 
      3*J^2*(29738710 + 34832880*En^4 - 18737089*la + 886860*la^2 + 
        (703686 - 149848*la)*m^2 - 6*En^2*(12012010 - 3826995*la + 
          211096*m^2))*M^4 + 8225415*M^6)*rp[t]^20 + 
    M*(J^4*(1209600*En^10 - 1152*En^8*(-630 + 513*la + 245*m^2) + 
        48*En^6*(-70245 + 4697*la + 30344*la^2 + (35840 - 32656*la)*m^2 + 
          70*m^4) + En^2*(3122280 - 22228230*la + 8274656*la^2 - 
          213360*la^3 + 4*(401730 - 1073953*la + 24060*la^2)*m^2 - 
          570*(-7 + 10*la)*m^4) + 3*(-320040 + 2336019*la - 906265*la^2 + 
          36955*la^3 + (-92400 + 333724*la - 11105*la^2)*m^2 + 
          35*(-6 + 13*la)*m^4) + 12*En^4*(-60480 + 1328979*la - 568127*la^2 + 
          7526*la^3 + (-230650 + 411105*la - 4894*la^2)*m^2 + 
          4*(-140 + 99*la)*m^4)) + 6*J^2*(-5132103 + 3153744*En^6 + 
        6544386*la - 719140*la^2 + 6830*la^3 + 
        (-222684 + 127641*la - 970*la^2)*m^2 - 
        12*En^4*(1377319 - 643764*la + 51457*m^2) + 
        2*En^2*(9091166 - 7879037*la + 455823*la^2 - 42*(-9597 + 2684*la)*
           m^2))*M^2 + 9*(-1731918 + 2794098*En^2 + 411079*la)*M^4)*
     rp[t]^21 + (J^4*la*(403200*En^10 - 1920*En^8*(-136 - 9*la + 49*m^2) + 
        40*En^6*(-28818 - 10707*la + 241*la^2 - 8*(-1828 + 31*la)*m^2 + 
          28*m^4) - 5*(73287 - 49815*la + 3358*la^2 - 22*(-905 + 47*la)*m^2 + 
          43*m^4) + 30*En^2*(39239 - 32751*la + 1600*la^2 + 
          (18844 - 746*la)*m^2 + 45*m^4) - 12*En^4*(26940 - 95719*la + 
          3343*la^2 + (79738 - 2272*la)*m^2 + 188*m^4)) + 
      J^2*(5024106 - 4141440*En^8 - 13059058*la + 2566065*la^2 - 56820*la^3 + 
        (370314 - 482823*la + 8220*la^2)*m^2 - 
        72*En^6*(82588 - 124615*la + 20272*m^2) + 
        12*En^4*(2318787 - 3532844*la + 295058*la^2 + (259371 - 107644*la)*
           m^2) - 2*En^2*(11380887 - 22948702*la + 3237058*la^2 - 
          32640*la^3 + (1011591 - 856928*la + 6840*la^2)*m^2))*M^2 + 
      3*(3834106 + 8940816*En^4 - 2280709*la + 94912*la^2 + 
        6*En^2*(-2075662 + 598161*la))*M^4)*rp[t]^22 + 
    M*(-(J^2*(1209600*En^10 + 5760*En^8*(-462 + 214*la + 35*m^2) + 
         48*En^6*(14805 + 62369*la - 17508*la^2 + 8*(-1575 + 1297*la)*m^2) + 
         12*En^4*(168840 - 940971*la + 222669*la^2 - 2590*la^3 + 
           (53550 - 89827*la + 770*la^2)*m^2) + 
         3*(100800 - 676792*la + 219725*la^2 - 8695*la^3 + 
           (12600 - 44381*la + 1285*la^2)*m^2) - 
         2*En^2*(793800 - 4546368*la + 1238402*la^2 - 29820*la^3 + 
           (138600 - 356317*la + 6420*la^2)*m^2))) + 
      12*(-342078 + 1085688*En^6 + 408752*la - 38707*la^2 + 360*la^3 + 
        12*En^4*(-201898 + 75837*la) + En^2*(1679166 - 1293491*la + 
          58587*la^2))*M^2)*rp[t]^23 + 
    2*(-30*(-1 + En^2)*J^2*la*(-1905 + 6720*En^8 + 1019*la - 66*la^2 + 
        5*(-45 + 2*la)*m^2 + 32*En^6*(-254 - 27*la + 35*m^2) + 
        En^2*(7880 - 4031*la + 159*la^2 + (1390 - 40*la)*m^2) + 
        En^4*(-4588 + 3974*la - 74*la^2 + 16*(-143 + 2*la)*m^2)) + 
      (350964 + 1461024*En^8 - 845144*la + 139131*la^2 - 3000*la^3 + 
        180*En^6*(-25204 + 13937*la) + 6*En^4*(840222 - 950159*la + 
          48201*la^2) + 3*En^2*(-772200 + 1345451*la - 140864*la^2 + 
          1340*la^3))*M^2)*rp[t]^24 + 6*(-1 + En^2)*
     (7560 + 40320*En^8 - 46127*la + 12027*la^2 - 460*la^3 + 
      96*En^6*(-1260 + 1831*la) + 8*En^4*(16065 - 47455*la + 4064*la^2) + 
      En^2*(-55440 + 249692*la - 42974*la^2 + 770*la^3))*M*rp[t]^25 + 
    60*(-1 + En^2)^2*la*(-282 + 1344*En^6 + 113*la - 7*la^2 + 
      96*En^4*(-29 + 4*la) + 2*En^2*(858 - 233*la + 7*la^2))*rp[t]^26))/
  (En*la*(1 + la)*rp[t]^16*(3*M + la*rp[t])*(J^2 + rp[t]^2)^10) + 
 ((16*J*mu*Pi*YPhiBar*(-2*M + rp[t])^2*(248661*J^16*M^5 + 
      3*J^16*(-118514 + 30561*la)*M^4*rp[t] + 
      3*J^14*M^3*(J^2*(61115 - 43832*la + 292*la^2) + 678420*M^2)*rp[t]^2 + 
      J^14*M^2*(J^2*(-39978 + 68105*la - 1004*la^2 + 12*la^3) + 
        6*(-484081 + 48522*En^2 + 126624*la)*M^2)*rp[t]^3 + 
      J^12*M*(J^4*(3060 - 14945*la + 367*la^2 - 12*la^3) + 
        12*J^2*(124503 - 90422*la + 879*la^2 + 5*En^2*(-4666 + 1647*la))*
         M^2 + 7302744*M^4)*rp[t]^4 + J^12*(3*J^4*la*(385 - 14*la + la^2) + 
        J^2*(-324711 + 559016*la - 12138*la^2 + 120*la^3 - 
          6*En^2*(-13917 + 16084*la + 204*la^2))*M^2 + 
        18*(-577801 + 123810*En^2 + 153478*la)*M^4)*rp[t]^5 + 
      J^10*M*(J^4*(3*(8250 - 40623*la + 1484*la^2 - 40*la^3) + 
          En^2*(-7500 + 29442*la + 964*la^2 - 8*la^3)) + 
        12*J^2*(444595 + 6225*En^4 - 327374*la + 4256*la^2 + 
          2*En^2*(-89417 + 32052*la))*M^2 + 15010380*M^4)*rp[t]^6 + 
      J^10*(J^4*la*(2*En^2*(-1365 - 88*la + 2*la^2) + 
          15*(623 - 34*la + 2*la^2)) + J^2*(-1155213 + 2012532*la - 
          59074*la^2 + 516*la^3 + 12*En^4*(-3086 + 1947*la) - 
          6*En^2*(-106849 + 124702*la + 818*la^2))*M^2 + 
        6*(-3555329 + 1228986*En^2 + 960128*la)*M^4)*rp[t]^7 + 
      J^8*M*(J^4*(87615 - 435547*la + 21785*la^2 - 516*la^3 - 
          12*En^4*(-310 + 1014*la + 63*la^2) + En^2*(-57720 + 227314*la + 
            4472*la^2 - 32*la^3)) + 12*J^2*(909112 + 46386*En^4 - 679710*la + 
          11225*la^2 + 15*En^2*(-39512 + 14405*la))*M^2 + 19343610*M^4)*
       rp[t]^8 + J^8*(J^4*la*(-4*En^4*(-324 - 53*la + la^2) + 
          2*En^2*(-10515 - 472*la + 8*la^2) + 3*(11031 - 836*la + 43*la^2)) + 
        J^2*(-2351745 + 1944*En^6 + 4153696*la - 156850*la^2 + 1248*la^3 + 
          36*En^4*(-7795 + 4928*la) + 6*En^2*(354921 - 418478*la + 100*la^2))*
         M^2 + 30*(-914425 + 456750*En^2 + 251323*la)*M^4)*rp[t]^9 + 
      J^6*M*(J^4*(177210 - 891553*la + 648*En^6*la + 58310*la^2 - 1248*la^3 - 
          12*En^4*(-2400 + 7753*la + 388*la^2) + 2*En^2*(-96390 + 379593*la + 
            1840*la^2 + 4*la^3)) + 6*J^2*(295254*En^4 + 
          80*En^2*(-27547 + 10265*la) + 15*(155381 - 118124*la + 2392*la^2))*
         M^2 + 16016436*M^4)*rp[t]^10 + 
      J^6*(-2*J^4*la*(-33489 + 3390*la - 156*la^2 + 
          12*En^4*(-421 - 60*la + la^2) + 2*En^2*(17544 + 400*la + la^2)) + 
        J^2*(17496*En^6 + 36*En^4*(-25729 + 15841*la) + 
          30*En^2*(131769 - 158224*la + 1302*la^2) + 
          5*(-599895 + 1076246*la - 50538*la^2 + 372*la^3))*M^2 + 
        18*(-1259609 + 864090*En^2 + 352576*la)*M^4)*rp[t]^11 + 
      3*J^4*M*(J^4*(74730 - 381555*la + 1944*En^6*la + 31635*la^2 - 
          620*la^3 - 4*En^4*(-8790 + 25379*la + 1047*la^2) + 
          10*En^2*(-11864 + 47419*la - 674*la^2 + 8*la^3)) + 
        4*J^2*(960215 + 249948*En^4 - 742978*la + 17953*la^2 + 
          5*En^2*(-250690 + 95661*la))*M^2 + 2776080*M^4)*rp[t]^12 + 
      J^4*(J^4*la*(En^4*(36504 + 5140*la - 44*la^2) - 
          60*En^2*(2163 - 22*la + 2*la^2) + 15*(5655 - 746*la + 31*la^2)) + 
        J^2*(-2458593 + 98352*En^6 + 4486920*la - 255134*la^2 + 1752*la^3 + 
          24*En^4*(-64049 + 41556*la) + 30*En^2*(149731 - 183840*la + 
            3164*la^2))*M^2 + 6*(-1962481 + 1811682*En^2 + 559514*la)*M^4)*
       rp[t]^13 + J^2*M*(J^4*(182070 - 945575*la + 96952*la^2 - 1752*la^3 + 
          144*En^6*(-200 + 181*la) - 24*En^4*(-6700 + 21393*la + 464*la^2) + 
          10*En^2*(-40038 + 163943*la - 6198*la^2 + 52*la^3)) + 
        12*J^2*(497453 + 259767*En^4 - 391882*la + 11044*la^2 + 
          30*En^2*(-29443 + 11448*la))*M^2 + 2488644*M^4)*rp[t]^14 + 
      J^2*(J^4*la*(68985 - 11622*la + 438*la^2 - 480*En^6*(19 + 4*la) - 
          16*En^4*(-3516 - 410*la + la^2) - 10*En^2*(14661 - 856*la + 
            26*la^2)) + J^2*(-1267731 + 42480*En^6 + 2354756*la - 
          158438*la^2 + 1020*la^3 + 36*En^4*(-44644 + 30411*la) + 
          6*En^2*(531627 - 662894*la + 17386*la^2))*M^2 + 
        6*(-586243 + 725550*En^2 + 170144*la)*M^4)*rp[t]^15 + 
      M*(J^4*(93015 + 17280*En^8 - 491849*la + 60967*la^2 - 1020*la^3 + 
          240*En^6*(132 + 19*la) + 12*En^4*(13110 - 46804*la + 563*la^2) + 
          2*En^2*(-142560 + 591651*la - 37328*la^2 + 256*la^3)) + 
        12*J^2*(148410 + 165618*En^4 - 118954*la + 3831*la^2 + 
          En^2*(-360044 + 140969*la))*M^2 + 327465*M^4)*rp[t]^16 + 
      (J^4*la*(5760*En^8 - 480*En^6*(-23 + 6*la) + 
          12*En^4*(4712 + 5*la + 3*la^2) + 15*(2353 - 496*la + 17*la^2) - 
          2*En^2*(52395 - 6112*la + 128*la^2)) + 
        J^2*(-376983 + 132408*En^6 + 712112*la - 55494*la^2 + 336*la^3 + 
          36*En^4*(-31303 + 20328*la) + 6*En^2*(221739 - 275902*la + 
            9372*la^2))*M^2 + 9*(-51476 + 85644*En^2 + 15187*la)*M^4)*
       rp[t]^17 + M*(J^2*(-46080*En^8 + 24*En^6*(360 + 1879*la) + 
          12*En^4*(11040 - 34085*la + 1284*la^2) + 
          3*(9150 - 49209*la + 7210*la^2 - 112*la^3) + 
          2*En^2*(-61350 + 250227*la - 21284*la^2 + 124*la^3)) + 
        3*(78223 + 193332*En^4 - 63680*la + 2300*la^2 + 
          80*En^2*(-3275 + 1272*la))*M^2)*rp[t]^18 + 
      (-4*(-1 + En^2)*J^2*la*(3240*En^4 + 3840*En^6 - 
          2*En^2*(4329 - 616*la + 5*la^2) + 3*(869 - 224*la + 7*la^2)) + 
        (-49635 + 172728*En^6 + 95121*la - 8410*la^2 + 48*la^3 + 
          12*En^4*(-31187 + 18177*la) + 6*En^2*(41911 - 50948*la + 
            2010*la^2))*M^2)*rp[t]^19 + 2*(-1 + En^2)*
       (-1800 + 8640*En^6 + 9809*la - 1660*la^2 + 24*la^3 + 
        36*En^4*(-480 + 853*la) + 6*En^2*(1750 - 6309*la + 515*la^2))*M*
       rp[t]^20 + 12*(-1 + En^2)^2*la*(114 + 480*En^4 - 35*la + la^2 + 
        40*En^2*(-13 + 2*la))*rp[t]^21))/((1 + la)*rp[t]^13*(3*M + la*rp[t])*
     (J^2 + rp[t]^2)^8) - ((16*I)*J^3*m*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*
     (520317*J^16*M^5 - 3*J^16*(247618 - 74193*la + 5748*m^2)*M^4*rp[t] + 
      3*J^14*M^3*(J^2*(126613 - 104120*la + 4404*la^2 + (7058 - 2080*la)*
           m^2 + 12*m^4) + 1542960*M^2)*rp[t]^2 + 
      J^14*M^2*(J^2*(-81096 + 155975*la - 16188*la^2 + 84*la^3 + 
          (-8436 + 7670*la - 60*la^2)*m^2 + 12*(-3 + la)*m^4) + 
        12*(-552887 + 57042*En^2 + 163968*la - 11529*m^2)*M^2)*rp[t]^3 + 
      J^12*M*(J^4*(5931 - 32180*la + 6429*la^2 - 84*la^3 + 
          5*(216 - 611*la + 12*la^2)*m^2 + (9 - 12*la)*m^4) - 
        6*J^2*(-567998 + 462488*la - 18822*la^2 + (-28603 + 8300*la)*m^2 - 
          36*m^4 + 2*En^2*(53650 - 24084*la + 2347*m^2))*M^2 + 18216468*M^4)*
       rp[t]^4 + J^12*(3*J^4*la*(744 - 273*la + 7*la^2 - 5*(-26 + la)*m^2 + 
          m^4) + 2*J^2*(-365781 + 696882*la - 69564*la^2 + 342*la^3 + 
          (-34611 + 30925*la - 210*la^2)*m^2 + 36*(-3 + la)*m^4 + 
          6*En^2*(2*(7627 - 10993*la + 728*la^2) + (1751 - 845*la)*m^2 + 
            8*m^4))*M^2 + 12*(-2186149 + 479286*En^2 + 640509*la - 39837*m^2)*
         M^4)*rp[t]^5 + J^10*M*(J^4*(53856 - 289595*la + 55629*la^2 - 
          684*la^3 + 5*(1800 - 4991*la + 84*la^2)*m^2 + (54 - 72*la)*m^4 + 
          4*En^2*(-3708 + 17933*la - 3233*la^2 + 26*la^3 - 
            5*(183 - 377*la + 6*la^2)*m^2 + 4*(-3 + 2*la)*m^4)) + 
        6*J^2*(2258202 + 32670*En^4 - 1817240*la + 70578*la^2 + 
          (100029 - 28520*la)*m^2 + 90*m^4 + En^2*(-910950 + 402240*la - 
            36004*m^2))*M^2 + 41532480*M^4)*rp[t]^6 + 
      J^10*(J^4*la*(-4*En^2*(1351 - 557*la + 13*la^2 - 5*(-65 + 3*la)*m^2 + 
            4*m^4) + 3*(6754 - 2383*la + 57*la^2 - 5*(-216 + 7*la)*m^2 + 
            6*m^4)) - 2*J^2*(1463616 - 2757332*la + 262476*la^2 - 1218*la^3 + 
          3*(40956 - 35845*la + 210*la^2)*m^2 - 90*(-3 + la)*m^4 + 
          6*En^4*(7462 - 6675*la + 728*m^2) - 6*En^2*(131171 - 185865*la + 
            11760*la^2 + (13746 - 6430*la)*m^2 + 40*m^4))*M^2 + 
        12*(-5014079 + 1762314*En^2 + 1447388*la - 76545*m^2)*M^4)*rp[t]^7 + 
      J^8*M*(J^4*(12*En^4*(606 - 2868*la + 379*la^2 + (200 - 260*la)*m^2 + 
            4*m^4) + 20*En^2*(-6480 + 30783*la - 5307*la^2 + 38*la^3 - 
            2*(741 - 1471*la + 18*la^2)*m^2 + 4*(-3 + 2*la)*m^4) - 
          3*(-72390 + 385159*la - 70517*la^2 + 812*la^3 - 
            5*(2172 - 5873*la + 84*la^2)*m^2 + 15*(-3 + 4*la)*m^4)) + 
        6*J^2*(5214298 + 268884*En^4 - 4136232*la + 151518*la^2 + 
          (194615 - 54500*la)*m^2 + 120*m^4 - 2*En^2*(1694888 - 734076*la + 
            59005*m^2))*M^2 + 60230790*M^4)*rp[t]^8 + 
      J^8*(J^4*la*(-20*En^2*(2362 - 935*la + 19*la^2 + (526 - 18*la)*m^2 + 
            4*m^4) + 4*En^4*(634 - 305*la + 7*la^2 - 4*(-52 + 3*la)*m^2 + 
            4*m^4) + 3*(27225 - 9157*la + 203*la^2 - 5*(-778 + 21*la)*m^2 + 
            15*m^4)) + 2*J^2*(-3405003 + 3024*En^6 + 6329690*la - 
          567276*la^2 + 2478*la^3 - 5*(48561 - 41605*la + 210*la^2)*m^2 + 
          120*(-3 + la)*m^4 - 12*En^4*(31724 - 27393*la + 2800*m^2) + 
          6*En^2*(495748 - 688180*la + 41184*la^2 - 5*(-9333 + 4175*la)*m^2 + 
            80*m^4))*M^2 + 30*(-2933476 + 1477740*En^2 + 830443*la - 
          35238*m^2)*M^4)*rp[t]^9 + 
      J^6*M*(J^4*(509220 - 2679487*la + 2016*En^6*la + 461121*la^2 - 
          4956*la^3 + 25*(2628 - 6923*la + 84*la^2)*m^2 - 
          60*(-3 + 4*la)*m^4 + 24*En^4*(2682 - 12208*la + 1523*la^2 + 
            (820 - 990*la)*m^2 + 8*m^4) + 4*En^2*(-125280 + 580193*la - 
            94917*la^2 + 594*la^3 - 25*(1059 - 1979*la + 18*la^2)*m^2 + 
            40*(-3 + 2*la)*m^4)) + 6*J^2*(967074*En^4 - 
          10*En^2*(717453 - 304904*la + 20588*m^2) + 
          5*(1540727 - 1198128*la + 40710*la^2 + (45343 - 12480*la)*m^2 + 
            18*m^4))*M^2 + 57054672*M^4)*rp[t]^10 + 
      J^6*(J^4*la*(8*En^4*(2822 - 1297*la + 23*la^2 + (860 - 30*la)*m^2 + 
            8*m^4) + 3*(63880 - 20187*la + 413*la^2 - 25*(-312 + 7*la)*m^2 + 
            20*m^4) - 4*En^2*(45535 - 17253*la + 297*la^2 - 
            25*(-373 + 9*la)*m^2 + 40*m^4)) + 
        2*J^2*(30240*En^6 - 54*En^4*(27110 - 21861*la + 2240*m^2) + 
          30*En^2*(211113 - 289807*la + 16096*la^2 + (16844 - 7220*la)*m^2 + 
            16*m^4) + 5*(3*(-339430 + 618461*la - 51160*la^2 + 210*la^3) + 
            (-57426 + 48205*la - 210*la^2)*m^2 + 18*(-3 + la)*m^4))*M^2 + 
        12*(-7032557 + 4894710*En^2 + 1940552*la - 60627*m^2)*M^4)*rp[t]^11 + 
      J^4*M*(J^4*(20160*En^6*la + 12*En^4*(24846 - 93700*la + 10985*la^2 + 
            (7080 - 7000*la)*m^2 + 24*m^4) + 20*En^2*(-52776 + 247339*la - 
            37943*la^2 + 206*la^3 - 60*(167 - 295*la + 2*la^2)*m^2 + 
            8*(-3 + 2*la)*m^4) - 5*(-154503 + 797261*la - 125865*la^2 + 
            1260*la^3 - 5*(3168 - 8141*la + 84*la^2)*m^2 + 
            9*(-3 + 4*la)*m^4)) + 6*J^2*(7500834 + 1772376*En^4 - 
          5673320*la + 175122*la^2 + (157809 - 42740*la)*m^2 + 36*m^4 - 
          10*En^2*(971746 - 397436*la + 19921*m^2))*M^2 + 34494660*M^4)*
       rp[t]^12 + J^4*(J^4*la*(15*(19382 - 5575*la + 105*la^2 - 
            5*(-374 + 7*la)*m^2 + 3*m^4) - 20*En^2*(19352 - 7127*la + 
            103*la^2 + (3500 - 60*la)*m^2 + 8*m^4) + 
          4*En^4*(25326 - 10555*la + 125*la^2 - 120*(-61 + la)*m^2 + 
            24*m^4)) + 2*J^2*(-5056443 + 242784*En^6 + 8933830*la - 
          664884*la^2 + 2562*la^3 - 3*(67551 - 55645*la + 210*la^2)*m^2 + 
          36*(-3 + la)*m^4 - 24*En^4*(101260 - 91479*la + 8540*m^2) + 
          30*En^2*(293542 - 387546*la + 18864*la^2 + (16773 - 6935*la)*m^2 + 
            8*m^4))*M^2 + 12*(-4321387 + 4149762*En^2 + 1155233*la - 
          23079*m^2)*M^4)*rp[t]^13 + 
      J^2*M*(J^4*(788436 - 3926145*la + 550119*la^2 - 5124*la^3 + 
          15*(3792 - 9527*la + 84*la^2)*m^2 + (54 - 72*la)*m^4 - 
          4032*En^6*(40 - 43*la + 5*m^2) + 48*En^4*(5646 - 40700*la + 
            5265*la^2 + (3350 - 2930*la)*m^2 + 4*m^4) + 
          20*En^2*(-75528 + 341779*la - 45447*la^2 + 214*la^3 - 
            5*(2073 - 3499*la + 18*la^2)*m^2 + 4*(-3 + 2*la)*m^4)) + 
        6*J^2*(4706018 + 2348514*En^4 - 3434408*la + 94182*la^2 + 
          (60703 - 16200*la)*m^2 + 6*m^4 - 2*En^2*(4296005 - 1644144*la + 
            50482*m^2))*M^2 + 12145248*M^4)*rp[t]^14 + 
      J^2*(J^4*la*(-960*En^6*(53 - 6*la + 7*m^2) + 
          16*En^4*(6196 - 5595*la + 45*la^2 + (3430 - 30*la)*m^2 + 4*m^4) - 
          20*En^2*(27901 - 8803*la + 107*la^2 - 5*(-719 + 9*la)*m^2 + 
            4*m^4) + 3*(98614 - 24653*la + 427*la^2 + (6680 - 105*la)*m^2 + 
            6*m^4)) - 2*J^2*(3262476 + 209088*En^6 - 5527792*la + 
          360084*la^2 - 1302*la^3 + (78936 - 63925*la + 210*la^2)*m^2 - 
          6*(-3 + la)*m^4 + 6*En^4*(591250 - 470829*la + 26600*m^2) - 
          6*En^2*(1379989 - 1674511*la + 66096*la^2 + (43506 - 17470*la)*
             m^2 + 8*m^4))*M^2 + 12*(-1551389 + 2057646*En^2 + 399964*la - 
          3747*m^2)*M^4)*rp[t]^15 + 
      M*(J^4*(529776 + 120960*En^8 - 2503775*la + 300549*la^2 - 2604*la^3 + 
          5*(4500 - 11081*la + 84*la^2)*m^2 + (9 - 12*la)*m^4 - 
          2688*En^6*(-195 + 34*la + 15*m^2) + 12*En^4*(42606 - 233180*la + 
            22045*la^2 - 20*(-556 + 453*la)*m^2 + 4*m^4) + 
          4*En^2*(-394308 + 1576313*la - 162453*la^2 + 666*la^3 - 
            10*(2769 - 4511*la + 18*la^2)*m^2 + 4*(-3 + 2*la)*m^4)) + 
        6*J^2*(2099796*En^4 + 2*(866131 - 606380*la + 14469*la^2) + 
          (9949 - 2620*la)*m^2 - 2*En^2*(2247740 - 792580*la + 10467*m^2))*
         M^2 + 1899045*M^4)*rp[t]^16 + 
      (J^4*la*(40320*En^8 - 1920*En^6*(-92 - 9*la + 7*m^2) + 
          3*(65809 - 13623*la + 217*la^2 - 5*(-526 + 7*la)*m^2 + m^4) - 
          4*En^2*(144526 - 32337*la + 333*la^2 + (9550 - 90*la)*m^2 + 
            4*m^4) + 4*En^4*(48706 - 25175*la + 145*la^2 - 20*(-566 + 3*la)*
             m^2 + 4*m^4)) + 2*J^2*(410832*En^6 + 
          3*(-414007 + 667674*la - 37132*la^2 + 126*la^3) + 
          (-13083 + 10435*la - 30*la^2)*m^2 - 36*En^4*(111812 - 66171*la + 
            1288*m^2) - 6*En^2*(-8*(97577 - 106545*la + 3200*la^2) + 
            7*(-1313 + 515*la)*m^2))*M^2 + 3*(-991126 + 1793256*En^2 + 
          245897*la)*M^4)*rp[t]^17 + 
      M*(J^2*(211680 - 403200*En^8 - 940037*la + 93771*la^2 - 756*la^3 + 
          5*(756 - 1829*la + 12*la^2)*m^2 - 672*En^6*(-120 - 499*la + 
            30*m^2) + 24*En^4*(46830 - 123944*la + 5947*la^2 + 
            (1680 - 1310*la)*m^2) - 20*En^2*(50652 - 173943*la + 12811*la^2 - 
            46*la^3 + (1197 - 1897*la + 6*la^2)*m^2)) + 
        3*(568461 + 1585596*En^4 - 380904*la + 7776*la^2 + 
          4*En^2*(-516263 + 168136*la))*M^2)*rp[t]^18 + 
      (J^2*la*(-134400*En^8 - 960*En^6*(-23 - 18*la + 7*m^2) - 
          8*En^4*(-49974 + 7145*la - 31*la^2 + 6*(-284 + la)*m^2) + 
          20*En^2*(-18261 + 2611*la - 23*la^2 + 3*(-137 + la)*m^2) + 
          3*(26040 - 4297*la + 63*la^2 - 5*(-88 + la)*m^2)) + 
        (-422316 + 1670976*En^6 + 646795*la - 30132*la^2 + 96*la^3 + 
          36*En^4*(-98886 + 47825*la) + 36*En^2*(64201 - 63615*la + 
            1408*la^2))*M^2)*rp[t]^19 + (37800 + 201600*En^8 - 157527*la + 
        12786*la^2 - 96*la^3 + 1344*En^6*(-450 + 433*la) + 
        12*En^4*(53550 - 105356*la + 2591*la^2) + 
        4*En^2*(-69300 + 209813*la - 10737*la^2 + 34*la^3))*M*rp[t]^20 + 
      4*(-1 + En^2)*la*(16800*En^6 + 1440*En^4*(-24 + la) - 
        6*(575 - 74*la + la^2) + En^2*(21150 - 1789*la + 11*la^2))*rp[t]^21))/
    (la*(1 + la)*rp[t]^13*(3*M + la*rp[t])*(J^2 + rp[t]^2)^9))*
  Derivative[1][rp][t]
]


Clear[gSourceZM7]
gSourceZM7[syms_Association]:=
Module[{mu,M,J,la,YBar,YPhiBar,YPhiPhiBar,En,m,rp,t},
	
	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	
	(8*J*mu*Pi*YPhiBar*(-2*M + rp[t])^2*(676190592*J^24*M^9 + 
    576*J^24*(-3045294 + 811549*la)*M^8*rp[t] + 
    36*J^22*M^7*(J^2*(53076207 - 33728269*la + 2256436*la^2) + 224258496*M^2)*
     rp[t]^2 + 6*J^22*M^6*(J^2*(-188225091 + 220816685*la - 35178826*la^2 + 
        14646*la^3) + 12*(-290369835 + 25552101*En^2 + 77911765*la)*M^2)*
     rp[t]^3 + 2*J^20*M^5*(J^4*(194780757 - 392387624*la + 115273437*la^2 - 
        58586*la^3 + 3100*la^4) + 9*J^2*(1262745052 - 807654102*la + 
        54990254*la^2 + 3*En^2*(-71996930 + 22394853*la))*M^2 + 
      22088892672*M^4)*rp[t]^4 + 
    J^20*M^4*(J^4*(-78004488 + 271443225*la - 136817638*la^2 + 23285*la^3 - 
        12916*la^4 + 16*la^5) + 6*J^2*(-2234107729 + 2637026453*la - 
        426962864*la^2 + 772586*la^3 + En^2*(554395548 - 428233134*la + 
          30266937*la^2))*M^2 + 72*(-1586028135 + 291677370*En^2 + 
        428656361*la)*M^4)*rp[t]^5 + 
    J^18*M^3*(J^6*(8334792 - 54540791*la + 47466279*la^2 + 34160*la^3 + 
        10570*la^4 - 32*la^5) + J^4*(4612237260 - 9344803193*la + 
        2785735023*la^2 - 9654812*la^3 + 106960*la^4 - 
        6*En^2*(244440214 - 368836537*la + 65395869*la^2 + 1067308*la^3))*
       M^2 + 18*J^2*(6882953212 + 71154792*En^4 - 4432892990*la + 
        307396902*la^2 + 3*En^2*(-821173448 + 257299133*la))*M^4 + 
      146507961600*M^6)*rp[t]^6 + 
    J^18*M^2*(J^6*(-362880 + 5853000*la - 9579672*la^2 - 21871*la^3 - 
        4231*la^4 + 24*la^5) + J^4*(-920895846 + 3221725934*la - 
        1645018929*la^2 + 8015499*la^3 - 224544*la^4 + 240*la^5 - 
        12*En^2*(-29067249 + 81979974*la - 28751334*la^2 - 1026523*la^3 + 
          4534*la^4))*M^2 + 6*J^2*(-12148898195 + 14434310061*la - 
        2377099950*la^2 + 7731378*la^3 + 162*En^4*(-2134531 + 841419*la) + 
        En^2*(6317221716 - 4910868465*la + 355773579*la^2))*M^4 + 
      216*(-1749867831 + 505884769*En^2 + 476599049*la)*M^6)*rp[t]^7 + 
    J^16*M*(4*J^8*la*(-64050 + 258589*la + 1187*la^2 + 206*la^3 - 2*la^4) - 
      J^6*(-98076186 + 644970197*la - 567575885*la^2 + 3352740*la^3 - 
        185440*la^4 + 480*la^5 + 2*En^2*(20984598 - 118153663*la + 
          78637714*la^2 + 4603170*la^3 - 46240*la^4 + 96*la^5))*M^2 + 
      J^4*(25013203806 - 50993477215*la + 15438714615*la^2 - 101260516*la^3 + 
        791048*la^4 + 72*En^4*(17848109 - 18582303*la + 1557128*la^2) - 
        6*En^2*(2782283138 - 4220960267*la + 764152827*la^2 + 10384982*la^3))*
       M^4 + 18*J^2*(774787536*En^4 + 3*En^2*(-4269220970 + 1348192127*la) + 
        2*(11365772934 - 7374389281*la + 521245225*la^2))*M^6 + 
      327952437120*M^8)*rp[t]^8 + 
    J^16*(J^8*la^2*(-45640 - 343*la - 62*la^2 + la^3) + 
      J^6*(-4254120 + 68930901*la - 113856300*la^2 + 731851*la^3 - 
        75044*la^4 + 360*la^5 + 6*En^2*(327240 - 4795837*la + 6488452*la^2 + 
          552623*la^3 - 9651*la^4 + 48*la^5))*M^2 + 
      J^4*(-4978723170 + 17519274427*la - 9069841612*la^2 + 89278723*la^3 - 
        1667860*la^4 + 1632*la^5 - 12*En^4*(31332858 - 69906464*la + 
          15900953*la^2 + 577811*la^3) - 6*En^2*(-660881256 + 1872111453*la - 
          667699268*la^2 - 20212027*la^3 + 99180*la^4))*M^4 + 
      6*J^2*(-40020936643 + 50601456*En^6 + 47886815019*la - 
        8028438932*la^2 + 38130046*la^3 + 18*En^4*(-209423661 + 
          83006702*la) + 3*En^2*(10936944862 - 8560910676*la + 
          636486313*la^2))*M^6 + 1512*(-558419117 + 226352058*En^2 + 
        153350843*la)*M^8)*rp[t]^9 + 
    J^14*M*(-3*J^8*la*(1001185 - 4070978*la + 25692*la^2 - 4937*la^3 + 
        40*la^4 + 2*En^2*(-227505 + 818248*la + 94804*la^2 - 2625*la^3 + 
          24*la^4)) - 2*J^6*(-264171762 + 1746692183*la - 1555607183*la^2 + 
        20216151*la^3 - 692161*la^4 + 1632*la^5 + 
        2*En^4*(-12764034 + 62286903*la - 31369611*la^2 - 2457964*la^3 + 
          8658*la^4) + En^2*(238234356 - 1345845803*la + 907172554*la^2 + 
          45867658*la^3 - 508344*la^4 + 1176*la^5))*M^2 + 
      J^4*(82157328336 - 168619410783*la + 51897689133*la^2 - 
        508634836*la^3 + 3402032*la^4 + 144*En^6*(-2360556 + 1313711*la) + 
        72*En^4*(194811699 - 203607643*la + 17511678*la^2) - 
        6*En^2*(14434391662 - 22027228839*la + 4076998335*la^2 + 
          43786200*la^3))*M^4 + 18*J^2*(50658268122 + 3822335928*En^4 - 
        33130939508*la + 2389136050*la^2 + 3*En^2*(-13360496332 + 
          4254926823*la))*M^6 + 522019137024*M^8)*rp[t]^10 + 
    J^14*(J^8*la^2*(-535150 + 2982*la - 1133*la^2 + 15*la^3 + 
        2*En^2*(121100 + 18249*la - 779*la^2 + 12*la^3)) + 
      J^6*(-22823100 + 371684706*la - 620078019*la^2 + 9838784*la^3 - 
        563503*la^4 + 2448*la^5 - 4*En^4*(624780 - 8609196*la + 
          9852954*la^2 + 1244537*la^3 - 10057*la^4 + 8*la^5) + 
        En^2*(22260420 - 326966016*la + 446200456*la^2 + 33454346*la^3 - 
          639986*la^4 + 3528*la^5))*M^2 + 
      J^4*(-16297451352 + 57713169866*la - 30324625601*la^2 + 
        458317653*la^3 - 7193512*la^4 + 6656*la^5 + 
        72*En^6*(1823468 - 2991585*la + 333033*la^2) - 
        12*En^4*(342475389 - 765731075*la + 177514653*la^2 + 5707778*la^3) - 
        6*En^2*(-3424175070 + 9746435839*la - 3539524819*la^2 - 
          86731764*la^3 + 479160*la^4))*M^4 + 
      6*J^2*(533206224*En^6 + 18*En^4*(-1034639002 + 412513151*la) + 
        3*En^2*(34193076462 - 26965666719*la + 2060290379*la^2) + 
        4*(-22235896263 + 26810692914*la - 4580567512*la^2 + 28879811*la^3))*
       M^6 + 432*(-3104165437 + 1660550595*En^2 + 860017731*la)*M^8)*
     rp[t]^11 + J^12*M*(J^8*la*(-16114425 + 66040178*la - 1207808*la^2 + 
        111989*la^3 - 816*la^4 + 4*En^4*(-429705 + 1450980*la + 262002*la^2 - 
          3742*la^3 + 8*la^4) - 2*En^2*(-7737840 + 27964514*la + 
          2907292*la^2 - 87505*la^3 + 882*la^4)) - 
      2*J^6*(-861339006 + 5729313752*la - 5169215716*la^2 + 106528428*la^3 - 
        2995082*la^4 + 6656*la^5 + 12*En^6*(835659 - 3540101*la + 
          1238802*la^2 + 90826*la^3) + 2*En^4*(-139745304 + 682206003*la - 
          347752539*la^2 - 24562832*la^3 + 103392*la^4) + 
        En^2*(1232586108 - 6988664485*la + 4776341258*la^2 + 200329778*la^3 - 
          2475696*la^4 + 6432*la^5))*M^2 + 
      J^4*(182005314624 + 18511200*En^8 - 376296411057*la + 
        117865338207*la^2 - 1556571584*la^3 + 9603568*la^4 + 
        144*En^6*(-25019628 + 13925221*la) + 
        144*En^4*(481960669 - 505832613*la + 44723543*la^2) - 
        18*En^2*(15024712858 - 23075865939*la + 4373088107*la^2 + 
          33919370*la^3))*M^4 + 108*J^2*(13375273876 + 1877427072*En^4 - 
        8823072298*la + 649748194*la^2 + En^2*(-13990548738 + 4496616889*la))*
       M^6 + 605866770432*M^8)*rp[t]^12 + 
    2*J^12*(J^8*la^2*(-1435840 + 28950*la - 4319*la^2 + 51*la^3 - 
        2*En^4*(77490 + 18774*la - 439*la^2 + 2*la^3) + 
        7*En^2*(196090 + 27021*la - 1243*la^2 + 21*la^3)) + 
      J^6*(-37037520 + 606582756*la - 1023087882*la^2 + 26753011*la^3 - 
        1223759*la^4 + 4992*la^5 + 12*En^6*(39915 - 554241*la + 540295*la^2 + 
          84166*la^3 + 93*la^4) - 2*En^4*(6854895 - 94331154*la + 
          108564730*la^2 + 12591800*la^3 - 121054*la^4 + 188*la^5) + 
        En^2*(57503250 - 846692469*la + 1166526469*la^2 + 74373911*la^3 - 
          1571618*la^4 + 9648*la^5))*M^2 + 
      J^4*(-17984641824 + 64135267707*la - 34241709072*la^2 + 
        709464001*la^3 - 10175924*la^4 + 9072*la^5 + 
        216*En^8*(-25835 + 26132*la) + 36*En^6*(19454637 - 31829509*la + 
          3616913*la^2) - 6*En^4*(1697430687 - 3804134563*la + 
          900232784*la^2 + 24944111*la^3) + 
        En^2*(32034020238 - 91662798060*la + 33945184365*la^2 + 
          623200341*la^3 - 3990696*la^4))*M^4 + 
      18*J^2*(-23413527295 + 421713744*En^6 + 28467044339*la - 
        4961731704*la^2 + 39315478*la^3 + 60*En^4*(-152729271 + 
          61278118*la) + En^2*(35768850552 - 28440516638*la + 
          2236593831*la^2))*M^6 + 1512*(-513460551 + 349664538*En^2 + 
        143615209*la)*M^8)*rp[t]^13 + 
    2*J^10*M*(J^8*la*(-26151840 + 108115017*la - 3410061*la^2 + 244258*la^3 - 
        1664*la^4 + En^2*(39976695 - 145253770*la - 13164686*la^2 + 
          433586*la^3 - 4824*la^4) + 4*En^6*(81345 - 284186*la - 70761*la^2 - 
          317*la^3 + 8*la^4) + En^4*(-9429120 + 31837108*la + 5373226*la^2 - 
          90674*la^3 + 376*la^4)) + J^6*(1892680344 - 12673648083*la + 
        11597846544*la^2 - 334247707*la^3 + 8495018*la^4 - 18144*la^5 + 
        720*En^8*(1235 - 4869*la + 937*la^2) - 
        12*En^6*(8983095 - 37844071*la + 13351546*la^2 + 896112*la^3) - 
        4*En^4*(-347015205 + 1694676123*la - 875527605*la^2 - 54463148*la^3 + 
          271743*la^4) - En^2*(3837865752 - 21848619261*la + 
          15160410948*la^2 + 493986272*la^3 - 6959032*la^4 + 20736*la^5))*
       M^2 + J^4*(143228183652 + 99023904*En^8 - 298523354397*la + 
        95276632803*la^2 - 1599927020*la^3 + 9447376*la^4 + 
        288*En^6*(-29873172 + 16636175*la) + 
        144*En^4*(712832170 - 751488960*la + 68411559*la^2) - 
        18*En^2*(15697601770 - 24281030787*la + 4719488283*la^2 + 
          21817220*la^3))*M^4 + 126*J^2*(6618789804 + 1570981464*En^4 - 
        4407068258*la + 331790482*la^2 + 3*En^2*(-2943588384 + 955630439*la))*
       M^6 + 258304806144*M^8)*rp[t]^14 + 
    2*J^10*(2*J^8*la^2*(-2330690 + 85239*la - 9467*la^2 + 104*la^3 + 
        En^6*(31360 + 11354*la + 146*la^2 - 8*la^3) - 
        En^4*(848715 + 195384*la - 5329*la^2 + 47*la^3) + 
        En^2*(3544945 + 436419*la - 21754*la^2 + 402*la^3)) + 
      J^6*(-80965980 + 1334478924*la - 2278205718*la^2 + 85244939*la^3 - 
        3480871*la^4 + 13608*la^5 - 24*En^8*(1170 - 24120*la + 21884*la^2 + 
          3869*la^3) - 12*En^6*(-432675 + 5962119*la - 5795642*la^2 - 
          843449*la^3 + 363*la^4) - 2*En^4*(34142040 - 468925530*la + 
          543162079*la^2 + 56713416*la^3 - 642797*la^4 + 1384*la^5) + 
        6*En^2*(29793960 - 439903722*la + 612536892*la^2 + 31444202*la^3 - 
          746095*la^4 + 5184*la^5))*M^2 + J^4*(-28188136314 - 11664*En^10 + 
        101308111278*la - 55032872241*la^2 + 1469481959*la^3 - 
        20060752*la^4 + 17424*la^5 + 864*En^8*(-70419 + 70094*la) + 
        72*En^6*(46805397 - 76353680*la + 8887204*la^2) - 
        12*En^4*(2515674975 - 5652680452*la + 1367640640*la^2 + 
          31450596*la^3) - 12*En^2*(-5570056602 + 16031803689*la - 
          6064731269*la^2 - 71443013*la^3 + 572354*la^4))*M^4 + 
      6*J^2*(3561691536*En^6 + 18*En^4*(-2988902981 + 1207152939*la) + 
        21*En^2*(7517526048 - 6032021205*la + 489146563*la^2) + 
        7*(-11548257765 + 14169800643*la - 2522761354*la^2 + 24246254*la^3))*
       M^6 + 216*(-3056877383 + 2591825985*En^2 + 863848569*la)*M^8)*
     rp[t]^15 + 2*J^8*M*(J^8*la*(-57172950 + 238689798*la - 11056424*la^2 + 
        697012*la^3 - 4536*la^4 + 24*En^8*(-780 + 4781*la + 1807*la^2 + 
          56*la^3) + 4*En^6*(882015 - 3046433*la - 720598*la^2 - 539*la^3 + 
          54*la^4) + 2*En^4*(-23480640 + 79263917*la + 12306736*la^2 - 
          242949*la^3 + 1384*la^4) - 3*En^2*(-41426985 + 151440433*la + 
          11445487*la^2 - 417548*la^3 + 5184*la^4)) - 
      J^6*(-2951811558 + 19914916443*la + 11664*En^10*la - 18510577995*la^2 + 
        698234108*la^3 - 16784632*la^4 + 34848*la^5 - 
        144*En^8*(68705 - 265797*la + 50791*la^2) + 
        12*En^6*(43704525 - 182446082*la + 65053232*la^2 + 3910864*la^3) + 
        4*En^4*(-1030454865 + 5038061337*la - 2641871325*la^2 - 
          140057356*la^3 + 830580*la^4) + En^2*(7995432942 - 45719182383*la + 
          32259954636*la^2 + 721284748*la^3 - 12218240*la^4 + 43776*la^5))*
       M^2 + J^4*(472817952*En^8 + 288*En^6*(-84652590 + 47254309*la) + 
        72*En^4*(2797964077 - 2962070159*la + 278091914*la^2) - 
        42*En^2*(9883753926 - 15411329277*la + 3078737969*la^2 + 
          4059314*la^3) + 7*(23456236986 - 49328177757*la + 
          16064534037*la^2 - 330560572*la^3 + 1907192*la^4))*M^4 + 
      18*J^2*(39285388956 + 14889465360*En^4 - 26425777106*la + 
        2036333362*la^2 + 21*En^2*(-3114579262 + 1022369961*la))*M^6 + 
      160595265600*M^8)*rp[t]^16 + 
    2*J^8*(J^8*la^2*(-10193100 + 562907*la - 54226*la^2 + 567*la^3 + 
        4*En^6*(168560 + 58709*la + 372*la^2 - 27*la^3) + 
        8*En^8*(-700 - 478*la - 47*la^2 + la^3) - 
        2*En^4*(4219605 + 912658*la - 28711*la^2 + 346*la^3) + 
        En^2*(22035840 + 2340013*la - 127640*la^2 + 2592*la^3)) - 
      J^6*(125530020 - 2084097411*la + 3606428784*la^2 + 3888*En^10*la^2 - 
        179787025*la^3 + 6895040*la^4 - 26136*la^5 + 
        24*En^8*(12870 - 268830*la + 236983*la^2 + 39412*la^3) + 
        12*En^6*(-2168505 + 28987974*la - 28044120*la^2 - 3758032*la^3 + 
          6832*la^4) + 2*En^4*(101382300 - 1394562540*la + 1628518379*la^2 + 
          148736864*la^3 - 1998932*la^4 + 5216*la^5) - 
        3*En^2*(123965100 - 1835348061*la + 2585959484*la^2 + 96917910*la^3 - 
          2675234*la^4 + 21888*la^5))*M^2 + 
      J^4*(-128304*En^10 + 25920*En^8*(-11587 + 11150*la) + 
        72*En^6*(133003449 - 217964528*la + 26113776*la^2) - 
        12*En^4*(4956240207 - 11149449536*la + 2760765297*la^2 + 
          49747739*la^3) - 42*En^2*(-2334128976 + 6763627761*la - 
          2619532766*la^2 - 12280429*la^3 + 170748*la^4) + 
        7*(-4594696290 + 16658646213*la - 9221825928*la^2 + 305325685*la^3 - 
          4057724*la^4 + 3456*la^5))*M^4 + 
      6*J^2*(-68295402699 + 6605453088*En^6 + 84649458123*la - 
        15415775356*la^2 + 175299566*la^3 + 18*En^4*(-4056046343 + 
          1651257602*la) + 21*En^2*(7945167990 - 6440644824*la + 
          539640551*la^2))*M^6 + 216*(-1895271203 + 1974768102*En^2 + 
        541602137*la)*M^8)*rp[t]^17 + 
    2*J^6*M*(J^8*la*(-88648965 + 374256798*la - 23564968*la^2 + 
        1384637*la^3 - 8712*la^4 - 1296*En^10*la*(1 + la) + 
        48*En^8*(-4290 + 25981*la + 9263*la^2 + 217*la^3) + 
        4*En^6*(4410795 - 14725222*la - 3303604*la^2 + 7012*la^3 + 
          112*la^4) + 4*En^4*(-34881975 + 118191440*la + 16447241*la^2 - 
          385421*la^3 + 2608*la^4) - 3*En^2*(-86182785 + 317119971*la + 
          18540759*la^2 - 765382*la^3 + 10944*la^4)) - 
      J^6*(128304*En^10*la - 144*En^8*(394363 - 1293281*la + 241821*la^2) + 
        12*En^6*(122382627 - 521776258*la + 190239864*la^2 + 9818480*la^3) + 
        4*En^4*(-2042078715 + 9955266660*la - 5296274250*la^2 - 
          228618752*la^3 + 1612230*la^4) + 14*(-239211630 + 1627823970*la - 
          1539397356*la^2 + 72997433*la^3 - 1701307*la^4 + 3456*la^5) + 
        7*En^2*(1672110108 - 9611349177*la + 6911677644*la^2 + 
          76480076*la^3 - 1891984*la^4 + 9072*la^5))*M^2 + 
      J^4*(138110453064 + 1281297312*En^8 - 293369243439*la + 
        97646008389*la^2 - 2400049844*la^3 + 13698544*la^4 + 
        144*En^6*(-319406892 + 176629093*la) + 
        72*En^4*(3807118883 - 4052286631*la + 393760428*la^2) + 
        126*En^2*(-3476423462 + 5471091127*la - 1126089315*la^2 + 
          2444648*la^3))*M^4 + 54*J^2*(4738661256*En^4 + 
        En^2*(-16602530580 + 5515750631*la) + 27*(299702489 - 203866001*la + 
          16100550*la^2))*M^6 + 71000012160*M^8)*rp[t]^18 + 
    2*J^6*(J^8*la^2*(-15809190 + 1212610*la - 108071*la^2 + 1089*la^3 + 
        8*En^8*(-7310 - 4751*la - 403*la^2 + 8*la^3) - 
        8*En^6*(-417245 - 142093*la - 445*la^2 + 28*la^3) - 
        2*En^4*(12519270 + 2478823*la - 93333*la^2 + 1304*la^3) + 
        En^2*(45827340 + 3973226*la - 239662*la^2 + 5472*la^3)) - 
      J^6*(42768*En^10*la^2 + 24*En^8*(189090 - 1484790*la + 1088965*la^2 + 
          183156*la^3) + 4*En^6*(-16479045 + 245156274*la - 244443972*la^2 - 
          28583464*la^3 + 109016*la^4) - 42*En^2*(12941595 - 192215334*la + 
          274561488*la^2 + 5980963*la^3 - 214991*la^4 + 2268*la^5) - 
        7*(-20205720 + 338325930*la - 594489759*la^2 + 37856210*la^3 - 
          1401325*la^4 + 5184*la^5) + En^4*(405881550 - 5533772754*la + 
          6492030698*la^2 + 501294940*la^3 - 7943148*la^4 + 23744*la^5))*
       M^2 + J^4*(-26908844796 + 1901232*En^10 + 98539498290*la - 
        55691122245*la^2 + 2227289237*la^3 - 29204536*la^4 + 24480*la^5 + 
        864*En^8*(-800953 + 936330*la) + 72*En^6*(258134529 - 413341165*la + 
          50237537*la^2) - 12*En^4*(6769325904 - 15256910227*la + 
          3879781833*la^2 + 48894798*la^3) - 42*En^2*(-2457646272 + 
          7179789831*la - 2854924211*la^2 + 8561624*la^3 + 84848*la^4))*M^4 + 
      9*J^2*(-28021067081 + 5509931712*En^6 + 35123263823*la - 
        6553055294*la^2 + 86561882*la^3 + 12*En^4*(-3879985628 + 
          1596352211*la) + En^2*(84618518292 - 69379776326*la + 
          6017789958*la^2))*M^6 + 36*(-5012246007 + 6379708473*En^2 + 
        1449868553*la)*M^8)*rp[t]^19 + 
    2*J^4*M*(-(J^8*la*(14256*En^10*la*(1 + la) - 
         48*En^8*(-61380 + 126319*la + 49611*la^2 + 975*la^3) + 
         4*En^6*(-11272605 + 41936198*la + 8322856*la^2 - 60124*la^3 + 
           48*la^4) + 7*(14270985 - 61034466*la + 4999760*la^2 - 
           282229*la^3 + 1728*la^4) + 21*En^2*(-17993655 + 66720447*la + 
           2576849*la^2 - 127734*la^3 + 2268*la^4) - 
         4*En^4*(-69817545 + 234989833*la + 28624629*la^2 - 788105*la^3 + 
           5936*la^4))) + J^6*(432*En^10*(-19364 + 21*la) + 
        144*En^8*(127505 - 3146913*la + 765427*la^2) - 
        12*En^6*(250932423 - 1015798256*la + 366977796*la^2 + 
          16143892*la^3) + En^4*(11231528148 - 54555589440*la + 
          29522456736*la^2 + 952075888*la^3 - 8104080*la^4) + 
        2*(1391690700 - 9565597959*la + 9222546711*la^2 - 535308760*la^3 + 
          12273422*la^4 - 24480*la^5) - 7*En^2*(1755795042 - 10161194421*la + 
          7469845716*la^2 - 11593148*la^3 - 1055456*la^4 + 9216*la^5))*M^2 + 
      J^4*(84575756493 + 2882203776*En^8 - 181698994329*la + 
        61924587204*la^2 - 1784513818*la^3 + 10152716*la^4 + 
        144*En^6*(-404293608 + 222572663*la) + 
        144*En^4*(1823506961 - 1958162377*la + 198122912*la^2) + 
        18*En^2*(-18481072522 + 29396640239*la - 6249692155*la^2 + 
          36476150*la^3))*M^4 + 9*J^2*(19060921728*En^4 + 
        3*En^2*(-17880037242 + 6016059833*la) + 
        2*(10660604886 - 7341719879*la + 595096275*la^2))*M^6 + 
      21187305216*M^8)*rp[t]^20 + 
    J^4*(2*J^8*la^2*(8*En^6*(1078015 + 337721*la - 2237*la^2 + 12*la^3) + 
        8*En^8*(-76210 - 41213*la - 2509*la^2 + 24*la^3) + 
        14*(-1272930 + 129631*la - 11051*la^2 + 108*la^3) + 
        7*En^2*(9564450 + 609799*la - 41582*la^2 + 1134*la^3) - 
        2*En^4*(24984210 + 4451298*la - 198269*la^2 + 2968*la^3)) - 
      2*J^6*(432*En^10*(-10620 + 10496*la + 1373*la^2) + 
        24*En^8*(-1005030 - 676038*la + 3110423*la^2 + 379980*la^3) + 
        4*En^6*(-38686005 + 503449956*la - 478969218*la^2 - 48585692*la^3 + 
          296458*la^4) + 2*(58293270 - 986029371*la + 1763293848*la^2 - 
          139619390*la^3 + 5068321*la^4 - 18360*la^5) - 
        21*En^2*(27096930 - 404471817*la + 587552766*la^2 + 2134550*la^3 - 
          264952*la^4 + 4608*la^5) + 2*En^4*(283265505 - 3806994090*la + 
          4488104387*la^2 + 276412896*la^3 - 5130064*la^4 + 17528*la^5))*
       M^2 + J^4*(-32752325952 - 185978592*En^10 + 121326523941*la - 
        70157167326*la^2 + 3326356553*la^3 - 43384612*la^4 + 35856*la^5 + 
        864*En^8*(-5173823 + 4235252*la) + 432*En^6*(111220918 - 
          174801395*la + 21664663*la^2) - 24*En^4*(6489945912 - 
          14725953215*la + 3874387744*la^2 + 24904739*la^3) + 
        12*En^2*(13030492770 - 38445823422*la + 15745025537*la^2 - 
          178170733*la^3 + 182800*la^4))*M^4 + 
      6*J^2*(-36747806277 + 14085755424*En^6 + 46645136681*la - 
        8931393248*la^2 + 135116594*la^3 + 72*En^4*(-1304519443 + 
          543501654*la) + 3*En^2*(45544336332 - 37801358394*la + 
          3398624719*la^2))*M^6 + 72*(-1490760195 + 2318177730*En^2 + 
        437009869*la)*M^8)*rp[t]^21 + 
    J^2*M*(-2*J^8*la*(82358010 - 357671010*la + 37116694*la^2 - 
        2047982*la^3 + 12240*la^4 + 144*En^10*(-20940 + 2701*la + 
          1393*la^2) - 48*En^8*(329670 + 141405*la + 72887*la^2 + 
          1861*la^3) + 4*En^6*(-26402025 + 85739398*la + 14553634*la^2 - 
          199890*la^3 + 624*la^4) + 21*En^2*(-18837735 + 70592813*la + 
          1045037*la^2 - 85936*la^3 + 2304*la^4) - 
        8*En^4*(-48682620 + 161850706*la + 16766177*la^2 - 526311*la^3 + 
          4382*la^4)) + J^6*(3361199112 + 4147200*En^12 - 23378133279*la + 
        23037565707*la^2 - 1606655756*la^3 + 36557314*la^4 - 71712*la^5 - 
        288*En^10*(-892308 + 386251*la) + 288*En^8*(3831121 - 10188491*la + 
          1770891*la^2) - 24*En^6*(340544229 - 1309505116*la + 
          469322012*la^2 + 17727216*la^3) - 8*En^4*(-2693593467 + 
          13133898459*la - 7293701997*la^2 - 139772876*la^3 + 1599546*la^4) - 
        2*En^2*(9272367648 - 54152991555*la + 40851301128*la^2 - 
          641851072*la^3 + 766544*la^4 + 45792*la^5))*M^2 + 
      J^4*(73521371724 + 6434451648*En^8 - 160005856957*la + 
        55954603707*la^2 - 1863860684*la^3 + 10618768*la^4 + 
        576*En^6*(-171389052 + 96065879*la) + 
        288*En^4*(1228244572 - 1334826094*la + 141431889*la^2) + 
        6*En^2*(-59606815854 + 95932283397*la - 21108154001*la^2 + 
          203178340*la^3))*M^4 + 18*J^2*(6315406492 + 8694950472*En^4 - 
        4409265350*la + 367382142*la^2 + 9*En^2*(-2168470472 + 738901691*la))*
       M^6 + 7663493376*M^8)*rp[t]^22 + 
    J^2*(4*J^8*la^2*(-7349160 + 968744*la - 80491*la^2 + 765*la^3 + 
        1920*En^10*(137 + 33*la + la^2) + 4*En^8*(286150 - 5495*la - 
          6043*la^2 + 32*la^3) + 4*En^6*(2442335 + 589494*la - 14030*la^2 + 
          156*la^3) + 7*En^2*(5005155 + 176497*la - 15152*la^2 + 576*la^3) - 
        En^4*(34712385 + 5582117*la - 276611*la^2 + 4382*la^3)) + 
      J^6*(-139366440 + 2386940112*la - 4356645564*la^2 + 421335097*la^3 - 
        15140843*la^4 + 53784*la^5 + 207360*En^12*(-21 + 13*la) - 
        288*En^10*(282060 - 571512*la + 43621*la^2) - 
        48*En^8*(1769670 - 15668394*la + 9883021*la^2 + 704670*la^3) - 
        8*En^6*(-60363495 + 678678228*la - 611622120*la^2 - 57558478*la^3 + 
          453026*la^4) + 12*En^2*(71146350 - 1070926929*la + 
          1589068144*la^2 - 28093334*la^3 - 49188*la^4 + 11448*la^5) - 
        4*En^4*(272522070 - 3656740839*la + 4378705075*la^2 + 
          183417580*la^3 - 4184374*la^4 + 17248*la^5))*M^2 + 
      J^4*(-14131750998 + 557949600*En^10 + 53058656214*la - 
        31472043469*la^2 + 1744393527*la^3 - 22741248*la^4 + 18544*la^5 + 
        12096*En^8*(-433343 + 325686*la) + 144*En^6*(278257191 - 
          447742932*la + 58697344*la^2) + 24*En^4*(-4366769001 + 
          10029335944*la - 2752575892*la^2 + 1768012*la^3) + 
        12*En^2*(6986441763 - 20848768620*la + 8818470100*la^2 - 
          177972349*la^3 + 506154*la^4))*M^4 + 
      6*J^2*(-10828561879 + 8364621024*En^6 + 13941610649*la - 
        2744899398*la^2 + 47020522*la^3 + 90*En^4*(-479882343 + 
          202053019*la) + En^2*(49758413052 - 41804905053*la + 
          3895923111*la^2))*M^6 + 504*(-38374551 + 73311585*En^2 + 
        11415209*la)*M^8)*rp[t]^23 + 
    (2*J^8*la*(-49238190 + 207360*En^12*(-7 + la) + 217842642*la - 
        28173386*la^2 + 1534858*la^3 - 8964*la^4 + 
        48*En^10*(-560580 + 251771*la + 11135*la^2) + 
        48*En^8*(-602220 + 1448352*la + 138210*la^2 + 835*la^3) - 
        4*En^6*(-40879455 + 113099516*la + 19074576*la^2 - 340798*la^3 + 
          1252*la^4) + 4*En^4*(-93626505 + 311836372*la + 24956419*la^2 - 
          891376*la^3 + 8624*la^4) - 3*En^2*(-98944515 + 376634395*la - 
          5558683*la^2 - 107212*la^3 + 11448*la^4))*M - 
      J^6*(-1436424138 + 60825600*En^12 + 10134777341*la - 10239311013*la^2 + 
        846573764*la^3 - 19214736*la^4 + 37088*la^5 - 
        288*En^10*(-1809816 + 1329205*la) - 288*En^8*(5409659 - 11349439*la + 
          1650549*la^2) + 24*En^6*(273559215 - 1091558486*la + 
          410117856*la^2 + 11512192*la^3) + 8*En^4*(-1800598275 + 
          8906231583*la - 5134770675*la^2 - 15044548*la^3 + 706908*la^4) + 
        4*En^2*(2473996446 - 14613430983*la + 11354146689*la^2 - 
          356756839*la^3 + 2294864*la^4 + 11088*la^5))*M^3 + 
      J^4*(21520873590 + 3631020480*En^8 - 47539876571*la + 
        17099876307*la^2 - 651035668*la^3 + 3723752*la^4 + 
        576*En^6*(-101976162 + 58517005*la) + 
        72*En^4*(2276384225 - 2497761755*la + 277086116*la^2) + 
        6*En^2*(-21712879690 + 35357102767*la - 8056041543*la^2 + 
          108178130*la^3))*M^5 + 18*J^2*(1132745404 + 2468005776*En^4 - 
        803028410*la + 68890794*la^2 + 3*En^2*(-1444899506 + 497992243*la))*
       M^7 + 635209344*M^9)*rp[t]^24 + 
    (J^8*la^2*(-17584560 + 2960881*la - 242318*la^2 + 2241*la^3 - 
        23040*En^12*(22 + la) + 26880*En^10*(-330 - 20*la + la^2) + 
        80*En^8*(-132230 + 3919*la - 991*la^2 + 2*la^3) + 
        16*En^6*(3678625 + 911836*la - 29441*la^2 + 313*la^3) - 
        4*En^4*(33305565 + 4689704*la - 245169*la^2 + 4312*la^3) + 
        2*En^2*(52578540 - 133217*la - 59768*la^2 + 5724*la^3)) + 
      J^6*(-58797360 + 1022989773*la - 1912822836*la^2 + 223226203*la^3 - 
        7983692*la^4 + 27816*la^5 - 69120*En^12*(-567 + 586*la) + 
        288*En^10*(410400 - 1238544*la + 239935*la^2) - 
        48*En^8*(4051350 - 20990946*la + 9297853*la^2 + 1097004*la^3) - 
        8*En^6*(-45308025 + 541277622*la - 508411056*la^2 - 41942512*la^3 + 
          394904*la^4) + 6*En^2*(75300120 - 1147334574*la + 1747291544*la^2 - 
          74437141*la^3 + 824205*la^4 + 11088*la^5) - 
        4*En^4*(178984890 - 2454177360*la + 3035933729*la^2 + 44285056*la^3 - 
          1940652*la^4 + 11264*la^5))*M^2 + 
      J^4*(-4100436690 + 22896864*En^10 + 15643207631*la - 9546778644*la^2 + 
        611898983*la^3 - 7994788*la^4 + 6432*la^5 + 
        6912*En^8*(-249617 + 311725*la) + 144*En^6*(162459447 - 
          272410100*la + 38785924*la^2) + 12*En^4*(-4070000040 + 
          9436634584*la - 2704853725*la^2 + 22032713*la^3) + 
        6*En^2*(5084739528 - 15348010461*la + 6712801856*la^2 - 
          196906353*la^3 + 705044*la^4))*M^4 + 
      6*J^2*(-1930737431 + 3375183312*En^6 + 2526235599*la - 512515092*la^2 + 
        9848070*la^3 + 18*En^4*(-693156397 + 292423358*la) + 
        3*En^2*(3696590526 - 3138888492*la + 302606213*la^2))*M^6 + 
      216*(-7391871 + 17529310*En^2 + 2234601*la)*M^8)*rp[t]^25 + 
    (J^6*la*(-41563275 + 188175646*la - 30042348*la^2 + 1624899*la^3 - 
        9272*la^4 - 46080*En^12*(-567 + 146*la) + 
        96*En^10*(824640 - 666097*la + 10775*la^2) - 
        96*En^8*(1343610 - 1684711*la - 331461*la^2 + 1797*la^3) - 
        8*En^6*(-30595305 + 88358698*la + 16002508*la^2 - 317796*la^3 + 
          1296*la^4) - 6*En^2*(-52389810 + 203832593*la - 10569943*la^2 + 
          182927*la^3 + 5544*la^4) + 8*En^4*(-61564095 + 211296836*la + 
          8812263*la^2 - 434587*la^3 + 5632*la^4))*M + 
      2*J^4*(205925562 + 80179200*En^12 - 1478771785*la + 1537585693*la^2 - 
        149212363*la^3 + 3387397*la^4 - 6432*la^5 - 
        144*En^10*(447576 + 27385*la) + 144*En^8*(-149039 - 3192963*la + 
          767087*la^2) - 12*En^6*(147349185 - 648160822*la + 267472952*la^2 + 
          2641744*la^3) - 2*En^4*(-1678449600 + 8403774129*la - 
          5044764945*la^2 + 79064108*la^3 + 199650*la^4) - 
        2*En^2*(897210936 - 5361692489*la + 4299128153*la^2 - 
          207498551*la^3 + 1711700*la^4 + 3468*la^5))*M^3 + 
      J^2*(3807217344 + 2638913472*En^8 - 8557503955*la + 3174681945*la^2 - 
        136878596*la^3 + 786928*la^4 + 144*En^6*(-173545980 + 97504751*la) + 
        72*En^4*(671570799 - 735169183*la + 84474058*la^2) + 
        6*En^2*(-4855753430 + 7984074371*la - 1880261427*la^2 + 
          32115112*la^3))*M^5 + 18*(93026550 + 333058968*En^4 - 67086672*la + 
        5936122*la^2 + 3*En^2*(-148982060 + 51793851*la))*M^7)*rp[t]^26 + 
    (J^6*la^2*(-7427350 + 1590214*la - 128877*la^2 + 1159*la^3 + 
        7680*En^12*(569 + 2*la) + 3840*En^10*(3413 - 254*la + 8*la^2) - 
        16*En^8*(1382630 + 234733*la - 1903*la^2 + 24*la^3) + 
        16*En^6*(2764295 + 938987*la - 30429*la^2 + 324*la^3) + 
        14*En^2*(3978420 - 217801*la + 5735*la^2 + 396*la^3) - 
        4*En^4*(21912780 + 2154273*la - 126151*la^2 + 2816*la^3)) + 
      J^4*(-16577460 + 294377922*la - 566643171*la^2 + 79144796*la^3 - 
        2824699*la^4 + 9648*la^5 + 345600*En^12*(-231 + 310*la) - 
        288*En^10*(-221520 + 268624*la + 41371*la^2) - 
        48*En^8*(-1133910 + 1662126*la + 1034679*la^2 + 834772*la^3) - 
        8*En^6*(-17999415 + 294201342*la - 316830540*la^2 - 12577856*la^3 + 
          185776*la^4) + 6*En^2*(27083700 - 418224016*la + 655600324*la^2 - 
          46400745*la^3 + 675421*la^4 + 3468*la^5) - 
        4*En^4*(82077975 - 1153138515*la + 1481679469*la^2 - 30434271*la^3 - 
          327377*la^4 + 4696*la^5))*M^2 + J^2*(-717694464 - 476351712*En^10 + 
        2791115698*la - 1758774613*la^2 + 129222641*la^3 - 1693992*la^4 + 
        1344*la^5 + 1728*En^8*(-884011 + 1011854*la) + 
        72*En^6*(148243098 - 239339261*la + 35418637*la^2) + 
        12*En^4*(-1231622739 + 2832206527*la - 836990249*la^2 + 
          12541902*la^3) + 6*En^2*(1140592146 - 3472697751*la + 
          1567801051*la^2 - 60060660*la^3 + 236536*la^4))*M^4 + 
      6*(-157477078 + 701723376*En^6 + 209886510*la - 43978012*la^2 + 
        940304*la^3 + 18*En^4*(-96297222 + 40152923*la) + 
        3*En^2*(383885518 - 328298331*la + 32597151*la^2))*M^6)*rp[t]^27 + 
    (-(J^4*la*(11725905 - 54671490*la + 10725184*la^2 - 577325*la^3 + 
         3216*la^4 - 691200*En^12*(-77 + 26*la) + 
         96*En^10*(-437760 + 102859*la + 26467*la^2) + 
         96*En^8*(-394320 + 311647*la - 319053*la^2 + 2643*la^3) + 
         8*En^6*(-12299055 + 48990242*la + 5999904*la^2 - 156572*la^3 + 
           768*la^4) + 6*En^2*(-18858435 + 75309821*la - 7310911*la^2 + 
           172093*la^3 + 1734*la^4) - 4*En^4*(-56617425 + 202502450*la - 
           3571748*la^2 - 169152*la^3 + 4696*la^4))*M) - 
      4*J^2*(-17750601 + 26818560*En^12 + 130327147*la - 140117077*la^2 + 
        15837946*la^3 - 359989*la^4 + 672*la^5 + 
        216*En^10*(-580396 + 385761*la) - 72*En^8*(94547 - 3450875*la + 
          971825*la^2) - 6*En^6*(-74243442 + 305102003*la - 128721114*la^2 + 
          1177390*la^3) - 3*En^4*(174663090 - 860182335*la + 528612455*la^2 - 
          17618552*la^3 + 27800*la^4) + En^2*(201633579 - 1214237878*la + 
          1002971237*la^2 - 65481719*la^3 + 591656*la^4 + 624*la^5))*M^3 + 
      3*(102547924 + 491477472*En^8 - 235251071*la + 90288421*la^2 - 
        4374024*la^3 + 25280*la^4 + 144*En^6*(-13215788 + 6909687*la) + 
        48*En^4*(48408781 - 51905597*la + 6057289*la^2) + 
        2*En^2*(-508505686 + 840323037*la - 203547373*la^2 + 4150298*la^3))*
       M^5)*rp[t]^28 + 2*((-1 + En^2)*J^4*la^2*(1048820 - 286244*la + 
        23015*la^2 - 201*la^3 + 19200*En^10*(-229 + 2*la) + 
        1920*En^8*(-511 + 184*la + 2*la^2) - 8*En^6*(-240710 + 300751*la - 
          5977*la^2 + 32*la^3) - 12*En^2*(748335 - 81330*la + 2024*la^2 + 
          89*la^3) + 8*En^4*(1399515 + 138532*la - 9958*la^2 + 160*la^3)) - 
      J^2*(1396980 - 25495122*la + 50843238*la^2 - 8453357*la^3 + 
        301303*la^4 - 1008*la^5 + 34560*En^12*(-651 + 1034*la) + 
        144*En^10*(412500 - 1186672*la + 218829*la^2) + 
        24*En^8*(-1264230 - 216186*la + 3205749*la^2 + 151316*la^3) + 
        4*En^6*(-10740870 + 152349489*la - 162315057*la^2 + 2813966*la^3 + 
          38501*la^4) + 2*En^4*(26620650 - 363634152*la + 473063159*la^2 - 
          26564600*la^3 + 89646*la^4 + 1132*la^5) - 
        En^2*(18234720 - 283707222*la + 456834845*la^2 - 45919733*la^3 + 
          726866*la^4 + 1872*la^5))*M^2 + (-28616436 + 124357680*En^10 + 
        113927614*la - 74432614*la^2 + 6223288*la^3 - 81856*la^4 + 64*la^5 + 
        216*En^8*(-3397343 + 2370084*la) + 36*En^6*(38754863 - 55819265*la + 
          7991549*la^2) + 6*En^4*(-186502065 + 414309735*la - 
          123208088*la^2 + 2473525*la^3) + 3*En^2*(120640722 - 368079736*la + 
          170560391*la^2 - 7934893*la^3 + 32568*la^4))*M^4)*rp[t]^29 + 
    2*(-((-1 + En^2)*J^2*la*(-989130 + 4792645*la - 1154357*la^2 + 
         61860*la^3 - 336*la^4 + 69120*En^10*(-217 + 86*la) + 
         144*En^8*(171980 - 165713*la + 7139*la^2) + 
         48*En^6*(95070 - 501266*la - 50764*la^2 + 1305*la^3) + 
         4*En^4*(-6263130 + 20944696*la - 1146907*la^2 - 18039*la^3 + 
           248*la^4) - 2*En^2*(-5857680 + 23563334*la - 3266883*la^2 + 
           66163*la^3 + 636*la^4))*M) + (2775888 + 7879680*En^12 - 
        20969510*la + 23446339*la^2 - 3068119*la^3 + 69808*la^4 - 128*la^5 + 
        432*En^10*(-190484 + 195477*la) + 576*En^8*(404449 - 878486*la + 
          163527*la^2) + 36*En^6*(-8005376 + 27052887*la - 10574142*la^2 + 
          201056*la^3) + 12*En^4*(14169420 - 65898342*la + 40160002*la^2 - 
          1859080*la^3 + 4723*la^4) - 4*En^2*(10792152 - 64853341*la + 
          54801257*la^2 - 4444436*la^3 + 41498*la^4 + 24*la^5))*M^3)*
     rp[t]^30 + 4*(-1 + En^2)*((-1 + En^2)*J^2*la^2*(-88610 + 31109*la - 
        2480*la^2 + 21*la^3 + 1920*En^8*(653 + 2*la) - 
        960*En^6*(819 - 208*la + 2*la^2) + En^4*(-1276040 + 5068*la + 
          8444*la^2 - 64*la^3) + 10*En^2*(94858 - 17977*la + 451*la^2 + 
          12*la^3)) + (17280*En^10*(-84 + 151*la) + 
        72*En^8*(70140 - 350008*la + 102249*la^2) + 
        36*En^6*(-182700 + 1512258*la - 1069655*la^2 + 19585*la^3) - 
        6*En^4*(-643860 + 7520289*la - 8637076*la^2 + 552261*la^3 + 
          391*la^4) - 6*(-8820 + 167172*la - 348327*la^2 + 68699*la^3 - 
          2444*la^4 + 8*la^5) - En^2*(937440 - 14277105*la + 22947002*la^2 - 
          2813150*la^3 + 37469*la^4 + 120*la^5))*M^2)*rp[t]^31 + 
    4*(-1 + En^2)^2*la*(-37530 + 191523*la - 56770*la^2 + 3025*la^3 - 
      16*la^4 + 11520*En^8*(-84 + 37*la) + 24*En^6*(101220 - 166343*la + 
        781*la^2) - 12*En^4*(169860 - 480727*la + 40847*la^2 + 438*la^3) + 
      En^2*(614700 - 2460326*la + 451260*la^2 - 8262*la^3 - 68*la^4))*M*
     rp[t]^32 - 8*(-1 + En^2)^3*la^2*(-3370 + 1547*la - 122*la^2 + la^3 + 
      2880*En^6*(29 + la) + 480*En^4*(-265 + 30*la + la^2) + 
      2*En^2*(25450 - 7135*la + 178*la^2 + 3*la^3))*rp[t]^33))/
  (En^2*(1 + la)*rp[t]^20*(3*M + la*rp[t])^2*(J^2 + rp[t]^2)^11) - 
 ((8*I)*J^3*m*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*(1540749924*J^24*M^9 - 
    108*J^24*(36803319 - 10951666*la + 583353*m^2)*M^8*rp[t] + 
    9*J^22*M^7*(J^2*(476975487 - 336977040*la + 29771816*la^2 + 
        (16752396 - 4951104*la)*m^2 + 36696*m^4) + 2168916264*M^2)*rp[t]^2 + 
    3*J^22*M^6*(-(J^2*(835045785 - 1084009486*la + 225553512*la^2 - 
         5096084*la^3 + (49261029 - 35514188*la + 2783012*la^2)*m^2 + 
         (242052 - 75024*la)*m^4 + 48*m^6)) + 
      54*(-311057942 + 29344407*En^2 + 92311624*la - 4656658*m^2)*M^2)*
     rp[t]^3 + 2*J^20*M^5*(J^4*(424102464 - 940782240*la + 356182954*la^2 - 
        18180162*la^3 + 85924*la^4 + (37851984 - 52149384*la + 9931102*la^2 - 
          79916*la^3)*m^2 + (316287 - 247620*la + 19348*la^2)*m^4 - 
        48*(-3 + la)*m^6) - 9*J^2*(-3025673983 + 2132085228*la - 
        187412552*la^2 + 2*(-50278483 + 14799908*la)*m^2 - 198840*m^4 + 
        3*En^2*(182881593 - 67146892*la + 4492926*m^2))*M^2 + 
      56911296600*M^4)*rp[t]^4 + 
    J^20*M^4*(-(J^4*(165302532 - 630453678*la + 402970799*la^2 - 
         35384615*la^3 + 376188*la^4 - 240*la^5 + 
         (21287475 - 53333925*la + 19317927*la^2 - 351172*la^3 + 272*la^4)*
          m^2 + (272457 - 431748*la + 85148*la^2 - 112*la^3)*m^4 + 
         8*(27 - 24*la + 2*la^2)*m^6)) + 
      3*J^2*(-10602232839 + 13729732068*la - 2842895040*la^2 + 
        63272096*la^3 + (-593074131 + 425778500*la - 33163452*la^2)*m^2 + 
        8*(-329307 + 101540*la)*m^4 - 432*m^6 + 
        6*En^2*(458239129 - 415052803*la + 45404385*la^2 + 
          (25548233 - 9501104*la)*m^2 + 101124*m^4))*M^2 + 
      54*(-5445803394 + 1072508253*En^2 + 1611194096*la - 76366078*m^2)*M^4)*
     rp[t]^5 + J^18*M^3*(J^6*(16971732 - 121281612*la + 131254147*la^2 - 
        17974642*la^3 + 326102*la^4 - 480*la^5 + 
        (3097827 - 14960127*la + 9795016*la^2 - 305634*la^3 + 544*la^4)*m^2 - 
        7*(-8271 + 26586*la - 10602*la^2 + 32*la^3)*m^4 + 
        8*(9 - 18*la + 4*la^2)*m^6) - 6*J^4*(-1796350923 + 3975795327*la - 
        1498310670*la^2 + 75317592*la^3 - 353908*la^4 + 
        (-152385795 + 209022952*la - 39561770*la^2 + 306388*la^3)*m^2 - 
        14*(82359 - 64108*la + 4980*la^2)*m^4 + 144*(-3 + la)*m^6 + 
        2*En^2*(292916778 - 514885071*la + 137712711*la^2 - 3818324*la^3 + 
          (28267848 - 26961047*la + 2673795*la^2)*m^2 - 8*(-32289 + 12899*la)*
           m^4 + 108*m^6))*M^2 + 36*J^2*(8836302529 + 98384337*En^4 - 
        6208419466*la + 542625516*la^2 + (275665169 - 80779088*la)*m^2 + 
        484806*m^4 - 6*En^2*(558306930 - 204081120*la + 13003033*m^2))*M^4 + 
      404179449912*M^6)*rp[t]^6 + 
    J^18*M^2*(-(J^6*(695628 - 12246960*la + 24342136*la^2 - 5004300*la^3 + 
         139681*la^4 - 360*la^5 + (180801 - 2169504*la + 2716855*la^2 - 
           131483*la^3 + 408*la^4)*m^2 + (4842 - 39576*la + 31969*la^2 - 
           168*la^3)*m^4 + 3*(3 - 16*la + 8*la^2)*m^6)) + 
      J^4*(-2102343534 + 8001564300*la - 5092430716*la^2 + 440271220*la^3 - 
        4651436*la^4 + 2944*la^5 + (-258008454 + 643425363*la - 
          231612921*la^2 + 4048988*la^3 - 3008*la^4)*m^2 + 
        2*(-1498041 + 2358612*la - 462196*la^2 + 560*la^3)*m^4 - 
        72*(27 - 24*la + 2*la^2)*m^6 - 6*En^2*(-132689424 + 433565877*la - 
          222564594*la^2 + 14347980*la^3 - 63676*la^4 + 
          (-20149065 + 39670163*la - 10045151*la^2 + 106748*la^3)*m^2 + 
          (-324771 + 351676*la - 35476*la^2)*m^4 + 36*(-9 + 4*la)*m^6))*M^2 - 
      3*J^2*(61984160199 - 80045871504*la + 16483889920*la^2 - 
        360684552*la^3 + (3262381803 - 2331041892*la + 180352796*la^2)*m^2 + 
        (12903444 - 3957648*la)*m^4 + 1728*m^6 + 
        36*En^4*(51587852 - 24744649*la + 1772860*m^2) - 
        6*En^2*(5610002698 - 5058947711*la + 549572857*la^2 + 
          (297277214 - 109793696*la)*m^2 + 1053900*m^4))*M^4 + 
      54*(-19357385566 + 5987223735*En^2 + 5707123592*la - 251629482*m^2)*
       M^6)*rp[t]^7 + 2*J^16*M*
     (-(J^8*la*(2*(122742 - 584952*la + 179893*la^2 - 7374*la^3 + 30*la^4) + 
         (62997 - 194031*la + 13948*la^2 - 68*la^3)*m^2 + 
         (1656 - 3395*la + 28*la^2)*m^4 + (3 - 4*la)*m^6)) - 
      J^6*(-108028296 + 770546466*la - 830646399*la^2 + 111962286*la^3 - 
        2017531*la^4 + 2944*la^5 + (-18848313 + 90574674*la - 58933577*la^2 + 
          1766863*la^3 - 3008*la^4)*m^2 + (-320337 + 1022214*la - 
          404782*la^2 + 1120*la^3)*m^4 - 36*(9 - 18*la + 4*la^2)*m^6 + 
        En^2*(44577594 - 290171925*la + 273016976*la^2 - 31448744*la^3 + 
          320224*la^4 + 96*la^5 + 3*(3435501 - 14087006*la + 7321550*la^2 - 
            181064*la^3 + 128*la^4)*m^2 + (266967 - 663600*la + 181300*la^2 - 
            336*la^3)*m^4 + 18*(27 - 36*la + 4*la^2)*m^6))*M^2 + 
      3*J^4*(10512582786 - 23206566921*la + 8700181798*la^2 - 
        429855060*la^3 + 2005660*la^4 - 2*(-420653259 + 574115088*la - 
          107924903*la^2 + 801150*la^3)*m^2 + 
        (5675769 - 4391868*la + 339164*la^2)*m^4 - 576*(-3 + la)*m^6 + 
        6*En^4*(91821426 - 115531430*la + 16451553*la^2 + 
          (7418608 - 3729584*la)*m^2 + 46892*m^4) - 
        3*En^2*(2397434758 - 4195733634*la + 1114579625*la^2 - 
          30379688*la^3 + (220545446 - 208813620*la + 20505220*la^2)*m^2 - 
          36*(-50305 + 19888*la)*m^4 + 576*m^6))*M^4 + 
      9*J^2*(62885881005 + 2299573080*En^4 - 44035764932*la + 
        3824102936*la^2 + (1822894962 - 531464504*la)*m^2 + 2800416*m^4 - 
        3*En^2*(12499768601 - 4546295500*la + 273634206*m^2))*M^6 + 
      487105090938*M^8)*rp[t]^8 - 
    J^16*(J^8*la^2*(87408 - 41384*la + 2449*la^2 - 15*la^3 + 
        (22064 - 2328*la + 17*la^2)*m^2 - 7*(-81 + la)*m^4 + m^6) - 
      J^6*(2*En^2*(1867158 - 31861503*la + 58627343*la^2 - 11093828*la^3 + 
          197861*la^4 + 144*la^5 + (662364 - 7168863*la + 7702394*la^2 - 
            339917*la^3 + 576*la^4)*m^2 + (26757 - 181905*la + 113939*la^2 - 
            504*la^3)*m^4 + 27*(3 - 12*la + 4*la^2)*m^6) - 
        3*(2955024 - 51941214*la + 102886623*la^2 - 20809429*la^3 + 
          576583*la^4 - 1472*la^5 + (736758 - 8793739*la + 10943327*la^2 - 
            508339*la^3 + 1504*la^4)*m^2 + (17991 - 145832*la + 116886*la^2 - 
            560*la^3)*m^4 + 9*(3 - 16*la + 8*la^2)*m^6))*M^2 + 
      J^4*(12316639590 - 46764745254*la + 29617830146*la^2 - 
        2515982678*la^3 + 26379836*la^4 - 16560*la^5 + 
        (1430280891 - 3547922787*la + 1268307721*la^2 - 21228620*la^3 + 
          15120*la^4)*m^2 - 3*(-4944231 + 7732572*la - 1505476*la^2 + 
          1680*la^3)*m^4 + 288*(27 - 24*la + 2*la^2)*m^6 + 
        12*En^4*(75602907 - 202944978*la + 75095464*la^2 - 2558981*la^3 + 
          2*(5539899 - 7778480*la + 1044434*la^2)*m^2 + (169932 - 95444*la)*
           m^4 + 144*m^6) + 6*En^2*(-4*(408532449 - 1329064425*la + 
            677747474*la^2 - 42944012*la^3 + 190008*la^4) + 
          (-237348519 + 463636686*la - 116223254*la^2 + 1172344*la^3)*m^2 - 
          27*(127811 - 136780*la + 13636*la^2)*m^4 + 288*(-9 + 4*la)*m^6))*
       M^4 - 3*J^2*(-220817667201 + 297944784*En^6 + 284250044460*la - 
        58173559952*la^2 + 1248461600*la^3 + (-10827655317 + 7695058188*la - 
          590995684*la^2)*m^2 + 96*(-389931 + 118964*la)*m^4 - 4032*m^6 - 
        72*En^4*(302975568 - 144337965*la + 9909214*m^2) + 
        6*En^2*(31491444327 - 28256848921*la + 3045470683*la^2 + 
          (1573271841 - 576461600*la)*m^2 + 4882128*m^4))*M^6 - 
      54*(-46716969644 + 20245106847*En^2 + 13718152436*la - 555412308*m^2)*
       M^8)*rp[t]^9 + 
    J^14*M*(-(J^8*la*(6256188 - 29733239*la + 8990537*la^2 - 365593*la^3 + 
         1472*la^4 + (1539621 - 4712199*la + 324739*la^2 - 1504*la^3)*m^2 + 
         (36882 - 74956*la + 560*la^2)*m^4 + (54 - 72*la)*m^6 + 
         2*En^2*(-1299096 + 6110381*la - 1871405*la^2 + 53207*la^3 + 
           72*la^4 + (-457281 + 1284005*la - 92645*la^2 + 288*la^3)*m^2 - 
           2*(9117 - 15589*la + 126*la^2)*m^4 + 54*(-1 + la)*m^6))) + 
      J^6*(-4*En^4*(-27918702 + 164345958*la - 128239917*la^2 + 
          10585972*la^3 - 32838*la^4 + 3*(-2292111 + 7710396*la - 
            2879756*la^2 + 38592*la^3)*m^2 - 9*(22119 - 38444*la + 5460*la^2)*
           m^4 + 144*(-3 + 2*la)*m^6) - 
        4*En^2*(2*(137726028 - 892603269*la + 834475646*la^2 - 
            94459171*la^3 + 958422*la^4 + 234*la^5) + 
          (61162641 - 248650056*la + 127903316*la^2 - 3001288*la^3 + 
            1956*la^4)*m^2 - 9*(-159507 + 391182*la - 105476*la^2 + 168*la^3)*
           m^4 + 72*(27 - 36*la + 4*la^2)*m^6) + 
        3*(422424333 - 3006524178*la + 3226625701*la^2 - 427160912*la^3 + 
          7634282*la^4 - 11040*la^5 + (69982551 - 334384143*la + 
            216039844*la^2 - 6193530*la^3 + 10080*la^4)*m^2 + 
          (1064277 - 3370314*la + 1325002*la^2 - 3360*la^3)*m^4 + 
          96*(9 - 18*la + 4*la^2)*m^6))*M^2 - 
      6*J^4*(-37497356601 + 82525520777*la - 30756225650*la^2 + 
        1489899720*la^3 - 6893260*la^4 + (-2804132007 + 3805221864*la - 
          709920414*la^2 + 5028620*la^3)*m^2 - 
        8*(2068587 - 1591164*la + 122164*la^2)*m^4 + 1344*(-3 + la)*m^6 + 
        144*En^6*(1103092 - 765629*la + 54716*m^2) - 
        12*En^4*(542300817 - 677560802*la + 95610886*la^2 + 
          (41853688 - 20813420*la)*m^2 + 239192*m^4) + 
        2*En^2*(20251896516 - 35266537833*la + 9296231094*la^2 - 
          248409944*la^3 + (1762468479 - 1654654995*la + 160663799*la^2)*
           m^2 - 48*(-264723 + 103502*la)*m^4 + 3024*m^6))*M^4 + 
      9*J^2*(303946828011 + 24504946236*En^4 - 212009626376*la + 
        18276370296*la^2 + (8079000696 - 2342020352*la)*m^2 + 10606512*m^4 - 
        12*En^2*(21196983391 - 7665698948*la + 431277528*m^2))*M^6 + 
      1680820099344*M^8)*rp[t]^10 + 
    J^14*(J^8*la^2*(-1113768 + 517986*la - 30385*la^2 + 184*la^3 + 
        (-269486 + 27217*la - 188*la^2)*m^2 + 7*(-901 + 10*la)*m^4 - 9*m^6 + 
        2*En^2*(230760 - 119028*la + 5227*la^2 + 12*la^3 + 
          (79856 - 9227*la + 48*la^2)*m^2 - 7*(-445 + 6*la)*m^4 + 9*m^6)) + 
      J^6*(-4*En^4*(1164780 - 19796751*la + 33383073*la^2 - 5216469*la^3 + 
          36815*la^4 + 232*la^5 + (483732 - 4754517*la + 4233112*la^2 - 
            138200*la^3 - 24*la^4)*m^2 + (24840 - 135111*la + 59360*la^2 - 
            140*la^3)*m^4 + 12*(9 - 24*la + 4*la^2)*m^6) + 
        2*En^2*(23164056 - 393559884*la + 719773850*la^2 - 133814533*la^3 + 
          2376756*la^4 + 1404*la^5 + (7931682 - 85028346*la + 90384880*la^2 - 
            3783266*la^3 + 5868*la^4)*m^2 - 9*(-32436 + 217083*la - 
            133973*la^2 + 504*la^3)*m^4 + 216*(3 - 12*la + 4*la^2)*m^6) - 
        3*(17355465 - 304476666*la + 600761779*la^2 - 119280908*la^3 + 
          3275655*la^4 - 8280*la^5 + (4125447 - 48935979*la + 60462283*la^2 - 
            2681735*la^3 + 7560*la^4)*m^2 + (90360 - 725988*la + 
            577199*la^2 - 2520*la^3)*m^4 + 36*(3 - 16*la + 8*la^2)*m^6))*
       M^2 + J^4*(4*(-10997803377 + 41639832771*la - 26225585003*la^2 + 
          2183403645*la^3 - 22684585*la^4 + 14120*la^5) - 
        3*(1596718188 - 3936438591*la + 1396250533*la^2 - 22265500*la^3 + 
          15200*la^4)*m^2 + 24*(-1812393 + 2815476*la - 544628*la^2 + 
          560*la^3)*m^4 - 672*(27 - 24*la + 2*la^2)*m^6 + 
        72*En^6*(4729220 - 9656001*la + 1964681*la^2 - 8*(-73167 + 56968*la)*
           m^2 + 6196*m^4) - 12*En^4*(898566156 - 2394718581*la + 
          878057740*la^2 - 29416354*la^3 + 4*(31590912 - 43829231*la + 
            5804273*la^2)*m^2 - 64*(-27564 + 15187*la)*m^4 + 1008*m^6) - 
        6*En^2*(-9235825263 + 29896783467*la - 15131231868*la^2 + 
          939775968*la^3 - 4131740*la^4 + (-1274090040 + 2466257223*la - 
            611117131*la^2 + 5796716*la^3)*m^2 - 4*(4081671 - 4312428*la + 
            424676*la^2)*m^4 + 1008*(-9 + 4*la)*m^6))*M^4 + 
      6*J^2*(-267205970277 + 1693566360*En^6 + 342676970031*la - 
        69633656924*la^2 + 1461404630*la^3 + (-12049910289 + 8511432156*la - 
          648413908*la^2)*m^2 + 252*(-141271 + 42876*la)*m^4 - 3024*m^6 - 
        18*En^4*(3247151232 - 1535011605*la + 100219984*m^2) + 
        3*En^2*(107161709406 - 95608701565*la + 10211522739*la^2 - 
          24*(-208098411 + 75540976*la)*m^2 + 13160112*m^4))*M^6 + 
      108*(23080072473*En^2 - 14*(2883286459 - 842667940*la + 30797673*m^2))*
       M^8)*rp[t]^11 + 2*J^12*M*(J^8*la*(-18369720 + 87026237*la - 
        25814417*la^2 + 1039568*la^3 - 4140*la^4 + 
        (-4308417 + 13090561*la - 859880*la^2 + 3780*la^3)*m^2 - 
        12*(7710 - 15526*la + 105*la^2)*m^4 + 36*(-3 + 4*la)*m^6 + 
        En^2*(16115490 - 75371864*la + 22674967*la^2 - 641538*la^3 - 
          702*la^4 - 2*(-2737359 + 7600616*la - 519925*la^2 + 1467*la^3)*
           m^2 + 9*(22080 - 37121*la + 252*la^2)*m^4 - 432*(-1 + la)*m^6) + 
        2*En^4*(-801882 + 3831810*la - 1072359*la^2 + 13212*la^3 + 232*la^4 - 
          2*(165609 - 428239*la + 26629*la^2 + 12*la^3)*m^2 + 
          (-16848 + 23149*la - 140*la^2)*m^4 + 24*(-3 + 2*la)*m^6)) - 
      J^6*(-2266475013 + 16089712281*la - 17179495547*la^2 + 
        2227912970*la^3 - 39425045*la^4 + 56480*la^5 - 
        3*(117834642 - 559277076*la + 358444383*la^2 - 9772875*la^3 + 
          15200*la^4)*m^2 + 12*(-392751 + 1234182*la - 481726*la^2 + 
          1120*la^3)*m^4 - 336*(9 - 18*la + 4*la^2)*m^6 + 
        12*En^6*(1906008 - 10164212*la + 6042763*la^2 - 245736*la^3 + 
          6*(77780 - 202093*la + 41829*la^2)*m^2 - 4*(-3315 + 3136*la)*m^4 + 
          24*m^6) + 12*En^4*(-55674162 + 325340154*la - 251548465*la^2 + 
          20419280*la^3 - 66214*la^4 + (-13234716 + 43929393*la - 
            16169410*la^2 + 205772*la^3)*m^2 + (-351594 + 597648*la - 
            83048*la^2)*m^4 + 168*(-3 + 2*la)*m^6) + 
        En^2*(3126223971 - 20159235495*la + 18709428222*la^2 - 
          2075502326*la^3 + 20910616*la^4 + 4008*la^5 + 
          6*(110399229 - 444380991*la + 225872915*la^2 - 4979204*la^3 + 
            2988*la^4)*m^2 - 12*(-1147455 + 2771928*la - 737044*la^2 + 
            1008*la^3)*m^4 + 504*(27 - 36*la + 4*la^2)*m^6))*M^2 + 
      3*J^4*(90890322624 + 9804240*En^8 - 199323014583*la + 
        73779865376*la^2 - 3493483890*la^3 + 16002000*la^4 - 
        4*(-1568113542 + 2113916544*la - 391068889*la^2 + 2630490*la^3)*m^2 + 
        14*(2259531 - 1727892*la + 131908*la^2)*m^4 - 2016*(-3 + la)*m^6 - 
        144*En^6*(12673569 - 8698659*la + 602318*m^2) + 
        6*En^4*(5849339046 - 7249963082*la + 1012467215*la^2 - 
          448*(-954826 + 468987*la)*m^2 + 2149952*m^4) - 
        En^2*(138342200436 - 239535913650*la + 62584016075*la^2 - 
          1633781072*la^3 + 8*(1409860737 - 1310437050*la + 125591498*la^2)*
           m^2 - 1008*(-68619 + 26536*la)*m^4 + 12096*m^6))*M^4 + 
      18*J^2*(131540906043 + 19648955520*En^4 - 91330206076*la + 
        7806642312*la^2 - 826*(-3813579 + 1098484*la)*m^2 + 3439464*m^4 - 
        3*En^2*(48497889537 - 17423988524*la + 902958126*m^2))*M^6 + 
      1065514709952*M^8)*rp[t]^12 + 
    J^12*(J^8*la^2*(-3*(2180110 - 993632*la + 57668*la^2 - 345*la^3 + 
          7*(71779 - 6895*la + 45*la^2)*m^2 - 35*(-301 + 3*la)*m^4 + 
          12*m^6) - 4*En^4*(145098 - 75761*la + 1507*la^2 + 58*la^3 + 
          (58338 - 6551*la - 6*la^2)*m^2 - 7*(-411 + 5*la)*m^4 + 12*m^6) + 
        6*En^2*(953852 - 483238*la + 21095*la^2 + 39*la^3 + 
          (318486 - 34872*la + 163*la^2)*m^2 - 42*(-269 + 3*la)*m^4 + 
          24*m^6)) + J^6*(-8*En^6*(-217404 + 4003302*la - 6185863*la^2 + 
          646866*la^3 + 4943*la^4 + (-103284 + 959550*la - 664159*la^2 + 
            9104*la^3)*m^2 - 4*(1629 - 6711*la + 1603*la^2)*m^4 + 
          12*(-3 + 4*la)*m^6) - 8*En^4*(7014924 - 118399608*la + 
          197848077*la^2 - 30415283*la^3 + 225781*la^4 + 1162*la^5 + 
          (2834622 - 27439809*la + 24049951*la^2 - 747110*la^3 - 84*la^4)*
           m^2 + (134946 - 714936*la + 306320*la^2 - 560*la^3)*m^4 + 
          42*(9 - 24*la + 4*la^2)*m^6) + 2*En^2*(132049197 - 2232114867*la + 
          4053795285*la^2 - 738432983*la^3 + 13013089*la^4 + 6012*la^5 + 
          3*(14464989 - 153357696*la + 161013056*la^2 - 6324963*la^3 + 
            8964*la^4)*m^2 - 36*(-39543 + 259877*la - 157815*la^2 + 504*la^3)*
           m^4 + 756*(3 - 12*la + 4*la^2)*m^6) - 
        3*(62180811 - 1088364288*la + 2137752104*la^2 - 415538885*la^3 + 
          11289025*la^4 - 28240*la^5 + (13981653 - 164638956*la + 
            201737757*la^2 - 8491925*la^3 + 22800*la^4)*m^2 - 
          28*(-9603 + 76464*la - 60302*la^2 + 240*la^3)*m^4 + 
          84*(3 - 16*la + 8*la^2)*m^6))*M^2 - 
      3*J^4*(35603318772 - 134351338872*la + 84073157951*la^2 - 
        6838029595*la^3 + 70275440*la^4 - 43360*la^5 + 
        (3591982422 - 8791858854*la + 3091141682*la^2 - 46711160*la^3 + 
          30560*la^4)*m^2 - 98*(-284427 + 438924*la - 84372*la^2 + 80*la^3)*
         m^4 + 336*(27 - 24*la + 2*la^2)*m^6 + 
        144*En^8*(75491 - 96028*la + 5608*m^2) - 
        24*En^6*(54971762 - 110921503*la + 22293011*la^2 + 
          (6560988 - 5014816*la)*m^2 + 64276*m^4) + 
        4*En^4*(4880470941 - 12899389179*la + 4680423856*la^2 - 
          153668129*la^3 + 16*(40850331 - 55910668*la + 7286733*la^2)*m^2 - 
          112*(-72363 + 38927*la)*m^4 + 3024*m^6) + 
        2*En^2*(-31677597063 + 101951140860*la - 51154823938*la^2 + 
          3103398652*la^3 - 13502600*la^4 + 4*(-1028376999 + 1969261242*la - 
            481400946*la^2 + 4244776*la^3)*m^2 - 28*(1605087 - 1674396*la + 
            162932*la^2)*m^4 + 2016*(-9 + 4*la)*m^6))*M^4 + 
      6*J^2*(-463420507563 + 8730510048*En^6 + 591659852244*la - 
        119237428288*la^2 + 2438154784*la^3 - 7*(2698392465 - 1892857836*la + 
          142926932*la^2)*m^2 + 1176*(-39435 + 11908*la)*m^4 - 3024*m^6 - 
        144*En^4*(1310576894 - 613979355*la + 37774618*m^2) + 
        6*En^2*(3*(41019822203 - 36358870197*la + 3842605559*la^2) - 
          49*(-107624577 + 38632304*la)*m^2 + 11356380*m^4))*M^6 + 
      756*(-7326697054 + 5336921127*En^2 + 2129233984*la - 68635074*m^2)*M^8)*
     rp[t]^13 + J^10*M*(J^8*la*(-131611932 + 621186894*la - 180252165*la^2 + 
        7173815*la^3 - 28240*la^4 + 3*(-9728802 + 29307881*la - 
          1822475*la^2 + 7600*la^3)*m^2 - 24*(22917 - 45724*la + 280*la^2)*
         m^4 + 168*(-3 + 4*la)*m^6 - 8*En^6*
         (2*(-73899 + 394961*la - 86313*la^2 - 2057*la^3 + 38*la^4) - 
          (69996 - 174691*la + 6124*la^2 + 74*la^3)*m^2 + 
          8*(-549 + 574*la)*m^4 + 8*(-3 + la)*m^6) - 
        2*En^2*(-91861992 + 426814284*la - 125785283*la^2 + 3527737*la^3 + 
          3006*la^4 + 3*(-9981906 + 27358439*la - 1754515*la^2 + 4482*la^3)*
           m^2 - 24*(40317 - 66500*la + 378*la^2)*m^4 + 1512*(-1 + la)*m^6) + 
        4*En^4*(2*(-4829139 + 22878492*la - 6305258*la^2 + 82249*la^3 + 
            1162*la^4) - 7*(554508 - 1409245*la + 83608*la^2 + 24*la^3)*m^2 - 
          28*(6534 - 8717*la + 40*la^2)*m^4 + 168*(-3 + 2*la)*m^6)) + 
      6*J^6*(1837680759 - 13005581154*la + 13804010836*la^2 - 
        1747792955*la^3 + 30564580*la^4 - 43360*la^5 + 
        (266832423 - 1256445909*la + 797883472*la^2 - 20564110*la^3 + 
          30560*la^4)*m^2 - 7*(-434313 + 1354446*la - 524958*la^2 + 
          1120*la^3)*m^4 + 168*(9 - 18*la + 4*la^2)*m^6 + 
        24*En^8*(30804 - 156701*la + 55359*la^2 + (6412 - 11264*la)*m^2 + 
          116*m^4) - 8*En^6*(11214540 - 59085279*la + 34671935*la^2 - 
          1401020*la^3 + 4*(667671 - 1698876*la + 344752*la^2)*m^2 + 
          (71004 - 64968*la)*m^4 + 72*m^6) - 
        2*En^4*(-609465555 + 3531493287*la - 2701810197*la^2 + 
          214971472*la^3 - 720802*la^4 + 4*(-34724763 + 113493528*la - 
            41077480*la^2 + 488920*la^3)*m^2 - 112*(29655 - 48933*la + 
            6626*la^2)*m^4 + 1008*(-3 + 2*la)*m^6) - 
        2*En^2*(1795626342 - 11510816412*la + 10593570754*la^2 - 
          1147722919*la^3 + 11431264*la^4 + 1632*la^5 + 
          2*(180053955 - 716326368*la + 359008544*la^2 - 7341008*la^3 + 
            4056*la^4)*m^2 - 56*(-114345 + 272109*la - 71372*la^2 + 84*la^3)*
           m^4 + 168*(27 - 36*la + 4*la^2)*m^6))*M^2 + 
      12*J^4*(78973345341 + 56757888*En^8 - 172448859531*la + 
        63324348062*la^2 - 2919422904*la^3 + 13216068*la^4 - 
        7*(-706387077 + 945069360*la - 173211358*la^2 + 1100796*la^3)*m^2 + 
        14*(1478853 - 1124484*la + 85372*la^2)*m^4 - 1008*(-3 + la)*m^6 - 
        144*En^6*(33042411 - 22401094*la + 1489787*m^2) + 
        24*En^4*(2378709327 - 2920536924*la + 402957812*la^2 - 
          28*(-5826220 + 2821557*la)*m^2 + 684950*m^4) - 
        6*En^2*(26591052234 - 45740166009*la + 11828405283*la^2 - 
          300254644*la^3 + 7*(286372770 - 262921995*la + 24816151*la^2)*m^2 - 
          168*(-59760 + 22871*la)*m^4 + 1260*m^6))*M^4 + 
      504*J^2*(11965166160 + 3009843027*En^4 - 8261243038*la + 
        699165900*la^2 + (251990535 - 72074656*la)*m^2 + 221010*m^4 - 
        6*En^2*(2814856997 - 1003557732*la + 46857957*m^2))*M^6 + 
      2003327529840*M^8)*rp[t]^14 + 
    J^10*(J^8*la^2*(-23428167 + 10433810*la - 597740*la^2 + 3530*la^3 - 
        3*(1700657 - 154170*la + 950*la^2)*m^2 + 84*(-1117 + 10*la)*m^4 - 
        84*m^6 + 8*En^6*(28794 - 13545*la - 767*la^2 + 38*la^3 + 
          (12866 - 947*la - 37*la^2)*m^2 + 756*m^4 + 4*m^6) - 
        28*En^4*(249198 - 128446*la + 2717*la^2 + 83*la^3 - 
          2*(-48744 + 5243*la + 3*la^2)*m^2 + (4452 - 40*la)*m^4 + 12*m^6) + 
        6*En^2*(5435017 - 2697019*la + 116574*la^2 + 167*la^3 + 
          (1740851 - 178523*la + 747*la^2)*m^2 + (54908 - 504*la)*m^4 + 
          84*m^6)) + J^6*(48*En^8*(-2088 + 62622*la - 91832*la^2 + 659*la^3 - 
          4*(270 - 3201*la + 1423*la^2)*m^2 + 12*(-6 + 19*la)*m^4) - 
        24*En^6*(-862200 + 15708078*la - 23936445*la^2 + 2494778*la^3 + 
          14767*la^4 + 8*(-50391 + 457707*la - 309723*la^2 + 4264*la^3)*m^2 - 
          4*(6030 - 23937*la + 5509*la^2)*m^4 + 24*(-3 + 4*la)*m^6) - 
        4*En^4*(77411376 - 1295755893*la + 2142568377*la^2 - 322976625*la^3 + 
          2491247*la^4 + 10428*la^5 - 4*(-7574553 + 71954889*la - 
            61931356*la^2 + 1803860*la^3 + 120*la^4)*m^2 - 
          28*(-47223 + 240717*la - 99920*la^2 + 140*la^3)*m^4 + 
          252*(9 - 24*la + 4*la^2)*m^6) + 6*En^2*(152518086 - 2562535641*la + 
          4616153769*la^2 - 821089494*la^3 + 14288042*la^4 + 4896*la^5 + 
          8*(5967801 - 62471877*la + 64637486*la^2 - 2349649*la^3 + 
            3042*la^4)*m^2 - 28*(-48078 + 310287*la - 185465*la^2 + 504*la^3)*
           m^4 + 504*(3 - 12*la + 4*la^2)*m^6) - 
        3*(151558533 - 2645378472*la + 5168559428*la^2 - 980172990*la^3 + 
          26285580*la^4 - 65040*la^5 + (31908177 - 372405204*la + 
            451926358*la^2 - 17930490*la^3 + 45840*la^4)*m^2 - 
          14*(-37458 + 295632*la - 231301*la^2 + 840*la^3)*m^4 + 
          126*(3 - 16*la + 8*la^2)*m^6))*M^2 - 
      6*J^4*(12096*En^10 + 4*(7750292841 - 29128489365*la + 
          18089784035*la^2 - 1431484135*la^3 + 14524069*la^4 - 8880*la^5) + 
        7*(407236572 - 988444179*la + 344117809*la^2 - 4899772*la^3 + 
          3072*la^4)*m^2 - 14*(-1310319 + 2009100*la - 383852*la^2 + 
          336*la^3)*m^4 + 168*(27 - 24*la + 2*la^2)*m^6 + 
        288*En^8*(223429 - 279145*la + 16544*m^2) - 
        24*En^6*(145190933 - 289115086*la + 57291554*la^2 - 
          2*(-8302607 + 6198832*la)*m^2 + 150618*m^4) + 
        8*En^4*(4002409917 - 10476071319*la + 3754588044*la^2 - 
          120345302*la^3 + 14*(36056535 - 48645514*la + 6221422*la^2)*m^2 - 
          224*(-23547 + 12380*la)*m^4 + 1260*m^6) + 
        En^2*(-73426706505 + 234727770906*la - 116604851940*la^2 + 
          6877869928*la^3 - 29468920*la^4 + 14*(-633871557 + 1197365709*la - 
            287994889*la^2 + 2328964*la^3)*m^2 - 42*(1883889 - 1941780*la + 
            186844*la^2)*m^4 + 2520*(-9 + 4*la)*m^6))*M^4 + 
      6*J^2*(26968421664*En^6 - 252*En^4*(1619151868 - 750404655*la + 
          43112972*m^2) + 42*En^2*(28692658146 - 25242275101*la + 
          2634549627*la^2 + (1106150070 - 391804640*la)*m^2 + 1857516*m^4) - 
        7*(84515857353 - 107312673528*la + 21416146592*la^2 - 
          424675208*la^3 + (3039529365 - 2115751068*la + 158230180*la^2)*
           m^2 + (5984604 - 1798320*la)*m^4 + 288*m^6))*M^6 + 
      108*(-48351219182 + 43985327799*En^2 + 13955155016*la - 384121914*m^2)*
       M^8)*rp[t]^15 + 2*J^8*M*
     (-(J^8*la*(-24*En^8*(-3*(464 - 4502*la + 53*la^2 + 109*la^3) + 
           4*(-180 + 591*la + 38*la^2)*m^2 + 12*(-4 + 3*la)*m^4) + 
         4*En^6*(-1758744 + 9265198*la - 2025609*la^2 - 38309*la^3 + 
           708*la^4 - 8*(102492 - 249207*la + 8912*la^2 + 73*la^3)*m^2 + 
           (-48816 + 48860*la)*m^4 + 48*(-3 + la)*m^6) + 
         3*En^2*(-106096536 + 489118363*la - 140731594*la^2 + 3892446*la^3 + 
           2448*la^4 + 4*(-8235108 + 22231727*la - 1315930*la^2 + 3042*la^3)*
            m^2 - 84*(10876 - 17605*la + 84*la^2)*m^4 + 1008*(-1 + la)*m^6) - 
         2*En^4*(-53286660 + 249941691*la - 67612230*la^2 + 921386*la^3 + 
           10428*la^4 - 4*(5184207 - 12904829*la + 720800*la^2 + 120*la^3)*
            m^2 - 280*(3195 - 4093*la + 14*la^2)*m^4 + 504*(-3 + 2*la)*m^6) + 
         3*(53456886 - 251197631*la + 71048830*la^2 - 2787590*la^3 + 
           10840*la^4 + (11093019 - 33077492*la + 1931840*la^2 - 7640*la^3)*
            m^2 + 14*(12756 - 25221*la + 140*la^2)*m^4 - 42*(-3 + 4*la)*
            m^6))) - 3*J^6*(-3207555495 + 22616574744*la + 12096*En^10*la - 
        23836318598*la^2 + 2933844004*la^3 - 50590074*la^4 + 71040*la^5 - 
        14*(30491064 - 142228560*la + 89377205*la^2 - 2163567*la^3 + 
          3072*la^4)*m^2 + 14*(-287667 + 890538*la - 342818*la^2 + 672*la^3)*
         m^4 - 168*(9 - 18*la + 4*la^2)*m^6 - 96*En^8*(93108 - 465423*la + 
          162719*la^2 + (19596 - 33432*la)*m^2 + 372*m^4) + 
        16*En^6*(30136383 - 156111540*la + 90246717*la^2 - 3601330*la^3 + 
          2*(3501036 - 8598939*la + 1702183*la^2)*m^2 - 3*(-59223 + 50632*la)*
           m^4 + 90*m^6) + 8*En^4*(-503753436 + 2893464051*la - 
          2186203348*la^2 + 169840344*la^3 - 582532*la^4 + 
          14*(-7757532 + 25014765*la - 8884564*la^2 + 96576*la^3)*m^2 - 
          21*(105681 - 169528*la + 22404*la^2)*m^4 + 420*(-3 + 2*la)*m^6) + 
        En^2*(8374353723 - 53292888738*la + 48570009008*la^2 - 
          5115967512*la^3 + 50095712*la^4 + 4896*la^5 + 
          28*(56244834 - 220359453*la + 108540221*la^2 - 2027196*la^3 + 
            1032*la^4)*m^2 - 126*(-181233 + 425248*la - 110124*la^2 + 
            112*la^3)*m^4 + 420*(27 - 36*la + 4*la^2)*m^6))*M^2 + 
      6*J^4*(295039584*En^8 - 144*En^6*(103113828 - 69085856*la + 
          4298471*m^2) + 42*En^4*(2969364510 - 3600181146*la + 
          489710755*la^2 + (189408080 - 89875312*la)*m^2 + 614980*m^4) - 
        7*En^2*(37370611062 - 63818261106*la + 16305392117*la^2 - 
          399808472*la^3 + (2553754194 - 2309817132*la + 214209532*la^2)*
           m^2 + (9937116 - 3766656*la)*m^4 + 864*m^6) - 
        7*(-14441166360 + 31366477323*la - 11408469282*la^2 + 
          509544516*la^3 - 2275364*la^4 + 2*(-400491531 + 531251340*la - 
            96378977*la^2 + 575762*la^3)*m^2 - 3*(894753 - 676636*la + 
            51100*la^2)*m^4 + 96*(-3 + la)*m^6))*M^4 + 
      18*J^2*(158418713037 + 63270450936*En^4 - 108623137828*la + 
        9085296312*la^2 + (2836428594 - 805080088*la)*m^2 + 1905936*m^4 - 
        21*En^2*(13324384067 - 4704907204*la + 191180778*m^2))*M^6 + 
      694031652486*M^8)*rp[t]^16 - 
    J^8*(J^8*la^2*(16*En^8*(1302 - 143*la - 262*la^2 + 7*la^3 + 
          (560 + 128*la - 12*la^2)*m^2 + 4*(7 + la)*m^4) - 
        16*En^6*(169878 - 80311*la - 3764*la^2 + 177*la^3 - 
          2*(-37394 + 2838*la + 73*la^2)*m^2 + 4172*m^4 + 12*m^6) - 
        72*En^2*(1568539 - 759783*la + 32343*la^2 + 34*la^3 + 
          (478366 - 45156*la + 169*la^2)*m^2 - 98*(-132 + la)*m^4 + 14*m^6) + 
        3*(19030347 - 8250820*la + 465250*la^2 - 2710*la^3 + 
          (3874703 - 328420*la + 1910*la^2)*m^2 - 98*(-621 + 5*la)*m^4 + 
          42*m^6) + 4*En^4*(9607353 - 4875240*la + 108182*la^2 + 2607*la^3 - 
          8*(-454881 + 46390*la + 15*la^2)*m^2 - 28*(-5424 + 35*la)*m^4 + 
          252*m^6)) + J^6*(24192*En^10*la^2 - 
        576*En^8*(3*(-696 + 21048*la - 30335*la^2 + 502*la^3) + 
          (-1080 + 13074*la - 5737*la^2)*m^2 + 6*(-12 + 41*la)*m^4) + 
        48*En^6*(-2429730 + 42226578*la - 63187067*la^2 + 6520682*la^3 + 
          29490*la^4 + 6*(-189615 + 1599360*la - 1044547*la^2 + 14140*la^3)*
           m^2 - 18*(3705 - 13277*la + 2849*la^2)*m^4 + 30*(-3 + 4*la)*m^6) + 
        16*En^4*(64026207 - 1070895519*la + 1750166454*la^2 - 
          257662866*la^3 + 2042686*la^4 + 6900*la^5 - 
          7*(-3414555 + 32145222*la - 27182390*la^2 + 724240*la^3 + 24*la^4)*
           m^2 - 7*(-130599 + 642120*la - 258760*la^2 + 280*la^3)*m^4 + 
          105*(9 - 24*la + 4*la^2)*m^6) - 6*En^2*(358574013 - 5970684051*la + 
          10651250993*la^2 - 1841941996*la^3 + 31452798*la^4 + 7344*la^5 + 
          7*(15157665 - 155916486*la + 158328244*la^2 - 5231234*la^3 + 
            6192*la^4)*m^2 - 42*(-58041 + 368313*la - 216923*la^2 + 504*la^3)*
           m^4 + 630*(3 - 12*la + 4*la^2)*m^6) + 
        3*(265173615 - 4613433828*la + 8957337334*la^2 - 1649716650*la^3 + 
          43561734*la^4 - 106560*la^5 + 7*(7361811 - 84998046*la + 
            101993726*la^2 - 3786066*la^3 + 9216*la^4)*m^2 - 
          14*(-49995 + 391224*la - 303758*la^2 + 1008*la^3)*m^4 + 
          126*(3 - 16*la + 8*la^2)*m^6))*M^2 + 
      6*J^4*(145152*En^10 + 288*En^8*(1231395 - 1458019*la + 93354*m^2) - 
        24*En^6*(452618069 - 901514434*la + 176009350*la^2 + 
          (47933358 - 35756848*la)*m^2 + 379450*m^4) + 
        28*En^4*(2535193773 - 6526318356*la + 2302946376*la^2 - 
          71723555*la^3 + (298947306 - 394235360*la + 49113092*la^2)*m^2 - 
          140*(-17229 + 8879*la)*m^4 + 360*m^6) + 
        7*En^2*(-17282684763 + 54868288752*la - 26946667328*la^2 + 
          1535272176*la^3 - 6441056*la^4 + 2*(-955874007 + 1774988454*la - 
            418835614*la^2 + 3062360*la^3)*m^2 - 2*(6590889 - 6718884*la + 
            639884*la^2)*m^4 + 288*(-9 + 4*la)*m^6) + 
        7*(2*(2842565688 - 10629215799*la + 6540696515*la^2 - 
            500857505*la^3 + 5006278*la^4 - 3032*la^5) + 
          (465364377 - 1118741799*la + 385253629*la^2 - 5139068*la^3 + 
            3088*la^4)*m^2 + (2391021 - 3643572*la + 692076*la^2 - 560*la^3)*
           m^4 + 16*(27 - 24*la + 2*la^2)*m^6))*M^4 - 
      6*J^2*(-561423181977 + 56260214064*En^6 + 707912370060*la - 
        139623240016*la^2 + 2669135392*la^3 + 
        (-17216582589 + 11883041196*la - 879626948*la^2)*m^2 + 
        48*(-539739 + 161428*la)*m^4 - 864*m^6 - 
        504*En^4*(1223514060 - 561797601*la + 29597990*m^2) + 
        42*En^2*(34136527437 - 29752358383*la + 3056950221*la^2 + 
          (1141986651 - 398408544*la)*m^2 + 1410360*m^4))*M^6 - 
      108*(-33631305283 + 37880927775*En^2 + 9624747526*la - 215860941*m^2)*
       M^8)*rp[t]^17 + J^6*M*(-2*J^8*la*(4032*En^10*la*(1 + la) - 
        96*En^8*(-4176 + 39977*la - 1414*la^2 - 789*la^3 + 
          (-2160 + 7132*la + 286*la^2)*m^2 + 24*(-6 + 5*la)*m^4) + 
        16*En^6*(-2464803 + 12436415*la - 2702025*la^2 - 40486*la^3 + 
          730*la^4 - (1152225 - 2608312*la + 93800*la^2 + 504*la^3)*m^2 + 
          18*(-3739 + 3367*la)*m^4 + 30*(-3 + la)*m^6) + 
        6*En^2*(-124668180 + 568642669*la - 159056943*la^2 + 4307737*la^3 + 
          1836*la^4 + 7*(-5226810 + 13824640*la - 739305*la^2 + 1548*la^3)*
           m^2 - 42*(19665 - 31283*la + 126*la^2)*m^4 + 630*(-1 + la)*m^6) - 
        8*En^4*(-44152101 + 206084556*la - 54505806*la^2 + 768550*la^3 + 
          6900*la^4 - 7*(2339955 - 5750548*la + 295318*la^2 + 24*la^3)*m^2 - 
          7*(88182 - 108821*la + 280*la^2)*m^4 + 210*(-3 + 2*la)*m^6) + 
        3*(93518082 - 437190340*la + 119958299*la^2 - 4626407*la^3 + 
          17760*la^4 - 7*(-2556912 + 7531661*la - 409571*la^2 + 1536*la^3)*
           m^2 + 14*(17007 - 33334*la + 168*la^2)*m^4 - 42*(-3 + 4*la)*
           m^6)) - 6*J^6*(145152*En^10*la - 96*En^8*(689184 - 2580927*la + 
          860530*la^2 + (147321 - 189740*la)*m^2 + 2853*m^4) + 
        16*En^6*(88423824 - 487277023*la + 280376502*la^2 - 11001246*la^3 + 
          2*(9827895 - 24862104*la + 4894484*la^2)*m^2 + (471840 - 381980*la)*
           m^4 + 120*m^6) + 14*En^4*(-654355017 + 3658779492*la - 
          2712355714*la^2 + 204701688*la^3 - 707676*la^4 + 
          2*(-65834919 + 206942772*la - 71426132*la^2 + 683152*la^3)*m^2 - 
          10*(207603 - 325092*la + 42068*la^2)*m^4 + 240*(-3 + 2*la)*m^6) + 
        14*En^2*(990673677 - 6263439756*la + 5648373648*la^2 - 
          574849496*la^3 + 5498056*la^4 + 312*la^5 + 
          2*(86335641 - 331534464*la + 159891356*la^2 - 2682680*la^3 + 
            1260*la^4)*m^2 - 2*(-962163 + 2228850*la - 570500*la^2 + 
            504*la^3)*m^4 + 24*(27 - 36*la + 4*la^2)*m^6) - 
        7*(589986144 - 4140560250*la + 4326333071*la^2 - 514638552*la^3 + 
          8729142*la^4 - 12128*la^5 + (70333335 - 324499005*la + 
            201530260*la^2 - 4552086*la^3 + 6176*la^4)*m^2 + 
          (528147 - 1623534*la + 620942*la^2 - 1120*la^3)*m^4 + 
          16*(9 - 18*la + 4*la^2)*m^6))*M^2 + 
      12*J^4*(96294195639 + 788949504*En^8 - 207704563977*la + 
        74667706826*la^2 - 3210010440*la^3 + 14110860*la^4 + 
        (4570753059 - 6006011088*la + 1077706078*la^2 - 6021900*la^3)*m^2 + 
        4*(2918043 - 2195196*la + 164948*la^2)*m^4 - 288*(-3 + la)*m^6 - 
        144*En^6*(224714477 - 143637347*la + 8520755*m^2) + 
        84*En^4*(2259057441 - 2715297310*la + 364334026*la^2 + 
          (133191656 - 61450188*la)*m^2 + 304444*m^4) - 
        14*En^2*(22351162500 - 37836859215*la + 9521875840*la^2 - 
          223253152*la^3 + (1337221611 - 1188716745*la + 108106565*la^2)*
           m^2 - 24*(-158415 + 59522*la)*m^4 + 216*m^6))*M^4 + 
      9*J^2*(442673423205 + 272850996600*En^4 - 300886636064*la + 
        24814315032*la^2 + (6414699324 - 1805859008*la)*m^2 + 3077112*m^4 - 
        24*En^2*(40461387069 - 14099979980*la + 472053324*m^2))*M^6 + 
      692394750216*M^8)*rp[t]^18 - 
    J^6*(J^8*la^2*(192*En^8*(1244 - 163*la - 226*la^2 + 5*la^3 + 
          (530 + 103*la - 7*la^2)*m^2 + 2*(13 + la)*m^4) - 
        32*En^6*(471162 - 221350*la - 8570*la^2 + 365*la^3 - 
          84*(-2475 + 193*la + 3*la^2)*m^2 + 11403*m^4 + 15*m^6) + 
        3*(33290229 - 13983596*la + 773466*la^2 - 4440*la^3 + 
          21*(297353 - 23322*la + 128*la^2)*m^2 - 98*(-827 + 6*la)*m^4 + 
          42*m^6) + 16*En^4*(7952700 - 3970407*la + 92053*la^2 + 1725*la^3 - 
          7*(-410460 + 39031*la + 6*la^2)*m^2 - 35*(-2985 + 14*la)*m^4 + 
          105*m^6) - 6*En^2*(44197293 - 20796004*la + 864694*la^2 + 
          612*la^3 + 7*(1819385 - 153974*la + 516*la^2)*m^2 - 
          294*(-953 + 6*la)*m^4 + 210*m^6)) + 
      J^6*(290304*En^10*la^2 - 192*En^8*(-220428 + 1402269*la - 
          1535984*la^2 + 34297*la^3 + (-68220 + 295497*la - 99775*la^2)*m^2 + 
          3*(-984 + 1897*la)*m^4) + 16*En^6*(-12493440 + 374854332*la - 
          590078671*la^2 + 60477738*la^3 + 195002*la^4 + 
          (-8373600 + 81196836*la - 54382132*la^2 + 707504*la^3)*m^2 - 
          10*(57168 - 190023*la + 38563*la^2)*m^4 + 120*(-3 + 4*la)*m^6) + 
        28*En^4*(87936840 - 1387793691*la + 2203781826*la^2 - 
          314519886*la^3 + 2520050*la^4 + 6816*la^5 + 
          (29766960 - 272498298*la + 223312288*la^2 - 5201600*la^3 - 48*la^4)*
           m^2 - 10*(-87966 + 419763*la - 164960*la^2 + 140*la^3)*m^4 + 
          60*(9 - 24*la + 4*la^2)*m^6) - 42*En^2*(85328208 - 1411721871*la + 
          2494872353*la^2 - 417234838*la^3 + 6938368*la^4 + 936*la^5 + 
          (23796954 - 238891716*la + 236893696*la^2 - 6974940*la^3 + 
            7560*la^4)*m^2 - 6*(-69432 + 433955*la - 252189*la^2 + 504*la^3)*
           m^4 + 72*(3 - 12*la + 4*la^2)*m^6) + 
        21*(48922677 - 847744260*la + 1633233594*la^2 - 290297184*la^3 + 
          7526578*la^4 - 18192*la^5 + (8587179 - 97882206*la + 
            115953206*la^2 - 3996674*la^3 + 9264*la^4)*m^2 - 
          6*(-15408 + 119596*la - 92183*la^2 + 280*la^3)*m^4 + 
          12*(3 - 16*la + 8*la^2)*m^6))*M^2 - 
      6*J^4*(-38066705982 + 11691648*En^10 + 141371291172*la - 
        85999777164*la^2 + 6327443276*la^3 - 62162092*la^4 + 37280*la^5 + 
        (-2678882886 + 6371360127*la - 2168213741*la^2 + 26945020*la^3 - 
          15520*la^4)*m^2 + 4*(-2612457 + 3957684*la - 747572*la^2 + 
          560*la^3)*m^4 - 48*(27 - 24*la + 2*la^2)*m^6 - 
        288*En^8*(1609629 - 3928545*la + 170240*m^2) + 
        24*En^6*(1077995267 - 1954199651*la + 363694031*la^2 + 
          (98413710 - 70625120*la)*m^2 + 545950*m^4) - 
        28*En^4*(3886120422 - 9920406513*la + 3458257708*la^2 - 
          104256506*la^3 + (433257612 - 552300700*la + 66463812*la^2)*m^2 - 
          32*(-75804 + 38413*la)*m^4 + 216*m^6) - 
        7*En^2*(-20778555105 + 65468659986*la - 31699560680*la^2 + 
          1726216112*la^3 - 7047592*la^4 + (-2037034332 + 3706921338*la - 
            856021122*la^2 + 5582792*la^3)*m^2 - 4*(2544309 - 2567940*la + 
            242284*la^2)*m^4 + 144*(-9 + 4*la)*m^6))*M^4 - 
      3*J^2*(-788243065101 + 154138214304*En^6 + 984975585450*la - 
        191507672152*la^2 + 3503382700*la^3 + 
        (-19604171817 + 13408441788*la - 981898484*la^2)*m^2 + 
        12*(-1749423 + 520892*la)*m^4 - 432*m^6 - 
        504*En^4*(2653282744 - 1206654495*la + 55450712*m^2) + 
        12*En^2*(3*(69663674222 - 59915386191*la + 6029020433*la^2) + 
          (5714736708 - 1961218240*la)*m^2 + 4792824*m^4))*M^6 - 
      54*(-33726602794 + 47048407257*En^2 + 9551739576*la - 162322158*m^2)*
       M^8)*rp[t]^19 + 2*J^4*M*
     (-(J^8*la*(48384*En^10*la*(1 + la) - 96*En^8*(-141282 + 290517*la - 
           15230*la^2 - 3704*la^3 + (-44850 + 53227*la + 838*la^2)*m^2 + 
           (-1968 + 941*la)*m^4) + 16*En^6*(-4461480 + 36982772*la - 
           8384958*la^2 - 95793*la^3 + 1750*la^4 - 
           2*(1425135 - 3700816*la + 137116*la^2 + 497*la^3)*m^2 + 
           5*(-38376 + 31997*la)*m^4 + 40*(-3 + la)*m^6) + 
         21*En^2*(-59319036 + 268407379*la - 72749882*la^2 + 1911812*la^3 + 
           468*la^4 + 4*(-4098684 + 10545784*la - 497315*la^2 + 945*la^3)*
            m^2 - 14*(20136 - 31537*la + 108*la^2)*m^4 + 144*(-1 + la)*m^6) - 
         14*En^4*(-60436800 + 265602993*la - 67497126*la^2 + 966272*la^3 + 
           6816*la^4 - 4*(5103990 - 12123080*la + 540359*la^2 + 12*la^3)*
            m^2 - 10*(59292 - 70951*la + 140*la^2)*m^4 + 120*(-3 + 2*la)*
            m^6) - 21*(-17251794 + 80142667*la - 21188462*la^2 + 
           800608*la^3 - 3032*la^4 + (-2978913 + 8650160*la - 434104*la^2 + 
             1544*la^3)*m^2 - 28*(1122 - 2181*la + 10*la^2)*m^4 + 
           4*(-3 + 4*la)*m^6))) - 3*J^6*(-3968301609 + 27667153146*la - 
        28589932550*la^2 + 3260891452*la^3 - 54262222*la^4 + 74560*la^5 - 
        2*(204576219 - 932229942*la + 571523211*la^2 - 11969195*la^3 + 
          15520*la^4)*m^2 + 28*(-82917 + 253194*la - 96242*la^2 + 160*la^3)*
         m^4 - 48*(9 - 18*la + 4*la^2)*m^6 + 576*En^10*(46952 - 13313*la + 
          2170*m^2) - 96*En^8*(-3129726 - 3591059*la + 2329827*la^2 + 
          (43070 - 353980*la)*m^2 + 8272*m^4) + 
        24*En^6*(175971120 - 766079168*la + 400885499*la^2 - 14987828*la^3 + 
          10*(2795926 - 6789935*la + 1274623*la^2)*m^2 + (469790 - 365920*la)*
           m^4 + 60*m^6) + 56*En^4*(-253265013 + 1401682347*la - 
          1028179587*la^2 + 75458840*la^3 - 258110*la^4 + 
          (-49626072 + 149431437*la - 49527662*la^2 + 400004*la^3)*m^2 + 
          (-531729 + 816264*la - 103780*la^2)*m^4 + 36*(-3 + 2*la)*m^6) + 
        7*En^2*(2390937597 - 15038417442*la + 13393691420*la^2 - 
          1303012004*la^3 + 12085392*la^4 + 240*la^5 + 
          2*(188001981 - 704610066*la + 331659866*la^2 - 4920728*la^3 + 
            2136*la^4)*m^2 - 12*(-250203 + 572968*la - 145124*la^2 + 
            112*la^3)*m^4 + 24*(27 - 36*la + 4*la^2)*m^6))*M^2 + 
      3*J^4*(5716055520*En^8 + 2*(67976027838 - 145257250551*la + 
          51465447535*la^2 - 2112183885*la^3 + 9120630*la^4) + 
        (5246962632 - 6824445864*la + 1210432114*la^2 - 6296820*la^3)*m^2 + 
        7*(1356903 - 1015716*la + 75956*la^2)*m^4 - 144*(-3 + la)*m^6 - 
        288*En^6*(308664481 - 197294307*la + 11885533*m^2) + 
        84*En^4*(4911379386 - 5874093966*la + 775374553*la^2 - 
          32*(-8023612 + 3581595*la)*m^2 + 371056*m^4) - 
        6*En^2*(92006905452 - 153726786246*la + 37889137973*la^2 - 
          836355648*la^3 + 4*(1132990887 - 988309710*la + 88035662*la^2)*
           m^2 + (8674488 - 3233472*la)*m^4 + 288*m^6))*M^4 + 
      9*J^2*(111643095483 + 106964779968*En^4 - 75051559388*la + 
        6086320168*la^2 + (1213735998 - 338763176*la)*m^2 + 367416*m^4 - 
        3*En^2*(101649433637 - 34767294876*la + 879320166*m^2))*M^6 + 
      118171486440*M^8)*rp[t]^20 - 
    J^4*(J^8*la^2*(64*En^8*(80526 - 9628*la - 3662*la^2 + 56*la^3 + 
          (25170 + 1062*la - 63*la^2)*m^2 + 3*(333 + 5*la)*m^4) + 
        21*(6141093 - 2481392*la + 134100*la^2 - 758*la^3 + 
          (1037771 - 74514*la + 386*la^2)*m^2 - 14*(-763 + 5*la)*m^4 + 
          4*m^6) - 84*En^2*(5255175 - 2406062*la + 96631*la^2 + 39*la^3 + 
          (1423733 - 104720*la + 315*la^2)*m^2 - 14*(-1705 + 9*la)*m^4 + 
          12*m^6) - 32*En^6*(941760 - 669686*la - 21941*la^2 + 875*la^3 - 
          7*(-74830 + 7276*la + 71*la^2)*m^2 + 32340*m^4 + 20*m^6) + 
        28*En^4*(10792815 - 4993227*la + 118502*la^2 + 1704*la^3 - 
          2*(-1786320 + 146341*la + 6*la^2)*m^2 + (100110 - 350*la)*m^4 + 
          60*m^6)) + J^6*(2304*En^10*(-31080 + 47407*la - 3344*la^2 + 
          35*(-84 + 61*la)*m^2) - 576*En^8*(1046496 - 2019700*la - 
          787053*la^2 + 33950*la^3 + (54120 + 33010*la - 65385*la^2)*m^2 + 
          12*(-278 + 459*la)*m^4) + 48*En^6*(-30387240 + 365911866*la - 
          453597802*la^2 + 42133712*la^3 + 80387*la^4 + 
          15*(-407604 + 3850966*la - 2449237*la^2 + 26488*la^3)*m^2 - 
          10*(29853 - 94467*la + 18431*la^2)*m^4 + 30*(-3 + 4*la)*m^6) + 
        56*En^4*(69610590 - 1073697255*la + 1687542390*la^2 - 
          235910310*la^3 + 1867634*la^4 + 4020*la^5 + 
          2*(11866824 - 102415269*la + 79860539*la^2 - 1544050*la^3 + 
            12*la^4)*m^2 + (460890 - 2147304*la + 826640*la^2 - 560*la^3)*
           m^4 + 18*(9 - 24*la + 4*la^2)*m^6) - 
        42*En^2*(102912471 - 1703037021*la + 2982254635*la^2 - 
          477411274*la^3 + 7665746*la^4 + 360*la^5 + 
          (26648397 - 259490856*la + 250199040*la^2 - 6442946*la^3 + 
            6408*la^4)*m^2 - 4*(-82251 + 507213*la - 291263*la^2 + 504*la^3)*
           m^4 + 36*(3 - 12*la + 4*la^2)*m^6) + 
        3*(330604065 - 5694919080*la + 10858962184*la^2 - 1846187826*la^3 + 
          46854690*la^4 - 111840*la^5 + (50606847 - 568431204*la + 
            663803558*la^2 - 21089610*la^3 + 46560*la^4)*m^2 - 
          4*(-102285 + 787872*la - 603106*la^2 + 1680*la^3)*m^4 + 
          36*(3 - 16*la + 8*la^2)*m^6))*M^2 + 
      3*J^4*(54088219296 + 700507008*En^10 - 198932531178*la + 
        119251075779*la^2 - 8352164435*la^3 + 80451380*la^4 - 47760*la^5 + 
        (3104786313 - 7298711091*la + 2452674033*la^2 - 28247900*la^3 + 
          15600*la^4)*m^2 + (8544519 - 12872508*la + 2418724*la^2 - 
          1680*la^3)*m^4 + 24*(27 - 24*la + 2*la^2)*m^6 + 
        288*En^8*(37563217 - 27544890*la + 915260*m^2) - 
        48*En^6*(1488829525 - 2695603213*la + 502792933*la^2 + 
          (147997658 - 97886832*la)*m^2 + 450318*m^4) + 
        56*En^4*(4202023779 - 10778693079*la + 3716837760*la^2 - 
          106904051*la^3 + 8*(54130344 - 66248692*la + 7662991*la^2)*m^2 - 
          8*(-187203 + 93539*la)*m^4 + 72*m^6) + 
        2*En^2*(-129404697723 + 402782241312*la - 191090349828*la^2 + 
          9775162568*la^3 - 38590640*la^4 + 4*(-2638877715 + 4696645242*la - 
            1059816514*la^2 + 6094504*la^3)*m^2 - 
          12*(2925927 - 2926716*la + 273812*la^2)*m^4 + 288*(-9 + 4*la)*m^6))*
       M^4 - 3*J^2*(-400338654579 + 145061081280*En^6 + 494411656372*la - 
        94468374848*la^2 + 1638979360*la^3 + (-7473866703 + 5063112212*la - 
          366657516*la^2)*m^2 + 24*(-209641 + 62156*la)*m^4 - 48*m^6 - 
        288*En^4*(3691500674 - 1643704635*la + 59107018*m^2) + 
        6*En^2*(265961229141 - 224215372975*la + 21932942949*la^2 + 
          (5396719989 - 1820904944*la)*m^2 + 2700132*m^4))*M^6 - 
      54*(-11590685114 + 20146270161*En^2 + 3241205648*la - 36728838*m^2)*
       M^8)*rp[t]^21 + 
    J^2*M*(-2*J^8*la*(192*En^10*(-122430 + 53659*la + 1917*la^2 + 
          70*(-168 + 29*la)*m^2) - 96*En^8*(2058342 - 895355*la - 
          17752*la^2 - 9973*la^3 + 30*(3636 + 1034*la + 41*la^2)*m^2 + 
          8*(-834 + 343*la)*m^4) + 8*En^6*(-61851240 + 205744936*la - 
          35740500*la^2 - 262574*la^3 + 5376*la^4 - 
          5*(2516454 - 6257811*la + 192332*la^2 + 490*la^3)*m^2 + 
          100*(-6003 + 4760*la)*m^4 + 60*(-3 + la)*m^6) + 
        42*En^2*(-35797140 + 161624763*la - 42113691*la^2 + 1062749*la^3 + 
          90*la^4 + (-9163989 + 22796986*la - 926625*la^2 + 1602*la^3)*m^2 - 
          4*(27795 - 42938*la + 126*la^2)*m^4 + 36*(-1 + la)*m^6) - 
        28*En^4*(-47665908 + 205485156*la - 51713280*la^2 + 730394*la^3 + 
          4020*la^4 + (-16256502 + 36147377*la - 1305092*la^2 + 24*la^3)*
           m^2 + (-310212 + 362206*la - 560*la^2)*m^4 + 36*(-3 + 2*la)*m^6) - 
        3*(-116568450 + 536736597*la - 135353323*la^2 + 4992437*la^3 - 
          18640*la^4 + (-17530239 + 50087587*la - 2299835*la^2 + 7760*la^3)*
           m^2 - 4*(34725 - 66976*la + 280*la^2)*m^4 + 12*(-3 + 4*la)*m^6)) + 
      3*J^6*(5680184742 + 14999040*En^12 - 39207260244*la + 
        39921667673*la^2 - 4319670130*la^3 + 70320930*la^4 - 95520*la^5 + 
        (479709009 - 2156529777*la + 1303911016*la^2 - 25169550*la^3 + 
          31200*la^4)*m^2 + (1908783 - 5792106*la + 2188858*la^2 - 3360*la^3)*
         m^4 + 24*(9 - 18*la + 4*la^2)*m^6 + 1536*En^10*(599817 - 307337*la + 
          11550*m^2) + 96*En^8*(39693522 - 76193919*la + 15309935*la^2 + 
          (640170 - 1890800*la)*m^2 + 21966*m^4) - 
        32*En^6*(375963552 - 1592980608*la + 838776617*la^2 - 32141554*la^3 + 
          2*(35517825 - 76083036*la + 13028008*la^2)*m^2 - 
          6*(-99465 + 75386*la)*m^4 + 36*m^6) - 
        28*En^4*(-1067736987 + 6074464674*la - 4457358590*la^2 + 
          314978000*la^3 - 1045004*la^4 + 4*(-52069689 + 148721232*la - 
            47013520*la^2 + 310008*la^3)*m^2 - 24*(55551 - 83902*la + 
            10512*la^2)*m^4 + 48*(-3 + 2*la)*m^6) - 
        8*En^2*(3749422608 - 23359239270*la + 20411453786*la^2 - 
          1861642973*la^3 + 16620568*la^4 - 216*la^5 + 
          (498887667 - 1820206128*la + 834616800*la^2 - 10806352*la^3 + 
            4344*la^4)*m^2 - 12*(-217800 + 493689*la - 123872*la^2 + 84*la^3)*
           m^4 + 12*(27 - 36*la + 4*la^2)*m^6))*M^2 + 
      2*J^4*(208829126097 + 15749717760*En^8 - 440633336049*la + 
        153319870018*la^2 - 5946183720*la^3 + 25165900*la^4 + 
        (6052985829 - 7788351528*la + 1364851318*la^2 - 6582380*la^3)*m^2 + 
        (6856902 - 5108664*la + 380296*la^2)*m^4 - 48*(-3 + la)*m^6 - 
        864*En^6*(276346757 - 186725742*la + 10728289*m^2) + 
        144*En^4*(6919954227 - 8124735864*la + 1038499160*la^2 - 
          4*(-70463584 + 30408983*la)*m^2 + 222914*m^4) - 
        6*En^2*(178313987898 - 291575259435*la + 69767598415*la^2 - 
          1418292436*la^3 + (6525590508 - 5582346075*la + 486885047*la^2)*
           m^2 + (7377336 - 2730264*la)*m^4 + 108*m^6))*M^4 + 
      36*J^2*(19336159295 + 29866611285*En^4 - 12821783666*la + 
        1019343148*la^2 + (138243805 - 38244208*la)*m^2 + 19710*m^4 - 
        6*En^2*(11061025916 - 3688592792*la + 60850473*m^2))*M^6 + 
      49611716568*M^8)*rp[t]^22 + 
    J^2*(J^8*la^2*(3840*En^10*(2107 - 34*la - 6*la^2 + 7*(29 + la)*m^2) - 
        64*En^8*(-993066 + 6029*la - 11012*la^2 + 119*la^3 - 
          5*(9112 - 359*la + 21*la^2)*m^2 + 4*(839 + 5*la)*m^4) - 
        3*(41490675 - 15939644*la + 837916*la^2 - 4660*la^3 + 
          (6096503 - 396640*la + 1940*la^2)*m^2 - 140*(-337 + 2*la)*m^4 + 
          12*m^6) + 42*En^2*(12695451 - 5655510*la + 216512*la^2 + 30*la^3 + 
          (3174193 - 197146*la + 534*la^2)*m^2 - 28*(-1343 + 6*la)*m^4 + 
          12*m^6) - 56*En^4*(8473047 - 3929799*la + 91871*la^2 + 1005*la^3 + 
          2*(1414191 - 90262*la + 3*la^2)*m^2 + (52278 - 140*la)*m^4 + 
          18*m^6) + 16*En^6*(11009220 - 2844353*la - 65815*la^2 + 2688*la^3 - 
          35*(-65512 + 5385*la + 35*la^2)*m^2 + 100870*m^4 + 30*m^6)) + 
      J^6*(483840*En^12*(-84 + 61*la) + 1152*En^10*(-756000 + 1593282*la - 
          215753*la^2 + 840*(-24 + 37*la)*m^2) + 
        288*En^8*(-5338512 + 26474538*la - 13327724*la^2 + 197955*la^3 + 
          (208260 + 455970*la - 339790*la^2)*m^2 + 18*(-526 + 813*la)*m^4) - 
        48*En^6*(-49950864 + 519774852*la - 637400008*la^2 + 62770696*la^3 + 
          46033*la^4 + 4*(-3301821 + 24300465*la - 13465921*la^2 + 
            103936*la^3)*m^2 - 6*(43506 - 133209*la + 25277*la^2)*m^4 + 
          12*(-3 + 4*la)*m^6) - 28*En^4*(134737344 - 2273433993*la + 
          3667068402*la^2 - 503251998*la^3 + 3840506*la^4 + 6552*la^5 + 
          4*(13355523 - 107041011*la + 78652924*la^2 - 1211540*la^3 + 
            24*la^4)*m^2 - 4*(-147069 + 672249*la - 254480*la^2 + 140*la^3)*
           m^4 + 12*(9 - 24*la + 4*la^2)*m^6) + 
        6*En^2*(646268814 - 10676344059*la + 18404576285*la^2 - 
          2758930936*la^3 + 42398988*la^4 - 1296*la^5 + 
          4*(36476001 - 343374534*la + 321130684*la^2 - 7122722*la^3 + 
            6516*la^4)*m^2 - 12*(-96498 + 588087*la - 334145*la^2 + 504*la^3)*
           m^4 + 72*(3 - 12*la + 4*la^2)*m^6) - 
        3*(238632291 - 4068791688*la + 7645854944*la^2 - 1228019160*la^3 + 
          30407475*la^4 - 71640*la^5 + (30094128 - 332599956*la + 
            382443757*la^2 - 11124525*la^3 + 23400*la^4)*m^2 + 
          (169254 - 1294392*la + 984431*la^2 - 2520*la^3)*m^4 + 
          9*(3 - 16*la + 8*la^2)*m^6))*M^2 + 
      J^4*(7518828672*En^10 + 2*(-41946442893 + 152158314558*la - 
          89506967614*la^2 + 5898222130*la^3 - 55564110*la^4 + 32640*la^5) + 
        (-3618398466 + 8401869087*la - 2786539821*la^2 + 29603980*la^3 - 
          15680*la^4)*m^2 + 2*(-3098169 + 4643028*la - 868084*la^2 + 
          560*la^3)*m^4 - 8*(27 - 24*la + 2*la^2)*m^6 - 
        1728*En^8*(15674023 - 13066067*la + 1066272*m^2) + 
        144*En^6*(1157228491 - 2448017934*la + 480575978*la^2 - 
          18*(-8025653 + 4877136*la)*m^2 + 198638*m^4) - 
        48*En^4*(11918126259 - 30257971551*la + 10131959004*la^2 - 
          266622086*la^3 + 2*(493211427 - 579272686*la + 64388266*la^2)*m^2 - 
          32*(-56847 + 28073*la)*m^4 + 36*m^6) - 
        6*En^2*(-85099006683 + 258757841025*la - 118998052858*la^2 + 
          5573031852*la^3 - 21143500*la^4 + (-5167410201 + 8989136823*la - 
            1981375459*la^2 + 9958604*la^3)*m^2 + 
          (-10025451 + 9947868*la - 923636*la^2)*m^4 + 36*(-9 + 4*la)*m^6))*
       M^4 + 3*J^2*(-139976745291 + 109073307456*En^6 + 170298909792*la - 
        31866115264*la^2 + 518834184*la^3 + (-1715722431 + 1150879316*la - 
          82401836*la^2)*m^2 + 4*(-135417 + 39988*la)*m^4 - 
        36*En^4*(17031781452 - 7255315365*la + 168082684*m^2) + 
        6*En^2*(118061806618 - 96740437787*la + 9120773469*la^2 + 
          (1514647646 - 502525472*la)*m^2 + 336300*m^4))*M^6 + 
      54*(-2453907430 + 5354381067*En^2 + 675965544*la - 3786562*m^2)*M^8)*
     rp[t]^23 + 2*(J^8*la*(80640*En^12*(-168 + 29*la) + 
        384*En^10*(-751380 + 398738*la - 5125*la^2 + 420*(-48 + 19*la)*m^2) + 
        48*En^8*(-10765224 + 13360274*la - 125115*la^2 - 32395*la^3 + 
          10*(41064 + 28433*la + 186*la^2)*m^2 + 6*(-3156 + 1217*la)*m^4) - 
        8*En^6*(-99793404 + 294054896*la - 56274633*la^2 - 191467*la^3 + 
          5488*la^4 - 4*(6755922 - 12864607*la + 259448*la^2 + 483*la^3)*
           m^2 + 18*(-29132 + 22337*la)*m^4 + 24*(-3 + la)*m^6) - 
        3*En^2*(-450336024 + 2019971393*la - 493583936*la^2 + 11832604*la^3 - 
          648*la^4 + 4*(-25032354 + 60016851*la - 2065210*la^2 + 3258*la^3)*
           m^2 - 36*(21716 - 33145*la + 84*la^2)*m^4 + 144*(-1 + la)*m^6) + 
        14*En^4*(-92528244 + 439174287*la - 113488248*la^2 + 1531604*la^3 + 
          6552*la^4 + 4*(-9120627 + 18710765*la - 519692*la^2 + 24*la^3)*
           m^2 - 8*(49437 - 56606*la + 70*la^2)*m^4 + 24*(-3 + 2*la)*m^6) + 
        3*(-84110382 + 381912849*la - 90501590*la^2 + 3245840*la^3 - 
          11940*la^4 + (-10407426 + 29216703*la - 1217900*la^2 + 3900*la^3)*
           m^2 + (-57408 + 109921*la - 420*la^2)*m^4 + 3*(-3 + 4*la)*m^6))*
       M + J^6*(4457981727 - 329978880*En^12 - 30300888702*la + 
        30248587583*la^2 - 3062773190*la^3 + 48635815*la^4 - 65280*la^5 + 
        (282983319 - 1254009102*la + 747313541*la^2 - 13226955*la^3 + 
          15680*la^4)*m^2 + (695673 - 2098566*la + 788718*la^2 - 1120*la^3)*
         m^4 + 4*(9 - 18*la + 4*la^2)*m^6 + 1728*En^10*
         (-2207900 + 1431973*la + 16380*m^2) + 
        288*En^8*(17766060 - 32314037*la + 7959581*la^2 + 
          (1914768 - 2167376*la)*m^2 + 6876*m^4) - 
        48*En^6*(187272555 - 1273913072*la + 783657805*la^2 - 31795270*la^3 + 
          2*(38887926 - 73771431*la + 11499467*la^2)*m^2 + 
          (268347 - 199384*la)*m^4 + 6*m^6) - 
        24*En^4*(-1490053320 + 8609607993*la - 6186978248*la^2 + 
          400724808*la^3 - 1265988*la^4 + 2*(-124435944 + 337334019*la - 
            101725720*la^2 + 536600*la^3)*m^2 + (-819855 + 1221864*la - 
            151228*la^2)*m^4 + 12*(-3 + 2*la)*m^6) - 
        3*En^2*(10071655119 - 61180544679*la + 51735271248*la^2 - 
          4289131488*la^3 + 36596928*la^4 - 1536*la^5 + 
          (1001116035 - 3553673142*la + 1586704006*la^2 - 17755432*la^3 + 
            6624*la^4)*m^2 + (3009645 - 6760368*la + 1682044*la^2 - 
            1008*la^3)*m^4 + 6*(27 - 36*la + 4*la^2)*m^6))*M^3 + 
      3*J^4*(24614898894 + 836934336*En^8 - 51079833357*la + 
        17383479326*la^2 - 629466804*la^3 + 2605228*la^4 + 
        (467346150 - 594679312*la + 102940278*la^2 - 458572*la^3)*m^2 + 
        (247005 - 183212*la + 13580*la^2)*m^4 - 
        288*En^6*(213807056 - 138920084*la + 5440117*m^2) + 
        6*En^4*(33247937706 - 37051674062*la + 4458280421*la^2 + 
          (823831600 - 344538288*la)*m^2 + 264380*m^4) + 
        En^2*(-162199761258 + 256693292958*la - 59003982911*la^2 + 
          1074628728*la^3 + (-3722845410 + 3124105180*la - 266912524*la^2)*
           m^2 + 4*(-462075 + 169904*la)*m^4))*M^5 + 
      9*J^2*(8270169737 + 21429154296*En^4 - 5394230836*la + 419115768*la^2 + 
        (28701514 - 7868952*la)*m^2 - 3*En^2*(12011108829 - 3881744380*la + 
          30333526*m^2))*M^7 + 2422284750*M^9)*rp[t]^24 + 
    (-(J^8*la^2*(161280*En^12*(29 + la) - 7680*En^10*(-12488 + 55*la - 
           15*la^2 + 14*(-23 + la)*m^2) + 96*En^8*(1810244 + 28865*la - 
           12850*la^2 + 105*la^3 - 10*(5716 - 108*la + 7*la^2)*m^2 + 
           2*(1583 + 5*la)*m^4) + 3*(29924409 - 10727920*la + 545955*la^2 - 
           2985*la^3 + 3*(1204119 - 70340*la + 325*la^2)*m^2 - 
           7*(-2783 + 15*la)*m^4 + 3*m^6) - 32*En^6*(8688156 - 2428625*la - 
           27793*la^2 + 1372*la^3 - 21*(-113194 + 5038*la + 23*la^2)*m^2 + 
           43974*m^4 + 6*m^6) - 24*En^2*(19993338 - 8437436*la + 
           303731*la^2 - 27*la^3 + (4321009 - 221812*la + 543*la^2)*m^2 - 
           126*(-262 + la)*m^4 + 6*m^6) + 28*En^4*(16606923 - 8966691*la + 
           197552*la^2 + 1638*la^3 + 8*(787686 - 36607*la + 3*la^2)*m^2 + 
           (66552 - 140*la)*m^4 + 12*m^6))) - 
      J^6*(380298159 - 6370777866*la + 11724168889*la^2 - 1749832995*la^3 + 
        42129865*la^4 - 97920*la^5 + 1935360*En^12*(-216 + 227*la) + 
        (36047331 - 391625937*la + 443072749*la^2 - 11730845*la^3 + 
          23520*la^4)*m^2 + (124101 - 942744*la + 712642*la^2 - 1680*la^3)*
         m^4 + 3*(3 - 16*la + 8*la^2)*m^6 - 1152*En^10*
         (1649340 - 4396200*la + 687199*la^2 + 420*(-75 + 79*la)*m^2) - 
        192*En^8*(-9685764 + 35115396*la - 18339677*la^2 + 706266*la^3 + 
          (-676260 + 3891546*la - 1121593*la^2)*m^2 + 6*(-1536 + 2291*la)*
           m^4) + 16*En^6*(2690010 + 815782542*la - 1629254795*la^2 + 
          196513122*la^3 - 22802*la^4 + 2*(-26302491 + 158552772*la - 
            77012383*la^2 + 397124*la^3)*m^2 - 2*(180045 - 538701*la + 
            100177*la^2)*m^4 + 6*(-3 + 4*la)*m^6) + 
        16*En^4*(167738769 - 3187771542*la + 5169863418*la^2 - 
          656623770*la^3 + 4723198*la^4 + 6372*la^5 + 
          (68187393 - 509577834*la + 353340026*la^2 - 4240360*la^3 + 
            120*la^4)*m^2 + (367011 - 1652064*la + 616840*la^2 - 280*la^3)*
           m^4 + 3*(9 - 24*la + 4*la^2)*m^6) - 
        6*En^2*(444564801 - 7152697236*la + 11913618574*la^2 - 
          1609150944*la^3 + 23469067*la^4 - 2304*la^5 + 
          (75526893 - 687134601*la + 623116358*la^2 - 11777667*la^3 + 
            9936*la^4)*m^2 - 3*(-112173 + 676577*la - 380835*la^2 + 504*la^3)*
           m^4 + 9*(3 - 12*la + 4*la^2)*m^6))*M^2 + 
      J^4*(-30082949082 - 1840548096*En^10 + 107074540782*la - 
        61506848866*la^2 + 3760468150*la^3 - 34557068*la^4 + 20080*la^5 + 
        (-847006299 + 1941902343*la - 635468901*la^2 + 6202652*la^3 - 
          3152*la^4)*m^2 + (-672489 + 1002852*la - 186620*la^2 + 112*la^3)*
         m^4 - 1728*En^8*(-9990965 - 3118625*la + 1063294*m^2) + 
        144*En^6*(893920843 - 1882038306*la + 348155134*la^2 + 
          (77927754 - 44289744*la)*m^2 + 36398*m^4) + 
        12*En^4*(-30209181903 + 71899935726*la - 22470786856*la^2 + 
          503621393*la^3 - 2*(745325895 - 843293552*la + 90362466*la^2)*m^2 + 
          4*(-272163 + 133085*la)*m^4) + 6*En^2*(39925696755 - 
          116765931756*la + 51358912840*la^2 - 2130927328*la^3 + 
          7725120*la^4 + (1502797743 - 2556570446*la + 550774646*la^2 - 
            2405496*la^3)*m^2 + (1263993 - 1245220*la + 114828*la^2)*m^4))*
       M^4 + 3*J^2*(-30296989053 + 62723565648*En^6 + 36189033084*la - 
        6607879248*la^2 + 99782816*la^3 - 3*(59837643 - 39738484*la + 
          2812956*la^2)*m^2 - 72*En^4*(3207695496 - 1281655085*la + 
          13175934*m^2) + 6*En^2*(32892011155 - 25999367141*la + 
          2344654303*la^2 + (191299397 - 62450976*la)*m^2))*M^6 + 
      54*(-242094720 + 666209235*En^2 + 65549596*la)*M^8)*rp[t]^25 + 
    (J^6*la*(-267838056 + 1189356561*la - 259456405*la^2 + 9011565*la^3 - 
        32640*la^4 - 645120*En^12*(-432 + 113*la) + 
        (-24887739 + 68585127*la - 2578415*la^2 + 7840*la^3)*m^2 - 
        2*(42057 - 79982*la + 280*la^2)*m^4 + (-6 + 8*la)*m^6 + 
        1920*En^10*(660996 - 432167*la - 3659*la^2 + 84*(-150 + 41*la)*m^2) + 
        192*En^8*(-6362676 + 6027527*la - 330046*la^2 - 16103*la^3 + 
          (-458400 + 678076*la + 298*la^2)*m^2 + 16*(-384 + 143*la)*m^4) - 
        32*En^6*(1363959 + 88657477*la - 31708431*la^2 - 18846*la^3 + 
          1862*la^4 - (17822739 - 27455974*la + 338464*la^2 + 476*la^3)*m^2 + 
          2*(-60237 + 45115*la)*m^4 + 2*(-3 + la)*m^6) - 
        6*En^2*(-310076496 + 1343096963*la - 292469219*la^2 + 6593121*la^3 - 
          1152*la^4 + (-51707697 + 119483993*la - 3440395*la^2 + 4968*la^3)*
           m^2 + (-226974 + 342790*la - 756*la^2)*m^4 + 18*(-1 + la)*m^6) + 
        16*En^4*(-116601255 + 618626154*la - 153070290*la^2 + 1919014*la^3 + 
          6372*la^4 + (-46409067 + 88295914*la - 1842670*la^2 + 120*la^3)*
           m^2 - 5*(49302 - 55573*la + 56*la^2)*m^4 + 6*(-3 + 2*la)*m^6))*M + 
      J^4*(3254909913 + 1974067200*En^12 - 21635708022*la + 
        21047488491*la^2 - 1961440032*la^3 + 30292742*la^4 - 40160*la^5 + 
        (134168949 - 585840513*la + 344001004*la^2 - 5558294*la^3 + 
          6304*la^4)*m^2 + (151731 - 455190*la + 170198*la^2 - 224*la^3)*
         m^4 - 13824*En^10*(-24916 + 92049*la + 5530*m^2) + 
        576*En^8*(3*(-8282082 + 5996875*la + 1114628*la^2) + 
          (2411311 - 2146652*la)*m^2 + 1655*m^4) + 
        96*En^6*(-112260348 + 988514085*la - 590107502*la^2 + 20560490*la^3 - 
          2*(22759881 - 39595344*la + 5731988*la^2)*m^2 + 
          12*(-4157 + 3043*la)*m^4) + 12*En^4*(4082625459 - 21615460074*la + 
          14305899027*la^2 - 773205916*la^3 + 2291578*la^4 + 
          (392160807 - 1015706700*la + 293581564*la^2 - 1229536*la^3)*m^2 + 
          (495729 - 730692*la + 89516*la^2)*m^4) + 
        4*En^2*(-7391575917 + 42759234882*la - 34366491412*la^2 + 
          2487422218*la^3 - 20152268*la^4 + 1356*la^5 - 
          3*(149044731 - 515263432*la + 224224604*la^2 - 2155544*la^3 + 
            748*la^4)*m^2 + (-573327 + 1277502*la - 315476*la^2 + 168*la^3)*
           m^4))*M^3 + 6*J^2*(5406826617 + 3723176448*En^8 - 10988236245*la + 
        3642172058*la^2 - 121486632*la^3 + 490588*la^4 + 
        (49344723 - 62086968*la + 10615686*la^2 - 43548*la^3)*m^2 - 
        144*En^6*(285551666 - 155420437*la + 2318182*m^2) - 
        12*En^4*(-6678293145 + 6870699562*la - 760057790*la^2 + 
          28*(-2362890 + 961333*la)*m^2) - 6*En^2*(7780642892 - 
          11801960487*la + 2582273526*la^2 - 40895960*la^3 + 
          (79589365 - 65579109*la + 5493953*la^2)*m^2))*M^5 + 
      9*(1651737777 + 7297505676*En^4 - 1056994248*la + 80035144*la^2 + 
        12*En^2*(-765755199 + 238956548*la))*M^7)*rp[t]^26 + 
    (-(J^6*la^2*(47587581 - 15494810*la + 759625*la^2 - 4080*la^3 - 
         215040*En^12*(217 + la) + 7*(615737 - 32055*la + 140*la^2)*m^2 + 
         (14259 - 70*la)*m^4 + m^6 - 7680*En^10*(27314 + 194*la - 30*la^2 + 
           21*(-24 + la)*m^2) + 64*En^8*(3159672 - 64547*la - 20158*la^2 + 
           133*la^3 + (246930 + 747*la - 63*la^2)*m^2 + 6*(513 + la)*m^4) - 
         32*En^6*(173844 - 3103468*la - 10252*la^2 + 931*la^3 - 
           14*(-219091 + 5077*la + 17*la^2)*m^2 + 20181*m^4 + m^6) + 
         16*En^4*(21319617 - 12667200*la + 253453*la^2 + 1593*la^3 + 
           (7962822 - 263695*la + 30*la^2)*m^2 + (41433 - 70*la)*m^4 + 
           3*m^6) - 6*En^2*(55042803 - 20417096*la + 682441*la^2 - 192*la^3 + 
           (8895257 - 372797*la + 828*la^2)*m^2 - 7*(-5471 + 18*la)*m^4 + 
           3*m^6))) + J^4*(5806080*En^12*(-174 + 227*la) - 
        2304*En^10*(-152040 - 118004*la + 97453*la^2 + 140*(-75 + 157*la)*
           m^2) + 192*En^8*(13157928 - 49346931*la + 5380484*la^2 + 
          931513*la^3 + (-1316700 + 4862787*la - 1093181*la^2)*m^2 + 
          3*(-756 + 1103*la)*m^4) + 48*En^6*(-20732040 - 172845288*la + 
          419372767*la^2 - 44726002*la^3 + 27930*la^4 - 
          4*(-2875320 + 15392145*la - 6806613*la^2 + 23540*la^3)*m^2 + 
          18*(1260 - 3707*la + 679*la^2)*m^4) + 
        3*(-47406744 + 772582458*la - 1381586687*la^2 + 187780900*la^3 - 
          4380907*la^4 + 10040*la^5 + (-2892960 + 30883579*la - 
            34367547*la^2 + 824251*la^3 - 1576*la^4)*m^2 + 
          7*(-648 + 4892*la - 3677*la^2 + 8*la^3)*m^4) + 
        4*En^4*(-529353468 + 8676285516*la - 12645221841*la^2 + 
          1301251209*la^3 - 8670899*la^4 - 9240*la^5 + 
          (-113434020 + 800154141*la - 527589640*la^2 + 4905320*la^3 - 
            168*la^4)*m^2 + (-224532 + 998175*la - 368480*la^2 + 140*la^3)*
           m^4) - 2*En^2*(-694203804 + 10443891573*la - 16372083029*la^2 + 
          1891917413*la^3 - 25990844*la^4 + 4068*la^5 + 
          (-69483960 + 612146226*la - 539009264*la^2 + 8630114*la^3 - 
            6732*la^4)*m^2 + (-129276 + 772683*la - 431333*la^2 + 504*la^3)*
           m^4))*M^2 - J^2*(7408641024*En^10 + 
        4*(1682894682 - 5845806279*la + 3262689011*la^2 - 182191025*la^3 + 
          1629029*la^4 - 936*la^5) + (90389880 - 204597945*la + 
          66059347*la^2 - 590468*la^3 + 288*la^4)*m^2 + 
        1728*En^8*(6049639 - 9472259*la + 362624*m^2) + 
        72*En^6*(-1558950838 + 2433904833*la - 365260225*la^2 + 
          4*(-8682959 + 4700864*la)*m^2) + 12*En^4*(13287251232 - 
          28435410309*la + 8033796844*la^2 - 141747010*la^3 + 
          36*(6843614 - 7500175*la + 778433*la^2)*m^2) + 
        6*En^2*(-11989613622 + 33289365447*la - 13845699932*la^2 + 
          491325552*la^3 - 1693516*la^4 + (-196309788 + 327037923*la - 
            68952295*la^2 + 260892*la^3)*m^2))*M^4 + 
      18*(-511660480 + 2921423688*En^6 + 598194151*la - 106241452*la^2 + 
        1469422*la^3 + 6*En^4*(-1151751248 + 429891129*la) + 
        En^2*(4320462990 - 3280624761*la + 281722327*la^2))*M^6)*rp[t]^27 + 
    2*(-(J^4*la*(49989942 - 214785616*la + 42045361*la^2 - 1408448*la^3 + 
         5020*la^4 - 5806080*En^12*(-58 + 19*la) + 
         (2990430 - 8087175*la + 272768*la^2 - 788*la^3)*m^2 + 
         4*(1152 - 2177*la + 7*la^2)*m^4 + 1536*En^10*(-73815 - 17551*la + 
           1152*la^2 + 35*(-150 + 77*la)*m^2) - 
         96*En^8*(8888502 - 7620731*la - 592444*la^2 - 9574*la^3 + 
           (-883890 + 826257*la - 14*la^2)*m^2 + (-1512 + 551*la)*m^4) + 
         16*En^6*(20358252 + 61208470*la - 23346420*la^2 + 22819*la^3 + 
           810*la^4 - 2*(5820795 - 7902306*la + 61340*la^2 + 67*la^3)*m^2 + 
           (-22752 + 16751*la)*m^4) + 2*En^4*(368755254 - 1645356699*la + 
           314065719*la^2 - 3584180*la^3 - 9240*la^4 - 
           14*(-5497965 + 9832231*la - 153991*la^2 + 12*la^3)*m^2 + 
           7*(21528 - 23959*la + 20*la^2)*m^4) - 
         En^2*(483309090 - 1935148769*la + 349794383*la^2 - 7350122*la^3 + 
           2034*la^4 + (47457450 - 105937400*la + 2538410*la^2 - 3366*la^3)*
            m^2 + (87120 - 130361*la + 252*la^2)*m^4))*M) - 
      J^2*(804142080*En^12 + 576*En^10*(-6820356 + 4251395*la + 62790*m^2) + 
        288*En^8*(4410042 + 14050185*la - 5914625*la^2 + 
          (-902886 + 729820*la)*m^2) + 12*En^6*(827726976 - 3303882492*la + 
          1413321323*la^2 - 30968064*la^3 + 18*(2380680 - 3907959*la + 
            536239*la^2)*m^2) + 4*En^4*(-3102916896 + 14040742716*la - 
          8172418887*la^2 + 333469664*la^3 - 917298*la^4 + 
          3*(-33541668 + 83673249*la - 23330474*la^2 + 77724*la^3)*m^2) - 
        3*(124361676 - 802635483*la + 756206689*la^2 - 63664502*la^3 + 
          953467*la^4 - 1248*la^5 + 3*(805608 - 3465984*la + 2005337*la^2 - 
            29477*la^3 + 32*la^4)*m^2) + En^2*(4706319780 - 25456153887*la + 
          19163058470*la^2 - 1160472838*la^3 + 8877528*la^4 - 792*la^5 + 
          2*(59693868 - 201391209*la + 85562597*la^2 - 704764*la^3 + 
            228*la^4)*m^2))*M^3 + 3*(557768292 + 3511623888*En^8 - 
        1105985483*la + 355722324*la^2 - 10775070*la^3 + 42360*la^4 + 
        144*En^6*(-91017749 + 42415279*la) + 
        6*En^4*(2564591982 - 2430007570*la + 245952355*la^2) + 
        En^2*(-6362382996 + 9206397258*la - 1908057175*la^2 + 25559792*la^3))*
       M^5)*rp[t]^28 + (J^4*la^2*(-17722089 + 645120*En^12*(-173 + la) + 
        5065104*la - 238024*la^2 + 1255*la^3 + 
        (-1033550 + 47683*la - 197*la^2)*m^2 + 7*(-223 + la)*m^4 + 
        15360*En^10*(2338 + 41*la - 15*la^2 + 7*(26 + la)*m^2) + 
        32*En^6*(-2912448 - 2561030*la + 1865*la^2 + 405*la^3 + 
          (1981490 - 26352*la - 67*la^2)*m^2 + 3808*m^4) + 
        28*En^4*(-9516435 + 3893574*la - 69101*la^2 - 330*la^3 + 
          (-1876650 + 44669*la - 6*la^2)*m^2 + 5*(-723 + la)*m^4) - 
        64*En^8*(-4410552 - 257984*la - 12418*la^2 + 70*la^3 + 
          (451550 + 134*la - 21*la^2)*m^2 + (757 + la)*m^4) - 
        2*En^2*(-85395114 + 24970962*la - 766957*la^2 + 339*la^3 + 
          (-8137640 + 277304*la - 561*la^2)*m^2 + 14*(-1049 + 3*la)*m^4)) - 
      J^2*(3870720*En^12*(-183 + 277*la) + 2304*En^10*(843570 - 2270547*la + 
          344053*la^2 + 105*(-126 + 199*la)*m^2) + 
        576*En^8*(-2120580 + 2561496*la + 3289671*la^2 - 199394*la^3 + 
          (185220 - 605214*la + 123035*la^2)*m^2) + 
        24*En^6*(-40389300 + 580796178*la - 630833239*la^2 + 35345242*la^3 - 
          31267*la^4 + (-5794740 + 28888914*la - 11996573*la^2 + 28736*la^3)*
           m^2) + 8*En^4*(175514850 - 2165869341*la + 2634659481*la^2 - 
          192064491*la^3 + 1172085*la^4 + 986*la^5 + 
          3*(3373650 - 22746651*la + 14392341*la^2 - 104250*la^3 + 4*la^4)*
           m^2) + 3*(11234160 - 176330856*la + 304002920*la^2 - 
          36788009*la^3 + 828813*la^4 - 1872*la^5 + 
          (317520 - 3331128*la + 3646365*la^2 - 78929*la^3 + 144*la^4)*m^2) - 
        2*En^2*(241133760 - 3300284088*la + 4773793880*la^2 - 
          447635695*la^3 + 5756517*la^4 - 1188*la^5 + 
          (9525600 - 81520128*la + 69866032*la^2 - 945773*la^3 + 684*la^4)*
           m^2))*M^2 + (-710262432 + 4123204992*En^10 + 2396764428*la - 
        1294083563*la^2 + 64925103*la^3 - 563416*la^4 + 320*la^5 + 
        2160*En^8*(-10812647 + 6698336*la) + 
        72*En^6*(598800804 - 756992767*la + 94140187*la^2) + 
        12*En^4*(-2787346521 + 5379561579*la - 1370366608*la^2 + 
          17989013*la^3) + 12*En^2*(856406277 - 2246176866*la + 
          878789077*la^2 - 25847118*la^3 + 84356*la^4))*M^4)*rp[t]^29 + 
    (J^2*la*(-23625000 + 97182100*la - 16596217*la^2 + 534043*la^3 - 
        1872*la^4 - 645120*En^12*(-732 + 277*la) + 
        (-655200 + 1739171*la - 52429*la^2 + 144*la^3)*m^2 - 
        384*En^10*(3382050 - 2236795*la - 3673*la^2 + 1890*(-28 + 11*la)*
           m^2) - 192*En^8*(-4221630 + 636445*la + 438020*la^2 + 3119*la^3 + 
          (372120 - 306012*la + 22*la^2)*m^2) + 
        4*En^4*(-241947090 + 791115576*la - 95872812*la^2 + 984274*la^3 + 
          1972*la^4 + 3*(-4567080 + 7782565*la - 92544*la^2 + 8*la^3)*m^2) + 
        8*En^6*(84137130 - 327147038*la + 39295782*la^2 - 67838*la^3 - 
          820*la^4 + (11700360 - 14725543*la + 76028*la^2 + 66*la^3)*m^2) - 
        2*En^2*(-167121360 + 601235618*la - 84246367*la^2 + 1638645*la^3 - 
          594*la^4 + (-6491520 + 14047831*la - 279965*la^2 + 342*la^3)*m^2))*
       M + 2*(40535640 + 155554560*En^12 - 252557406*la + 229264661*la^2 - 
        17105963*la^3 + 247718*la^4 - 320*la^5 + 6912*En^10*
         (-220205 + 201591*la) + 72*En^8*(58488000 - 110777069*la + 
          17974123*la^2) + 24*En^6*(-213037740 + 618681369*la - 
          206954407*la^2 + 2555984*la^3) + 6*En^4*(490565700 - 
          1936124835*la + 994675017*la^2 - 28795936*la^3 + 73034*la^4) + 
        2*En^2*(-357828840 + 1801752768*la - 1263707838*la^2 + 
          61779925*la^3 - 444272*la^4 + 48*la^5))*M^3)*rp[t]^30 + 
    2*((-1 + En^2)*J^2*la^2*(39352320*En^10 + 1920*En^8*(-35539 - 226*la - 
          30*la^2 + 7*(127 + la)*m^2) - 6*En^2*(4534740 - 941538*la + 
          24955*la^2 + 3*la^3 + 8*(21945 - 594*la + la^2)*m^2) + 
        96*En^6*(-7*(5650 - 10493*la - 112*la^2 + la^3) + 
          (-44940 + 141*la + la^2)*m^2) + 3*(695450 - 168191*la + 7540*la^2 - 
          39*la^3 + (18830 - 767*la + 3*la^2)*m^2) - 
        4*En^4*(-14451990 + 2936005*la - 30277*la^2 - 242*la^3 + 
          3*(-300370 + 4413*la + 3*la^2)*m^2)) + 
      (1693440*En^12*(-36 + 61*la) + 576*En^10*(476280 - 1779210*la + 
          416671*la^2) + 24*En^8*(-20480040 + 119113758*la - 58691280*la^2 + 
          591635*la^3) + 12*En^6*(36673560 - 291411282*la + 221767019*la^2 - 
          6071734*la^3 + 6041*la^4) + En^2*(40007520 - 497931912*la + 
          661806090*la^2 - 48353246*la^3 + 579310*la^4 - 144*la^5) + 
        3*(-635040 + 9530232*la - 15742710*la^2 + 1658059*la^3 - 35955*la^4 + 
          80*la^5) - 2*En^4*(100018800 - 1014535764*la + 1053603177*la^2 - 
          51002941*la^3 + 283291*la^4 + 188*la^5))*M^2)*rp[t]^31 + 
    4*(-1 + En^2)*la*(665280 - 2600502*la + 377001*la^2 - 11609*la^3 + 
      40*la^4 + 40320*En^10*(-504 + 211*la) + 
      192*En^8*(372960 - 413366*la + 1579*la^2) - 
      12*En^6*(7837200 - 14115762*la + 442579*la^2 + 1707*la^3) + 
      4*En^2*(-3281040 + 10500171*la - 1063741*la^2 + 17844*la^3 + la^4) - 
      2*En^4*(-27609120 + 68967870*la - 4435371*la^2 + 24629*la^3 + 92*la^4))*
     M*rp[t]^32 - 4*(-1 + En^2)^2*la^2*(116970 - 23159*la + 986*la^2 - 
      5*la^3 + 13440*En^8*(257 + 5*la) + 1920*En^6*(-4564 + 143*la + 
        3*la^2) - 4*En^2*(540750 - 74945*la + 1706*la^2 + la^3) + 
      4*En^4*(1838970 - 153799*la + 562*la^2 + 11*la^3))*rp[t]^33))/
  (En^2*la*(1 + la)*rp[t]^20*(3*M + la*rp[t])^2*(J^2 + rp[t]^2)^12) + 
 ((-8*mu*Pi*YBar*(2*M - rp[t])*(139581792*J^22*M^8 + 
      18*J^22*(-17393559 + 5430764*la)*M^7*rp[t] + 
      6*J^20*M^6*(J^2*(47661156 - 36558484*la + 2958349*la^2) + 
        231659442*M^2)*rp[t]^2 + 3*J^20*M^5*
       (J^2*(-45189894 + 66808911*la - 13245851*la^2 + 69584*la^3) + 
        6*(-173399420 + 24057963*En^2 + 54136705*la)*M^2)*rp[t]^3 + 
      J^18*M^4*(J^4*(34985760 - 95048398*la + 36203563*la^2 - 426480*la^3 + 
          392*la^4) + 3*J^2*(951347436 - 729570157*la + 59177812*la^2 + 
          6*En^2*(-41942450 + 16548427*la))*M^2 + 6207166296*M^4)*rp[t]^4 + 
      J^18*M^3*(J^4*(-4624344 + 24535057*la - 17105519*la^2 + 342916*la^3 - 
          740*la^4) + 6*J^2*(-225754255 + 333626282*la - 66278029*la^2 + 
          371156*la^3 + En^2*(84613104 - 86610227*la + 8685479*la^2))*M^2 + 
        18*(-775542067 + 230761872*En^2 + 242007074*la)*M^4)*rp[t]^5 + 
      2*J^16*M^2*(J^6*(120960 - 1621752*la + 2197328*la^2 - 67622*la^3 + 
          261*la^4) + J^4*(3*En^2*(-27192624 + 58294188*la - 15147291*la^2 + 
            35332*la^3) + 2*(87483273 - 237536824*la + 90617635*la^2 - 
            1136634*la^3 + 1040*la^4))*M^2 + 
        3*J^2*(2130773940 + 40766832*En^4 - 1632947353*la + 132709645*la^2 + 
          6*En^2*(-201817486 + 79541333*la))*M^4 + 8170299630*M^6)*rp[t]^6 + 
      J^16*M*(J^6*la*(169680 - 577407*la + 26062*la^2 - 163*la^3) - 
        2*J^4*(23150727 - 122735331*la + 85660355*la^2 - 1826031*la^3 + 
          3936*la^4 + En^2*(-12371148 + 56271863*la - 30588857*la^2 + 
            153126*la^3 + 596*la^4))*M^2 + 3*J^2*(-2025656372 + 
          2991015773*la - 595116451*la^2 + 3537640*la^3 + 
          24*En^4*(-4219945 + 2296717*la) + 4*En^2*(408589722 - 
            417525187*la + 42018454*la^2))*M^4 + 
        18*(-2046001082 + 986921385*En^2 + 637690153*la)*M^6)*rp[t]^7 + 
      J^14*(J^8*la^2*(29960 - 1953*la + 19*la^2) + 
        2*J^6*(1212300 - 16237941*la + 22011944*la^2 - 719142*la^3 + 
          2784*la^4 + En^2*(-694440 + 8547804*la - 9851222*la^2 + 
            80328*la^3 + 864*la^4))*M^2 + J^4*(1572360444 - 4264826394*la + 
          1628803997*la^2 - 21675632*la^3 + 19104*la^4 + 
          24*En^4*(5518728 - 8570993*la + 1161365*la^2) + 
          12*En^2*(-131819223 + 281909652*la - 73417651*la^2 + 235356*la^3))*
         M^4 + 3*J^2*(11266972236 + 756780480*En^4 - 8622784897*la + 
          701604992*la^2 + 6*En^2*(-1733125014 + 681992939*la))*M^6 + 
        27996555744*M^8)*rp[t]^8 + 
      J^14*M*(-(J^6*la*(-1700085 + 5784423*la - 276588*la^2 + 1744*la^3 + 
           En^2*(961830 - 2999120*la + 36228*la^2 + 834*la^3))) - 
        2*J^4*(104180670 - 551623735*la + 385231803*la^2 - 8706931*la^3 + 
          18176*la^4 + 6*En^4*(1954620 - 7489269*la + 2907811*la^2 + 
            13462*la^3) + 2*En^2*(-60224400 + 273059821*la - 148548349*la^2 + 
            1029606*la^3 + 1936*la^4))*M^2 + 
        6*J^2*(-2683645128 + 5905080*En^6 + 3956400086*la - 787844146*la^2 + 
          4944528*la^3 + 12*En^4*(-39508889 + 21372017*la) + 
          En^2*(3523886796 - 3592806135*la + 362781381*la^2))*M^4 + 
        36*(-1758078959 + 1234927488*En^2 + 546692856*la)*M^6)*rp[t]^9 + 
      2*J^12*(J^8*la^2*(3*(50015 - 3444*la + 34*la^2) + 
          En^2*(-84700 + 1491*la + 67*la^2)) + 
        J^6*(5463450 - 73069935*la + 99056693*la^2 - 3428365*la^3 + 
          12936*la^4 - 12*En^4*(-56925 + 665371*la - 642146*la^2 - 
            7868*la^3 + 99*la^4) + 2*En^2*(-3396465 + 41633226*la - 
            47936110*la^2 + 544134*la^3 + 2940*la^4))*M^2 + 
        J^4*(2087688510 - 5652563550*la + 2159665919*la^2 - 30347416*la^3 + 
          24400*la^4 + 72*En^6*(-180896 + 162777*la) + 
          24*En^4*(26086683 - 40203692*la + 5443517*la^2) + 
          3*En^2*(-1142200464 + 2435234986*la - 635476159*la^2 + 
            2627380*la^3))*M^4 + 6*J^2*(4855799844 + 777378600*En^4 - 
          3707013554*la + 301595773*la^2 + 12*En^2*(-544823552 + 
            213896507*la))*M^6 + 16217798508*M^8)*rp[t]^10 + 
      J^12*M*(J^6*la*(7659045 - 26042855*la + 1317612*la^2 - 8160*la^3 + 
          4*En^4*(233640 - 697051*la - 17060*la^2 + 527*la^3) - 
          2*En^2*(4704255 - 14624905*la + 245832*la^2 + 2976*la^3)) + 
        2*J^4*(-277256709 + 1465101828*la - 1023071784*la^2 + 24419504*la^3 - 
          46960*la^4 + 12*En^6*(226386 - 720568*la + 156639*la^2) - 
          6*En^4*(18683235 - 70903827*la + 27424508*la^2 + 82414*la^3) + 
          En^2*(524523735 - 2368681020*la + 1289108886*la^2 - 11599834*la^3 - 
            7296*la^4))*M^2 + 6*J^2*(-4641007746 + 55044144*En^6 + 
          6823756315*la - 1358241011*la^2 + 8929984*la^3 + 
          24*En^4*(-81992683 + 44032829*la) + En^2*(8909083044 - 
            9055505608*la + 917078096*la^2))*M^4 + 
        36*(-2046181528 + 1991810727*En^2 + 633608497*la)*M^6)*rp[t]^11 + 
      J^10*(J^8*la^2*(1351420 - 98283*la + 962*la^2 - 
          20*En^4*(-8400 - 364*la + 23*la^2) + 4*En^2*(-414050 + 9996*la + 
            251*la^2)) + J^6*(-24*En^6*(11835 - 151242*la + 120158*la^2 + 
            4070*la^3) - 12*En^4*(-1102095 + 12730228*la - 12193046*la^2 - 
            104880*la^3 + 1752*la^4) + 2*En^2*(-29751030 + 362810823*la - 
            417105610*la^2 + 6182278*la^3 + 12936*la^4) + 
          3*(9714240 - 129628457*la + 175610640*la^2 - 6418988*la^3 + 
            22560*la^4))*M^2 + 2*J^4*(3621965436 + 214704*En^8 - 
          9778302654*la + 3732921127*la^2 - 55001688*la^3 + 36136*la^4 + 
          1152*En^6*(-107603 + 95111*la) + 24*En^4*(109530990 - 
            167247763*la + 22616290*la^2) + 24*En^2*(-363042489 + 
            770961845*la - 201493229*la^2 + 1032002*la^3))*M^4 + 
        6*J^2*(11355991164 + 3700731024*En^4 - 8631494813*la + 
          700529692*la^2 + 6*En^2*(-3537191090 + 1383831811*la))*M^6 + 
        25438321296*M^8)*rp[t]^12 + 
      J^10*M*(-(J^6*la*(-20418345 + 69332441*la - 3704134*la^2 + 21640*la^3 + 
           8*En^6*(23910 - 81343*la - 7489*la^2 + 75*la^3) + 
           4*En^2*(10301535 - 31904239*la + 702155*la^2 + 3732*la^3) - 
           4*En^4*(2262375 - 6667051*la - 122337*la^2 + 4732*la^3))) + 
        2*J^4*(72*En^8*(-625 + 1807*la) + 48*En^6*(551037 - 1717993*la + 
            369170*la^2) - 6*En^4*(79491681 - 298186197*la + 114776208*la^2 + 
            141796*la^3) + 2*En^2*(671096205 - 3015260862*la + 
            1640728844*la^2 - 18411436*la^3 + 4304*la^4) - 
          3*(160857036 - 847341242*la + 590932967*la^2 - 14805416*la^3 + 
            23780*la^4))*M^2 + 12*J^2*(-2726368413 + 113377032*En^6 + 
          3990325642*la - 792125311*la^2 + 5386564*la^3 + 
          36*En^4*(-65820579 + 35051495*la) + En^2*(7279063986 - 
            7366824899*la + 747615571*la^2))*M^4 + 
        36*(-1616685575 + 2143087536*En^2 + 496689014*la)*M^6)*rp[t]^13 + 
      2*J^8*(J^8*la^2*(1800495 - 138187*la + 1295*la^2 + 
          En^4*(810600 + 27860*la - 2104*la^2) + 4*En^6*(-4900 - 938*la + 
            29*la^2) + 3*En^2*(-1208220 + 37977*la + 470*la^2)) + 
        2*J^6*(24*En^8*la*(-585 + 316*la) - 12*En^6*(59175 - 736986*la + 
            573746*la^2 + 16430*la^3) - 3*En^2*(12779325 - 154829935*la + 
            177597780*la^2 - 3304238*la^3 + 596*la^4) + 
          3*(4240575 - 56393842*la + 76256651*la^2 - 2929210*la^3 + 
            8801*la^4) - 2*En^4*(-7138800 + 81322356*la - 77171897*la^2 - 
            353784*la^3 + 9896*la^4))*M^2 + 
        2*J^4*(2138064435 + 1175040*En^8 - 5744542842*la + 2186446192*la^2 - 
          33388156*la^3 + 12936*la^4 + 216*En^6*(-1213747 + 1048473*la) + 
          24*En^4*(133729158 - 201903625*la + 27260927*la^2) + 
          3*En^2*(-2390142348 + 5049287808*la - 1320494619*la^2 + 
            8139860*la^3))*M^4 + 6*J^2*(4520040084 + 2799257688*En^4 - 
          3408334571*la + 274659153*la^2 + 6*En^2*(-1919554854 + 
            746646527*la))*M^6 + 6484600908*M^8)*rp[t]^14 + 
      J^8*M*(-(J^6*la*(-35633565 + 120689173*la - 6784116*la^2 + 34738*la^3 + 
           48*En^8*la*(336 + 181*la) + 8*En^6*(239100 - 785635*la - 
             62794*la^2 + 732*la^3) + 2*En^2*(53096625 - 163629011*la + 
             4542318*la^2 + 2880*la^3) - 4*En^4*(9774540 - 28394611*la - 
             326097*la^2 + 18196*la^3))) + 2*J^4*(-572532813 + 
          3000712020*la - 2085604146*la^2 + 54281914*la^3 - 55664*la^4 + 
          360*En^8*(-1250 + 4037*la) + 48*En^6*(2383815 - 7291743*la + 
            1541342*la^2) + 6*En^4*(-197615835 + 729296733*la - 
            279036048*la^2 + 198668*la^3) + En^2*(2226150783 - 
            9940204470*la + 5403136534*la^2 - 73516276*la^3 + 62824*la^4))*
         M^2 + 6*J^2*(-4374039168 + 537592752*En^6 + 6350199229*la - 
          1251623111*la^2 + 8588760*la^3 + 48*En^4*(-151202956 + 
            79878523*la) + 12*En^2*(1329115109 - 1336041195*la + 
            135594646*la^2))*M^4 + 36*(-835579006 + 1528621401*En^2 + 
          252579917*la)*M^6)*rp[t]^15 + 
      J^6*(J^8*la^2*(6280200 - 507651*la + 4284*la^2 + 1280*En^8*(1 + la) + 
          80*En^6*(-4788 - 826*la + 29*la^2) + 4*En^2*(-4667460 + 184829*la + 
            629*la^2) - 20*En^4*(-348918 - 8335*la + 830*la^2)) + 
        J^6*(60672240 - 802566213*la + 1081025940*la^2 - 43252056*la^3 + 
          89376*la^4 + 480*En^8*la*(-1170 + 773*la) - 
          24*En^6*(489105 - 6404865*la + 4891584*la^2 + 111080*la^3) - 
          4*En^4*(-36515475 + 404504787*la - 378857146*la^2 - 78836*la^3 + 
            39872*la^4) - 2*En^2*(128202750 - 1542062439*la + 
            1763388560*la^2 - 40117388*la^3 + 72312*la^4))*M^2 + 
        2*J^4*(3456979884 + 11656656*En^8 - 9211630506*la + 3480814477*la^2 - 
          53859560*la^3 - 7168*la^4 + 288*En^6*(-4553611 + 3732347*la) + 
          24*En^4*(414571902 - 620670334*la + 83816327*la^2) + 
          12*En^2*(-1322502645 + 2771878946*la - 723779225*la^2 + 
            5238984*la^3))*M^4 + 6*J^2*(4736711340 + 5572070352*En^4 - 
          3515200613*la + 278175720*la^2 + 6*En^2*(-2774197966 + 
            1068182143*la))*M^6 + 3761475840*M^8)*rp[t]^16 + 
      J^6*M*(-(J^6*la*(-42454755 + 143140187*la - 8405882*la^2 + 31752*la^3 + 
           1200*En^8*la*(125 + 63*la) + 8*En^6*(997920 - 3398883*la - 
             215438*la^2 + 3244*la^3) - 12*En^4*(8324715 - 23528521*la - 
             101891*la^2 + 12524*la^3) - 4*En^2*(-44392455 + 136063641*la - 
             4659823*la^2 + 12597*la^3))) + 
        2*J^4*(-466529274 + 2424439677*la - 1672628052*la^2 + 44315410*la^3 + 
          3640*la^4 + 72*En^8*(20313 + 107011*la) + 
          48*En^6*(6735816 - 18196199*la + 3673726*la^2) + 
          6*En^4*(-309436569 + 1133654055*la - 432968310*la^2 + 
            1302112*la^3) + 2*En^2*(1244961657 - 5509076040*la + 
            2984256174*la^2 - 48047704*la^3 + 62640*la^4))*M^2 + 
        12*J^2*(375105600*En^6 + 24*En^4*(-153301315 + 79909873*la) + 
          En^2*(5844981942 - 5807191507*la + 586880469*la^2) + 
          2*(-580886499 + 830159171*la - 160804282*la^2 + 1047320*la^3))*
         M^4 + 18*(-501174667 + 1365281280*En^2 + 145081908*la)*M^6)*
       rp[t]^17 + 2*J^4*(J^8*la^2*(3737715 - 316664*la + 2107*la^2 + 
          6400*En^8*(1 + la) + En^4*(8870520 + 128524*la - 17636*la^2) + 
          En^2*(-15599820 + 767563*la - 2372*la^2) + 
          4*En^6*(-196790 - 26963*la + 1362*la^2)) + 
        J^6*(144*En^8*(-16380 + 4971*la + 8522*la^2) - 
          24*En^6*(1033290 - 8980035*la + 6060886*la^2 + 112274*la^3) + 
          3*(8306010 - 108911531*la + 145569796*la^2 - 5959710*la^3 + 
            1764*la^4) - 4*En^4*(-28690875 + 317285535*la - 296316837*la^2 + 
            1545596*la^3 + 22298*la^4) - 2*En^2*(72480015 - 863245644*la + 
            981633892*la^2 - 26684518*la^3 + 78756*la^4))*M^2 + 
        2*J^4*(930972321 + 40849488*En^8 - 2442297036*la + 907572373*la^2 - 
          13470408*la^3 - 17864*la^4 + 360*En^6*(-2461639 + 2096530*la) + 
          24*En^4*(214126587 - 316140922*la + 42512090*la^2) + 
          3*En^2*(-1969142028 + 4071728542*la - 1056433931*la^2 + 
            8758916*la^3))*M^4 + 3*J^2*(1466844948 + 3589271280*En^4 - 
          1045517536*la + 78280281*la^2 + 24*En^2*(-316399468 + 
            119539803*la))*M^6 + 148150782*M^8)*rp[t]^18 + 
      J^4*M*(J^6*la*(34835535 - 116487937*la + 7040780*la^2 - 8876*la^3 - 
          144*En^8*(21280 + 1186*la + 1267*la^2) - 
          8*En^6*(4140840 - 9283859*la - 497570*la^2 + 7452*la^3) + 
          4*En^4*(39284550 - 110987825*la + 574039*la^2 + 43410*la^3) + 
          2*En^2*(-100410705 + 305347609*la - 12650534*la^2 + 63552*la^3)) - 
        2*J^4*(1978560*En^10 - 72*En^8*(-971120 + 725357*la) - 
          120*En^6*(3225408 - 9876266*la + 2098195*la^2) - 
          6*En^4*(-324671625 + 1176091179*la - 446416736*la^2 + 
            2497248*la^3) + En^2*(-1886018445 + 8215553952*la - 
            4410530768*la^2 + 81849292*la^3 - 133328*la^4) - 
          3*(-84933945 + 434558680*la - 294985996*la^2 + 7579892*la^3 + 
            19248*la^4))*M^2 + 3*J^2*(-1484323206 + 1471284000*En^6 + 
          2042540267*la - 376352927*la^2 + 1786736*la^3 + 
          48*En^4*(-204402237 + 103453171*la) + 8*En^2*(1364713779 - 
            1327256930*la + 132372564*la^2))*M^4 + 
        18*(-49313828 + 317790711*En^2 + 10382717*la)*M^6)*rp[t]^19 - 
      J^2*(J^8*la^2*(-6125220 + 537625*la - 1778*la^2 + 
          En^6*(6017440 + 633488*la - 26192*la^2) + 
          192*En^8*(2480 + 101*la + 6*la^2) + 12*En^2*(2938410 - 177616*la + 
            1343*la^2) + 4*En^4*(-6968910 - 5449*la + 10556*la^2)) + 
        J^6*(17280*En^10*(-237 + 149*la) - 288*En^8*(131430 - 317330*la + 
            53801*la^2) + 120*En^6*(322749 - 4300383*la + 3315188*la^2 + 
            45100*la^3) + 3*(-9201600 + 118778375*la - 156256812*la^2 + 
            6276680*la^3 + 22016*la^4) + 4*En^4*(-60248475 + 668311761*la - 
            620579486*la^2 + 7305812*la^3 + 24544*la^4) + 
          6*En^2*(37306710 - 436301569*la + 490114240*la^2 - 15489256*la^3 + 
            58040*la^4))*M^2 + J^4*(-1225690200 + 95566176*En^8 + 
          3102774390*la - 1102008983*la^2 + 12642272*la^3 + 74520*la^4 - 
          1152*En^6*(-3084807 + 2603729*la) - 48*En^4*(297427818 - 
            423419977*la + 55784802*la^2) - 48*En^2*(-235942353 + 
            476063307*la - 121519693*la^2 + 1122390*la^3))*M^4 - 
        9*J^2*(114060852 + 888107040*En^4 - 64461107*la + 2888284*la^2 + 
          6*En^2*(-205326062 + 74261317*la))*M^6 + 144467496*M^8)*rp[t]^20 + 
      (J^8*la*(19263195 - 63402275*la + 3806442*la^2 + 15160*la^3 - 
          17280*En^10*(-158 + 23*la) - 240*En^8*(-104388 + 63023*la + 
            3087*la^2) - 40*En^6*(644100 - 2228785*la - 119808*la^2 + 
            1858*la^3) + 4*En^4*(41392665 - 117517121*la + 2133725*la^2 + 
            25492*la^3) + 4*En^2*(-38761515 + 115984085*la - 5654151*la^2 + 
            37062*la^3))*M + J^6*(-172720728 + 47589120*En^10 + 
          854247051*la - 556517103*la^2 + 11634748*la^3 + 127900*la^4 - 
          144*En^8*(-1149433 + 489011*la) + 288*En^6*(2556293 - 8376829*la + 
            1796762*la^2) + 36*En^4*(-79049385 + 273350149*la - 
            100817416*la^2 + 806572*la^3) + 4*En^2*(465644835 - 
            1970617410*la + 1036674508*la^2 - 21480052*la^3 + 40432*la^4))*
         M^3 + 6*J^4*(-99242511 + 513934704*En^6 + 114239810*la - 
          14969261*la^2 - 192956*la^3 + 24*En^4*(-80404903 + 38516299*la) + 
          3*En^2*(463819828 - 430793581*la + 41346337*la^2))*M^5 + 
        18*J^2*(15252033 + 9671184*En^2 - 6042910*la)*M^7)*rp[t]^21 - 
      2*(J^8*la^2*(-960*En^10*(248 + 11*la) + 320*En^8*(-6610 - 249*la + 
            7*la^2) + 3*(-563195 + 49753*la + 173*la^2) + 
          20*En^4*(-734640 + 17014*la + 661*la^2) - 
          20*En^6*(-125550 - 24341*la + 844*la^2) + 
          En^2*(13593300 - 983777*la + 10198*la^2)) + 
        J^6*(-4814910 + 60159318*la - 76215390*la^2 + 2606982*la^3 + 
          40035*la^4 - 17280*En^10*(-831 + 919*la) + 
          144*En^8*(146040 - 396211*la + 50928*la^2) + 
          24*En^6*(312795 - 10433016*la + 8704734*la^2 + 16930*la^3) + 
          4*En^4*(-23487030 + 244845642*la - 218263939*la^2 + 3922560*la^3 + 
            688*la^4) + 2*En^2*(28630665 - 323214489*la + 353862380*la^2 - 
            12535454*la^3 + 54132*la^4))*M^2 - 
        J^4*(91830936 + 40388544*En^8 - 201585164*la + 56084722*la^2 + 
          830252*la^3 - 20464*la^4 + 144*En^6*(-10262959 + 7334753*la) + 
          48*En^4*(62834730 - 83470489*la + 10486485*la^2) + 
          3*En^2*(-505588632 + 970567300*la - 237568687*la^2 + 2359316*la^3))*
         M^4 - 9*J^2*(-11256884 + 57839328*En^4 + 11537517*la - 
          1282309*la^2 + En^2*(-28739820 + 6600362*la))*M^6 + 16511418*M^8)*
       rp[t]^22 + (J^6*la*(6698295 - 21287896*la + 1133974*la^2 + 
          21405*la^3 + 11520*En^10*(-1662 + 461*la) - 
          48*En^8*(592320 - 408436*la + 24015*la^2) + 
          4*En^4*(32328000 - 86441481*la + 2676613*la^2 + 3172*la^3) - 
          8*En^6*(1346940 - 11127273*la - 184574*la^2 + 6100*la^3) + 
          2*En^2*(-39625695 + 114656793*la - 6304810*la^2 + 47616*la^3))*M - 
        2*J^4*(14266908 + 44236800*En^10 - 62756501*la + 34043731*la^2 + 
          357137*la^3 - 35920*la^4 - 72*En^8*(493494 + 376667*la) - 
          48*En^6*(8857647 - 21059353*la + 3881400*la^2) + 
          En^4*(660623526 - 2081462922*la + 720564288*la^2 - 6972024*la^3) + 
          En^2*(-263560905 + 1054795419*la - 529483113*la^2 + 11720422*la^3 - 
            25548*la^4))*M^3 + 3*J^2*(23878852 + 312560928*En^6 - 
          51446343*la + 14728017*la^2 - 316248*la^3 + 
          24*En^4*(-23702999 + 10033575*la) + 12*En^2*(14516592 - 
            9866033*la + 597682*la^2))*M^5 - 54*(-1282690 + 2629977*En^2 + 
          448609*la)*M^7)*rp[t]^23 - 
      (J^6*la^2*(-1169640 - 11520*En^10*(-276 + la) + 95204*la + 2015*la^2 + 
          En^6*(2617280 + 448704*la - 22832*la^2) + 
          640*En^8*(7145 - 633*la + 10*la^2) + 12*En^4*(-1901150 + 93745*la + 
            414*la^2) + 4*En^2*(3463680 - 285553*la + 3425*la^2)) + 
        J^4*(-1733400 + 19614969*la - 21563692*la^2 - 12740*la^3 + 
          46368*la^4 + 518400*En^10*(-79 + 114*la) - 
          288*En^8*(-145200 + 163918*la + 30997*la^2) - 
          24*En^6*(-2773575 + 24217287*la - 14811072*la^2 + 96040*la^3) - 
          4*En^4*(25066125 - 227262117*la + 185293614*la^2 - 4046452*la^3 + 
            5408*la^4) + 2*En^2*(17284050 - 182644845*la + 189425530*la^2 - 
            7128748*la^3 + 35016*la^4))*M^2 + 
        J^2*(11698500 - 306002016*En^8 - 55224110*la + 32977095*la^2 - 
          1772736*la^3 + 12128*la^4 - 576*En^6*(-1875083 + 1114647*la) - 
          24*En^4*(42201612 - 49133915*la + 5491413*la^2) - 
          12*En^2*(-19878423 + 29951528*la - 5315379*la^2 + 47540*la^3))*
         M^4 + 9*(6509580 + 17682336*En^4 - 5638361*la + 545360*la^2 + 
          2*En^2*(-13445394 + 5593609*la))*M^6)*rp[t]^24 - 
      2*(J^4*la*(-597570 + 1696009*la - 27253*la^2 - 6476*la^3 + 
          57600*En^10*(-237 + 86*la) + 24*En^8*(579240 - 153357*la + 
            9941*la^2) + En^2*(11927475 - 32316446*la + 1878088*la^2 - 
            15855*la^3) + 4*En^6*(5701200 - 12786505*la + 187406*la^2 + 
            1692*la^3) + En^4*(-34382430 + 80039522*la - 3032426*la^2 + 
            8648*la^3))*M - J^2*(286164 + 16571520*En^10 - 4634256*la + 
          5952691*la^2 - 639069*la^3 + 10776*la^4 + 
          504*En^8*(-226903 + 204823*la) + 48*En^6*(4182060 - 7674553*la + 
            1182402*la^2) + 6*En^4*(-21394575 + 58068288*la - 17665343*la^2 + 
            189718*la^3) + En^2*(25129998 - 82006550*la + 31985274*la^2 - 
            615236*la^3 + 2624*la^4))*M^3 + 3*(-4232492 + 12245688*En^6 + 
          7139298*la - 1701458*la^2 + 27536*la^3 + 
          12*En^4*(-2922821 + 1536693*la) + 3*En^2*(8817592 - 9518559*la + 
            1056949*la^2))*M^5)*rp[t]^25 - 
      2*((-1 + En^2)*J^4*la^2*(103000 - 4378*la - 653*la^2 + 
          9600*En^8*(-235 + 2*la) + 1920*En^6*(-31 - 41*la + la^2) - 
          20*En^4*(-200738 + 9407*la + 70*la^2) - 
          20*En^2*(98476 - 8758*la + 151*la^2)) - 
        J^2*(12960 + 256218*la - 1023793*la^2 + 219759*la^3 - 7068*la^4 + 
          17280*En^10*(-375 + 637*la) + 144*En^8*(143820 - 534587*la + 
            122578*la^2) + 24*En^6*(-996660 + 5682555*la - 2683482*la^2 + 
            22690*la^3) - 4*En^2*(491400 - 4312266*la + 3610115*la^2 - 
            116022*la^3 + 948*la^4) + 4*En^4*(2910600 - 21976626*la + 
            15423353*la^2 - 356272*la^3 + 1179*la^4))*M^2 + 
        3*(981744 + 2449440*En^8 - 3085474*la + 1422571*la^2 - 53496*la^3 + 
          256*la^4 + 72*En^6*(-163602 + 116279*la) + 
          24*En^4*(692251 - 1014412*la + 139571*la^2) + 
          En^2*(-8265864 + 18689814*la - 5362901*la^2 + 62716*la^3))*M^4)*
       rp[t]^26 - 4*(-((-1 + En^2)*J^2*la*(-3450 - 31685*la + 17700*la^2 - 
           1009*la^3 + 43200*En^8*(-50 + 21*la) + 
           36*En^6*(133120 - 157114*la + 415*la^2) + 
           En^2*(669030 - 1530206*la + 91233*la^2 - 1921*la^3) + 
           2*En^4*(-1648320 + 3064141*la - 99772*la^2 + 76*la^3))*M) + 
        (254880*En^10 + 36*En^8*(-64676 + 68641*la) + 
          18*En^6*(298382 - 670228*la + 122407*la^2) + 
          6*En^4*(-824508 + 2876985*la - 1100710*la^2 + 9609*la^3) + 
          En^2*(1820412 - 8735954*la + 5223567*la^2 - 143059*la^3 + 
            152*la^4) - 2*(85392 - 534446*la + 455157*la^2 - 30409*la^3 + 
            344*la^4))*M^3)*rp[t]^27 - 4*(-1 + En^2)*
       ((-1 + En^2)*J^2*la^2*(960*En^6*(382 + 7*la) + 
          80*En^4*(-5626 + 235*la + 2*la^2) + 15*(-22 - 69*la + 7*la^2) + 
          4*En^2*(28550 - 2591*la + 89*la^2)) + 
        (-7560 + 123174*la - 206513*la^2 + 22351*la^3 - 456*la^4 + 
          12960*En^8*(-7 + 13*la) + 72*En^6*(3570 - 19422*la + 5809*la^2) + 
          18*En^4*(-13860 + 125687*la - 94450*la^2 + 426*la^3) + 
          En^2*(90720 - 1154439*la + 1404356*la^2 - 57040*la^3 - 264*la^4))*
         M^2)*rp[t]^28 - 8*(-1 + En^2)^2*la*(2700 - 11602*la + 1976*la^2 - 
        66*la^3 + 720*En^6*(-42 + 19*la) - 6*En^4*(-9420 + 17923*la + 
          199*la^2) - 3*En^2*(9630 - 30980*la + 1841*la^2 + 31*la^3))*M*
       rp[t]^29 + 8*(-1 + En^2)^3*la^2*(240*En^4*(22 + la) + 
        7*(70 - 19*la + la^2) + 16*En^2*(-290 + 26*la + la^2))*rp[t]^30))/
    (En*rp[t]^17*(3*M + la*rp[t])^2*(J^2 + rp[t]^2)^10) + 
   (8*J^2*mu*Pi*YPhiPhiBar*(2*M - rp[t])*(1784660850*J^26*M^9 - 
      1323*J^26*(3262773 - 1171108*la + 221238*m^2)*M^8*rp[t] + 
      27*J^24*M^7*(J^2*(2*(80415842 - 68380313*la + 8111950*la^2) - 
          67*(-362045 + 117992*la)*m^2 + 153144*m^4) + 886476630*M^2)*
       rp[t]^2 + 3*J^24*M^6*(-2*J^2*(390226785 - 608510971*la + 
          169707855*la^2 - 6995789*la^3 + (99367563 - 79356047*la + 
            7178525*la^2)*m^2 + (1410492 - 477180*la)*m^4 + 1236*m^6) + 
        9*(-2144061477 + 173300400*En^2 + 767341972*la - 139450404*m^2)*M^2)*
       rp[t]^3 + 3*J^22*M^5*(J^4*(241647120 - 642906098*la + 325554379*la^2 - 
          30982339*la^3 + 244752*la^4 + (93768012 - 143917617*la + 
            31693551*la^2 - 490336*la^3)*m^2 + 2*(1136061 - 976902*la + 
            84956*la^2)*m^4 + (4572 - 1664*la)*m^6) - 
        6*J^2*(-3235046195 + 2743610283*la - 324068998*la^2 + 
          4*(-117208951 + 38074117*la)*m^2 - 2750868*m^4 + 
          9*En^2*(55301163 - 24849781*la + 5681424*m^2))*M^2 + 
        49518845532*M^4)*rp[t]^4 + J^22*M^4*
       (-(J^4*(127448424 - 582578196*la + 496718506*la^2 - 83643463*la^3 + 
           1485072*la^4 - 3976*la^5 + (72096426 - 202703544*la + 
             85094753*la^2 - 3001048*la^3 + 8120*la^4)*m^2 + 
           (2692116 - 4721238*la + 1041312*la^2 - 4648*la^3)*m^4 + 
           210*(45 - 44*la + 4*la^2)*m^6)) + 
        3*J^2*(-10464236979 + 16279494670*la - 4521161430*la^2 + 
          185340818*la^3 + (-2565423591 + 2042061368*la - 183699980*la^2)*
           m^2 + 24*(-1413077 + 475647*la)*m^4 - 26016*m^6 + 
          6*En^2*(7*(54173981 - 60034872*la + 8906368*la^2) + 
            (88845649 - 37100800*la)*m^2 + 981792*m^4))*M^2 + 
        162*(-2217917699 + 373705020*En^2 + 791222164*la - 137626456*m^2)*
         M^4)*rp[t]^5 + J^20*M^3*(J^6*(11581920 - 99315816*la + 
          142872305*la^2 - 38966543*la^3 + 1180500*la^4 - 7268*la^5 + 
          (9432324 - 51619440*la + 39288155*la^2 - 2407628*la^3 + 14972*la^4)*
           m^2 + (519165 - 1864179*la + 836074*la^2 - 8596*la^3)*m^4 + 
          (2871 - 6372*la + 1556*la^2)*m^6) - 
        3*J^4*(-3239350176 + 8601222528*la - 4337946217*la^2 + 
          410352535*la^3 - 3303288*la^4 + 2*(-606425907 + 927696625*la - 
            203189526*la^2 + 3068352*la^3)*m^2 + (-27434790 + 23462340*la - 
            2027288*la^2)*m^4 + 8*(-6039 + 2188*la)*m^6 + 
          2*En^2*(436417968 - 939327905*la + 341091888*la^2 - 17039653*la^3 + 
            2*(89089818 - 96213227*la + 11222378*la^2)*m^2 + 
            (4568124 - 2034132*la)*m^4 + 8280*m^6))*M^2 + 
        18*J^2*(148317750*En^4 + 5*(4015625909 - 3395601554*la + 
            399139736*la^2) + (2781650435 - 900261812*la)*m^2 + 
          15010620*m^4 - 18*En^2*(358346760 - 160303027*la + 35408662*m^2))*
         M^4 + 565258346100*M^6)*rp[t]^6 + 
      J^20*M^2*(-2*J^6*(204120 - 4336596*la + 11401016*la^2 - 4923824*la^3 + 
          229746*la^4 - 2469*la^5 + (242838 - 3348165*la + 4894414*la^2 - 
            473125*la^3 + 5139*la^4)*m^2 - 3*(-6480 + 59874*la - 54763*la^2 + 
            987*la^3)*m^4 + 3*(54 - 323*la + 179*la^2)*m^6) + 
        J^4*(-1708008768 + 7795331820*la - 6621225766*la^2 + 
          1107601147*la^3 - 20035368*la^4 + 53464*la^5 + 
          (-934605054 + 2619009978*la - 1093670015*la^2 + 37635880*la^3 - 
            99464*la^4)*m^2 + 2*(-16333002 + 28472697*la - 6237264*la^2 + 
            26348*la^3)*m^4 - 12*(8361 - 8132*la + 736*la^2)*m^6 - 
          6*En^2*(-87384132 + 350639874*la - 244788735*la^2 + 29348041*la^3 - 
            157468*la^4 + (-56692734 + 127817886*la - 38263159*la^2 + 
              774952*la^3)*m^2 - 6*(430607 - 525574*la + 60432*la^2)*m^4 + 
            12*(-933 + 464*la)*m^6))*M^2 - 3*J^2*(64940140689 - 
          100764797588*la + 27853084536*la^2 - 1134486292*la^3 + 
          (15253379205 - 12096436060*la + 1081469248*la^2)*m^2 - 
          120*(-1548773 + 518475*la)*m^4 + 122904*m^6 + 
          36*En^4*(34800742 - 20781365*la + 4949588*m^2) - 
          6*En^2*(4923448841 - 5432460879*la + 801321736*la^2 + 
            (1111881613 - 461630008*la)*m^2 + 11427312*m^4))*M^4 + 
        54*(-25319014721 + 6685308612*En^2 + 8999788092*la - 1488935766*m^2)*
         M^6)*rp[t]^7 + J^18*M*(-(J^8*la*(289440 - 1810752*la + 
           1268607*la^2 - 87198*la^3 + 1475*la^4 + 
           (340836 - 1233108*la + 181363*la^2 - 3109*la^3)*m^2 + 
           (26865 - 62886*la + 1799*la^2)*m^4 + (219 - 327*la)*m^6)) - 
        J^6*(-155154960 + 1329124494*la - 1905403625*la^2 + 515837659*la^3 - 
          15918274*la^4 + 97708*la^5 + (-122577516 + 668580198*la - 
            506290387*la^2 + 30263340*la^3 - 183588*la^4)*m^2 + 
          (-6334866 + 22597281*la - 10061846*la^2 + 97692*la^3)*m^4 - 
          2*(15327 - 33804*la + 8212*la^2)*m^6 + 
          2*En^2*(25276320 - 203526156*la + 261119357*la^2 - 58404599*la^3 + 
            706626*la^4 + 244*la^5 + (25357932 - 120996438*la + 
              74886811*la^2 - 3578672*la^3 + 9796*la^4)*m^2 + 
            (1880316 - 5345415*la + 1680216*la^2 - 10724*la^3)*m^4 + 
            18*(831 - 1256*la + 156*la^2)*m^6))*M^2 + 
        6*J^4*(10050070089 - 26625718106*la + 13367890722*la^2 - 
          1255807699*la^3 + 10299640*la^4 + (3614108100 - 5508004678*la + 
            1199083167*la^2 - 17624208*la^3)*m^2 + 
          15*(5035773 - 4280846*la + 367356*la^2)*m^4 - 126*(-909 + 328*la)*
           m^6 + 6*En^4*(54397640 - 85708740*la + 16765459*la^2 - 
            2*(-9158435 + 5333684*la)*m^2 + 317160*m^4) - 
          En^2*(5677190058 - 12168572183*la + 4393273494*la^2 - 
            218212330*la^3 + (2239897782 - 2404533670*la + 278056720*la^2)*
             m^2 - 384*(-139571 + 61556*la)*m^4 + 82296*m^6))*M^4 + 
        18*J^2*(76403448701 + 1859337774*En^4 - 64393234130*la + 
          7527769832*la^2 - 4*(-2513587924 + 810096523*la)*m^2 + 
          49256700*m^4 - 6*En^2*(6422412253 - 2858685824*la + 607182972*m^2))*
         M^6 + 1471956827310*M^8)*rp[t]^8 - 
      J^18*(J^8*la^2*(51840 - 64232*la + 6417*la^2 - 163*la^3 + 
          (60228 - 13476*la + 349*la^2)*m^2 - 7*(-665 + 29*la)*m^4 + 
          37*m^6) - J^6*(2*(-2732940 + 58045482*la - 152151599*la^2 + 
            65154366*la^3 - 3095922*la^4 + 33183*la^5) - 
          2*(3164751 - 43486113*la + 63264439*la^2 - 5962635*la^3 + 
            63093*la^4)*m^2 + (-477495 + 4379688*la - 3975222*la^2 + 
            67494*la^3)*m^4 - 3*(1161 - 6892*la + 3796*la^2)*m^6 + 
          2*En^2*(2*(436590 - 9379467*la + 23640371*la^2 - 9241168*la^3 + 
              192502*la^4 + 240*la^5) + 2*(690849 - 8920125*la + 
              11537964*la^2 - 1003444*la^3 + 6504*la^4)*m^2 + 
            (163215 - 1294434*la + 944114*la^2 - 14448*la^3)*m^4 + 
            3*(729 - 3360*la + 1268*la^2)*m^6))*M^2 + 
        J^4*(10595700261 - 48273487032*la + 40828430105*la^2 - 
          6778268756*la^3 + 124907912*la^4 - 332280*la^5 + 
          (5584425174 - 15589522698*la + 6471457615*la^2 - 216657656*la^3 + 
            558408*la^4)*m^2 - 30*(-6028902 + 10440555*la - 2270384*la^2 + 
            9044*la^3)*m^4 + 54*(8847 - 8564*la + 772*la^2)*m^6 + 
          12*En^4*(38292345 - 130141452*la + 66662955*la^2 - 4154131*la^3 + 
            (23631198 - 39186050*la + 6363482*la^2)*m^2 + 
            (1002624 - 653956*la)*m^4 + 3624*m^6) + 
          6*En^2*(-1139163912 + 4553071638*la - 3160240361*la^2 + 
            376388733*la^3 - 2152152*la^4 + (-716310420 + 1604864706*la - 
              476314907*la^2 + 9360728*la^3)*m^2 - 12*(2549161 - 3078583*la + 
              349704*la^2)*m^4 + 108*(-1041 + 512*la)*m^6))*M^4 - 
        3*J^2*(-247110923229 + 148296960*En^6 + 382300985412*la - 
          105113573680*la^2 + 4249315972*la^3 + 
          (-55272666201 + 43646195064*la - 3875267972*la^2)*m^2 + 
          120*(-5105997 + 1699265*la)*m^4 - 343296*m^6 - 
          36*En^4*(438397634 - 260018665*la + 60189160*m^2) + 
          6*En^2*(29472708633 - 32362752045*la + 4743003232*la^2 + 
            (6384320139 - 2633370672*la)*m^2 + 60247248*m^4))*M^6 - 
        27*(-131881968155 + 48635438328*En^2 + 46687058700*la - 
          7287937158*m^2)*M^8)*rp[t]^9 + 
      J^16*M*(-(J^8*la*(3875040 - 24191637*la + 16777364*la^2 - 
           1173990*la^3 + 19817*la^4 + (4440948 - 15995571*la + 
             2292545*la^2 - 38227*la^3)*m^2 + (329715 - 765519*la + 
             20573*la^2)*m^4 - 42*(-56 + 83*la)*m^6 + 
           2*En^2*(-608040 + 3952719*la - 2753336*la^2 + 90134*la^3 + 
             297*la^4 + (-957024 + 3293899*la - 482267*la^2 + 5661*la^3)*
              m^2 - 5*(22401 - 45337*la + 1281*la^2)*m^4 + 6*(-246 + 283*la)*
              m^6))) - J^6*(-962171028 + 8232734382*la - 11756477903*la^2 + 
          3156097077*la^3 - 99204820*la^4 + 607148*la^5 + 
          (-734558049 + 3991055988*la - 3004950789*la^2 + 174656220*la^3 - 
            1031764*la^4)*m^2 + 5*(-7059357 + 24996393*la - 11042798*la^2 + 
            100828*la^3)*m^4 - 9*(16299 - 35748*la + 8644*la^2)*m^6 + 
          12*En^4*(-3865698 + 29443581*la - 32198017*la^2 + 5104985*la^3 + 
            1030*la^4 + (-4077462 + 16689170*la - 7663021*la^2 + 195136*la^3)*
             m^2 - 2*(166011 - 343966*la + 58120*la^2)*m^4 + 
            4*(-777 + 608*la)*m^6) + 2*En^2*(330263352 - 2649664200*la + 
            3379944467*la^2 - 750198779*la^3 + 9674028*la^4 + 1228*la^5 + 
            (322266672 - 1527480870*la + 937310515*la^2 - 43467584*la^3 + 
              114028*la^4)*m^2 - 2*(-11249703 + 31604652*la - 9805728*la^2 + 
              57232*la^3)*m^4 + 162*(939 - 1400*la + 172*la^2)*m^6))*M^2 - 
        6*J^4*(-38239213389 + 101051511176*la - 50476289942*la^2 + 
          4703784607*la^3 - 39295920*la^4 + (-13132565211 + 19927531587*la - 
            4308574422*la^2 + 61433680*la^3)*m^2 - 
          5*(50069889 - 42287118*la + 3602500*la^2)*m^4 + 
          48*(-6687 + 2404*la)*m^6 + 72*En^6*(924403 - 826669*la + 
            185570*m^2) - 6*En^4*(688976300 - 1078074096*la + 
            209259579*la^2 - 8*(-28084561 + 16196086*la)*m^2 + 3655784*m^4) + 
          3*En^2*(11353347792 - 24221900299*la + 8688370394*la^2 - 
            428616036*la^3 + 6*(718148924 - 765721761*la + 87686538*la^2)*
             m^2 + (95055132 - 41467732*la)*m^4 + 120096*m^6))*M^4 + 
        9*J^2*(21392376804*En^4 + 10*(39801237129 - 33419049397*la + 
            3882393594*la^2) + (49348260783 - 15827793856*la)*m^2 + 
          215938320*m^4 - 12*En^2*(23409910880 - 10361795579*la + 
            2103804954*m^2))*M^6 + 2772193269870*M^8)*rp[t]^10 + 
      J^16*(J^8*la^2*(-694080 + 848834*la - 86291*la^2 + 2189*la^3 + 
          (-784588 + 170946*la - 4299*la^2)*m^2 + 7*(-8155 + 333*la)*m^4 - 
          397*m^6 + 2*En^2*(108720 - 151284*la + 7619*la^2 + 59*la^3 + 
            (168616 - 41442*la + 803*la^2)*m^2 - 49*(-395 + 19*la)*m^4 + 
            249*m^6)) + J^6*(-33878250 + 719265762*la - 1879126310*la^2 + 
          797009178*la^3 - 38569182*la^4 + 412302*la^5 + 
          (-38056608 + 520856550*la - 753605626*la^2 + 69022326*la^3 - 
            710034*la^4)*m^2 + 15*(-178677 + 1625274*la - 1462722*la^2 + 
            23282*la^3)*m^4 - 27*(621 - 3662*la + 2006*la^2)*m^6 - 
          4*En^4*(2*(184275 - 4250898*la + 10264895*la^2 - 3308993*la^3 - 
              5725*la^4 + 585*la^5) + (676296 - 8526468*la + 9585180*la^2 - 
              606976*la^3 + 438*la^4)*m^2 + (100980 - 681351*la + 
              365156*la^2 - 3108*la^3)*m^4 + 72*(27 - 87*la + 17*la^2)*m^6) + 
          2*En^2*(11436390 - 244921410*la + 613862236*la^2 - 237788342*la^3 + 
            5277812*la^4 + 3624*la^5 + 2*(8839773 - 113311683*la + 
              145311618*la^2 - 12266248*la^3 + 76044*la^4)*m^2 + 
            (1977615 - 15474519*la + 11129288*la^2 - 155316*la^3)*m^4 + 
            27*(837 - 3792*la + 1412*la^2)*m^6))*M^2 + 
        J^4*(-40308117993 + 183272422224*la - 154263382391*la^2 + 
          25387610360*la^3 - 476503304*la^4 + 1263816*la^5 + 
          (-20354710212 + 56570852574*la - 23325543097*la^2 + 
            757043720*la^3 - 1899960*la^4)*m^2 + 
          30*(-20103030 + 34560753*la - 7456720*la^2 + 27916*la^3)*m^4 - 
          144*(9333 - 8996*la + 808*la^2)*m^6 + 144*En^6*
           (796572 - 2157107*la + 617727*la^2 + (403558 - 390878*la)*m^2 + 
            11168*m^4) - 12*En^4*(487888749 - 1646552916*la + 
            836646603*la^2 - 51836891*la^3 + (292758930 - 480282580*la + 
              77057632*la^2)*m^2 + (11737968 - 7526132*la)*m^4 + 35472*m^6) - 
          6*En^2*(-6850950192 + 27261286572*la - 18799060801*la^2 + 
            2221774341*la^3 - 13506368*la^4 + (-4157724006 + 9249163242*la - 
              2718485645*la^2 + 51587944*la^3)*m^2 - 
            2*(82243269 - 98119632*la + 10995616*la^2)*m^4 + 
            432*(-1149 + 560*la)*m^6))*M^4 + 3*J^2*(1839478464*En^6 + 
          5*(-128735792205 + 198496781610*la - 54246008350*la^2 + 
            2173562686*la^3) + (-136052493129 + 106903241346*la - 
            9418191898*la^2)*m^2 + 240*(-5624145 + 1860029*la)*m^4 - 
          627984*m^6 - 36*En^4*(31*(81793684 - 48144489*la) + 
            334236932*m^2) + 6*En^2*(107674280319 - 117591822017*la + 
            17107976840*la^2 + (22235899695 - 9103366232*la)*m^2 + 
            189350592*m^4))*M^6 + 27*(-248441919979 + 120335991432*En^2 + 
          87539125564*la - 12759571176*m^2)*M^8)*rp[t]^11 + 
      J^14*M*(J^8*la*(-24016500 + 149578848*la - 102565417*la^2 + 
          7307698*la^3 - 123081*la^4 + (-26695107 + 95659146*la - 
            13313599*la^2 + 215415*la^3)*m^2 - 45*(41094 - 94538*la + 
            2373*la^2)*m^4 + 27*(-419 + 617*la)*m^6 + 
          4*En^4*(-252990 + 1807965*la - 1166716*la^2 - 6110*la^3 + 
            1055*la^4 + (-462702 + 1588465*la - 198061*la^2 + 209*la^3)*m^2 - 
            12*(5725 - 9974*la + 217*la^2)*m^4 + 4*(-327 + 263*la)*m^6) - 
          2*En^2*(-7963920 + 51506370*la - 35483975*la^2 + 1236134*la^3 + 
            2703*la^4 + (-12245382 + 41780806*la - 5939405*la^2 + 66567*la^3)*
             m^2 + (-1356525 + 2703818*la - 69468*la^2)*m^4 + 
            54*(-282 + 319*la)*m^6)) + J^6*(3659190930 - 31266811188*la + 
          44454099411*la^2 - 11819446667*la^3 + 378377152*la^4 - 
          2308996*la^5 + (2686681116 - 14531383638*la + 10869159289*la^2 - 
            611966660*la^3 + 3514140*la^4)*m^2 - 15*(-7901928 + 27750345*la - 
            12155350*la^2 + 103964*la^3)*m^4 + 24*(17271 - 37692*la + 
            9076*la^2)*m^6 - 24*En^6*(476190 - 3576708*la + 3090791*la^2 - 
            235745*la^3 + 2*(243846 - 840835*la + 222675*la^2)*m^2 + 
            (36828 - 45460*la)*m^4 + 288*m^6) - 12*En^4*(-49571784 + 
            374916531*la - 406545499*la^2 + 64017031*la^3 - 40262*la^4 + 
            (-51099756 + 206677480*la - 93695618*la^2 + 2326272*la^3)*m^2 - 
            2*(1980003 - 4022438*la + 665768*la^2)*m^4 + 16*(-1959 + 1486*la)*
             m^6) - 6*En^2*(663819489 - 5303865213*la + 6722033283*la^2 - 
            1478819219*la^3 + 20285232*la^4 - 1708*la^5 + 
            (627653628 - 2952473730*la + 1794182915*la^2 - 80347952*la^3 + 
              201044*la^4)*m^2 + (40827438 - 113130207*la + 34586488*la^2 - 
              182532*la^3)*m^4 + 216*(1047 - 1544*la + 188*la^2)*m^6))*M^2 + 
        3*J^4*(4108320*En^8 + 5*(39841999632 - 104983083590*la + 
            52135526995*la^2 - 4812616673*la^3 + 40937536*la^4) + 
          (64867099968 - 97929884181*la + 21009473041*la^2 - 289467360*la^3)*
           m^2 + 20*(55466541 - 46519062*la + 3932956*la^2)*m^4 - 
          168*(-7011 + 2512*la)*m^6 - 144*En^6*(11598335 - 10260786*la + 
            2268526*m^2) + 12*En^4*(4009028418 - 6224418564*la + 
            1197641857*la^2 + (1259822282 - 718519432*la)*m^2 + 
            19057312*m^4) - 2*En^2*(124746961230 - 264750463041*la + 
            94269065938*la^2 - 4612664302*la^3 + 
            (45283637358 - 47910340770*la + 5425356968*la^2)*m^2 - 
            192*(-4717257 + 2032567*la)*m^4 + 913248*m^6))*M^4 + 
        18*J^2*(374973312582 + 37333813626*En^4 - 313491006673*la + 
          36156412110*la^2 - 8*(-5417685837 + 1727972513*la)*m^2 + 
          165935112*m^4 - 3*En^2*(116115257599 - 51070748959*la + 
            9837154560*m^2))*M^6 + 3888606239640*M^8)*rp[t]^12 + 
      J^14*(J^8*la^2*(-4302000 + 5185650*la - 536594*la^2 + 13591*la^3 - 
          7*(673591 - 142383*la + 3467*la^2)*m^2 + 35*(-9139 + 347*la)*m^4 - 
          1908*m^6 - 20*En^4*(9378 - 13629*la - 142*la^2 + 47*la^3 - 
            (-16646 + 3991*la + la^2)*m^2 - 7*(-341 + 15*la)*m^4 + 44*m^6) + 
          2*En^2*(1423440 - 1953252*la + 104339*la^2 + 599*la^3 + 
            5*(431312 - 102978*la + 1903*la^2)*m^2 - 7*(-33455 + 1459*la)*
             m^4 + 2565*m^6)) - J^6*(128779875 - 2732695758*la + 
          7112629232*la^2 - 2984050426*la^3 + 147061242*la^4 - 1567770*la^5 + 
          (139745277 - 1903679964*la + 2736930838*la^2 - 242613410*la^3 + 
            2421270*la^4)*m^2 - 30*(-302526 + 2725923*la - 2430515*la^2 + 
            36099*la^3)*m^4 + 252*(189 - 1108*la + 604*la^2)*m^6 + 
          24*En^6*(2*(-5670 + 169710*la - 407542*la^2 + 87488*la^3 + 
              2987*la^4) + (-23292 + 334134*la - 317267*la^2 + 6560*la^3)*
             m^2 - 4*(1035 - 6222*la + 1976*la^2)*m^4 + 12*(-9 + 16*la)*
             m^6) + 4*En^4*(2*(2378565 - 54502164*la + 130456646*la^2 - 
              41714810*la^3 + 10289*la^4 + 6939*la^5) + 
            (8590428 - 106855386*la + 118479573*la^2 - 7331500*la^3 + 
              5784*la^4)*m^2 + (1232280 - 8122395*la + 4253972*la^2 - 
              32340*la^3)*m^4 + 36*(567 - 1752*la + 332*la^2)*m^6) - 
          6*En^2*(23051385 - 491878656*la + 1225072884*la^2 - 
            469646218*la^3 + 11090572*la^4 + 1600*la^5 + 
            (34701975 - 441145200*la + 560227344*la^2 - 45674992*la^3 + 
              269296*la^4)*m^2 - 3*(-1213800 + 9349469*la - 6616478*la^2 + 
              83076*la^3)*m^4 + 36*(945 - 4224*la + 1556*la^2)*m^6))*M^2 - 
        3*J^4*(-5*(-6998883048 + 31748660936*la - 26577246191*la^2 + 
            4329461061*la^3 - 82738824*la^4 + 218800*la^5) + 
          2*(8409592728 - 23250025641*la + 9512582823*la^2 - 298033400*la^3 + 
            727240*la^4)*m^2 - 20*(-22417254 + 38237001*la - 8181808*la^2 + 
            28700*la^3)*m^4 + 84*(9819 - 9428*la + 844*la^2)*m^6 + 
          144*En^8*(10921 - 19802*la + 2408*m^2) - 
          96*En^6*(5059446 - 13547384*la + 3840434*la^2 - 
            5*(-502657 + 478215*la)*m^2 + 66652*m^4) + 
          4*En^4*(2858190075 - 9569075682*la + 4817838315*la^2 - 
            296407729*la^3 + 2*(830136312 - 1345256557*la + 212898919*la^2)*
             m^2 - 80*(-778821 + 489443*la)*m^4 + 146832*m^6) + 
          2*En^2*(-25161778026 + 99623429088*la - 68194487463*la^2 + 
            7986496155*la^3 - 51487288*la^4 + (-14660481456 + 
              32349825558*la - 9402451593*la^2 + 171196856*la^3)*m^2 - 
            16*(33057837 - 38883921*la + 4292648*la^2)*m^4 + 
            1008*(-1257 + 608*la)*m^6))*M^4 + 
        6*J^2*(-606518328195 + 5218833888*En^6 + 931573137597*la - 
          252801590555*la^2 + 10022460863*la^3 + 
          (-119929702779 + 93688549176*la - 8181608956*la^2)*m^2 + 
          168*(-6206643 + 2039285*la)*m^4 - 393120*m^6 - 
          18*En^4*(8902954912 - 5195678371*la + 1118710816*m^2) + 
          3*En^2*(267733289370 - 290595689171*la + 41922598672*la^2 - 
            46*(-1137062451 + 461445344*la)*m^2 + 392249088*m^4))*M^6 + 
        108*(-87161051711 + 53424659652*En^2 + 30544297556*la - 
          4098986724*m^2)*M^8)*rp[t]^13 + 
      J^12*M*(-(J^8*la*(91287000 - 567022119*la + 383861774*la^2 - 
           27850180*la^3 + 467931*la^4 + (97998498 - 349068414*la + 
             46972125*la^2 - 735645*la^3)*m^2 + 15*(417084 - 949675*la + 
             22141*la^2)*m^4 - 72*(-446 + 653*la)*m^6 + 
           8*En^6*(-22950 + 222123*la - 115302*la^2 - 11054*la^3 + 205*la^4 - 
             2*(23532 - 95668*la + 4899*la^2 + 283*la^3)*m^2 + 
             4*(-2085 + 3286*la + 7*la^2)*m^4 + 24*(-9 + 4*la)*m^6) + 
           6*En^2*(-16052715 + 103218015*la - 70226883*la^2 + 2601342*la^3 + 
             2881*la^4 + (-24034884 + 81194949*la - 11152175*la^2 + 118513*
                la^3)*m^2 + (-2496360 + 4887859*la - 112329*la^2)*m^4 + 
             72*(-318 + 355*la)*m^6) - 4*En^4*(-3265920 + 23132160*la - 
             14790511*la^2 - 25008*la^3 + 12601*la^4 + 
             (-5878626 + 19872285*la - 2429306*la^2 + 3322*la^3)*m^2 - 
             4*(209625 - 355706*la + 6951*la^2)*m^4 + 8*(-1713 + 1322*la)*
              m^6))) - 2*J^6*(5*(-952825383 + 8128550538*la - 
            11499295927*la^2 + 3023425897*la^3 - 98549920*la^4 + 
            599596*la^5) - 9*(371555508 - 1998774864*la + 1483650257*la^2 - 
            80546960*la^3 + 448820*la^4)*m^2 + 15*(-8880219 + 30906177*la - 
            13415182*la^2 + 107100*la^3)*m^4 - 21*(18243 - 39636*la + 
            9508*la^2)*m^6 + 72*En^8*(-2262 + 22099*la - 10746*la^2 + 
            16*(-95 + 274*la)*m^2 + 20*m^4) + 24*En^6*(3063141 - 
            22746201*la + 19435109*la^2 - 1491852*la^3 + 
            (3100854 - 10485619*la + 2728967*la^2)*m^2 - 
            4*(-56913 + 67895*la)*m^4 + 1656*m^6) + 
          6*En^4*(-292599891 + 2194796721*la - 2357149671*la^2 + 
            368109863*la^3 - 550542*la^4 + (-293682576 + 1171484318*la - 
              523399513*la^2 + 12607456*la^3)*m^2 - 8*(2682357 - 5329732*la + 
              860956*la^2)*m^4 + 224*(-591 + 439*la)*m^6) + 
          3*En^2*(2445460020 - 19447809654*la + 24467758123*la^2 - 
            5327466623*la^3 + 77569324*la^4 - 22244*la^5 + 
            (2229876360 - 10399038498*la + 6249293435*la^2 - 268486848*la^3 + 
              637756*la^4)*m^2 - 8*(-16656309 + 45396126*la - 13648784*la^2 + 
              64456*la^3)*m^4 + 504*(1155 - 1688*la + 204*la^2)*m^6))*M^2 + 
        3*J^4*(375451996320 + 53947296*En^8 - 986010735456*la + 
          486376272373*la^2 - 44393818113*la^3 + 384305784*la^4 - 
          4*(-28707221259 + 43077142917*la - 9159570134*la^2 + 
            121389408*la^3)*m^2 + 28*(61582473 - 51270486*la + 4300708*la^2)*
           m^4 - 1008*(-1467 + 524*la)*m^6 - 144*En^6*(66726400 - 
            58260473*la + 12669936*m^2) + 12*En^4*
           (3*(4723108042 - 7269871064*la + 1384861283*la^2) + 
            (4261947568 - 2401022368*la)*m^2 + 59091872*m^4) - 
          2*En^2*(311090431824 - 656281466712*la + 231715782346*la^2 - 
            11226127993*la^3 + 4*(26809967718 - 28109541303*la + 
              3141689210*la^2)*m^2 - 504*(-3765989 + 1600919*la)*m^4 + 
            1478736*m^6))*M^4 + 36*J^2*(263205761085 + 44035020558*En^4 - 
          218943209278*la + 25038019008*la^2 + (27957812841 - 8859396532*la)*
           m^2 + 91192668*m^4 - 18*En^2*(17228887560 - 7523302895*la + 
            1360977478*m^2))*M^6 + 4118875595352*M^8)*rp[t]^14 + 
      J^12*(J^8*la^2*(-16353000 + 19396394*la - 2043452*la^2 + 51659*la^3 - 
          3*(5768161 - 1177375*la + 27675*la^2)*m^2 + 105*(-10295 + 361*la)*
           m^4 - 5412*m^6 + 8*En^6*(5130 - 6853*la - 1352*la^2 + 83*la^3 + 
            (9674 - 1159*la - 249*la^2)*m^2 + 28*(55 + la)*m^4 + 36*m^6) - 
          4*En^4*(603270 - 869043*la - 4204*la^2 + 2831*la^3 + 
            5*(210910 - 49811*la + 43*la^2)*m^2 - 7*(-20765 + 831*la)*m^4 + 
            2300*m^6) + 6*En^2*(2868045 - 3874284*la + 219534*la^2 + 
            827*la^3 + (4230793 - 976587*la + 17063*la^2)*m^2 + 
            (430430 - 16674*la)*m^4 + 3852*m^6)) - 
        J^6*(48*En^8*la*(-4434 + 13865*la + 1589*la^2 + 32*(-90 + 47*la)*
             m^2 + 60*m^4) + 24*En^6*(-147420 + 4371198*la - 10356787*la^2 + 
            2238095*la^3 + 65656*la^4 + (-302796 + 4254270*la - 
              3957279*la^2 + 87808*la^3)*m^2 - 4*(13455 - 77061*la + 
              23648*la^2)*m^4 + 12*(-117 + 184*la)*m^6) + 
          4*En^4*(28304640 - 643247955*la + 1524156528*la^2 - 
            482684220*la^3 + 1123138*la^4 + 74326*la^5 + 
            (50132628 - 614058042*la + 670072709*la^2 - 40333580*la^3 + 
              33850*la^4)*m^2 - 4*(-1707615 + 10995981*la - 5607940*la^2 + 
              36708*la^3)*m^4 + 252*(351 - 1056*la + 196*la^2)*m^6) + 
          3*(-5*(-22346415 + 473865384*la - 1228114144*la^2 + 
              508829426*la^3 - 25532754*la^4 + 271396*la^5) + 
            (116510121 - 1578308292*la + 2252288644*la^2 - 192263940*la^3 + 
              1857660*la^4)*m^2 - 20*(-343161 + 3059760*la - 2700827*la^2 + 
              37275*la^3)*m^4 + 84*(351 - 2047*la + 1111*la^2)*m^6) - 
          6*En^2*(85196475 - 1810410837*la + 4476873746*la^2 - 
            1695828812*la^3 + 42538364*la^4 - 16056*la^5 + 
            (124403643 - 1566322332*la + 1967055364*la^2 - 153872888*la^3 + 
              857784*la^4)*m^2 + (12104820 - 91411188*la + 63478976*la^2 - 
              707952*la^3)*m^4 + 84*(1053 - 4656*la + 1700*la^2)*m^6))*M^2 - 
        3*J^4*(65953597536 - 298375192104*la + 248199006189*la^2 - 
          39945700697*la^3 + 776892848*la^4 - 2048304*la^5 + 
          2*(14955622344 - 41086085862*la + 16659518673*la^2 - 
            501322936*la^3 + 1187640*la^4)*m^2 - 
          28*(-25064658 + 42397449*la - 8994256*la^2 + 29484*la^3)*m^4 + 
          504*(2061 - 1972*la + 176*la^2)*m^6 + 144*En^8*
           (145109 - 262738*la + 33544*m^2) - 48*En^6*(59185446 - 
            156062017*la + 43694457*la^2 - 8*(-3588267 + 3340522*la)*m^2 + 
            716424*m^4) + 4*En^4*(10175858199 - 33771781578*la + 
            16827434715*la^2 - 1026318041*la^3 + 
            8*(711320319 - 1136491724*la + 177103544*la^2)*m^2 - 
            112*(-1768281 + 1081349*la)*m^4 + 339024*m^6) + 
          4*En^2*(-31476148461 + 123905867814*la - 84101911255*la^2 + 
            9742797830*la^3 - 66415366*la^4 + (-17488775106 + 
              38234012646*la - 10968097747*la^2 + 189981032*la^3)*m^2 - 
            42*(13379097 - 15491526*la + 1683088*la^2)*m^4 + 
            756*(-1365 + 656*la)*m^6))*M^4 + 6*J^2*(-851737743189 + 
          17834193792*En^6 + 1302302074140*la - 350501161360*la^2 + 
          13717955308*la^3 - 7*(22202485839 - 17225722908*la + 
            1489328312*la^2)*m^2 + 168*(-6859371 + 2238601*la)*m^4 - 
          341208*m^6 - 36*En^4*(10572702346 - 6108761337*la + 
            1253959588*m^2) + 6*En^2*(239067042885 - 257658872307*la + 
            36808553224*la^2 - 21*(-2083374013 + 836415320*la)*m^2 + 
            279274128*m^4))*M^6 + 756*(-13198411441 + 9996771564*En^2 + 
          4595199916*la - 556724058*m^2)*M^8)*rp[t]^15 + 
      2*J^10*M*(J^8*la*(-48*En^8*la*(-739 - 102*la + 164*la^2 - 
            36*(11 + 6*la)*m^2 + 10*m^4) - 4*En^6*(-298350 + 2841903*la - 
            1484571*la^2 - 124604*la^3 + 2452*la^4 - 
            2*(305916 - 1211944*la + 69051*la^2 + 2971*la^3)*m^2 - 
            4*(27105 - 40708*la + 14*la^2)*m^4 + 24*(-117 + 46*la)*m^6) - 
          3*En^2*(-59332455 + 379004474*la - 254204251*la^2 + 10002990*la^3 + 
            627*la^4 + (-86155398 + 287748148*la - 37938705*la^2 + 
              379467*la^3)*m^2 - 12*(690865 - 1323438*la + 26768*la^2)*m^4 + 
            168*(-354 + 391*la)*m^6) + 
          3*(-5*(7919730 - 49040851*la + 32719240*la^2 - 2417113*la^3 + 
              40499*la^4) + (-40839477 + 144438346*la - 18688735*la^2 + 
              282605*la^3)*m^2 - 5*(472605 - 1063978*la + 22925*la^2)*m^4 + 
            21*(-473 + 689*la)*m^6) + 2*En^4*(-19434870 + 136192470*la - 
            86142054*la^2 + 180772*la^3 + 68053*la^4 + 
            (-34321476 + 113956854*la - 13597721*la^2 + 21997*la^3)*m^2 - 
            4*(1162875 - 1918606*la + 32172*la^2)*m^4 + 56*(-1059 + 796*la)*
             m^6)) - 3*J^6*(-2992591935 + 25480809162*la - 35840395415*la^2 + 
          9300081476*la^3 - 308512355*la^4 + 1871116*la^5 + 
          (-1992747474 + 10648728558*la - 7833268589*la^2 + 407774676*la^3 - 
            2201084*la^4)*m^2 + 7*(-10011870 + 34510929*la - 14837974*la^2 + 
            110236*la^3)*m^4 - 294*(549 - 1188*la + 284*la^2)*m^6 + 
          24*En^8*(-29406 + 294343*la - 146970*la^2 + 16*(-1235 + 3877*la)*
             m^2 + 260*m^4) + 4*En^6*(36455796 - 266338596*la + 
            224250359*la^2 - 17293315*la^3 + 8*(4498212 - 14982141*la + 
              3817253*la^2)*m^2 + (2464824 - 2919480*la)*m^4 + 13824*m^6) + 
          2*En^4*(-1050978429 + 7808410539*la - 8296211501*la^2 + 
            1282508049*la^3 - 3072434*la^4 + 4*(-256209513 + 1003166146*la - 
              440778534*la^2 + 10227744*la^3)*m^2 - 
            56*(1257087 - 2414792*la + 377948*la^2)*m^4 + 
            112*(-2769 + 2026*la)*m^6) + 2*En^2*(3069757728 - 
            24283062984*la + 30294218764*la^2 - 6516234441*la^3 + 
            100456643*la^4 - 48028*la^5 + (2681739666 - 12389200362*la + 
              7349510565*la^2 - 300208976*la^3 + 674012*la^4)*m^2 - 
            7*(-20567592 + 55015623*la - 16243992*la^2 + 68068*la^3)*m^4 + 
            378*(1263 - 1832*la + 220*la^2)*m^6))*M^2 + 
        6*J^4*(131840259273 + 84188160*En^8 - 344890411734*la + 
          168787105376*la^2 - 15197070525*la^3 + 133789464*la^4 - 
          7*(-5341601592 + 7957085352*la - 1674663295*la^2 + 21235536*la^3)*
           m^2 + 7*(68488245 - 56588430*la + 4708892*la^2)*m^4 - 
          42*(-7659 + 2728*la)*m^6 - 288*En^6*(28925093 - 24909730*la + 
            5336770*m^2) + 6*En^4*(16945124400 - 25831752636*la + 
            4863238015*la^2 + (4824185086 - 2685907896*la)*m^2 + 
            59554712*m^4) - 3*En^2*(92921081958 - 194655611505*la + 
            68059270746*la^2 - 3257145638*la^3 + 14*(2153975295 - 
              2233030531*la + 245659056*la^2)*m^2 - 
            224*(-2035563 + 853217*la)*m^4 + 264600*m^6))*M^4 + 
        18*J^2*(73975686246*En^4 + 5*(55835910273 - 46166248990*la + 
            5226061344*la^2) + (26708311848 - 8399828108*la)*m^2 + 
          71656860*m^4 - 42*En^2*(9698535629 - 4200767114*la + 
            703200780*m^2))*M^6 + 1650163231575*M^8)*rp[t]^16 - 
      J^10*(J^8*la^2*(5*(8512875 - 9915238*la + 1063618*la^2 - 26824*la^3) + 
          3*(14417809 - 2824650*la + 63900*la^2)*m^2 - 210*(-11651 + 375*la)*
           m^4 + 10038*m^6 + 80*En^8*(18 - 9*la - 26*la^2 + la^3 - 
            8*(-4 - 3*la + la^2)*m^2 + 4*(1 + la)*m^4) - 
          8*En^6*(65430 - 88459*la - 15756*la^2 + 1009*la^3 + 
            (123522 - 16747*la - 2677*la^2)*m^2 + 84*(235 + la)*m^4 + 
            468*m^6) + 4*En^4*(3576195 - 5097198*la + 6736*la^2 + 
            15445*la^3 + 8*(767270 - 177553*la + 317*la^2)*m^2 - 
            196*(-4105 + 141*la)*m^4 + 9940*m^6) - 
          6*En^2*(10596705 - 14061792*la + 845114*la^2 + 1547*la^3 + 
            (15157989 - 3362183*la + 55007*la^2)*m^2 - 28*(-50965 + 1717*la)*
             m^4 + 9996*m^6)) + J^6*(48*En^8*la*(-57642 + 181205*la + 
            14561*la^2 + 64*(-585 + 358*la)*m^2 + 780*m^4) + 
          24*En^6*(-827820 + 26000154*la - 60545035*la^2 + 13171439*la^3 + 
            319802*la^4 + 4*(-406251 + 6180561*la - 5645243*la^2 + 
              133536*la^3)*m^2 - 4*(67815 - 418251*la + 126728*la^2)*m^4 + 
            36*(-171 + 256*la)*m^6) + 4*En^4*(103602240 - 2309978403*la + 
            5410827498*la^2 - 1693374042*la^3 + 7594022*la^4 + 235970*la^5 + 
            4*(45146619 - 535415685*la + 572518651*la^2 - 33321340*la^3 + 
              28784*la^4)*m^2 - 28*(-834975 + 5144439*la - 2525036*la^2 + 
              13692*la^3)*m^4 + 252*(837 - 2472*la + 452*la^2)*m^6) + 
          3*(210505365 - 4459475928*la + 11500854748*la^2 - 4695890382*la^3 + 
            239828118*la^4 - 2540892*la^5 + (209548143 - 2818838508*la + 
              3986591048*la^2 - 325673940*la^3 + 3040332*la^4)*m^2 - 
            14*(-781605 + 6890604*la - 6017878*la^2 + 76902*la^3)*m^4 + 
            126*(297 - 1724*la + 932*la^2)*m^6) - 
          6*En^2*(214455465 - 4540737651*la + 11136796416*la^2 - 
            4160055508*la^3 + 110633456*la^4 - 96384*la^5 + 
            (301816179 - 3765189396*la + 4669663288*la^2 - 347225552*la^3 + 
              1820256*la^4)*m^2 - 14*(-1908765 + 14085648*la - 9578626*la^2 + 
              93912*la^3)*m^4 + 126*(1161 - 5088*la + 1844*la^2)*m^6))*M^2 + 
        6*J^4*(46322285391 - 208909422480*la + 172497000607*la^2 - 
          27360053904*la^3 + 541146136*la^4 - 1422248*la^5 + 
          7*(2799080208 - 7629706746*la + 3061271099*la^2 - 87960120*la^3 + 
            202024*la^4)*m^2 - 14*(-28080522 + 47089137*la - 9901904*la^2 + 
            30268*la^3)*m^4 + 42*(10791 - 10292*la + 916*la^2)*m^6 + 
          576*En^8*(114314 - 207425*la + 24406*m^2) - 
          96*En^6*(52911000 - 135628807*la + 37443298*la^2 - 
            4*(-6370493 + 5632241*la)*m^2 + 613462*m^4) + 
          4*En^4*(12229440291 - 40312426296*la + 19848205995*la^2 - 
            1197643487*la^3 + 14*(464403069 - 734252099*la + 112587431*la^2)*
             m^2 - 28*(-7336806 + 4347629*la)*m^4 + 240240*m^6) + 
          2*En^2*(-56660938182 + 221417290782*la - 148812843861*la^2 + 
            17012915001*la^3 - 122186072*la^4 + 7*(-4255183152 + 
              9194124630*la - 2595363073*la^2 + 42277480*la^3)*m^2 - 
            28*(29350431 - 33428229*la + 3572792*la^2)*m^4 + 
            756*(-1473 + 704*la)*m^6))*M^4 - 6*J^2*(-903949396389 + 
          40587516288*En^6 + 1374617692908*la - 366314051336*la^2 + 
          14110419452*la^3 + (-149284524033 + 114886054872*la - 
            9821313604*la^2)*m^2 + 120*(-7588209 + 2459545*la)*m^4 - 
          202752*m^6 - 252*En^4*(2563021562 - 1460344699*la + 
            284168552*m^2) + 42*En^2*(44996218695 - 48122879375*la + 
            6796365632*la^2 + (7610103261 - 3013889648*la)*m^2 + 
            39473808*m^4))*M^6 - 27*(-296476303499 + 273499178736*En^2 + 
          102400844348*la - 10886538882*m^2)*M^8)*rp[t]^17 + 
      J^8*M*(-2*J^8*la*(48*En^8*la*(-9215 - 1130*la + 1936*la^2 - 
            4*(1217 + 632*la)*m^2 + 130*m^4) - 3*(-74597670 + 460241224*la - 
            301946894*la^2 + 22705226*la^3 - 379185*la^4 + 
            (-73422825 + 257408345*la - 31799051*la^2 + 463173*la^3)*m^2 - 
            7*(537585 - 1195711*la + 23709*la^2)*m^4 + 630*(-20 + 29*la)*
             m^6) + 4*En^6*(-1703160 + 16685742*la - 8822991*la^2 - 
            616766*la^3 + 13363*la^4 - 8*(415656 - 1744971*la + 108721*la^2 + 
              3355*la^3)*m^2 - 12*(45915 - 73416*la + 98*la^2)*m^4 + 
            72*(-171 + 64*la)*m^6) + 3*En^2*(-149409225 + 948011428*la - 
            625403014*la^2 + 26118420*la^3 - 24342*la^4 + 
            (-209058150 + 690235670*la - 86558170*la^2 + 809238*la^3)*m^2 - 
            14*(1305495 - 2439121*la + 42861*la^2)*m^4 + 252*(-390 + 427*la)*
             m^6) - 2*En^4*(-70980840 + 488296155*la - 304693251*la^2 + 
            1821782*la^3 + 218003*la^4 + 4*(-30859143 + 99153303*la - 
              11487718*la^2 + 20366*la^3)*m^2 - 28*(568275 - 892646*la + 
              12180*la^2)*m^4 + 56*(-2523 + 1862*la)*m^6)) - 
        6*J^6*(-4203159705 + 35710857798*la - 49895638425*la^2 + 
          12744831783*la^3 - 429970748*la^4 + 2598756*la^5 - 
          7*(375494913 - 1989672024*la + 1447884985*la^2 - 71792300*la^3 + 
            374788*la^4)*m^2 + 7*(-11314521 + 38611641*la - 16439406*la^2 + 
            113372*la^3)*m^4 - 7*(20187 - 43524*la + 10372*la^2)*m^6 + 
          96*En^8*(31464 + 453870*la - 241048*la^2 + 45*(911 + 2008*la)*m^2 + 
            3645*m^4) + 8*En^6*(74747025 - 477418236*la + 391388872*la^2 - 
            30124528*la^3 + 4*(18201015 - 53171081*la + 12911941*la^2)*m^2 + 
            (4632804 - 4986700*la)*m^4 + 14760*m^6) + 
          2*En^4*(-2506515381 + 18759800646*la - 19731907162*la^2 + 
            3013129994*la^3 - 10074500*la^4 + 14*(-168412830 + 654856786*la - 
              283694635*la^2 + 6271168*la^3)*m^2 - 28*(5416815 - 9993170*la + 
              1509632*la^2)*m^4 + 280*(-1587 + 1148*la)*m^6) + 
          2*En^2*(5560034724 - 43613725347*la + 53854432917*la^2 - 
            11416423077*la^3 + 185722324*la^4 - 121644*la^5 + 
            7*(659218356 - 3009404214*la + 1756517531*la^2 - 67350464*la^3 + 
              142380*la^4)*m^2 - 14*(-15311637 + 40152816*la - 
              11635712*la^2 + 43008*la^3)*m^4 + 378*(1371 - 1976*la + 
              236*la^2)*m^6))*M^2 + 12*J^4*(139979505969 + 362657088*En^8 - 
          364472959176*la + 176682716272*la^2 - 15641700129*la^3 + 
          139911360*la^4 + (36142714143 - 53367829299*la + 11100341198*la^2 - 
            133883568*la^3)*m^2 + 5*(76254417 - 62519934*la + 5160644*la^2)*
           m^4 - 24*(-7983 + 2836*la)*m^6 - 72*En^6*(258177655 - 
            226858317*la + 45223690*m^2) + 42*En^4*(4166314716 - 
            6238683096*la + 1156359361*la^2 - 16*(-69215327 + 37924636*la)*
             m^2 + 11445160*m^4) - 7*En^2*(52653238992 - 109483805391*la + 
            37855902058*la^2 - 1783834912*la^3 + 
            2*(7957980072 - 8128911087*la + 877071742*la^2)*m^2 + 
            (195728076 - 80889636*la)*m^4 + 80784*m^6))*M^4 + 
        9*J^2*(896966014944 + 360738785736*En^4 - 736067539930*la + 
          82291222428*la^2 + (75036688467 - 23395196584*la)*m^2 + 
          157734600*m^4 - 24*En^2*(66530156384 - 28545314219*la + 
            4314920946*m^2))*M^6 + 1980424866510*M^8)*rp[t]^18 - 
      J^8*(J^8*la^2*(1040*En^8*(18 - 9*la - 26*la^2 + la^3 - 
            8*(-4 - 3*la + la^2)*m^2 + 4*(1 + la)*m^4) - 
          16*En^6*(183690 - 255885*la - 38834*la^2 + 2809*la^3 - 
            14*(-23866 + 3579*la + 439*la^2)*m^2 + (50050 - 98*la)*m^4 + 
            1026*m^6) + 3*(26729115 - 30493288*la + 3330038*la^2 - 
            83722*la^3 + (25908883 - 4832544*la + 104906*la^2)*m^2 - 
            98*(-13235 + 389*la)*m^4 + 4242*m^6) - 
          6*En^2*(26679345 - 34699043*la + 2213086*la^2 - 82*la^3 + 
            (36772379 - 7777312*la + 118058*la^2)*m^2 - 98*(-32035 + 923*la)*
             m^4 + 16506*m^6) + 4*En^4*(13003515 - 18240516*la + 
            133442*la^2 + 50009*la^3 + 4*(5493110 - 1233703*la + 2987*la^2)*
             m^2 - 196*(-13985 + 381*la)*m^4 + 23660*m^6)) + 
        J^6*(576*En^8*(-37800 + 27960*la + 84564*la^2 + 2347*la^3 + 
            2*(-19320 + 14465*la + 5399*la^2)*m^2 + 5*(-588 + 505*la)*m^4) + 
          96*En^6*(-1987335 + 26460081*la - 54550942*la^2 + 11738753*la^3 + 
            232760*la^4 + (-2664495 + 24872505*la - 20028239*la^2 + 
              495936*la^3)*m^2 + (-305775 + 1568937*la - 429296*la^2)*m^4 + 
            15*(-225 + 328*la)*m^6) + 4*En^4*(227418030 - 5524051779*la + 
            12965878596*la^2 - 4005921036*la^3 + 27211468*la^4 + 
            491284*la^5 + 28*(14809905 - 176119041*la + 186547961*la^2 - 
              10429940*la^3 + 9001*la^4)*m^2 - 14*(-3794310 + 22114803*la - 
              10382276*la^2 + 45444*la^3)*m^4 + 1260*(243 - 708*la + 
              128*la^2)*m^6) - 18*En^2*(130773825 - 2737282899*la + 
            6636981160*la^2 - 2438030492*la^3 + 68545640*la^4 - 90992*la^5 + 
            7*(24989451 - 308278436*la + 376300672*la^2 - 26220160*la^3 + 
              128656*la^4)*m^2 - 14*(-969135 + 6977927*la - 4642616*la^2 + 
              39732*la^3)*m^4 + 42*(1269 - 5520*la + 1988*la^2)*m^6) + 
          3*(295576155 - 6255061836*la + 16041472912*la^2 - 6437484428*la^3 + 
            334386276*la^4 - 3529524*la^5 + 7*(39801987 - 530510148*la + 
              742030448*la^2 - 57569500*la^3 + 518292*la^4)*m^2 - 
            14*(-892665 + 7776858*la - 6717542*la^2 + 79254*la^3)*m^4 + 
            42*(783 - 4526*la + 2438*la^2)*m^6))*M^2 + 
        6*J^4*(49188478887 + 6863616*En^10 - 221026979064*la + 
          180889719265*la^2 - 28179175772*la^3 + 566253624*la^4 - 
          1483160*la^5 + (19079574210 - 51506464890*la + 20412863147*la^2 - 
            556292792*la^3 + 1237096*la^4)*m^2 - 
          10*(-31500126 + 52359105*la - 10912592*la^2 + 31052*la^3)*m^4 + 
          168*(1611 - 1532*la + 136*la^2)*m^6 + 576*En^8*
           (1160975 - 923469*la + 267389*m^2) - 
          48*En^6*(11*(18831438 - 55404247*la + 15500357*la^2) + 
            (105627022 - 95874766*la)*m^2 + 2703380*m^4) + 
          28*En^4*(3074243733 - 9872486088*la + 4766236365*la^2 - 
            283513537*la^3 + 2*(755059545 - 1177437718*la + 176548192*la^2)*
             m^2 + (40640376 - 23334244*la)*m^4 + 30696*m^6) + 
          14*En^2*(-10750904298 + 41691503844*la - 27718130207*la^2 + 
            3117942979*la^3 - 23480944*la^4 + (-5309910702 + 11289117366*la - 
              3122304843*la^2 + 47137400*la^3)*m^2 - 
            2*(59662719 - 66836532*la + 7028416*la^2)*m^4 + 
            72*(-1581 + 752*la)*m^6))*M^4 - 6*J^2*(-726875404794 + 
          69722185536*En^6 + 1097754287517*la - 288970033045*la^2 + 
          10909879987*la^3 + (-105545047788 + 80458528257*la - 
            6791430311*la^2)*m^2 + 60*(-8399037 + 2703685*la)*m^4 - 
          78948*m^6 - 252*En^4*(3153842272 - 1774150561*la + 326837500*m^2) + 
          6*En^2*(7*(44218471041 - 46897055135*la + 6531160984*la^2) + 
            (47293715247 - 18413829176*la)*m^2 + 187264224*m^4))*M^6 - 
        27*(-178281222955 + 199172641728*En^2 + 60956900300*la - 
          5447315508*m^2)*M^8)*rp[t]^19 + 
      J^6*M*(-2*J^8*la*(96*En^8*(-71190 + 20044*la - 4241*la^2 + 4452*la^3 + 
            (-75390 + 14011*la - 6424*la^2)*m^2 + 5*(-1176 + 281*la)*m^4) + 
          8*En^6*(3*(-2575080 + 11211337*la - 5471342*la^2 - 312504*la^3 + 
              7152*la^4) - 4*(2669475 - 6905843*la + 437689*la^2 + 8631*la^3)*
             m^2 - 2*(620985 - 815276*la + 1078*la^2)*m^4 + 
            60*(-225 + 82*la)*m^6) + 3*En^2*(-273279195 + 1708130766*la - 
            1103570340*la^2 + 48821396*la^3 - 90510*la^4 + 
            14*(-25974987 + 84531842*la - 9922637*la^2 + 86199*la^3)*m^2 - 
            14*(1985295 - 3612934*la + 54684*la^2)*m^4 + 252*(-426 + 463*la)*
             m^6) - 2*En^4*(-157210200 + 1169245635*la - 725855004*la^2 + 
            7404004*la^3 + 458086*la^4 + 14*(-20257710 + 65254683*la - 
              7384915*la^2 + 13535*la^3)*m^2 - 28*(1288875 - 1907110*la + 
              20454*la^2)*m^4 + 280*(-732 + 533*la)*m^6) - 
          3*(-104733090 + 643570919*la - 414039943*la^2 + 31669730*la^3 - 
            526819*la^4 + 7*(-13938414 + 48318964*la - 5648375*la^2 + 
              79067*la^3)*m^2 - 7*(613200 - 1346834*la + 24493*la^2)*m^4 + 
            21*(-527 + 761*la)*m^6)) + 6*J^6*(4461768801 - 37822208664*la + 
          52430288461*la^2 - 13135440129*la^3 + 450217720*la^4 - 
          2710636*la^5 + (2582147394 - 13535488374*la + 9723036907*la^2 - 
            455688780*la^3 + 2297268*la^4)*m^2 - 15*(-4268604 + 14418451*la - 
            6078386*la^2 + 38836*la^3)*m^4 + 4*(21159 - 45468*la + 
            10804*la^2)*m^6 + 576*En^10*(33810 - 8891*la + 6510*m^2) + 
          96*En^8*(4*(988086 - 1209190*la + 295661*la^2) - 
            4*(-323035 + 262702*la)*m^2 + 6095*m^4) - 
          8*En^6*(52515747 - 961474566*la + 886343699*la^2 - 69310789*la^3 + 
            2*(69915720 - 222026977*la + 55509657*la^2)*m^2 - 
            760*(-14943 + 14410*la)*m^4 + 18000*m^6) - 
          14*En^4*(-656022915 + 4694169078*la - 4798650722*la^2 + 
            718941434*la^3 - 3115428*la^4 + 4*(-137640759 + 532029226*la - 
              225958637*la^2 + 4633184*la^3)*m^2 - 4*(7789119 - 13802914*la + 
              2013520*la^2)*m^4 + 16*(-3579 + 2566*la)*m^6) - 
          14*En^2*(1062570699 - 8255691108*la + 10083456637*la^2 - 
            2101282157*la^3 + 35901752*la^4 - 29236*la^5 + 
            (835376706 - 3746639022*la + 2140209165*la^2 - 75731792*la^3 + 
              150284*la^4)*m^2 + (31688118 - 81466137*la + 23173768*la^2 - 
              75292*la^3)*m^4 + 36*(1479 - 2120*la + 252*la^2)*m^6))*M^2 - 
        3*J^4*(-450642671856 + 985113792*En^8 + 1166361155214*la - 
          558741606713*la^2 + 48418254813*la^3 - 439518480*la^4 + 
          (-103002556572 + 150505401099*la - 30887549569*la^2 + 
            352064160*la^3)*m^2 - 10*(84951549 - 69112038*la + 5659100*la^2)*
           m^4 + 36*(-8307 + 2944*la)*m^6 + 288*En^6*(483580775 - 
            384834786*la + 68536950*m^2) - 168*En^4*(5201106318 - 
            7654402068*la + 1398288279*la^2 + (1302779542 - 694520392*la)*
             m^2 + 10196336*m^4) + 4*En^2*(363094864398 - 749787566289*la + 
            255902451058*la^2 - 11812139854*la^3 + 
            2*(50207927283 - 50311656177*la + 5302265356*la^2)*m^2 - 
            288*(-3262921 + 1330071*la)*m^4 + 257904*m^6))*M^4 + 
        18*J^2*(160099866612*En^4 + 5*(54056384019 - 43927776377*la + 
            4834835994*la^2) + (18898338204 - 5834488228*la)*m^2 + 
          28938660*m^4 - 9*En^2*(64857220005 - 27480464591*la + 
            3592683056*m^2))*M^6 + 867609145620*M^8)*rp[t]^20 - 
      J^6*(J^8*la^2*(960*En^8*(-2340 + 603*la - 74*la^2 + 7*la^3 + 
            (-2382 + 236*la - 42*la^2)*m^2 + (-177 + 19*la)*m^4) - 
          32*En^6*(704790 - 543699*la - 62982*la^2 + 4605*la^3 - 
            7*(-139210 + 15863*la + 1143*la^2)*m^2 - 7*(-15605 + 49*la)*m^4 + 
            1125*m^6) + 3*(37528365 - 41817756*la + 4646264*la^2 - 
            116346*la^3 + 7*(4915113 - 863502*la + 17938*la^2)*m^2 - 
            98*(-15075 + 403*la)*m^4 + 3724*m^6) - 
          6*En^2*(48741315 - 61446153*la + 4158790*la^2 - 7422*la^3 + 
            7*(9136241 - 1810448*la + 25302*la^2)*m^2 - 490*(-9721 + 237*la)*
             m^4 + 18018*m^6) + 4*En^4*(29126475 - 43785885*la + 
            608560*la^2 + 106222*la^3 + 14*(3606500 - 821563*la + 2267*la^2)*
             m^2 - 98*(-63125 + 1299*la)*m^4 + 34300*m^6)) + 
        J^6*(-2304*En^10*(-25200 + 35280*la - 3098*la^2 + 105*(-84 + 61*la)*
             m^2) - 192*En^8*(-2872800 + 7863792*la - 2967506*la^2 + 
            69627*la^3 + (-980280 + 2545050*la - 501832*la^2)*m^2 + 
            45*(196 + 257*la)*m^4) + 48*En^6*(11661300 + 44582034*la - 
            229400708*la^2 + 54386546*la^3 + 872734*la^4 + 
            (-8264520 + 96301296*la - 85162133*la^2 + 2388064*la^3)*m^2 - 
            90*(20225 - 85167*la + 20776*la^2)*m^4 + 30*(-279 + 400*la)*
             m^6) + 28*En^4*(65330550 - 1438649793*la + 3212300880*la^2 - 
            963275184*la^3 + 8939996*la^4 + 100196*la^5 + 
            2*(47417184 - 576614100*la + 603806345*la^2 - 31507628*la^3 + 
              26536*la^4)*m^2 + (11491740 - 63441678*la + 28512488*la^2 - 
              99624*la^3)*m^4 + 36*(1107 - 3192*la + 572*la^2)*m^6) - 
          42*En^2*(76153905 - 1566540741*la + 3751280192*la^2 - 
            1352549388*la^3 + 40017992*la^4 - 69504*la^5 + 
            (96712371 - 1170403608*la + 1396581368*la^2 - 89357888*la^3 + 
              408864*la^4)*m^2 + (6154740 - 43243014*la + 28154828*la^2 - 
              209496*la^3)*m^4 + 12*(1377 - 5952*la + 2132*la^2)*m^6) + 
          3*(313432875 - 6630334068*la + 16896593460*la^2 - 6639468012*la^3 + 
            350381868*la^4 - 3682428*la^5 + 3*(92222607 - 1214193976*la + 
              1675203880*la^2 - 122325828*la^3 + 1060172*la^4)*m^2 - 
            20*(-510633 + 4395021*la - 3754243*la^2 + 40803*la^3)*m^4 + 
            12*(1647 - 9484*la + 5092*la^2)*m^6))*M^2 - 
        3*J^4*(-79213914072 + 621333504*En^10 + 354290391036*la - 
          286739099764*la^2 + 43656154895*la^3 - 890146496*la^4 + 
          2322744*la^5 + (-27438192954 + 73202884908*la - 28600256999*la^2 + 
            733806440*la^3 - 1578120*la^4)*m^2 + 
          10*(-35358750 + 58254393*la - 12034160*la^2 + 31836*la^3)*m^4 - 
          18*(11763 - 11156*la + 988*la^2)*m^6 - 288*En^8*
           (-20052587 + 1468842*la + 587680*m^2) + 
          192*En^6*(3*(79134438 - 186132455*la + 47254332*la^2) + 
            (77132487 - 72822347*la)*m^2 + 1844202*m^4) - 
          56*En^4*(3944837943 - 12286125786*la + 5823980769*la^2 - 
            341731079*la^3 + 2*(911954520 - 1379417795*la + 199503569*la^2)*
             m^2 - 8*(-4648269 + 2592481*la)*m^4 + 16968*m^6) - 
          4*En^2*(-74279070864 + 286586822664*la - 188358107519*la^2 + 
            20749404691*la^3 - 162866072*la^4 + (-34121240748 + 
              70959011046*la - 19130113865*la^2 + 263150872*la^3)*m^2 - 
            72*(8070877 - 8897581*la + 921128*la^2)*m^4 + 
            216*(-1689 + 800*la)*m^6))*M^4 - 3*J^2*(164576928384*En^6 + 
          5*(-175615373715 + 262777504570*la - 68099483450*la^2 + 
            2505490966*la^3) + (-107154736179 + 80801092696*la - 
            6725071548*la^2)*m^2 + 120*(-3099245 + 990863*la)*m^4 - 
          36384*m^6 - 72*En^4*(19627507412 - 10994318067*la + 
            1889068496*m^2) + 6*En^2*(454165573599 - 476494913502*la + 
            65111315680*la^2 + (59992642965 - 22887289152*la)*m^2 + 
            166173792*m^4))*M^6 - 54*(-39190626601 + 53254694916*En^2 + 
          13225039836*la - 926760432*m^2)*M^8)*rp[t]^21 + 
      J^4*M*(2*J^8*la*(576*En^10*(-32970 + 14293*la - 107*la^2 + 
            70*(-168 + 29*la)*m^2) + 96*En^8*(-1872360 + 1424768*la - 
            79417*la^2 - 14508*la^3 + 2*(-326025 + 211899*la + 7694*la^2)*
             m^2 + 5*(1176 + 323*la)*m^4) - 8*En^6*(23136300 + 40060596*la - 
            38109390*la^2 - 1883678*la^3 + 44569*la^4 - 
            2*(8182320 - 27505912*la + 2310777*la^2 + 28021*la^3)*m^2 - 
            10*(368295 - 393484*la + 392*la^2)*m^4 + 60*(-279 + 100*la)*
             m^6) - 21*En^2*(-52981605 + 324598856*la - 205103472*la^2 + 
            9572388*la^3 - 25650*la^4 + 2*(-33515361 + 106527545*la - 
              11415305*la^2 + 91719*la^3)*m^2 + (-4195500 + 7439458*la - 
              96558*la^2)*m^4 + 24*(-462 + 499*la)*m^6) + 
          3*(-111057390 + 679850746*la - 427329934*la^2 + 33209856*la^3 - 
            549817*la^4 + (-96808167 + 330727346*la - 36191323*la^2 + 
              485855*la^3)*m^2 - 5*(700626 - 1519307*la + 25277*la^2)*m^4 + 
            12*(-554 + 797*la)*m^6) + 14*En^4*(-45248220 + 299989515*la - 
            175654974*la^2 + 2633120*la^3 + 94310*la^4 + 
            (-65114016 + 213578202*la - 22950820*la^2 + 41732*la^3)*m^2 - 
            4*(1947525 - 2720426*la + 22638*la^2)*m^4 + 8*(-3333 + 2402*la)*
             m^6)) - 3*J^6*(-7182119430 + 14999040*En^12 + 60723302952*la - 
          83347736439*la^2 + 20371660433*la^3 - 708386908*la^4 + 
          4246364*la^5 + (-3755288772 + 19420122696*la - 13737747413*la^2 + 
            603382020*la^3 - 2933380*la^4)*m^2 + 5*(-14503383 + 48489105*la - 
            20240910*la^2 + 119644*la^3)*m^4 - 3*(22131 - 47412*la + 
            11236*la^2)*m^6 + 1152*En^10*(788235 - 369062*la + 64330*m^2) - 
          96*En^8*(-40814292 + 38699041*la + 295538*la^2 + 
            (588050 - 1178360*la)*m^2 + 119410*m^4) + 
          16*En^6*(312439095 - 2114141334*la + 1558592074*la^2 - 
            112419144*la^3 + 2*(83361558 - 327203145*la + 84520765*la^2)*
             m^2 + (16810284 - 14901780*la)*m^4 + 12744*m^6) + 
          28*En^4*(-899618697 + 5994000234*la - 5943522418*la^2 + 
            876029586*la^3 - 4656196*la^4 + 2*(-342633264 + 1281440110*la - 
              523269867*la^2 + 9513888*la^3)*m^2 - 8*(3686091 - 6300136*la + 
              890236*la^2)*m^4 + 32*(-996 + 709*la)*m^6) + 
          4*En^2*(7351446852 - 56932983981*la + 68897510155*la^2 - 
            14063020103*la^3 + 250723084*la^4 - 239140*la^5 + 
            (5493004344 - 23999407170*la + 13327009795*la^2 - 
              426470976*la^3 + 792572*la^4)*m^2 - 
            4*(-39250161 + 99002754*la - 27665136*la^2 + 78904*la^3)*m^4 + 
            108*(1587 - 2264*la + 268*la^2)*m^6))*M^2 + 
        3*J^4*(10096619328*En^8 + 5*(54555062832 - 140017802560*la + 
            66047371015*la^2 - 5566357973*la^3 + 51204936*la^4) - 
          2*(-26384287959 + 38083770453*la - 7699080958*la^2 + 82369280*la^3)*
           m^2 + 10*(31550067 - 25470594*la + 2069132*la^2)*m^4 - 
          56*(-1233 + 436*la)*m^6 - 288*En^6*(618482460 - 452781133*la + 
            82853408*m^2) + 24*En^4*(32327662974 - 47631151392*la + 
            8632827401*la^2 - 8*(-974237977 + 498493302*la)*m^2 + 
            40414096*m^4) - 6*En^2*(177939756720 - 364558313067*la + 
            122265299756*la^2 - 5476910509*la^3 + 
            (43268681436 - 42349453370*la + 4342749676*la^2)*m^2 - 
            4*(-70289337 + 28279747*la)*m^4 + 45576*m^6))*M^4 + 
        18*J^2*(119298134381 + 104143389966*En^4 - 95679370634*la + 
          10322538760*la^2 + (6479286811 - 1978591908*la)*m^2 + 6369996*m^4 - 
          6*En^2*(52415687672 - 21780557597*la + 2278877850*m^2))*M^6 + 
        263660328180*M^8)*rp[t]^22 - 
      J^4*(J^8*la^2*(11520*En^10*(567 - 38*la - 3*la^2 + 7*(29 + la)*m^2) + 
          320*En^8*(186696 - 11121*la - 928*la^2 + 77*la^3 + 
            (66058 + 2148*la - 322*la^2)*m^2 + (-463 + 125*la)*m^4) - 
          42*En^2*(9428265 - 11478013*la + 822016*la^2 - 2758*la^3 + 
            (11779115 - 2116606*la + 27074*la^2)*m^2 - 56*(-12810 + 263*la)*
             m^4 + 1860*m^6) + 3*(39801255 - 43187900*la + 4876036*la^2 - 
            121474*la^3 + (34102369 - 5567994*la + 110406*la^2)*m^2 - 
            210*(-5733 + 139*la)*m^4 + 2236*m^6) - 
          16*En^6*(-3440700 - 2567829*la - 284968*la^2 + 19481*la^3 - 
            35*(-88404 + 19319*la + 749*la^2)*m^2 - 210*(-3015 + 7*la)*m^4 + 
            2790*m^6) + 28*En^4*(8270145 - 10614027*la + 234884*la^2 + 
            22106*la^3 + 2*(5816620 - 1326551*la + 3799*la^2)*m^2 - 
            70*(-18989 + 291*la)*m^4 + 4460*m^6)) + 
        J^6*(483840*En^12*(-84 + 61*la) - 9*(-83891115 + 1775965784*la - 
            4491320356*la^2 + 1718214908*la^3 - 91978016*la^4 + 
            961810*la^5) + 9*(68018253 - 881253946*la + 1195688032*la^2 - 
            81350290*la^3 + 677630*la^4)*m^2 - 30*(-584586 + 4970958*la - 
            4199275*la^2 + 41979*la^3)*m^4 + 54*(432 - 2479*la + 1327*la^2)*
           m^6 + 2304*En^10*(-404460 + 789810*la - 102071*la^2 + 
            12915*(-4 + 5*la)*m^2) - 96*En^8*(22706460 - 80564748*la + 
            17044355*la^2 + 169005*la^3 - 20*(52164 - 51738*la + 32113*la^2)*
             m^2 + 30*(-3528 + 7909*la)*m^4) + 48*En^6*
           (-309960 + 225378174*la - 459618651*la^2 + 87978173*la^3 + 
            1031336*la^4 + 3*(-21192 + 39198824*la - 42479507*la^2 + 
              1249920*la^3)*m^2 - 2*(1526085 - 5657007*la + 1264256*la^2)*
             m^4 + 18*(-333 + 472*la)*m^6) + 28*En^4*(110082240 - 
            1954914327*la + 4066527144*la^2 - 1187243616*la^3 + 
            14037772*la^4 + 99604*la^5 + 2*(60761232 - 717167364*la + 
              719954317*la^2 - 33064300*la^3 + 26726*la^4)*m^2 - 
            4*(-2843235 + 14977275*la - 6474604*la^2 + 18060*la^3)*m^4 + 
            36*(621 - 1776*la + 316*la^2)*m^6) - 
          6*En^2*(528444945 - 10829052147*la + 25768942196*la^2 - 
            9110610408*la^3 + 281702168*la^4 - 589296*la^5 + 
            (655434585 - 7678571640*la + 8873346704*la^2 - 508499792*la^3 + 
              2163696*la^4)*m^2 - 4*(-7788825 + 53466633*la - 34100096*la^2 + 
              220332*la^3)*m^4 + 36*(1485 - 6384*la + 2276*la^2)*m^6))*M^2 + 
        J^4*(10220981760*En^10 + 5*(28819322424 - 127996302108*la + 
            102060900308*la^2 - 15077020973*la^3 + 311435112*la^4 - 
            809240*la^5) + (42640444818 - 112180217346*la + 
            43122512363*la^2 - 1033573000*la^3 + 2147240*la^4)*m^2 - 
          10*(-39691674 + 64822041*la - 13274448*la^2 + 32620*la^3)*m^4 + 
          12*(12249 - 11588*la + 1024*la^2)*m^6 - 
          864*En^8*(-25380469 + 24084190*la + 1378036*m^2) - 
          288*En^6*(739825476 - 1411280939*la + 333103189*la^2 - 
            20*(-10075285 + 8739996*la)*m^2 + 2984332*m^4) + 
          24*En^4*(24400634265 - 76557155766*la + 36252507495*la^2 - 
            2104301977*la^3 + 4*(2854690443 - 4100381048*la + 563291336*la^2)*
             m^2 - 40*(-3772209 + 2050975*la)*m^4 + 37176*m^6) + 
          6*En^2*(-109059130836 + 419670068058*la - 271816773723*la^2 + 
            29045280705*la^3 - 235860788*la^4 + (-45160547166 + 
              91324000758*la - 23881913683*la^2 + 294050056*la^3)*m^2 + 
            (-528645042 + 574044036*la - 58556768*la^2)*m^4 + 
            108*(-1797 + 848*la)*m^6))*M^4 - 3*J^2*(-389263230645 + 
          118948836096*En^6 + 574917201652*la - 145976916200*la^2 + 
          5192481748*la^3 + (-37065496857 + 27610310108*la - 2263044912*la^2)*
           m^2 + 24*(-3430061 + 1089275*la)*m^4 - 3768*m^6 - 
          36*En^4*(25479329942 - 14219742121*la + 2123241956*m^2) + 
          6*En^2*(246533147165 - 253950546011*la + 33719955432*la^2 + 
            (25826673601 - 9629302104*la)*m^2 + 43544688*m^4))*M^6 - 
        54*(-11977933121 + 20093267604*En^2 + 3973349276*la - 192470046*m^2)*
         M^8)*rp[t]^23 + J^2*M*(J^8*la*(-161280*En^12*(-168 + 29*la) - 
          1152*En^10*(-534870 + 272061*la - 3894*la^2 + 70*(-984 + 311*la)*
             m^2) + 192*En^8*(7573230 - 6345866*la - 104459*la^2 - 
            34352*la^3 + 5*(-65856 + 22829*la + 3452*la^2)*m^2 + 
            5*(-7056 + 3877*la)*m^4) - 16*En^6*(-2784780 + 138284802*la - 
            59264481*la^2 - 2426516*la^3 + 62356*la^4 - 
            2*(67446 - 35480004*la + 3925537*la^2 + 29869*la^3)*m^2 - 
            6*(1026345 - 960372*la + 686*la^2)*m^4 + 36*(-333 + 118*la)*
             m^6) + 3*(-178385940 + 1088270146*la - 664397821*la^2 + 
            52367590*la^3 - 862029*la^4 + (-142631466 + 478289888*la - 
              48397525*la^2 + 621915*la^3)*m^2 - 5*(801039 - 1715090*la + 
              26061*la^2)*m^4 + 63*(-83 + 119*la)*m^6) - 
          6*En^2*(-367232805 + 2239915046*la - 1391724912*la^2 + 
            68014684*la^3 - 230586*la^4 + 6*(-75662779 + 231559244*la - 
              21934875*la^2 + 162473*la^3)*m^2 - 4*(5300415 - 9171146*la + 
              101976*la^2)*m^4 + 72*(-498 + 535*la)*m^6) + 
          28*En^4*(-75378060 + 399943635*la - 218809008*la^2 + 4410040*la^3 + 
            94678*la^4 + (-83741088 + 263795220*la - 24769478*la^2 + 
              43486*la^3)*m^2 - 4*(1923375 - 2557078*la + 16548*la^2)*m^4 + 
            8*(-1869 + 1336*la)*m^6)) + J^6*(785272320*En^12 - 
          5*(-2614643766 + 22006182486*la - 29795336689*la^2 + 
            7046410399*la^3 - 248135310*la^4 + 1480012*la^5) + 
          (5916904344 - 30099772902*la + 20917391951*la^2 - 853217180*la^3 + 
            3995060*la^4)*m^2 - 5*(-16424874 + 54359937*la - 22472342*la^2 + 
            122780*la^3)*m^4 + (46206 - 98712*la + 23336*la^2)*m^6 + 
          3456*En^10*(3258423 - 1950705*la + 10220*m^2) + 
          288*En^8*(11099628 - 51076119*la + 12653930*la^2 + 
            72*(-122665 + 35879*la)*m^2 + 213930*m^4) - 
          48*En^6*(833161221 - 3199644486*la + 1934276319*la^2 - 
            134746467*la^3 + 4*(62018586 - 212459221*la + 49657573*la^2)*
             m^2 - 4*(-3599433 + 3008345*la)*m^4 + 4896*m^6) - 
          12*En^4*(-5544804591 + 37229012946*la - 37229945786*la^2 + 
            5479082578*la^3 - 33957412*la^4 + 4*(-1144362759 + 
              3988504102*la - 1529925142*la^2 + 23389024*la^3)*m^2 - 
            8*(15394377 - 25512452*la + 3506396*la^2)*m^4 + 
            16*(-4389 + 3106*la)*m^6) - 6*En^2*(10695216240 - 
            83460231594*la + 100088616543*la^2 - 19831607073*la^3 + 
            365881474*la^4 - 392244*la^5 + (7496811000 - 31637742138*la + 
              16968677885*la^2 - 480730448*la^3 + 835356*la^4)*m^2 + 
            (145145772 - 359652033*la + 98836072*la^2 - 247548*la^3)*m^4 + 
            54*(1695 - 2408*la + 284*la^2)*m^6))*M^2 + 
        6*J^4*(60752356125 + 8237096064*En^8 - 153951593754*la + 
          71124540222*la^2 - 5776966787*la^3 + 53760216*la^4 + 
          (9223599612 - 13130353738*la + 2610922655*la^2 - 26037648*la^3)*
           m^2 + (35140311 - 28155402*la + 2269556*la^2)*m^4 + 
          (3582 - 1264*la)*m^6 - 1152*En^6*(51546195 - 42129068*la + 
            9248056*m^2) + 6*En^4*(41301141096 - 61755632772*la + 
            11055848479*la^2 + (9136690718 - 4449280168*la)*m^2 + 
            26333992*m^4) - En^2*(291659736594 - 588180601427*la + 
            191785998766*la^2 - 8198148818*la^3 + 
            (57113851422 - 54421036222*la + 5414740080*la^2)*m^2 - 
            192*(-1163593 + 462453*la)*m^4 + 16056*m^6))*M^4 + 
        18*J^2*(36704504425 + 49928452326*En^4 - 28918272098*la + 
          3040980568*la^2 + (1356979328 - 409484796*la)*m^2 + 637068*m^4 - 
          6*En^2*(20069595293 - 8082011156*la + 585452412*m^2))*M^6 + 
        49984373106*M^8)*rp[t]^24 + 
      J^2*(-(J^8*la^2*(-161280*En^12*(29 + la) + 3840*En^10*
            (-26775 + 450*la - 33*la^2 + 7*(-485 + 7*la)*m^2) + 
           160*En^8*(-1492776 - 48801*la - 7790*la^2 + 343*la^3 + 
             (72200 + 4860*la - 980*la^2)*m^2 + (7366 + 310*la)*m^4) + 
           3*(31980315 - 33622316*la + 3849443*la^2 - 95281*la^3 + 
             (25085011 - 3747900*la + 70775*la^2)*m^2 - 35*(-19635 + 431*la)*
              m^4 + 879*m^6) - 16*En^6*(1278540 - 3401547*la - 416992*la^2 + 
             27671*la^3 - 7*(-96196 + 184311*la + 4021*la^2)*m^2 - 
             14*(-74755 + 119*la)*m^4 + 1998*m^6) + 
           28*En^4*(13386285 - 13289049*la + 425428*la^2 + 22438*la^3 + 
             20*(746840 - 148639*la + 419*la^2)*m^2 - 28*(-46705 + 537*la)*
              m^4 + 2500*m^6) - 6*En^2*(65264025 - 78525685*la + 
             5904312*la^2 - 27754*la^3 + (79606233 - 12403562*la + 144638*
                la^2)*m^2 - 196*(-18455 + 319*la)*m^4 + 6012*m^6))) + 
        J^6*(483840*En^12*(-1068 + 1079*la) + 5*(-91408905 + 1935689652*la - 
            4841302922*la^2 + 1786159088*la^3 - 96785832*la^4 + 
            1006158*la^5) + (-327272913 + 4156140426*la - 5528402282*la^2 + 
            346712270*la^3 - 2771730*la^4)*m^2 + 15*(-446049 + 3748080*la - 
            3131858*la^2 + 28770*la^3)*m^4 - 3*(1809 - 10348*la + 5524*la^2)*
           m^6 + 2304*En^10*(-1311030 + 3234378*la - 473855*la^2 + 
            210*(81 + 49*la)*m^2) + 288*En^8*(1780380 + 7769052*la - 
            8338087*la^2 - 92189*la^3 + 8*(317415 - 733525*la + 41532*la^2)*
             m^2 + 10*(-8232 + 14225*la)*m^4) - 48*En^6*(-68631570 + 
            570276294*la - 656500031*la^2 + 107432525*la^3 + 674858*la^4 + 
            2*(-1625679 + 87359895*la - 80786951*la^2 + 1895712*la^3)*m^2 - 
            18*(156845 - 537289*la + 112952*la^2)*m^4 + 6*(-387 + 544*la)*
             m^6) - 4*En^4*(703998540 - 12066655365*la + 25508533548*la^2 - 
            7562612460*la^3 + 106857788*la^4 + 479396*la^5 + 
            4*(226057527 - 2384609883*la + 2205202411*la^2 - 82995820*la^3 + 
              63680*la^4)*m^2 - 4*(-12315915 + 62428161*la - 26111780*la^2 + 
              58548*la^3)*m^4 + 36*(1377 - 3912*la + 692*la^2)*m^6) + 
          18*En^2*(124442325 - 2628310149*la + 6270624462*la^2 - 
            2160179704*la^3 + 69139788*la^4 - 165152*la^5 + 
            (155567361 - 1740974314*la + 1930838368*la^2 - 96541096*la^3 + 
              381328*la^4)*m^2 + (4896015 - 32897846*la + 20583242*la^2 - 
              115584*la^3)*m^4 + 3*(1593 - 6816*la + 2420*la^2)*m^6))*M^2 + 
        J^4*(-64513602009 + 9489125376*En^10 + 283113515016*la - 
          221067132061*la^2 + 31351446028*la^3 - 654775272*la^4 + 
          1693336*la^5 + (-15099968538 + 39093164178*la - 14758902035*la^2 + 
            327857368*la^3 - 657320*la^4)*m^2 + 2*(-44534178 + 72109089*la - 
            14641296*la^2 + 33404*la^3)*m^4 - 6*(2547 - 2404*la + 212*la^2)*
           m^6 - 3456*En^8*(18179339 - 9691765*la + 472378*m^2) + 
          576*En^6*(216663504 - 490172553*la + 130684078*la^2 + 
            (104522404 - 77049944*la)*m^2 + 654866*m^4) - 
          12*En^4*(29643692769 - 98356321428*la + 46870645215*la^2 - 
            2651367391*la^3 + 2*(7080846291 - 9543339061*la + 
              1233969601*la^2)*m^2 + (100315632 - 53365828*la)*m^4 + 
            10104*m^6) - 6*En^2*(-59758396812 + 227682032958*la - 
            143660604125*la^2 + 14606041985*la^3 - 121651832*la^4 + 
            (-20423058252 + 39988492842*la - 10108054279*la^2 + 
              109551864*la^3)*m^2 - 4*(35445387 - 37953021*la + 3818648*la^2)*
             m^4 + 12*(-1905 + 896*la)*m^6))*M^4 + 
        3*J^2*(-120708047049 + 62532463104*En^6 + 174994116260*la - 
          43253179840*la^2 + 1471706948*la^3 + (-7839802005 + 5762461144*la - 
            464635188*la^2)*m^2 + 24*(-344917 + 108817*la)*m^4 - 
          36*En^4*(12449950562 - 6696467949*la + 721402536*m^2) + 
          6*En^2*(96033228533 - 95689535345*la + 12154174112*la^2 + 
            (6763884367 - 2461139760*la)*m^2 + 5114064*m^4))*M^6 + 
        27*(-4580745429 + 9696958872*En^2 + 1486474964*la - 36879138*m^2)*
         M^8)*rp[t]^25 + (J^8*la*(161280*En^12*(-2136 + 535*la) - 
          5*(64822140 - 393336591*la + 230713380*la^2 - 18397278*la^3 + 
            300769*la^4) + (-228392862 + 748664751*la - 69143485*la^2 + 
            849055*la^3)*m^2 - 5*(915615 - 1936143*la + 26845*la^2)*m^4 + 
          6*(-608 + 869*la)*m^6 + 1152*En^10*(-1747200 + 1052876*la + 
            3295*la^2 + 140*(162 + 25*la)*m^2) + 192*En^8*
           (1602720 + 1960114*la + 258991*la^2 - 52532*la^3 + 
            6*(426790 - 228127*la + 908*la^2)*m^2 + 5*(-16464 + 7057*la)*
             m^4) - 16*En^6*(3*(-46384740 + 102913233*la - 23929681*la^2 - 
              597634*la^3 + 19817*la^4) - 4*(1853484 - 25431511*la + 
              2109569*la^2 + 10479*la^3)*m^2 + (-5685510 + 4901456*la - 
              2548*la^2)*m^4 + 12*(-387 + 136*la)*m^6) - 
          6*En^2*(-259745355 + 1632754839*la - 1000537926*la^2 + 
            50629690*la^3 - 201273*la^4 + (-322668258 + 937821233*la - 
              75933005*la^2 + 517467*la^3)*m^2 + (-9979455 + 16883731*la - 
              161091*la^2)*m^4 + 18*(-534 + 571*la)*m^6) + 
          4*En^4*(-476867520 + 2490392595*la - 1423848354*la^2 + 
            35501852*la^3 + 460598*la^4 + 4*(-155592609 + 432142131*la - 
              31915414*la^2 + 53198*la^3)*m^2 - 4*(8316375 - 10618766*la + 
              54012*la^2)*m^4 + 8*(-4143 + 2942*la)*m^6))*M + 
        J^6*(5881932990 - 3071416320*En^12 - 48998124606*la + 
          64989861035*la^2 - 14683594513*la^3 + 522446980*la^4 - 
          3098428*la^5 + (2129407245 - 10627623708*la + 7239410929*la^2 - 
            271744428*la^3 + 1224132*la^4)*m^2 + (18587925 - 60914889*la + 
            24945134*la^2 - 125916*la^3)*m^4 + 5*(963 - 2052*la + 484*la^2)*
           m^6 + 20736*En^10*(-351141 + 301034*la + 29050*m^2) + 
          576*En^8*(51701058 - 71705916*la + 10124204*la^2 - 
            (1382545 + 2037648*la)*m^2 + 77065*m^4) - 
          48*En^6*(440356203 - 1950226668*la + 1458424512*la^2 - 
            119693680*la^3 + 4*(87485547 - 217130041*la + 42343213*la^2)*
             m^2 + (6575532 - 5273540*la)*m^4 + 792*m^6) - 
          12*En^4*(-2908254429 + 22966542189*la - 24127623061*la^2 + 
            3525453709*la^3 - 24260514*la^4 + (-3078469542 + 9823110194*la - 
              3495394681*la^2 + 42934336*la^3)*m^2 + 
            (-41935062 + 67731692*la - 9091568*la^2)*m^4 + (-9588 + 6752*la)*
             m^6) - 2*En^2*(17432903808 - 136675715934*la + 
            160555312799*la^2 - 30200559671*la^3 + 570865308*la^4 - 
            667268*la^5 + (10542656376 - 42730095102*la + 22036792015*la^2 - 
              541979456*la^3 + 879772*la^4)*m^2 - 
            2*(-59259807 + 144462708*la - 39092832*la^2 + 86128*la^3)*m^4 + 
            18*(1803 - 2552*la + 300*la^2)*m^6))*M^3 - 
        6*J^4*(-19017148233 + 1285203456*En^8 + 47258750504*la - 
          21222980906*la^2 + 1640603695*la^3 - 15413584*la^4 + 
          (-1974060231 + 2767716103*la - 540655054*la^2 + 4991952*la^3)*m^2 + 
          (-3555585 + 2827950*la - 226244*la^2)*m^4 + 
          72*En^6*(345732039 - 365497465*la + 78829530*m^2) - 
          6*En^4*(20467370780 - 29730076992*la + 5046684775*la^2 - 
            8*(-405086659 + 187610074*la)*m^2 + 3758504*m^4) + 
          En^2*(115935443760 - 225706301885*la + 70192746118*la^2 - 
            2786100500*la^3 + 2*(7655811972 - 7090308245*la + 683556354*la^2)*
             m^2 - 12*(-2208379 + 867793*la)*m^4))*M^5 + 
        9*J^2*(33127759764*En^4 + 10*(1418287657 - 1091339411*la + 
            111096622*la^2) + (262384697 - 78183408*la)*m^2 - 
          12*En^2*(4963531936 - 1908700895*la + 68954754*m^2))*M^7 + 
        4476971250*M^9)*rp[t]^26 + 
      (-(J^8*la^2*(58149675 - 58527890*la + 6774315*la^2 - 166345*la^3 + 
           161280*En^12*(359 + 3*la) + 7*(5726961 - 770250*la + 13825*la^2)*
            m^2 - 35*(-22411 + 445*la)*m^4 + 613*m^6 - 
           7680*En^10*(-43344 - 447*la + 15*la^2 + 7*(82 + la)*m^2) + 
           480*En^8*(-93264 + 20733*la - 5486*la^2 + 161*la^3 - 
             4*(41208 - la + 77*la^2)*m^2 + (5562 + 74*la)*m^4) - 
           32*En^6*(18*(660110 - 96962*la - 9763*la^2 + 742*la^3) - 
             7*(-149204 + 107659*la + 1419*la^2)*m^2 - 7*(-68605 + 77*la)*
              m^4 + 387*m^6) - 6*En^2*(46279155 - 57201417*la + 
             4453889*la^2 - 25803*la^3 + (56376649 - 7273858*la + 77157*la^2)*
              m^2 - 7*(-242735 + 3543*la)*m^4 + 1611*m^6) + 
           4*En^4*(83650095 - 88549503*la + 3683096*la^2 + 110450*la^3 + 
             4*(27457640 - 3964411*la + 10679*la^2)*m^2 + 
             (5637380 - 49476*la)*m^4 + 5540*m^6))) - 
        J^6*(206283105 - 4341115242*la + 10652943562*la^2 - 3731593698*la^3 + 
          204127062*la^4 - 2107638*la^5 + 5806080*En^12*(-285 + 353*la) + 
          (120275253 - 1491847494*la + 1940083766*la^2 - 110955870*la^3 + 
            850218*la^4)*m^2 - 3*(-509985 + 4236258*la - 3502466*la^2 + 
            29554*la^3)*m^4 + 21*(27 - 154*la + 82*la^2)*m^6 - 
          4608*En^10*(252945 - 1056573*la + 212963*la^2 + 105*(-555 + 833*la)*
             m^2) - 192*En^8*(-25371360 + 102240846*la - 34423302*la^2 + 
            70331*la^3 - 6*(-502530 + 430405*la + 203373*la^2)*m^2 + 
            15*(-6468 + 10261*la)*m^4) + 96*En^6*(-21246435 + 151069941*la - 
            221871960*la^2 + 50632721*la^3 + 51448*la^4 + 
            (-12662811 + 120728271*la - 79136923*la^2 + 1187776*la^3)*m^2 + 
            (-677565 + 2205411*la - 444208*la^2)*m^4 + 3*(-63 + 88*la)*m^6) + 
          4*En^4*(231063840 - 6495462693*la + 16192977678*la^2 - 
            4997744802*la^3 + 79265422*la^4 + 221050*la^5 + 
            2*(347208498 - 3186816000*la + 2666236672*la^2 - 77657080*la^3 + 
              56183*la^4)*m^2 + (17274600 - 84890247*la + 34546948*la^2 - 
              62916*la^3)*m^4 + 36*(189 - 534*la + 94*la^2)*m^6) - 
          2*En^2*(580060845 - 12862242423*la + 30513176428*la^2 - 
            9983435774*la^3 + 326868068*la^4 - 858552*la^5 + 
            (689803317 - 7316352558*la + 7742167644*la^2 - 329921456*la^3 + 
              1208568*la^4)*m^2 + (12208455 - 80465391*la + 49471592*la^2 - 
              242004*la^3)*m^4 + 3*(1701 - 7248*la + 2564*la^2)*m^6))*M^2 + 
        J^4*(-20428670853 + 8896117248*En^10 + 87815084160*la - 
          66545736851*la^2 + 8924272352*la^3 - 188007848*la^4 + 483624*la^5 + 
          (-3278578032 + 8338959654*la - 3087419893*la^2 + 63080360*la^3 - 
            121944*la^4)*m^2 + 6*(-1512774 + 2429169*la - 489168*la^2 + 
            1036*la^3)*m^4 - 3456*En^8*(-4230522 + 1091279*la + 
            1348511*m^2) + 144*En^6*(119580240 - 900617603*la + 
            296878467*la^2 - 6*(-42033253 + 27062521*la)*m^2 + 480408*m^4) + 
          36*En^4*(-4855995279 + 16093423364*la - 7315180405*la^2 + 
            379201133*la^3 - 2*(885137467 - 1120326982*la + 136321328*la^2)*
             m^2 + 4*(-1213648 + 633807*la)*m^4) + 
          6*En^2*(24324922692 - 89306211660*la + 53569364797*la^2 - 
            5010687361*la^3 + 42399776*la^4 + (5637004854 - 10667933634*la + 
              2602565353*la^2 - 24482568*la^3)*m^2 + 
            6*(2835299 - 2997080*la + 297760*la^2)*m^4))*M^4 + 
        9*J^2*(-7873138163 + 11701588032*En^6 + 11123960426*la - 
          2655239862*la^2 + 85236654*la^3 + (-255359987 + 185047178*la - 
            14666570*la^2)*m^2 - 12*En^4*(4393359556 - 2148716971*la + 
            110726580*m^2) + 6*En^2*(8165887553 - 7721125887*la + 
            920023224*la^2 + (270805857 - 96147048*la)*m^2))*M^6 + 
        27*(-415238757 + 1133144712*En^2 + 131124964*la)*M^8)*rp[t]^27 + 
      (J^6*la*(-146346480 + 875360142*la - 483526157*la^2 + 38879058*la^3 - 
          630485*la^4 - 5806080*En^12*(-190 + 59*la) + 
          (-83768289 + 267451662*la - 22255643*la^2 + 260779*la^3)*m^2 + 
          (-1045530 + 2184426*la - 27629*la^2)*m^4 + (-381 + 543*la)*m^6 + 
          2304*En^10*(342720 - 347161*la - 5970*la^2 + 70*(-1110 + 421*la)*
             m^2) + 192*En^8*(-16998030 + 16376078*la + 371135*la^2 - 
            48268*la^3 + (2006550 - 297441*la - 6176*la^2)*m^2 + 
            15*(-4312 + 1703*la)*m^4) - 16*En^6*(-83670300 + 172601661*la - 
            73818138*la^2 - 466408*la^3 + 38144*la^4 - 
            4*(13026741 - 32818231*la + 1384521*la^2 + 4675*la^3)*m^2 + 
            (-2725590 + 2228456*la - 868*la^2)*m^4 + 12*(-63 + 22*la)*m^6) - 
          2*En^2*(-405589275 + 2659720764*la - 1564071317*la^2 + 
            80763710*la^3 - 358101*la^4 + (-475615620 + 1302586630*la - 
              87597185*la^2 + 548739*la^3)*m^2 + (-8282565 + 13732682*la - 
              112812*la^2)*m^4 + 6*(-570 + 607*la)*m^6) + 
          4*En^4*(-155964690 + 1422412980*la - 974776776*la^2 + 
            27643402*la^3 + 215083*la^4 + (-475823262 + 1135303917*la - 
              61166537*la^2 + 95869*la^3)*m^2 - 4*(2911800 - 3599212*la + 
              14595*la^2)*m^4 + 4*(-1137 + 803*la)*m^6))*M + 
        J^4*(1889669520 + 3721697280*En^12 - 15397849644*la + 
          19785371375*la^2 - 4191456639*la^3 + 150270064*la^4 - 885428*la^5 + 
          (470747880 - 2300239110*la + 1533218133*la^2 - 52500660*la^3 + 
            227308*la^4)*m^2 + (1910016 - 6200091*la + 2515906*la^2 - 
            11732*la^3)*m^4 + 3456*En^10*(-3098256 + 1660427*la + 
            28070*m^2) + 576*En^8*(-7679358 + 16861854*la + 237184*la^2 + 
            (4920620 - 5495280*la)*m^2 + 20125*m^4) + 
          24*En^6*(11*(33159780 + 74097792*la - 140029689*la^2 + 
              13462351*la^3) - 78*(6586750 - 13277141*la + 2224501*la^2)*
             m^2 + 12*(-206839 + 161025*la)*m^4) + 
          12*En^4*(1271744631 - 11298965571*la + 11601297927*la^2 - 
            1552553459*la^3 + 11338270*la^4 - 2*(-622745640 + 1829356272*la - 
              604853585*la^2 + 5804096*la^3)*m^2 + 6*(1035105 - 1636570*la + 
              215336*la^2)*m^4) + 2*En^2*(-7287465159 + 55051875921*la - 
            61280016005*la^2 + 10480150613*la^3 - 200722368*la^4 + 
            249908*la^5 + (-3021083640 + 11739516270*la - 5812221541*la^2 + 
              122152016*la^3 - 185164*la^4)*m^2 + (-14412042 + 34618329*la - 
              9237192*la^2 + 17948*la^3)*m^4))*M^3 - 
        3*J^2*(-7562739648 + 2510238816*En^8 + 18263644642*la - 
          7897123869*la^2 + 571505243*la^3 - 5408160*la^4 + 
          (-390735672 + 539008815*la - 103345183*la^2 + 877856*la^3)*m^2 + 
          144*En^6*(246295515 - 192059194*la + 17833750*m^2) + 
          12*En^4*(-7878600554 + 10134248484*la - 1523187725*la^2 + 
            (-517244578 + 228914696*la)*m^2) + 6*En^2*(10265303250 - 
            18790359727*la + 5438618446*la^2 - 192988146*la^3 + 
            (627606978 - 564990142*la + 52784632*la^2)*m^2))*M^5 + 
        54*(217443432 + 939455718*En^4 - 162385645*la + 15882894*la^2 + 
          3*En^2*(-399927995 + 145084231*la))*M^7)*rp[t]^28 + 
      (-(J^6*la^2*(26271405 + 645120*En^12*(-284 + la) - 24632282*la + 
           2870222*la^2 - 69803*la^3 + (14667646 - 1747881*la + 29769*la^2)*
            m^2 - 7*(-25555 + 459*la)*m^4 + 64*m^6 - 23040*En^10*
            (5754 + 135*la - 5*la^2 + 7*(-182 + 3*la)*m^2) + 
           320*En^8*(1694610 + 81219*la - 8942*la^2 + 217*la^3 - 
             2*(94307 + 1116*la + 133*la^2)*m^2 + (6511 + 43*la)*m^4) - 
           32*En^6*(6940080 - 2135085*la - 60322*la^2 + 8657*la^3 + 
             (4697532 - 524833*la - 4453*la^2)*m^2 - 21*(-10925 + 9*la)*m^4 + 
             63*m^6) - 2*En^2*(72769635 - 91171629*la + 7213283*la^2 - 
             47821*la^3 + (82719007 - 8524686*la + 82199*la^2)*m^2 - 
             7*(-201095 + 2491*la)*m^4 + 573*m^6) + 
           4*En^4*(28406115 - 63767886*la + 3056182*la^2 + 52279*la^3 + 
             (82879090 - 7832267*la + 19843*la^2)*m^2 - 7*(-281305 + 1923*la)*
              m^4 + 760*m^6))) + 2*J^4*(-33759180 + 693790947*la - 
          1646340394*la^2 + 534441201*la^3 - 29416017*la^4 + 301353*la^5 + 
          967680*En^12*(-897 + 1283*la) + (-13608000 + 164431566*la - 
            208617065*la^2 + 10770765*la^3 - 79023*la^4)*m^2 + 
          (-79380 + 652149*la - 533721*la^2 + 4137*la^3)*m^4 + 
          1152*En^10*(1302210 - 3061086*la + 374888*la^2 + 315*(-106 + 91*la)*
             m^2) + 96*En^8*(-491400 - 16666218*la + 7510560*la^2 + 
            781079*la^3 - 6*(314370 - 1667635*la + 478044*la^2)*m^2 + 
            105*(-252 + 383*la)*m^4) + 12*En^6*(-64029420 + 223642056*la + 
            317998628*la^2 - 136321892*la^3 + 344050*la^4 + 
            (54443340 - 350177610*la + 182952571*la^2 - 1677984*la^3)*m^2 + 
            108*(4900 - 15401*la + 3008*la^2)*m^4) + 
          En^2*(247813020 - 5347055187*la + 11998773268*la^2 - 
            3514755542*la^3 + 116154356*la^4 - 326208*la^5 + 
            8*(25988445 - 260994681*la + 263141091*la^2 - 9388774*la^3 + 
              31890*la^4)*m^2 + (1508220 - 9771093*la + 5913302*la^2 - 
              25284*la^3)*m^4) + 2*En^4*(-15331680 + 2917252989*la - 
            7930995316*la^2 + 2276875276*la^3 - 38279418*la^4 - 63854*la^5 + 
            (-314038620 + 2562721038*la - 1953167893*la^2 + 42732620*la^3 - 
              29048*la^4)*m^2 + (-2619540 + 12555387*la - 4995124*la^2 + 
              7476*la^3)*m^4))*M^2 - J^2*(4148408736 + 10543592448*En^10 - 
          17257016352*la + 12543829385*la^2 - 1558936001*la^3 + 
          33039032*la^4 - 84480*la^5 + 2*(164761668 - 411230871*la + 
            149163896*la^2 - 2783248*la^3 + 5184*la^4)*m^2 + 
          432*En^8*(-50035067 + 8066486*la + 5052632*m^2) + 
          288*En^6*(-102178518 + 291716542*la - 69676742*la^2 + 
            3*(-10333439 + 6075397*la)*m^2) + 36*En^4*(2145482529 - 
            5973808310*la + 2333876685*la^2 - 99340479*la^3 + 
            (296576064 - 355449478*la + 40948498*la^2)*m^2) + 
          6*En^2*(-6832977750 + 23242401408*la - 12831682027*la^2 + 
            1052668623*la^3 - 8960664*la^4 + (-713373048 + 1305633918*la - 
              307559605*la^2 + 2485272*la^3)*m^2))*M^4 + 
        18*(-368828564 + 2022574752*En^6 + 503958937*la - 115158435*la^2 + 
          3429067*la^3 + 6*En^4*(-813671144 + 351723239*la) + 
          3*En^2*(1030046532 - 910425885*la + 100042928*la^2))*M^6)*
       rp[t]^29 + (-(J^4*la*(47873700 - 276481077*la + 139114894*la^2 - 
           11232940*la^3 + 180447*la^4 - 645120*En^12*(-1794 + 643*la) + 
           (18911340 - 58658142*la + 4346305*la^2 - 48537*la^3)*m^2 + 
           3*(36120 - 74603*la + 861*la^2)*m^4 - 1152*En^10*
            (-3*(-578550 + 327129*la + 5015*la^2) + 70*(-636 + 145*la)*m^2) - 
           192*En^8*(-3*(68040 + 1030954*la + 72933*la^2 + 8284*la^3) - 
             6*(212275 - 291579*la + 1006*la^2)*m^2 + 35*(-504 + 191*la)*
              m^4) - 8*En^6*(-131564790 + 67647603*la + 114049542*la^2 - 
             602026*la^3 - 31537*la^4 + 2*(55353060 - 91864212*la + 2026531*
                la^2 + 4819*la^3)*m^2 + 36*(29540 - 23294*la + 7*la^2)*m^4) + 
           4*En^4*(12644100 - 665324535*la + 465352879*la^2 - 13915584*la^3 - 
             63181*la^4 + (214177950 - 449721157*la + 17190170*la^2 - 25210*
                la^3)*m^2 + 4*(441000 - 531080*la + 1743*la^2)*m^4) - 
           2*En^2*(174017970 - 1087687299*la + 561091559*la^2 - 
             29075954*la^3 + 138777*la^4 + (142901010 - 368605495*la + 
               20186537*la^2 - 116247*la^3)*m^2 + (1021860 - 1664093*la + 
               11823*la^2)*m^4))*M) - 2*J^2*(717776640*En^12 + 
          1728*En^10*(-2665593 + 2041794*la + 65170*m^2) + 
          216*En^8*(29651826 - 31412243*la - 465414*la^2 + 
            4*(-976965 + 850556*la)*m^2) + 72*En^6*(-958536 - 165902947*la + 
            141692483*la^2 - 9271108*la^3 + (23485170 - 42124113*la + 
              6368573*la^2)*m^2) + 6*En^4*(-746268282 + 4800244665*la - 
            4002827147*la^2 + 418761963*la^3 - 3122806*la^4 + 
            (-221946480 + 609449118*la - 189099085*la^2 + 1402976*la^3)*
             m^2) - 2*(98761032 - 773657163*la + 948751197*la^2 - 
            183701693*la^3 + 6614950*la^4 - 38692*la^5 + 
            (12057660 - 57609414*la + 37531657*la^2 - 1163090*la^3 + 
              4836*la^4)*m^2) + 3*En^2*(742573332 - 5058561672*la + 
            5095050159*la^2 - 743845955*la^3 + 14269052*la^4 - 18580*la^5 + 
            (132115320 - 493029978*la + 234587055*la^2 - 4167360*la^3 + 
              5900*la^4)*m^2))*M^3 + 3*(725538528 + 4288912416*En^8 - 
          1686063712*la + 694722879*la^2 - 46119199*la^3 + 438440*la^4 + 
          144*En^6*(-114492192 + 61468973*la) + 36*En^4*(546500594 - 
            601170792*la + 76913799*la^2) + 6*En^2*(-1369688400 + 
            2312835122*la - 610486838*la^2 + 18535625*la^3))*M^5)*rp[t]^30 + 
      (-(J^4*la^2*(8587215 - 645120*En^12*(-298 + la) - 7128834*la + 
           831776*la^2 - 19999*la^3 + (3302110 - 343821*la + 5549*la^2)*m^2 - 
           7*(-2645 + 43*la)*m^4 - 3840*En^10*(85869 + 1584*la - 75*la^2 + 
             7*(-301 + 17*la)*m^2) + 8*En^6*(22054410 + 8946801*la - 
             80792*la^2 - 14435*la^3 + (-19103350 + 802119*la + 4609*la^2)*
              m^2 + 112*(-1595 + la)*m^4) + 320*En^8*(630 + 18597*la - 
             5012*la^2 + 121*la^3 - 2*(-67543 + 876*la + 43*la^2)*m^2 + 
             7*(253 + la)*m^4) - 12*En^4*(-1241625 + 10892180*la - 
             541040*la^2 - 5209*la^3 + (-12306350 + 753309*la - 1781*la^2)*
              m^2 + 49*(-2025 + 11*la)*m^4) + En^2*(-62622000 + 67126350*la - 
             5279452*la^2 + 38150*la^3 + (-49466900 + 3988482*la - 34978*
                la^2)*m^2 + 28*(-12385 + 131*la)*m^4))) - 
        2*J^2*(7371000 - 143917596*la + 323571910*la^2 - 94115579*la^3 + 
          5191911*la^4 - 52716*la^5 + 241920*En^12*(-1236 + 1975*la) + 
          4*(357210 - 4199148*la + 5191043*la^2 - 239797*la^3 + 1683*la^4)*
           m^2 + 1152*En^10*(842310 - 2679348*la + 513005*la^2 + 
            105*(-378 + 619*la)*m^2) + 72*En^8*(-14689080 + 58871430*la - 
            12388281*la^2 - 1089361*la^3 + 8*(277830 - 984840*la + 
              217217*la^2)*m^2) + 12*En^6*(27234900 + 14461998*la - 
            254029631*la^2 + 55661487*la^3 - 237192*la^4 + 
            (-17384220 + 95425842*la - 43827911*la^2 + 255488*la^3)*m^2) - 
          6*En^2*(14696640 - 268336566*la + 528741621*la^2 - 126879326*la^3 + 
            4174070*la^4 - 12268*la^5 + 2*(2381400 - 22737231*la + 
              21890931*la^2 - 646846*la^3 + 2038*la^4)*m^2) + 
          2*En^4*(2*(35068950 - 817392960*la + 1548372500*la^2 - 
              318941306*la^3 + 5423461*la^4 + 4991*la^5) + 
            (60725700 - 454383342*la + 320985965*la^2 - 5246316*la^3 + 
              3346*la^4)*m^2))*M^2 + 3*(-136928016 + 725193216*En^10 + 
          544139728*la - 374641717*la^2 + 42084275*la^3 - 894528*la^4 + 
          2272*la^5 + 144*En^8*(-29870233 + 21120514*la) + 
          144*En^6*(56294118 - 82343571*la + 12730319*la^2) + 
          12*En^4*(-530273661 + 1193285246*la - 382324221*la^2 + 
            11973655*la^3) + 4*En^2*(492494823 - 1514083188*la + 
            750641636*la^2 - 51173533*la^3 + 433938*la^4))*M^4)*rp[t]^31 + 
      2*(-(J^2*la*(5212620 - 28186164*la + 12321181*la^2 - 994169*la^3 + 
           15798*la^4 + 80640*En^12*(-2472 + 983*la) - 
           6*(-165060 + 496740*la - 32447*la^2 + 345*la^3)*m^2 + 
           576*En^10*(1127910 - 899039*la + 1202*la^2 + 70*(-756 + 307*la)*
              m^2) + 48*En^8*(-14796180 + 14043443*la + 989744*la^2 + 
             12316*la^3 + (2235240 - 2008106*la + 3104*la^2)*m^2) - 
           4*En^6*(-54384750 - 36145437*la + 52749915*la^2 - 522244*la^3 - 
             7588*la^4 + 2*(17592120 - 24609384*la + 317179*la^2 + 547*la^3)*
              m^2) - 2*En^4*(-49395150 + 333864465*la - 137316202*la^2 + 
             4084420*la^3 + 10145*la^4 + (-41258700 + 78841254*la - 2150269*
                la^2 + 2945*la^3)*m^2) + 3*En^2*(-20553120 + 105270312*la - 
             41430429*la^2 + 2118038*la^3 - 10601*la^4 + 
             (-6526800 + 15932068*la - 703485*la^2 + 3727*la^3)*m^2))*M) + 
        (20450880 + 68947200*En^12 - 151279884*la + 174277028*la^2 - 
          29886641*la^3 + 1077011*la^4 - 6248*la^5 + 63936*En^10*
           (-11445 + 11693*la) + 216*En^8*(9591990 - 20893409*la + 
            4108310*la^2) + 36*En^6*(-70643790 + 239084480*la - 
            98976089*la^2 + 3118541*la^3) + 6*En^4*(245574720 - 
            1138340307*la + 731806217*la^2 - 52016461*la^3 + 384330*la^4) + 
          6*En^2*(-59965380 + 356608857*la - 315294061*la^2 + 36710656*la^3 - 
            697393*la^4 + 936*la^5))*M^3)*rp[t]^32 + 
      2*((-1 + En^2)*J^2*la^2*(931770 - 636475*la + 73882*la^2 - 1753*la^3 + 
          80640*En^10*(415 + 3*la) + 3*(57470 - 5171*la + 79*la^2)*m^2 + 
          1920*En^8*(-39333 + 222*la - 51*la^2 + 7*(383 + 5*la)*m^2) + 
          120*En^6*(357966 + 56439*la + 878*la^2 - 51*la^3 + 
            16*(-6790 + 71*la + la^2)*m^2) - 4*En^4*(-2054430 + 3445933*la - 
            120784*la^2 - 1967*la^3 + 15*(-181986 + 6373*la + 3*la^2)*m^2) - 
          2*En^2*(5022990 - 3516973*la + 256420*la^2 - 1357*la^3 + 
            6*(266770 - 16329*la + 121*la^2)*m^2)) + 
        3*(-272160 + 4916808*la - 10259562*la^2 + 2566391*la^3 - 
          141267*la^4 + 1420*la^5 + 2177280*En^12*(-4 + 7*la) + 
          1152*En^10*(34020 - 144060*la + 39019*la^2) + 
          72*En^8*(-975240 + 6602918*la - 3936753*la^2 + 113615*la^3) + 
          12*En^6*(5239080 - 49098486*la + 46051701*la^2 - 3345125*la^3 + 
            16314*la^4) - 4*En^4*(7144200 - 86168466*la + 111433631*la^2 - 
            13723047*la^3 + 228021*la^4 + 95*la^5) - 
          4*En^2*(-1428840 + 21275694*la - 35466713*la^2 + 6382885*la^3 - 
            206274*la^4 + 624*la^5))*M^2)*rp[t]^33 + 
      12*(-1 + En^2)*la*(95760 - 471162*la + 169247*la^2 - 13570*la^3 + 
        213*la^4 + 40320*En^10*(-72 + 31*la) + 
        96*En^8*(106680 - 140106*la + 2609*la^2) - 
        8*En^6*(1683360 - 3703105*la + 339180*la^2 + 904*la^3) + 
        En^4*(7917840 - 24486784*la + 4248610*la^2 - 84380*la^3 - 538*la^4) + 
        En^2*(-1884960 + 7532186*la - 1968055*la^2 + 92526*la^3 - 333*la^4))*
       M*rp[t]^34 - 4*(-1 + En^2)^2*la^2*(51030 - 26501*la + 3038*la^2 - 
        71*la^3 + 40320*En^8*(37 + la) + 11520*En^6*(-329 + 36*la + la^2) + 
        60*En^4*(53130 - 12963*la + 274*la^2 + 7*la^3) + 
        4*En^2*(-234990 + 89153*la - 5954*la^2 + 23*la^3))*rp[t]^35))/
    (En*la*(1 + la)*rp[t]^18*(3*M + la*rp[t])^2*(J^2 + rp[t]^2)^13))*
  Derivative[1][rp][t]
	
]


Clear[fSourceZM7]
fSourceZM7[syms_Association]:=
Module[{mu,M,J,la,YBar,YPhiBar,YPhiPhiBar,En,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	
	(8*J*mu*Pi*YPhiBar*(2*M - rp[t])^3*(6830208*J^22*M^7 + 
    18*J^22*(-859789 + 140883*la)*M^6*rp[t] + 
    3*J^20*M^5*(J^2*(4761371 - 1917677*la + 10616*la^2) + 25044096*M^2)*
     rp[t]^2 + 2*J^20*M^4*(J^2*(-3422185 + 2659257*la - 31834*la^2 + 
        308*la^3) + 9*(-9443115 + 1782075*En^2 + 1564277*la)*M^2)*rp[t]^3 + 
    J^18*M^3*(2*J^4*(892685 - 1276966*la + 24943*la^2 - 574*la^3) + 
      3*J^2*(52200785 - 21236719*la + 148800*la^2 + 
        6*En^2*(-3196811 + 610787*la))*M^2 + 375661440*M^4)*rp[t]^4 + 
    J^18*M^2*(J^4*(-238488 + 668143*la - 19067*la^2 + 798*la^3) - 
      4*J^2*(18720019 - 14679876*la + 222871*la^2 - 1946*la^3 + 
        3*En^2*(-3328864 + 1656667*la + 9882*la^2))*M^2 + 
      18*(-47137819 + 18478104*En^2 + 7899141*la)*M^4)*rp[t]^5 + 
    J^16*M*(J^6*(12600 - 89605*la + 3534*la^2 - 245*la^3) - 
      2*J^4*(-9742123 + 14048869*la - 348618*la^2 + 7238*la^3 + 
        6*En^2*(1109003 - 1161905*la - 16202*la^2 + 70*la^3))*M^2 + 
      3*J^2*(260070837 + 10379556*En^4 - 106945579*la + 915104*la^2 + 
        6*En^2*(-33121859 + 6414212*la))*M^4 + 1126984320*M^6)*rp[t]^6 + 
    J^16*(28*J^6*la*(170 - 9*la + la^2) + 
      J^4*(2*En^2*(1051740 - 2353310*la - 57965*la^2 + 588*la^3) + 
        3*(-865006 + 2440239*la - 88561*la^2 + 3346*la^3))*M^2 + 
      2*J^2*(-186104803 + 147379533*la - 2740528*la^2 + 22256*la^3 + 
        54*En^4*(-379362 + 93151*la) - 3*En^2*(-68915644 + 34690181*la + 
          117680*la^2))*M^4 + 18*(-141163541 + 86436849*En^2 + 23947339*la)*
       M^6)*rp[t]^7 + J^14*M*(J^6*(136620 - 977133*la + 48998*la^2 - 
        3073*la^3 + En^2*(-124020 + 756608*la + 29474*la^2 - 546*la^3)) - 
      4*J^4*(-24146770 + 35128824*la - 1070866*la^2 + 20660*la^3 + 
        6*En^4*(-796772 + 561554*la + 11607*la^2) + 
        3*En^2*(11466697 - 12123481*la - 102988*la^2 + 392*la^3))*M^2 + 
      3*J^2*(777203943 + 101607660*En^4 - 323295041*la + 3295576*la^2 + 
        6*En^2*(-154812476 + 30416205*la))*M^4 + 2253968640*M^6)*rp[t]^8 + 
    J^14*(7*J^6*la*(7375 - 495*la + 50*la^2 + 
        6*En^2*(-1085 - 63*la + 2*la^2)) - 2*J^4*(6410883 - 18224335*la + 
        814826*la^2 - 28596*la^3 + En^2*(-10860534 + 24461917*la + 
          392236*la^2 - 3444*la^3) + 2*En^4*(920808 - 1609746*la - 
          77237*la^2 + 546*la^3))*M^2 + 2*J^2*(-554770002 + 5087736*En^6 + 
        444023082*la - 9872212*la^2 + 76064*la^3 + 
        18*En^4*(-11157766 + 2775543*la) - 9*En^2*(-107262726 + 54663609*la + 
          35384*la^2))*M^4 + 36*(-140894857 + 120161952*En^2 + 24216023*la)*
       M^6)*rp[t]^9 + J^12*M*(J^6*(672345 - 4839635*la + 299672*la^2 - 
        17468*la^3 - 20*En^2*(63945 - 391658*la - 10601*la^2 + 168*la^3) + 
        4*En^4*(59895 - 319414*la - 26878*la^2 + 483*la^3)) + 
      2*J^4*(143525670 - 210821157*la + 7714853*la^2 - 141040*la^3 + 
        72*En^6*(-59134 + 21627*la) - 60*En^4*(-1565086 + 1112868*la + 
          18543*la^2) + 6*En^2*(-53474775 + 57105452*la + 148672*la^2 + 
          36*la^3))*M^2 + 18*J^2*(257985797 + 74155512*En^4 - 108649939*la + 
        1295784*la^2 + En^2*(-430067956 + 85819840*la))*M^4 + 3155556096*M^6)*
     rp[t]^10 + J^12*(J^6*la*(254135 - 21081*la + 1984*la^2 - 
        420*En^4*(-205 - 27*la + la^2) + 42*En^2*(-11190 - 482*la + 
          13*la^2)) - 2*J^4*(18982467 - 54422535*la + 2933454*la^2 - 
        97464*la^3 + 12*En^6*(-87997 + 112779*la + 5479*la^2) - 
        3*En^2*(16857447 - 38250964*la - 252572*la^2 + 396*la^3) + 
        2*En^4*(9064143 - 15915099*la - 633913*la^2 + 4746*la^3))*M^2 + 
      4*J^2*(-550929009 + 23865480*En^6 + 446072649*la - 11653354*la^2 + 
        86348*la^3 + 36*En^4*(-12235547 + 3086851*la) + 
        6*En^2*(223236183 - 115307863*la + 264766*la^2))*M^4 + 
      756*(-9373667 + 10478251*En^2 + 1633725*la)*M^6)*rp[t]^11 + 
    J^10*M*(J^6*(1981755 - 14368935*la + 1077238*la^2 - 59432*la^3 - 
        24*En^6*(6060 - 29353*la - 3366*la^2 + 35*la^3) - 
        30*En^2*(198177 - 1219406*la - 16843*la^2 + 90*la^3) + 
        4*En^4*(591525 - 3153199*la - 227479*la^2 + 4263*la^3)) + 
      4*J^4*(142050165 + 230904*En^8 - 210880764*la + 9111821*la^2 - 
        159970*la^3 + 288*En^6*(-69909 + 25738*la) - 
        6*En^4*(-34391101 + 24698358*la + 304442*la^2) + 
        3*En^2*(-148183605 + 160001502*la - 609402*la^2 + 6260*la^3))*M^2 + 
      42*J^2*(154087995 + 81818856*En^4 - 65765765*la + 905152*la^2 + 
        18*En^2*(-18734525 + 3802099*la))*M^4 + 3155556096*M^6)*rp[t]^12 + 
    J^10*(J^6*la*(749325 - 75539*la + 6736*la^2 + 
        112*En^6*(-455 - 92*la + 3*la^2) - 1260*En^4*(-675 - 79*la + 
          3*la^2) + 18*En^2*(-121400 - 3166*la + 39*la^2)) + 
      2*J^4*(72*En^8*(-2297 + 1880*la) - 12*En^6*(-841139 + 1075270*la + 
          45390*la^2) - 6*En^2*(-23317728 + 53348363*la - 204393*la^2 + 
          7710*la^3) - 4*En^4*(19960623 - 35241600*la - 1086220*la^2 + 
          8538*la^3) + 3*(-12472311 + 36101040*la - 2310307*la^2 + 
          73622*la^3))*M^2 + 8*J^2*(49604580*En^6 + 
        18*En^4*(-31568101 + 8083413*la) + 21*En^2*(58274907 - 30552494*la + 
          167148*la^2) + 7*(-54674679 + 44831442*la - 1358245*la^2 + 
          9770*la^3))*M^4 + 252*(-28058261 + 39652392*En^2 + 4963915*la)*M^6)*
     rp[t]^13 + J^8*M*(J^6*(3885705 - 28410195*la + 2544302*la^2 - 
        134518*la^3 - 48*En^8*(-390 + 2207*la + 279*la^2) - 
        24*En^6*(58860 - 281251*la - 29009*la^2 + 322*la^3) + 
        6*En^2*(-2736405 + 16922111*la - 18615*la^2 + 6030*la^3) + 
        4*En^4*(2611080 - 13939496*la - 815303*la^2 + 15732*la^3)) + 
      4*J^4*(2194344*En^8 + 576*En^6*(-146337 + 54506*la) - 
        6*En^4*(-88985099 + 64532698*la + 493702*la^2) + 
        21*En^2*(-38617299 + 42224096*la - 458454*la^2 + 3512*la^3) - 
        7*(-28086825 + 42191181*la - 2126356*la^2 + 36182*la^3))*M^2 + 
      42*J^2*(153322923 + 136650564*En^4 - 66392693*la + 1043296*la^2 + 
        6*En^2*(-70835087 + 14641740*la))*M^4 + 2253968640*M^6)*rp[t]^14 + 
    J^8*(J^6*la*(1469835 - 178141*la + 15224*la^2 + 
        En^4*(3757920 + 374292*la - 14388*la^2) - 
        48*En^8*(-130 - 39*la + la^2) + 56*En^6*(-8840 - 1673*la + 57*la^2) - 
        30*En^2*(201163 + 1346*la + 145*la^2)) + 
      2*J^4*(72*En^8*(-22735 + 17964*la) - 12*En^6*(-3536143 + 4562234*la + 
          157562*la^2) - 21*En^2*(-12126459 + 28017158*la - 436020*la^2 + 
          9108*la^3) - 4*En^4*(51882417 - 91894530*la - 1936712*la^2 + 
          15174*la^3) + 21*(-2454165 + 7180988*la - 539661*la^2 + 
          16642*la^3))*M^2 + 4*J^2*(240770088*En^6 + 
        18*En^4*(-105632468 + 27531479*la) + 
        63*En^2*(48897402 - 26064747*la + 231980*la^2) + 
        7*(-108434481 + 90155943*la - 3135808*la^2 + 22040*la^3))*M^4 + 
      36*(-139950797 + 244565937*En^2 + 25160083*la)*M^6)*rp[t]^15 + 
    J^6*M*(J^6*(-240*En^8*(-858 + 4349*la + 517*la^2) - 
        24*En^6*(243720 - 1193667*la - 103623*la^2 + 1294*la^3) + 
        42*En^2*(-709650 + 4417186*la - 80885*la^2 + 3840*la^3) + 
        4*En^4*(6849000 - 36333544*la - 1597081*la^2 + 29172*la^3) - 
        7*(-759825 + 5610015*la - 594718*la^2 + 30386*la^3)) + 
      4*J^4*(8851032*En^8 + 144*En^6*(-1446921 + 536806*la) - 
        6*En^4*(-149162881 + 109578106*la + 259288*la^2) + 
        14*(13865442 - 21105177*la + 1229251*la^2 - 20402*la^3) + 
        21*En^2*(-48504441 + 53809408*la - 1003654*la^2 + 7116*la^3))*M^2 + 
      18*J^2*(254146677 + 359696316*En^4 - 111796259*la + 1988584*la^2 + 
        14*En^2*(-62373886 + 13148923*la))*M^4 + 1126984320*M^6)*rp[t]^16 + 
    J^6*(J^6*la*(En^4*(9848640 + 811644*la - 27996*la^2) - 
        528*En^8*(-130 - 39*la + la^2) - 42*En^2*(260865 - 4264*la + 
          521*la^2) + 8*En^6*(-258640 - 42121*la + 1689*la^2) + 
        7*(287565 - 41639*la + 3436*la^2)) + 
      2*J^4*(72*En^8*(-83233 + 76504*la) - 12*En^6*(-9061871 + 11339918*la + 
          316886*la^2) + En^4*(-348791076 + 621908616*la + 6067540*la^2 - 
          40944*la^3) - 42*En^2*(-7592244 + 17754250*la - 513511*la^2 + 
          9414*la^3) + 21*(-2409411 + 7138287*la - 624994*la^2 + 18764*la^3))*
       M^2 + 4*J^2*(367763040*En^6 + 18*En^4*(-119339608 + 31788509*la) + 
        21*En^2*(129009636 - 70032499*la + 881020*la^2) + 
        2*(-268574307 + 226752027*la - 8982382*la^2 + 61964*la^3))*M^4 + 
      54*(-46526747 + 99246208*En^2 + 8510213*la)*M^6)*rp[t]^17 + 
    J^4*M*(J^6*(-48*En^8*(-1290 + 86247*la + 7231*la^2) - 
        8*En^6*(2056680 - 9145797*la - 690875*la^2 + 7966*la^3) + 
        42*En^2*(-884670 + 5558141*la - 214961*la^2 + 8154*la^3) + 
        4*En^4*(11576925 - 61270928*la - 1636919*la^2 + 22530*la^3) - 
        7*(-740385 + 5530413*la - 689894*la^2 + 34252*la^3)) + 
      4*J^4*(136684770 + 24498504*En^8 - 211174599*la + 14113891*la^2 - 
        229460*la^3 + 360*En^6*(-890268 + 333707*la) + 
        6*En^4*(168587027 - 126148994*la + 481188*la^2) + 
        21*En^2*(-42563451 + 48004098*la - 1307466*la^2 + 8884*la^3))*M^2 + 
      3*J^2*(757870743 + 1656401040*En^4 - 339143441*la + 6780376*la^2 + 
        72*En^2*(-44293983 + 9531080*la))*M^4 + 375661440*M^6)*rp[t]^18 + 
    J^4*(J^6*la*(-240*En^8*(-190 - 27*la + 13*la^2) - 
        126*En^2*(108440 - 4882*la + 383*la^2) - 
        12*En^4*(-1385785 - 86461*la + 2049*la^2) + 
        8*En^6*(-720980 - 109383*la + 3647*la^2) + 
        7*(280395 - 48373*la + 3872*la^2)) - 
      2*J^4*(35391033 + 730944*En^10 - 106375185*la + 10789026*la^2 - 
        316596*la^3 - 10008*En^8*(-2373 + 1492*la) + 
        60*En^6*(-2818733 + 3509248*la + 72232*la^2) - 
        4*En^4*(-98387694 + 178056753*la - 769579*la^2 + 9312*la^3) + 
        21*En^2*(-13274169 + 31497522*la - 1390694*la^2 + 23796*la^3))*M^2 + 
      2*J^2*(-531611877 + 737340336*En^6 + 456527157*la - 20463362*la^2 + 
        138964*la^3 + 72*En^4*(-45943669 + 12537187*la) + 
        12*En^2*(274666077 - 152008303*la + 2502522*la^2))*M^4 + 
      18*(-46392039 + 120049959*En^2 + 8644921*la)*M^6)*rp[t]^19 + 
    (-(J^8*(-3591945 + 27209985*la - 3980122*la^2 + 192668*la^3 + 
         1152*En^10*(-1020 + 253*la) + 48*En^8*(-121950 + 320057*la + 
           22945*la^2) + 40*En^6*(659016 - 2828355*la - 187109*la^2 + 
           1612*la^3) - 210*En^2*(-153675 + 977922*la - 61613*la^2 + 
           2094*la^3) + 4*En^4*(-12989295 + 69587674*la + 227083*la^2 + 
           11862*la^3))*M) + 2*J^6*(134495745 + 63724752*En^8 - 
        211331682*la + 16117903*la^2 - 257390*la^3 + 
        576*En^6*(-1110103 + 431474*la) + 60*En^4*(26005595 - 19891770*la + 
          216402*la^2) + 6*En^2*(-180926655 + 207738562*la - 7588878*la^2 + 
          49756*la^3))*M^3 + 3*J^4*(250952357 + 841498992*En^4 - 
        114423259*la + 2555904*la^2 + 6*En^2*(-214546571 + 47119695*la))*
       M^5 + 75132288*J^2*M^7)*rp[t]^20 + 
    (J^8*la*(11520*En^10*(33 + 5*la) - 240*En^8*(-8210 - 1009*la + 31*la^2) + 
        60*En^4*(311235 + 9571*la + 85*la^2) + 
        40*En^6*(-229400 - 37221*la + 785*la^2) - 
        42*En^2*(282790 - 23098*la + 1507*la^2) + 
        7*(194505 - 39983*la + 3112*la^2)) + 
      J^6*(9038592*En^10 + 144*En^8*(-399643 + 257752*la) - 
        24*En^6*(-13713137 + 17875874*la + 145194*la^2) + 
        8*En^4*(-75721689 + 139903512*la - 3025016*la^2 + 25002*la^3) - 
        12*En^2*(-28108776 + 67802517*la - 4151579*la^2 + 67242*la^3) + 
        3*(-11516178 + 35203515*la - 4119161*la^2 + 118426*la^3))*M^2 + 
      4*J^4*(-87582059 + 248942520*En^6 + 76648824*la - 3866779*la^2 + 
        25898*la^3 + 36*En^4*(-23537907 + 6562381*la) + 
        27*En^2*(24653734 - 13909989*la + 284418*la^2))*M^4 + 
      18*J^2*(-9248975 + 29008632*En^2 + 1758417*la)*M^6)*rp[t]^21 + 
    (-(J^6*(-1733355 + 483840*En^12 + 13358460*la - 2287388*la^2 + 
         108157*la^3 - 4608*En^10*(-1010 + 643*la) + 
         48*En^8*(-134190 + 354853*la + 40685*la^2) + 
         8*En^6*(2947680 - 13875459*la - 570353*la^2 + 2290*la^3) - 
         90*En^2*(-215319 + 1391531*la - 127579*la^2 + 3990*la^3) + 
         4*En^4*(-9874350 + 54178304*la - 1573771*la^2 + 41772*la^3))*M) + 
      2*J^4*(44018255 + 50013936*En^8 - 70510453*la + 6109542*la^2 - 
        95990*la^3 + 576*En^6*(-758914 + 305273*la) + 
        12*En^4*(67198961 - 52353118*la + 956438*la^2) + 
        6*En^2*(-73067460 + 85410527*la - 3952952*la^2 + 24990*la^3))*M^3 + 
      3*J^2*(49823009 + 258664644*En^4 - 23187583*la + 575712*la^2 + 
        18*En^2*(-17329025 + 3879356*la))*M^5 + 6830208*M^7)*rp[t]^22 + 
    (-(J^6*la*(161280*En^12 + 11520*En^10*(134 + la) + 
         En^6*(8263840 + 1242024*la - 11176*la^2) + 
         240*En^8*(-8730 - 3137*la + 31*la^2) - 60*En^4*(237390 - 5399*la + 
           559*la^2) + 42*En^2*(170025 - 21806*la + 1249*la^2) - 
         7*(93975 - 23077*la + 1748*la^2))) + 
      J^4*(-11199516 - 2658816*En^10 + 34924315*la - 4700867*la^2 + 
        132582*la^3 + 144*En^8*(-202909 + 211828*la) + 
        24*En^6*(9353989 - 12738710*la + 126170*la^2) + 
        8*En^4*(-39429963 + 73961250*la - 3013300*la^2 + 21702*la^3) - 
        6*En^2*(-22659891 + 55560196*la - 4428931*la^2 + 68040*la^3))*M^2 + 
      2*J^2*(-34580927 + 215247600*En^6 + 30909969*la - 1746976*la^2 + 
        11552*la^3 + 54*En^4*(-9809958 + 2768479*la) + 
        3*En^2*(107839856 - 61930197*la + 1509784*la^2))*M^4 + 
      18*(-837889 + 3195453*En^2 + 162783*la)*M^6)*rp[t]^23 + 
    (J^4*(553905 + 2419200*En^12 - 4360870*la + 873952*la^2 - 40393*la^3 - 
        2304*En^10*(-470 + 633*la) - 48*En^8*(39750 + 170379*la + 
          23779*la^2) + 8*En^6*(-1871100 + 9771843*la - 111673*la^2 + 
          2714*la^3) + 6*En^2*(-1294230 + 8492506*la - 1054540*la^2 + 
          30555*la^3) - 4*En^4*(-5147730 + 28653736*la - 1951277*la^2 + 
          38028*la^3))*M + 4*J^2*(4310870 + 19715400*En^8 - 7059434*la + 
        692408*la^2 - 10712*la^3 + 432*En^6*(-231581 + 92118*la) + 
        6*En^4*(21453079 - 16803548*la + 419663*la^2) + 
        3*En^2*(-17805428 + 21143687*la - 1185178*la^2 + 7204*la^3))*M^3 + 
      3*(4492587 + 36737676*En^4 - 2138333*la + 58744*la^2 + 
        6*En^2*(-5755592 + 1309673*la))*M^5)*rp[t]^24 + 
    2*((-1 + En^2)*J^4*la*(-105280 + 403200*En^10 + 31033*la - 2287*la^2 - 
        1920*En^8*(-314 + 43*la) - 24*En^6*(-11430 - 9079*la + 41*la^2) - 
        4*En^4*(599680 - 33463*la + 1267*la^2) + 
        2*En^2*(663670 - 116941*la + 5629*la^2)) + 
      J^2*(-3173760*En^10 + 72*En^8*(-230443 + 192056*la) + 
        12*En^6*(4666733 - 6083914*la + 156238*la^2) + 
        En^2*(16600914 - 41244863*la + 4068910*la^2 - 59208*la^3) + 
        6*(-180691 + 576969*la - 89160*la^2 + 2468*la^3) + 
        En^4*(-51759468 + 96582900*la - 5637902*la^2 + 36084*la^3))*M^2 + 
      (44941896*En^6 + 126*En^4*(-612678 + 172609*la) + 
        3*En^2*(12010006 - 6995635*la + 196384*la^2) + 
        2*(-1549073 + 1417725*la - 89410*la^2 + 584*la^3))*M^4)*rp[t]^25 + 
    2*(-((-1 + En^2)*J^2*(52650 + 1209600*En^10 - 425775*la + 99971*la^2 - 
         4516*la^3 + 11520*En^8*(-106 + 101*la) - 
         24*En^6*(73170 - 288693*la + 5315*la^2) - 
         4*En^4*(-653400 + 3307473*la - 277901*la^2 + 2518*la^3) + 
         En^2*(-895500 + 5858656*la - 896814*la^2 + 22274*la^3))*M) + 
      (765278 + 17919792*En^8 - 1285437*la + 142285*la^2 - 2168*la^3 + 
        72*En^6*(-632234 + 237359*la) + 12*En^4*(3235291 - 2501266*la + 
          75127*la^2) + 12*En^2*(-998492 + 1198923*la - 78265*la^2 + 
          456*la^3))*M^3)*rp[t]^26 + 4*(-1 + En^2)*
     (-2*(-1 + En^2)*J^2*la*(-5015 + 100800*En^8 + 1787*la - 128*la^2 + 
        480*En^6*(-4 + 17*la) - 6*En^4*(26230 - 2931*la + 29*la^2) + 
        En^2*(77480 - 18167*la + 743*la^2)) + 
      (47436 + 1702944*En^8 - 155828*la + 27610*la^2 - 750*la^3 + 
        36*En^6*(-112821 + 91012*la) + 6*En^4*(534160 - 868941*la + 
          46799*la^2) + 3*En^2*(-297151 + 730639*la - 82107*la^2 + 
          1006*la^3))*M^2)*rp[t]^27 + 4*(-1 + En^2)^2*
     (2250 + 120960*En^8 - 18827*la + 5190*la^2 - 229*la^3 + 
      288*En^6*(-940 + 2071*la) + 36*En^4*(5490 - 23977*la + 2143*la^2) + 
      2*En^2*(-24840 + 160566*la - 29227*la^2 + 629*la^3))*M*rp[t]^28 + 
    8*(-1 + En^2)^3*la*(-430 + 20160*En^6 + 187*la - 13*la^2 + 
      480*En^4*(-55 + 8*la) + 30*En^2*(290 - 85*la + 3*la^2))*rp[t]^29))/
  (En^2*(1 + la)*rp[t]^19*(3*M + la*rp[t])*(J^2 + rp[t]^2)^10) - 
 ((8*I)*J^3*m*mu*Pi*YPhiPhiBar*(2*M - rp[t])^3*(14502726*J^22*M^7 - 
    81*J^22*(404761 - 77526*la + 6296*m^2)*M^6*rp[t] + 
    3*J^20*M^5*(J^2*(10034225 - 4678557*la + 134008*la^2 - 
        8*(-43931 + 7862*la)*m^2 + 600*m^4) + 56606850*M^2)*rp[t]^2 + 
    J^20*M^4*(2*J^2*(-7143396 + 6353999*la - 414666*la^2 + 2156*la^3 + 
        (-428781 + 194918*la - 1540*la^2)*m^2 + 2*(-837 + 154*la)*m^4) + 
      9*(-42711015 + 8510250*En^2 + 8136798*la - 619520*m^2)*M^2)*rp[t]^3 + 
    J^18*M^3*(2*J^4*(1833432 - 2963012*la + 336357*la^2 - 4018*la^3 + 
        2*(85443 - 79158*la + 1435*la^2)*m^2 + (1161 - 574*la)*m^4) - 
      3*J^2*(-117815206 + 54650739*la - 1532480*la^2 + 80*(-48208 + 8569*la)*
         m^2 - 5736*m^4 + 18*En^2*(2511152 - 609899*la + 62456*m^2))*M^2 + 
      908155746*M^4)*rp[t]^4 + J^18*M^2*
     (J^4*(-476844 + 1486780*la - 267183*la^2 + 5586*la^3 + 
        (-66405 + 125879*la - 3990*la^2)*m^2 + (-711 + 798*la)*m^4) + 
      3*J^2*(-56004706 + 49573699*la - 3166768*la^2 + 16240*la^3 - 
        20*(157495 - 71086*la + 532*la^2)*m^2 + 4*(-2679 + 490*la)*m^4 + 
        2*En^2*(15368456 - 9560599*la + 358988*la^2 + (887884 - 207292*la)*
           m^2 + 2868*m^4))*M^2 + 9*(-228751319 + 94577508*En^2 + 
        43316370*la - 3059400*m^2)*M^4)*rp[t]^5 + 
    J^16*M*(J^6*(24084 - 187536*la + 51681*la^2 - 1715*la^3 + 
        (4995 - 24358*la + 1225*la^2)*m^2 + (81 - 245*la)*m^4) + 
      2*J^4*(21600501 - 34749956*la + 3860604*la^2 - 45444*la^3 + 
        10*(189201 - 173968*la + 2982*la^2)*m^2 + (11205 - 5502*la)*m^4 + 
        6*En^2*(-2478338 + 3183845*la - 282268*la^2 + 1750*la^3 + 
          (-255703 + 163274*la - 1890*la^2)*m^2 + 2*(-981 + 245*la)*m^4))*
       M^2 + 3*J^2*(632045817 + 26728632*En^4 - 291489131*la + 7979160*la^2 - 
        40*(-478119 + 84352*la)*m^2 + 24288*m^4 - 
        12*En^2*(41992382 - 10120661*la + 975576*m^2))*M^4 + 2929948578*M^6)*
     rp[t]^6 + J^16*(28*J^6*la*(324 - 138*la + 7*la^2 - 5*(-13 + la)*m^2 + 
        m^4) + J^4*(4*(-1407429 + 4369987*la - 768384*la^2 + 15813*la^3) - 
        5*(147861 - 278050*la + 8316*la^2)*m^2 + 9*(-767 + 854*la)*m^4 - 
        2*En^2*(-2231550 + 5995541*la - 970431*la^2 + 14364*la^3 + 
          (-378585 + 561817*la - 15540*la^2)*m^2 + 9*(-591 + 448*la)*m^4))*
       M^2 - 6*J^2*(150505005 - 132489138*la + 8259950*la^2 - 41700*la^3 + 
        5*(1569399 - 702746*la + 4956*la^2)*m^2 - 8*(-2847 + 518*la)*m^4 + 
        12*En^4*(1424311 - 472825*la + 54266*m^2) - 
        2*En^2*(85972785 - 53081035*la + 1943542*la^2 - 
          6*(-775818 + 179129*la)*m^2 + 12816*m^4))*M^4 + 
      9*(-739339993 + 477128082*En^2 + 139042202*la - 8985600*m^2)*M^6)*
     rp[t]^7 + J^14*M*(J^6*(285048 - 2211298*la + 596082*la^2 - 19446*la^3 + 
        5*(11196 - 54127*la + 2562*la^2)*m^2 + (792 - 2373*la)*m^4 + 
        2*En^2*(-120618 + 866731*la - 237972*la^2 + 6489*la^3 + 
          (-33300 + 137672*la - 7035*la^2)*m^2 + 9*(-88 + 203*la)*m^4)) + 
      6*J^4*(38780721 - 62066297*la + 6727775*la^2 - 77870*la^3 + 
        30*(105312 - 96014*la + 1547*la^2)*m^2 + (15948 - 7784*la)*m^4 + 
        4*En^4*(1904536 - 1772086*la + 91539*la^2 + (177824 - 59764*la)*m^2 + 
          1068*m^4) + En^2*(-55686732 + 71012414*la - 6138696*la^2 + 
          37212*la^3 - 12*(450462 - 284251*la + 3045*la^2)*m^2 + 
          96*(-369 + 91*la)*m^4))*M^2 + 3*J^2*(2046879624 + 282257712*En^4 - 
        937742461*la + 24970000*la^2 - 320*(-176355 + 30859*la)*m^2 + 
        59808*m^4 - 6*En^2*(425189556 - 101584549*la + 9112608*m^2))*M^4 + 
      6342267708*M^6)*rp[t]^8 + 
    J^14*(-21*J^6*la*(-5112 + 2129*la - 106*la^2 + 10*(-97 + 7*la)*m^2 - 
        13*m^4 + 2*En^2*(2112 - 989*la + 46*la^2 + (570 - 50*la)*m^2 + 
          13*m^4)) + J^4*(-18*En^2*(-2799254 + 7466477*la - 1178341*la^2 + 
          17024*la^3 - 3*(149640 - 219239*la + 5600*la^2)*m^2 + 
          72*(-75 + 56*la)*m^4) + 4*En^4*(-2037654 + 4592508*la - 
          595469*la^2 + 4998*la^3 - 12*(30570 - 32437*la + 595*la^2)*m^2 + 
          12*(-471 + 182*la)*m^4) + 3*(-10131999 + 31309372*la - 
          5369725*la^2 + 108510*la^3 - 5*(248484 - 463009*la + 12978*la^2)*
           m^2 + 12*(-823 + 910*la)*m^4))*M^2 + 
      3*J^2*(-976984392 + 9395568*En^6 + 854597391*la - 51807400*la^2 + 
        256960*la^3 - 80*(581985 - 258310*la + 1708*la^2)*m^2 + 
        112*(-1005 + 182*la)*m^4 - 24*En^4*(15143971 - 4971790*la + 
          540328*m^2) + 2*En^2*(874029960 - 535046275*la + 19024056*la^2 + 
          (43831824 - 9991472*la)*m^2 + 99120*m^4))*M^4 + 
      18*(-801965485 + 720705048*En^2 + 149628158*la - 8688120*m^2)*M^6)*
     rp[t]^9 + J^12*M*(J^6*(6*En^2*(-456552 + 3257356*la - 872093*la^2 + 
          23163*la^3 - 3*(39960 - 162872*la + 7665*la^2)*m^2 + 
          72*(-34 + 77*la)*m^4) - 4*En^4*(-115506 + 783491*la - 202986*la^2 + 
          4389*la^3 + (-38520 + 132451*la - 6300*la^2)*m^2 + 
          12*(-102 + 161*la)*m^4) - 3*(-514485 + 3974097*la - 1044525*la^2 + 
          33405*la^3 - 5*(18960 - 90749*la + 4011*la^2)*m^2 + 
          4*(-285 + 847*la)*m^4)) - 6*J^4*(-126185481 + 200747945*la - 
        21150350*la^2 + 240160*la^3 - 40*(235785 - 212916*la + 3206*la^2)*
         m^2 + 28*(-1413 + 686*la)*m^4 + 24*En^6*(154406 - 81239*la + 
          9868*m^2) - 4*En^4*(20413964 - 18778726*la + 943149*la^2 - 
          32*(-56051 + 18511*la)*m^2 + 9156*m^4) - 
        2*En^2*(-142196760 + 179818675*la - 15094342*la^2 + 88964*la^3 - 
          36*(356813 - 222074*la + 2170*la^2)*m^2 + 56*(-1233 + 301*la)*m^4))*
       M^2 + 6*J^2*(2225672625 + 671814864*En^4 - 1011816601*la + 
        26092600*la^2 - 280*(-195885 + 33974*la)*m^2 + 47208*m^4 - 
        24*En^2*(161212966 - 38131431*la + 3132864*m^2))*M^4 + 
      9682336188*M^6)*rp[t]^10 + 
    3*J^12*(2*J^6*la*(96859 - 39315*la + 1915*la^2 - 105*(-164 + 11*la)*m^2 + 
        196*m^4 + 70*En^4*(396 - 201*la + 9*la^2 - 13*(-10 + la)*m^2 + 
          4*m^4) - 35*En^2*(4796 - 2191*la + 99*la^2 + (1230 - 99*la)*m^2 + 
          24*m^4)) + J^4*(-33062373 + 101603826*la - 16929350*la^2 + 
        335040*la^3 - 60*(62275 - 114822*la + 2996*la^2)*m^2 + 
        84*(-293 + 322*la)*m^4 + 8*En^6*(206030 - 370461*la + 29743*la^2 - 
          4*(-8995 + 5391*la)*m^2 + 468*m^4) + 
        4*En^4*(-7350546 + 16371765*la - 2064203*la^2 + 16842*la^3 - 
          4*(313335 - 325996*la + 5460*la^2)*m^2 + 84*(-197 + 74*la)*m^4) - 
        6*En^2*(-14374995 + 38028248*la - 5827804*la^2 + 81656*la^3 - 
          4*(539415 - 778537*la + 18060*la^2)*m^2 + 28*(-759 + 560*la)*m^4))*
       M^2 + 4*J^2*(-532602444 + 23904432*En^6 + 462435823*la - 
        27132110*la^2 + 131940*la^3 - 35*(650301 - 285862*la + 1764*la^2)*
         m^2 + 14*(-3183 + 574*la)*m^4 - 24*En^4*(18160661 - 5887503*la + 
          599312*m^2) + 4*En^2*(332924079 - 201811673*la + 6929878*la^2 + 
          (15217272 - 3416686*la)*m^2 + 27132*m^4))*M^4 + 
      42*(-175389725 + 206760582*En^2 + 32420506*la - 1650432*m^2)*M^6)*
     rp[t]^11 + J^10*M*(J^6*(5053482 - 38841321*la + 9913200*la^2 - 
        309840*la^3 + 60*(14385 - 68043*la + 2786*la^2)*m^2 - 
        84*(-102 + 301*la)*m^4 - 4*En^4*(-1264032 + 8470798*la - 
          2135601*la^2 + 44793*la^3 - 16*(25200 - 84653*la + 3675*la^2)*m^2 + 
          252*(-44 + 67*la)*m^4) + 6*En^2*(-2360106 + 16702061*la - 
          4342902*la^2 + 111522*la^3 - 4*(146055 - 585496*la + 24885*la^2)*
           m^2 + 84*(-116 + 259*la)*m^4) + 8*En^6*(-34344 + 234146*la - 
          53529*la^2 + 749*la^3 - 4*(3420 - 9686*la + 343*la^2)*m^2 + 
          (-576 + 476*la)*m^4)) + 6*J^4*(275996037 + 467712*En^8 - 
        436001888*la + 44427790*la^2 - 493740*la^3 + 
        140*(132657 - 118526*la + 1659*la^2)*m^2 + (62874 - 30380*la)*m^4 - 
        288*En^6*(133034 - 68699*la + 7978*m^2) + 
        8*En^4*(49403801 - 44862156*la + 2180034*la^2 - 
          56*(-72071 + 23331*la)*m^2 + 16254*m^4) + 
        8*En^2*(-108910626 + 136405925*la - 11058352*la^2 + 62974*la^3 - 
          42*(214859 - 131497*la + 1155*la^2)*m^2 + 28*(-1359 + 329*la)*m^4))*
       M^2 + 42*J^2*(488267814 + 271207872*En^4 - 219945281*la + 
        5462624*la^2 - 16*(-654834 + 112513*la)*m^2 + 7080*m^4 - 
        6*En^2*(185859496 - 43448503*la + 3190632*m^2))*M^4 + 
      10650410820*M^6)*rp[t]^12 + 
    J^10*(J^6*la*(-56*En^6*(1722 - 957*la + 43*la^2 + (680 - 80*la)*m^2 + 
          28*m^4) + 420*En^4*(4334 - 2147*la + 93*la^2 - 
          4*(-340 + 31*la)*m^2 + 36*m^4) + 15*(126819 - 49960*la + 
          2372*la^2 - 28*(-745 + 46*la)*m^2 + 196*m^4) - 
        6*En^2*(867643 - 385180*la + 16760*la^2 - 420*(-499 + 36*la)*m^2 + 
          3332*m^4)) - 3*J^4*(72562515 - 221547986*la + 35676010*la^2 - 
        689580*la^3 + 70*(105999 - 193135*la + 4662*la^2)*m^2 - 
        42*(-935 + 1022*la)*m^4 + 192*En^8*(1484 - 1919*la + 220*m^2) - 
        32*En^6*(543090 - 956401*la + 74303*la^2 + (89850 - 52157*la)*m^2 + 
          1038*m^4) - 8*En^4*(-17977767 + 39515414*la - 4820518*la^2 + 
          37740*la^3 - 14*(204855 - 208648*la + 3080*la^2)*m^2 + 
          42*(-711 + 262*la)*m^4) + 8*En^2*(-33244500 + 87105419*la - 
          12894567*la^2 + 173928*la^3 - 7*(659070 - 933277*la + 19320*la^2)*
           m^2 + 42*(-843 + 616*la)*m^4))*M^2 + 
      6*J^2*(217458432*En^6 - 336*En^4*(7399863 - 2363148*la + 222982*m^2) + 
        14*En^2*(385786476 - 231242577*la + 7609084*la^2 - 
          36*(-435391 + 96103*la)*m^2 + 21060*m^4) + 
        7*(-234499086 + 201788413*la - 11389952*la^2 + 54192*la^3 - 
          4*(2188023 - 951974*la + 5460*la^2)*m^2 + 4*(-3351 + 602*la)*m^4))*
       M^4 + 126*(-193624249 + 289517844*En^2 + 35397182*la - 1527480*m^2)*
       M^6)*rp[t]^13 + 
    J^8*M*(J^6*(192*En^8*(174 - 1619*la + 287*la^2 + (90 - 235*la)*m^2 + 
          6*m^4) - 8*En^4*(-3126438 + 20695783*la - 5050086*la^2 + 
          101250*la^3 - 14*(67230 - 220861*la + 8400*la^2)*m^2 + 
          126*(-162 + 241*la)*m^4) + 24*En^2*(-1834560 + 12851394*la - 
          3229567*la^2 + 79467*la^3 - 21*(20200 - 79184*la + 2975*la^2)*m^2 + 
          42*(-130 + 287*la)*m^4) + 96*En^6*(-30864 + 205840*la - 
          45625*la^2 + 623*la^3 + (-11880 + 32214*la - 1057*la^2)*m^2 + 
          (-456 + 350*la)*m^4) + 3*(3711267 - 28362669*la + 6990670*la^2 - 
          212850*la^3 + 210*(2749 - 12828*la + 483*la^2)*m^2 - 
          14*(-327 + 959*la)*m^4)) + 6*J^4*(4825728*En^8 - 
        288*En^6*(613777 - 311168*la + 33679*m^2) + 
        56*En^4*(20378149 - 18194900*la + 850570*la^2 - 
          16*(-95891 + 30151*la)*m^2 + 4470*m^4) + 
        28*En^2*(-63454608 + 78643817*la - 6110088*la^2 + 33406*la^3 - 
          3*(1570569 - 942782*la + 7350*la^2)*m^2 + 30*(-495 + 119*la)*m^4) - 
        7*(-60997809 + 95531976*la - 9353504*la^2 + 101496*la^3 - 
          12*(299841 - 264856*la + 3430*la^2)*m^2 + (-9486 + 4564*la)*m^4))*
       M^2 + 42*J^2*(541239435 + 502003800*En^4 - 241110469*la + 
        5727880*la^2 - 40*(-243849 + 41492*la)*m^2 + 4944*m^4 - 
        12*En^2*(130905870 - 30154865*la + 1906056*m^2))*M^4 + 
      8451188676*M^6)*rp[t]^14 + 
    3*J^8*(4*J^6*la*(4*(87297 - 33190*la + 1530*la^2) - 
        735*(-71 + 4*la)*m^2 + 392*m^4 + 16*En^8*(2*(29 - 19*la + la^2) - 
          5*(-6 + la)*m^2 + 2*m^4) - 56*En^6*(1548 - 843*la + 37*la^2 + 
          (590 - 65*la)*m^2 + 22*m^4) + 60*En^4*(12523 - 6017*la + 248*la^2 + 
          (3710 - 294*la)*m^2 + 77*m^4) - 2*En^2*(673988 - 289495*la + 
          11995*la^2 - 105*(-1446 + 91*la)*m^2 + 1862*m^4)) + 
      J^4*(-4608*En^8*(672 - 829*la + 95*m^2) + 
        16*En^6*(5057385 - 8806356*la + 656500*la^2 + (767610 - 438662*la)*
           m^2 + 6870*m^4) - 42*En^2*(-12989443 + 33718154*la - 
          4786862*la^2 + 61688*la^3 - 6*(272295 - 376979*la + 6860*la^2)*
           m^2 + 30*(-309 + 224*la)*m^4) + 56*En^4*(-7552023 + 16239340*la - 
          1905850*la^2 + 14052*la^3 - 2*(560685 - 551396*la + 6860*la^2)*
           m^2 + 10*(-831 + 302*la)*m^4) + 7*(-16108239 + 48784526*la - 
          7537976*la^2 + 141912*la^3 - 2*(725415 - 1304774*la + 28980*la^2)*
           m^2 + 6*(-991 + 1078*la)*m^4))*M^2 + 
      4*J^2*(295774128*En^6 - 84*En^4*(27589975 - 8694493*la + 740830*m^2) + 
        14*En^2*(273505113 - 161622719*la + 5035110*la^2 + 
          (9479028 - 2054614*la)*m^2 + 9096*m^4) + 
        7*(-130578663 + 111119612*la - 5988362*la^2 + 27820*la^3 - 
          5*(820365 - 353118*la + 1876*la^2)*m^2 + 12*(-391 + 70*la)*m^4))*
       M^4 + 6*(-1080566417 + 2013263994*En^2 + 194950330*la - 6796800*m^2)*
       M^6)*rp[t]^15 + 
    J^6*M*(J^6*(768*En^8*(522 - 4437*la + 761*la^2 + (270 - 615*la)*m^2 + 
          18*m^4) + 42*En^2*(-2162610 + 15040419*la - 3632056*la^2 + 
          84846*la^3 - 6*(76830 - 293032*la + 9555*la^2)*m^2 + 
          270*(-16 + 35*la)*m^4) - 21*(-827964 + 6282157*la - 1483532*la^2 + 
          43860*la^3 - 2*(57090 - 262453*la + 9030*la^2)*m^2 + 
          58*(-12 + 35*la)*m^4) + 48*En^6*(-276120 + 1921621*la - 
          409838*la^2 + 5380*la^3 - 2*(50280 - 137802*la + 3941*la^2)*m^2 + 
          30*(-104 + 77*la)*m^4) - 56*En^4*(-1359180 + 8664173*la - 
          2031000*la^2 + 37998*la^3 - 8*(47880 - 150308*la + 4725*la^2)*m^2 + 
          30*(-192 + 281*la)*m^4)) + 6*J^4*(20366784*En^8 - 
        96*En^6*(5238168 - 2521661*la + 261920*m^2) + 
        56*En^4*(38241773 - 33783562*la + 1512063*la^2 + 
          (2617720 - 794420*la)*m^2 + 5070*m^4) - 
        7*(-68300121 + 105789614*la - 9867898*la^2 + 104308*la^3 - 
          20*(169998 - 148374*la + 1771*la^2)*m^2 + 12*(-555 + 266*la)*m^4) + 
        28*En^2*(-90605322 + 110811799*la - 8144476*la^2 + 42494*la^3 - 
          18*(321364 - 188937*la + 1295*la^2)*m^2 + 8*(-1611 + 385*la)*m^4))*
       M^2 + 6*J^2*(3036951756 + 4481564976*En^4 - 1334656373*la + 
        30079280*la^2 - 160*(-272913 + 45979*la)*m^2 + 15504*m^4 - 
        42*En^2*(262338836 - 59253423*la + 3045168*m^2))*M^4 + 
      4745554398*M^6)*rp[t]^16 + 
    3*J^6*(J^6*la*(768*En^8*(2*(29 - 19*la + la^2) - 5*(-6 + la)*m^2 + 
          2*m^4) + 560*En^4*(16240 - 7433*la + 282*la^2 + 
          (4520 - 287*la)*m^2 + 65*m^4) + 7*(311547 - 113318*la + 5052*la^2 - 
          420*(-98 + 5*la)*m^2 + 238*m^4) - 70*En^2*(158951 - 65958*la + 
          2572*la^2 + (32880 - 1764*la)*m^2 + 294*m^4) - 
        32*En^6*(49490 - 26816*la + 1149*la^2 - 70*(-253 + 25*la)*m^2 + 
          525*m^4)) + J^4*(-192*En^8*(51897 - 83947*la + 7260*m^2) + 
        32*En^6*(7939573 - 12407581*la + 863619*la^2 + (1062500 - 561785*la)*
           m^2 + 5700*m^4) + 56*En^4*(-14229309 + 30432834*la - 
          3443951*la^2 + 23466*la^3 - 10*(198315 - 186574*la + 1890*la^2)*
           m^2 + 30*(-317 + 114*la)*m^4) + 7*(-18150915 + 54375326*la - 
          7983982*la^2 + 146004*la^3 - 10*(138438 - 245635*la + 4998*la^2)*
           m^2 + 12*(-349 + 378*la)*m^4) - 84*En^2*(-9338223 + 23968205*la - 
          3218461*la^2 + 39344*la^3 + (-1021860 + 1380517*la - 21840*la^2)*
           m^2 + 4*(-1011 + 728*la)*m^4))*M^2 + 
      2*J^2*(-1474729800 + 987410160*En^6 + 1237521283*la - 63085400*la^2 + 
        285600*la^3 - 40*(924633 - 393662*la + 1932*la^2)*m^2 + 
        8*(-3687 + 658*la)*m^4 - 168*En^4*(35491139 - 10988130*la + 
          794564*m^2) + 14*En^2*(553569060 - 320640265*la + 9291056*la^2 - 
          8*(-1916943 + 407929*la)*m^2 + 9768*m^4))*M^4 + 
      3*(-1220856053 + 2813115312*En^2 + 216845662*la - 5676600*m^2)*M^6)*
     rp[t]^17 + J^4*M*(J^6*(192*En^8*(-16488 - 59008*la + 11967*la^2 + 
          (60 - 8075*la)*m^2 + 228*m^4) - 56*En^4*(-2545965 + 16344712*la - 
          3751272*la^2 + 63909*la^3 - 5*(142740 - 422071*la + 10500*la^2)*
           m^2 + 90*(-74 + 107*la)*m^4) - 21*(-939933 + 7057829*la - 
          1578934*la^2 + 45182*la^3 - 10*(11028 - 49901*la + 1561*la^2)*m^2 + 
          12*(-41 + 119*la)*m^4) + 32*En^6*(-1759320 + 8890081*la - 
          1677813*la^2 + 19657*la^3 - 5*(94320 - 226562*la + 4711*la^2)*m^2 + 
          20*(-396 + 287*la)*m^4) + 84*En^2*(-1562364 + 10794419*la - 
          2469369*la^2 + 54291*la^3 + (-295500 + 1093816*la - 30555*la^2)*
           m^2 + 12*(-158 + 343*la)*m^4)) + 
      6*J^4*(388625211 + 78630336*En^8 - 593276762*la + 52163300*la^2 - 
        535920*la^3 + 40*(386409 - 333148*la + 3654*la^2)*m^2 - 
        4*(-5247 + 2506*la)*m^4 - 240*En^6*(3461596 - 1691363*la + 
          171858*m^2) + 56*En^4*(49497739 - 43191834*la + 1810041*la^2 + 
          (2886896 - 844656*la)*m^2 + 3402*m^4) + 
        28*En^2*(-92745174 + 111165803*la - 7572982*la^2 + 37504*la^3 - 
          6*(791833 - 455834*la + 2730*la^2)*m^2 + 4*(-1737 + 413*la)*m^4))*
       M^2 + 3*J^2*(3455585229 + 8044378272*En^4 - 1493919521*la + 
        31640600*la^2 - 40*(-917355 + 153022*la)*m^2 + 8088*m^4 - 
        48*En^2*(324783662 - 71454623*la + 2737776*m^2))*M^4 + 
      1796990238*M^6)*rp[t]^18 + 
    J^4*(2*J^6*la*(96*En^8*(2*(-2363 - 529*la + 56*la^2) - 
          45*(-2 + 5*la)*m^2 + 76*m^4) + 21*(176751 - 60674*la + 2606*la^2 - 
          70*(-283 + 13*la)*m^2 + 84*m^4) + 420*En^4*(60845 - 28343*la + 
          957*la^2 + (16750 - 805*la)*m^2 + 150*m^4) - 
        42*En^2*(575191 - 227569*la + 8261*la^2 - 15*(-6994 + 315*la)*m^2 + 
          644*m^4) - 16*En^6*(605220 - 233271*la + 8579*la^2 - 
          175*(-946 + 61*la)*m^2 + 2660*m^4)) - 
      3*J^4*(104243439 + 4056192*En^10 - 307603182*la + 42385700*la^2 - 
        750960*la^3 + 60*(105947 - 185382*la + 3444*la^2)*m^2 + 
        (13236 - 14280*la)*m^4 + 192*En^8*(465957 - 308272*la + 27440*m^2) - 
        80*En^6*(5020251 - 8298721*la + 589779*la^2 + (757650 - 363314*la)*
           m^2 + 2046*m^4) - 56*En^4*(-18362667 + 39317239*la - 
          4199987*la^2 + 26034*la^3 - 2*(1134435 - 1019996*la + 8260*la^2)*
           m^2 + 6*(-1071 + 382*la)*m^4) + 14*En^2*(-58106121 + 
          146224508*la - 18128364*la^2 + 208896*la^3 + 
          (-5131980 + 6764068*la - 92400*la^2)*m^2 + 12*(-1095 + 784*la)*
           m^4))*M^2 + 6*J^2*(-846145476 + 1122115104*En^6 + 697801013*la - 
        33287350*la^2 + 146580*la^3 - 5*(3130215 - 1318130*la + 5964*la^2)*
         m^2 + 2*(-3855 + 686*la)*m^4 - 336*En^4*(16241367 - 4854345*la + 
          265916*m^2) + 8*En^2*(695796435 - 391799505*la + 10319582*la^2 - 
          6*(-2326488 + 486289*la)*m^2 + 5220*m^4))*M^4 + 
      9*(-465762835 + 1324808706*En^2 + 81242006*la - 1405440*m^2)*M^6)*
     rp[t]^19 + (J^8*(8064*En^10*(1000 - 543*la + 80*m^2) + 
        192*En^8*(246768 - 481517*la + 35280*la^2 - 40*(-411 + 730*la)*m^2 + 
          312*m^4) - 56*En^4*(-3161484 + 21184709*la - 4690659*la^2 + 
          71361*la^3 - 8*(107460 - 299363*la + 5775*la^2)*m^2 + 
          18*(-252 + 361*la)*m^4) + 42*En^2*(-3286386 + 22310273*la - 
          4693124*la^2 + 96384*la^3 - 4*(126735 - 455272*la + 10815*la^2)*
           m^2 + 12*(-172 + 371*la)*m^4) + 240*En^6*(-317220 + 1907807*la - 
          402877*la^2 + 3939*la^3 - 2*(64740 - 132734*la + 1827*la^2)*m^2 + 
          (-960 + 686*la)*m^4) - 3*(-5462190 + 40367777*la - 8426600*la^2 + 
          232680*la^3 - 20*(25635 - 114131*la + 3234*la^2)*m^2 + 
          4*(-390 + 1127*la)*m^4))*M + 6*J^6*(225311991 + 80924544*En^8 - 
        337593180*la + 27629275*la^2 - 275310*la^3 + 
        30*(219855 - 187238*la + 1883*la^2)*m^2 + (5499 - 2618*la)*m^4 - 
        96*En^6*(9582494 - 4808491*la + 420582*m^2) + 
        56*En^4*(46345841 - 39001324*la + 1454126*la^2 - 
          8*(-247901 + 70111*la)*m^2 + 1254*m^4) + 
        8*En^2*(-237590910 + 276114545*la - 16961448*la^2 + 79366*la^3 - 
          6*(1463007 - 825211*la + 4305*la^2)*m^2 + 36*(-207 + 49*la)*m^4))*
       M^3 + 3*J^4*(1330082870 + 4956473088*En^4 - 563993167*la + 
        11110400*la^2 - 80*(-114260 + 18873*la)*m^2 + 936*m^4 - 
        6*En^2*(1244411808 - 264864253*la + 6553512*m^2))*M^5 + 
      413023482*J^2*M^7)*rp[t]^20 + 
    (3*J^8*la*(8960*En^10*(97 - 6*la + 8*m^2) + 
        840*En^4*(25456 - 12251*la + 359*la^2 + (6680 - 238*la)*m^2 + 
          34*m^4) + 128*En^8*(9*(4527 - 295*la + 15*la^2) - 
          40*(-72 + 5*la)*m^2 + 52*m^4) + 7*(293079 - 93140*la + 3840*la^2 - 
          60*(-437 + 18*la)*m^2 + 76*m^4) - 70*En^2*(242375 - 87916*la + 
          2944*la^2 + (35820 - 1344*la)*m^2 + 140*m^4) - 
        80*En^6*(108108 - 61601*la + 1749*la^2 - 420*(-107 + 4*la)*m^2 + 
          322*m^4)) + 3*J^6*(-61242333 + 30567936*En^10 + 177046066*la - 
        22553425*la^2 + 386190*la^3 - 5*(547995 - 945589*la + 16002*la^2)*
         m^2 + (-3477 + 3738*la)*m^4 - 192*En^8*(265178 - 350573*la + 
          58260*m^2) + 32*En^6*(12242340 - 23344339*la + 1642397*la^2 + 
          (1986570 - 878311*la)*m^2 + 2382*m^4) - 
        24*En^2*(-25425245 + 61793437*la - 6835801*la^2 + 73864*la^3 - 
          3*(536430 - 690479*la + 8120*la^2)*m^2 + 6*(-393 + 280*la)*m^4) + 
        56*En^4*(-17636079 + 36452390*la - 3440722*la^2 + 19188*la^3 - 
          2*(805605 - 695248*la + 4480*la^2)*m^2 + (-2382 + 844*la)*m^4))*
       M^2 + J^4*(-1975344798 + 5807631744*En^6 + 1595216821*la - 
        70371120*la^2 + 300880*la^3 - 20*(1177941 - 490682*la + 2044*la^2)*
         m^2 + (-5364 + 952*la)*m^4 - 288*En^4*(72603181 - 20457688*la + 
          705982*m^2) + 6*En^2*(2722451472 - 1477498939*la + 34470924*la^2 + 
          (33809676 - 6948188*la)*m^2 + 5556*m^4))*M^4 + 
      9*J^2*(-108008411 + 379082148*En^2 + 18458410*la - 156584*m^2)*M^6)*
     rp[t]^21 + (J^6*(-3870720*En^12 + 96768*En^10*(-520 + 307*la) + 
        576*En^8*(7374 - 103061*la + 24095*la^2 - 55*(-354 + 365*la)*m^2 + 
          66*m^4) + 3*(3266289 - 23582927*la + 4509175*la^2 - 119805*la^3 + 
          5*(44745 - 196022*la + 5019*la^2)*m^2 + (411 - 1183*la)*m^4) + 
        96*En^6*(-313272 + 4877438*la - 1187367*la^2 + 9357*la^3 + 
          (-376440 + 688002*la - 6251*la^2)*m^2 + 6*(-188 + 133*la)*m^4) + 
        24*En^2*(-4463784 + 29074809*la - 5377463*la^2 + 102543*la^3 - 
          9*(54220 - 189344*la + 3815*la^2)*m^2 + 18*(-62 + 133*la)*m^4) - 
        56*En^4*(-3100788 + 20278507*la - 3943974*la^2 + 52902*la^3 - 
          2*(319050 - 844081*la + 12600*la^2)*m^2 + 6*(-282 + 401*la)*m^4))*
       M + 2*J^4*(266573706 + 167387904*En^8 - 390272684*la + 29322420*la^2 - 
        282820*la^3 + 10*(500529 - 421152*la + 3878*la^2)*m^2 + 
        (1917 - 910*la)*m^4 - 288*En^6*(8885557 - 4028442*la + 221059*m^2) + 
        24*En^4*(217542061 - 170456276*la + 5273814*la^2 - 
          16*(-336671 + 92431*la)*m^2 + 1374*m^4) + 
        6*En^2*(-477592974 + 532022605*la - 28569508*la^2 + 125846*la^3 - 
          9*(1197743 - 662794*la + 3010*la^2)*m^2 + (-3978 + 938*la)*m^4))*
       M^3 + 3*J^2*(311743501 + 1885622328*En^4 - 129297887*la + 
        2343928*la^2 - 8*(-128071 + 20952*la)*m^2 - 
        12*En^2*(181714222 - 37221861*la + 434424*m^2))*M^5 + 43630650*M^7)*
     rp[t]^22 + (12*J^6*la*(-107520*En^12 - 8960*En^10*(155 + 2*la) + 
        35*(8739 - 2510*la + 99*la^2 + (570 - 21*la)*m^2 + m^4) + 
        140*En^4*(37826 - 16033*la + 402*la^2 + (7390 - 196*la)*m^2 + 
          19*m^4) - 14*En^2*(234821 - 73185*la + 2245*la^2 + 
          (24510 - 765*la)*m^2 + 54*m^4) + 16*En^8*(2894 - 18450*la + 
          370*la^2 + (19890 - 375*la)*m^2 + 66*m^4) - 
        8*En^6*(118552 - 199487*la + 4213*la^2 - 35*(-3698 + 83*la)*m^2 + 
          378*m^4)) + J^4*(-73740165 - 71366400*En^10 + 207639238*la - 
        24051480*la^2 + 397140*la^3 - 5*(419973 - 714850*la + 11004*la^2)*
         m^2 + 3*(-405 + 434*la)*m^4 - 1152*En^8*(-97557 - 141188*la + 
          26580*m^2) + 48*En^6*(25702555 - 42143072*la + 2290216*la^2 + 
          (2195750 - 914914*la)*m^2 + 906*m^4) + 
        24*En^4*(-89066247 + 167517864*la - 12722878*la^2 + 63420*la^3 - 
          2*(2250585 - 1875796*la + 9660*la^2)*m^2 + 6*(-437 + 154*la)*m^4) - 
        18*En^2*(-53038485 + 122496917*la - 11634031*la^2 + 117404*la^3 + 
          (-2009745 + 2530777*la - 25620*la^2)*m^2 + (-1263 + 896*la)*m^4))*
       M^2 + 6*J^2*(-78175625 + 569102112*En^6 + 61617838*la - 2483142*la^2 + 
        10292*la^3 + (-443167 + 182666*la - 700*la^2)*m^2 - 
        12*En^4*(115767599 - 30294969*la + 462866*m^2) + 
        En^2*(815669714 - 423924278*la + 8544076*la^2 + (4531864 - 916812*la)*
           m^2))*M^4 + 9*(-11526261 + 49811706*En^2 + 1926290*la)*M^6)*
     rp[t]^23 + (J^4*(4028616 + 23224320*En^12 - 28190697*la + 4837410*la^2 - 
        123350*la^3 + 5*(34704 - 149663*la + 3458*la^2)*m^2 + 
        (144 - 413*la)*m^4 - 241920*En^10*(-140 + 97*la + 8*m^2) + 
        384*En^8*(-242064 + 53407*la + 42606*la^2 + (31440 - 27130*la)*m^2 + 
          24*m^4) - 8*En^4*(-17875728 + 100605653*la - 14956956*la^2 + 
          175770*la^3 - 8*(230940 - 585818*la + 6825*la^2)*m^2 + 
          18*(-104 + 147*la)*m^4) + 6*En^2*(-9840780 + 59964146*la - 
          9272268*la^2 + 163443*la^3 + (-622200 + 2117656*la - 36225*la^2)*
           m^2 + 3*(-200 + 427*la)*m^4) + 16*En^6*(-3258144 + 30077725*la - 
          5226570*la^2 + 32968*la^3 - 2*(667440 - 1131242*la + 7021*la^2)*
           m^2 + 2*(-648 + 455*la)*m^4))*M + 
      2*J^2*(64345812 + 321558336*En^8 - 91674839*la + 6234177*la^2 - 
        58098*la^3 + (569496 - 473596*la + 3990*la^2)*m^2 - 
        288*En^6*(6011836 - 2257559*la + 47772*m^2) + 
        12*En^4*(184874386 - 132245310*la + 3188595*la^2 + 
          (1800464 - 482004*la)*m^2) - 6*En^2*(147908942 - 156676423*la + 
          7142132*la^2 - 29534*la^3 + 2*(731774 - 397897*la + 1575*la^2)*
           m^2))*M^3 + 3*(33675504 + 329641200*En^4 - 13631817*la + 
        225040*la^2 + 18*En^2*(-16279708 + 3201411*la))*M^5)*rp[t]^24 + 
    (J^4*la*(1502361 + 7741440*En^12 - 379845*la + 14290*la^2 - 
        70*(-881 + 29*la)*m^2 + 49*m^4 - 322560*En^10*(-36 - la + 2*m^2) + 
        1680*En^4*(30932 - 8993*la + 192*la^2 + (3040 - 61*la)*m^2 + 3*m^4) + 
        768*En^8*(-40624 - 7007*la + 73*la^2 - 45*(-118 + la)*m^2 + 4*m^4) - 
        42*En^2*(514621 - 128365*la + 3590*la^2 + (31110 - 810*la)*m^2 + 
          29*m^4) - 32*En^6*(625548 - 474198*la + 7507*la^2 - 
          70*(-3257 + 47*la)*m^2 + 217*m^4)) - 
      J^2*(18184680 + 83317248*En^10 - 49619086*la + 5139363*la^2 - 
        81666*la^3 + (241200 - 405179*la + 5670*la^2)*m^2 + 
        576*En^8*(504961 - 432329*la + 17140*m^2) + 
        96*En^6*(-11262765 + 13524543*la - 451585*la^2 + 
          3*(-81960 + 32737*la)*m^2) + En^2*(-309293928 + 671908954*la - 
          52897962*la^2 + 497088*la^3 + (-4986720 + 6156074*la - 53760*la^2)*
           m^2) + 12*En^4*(83179992 - 138623440*la + 7835255*la^2 - 
          34818*la^3 + 4*(384945 - 311737*la + 1295*la^2)*m^2))*M^2 + 
      3*(-17142648 + 306138672*En^6 + 13154027*la - 478568*la^2 + 1920*la^3 + 
        24*En^4*(-21223675 + 5157862*la) + 2*En^2*(112695168 - 55946303*la + 
          953992*la^2))*M^4)*rp[t]^25 + 
    (J^2*(1023120 - 29030400*En^12 - 6892096*la + 1040091*la^2 - 25395*la^3 + 
        (20160 - 85643*la + 1785*la^2)*m^2 - 32256*En^10*
         (-1840 + 801*la + 40*m^2) + 192*En^8*(46200 - 585006*la + 
          51109*la^2 + (21840 - 17385*la)*m^2) - 
        96*En^6*(1021440 - 4134943*la + 358329*la^2 - 1825*la^3 + 
          (52080 - 83962*la + 371*la^2)*m^2) - 
        2*En^2*(10216080 - 57470554*la + 7118151*la^2 - 115641*la^3 + 
          (262080 - 871832*la + 12705*la^2)*m^2) + 
        4*En^4*(19569060 - 91703717*la + 9429510*la^2 - 96957*la^3 + 
          (650160 - 1594891*la + 14700*la^2)*m^2))*M + 
      6*(2395920 + 69693120*En^8 - 3311861*la + 201118*la^2 - 1808*la^3 + 
        24*En^6*(-7248482 + 2332579*la) + 4*En^4*(36052602 - 23624558*la + 
          427517*la^2) + 2*En^2*(-21167940 + 21269891*la - 804246*la^2 + 
          3116*la^3))*M^3)*rp[t]^26 + 
    2*(-3*(-1 + En^2)*J^2*la*(63210 + 1612800*En^10 - 13719*la + 491*la^2 - 
        35*(-34 + la)*m^2 + 35840*En^8*(-47 - 3*la + 2*m^2) + 
        32*En^6*(-2*(36155 - 7883*la + 62*la^2) + 35*(-146 + la)*m^2) - 
        16*En^4*(-218260 + 37419*la - 591*la^2 + 35*(-214 + 3*la)*m^2) + 
        2*En^2*(-585970 + 110043*la - 2727*la^2 + 35*(-418 + 9*la)*m^2)) + 
      (45960768*En^10 + 288*En^8*(-536549 + 258224*la) + 
        12*En^6*(16022652 - 15674929*la + 306651*la^2) + 
        9*(-115608 + 304596*la - 27777*la^2 + 424*la^3) - 
        36*En^2*(-643968 + 1313749*la - 83580*la^2 + 730*la^3) + 
        2*En^4*(-52924428 + 79135923*la - 3205577*la^2 + 12702*la^3))*M^2)*
     rp[t]^27 + 12*(-1 + En^2)*(645120*En^10 + 2016*En^8*(-1040 + 1311*la) + 
      16*En^6*(156240 - 397179*la + 12112*la^2) + 
      En^2*(262080 - 1350364*la + 128185*la^2 - 1845*la^3) + 
      2*(-5040 + 32549*la - 4243*la^2 + 99*la^3) + 
      2*En^4*(-650160 + 2497716*la - 154803*la^2 + 1051*la^3))*M*rp[t]^28 + 
    12*(-1 + En^2)^2*la*(3710 + 215040*En^8 - 677*la + 23*la^2 + 
      2240*En^6*(-221 + 10*la) - 30*En^2*(3010 - 411*la + 9*la^2) + 
      32*En^4*(11445 - 1039*la + 11*la^2))*rp[t]^29))/
  (En^2*la*(1 + la)*rp[t]^19*(3*M + la*rp[t])*(J^2 + rp[t]^2)^11) + 
 ((-8*mu*Pi*YBar*(-2*M + rp[t])^2*(1240533*J^20*M^6 + 
      3*J^20*(-794641 + 152362*la)*M^5*rp[t] + 
      3*J^18*M^4*(J^2*(595800 - 293237*la + 1952*la^2) + 3698340*M^2)*
       rp[t]^2 + J^18*M^3*(-2*J^2*(324937 - 330260*la + 5078*la^2 + 4*la^3) + 
        3*(-7124621 + 2328498*En^2 + 1363694*la)*M^2)*rp[t]^3 + 
      J^16*M^2*(2*J^4*(56832 - 120225*la + 3260*la^2 + 6*la^3) + 
        3*J^2*(5355854 - 2630427*la + 18112*la^2 + 
          En^2*(-3331892 + 842150*la))*M^2 + 43817283*M^4)*rp[t]^4 + 
      J^16*M*(-(J^4*(7560 - 42091*la + 1831*la^2 + 6*la^3)) + 
        2*J^2*(-2929159 + 2969473*la - 47178*la^2 - 52*la^3 + 
          6*En^2*(428589 - 301633*la + 2188*la^2))*M^2 + 
        6*(-14114156 + 9844047*En^2 + 2693474*la)*M^4)*rp[t]^5 + 
      J^14*(J^6*la*(-2800 + 189*la + la^2) + 2*J^4*(513861 - 1083652*la + 
          30330*la^2 + 78*la^3 + 2*En^2*(-278244 + 466229*la - 7985*la^2 + 
            18*la^3))*M^2 + 6*J^2*(10646612 + 1017348*En^4 - 5211293*la + 
          36756*la^2 + En^2*(-14154706 + 3572485*la))*M^4 + 99910224*M^6)*
       rp[t]^6 + J^14*M*(-(J^4*(68580 - 380383*la + 17061*la^2 + 78*la^3 + 
           2*En^2*(-41850 + 202095*la - 6299*la^2 + 36*la^3))) + 
        2*J^2*(-11688081 + 11803718*la - 192074*la^2 - 300*la^3 + 
          12*En^4*(-240616 + 89437*la) + 6*En^2*(3661446 - 2569681*la + 
            20788*la^2))*M^2 + 12*(-16162739 + 18223926*En^2 + 3068902*la)*
         M^4)*rp[t]^7 + J^12*(J^6*la*(-25375 + 1764*la + 13*la^2 + 
          6*En^2*(5075 - 266*la + 3*la^2)) + J^4*(4116987 - 8644198*la + 
          247798*la^2 + 900*la^3 + 12*En^4*(139245 - 169648*la + 982*la^2) + 
          4*En^2*(-2391858 + 3990188*la - 75839*la^2 + 174*la^3))*M^2 + 
        12*J^2*(12249113 + 4097100*En^4 - 5963461*la + 42398*la^2 + 
          6*En^2*(-4393449 + 1106185*la))*M^4 + 143929314*M^6)*rp[t]^8 + 
      J^12*M*(J^4*(-275895 + 1522567*la - 69955*la^2 - 450*la^3 + 
          En^2*(724680 - 3477270*la + 119554*la^2 - 696*la^3) + 
          4*En^4*(-35415 + 147809*la - 2103*la^2 + 2*la^3)) + 
        2*J^2*(-27028707 + 688752*En^6 + 27137986*la - 445694*la^2 - 
          1012*la^3 + 120*En^4*(-196121 + 72392*la) + 
          12*En^2*(6864356 - 4799000*la + 42965*la^2))*M^2 + 
        6*(-46869035 + 77070036*En^2 + 8822494*la)*M^4)*rp[t]^9 + 
      J^10*(J^6*la*(-101955 + 7263*la + 75*la^2 - 
          4*En^4*(12600 - 350*la + la^2) + 6*En^2*(43925 - 2520*la + 
            29*la^2)) + 2*J^4*(4785615 - 9984667*la + 289339*la^2 + 
          1518*la^3 + 24*En^6*(-13408 + 9747*la) + 
          12*En^4*(575625 - 693817*la + 5133*la^2) + 
          2*En^2*(-9036216 + 14989785*la - 314386*la^2 + 714*la^3))*M^2 + 
        6*J^2*(35761574 + 28581672*En^4 - 17256139*la + 120028*la^2 + 
          4*En^2*(-28083551 + 7042417*la))*M^4 + 133860744*M^6)*rp[t]^10 + 
      J^10*M*(-(J^4*(645075 - 3535169*la + 164525*la^2 + 1518*la^3 + 
           24*En^6*(-2370 + 9166*la + la^2) - 4*En^4*(-297900 + 1224436*la - 
             21909*la^2 + 50*la^3) + 2*En^2*(-1380825 + 6573725*la - 
             248527*la^2 + 1428*la^3))) + 2*J^2*(-39738627 + 5522832*En^6 + 
          39532970*la - 637638*la^2 - 2212*la^3 + 
          24*En^4*(-3468458 + 1269341*la) + 12*En^2*(14749523 - 10254904*la + 
            100699*la^2))*M^2 + 6*(-44041351 + 101444742*En^2 + 8159954*la)*
         M^4)*rp[t]^11 + J^8*(J^6*la*(-237990 + 17218*la + 253*la^2 - 
          8*En^6*(-2450 - 7*la + la^2) - 20*En^4*(21210 - 721*la + 5*la^2) + 
          6*En^2*(167310 - 10506*la + 119*la^2)) + 
        J^4*(14179347 + 33840*En^8 - 29299390*la + 837562*la^2 + 6636*la^3 + 
          48*En^6*(-111370 + 78641*la) + 24*En^4*(2071521 - 2463089*la + 
            22549*la^2) + 4*En^2*(-19597419 + 32274775*la - 741890*la^2 + 
            1614*la^3))*M^2 + 6*J^2*(33964768 + 56026008*En^4 - 16133441*la + 
          102684*la^2 + 2*En^2*(-74720608 + 18599669*la))*M^4 + 77472630*M^6)*
       rp[t]^12 + J^8*M*(J^4*(-963450 + 5226139*la + 11280*En^8*la - 
          241189*la^2 - 3318*la^3 + 72*En^6*(6850 - 25482*la + 79*la^2) + 
          En^2*(6050250 - 28542670*la + 1182418*la^2 - 6456*la^3) + 
          4*En^4*(-1097325 + 4412879*la - 96711*la^2 + 286*la^3)) + 
        2*J^2*(-38165241 + 19064016*En^6 + 37376044*la - 557626*la^2 - 
          3276*la^3 + 24*En^4*(-6898453 + 2505746*la) + 
          12*En^2*(19854783 - 13677653*la + 145229*la^2))*M^2 + 
        12*(42137637*En^2 + 71*(-182999 + 32796*la))*M^4)*rp[t]^13 + 
      J^6*(J^6*la*(-354630 + 25603*la + 553*la^2 - 
          8*En^6*(-21350 + 91*la + 3*la^2) - 4*En^4*(390180 - 15862*la + 
            143*la^2) + 6*En^2*(366450 - 25236*la + 269*la^2)) + 
        2*J^4*(6889365 + 169200*En^8 - 14012155*la + 374851*la^2 + 
          4914*la^3 + 24*En^6*(-408150 + 271991*la) + 
          60*En^4*(835995 - 985577*la + 11077*la^2) + 
          En^2*(-53463384 + 87047318*la - 2163656*la^2 + 4332*la^3))*M^2 + 
        12*J^2*(10217551 + 33648600*En^4 - 4701948*la + 22018*la^2 + 
          27*En^2*(-2336144 + 573505*la))*M^4 + 23661648*M^6)*rp[t]^14 + 
      J^6*M*(J^4*(-947970 + 5059661*la + 112800*En^8*la - 221315*la^2 - 
          4914*la^3 + 216*En^6*(9610 - 30958*la + 159*la^2) + 
          En^2*(8375400 - 38964030*la + 1747490*la^2 - 8664*la^3) + 
          4*En^4*(-2235150 + 8945362*la - 244477*la^2 + 738*la^3)) + 
        2*J^2*(-23423139 + 34434864*En^6 + 22253918*la - 255718*la^2 - 
          3332*la^3 + 240*En^4*(-848471 + 303172*la) + 
          12*En^2*(17064727 - 11562949*la + 129699*la^2))*M^2 + 
        12*(-4178003 + 20924646*En^2 + 678222*la)*M^4)*rp[t]^15 + 
      J^4*(J^6*la*(-347715 + 24132*la + 819*la^2 + 
          8*En^6*(88470 - 125*la + 7*la^2) - 36*En^4*(88610 - 4639*la + 
            41*la^2) + 6*En^2*(507015 - 37934*la + 361*la^2)) + 
        J^4*(8630937 + 2319696*En^8 - 17049334*la + 366910*la^2 + 9996*la^3 + 
          48*En^6*(-716624 + 499357*la) + 120*En^4*(1056342 - 1219921*la + 
            16115*la^2) + 4*En^2*(-23454819 + 37432239*la - 981680*la^2 + 
            1698*la^3))*M^2 + 12*J^2*(3449351 + 24713124*En^4 - 1455189*la - 
          1622*la^2 + En^2*(-32177258 + 7684110*la))*M^4 + 644409*M^6)*
       rp[t]^16 + (J^8*(-606555 + 3146593*la - 115441*la^2 - 4998*la^3 + 
          144*En^8*(-4620 + 5003*la) + 24*En^6*(122910 - 497702*la + 
            6597*la^2) + En^2*(7530300 - 34175090*la + 1616638*la^2 - 
            6792*la^3) + 20*En^4*(-581265 + 2272531*la - 74185*la^2 + 
            202*la^3))*M + 2*J^6*(-8283561 + 40776336*En^6 + 7288798*la - 
          2634*la^2 - 2300*la^3 + 24*En^4*(-6493843 + 2237276*la) + 
          12*En^2*(8968828 - 5890828*la + 66879*la^2))*M^3 + 
        J^4*(-3199683 + 59865912*En^2 - 94626*la)*M^5)*rp[t]^17 + 
      (J^8*la*(-221175 + 13409*la + 833*la^2 - 10560*En^8*(20 + la) + 
          8*En^6*(129250 - 4471*la + 37*la^2) - 20*En^4*(208200 - 13480*la + 
            101*la^2) + 6*En^2*(455025 - 35956*la + 283*la^2)) + 
        2*J^6*(1596321 - 165024*En^8 - 2945449*la + 15889*la^2 + 3450*la^3 + 
          24*En^6*(-918416 + 602113*la) + 12*En^4*(4263795 - 4684099*la + 
            67271*la^2) + 2*En^2*(-12754728 + 19632683*la - 518974*la^2 + 
            654*la^3))*M^2 + 3*J^4*(1334092 + 39246000*En^4 - 198189*la - 
          27768*la^2 + 8*En^2*(-4085857 + 906285*la))*M^4 - 1816596*J^2*M^6)*
       rp[t]^18 + (J^6*(-234495 + 449280*En^10 + 1144127*la - 18095*la^2 - 
          3450*la^3 - 576*En^8*(-3120 + 241*la) + 
          24*En^6*(177450 - 650618*la + 13253*la^2) + 
          En^2*(4265730 - 18527870*la + 879914*la^2 - 2616*la^3) + 
          4*En^4*(-2531340 + 9185224*la - 324103*la^2 + 742*la^3))*M + 
        4*J^4*(-524496 + 15039288*En^6 + 270853*la + 31658*la^2 - 516*la^3 + 
          60*En^4*(-551002 + 177579*la) + 18*En^2*(809647 - 493540*la + 
            4955*la^2))*M^3 - 9*J^2*(-335505 + 103866*En^2 + 86134*la)*M^5)*
       rp[t]^19 + (J^6*la*(-84480 + 149760*En^10 - 5760*En^8*(-105 + la) + 
          2978*la + 575*la^2 + 8*En^6*(191050 - 13299*la + 53*la^2) + 
          6*En^2*(256600 - 20270*la + 109*la^2) - 
          4*En^4*(904650 - 62755*la + 371*la^2)) + 
        J^4*(492705 + 4902480*En^8 - 650104*la - 68282*la^2 + 3096*la^3 + 
          144*En^6*(-272914 + 148473*la) + 24*En^4*(1966755 - 1982267*la + 
            28023*la^2) - 4*En^2*(3696201 - 5264045*la + 122846*la^2 + 
            6*la^3))*M^2 + 3*J^2*(-626062 + 4590864*En^4 + 433157*la - 
          12824*la^2 - 6*En^2*(74658 + 28439*la))*M^4 - 393129*M^6)*
       rp[t]^20 - 2*(J^4*(21150 + 777600*En^10 - 81857*la - 7480*la^2 + 
          774*la^3 - 1080*En^8*(-80 + 801*la) - 12*En^6*(236670 - 578138*la + 
            10911*la^2) + En^4*(2623950 - 8429362*la + 284138*la^2 - 
            500*la^3) - En^2*(666225 - 2658525*la + 111347*la^2 + 12*la^3))*
         M + J^2*(-263948 - 8563248*En^6 + En^4*(9217032 - 2452464*la) + 
          408525*la - 31052*la^2 + 272*la^3 + 6*En^2*(-200341 + 20173*la + 
            3022*la^2))*M^3 + 9*(-39254 + 138891*En^2 + 8854*la)*M^5)*
       rp[t]^21 - 2*((-1 + En^2)*J^4*la*(-7320 + 259200*En^8 - 
          9600*En^6*(-31 + la) - 516*la + 129*la^2 + 
          18*En^2*(12820 - 948*la + 7*la^2) - 4*En^4*(174510 - 10409*la + 
            31*la^2)) - J^2*(-30954 + 3405744*En^8 + 116675*la - 18249*la^2 + 
          408*la^3 + 24*En^6*(-282346 + 125589*la) + 
          12*En^4*(325821 - 271639*la + 2899*la^2) - 
          2*En^2*(250704 - 186300*la - 8165*la^2 + 96*la^3))*M^2 + 
        3*(81642 + 577332*En^4 - 47665*la + 1032*la^2 + 
          3*En^2*(-192358 + 53009*la))*M^4)*rp[t]^22 - 
      4*(-((-1 + En^2)*J^2*(216000*En^8 + 72*En^6*(-5720 + 8117*la) + 
           30*En^4*(7650 - 19846*la + 545*la^2) + En^2*(-31950 + 90814*la - 
             521*la^2 + 6*la^3) + 3*(-150 + 2353*la - 763*la^2 + 34*la^3))*
          M) + (-40528 + 471960*En^6 + 49398*la - 2556*la^2 + 16*la^3 + 
          6*En^4*(-143084 + 53013*la) + En^2*(428892 - 329577*la + 
            6006*la^2))*M^3)*rp[t]^23 - 4*(-1 + En^2)*
       (-((-1 + En^2)*J^2*la*(230 + 72000*En^6 - 203*la + 17*la^2 + 
           480*En^4*(-143 + 7*la) + 10*En^2*(1126 - 43*la + la^2))) + 
        (-6306 + 109044*En^6 + 16245*la - 1547*la^2 + 24*la^3 + 
          12*En^4*(-15329 + 13977*la) + 3*En^2*(27469 - 48782*la + 
            1900*la^2))*M^2)*rp[t]^24 - 8*(-1 + En^2)^2*
       (4320*En^6 + 18*En^4*(-380 + 1043*la) + 
        3*En^2*(930 - 4730*la + 337*la^2) + 2*(-90 + 625*la - 101*la^2 + 
          3*la^3))*M*rp[t]^25 - 8*(-1 + En^2)^3*la*(70 + 1440*En^4 + 
        120*En^2*(-8 + la) - 19*la + la^2)*rp[t]^26))/
    (En*rp[t]^16*(3*M + la*rp[t])*(J^2 + rp[t]^2)^9) + 
   (8*J^2*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*(17496675*J^24*M^7 - 
      441*J^24*(83364 - 21235*la + 5886*m^2)*M^6*rp[t] + 
      3*J^22*M^5*(J^2*(10364398 - 6381063*la + 389666*la^2 + 
          (1657637 - 340190*la)*m^2 + 9648*m^4) + 72706248*M^2)*rp[t]^2 + 
      3*J^22*M^4*(-(J^2*(4479072 - 5219024*la + 741545*la^2 - 7680*la^3 + 
           (1237912 - 648303*la + 11616*la^2)*m^2 - 8*(-2069 + 427*la)*m^4 + 
           8*m^6)) + 18*(-8490448 + 1507650*En^2 + 2150534*la - 570815*m^2)*
         M^2)*rp[t]^3 + J^20*M^3*(-2*J^4*(-1546164 + 3244291*la - 
          824486*la^2 + 19670*la^3 - 60*la^4 + (-671469 + 721004*la - 
            29918*la^2 + 68*la^3)*m^2 - 2*(7872 - 4403*la + 14*la^2)*m^4 + 
          2*(-9 + 2*la)*m^6) - 3*J^2*(-129310049 + 79195824*la - 
          4783984*la^2 + (-19736119 + 4026058*la)*m^2 - 104640*m^4 + 
          18*En^2*(2428460 - 800491*la + 267722*m^2))*M^2 + 1250214750*M^4)*
       rp[t]^4 + J^20*M^2*(2*J^4*(-176400 + 707152*la - 295653*la^2 + 
          12400*la^3 - 90*la^4 + 2*(-58260 + 129068*la - 9493*la^2 + 51*la^3)*
           m^2 + (-4359 + 5593*la - 42*la^2)*m^4 + (-9 + 6*la)*m^6) + 
        3*J^2*(-55907344 + 64834758*la - 9108876*la^2 + 95536*la^3 + 
          (-14782524 + 7695005*la - 133384*la^2)*m^2 + 40*(-4511 + 924*la)*
           m^4 - 72*m^6 + 2*En^2*(13329208 - 11182179*la + 889577*la^2 + 
            (3439532 - 942518*la)*m^2 + 36384*m^4))*M^2 + 
        18*(-146064818 + 53651790*En^2 + 36764533*la - 9285249*m^2)*M^4)*
       rp[t]^5 + J^18*M*(J^6*(15120 - 149856*la + 101643*la^2 - 6815*la^3 + 
          90*la^4 + (15312 - 88330*la + 10519*la^2 - 102*la^3)*m^2 + 
          (885 - 3101*la + 42*la^2)*m^4 + (3 - 6*la)*m^6) - 
        2*J^4*(-19308132 + 40347160*la - 10133437*la^2 + 244740*la^3 - 
          736*la^4 + (-8045529 + 8586656*la - 344512*la^2 + 752*la^3)*m^2 - 
          10*(17265 - 9576*la + 28*la^2)*m^4 + 18*(-9 + 2*la)*m^6 + 
          6*En^2*(1890402 - 3253401*la + 630449*la^2 - 6576*la^3 + 
            (880539 - 666290*la + 17138*la^2)*m^2 + (22332 - 6426*la)*m^4 + 
            24*m^6))*M^2 + 3*J^2*(741891640 + 22685400*En^4 - 451727478*la + 
          26953196*la^2 - 7*(-15334013 + 3107054*la)*m^2 + 510960*m^4 - 
          18*En^2*(28869066 - 9443249*la + 3035228*m^2))*M^4 + 
        4359252600*M^6)*rp[t]^6 + 
      J^18*(J^6*la*(5760 - 6608*la + 685*la^2 - 15*la^3 + 
          (5684 - 1068*la + 17*la^2)*m^2 - 7*(-45 + la)*m^4 + m^6) - 
        2*J^4*(2203920 - 8805857*la + 3635965*la^2 - 154324*la^3 + 
          1104*la^4 + (1401648 - 3086217*la + 219349*la^2 - 1128*la^3)*m^2 + 
          5*(9627 - 12236*la + 84*la^2)*m^4 + (81 - 54*la)*m^6 + 
          2*En^2*(-727560 + 2602616*la - 961790*la^2 + 23967*la^3 - 66*la^4 + 
            2*(-283113 + 504896*la - 31523*la^2 + 99*la^3)*m^2 - 
            18*(1483 - 1316*la + 7*la^2)*m^4 + 24*(-3 + la)*m^6))*M^2 - 
        3*J^2*(320925416 - 370211761*la + 51352378*la^2 - 545160*la^3 + 
          (80672500 - 41708177*la + 696640*la^2)*m^2 - 40*(-22146 + 4501*la)*
           m^4 + 288*m^6 + 24*En^4*(1058198 - 488983*la + 179158*m^2) - 
          2*En^2*(158843332 - 132285189*la + 10392083*la^2 - 
            4*(-9801578 + 2660405*la)*m^2 + 376416*m^4))*M^4 + 
        18*(-509587420 + 290996118*En^2 + 127360510*la - 30358491*m^2)*M^6)*
       rp[t]^7 + J^16*M*(J^6*(2*(94500 - 934560*la + 625414*la^2 - 
            42421*la^3 + 552*la^4) - 2*(-92529 + 530471*la - 61003*la^2 + 
            564*la^3)*m^2 + 5*(1971 - 6832*la + 84*la^2)*m^4 + 
          (27 - 54*la)*m^6 + 2*En^2*(-64260 + 612828*la - 409949*la^2 + 
            18867*la^3 - 132*la^4 + (-83574 + 424542*la - 50161*la^2 + 
              396*la^3)*m^2 - 3*(2280 - 6293*la + 84*la^2)*m^4 + 
            12*(-3 + 4*la)*m^6)) + J^4*(221791257 - 461345074*la + 
          114335122*la^2 - 2794132*la^3 + 8280*la^4 + 
          (88157808 - 93438380*la + 3609340*la^2 - 7560*la^3)*m^2 + 
          60*(28431 - 15631*la + 42*la^2)*m^4 - 144*(-9 + 2*la)*m^6 + 
          12*En^4*(2409051 - 3118902*la + 345260*la^2 + (1010404 - 416464*la)*
             m^2 + 19568*m^4) - 12*En^2*(22590555 - 38612604*la + 
            7383169*la^2 - 79062*la^3 + (10100547 - 7568528*la + 186524*la^2)*
             m^2 + (233508 - 66234*la)*m^4 + 192*m^6))*M^2 + 
        3*J^2*(258075504*En^4 + 5*(517967373 - 313322992*la + 
            18432160*la^2) + (352186929 - 70821862*la)*m^2 + 1478400*m^4 - 
          6*En^2*(470858562 - 152706299*la + 46881966*m^2))*M^4 + 
        10305810765*M^6)*rp[t]^8 - 
      J^16*(J^6*la*(-72000 + 81375*la - 8531*la^2 + 184*la^3 + 
          (-68656 + 12447*la - 188*la^2)*m^2 + 70*(-50 + la)*m^4 - 9*m^6 + 
          6*En^2*(7920 - 10059*la + 794*la^2 - 11*la^3 + 
            (10136 - 2137*la + 33*la^2)*m^2 + (805 - 21*la)*m^4 + 4*m^6)) + 
        J^4*(25330716 - 100838267*la + 41054800*la^2 - 1762618*la^3 + 
          12420*la^4 - 2*(-7714251 + 16866773*la - 1153040*la^2 + 5670*la^3)*
           m^2 + 90*(5324 - 6699*la + 42*la^2)*m^4 - 216*(-3 + 2*la)*m^6 + 
          12*En^4*(350586 - 1104213*la + 322456*la^2 - 3666*la^3 + 
            2*(143960 - 192387*la + 7795*la^2)*m^2 - 8*(-1823 + 861*la)*m^4 + 
            40*m^6) + 4*En^2*(-8721882 + 31005905*la - 11295464*la^2 + 
            288591*la^3 - 780*la^4 + (-6544044 + 11552527*la - 690988*la^2 + 
              2004*la^3)*m^2 - 6*(47121 - 41132*la + 189*la^2)*m^4 + 
            192*(-3 + la)*m^6))*M^2 - 3*J^2*(5500080*En^6 + 
          20*(-56050304 + 64275984*la - 8786131*la^2 + 94348*la^3) + 
          (-265756320 + 136332683*la - 2184120*la^2)*m^2 + 
          1120*(-2301 + 464*la)*m^4 - 672*m^6 - 
          24*En^4*(12114497 - 5537260*la + 1957774*m^2) + 
          6*En^2*(288639564 - 238427517*la + 18461629*la^2 + 
            (67702772 - 18177970*la)*m^2 + 578368*m^4))*M^4 - 
        9*(-2411157640 + 1907328924*En^2 + 597799985*la - 133058508*m^2)*M^6)*
       rp[t]^9 + J^14*M*(J^6*(1086750 - 10721421*la + 7067389*la^2 - 
          484759*la^3 + 6210*la^4 + (1024047 - 5829054*la + 644035*la^2 - 
            5670*la^3)*m^2 + 15*(3300 - 11305*la + 126*la^2)*m^4 - 
          108*(-1 + 2*la)*m^6 - 4*En^4*(-45360 + 437256*la - 275041*la^2 + 
            7993*la^3 - 26*la^4 + (-70620 + 320498*la - 34675*la^2 + 
              190*la^3)*m^2 - 12*(635 - 1281*la + 14*la^2)*m^4 + 
            20*(-3 + 2*la)*m^6) + 2*En^2*(-773010 + 7333542*la - 
            4830416*la^2 + 227598*la^3 - 1560*la^4 + 
            (-974772 + 4898718*la - 554518*la^2 + 4008*la^3)*m^2 - 
            3*(24540 - 66437*la + 756*la^2)*m^4 + 96*(-3 + 4*la)*m^6)) - 
        2*J^4*(-5*(77523246 - 160427326*la + 39156891*la^2 - 967616*la^3 + 
            2824*la^4) + 6*(-24312846 + 25564435*la - 945910*la^2 + 
            1900*la^3)*m^2 - 840*(2973 - 1620*la + 4*la^2)*m^4 + 
          168*(-9 + 2*la)*m^6 + 72*En^6*(72262 - 56017*la + 20972*m^2) - 
          12*En^4*(13892514 - 17789487*la + 1939496*la^2 + 
            (5585986 - 2266936*la)*m^2 + 98672*m^4) + 
          6*En^2*(123529500 - 209534538*la + 39459323*la^2 - 433268*la^3 + 
            (52702548 - 39053110*la + 915346*la^2)*m^2 - 
            56*(-19452 + 5431*la)*m^4 + 672*m^6))*M^2 + 
        3*J^2*(1334193768*En^4 + 5*(1226307372 - 736295381*la + 
            42606782*la^2) + (775016226 - 154511948*la)*m^2 + 2805600*m^4 - 
          6*En^2*(1547198384 - 496953569*la + 144571824*m^2))*M^4 + 
        17420065200*M^6)*rp[t]^10 + 
      J^14*(J^6*la*(3*(138000 - 153392*la + 16258*la^2 - 345*la^3 + 
            7*(18079 - 3145*la + 45*la^2)*m^2 - 35*(-167 + 3*la)*m^4 + 
            12*m^6) + 4*En^4*(16290 - 22701*la + 1352*la^2 - 13*la^3 + 
            5*(5026 - 1199*la + 19*la^2)*m^2 - 28*(-95 + 3*la)*m^4 + 
            20*m^6) - 6*En^2*(95280 - 118986*la + 9600*la^2 - 130*la^3 + 
            2*(59094 - 11943*la + 167*la^2)*m^2 + (8645 - 189*la)*m^4 + 
            32*m^6)) + 2*J^4*(-5*(8859852 - 35124684*la + 14073041*la^2 - 
            610762*la^3 + 4236*la^4) + 3*(-8555823 + 18554384*la - 
            1213115*la^2 + 5700*la^3)*m^2 - 420*(1683 - 2096*la + 12*la^2)*
           m^4 + 252*(-3 + 2*la)*m^6 + 24*En^6*(35589 - 97586*la + 
            17600*la^2 - 4*(-7184 + 5961*la)*m^2 + 1240*m^4) - 
          12*En^4*(2039499 - 6353202*la + 1824887*la^2 - 21744*la^3 + 
            7*(230720 - 303113*la + 11705*la^2)*m^2 + (75236 - 34552*la)*
             m^4 + 140*m^6) - 6*En^2*(-3*(5317785 - 18773816*la + 
              6729199*la^2 - 176109*la^3 + 466*la^4) + 
            (-11479164 + 20031641*la - 1139034*la^2 + 3042*la^3)*m^2 - 
            56*(7968 - 6826*la + 27*la^2)*m^4 + 224*(-3 + la)*m^6))*M^2 + 
        3*J^2*(61503408*En^6 + 5*(-531210624 + 605074933*la - 81313187*la^2 + 
            882456*la^3) + (-587610480 + 298753702*la - 4566720*la^2)*m^2 + 
          560*(-8781 + 1757*la)*m^4 - 1008*m^6 - 
          24*En^4*(63059883 - 28474719*la + 9657464*m^2) + 
          2*En^2*(2853891840 - 2335882493*la + 177848933*la^2 + 
            (630853536 - 167260720*la)*m^2 + 4662336*m^4))*M^4 + 
        36*(-1019838008 + 1050731094*En^2 + 250517290*la - 51319065*m^2)*M^6)*
       rp[t]^11 + J^12*M*(J^6*(5*(760725 - 7484592*la + 4850038*la^2 - 
            336188*la^3 + 4236*la^4) - 3*(-1143051 + 6452042*la - 
            680560*la^2 + 5700*la^3)*m^2 + 1260*(117 - 396*la + 4*la^2)*m^4 - 
          252*(-1 + 2*la)*m^6 - 24*En^6*(2520 - 28516*la + 16073*la^2 - 
            211*la^3 + 8*(589 - 2624*la + 213*la^2)*m^2 + (680 - 868*la)*
             m^4 + 8*m^6) - 4*En^4*(-532980 + 5083218*la - 3141538*la^2 + 
            95304*la^3 - 308*la^4 + 7*(-115260 + 513102*la - 52985*la^2 + 
              250*la^3)*m^2 - 28*(2895 - 5642*la + 48*la^2)*m^4 + 
            140*(-3 + 2*la)*m^6) + 6*En^2*(-3*(473130 - 4462533*la + 
              2888431*la^2 - 139259*la^3 + 932*la^4) + 
            (-1727754 + 8577112*la - 922899*la^2 + 6084*la^3)*m^2 - 
            28*(4230 - 11191*la + 108*la^2)*m^4 + 112*(-3 + 4*la)*m^6)) + 
        6*J^4*(124200*En^8 + 5*(61274670 - 126063524*la + 30229719*la^2 - 
            754680*la^3 + 2168*la^4) - 2*(-54056379 + 56309740*la - 
            1984010*la^2 + 3820*la^3)*m^2 + 140*(11418 - 6167*la + 14*la^2)*
           m^4 - 84*(-9 + 2*la)*m^6 - 24*En^6*(822784 - 625877*la + 
            228452*m^2) + 2*En^4*(145805763 - 184437528*la + 19758956*la^2 - 
            8*(-6980129 + 2783474*la)*m^2 + 883904*m^4) - 
          2*En^2*(408530673 - 687031347*la + 127120067*la^2 - 1428502*la^3 + 
            4*(41262843 - 30183968*la + 666116*la^2)*m^2 - 
            56*(-52908 + 14539*la)*m^4 + 1344*m^6))*M^2 + 
        6*J^2*(5191746393 + 2068048800*En^4 - 3090470128*la + 
          175383376*la^2 - 7*(-85823037 + 16943918*la)*m^2 + 1823808*m^4 - 
          6*En^2*(1709731240 - 543133509*la + 148138158*m^2))*M^4 + 
        21615803748*M^6)*rp[t]^12 + 
      J^12*(J^6*la*(10*(144900 - 158069*la + 16928*la^2 - 353*la^3) + 
          3*(423437 - 70170*la + 950*la^2)*m^2 - 840*(-62 + la)*m^4 + 
          84*m^6 - 8*En^6*(2610 - 4081*la + 160*la^2 - la^3 + 
            8*(609 - 172*la + 3*la^2)*m^2 - 28*(-25 + la)*m^4 + 8*m^6) + 
          28*En^4*(27360 - 37464*la + 2318*la^2 - 22*la^3 + 
            5*(8206 - 1877*la + 25*la^2)*m^2 + (4040 - 96*la)*m^4 + 20*m^6) - 
          6*En^2*(524895 - 643158*la + 53042*la^2 - 699*la^3 + 
            (628208 - 120769*la + 1521*la^2)*m^2 - 756*(-55 + la)*m^4 + 
            112*m^6)) - 6*J^4*(5*(7008510 - 27655230*la + 10876969*la^2 - 
            476723*la^3 + 3252*la^4) + (19147917 - 41119016*la + 
            2553940*la^2 - 11460*la^3)*m^2 + 70*(6513 - 8029*la + 42*la^2)*
           m^4 - 126*(-3 + 2*la)*m^6 + 24*En^8*(784 - 2117*la + 560*m^2) - 
          8*En^6*(414609 - 1112488*la + 196548*la^2 - 4*(-80792 + 64837*la)*
             m^2 + 12760*m^4) + 2*En^4*(21612582 - 66502641*la + 
            18743710*la^2 - 233878*la^3 + 4*(4104885 - 5287048*la + 
              193030*la^2)*m^2 - 56*(-12417 + 5494*la)*m^4 + 840*m^6) + 
          2*En^2*(-52966023 + 185541029*la - 65275523*la^2 + 1747023*la^3 - 
            4512*la^4 + 4*(-9073506 + 15627521*la - 835564*la^2 + 2052*la^3)*
             m^2 - 56*(22002 - 18494*la + 63*la^2)*m^4 + 448*(-3 + la)*m^6))*
         M^2 + 6*J^2*(-2251082352 + 155313504*En^6 + 2544226514*la - 
          335086540*la^2 + 3671280*la^3 - 7*(65440836 - 32929471*la + 
            477528*la^2)*m^2 + 22792*(-141 + 28*la)*m^4 - 504*m^6 - 
          96*En^4*(24616744 - 10965189*la + 3535931*m^2) + 
          2*En^2*(3164556444 - 2563085299*la + 191289541*la^2 + 
            (652253196 - 170299430*la)*m^2 + 4015872*m^4))*M^4 + 
        252*(-181011142 + 233993298*En^2 + 43980881*la - 8125791*m^2)*M^6)*
       rp[t]^13 + J^10*M*(J^6*(30*(301140 - 2953486*la + 1876663*la^2 - 
            131327*la^3 + 1626*la^4) + (7735707 - 43214454*la + 
            4317630*la^2 - 34380*la^3)*m^2 + 210*(1371 - 4585*la + 42*la^2)*
           m^4 - 378*(-1 + 2*la)*m^6 - 2688*En^8*la*(14 - 7*la + 10*m^2) - 
          24*En^6*(30240 - 333076*la + 183933*la^2 - 2615*la^3 + 
            24*(2298 - 9847*la + 755*la^2)*m^2 + (7440 - 8876*la)*m^4 + 
            48*m^6) - 4*En^4*(-2857680 + 26908068*la - 16298859*la^2 + 
            516215*la^3 - 1642*la^4 + 4*(-1049895 + 4557526*la - 
              446515*la^2 + 1790*la^3)*m^2 - 28*(13995 - 25922*la + 168*la^2)*
             m^4 + 420*(-3 + 2*la)*m^6) + 6*En^2*(-4731300 + 44350237*la - 
            28137132*la^2 + 1386468*la^3 - 9024*la^4 + 
            8*(-690291 + 3382895*la - 341927*la^2 + 2052*la^3)*m^2 - 
            28*(11910 - 30779*la + 252*la^2)*m^4 + 224*(-3 + 4*la)*m^6)) + 
        12*J^4*(259897503 + 745200*En^8 - 531142252*la + 124731987*la^2 - 
          3142180*la^3 + 8880*la^4 - 7*(-12121317 + 12488144*la - 
            416248*la^2 + 768*la^3)*m^2 + 14*(75087 - 40208*la + 84*la^2)*
           m^4 - 42*(-9 + 2*la)*m^6 - 24*En^6*(2123001 - 1579024*la + 
            562398*m^2) + 8*En^4*(57370542 - 71641860*la + 7520642*la^2 + 
            (20695843 - 8110438*la)*m^2 + 281876*m^4) - 
          2*En^2*(454902777 - 757335819*la + 137255509*la^2 - 1573864*la^3 + 
            7*(24637287 - 17732534*la + 363614*la^2)*m^2 - 
            42*(-61478 + 16639*la)*m^4 + 840*m^6))*M^2 + 
        42*J^2*(922678812 + 610550568*En^4 - 543653462*la + 30135932*la^2 + 
          (95666901 - 18681406*la)*m^2 + 234960*m^4 - 
          6*En^2*(381935814 - 119818409*la + 30151524*m^2))*M^4 + 
        19873652400*M^6)*rp[t]^14 + 
      J^10*(J^6*la*(3*(-5*(-229425 + 244996*la - 26482*la^2 + 542*la^3) + 
            (954163 - 149220*la + 1910*la^2)*m^2 - 490*(-69 + la)*m^4 + 
            42*m^6) - 8*En^6*(31410 - 48237*la + 2024*la^2 - 13*la^3 + 
            8*(7154 - 1935*la + 25*la^2)*m^2 - 28*(-275 + 7*la)*m^4 + 
            48*m^6) - 6*En^2*(1750455 - 2098367*la + 176848*la^2 - 
            2256*la^3 + 8*(251027 - 45337*la + 513*la^2)*m^2 - 
            196*(-595 + 9*la)*m^4 + 224*m^6) + 4*En^4*(1026270 - 1379193*la + 
            88684*la^2 - 821*la^3 + 20*(74634 - 16327*la + 179*la^2)*m^2 - 
            336*(-405 + 7*la)*m^4 + 420*m^6)) - 
        6*J^4*(59502000 - 233575158*la + 89888542*la^2 - 3973520*la^3 + 
          26640*la^4 - 7*(-4328589 + 9185394*la - 537842*la^2 + 2304*la^3)*
           m^2 + 42*(14383 - 17556*la + 84*la^2)*m^4 - 126*(-3 + 2*la)*m^6 + 
          96*En^8*(2548 - 6449*la + 1820*m^2) - 
          8*En^6*(2224569 - 5754014*la + 990640*la^2 - 
            4*(-420621 + 318913*la)*m^2 + 62280*m^4) + 
          4*En^4*(34207389 - 104343302*la + 28778668*la^2 - 376088*la^3 + 
            14*(1759265 - 2232588*la + 75930*la^2)*m^2 + (914452 - 390264*la)*
             m^4 + 700*m^6) + 2*En^2*(-118618269 + 411322154*la - 
            141587852*la^2 + 3863682*la^3 - 9708*la^4 + 
            28*(-2743071 + 4644177*la - 229903*la^2 + 519*la^3)*m^2 - 
            28*(77811 - 64232*la + 189*la^2)*m^4 + 560*(-3 + la)*m^6))*M^2 + 
        6*J^2*(461815776*En^6 - 168*En^4*(29383402 - 12850473*la + 
            3918670*m^2) + 42*En^2*(236445600 - 189314697*la + 
            13791931*la^2 + (44760632 - 11468748*la)*m^2 + 218752*m^4) - 
          7*(400573752 - 448569679*la + 57655658*la^2 - 636888*la^3 + 
            (73436412 - 36519503*la + 499392*la^2)*m^2 + (416160 - 82040*la)*
             m^4 + 48*m^6))*M^4 + 252*(-166736204 + 264266802*En^2 + 
          39979234*la - 6469941*m^2)*M^6)*rp[t]^15 + 
      J^8*M*(J^6*(-34944*En^8*la*(14 - 7*la + 10*m^2) - 
          48*En^6*(94500 - 896567*la + 479716*la^2 - 7206*la^3 + 
            4*(41385 - 153643*la + 11151*la^2)*m^2 + (21060 - 21434*la)*m^4 + 
            60*m^6) - 8*En^4*(-4454730 + 42604953*la - 25263162*la^2 + 
            839168*la^3 - 2584*la^4 + 14*(-452235 + 1953406*la - 
              179645*la^2 + 610*la^3)*m^2 - 42*(12805 - 22598*la + 112*la^2)*
             m^4 + 350*(-3 + 2*la)*m^6) + 3*(5116545 - 50022764*la + 
            31068992*la^2 - 2191780*la^3 + 26640*la^4 - 
            7*(-588837 + 3246706*la - 304468*la^2 + 2304*la^3)*m^2 + 
            14*(9165 - 30296*la + 252*la^2)*m^4 - 126*(-1 + 2*la)*m^6) + 
          6*En^2*(-10692360 + 99022147*la - 61345758*la^2 + 3080322*la^3 - 
            19416*la^4 + 14*(-847455 + 4079082*la - 380141*la^2 + 2076*la^3)*
             m^2 - 14*(42900 - 108437*la + 756*la^2)*m^4 + 
            280*(-3 + 4*la)*m^6)) + 6*J^4*(8910432*En^8 - 
          48*En^6*(6259481 - 4686992*la + 1551098*m^2) + 
          28*En^4*(69603435 - 84918858*la + 8689708*la^2 - 
            40*(-583922 + 222971*la)*m^2 + 253840*m^4) - 
          28*En^2*(102357072 - 168653526*la + 29828595*la^2 - 347546*la^3 + 
            (36001635 - 25380304*la + 476140*la^2)*m^2 + (426588 - 113834*la)*
             m^4 + 96*m^6) - 7*(-92602479 + 187763490*la - 42990034*la^2 + 
            1091220*la^3 - 3032*la^4 + 4*(-6855918 + 6973223*la - 
              218351*la^2 + 386*la^3)*m^2 - 20*(13695 - 7273*la + 14*la^2)*
             m^4 + 8*(-9 + 2*la)*m^6))*M^2 + 
        6*J^2*(5961172365 + 6141493008*En^4 - 3468511408*la + 
          186787456*la^2 + (536681433 - 103537814*la)*m^2 + 1015680*m^4 - 
          42*En^2*(432831266 - 133765229*la + 30242106*m^2))*M^4 + 
        13465795725*M^6)*rp[t]^16 + 
      J^8*(J^6*la*(-32*En^6*(47430 - 67920*la + 2789*la^2 - 19*la^3 + 
            14*(6030 - 1517*la + 13*la^2)*m^2 - 21*(-515 + 7*la)*m^4 + 
            30*m^6) + 3*(1949010 - 2031862*la + 221310*la^2 - 4440*la^3 + 
            21*(72513 - 10582*la + 128*la^2)*m^2 - 196*(-230 + 3*la)*m^4 + 
            42*m^6) + 16*En^4*(807660 - 1078959*la + 73169*la^2 - 646*la^3 + 
            35*(32270 - 6785*la + 61*la^2)*m^2 - 14*(-6635 + 84*la)*m^4 + 
            175*m^6) - 6*En^2*(3952095 - 4601723*la + 395252*la^2 - 
            4854*la^3 + 14*(308155 - 51071*la + 519*la^2)*m^2 - 
            98*(-2135 + 27*la)*m^4 + 280*m^6)) - 
        6*J^4*(96*En^8*(30669 - 40355*la + 17262*m^2) - 
          8*En^6*(5808945 - 17093738*la + 2919416*la^2 + 
            (4505992 - 3535028*la)*m^2 + 163400*m^4) + 
          28*En^4*(10695663 - 31423679*la + 8398052*la^2 - 114270*la^3 + 
            10*(709441 - 876913*la + 26815*la^2)*m^2 + (211420 - 87440*la)*
             m^4 + 100*m^6) + 14*En^2*(-3*(8938629 - 30705572*la + 
              10313888*la^2 - 285626*la^3 + 696*la^4) + 
            2*(-8157696 + 13492733*la - 606900*la^2 + 1260*la^3)*m^2 - 
            4*(91191 - 74032*la + 189*la^2)*m^4 + 64*(-3 + la)*m^6) - 
          7*(-10608810 + 41401919*la - 15518480*la^2 + 690738*la^3 - 
            4548*la^4 + (-4945197 + 10345362*la - 566408*la^2 + 2316*la^3)*
             m^2 - 10*(7926 - 9583*la + 42*la^2)*m^4 + 12*(-3 + 2*la)*m^6))*
         M^2 + 6*J^2*(954304272*En^6 + 4*(-648297744 + 717626699*la - 
            89485627*la^2 + 995100*la^3) + (-415156800 + 203743651*la - 
            2611320*la^2)*m^2 + 80*(-22599 + 4424*la)*m^4 - 144*m^6 - 
          168*En^4*(42576507 - 18335400*la + 5252582*m^2) + 
          14*En^2*(806461512 - 637348757*la + 45019337*la^2 + 
            (136580148 - 34222930*la)*m^2 + 498144*m^4))*M^4 + 
        9*(-3172574740 + 6091912536*En^2 + 748291445*la - 101516982*m^2)*M^6)*
       rp[t]^17 + J^6*M*(J^6*(576*En^8*(6300 - 11385*la + 3239*la^2 + 
            (4760 - 5964*la)*m^2 + 280*m^4) - 48*En^6*(10080 - 2440335*la + 
            1428116*la^2 - 24494*la^3 + 4*(89480 - 418061*la + 31269*la^2)*
             m^2 + (61400 - 55790*la)*m^4 + 80*m^6) - 
          56*En^4*(-1507950 + 13207275*la - 7456557*la^2 + 258667*la^3 - 
            758*la^4 + 5*(-372180 + 1572318*la - 129631*la^2 + 374*la^3)*
             m^2 - 10*(12885 - 21836*la + 84*la^2)*m^4 + 50*(-3 + 2*la)*
             m^6) - 21*(-912150 + 8893870*la - 5375266*la^2 + 381534*la^3 - 
            4548*la^4 + (-681273 + 3695838*la - 322102*la^2 + 2316*la^3)*
             m^2 - 10*(1698 - 5551*la + 42*la^2)*m^4 + 12*(-1 + 2*la)*m^6) + 
          42*En^2*(-3*(811890 - 7440661*la + 4498488*la^2 - 228956*la^3 + 
              1392*la^4) + 4*(-644691 + 3019649*la - 253365*la^2 + 1260*la^3)*
             m^2 - 2*(51120 - 126637*la + 756*la^2)*m^4 + 32*(-3 + 4*la)*
             m^6)) + 12*J^4*(300301347 + 4347216*En^8 - 602831774*la + 
          133701157*la^2 - 3413592*la^3 + 9320*la^4 + 
          (78244284 - 78426190*la + 2290820*la^2 - 3880*la^3)*m^2 + 
          20*(29919 - 15764*la + 28*la^2)*m^4 - 12*(-9 + 2*la)*m^6 - 
          24*En^6*(14222813 - 9495757*la + 2936210*m^2) + 
          28*En^4*(51033012 - 61216005*la + 6115060*la^2 + 
            (16113908 - 5912228*la)*m^2 + 126136*m^4) - 
          14*En^2*(116661717 - 190308684*la + 32643629*la^2 - 384388*la^3 + 
            (37261398 - 25610042*la + 432566*la^2)*m^2 + (327072 - 86156*la)*
             m^4 + 48*m^6))*M^2 + 3*J^2*(12509020944*En^4 + 
          5*(1625596038 - 930824597*la + 48342230*la^2) + 
          (605860329 - 115356662*la)*m^2 + 822000*m^4 - 
          12*En^2*(2506905116 - 759264417*la + 147374808*m^2))*M^4 + 
        6574082040*M^6)*rp[t]^18 + 
      J^6*(J^6*la*(6720*En^8*(162 - 51*la - la^2 - 10*(-13 + la)*m^2 + 
            8*m^4) + 21*(347565 - 352440*la + 38592*la^2 - 758*la^3 + 
            (251111 - 33774*la + 386*la^2)*m^2 - 70*(-85 + la)*m^4 + 4*m^6) - 
          42*En^2*(898755 - 1020465*la + 88768*la^2 - 1044*la^3 + 
            4*(234097 - 34475*la + 315*la^2)*m^2 - 42*(-845 + 9*la)*m^4 + 
            32*m^6) - 32*En^6*(3*(6450 - 65784*la + 3355*la^2 - 21*la^3) + 
            14*(13290 - 4673*la + 27*la^2)*m^2 - 35*(-895 + 7*la)*m^4 + 
            40*m^6) + 56*En^4*(543150 - 643755*la + 46046*la^2 - 379*la^3 + 
            5*(133420 - 25223*la + 187*la^2)*m^2 + (44300 - 420*la)*m^4 + 
            50*m^6)) + 6*J^4*(-68897892 + 1546560*En^10 + 266876404*la - 
          96744870*la^2 + 4327156*la^3 - 27960*la^4 + 
          (-28549917 + 58744216*la - 2982430*la^2 + 11640*la^3)*m^2 - 
          20*(17433 - 20888*la + 84*la^2)*m^4 + 36*(-3 + 2*la)*m^6 - 
          96*En^8*(-180047 - 55857*la + 11830*m^2) + 
          8*En^6*(17526759 - 37435030*la + 5688344*la^2 + 
            (8708860 - 6654060*la)*m^2 + 242600*m^4) - 
          28*En^4*(16051905 - 45906930*la + 11983390*la^2 - 167464*la^3 + 
            2*(5091915 - 5991773*la + 157055*la^2)*m^2 + (214836 - 86552*la)*
             m^4 + 60*m^6) - 14*En^2*(-30576423 + 104505110*la - 
            34100054*la^2 + 952338*la^3 - 2244*la^4 + 
            2*(-8636592 + 13863981*la - 555614*la^2 + 1062*la^3)*m^2 - 
            8*(35382 - 28294*la + 63*la^2)*m^4 + 32*(-3 + la)*m^6))*M^2 + 
        3*J^2*(2555408160*En^6 + 5*(-709635936 + 773625586*la - 
            92818859*la^2 + 1037232*la^3) + (-472735320 + 228665083*la - 
            2730720*la^2)*m^2 + 40*(-36753 + 7147*la)*m^4 - 72*m^6 - 
          336*En^4*(43433273 - 18535485*la + 4846756*m^2) + 
          4*En^2*(4692697116 - 3643090553*la + 246499181*la^2 - 
            8*(-84547266 + 20660435*la)*m^2 + 1693152*m^4))*M^4 + 
        18*(-777949120 + 1806883662*En^2 + 179729530*la - 19056129*m^2)*M^6)*
       rp[t]^19 + J^4*M*(J^6*(-5760*En^10*(1176 - 635*la + 336*m^2) + 
          192*En^8*(-221760 + 170219*la + 21846*la^2 - 140*(222 + 115*la)*
             m^2 + 3360*m^4) - 48*En^6*(1030680 - 6820469*la + 2818385*la^2 - 
            55565*la^3 + 20*(31797 - 162463*la + 10689*la^2)*m^2 + 
            (97500 - 82390*la)*m^4 + 60*m^6) - 56*En^4*(-2425140 + 
            19708509*la - 10822188*la^2 + 385496*la^3 - 1060*la^4 + 
            (-2837880 + 11206942*la - 774175*la^2 + 1910*la^3)*m^2 - 
            2*(67365 - 110494*la + 336*la^2)*m^4 + 30*(-3 + 2*la)*m^6) - 
          3*(-5921685 + 57569204*la - 33602556*la^2 + 2393936*la^3 - 
            27960*la^4 + (-3993249 + 21246518*la - 1703720*la^2 + 11640*la^3)*
             m^2 - 60*(1255 - 4060*la + 28*la^2)*m^4 + 36*(-1 + 2*la)*m^6) + 
          42*En^2*(-2763090 + 25418759*la - 15003406*la^2 + 768102*la^3 - 
            4488*la^4 + 2*(-1408581 + 6356044*la - 468329*la^2 + 2124*la^3)*
             m^2 - 4*(20130 - 48979*la + 252*la^2)*m^4 + 16*(-3 + 4*la)*
             m^6)) + 6*J^4*(97220016*En^8 + 5*(82481802 - 163283197*la + 
            34750232*la^2 - 890602*la^3 + 2388*la^4) + 
          (90039267 - 88796020*la + 2403170*la^2 - 3900*la^3)*m^2 + 
          10*(48924 - 25585*la + 42*la^2)*m^4 - 6*(-9 + 2*la)*m^6 - 
          48*En^6*(19873151 - 12746001*la + 4071126*m^2) + 
          28*En^4*(103579707 - 124671576*la + 12155068*la^2 - 
            4*(-7722317 + 2691842*la)*m^2 + 154336*m^4) - 
          4*En^2*(680957730 - 1096003869*la + 179993699*la^2 - 2128334*la^3 + 
            2*(94153287 - 62861600*la + 941924*la^2)*m^2 - 
            12*(-93488 + 24339*la)*m^4 + 96*m^6))*M^2 + 
        3*J^2*(9155723328*En^4 + 5*(801413417 - 449438896*la + 
            22292912*la^2) - 7*(-32750693 + 6149214*la)*m^2 + 196800*m^4 - 
          6*En^2*(3001471164 - 882189509*la + 135955974*m^2))*M^4 + 
        2200787550*M^6)*rp[t]^20 + 
      (-(J^10*la*(80640*En^10*(27 - 3*la + 8*m^2) - 13440*En^8*
            (-1026 - la - la^2 - 5*(31 + 5*la)*m^2 + 16*m^4) - 
           3*(4*(564540 - 552706*la + 60659*la^2 - 1165*la^3) + 
             (1467403 - 179640*la + 1940*la^2)*m^2 - 280*(-94 + la)*m^4 + 
             12*m^6) + 42*En^2*(1019925 - 1147315*la + 100076*la^2 - 
             1122*la^3 + 2*(509797 - 64499*la + 531*la^2)*m^2 - 
             28*(-995 + 9*la)*m^4 + 16*m^6) - 56*En^4*(860760 - 953934*la + 
             70300*la^2 - 530*la^3 + 5*(203364 - 30931*la + 191*la^2)*m^2 + 
             (46140 - 336*la)*m^4 + 30*m^6) + 16*En^6*(1102860 - 748767*la + 
             50234*la^2 - 259*la^3 + 140*(4916 - 1713*la + 7*la^2)*m^2 - 
             70*(-1415 + 7*la)*m^4 + 60*m^6))) - 
        6*J^8*(16943040*En^10 + 5*(9494994 - 36357808*la + 12607557*la^2 - 
            565294*la^3 + 3582*la^4) + (16645623 - 33618184*la + 
            1570190*la^2 - 5850*la^3)*m^2 + 15*(9563 - 11361*la + 42*la^2)*
           m^4 - 9*(-3 + 2*la)*m^6 + 48*En^8*(1120556 - 753359*la + 
            39200*m^2) - 8*En^6*(27784479 - 52187006*la + 7782120*la^2 + 
            (13723536 - 9037084*la)*m^2 + 204840*m^4) + 
          28*En^4*(15914007 - 46840411*la + 12132086*la^2 - 170722*la^3 + 
            2*(5152885 - 5670804*la + 121890*la^2)*m^2 + (133868 - 52776*la)*
             m^4 + 20*m^6) + 2*En^2*(-178147065 + 606203888*la - 
            189667022*la^2 + 5301102*la^3 - 12048*la^4 + 
            4*(-22412406 + 34744829*la - 1218796*la^2 + 2148*la^3)*m^2 - 
            8*(122676 - 96782*la + 189*la^2)*m^4 + 64*(-3 + la)*m^6))*M^2 + 
        3*J^6*(2165485248*En^6 + 10*(-175998312 + 187921959*la - 
            21450646*la^2 + 240408*la^3) + (-180571180 + 86001377*la - 
            951720*la^2)*m^2 + 40*(-8839 + 1708*la)*m^4 - 8*m^6 - 
          192*En^4*(55867001 - 23380383*la + 5135963*m^2) + 
          6*En^2*(1891657200 - 1426373821*la + 90573311*la^2 + 
            (211648532 - 50358890*la)*m^2 + 318112*m^4))*M^4 + 
        18*J^4*(-262195898 + 742066038*En^2 + 59023885*la - 4310973*m^2)*M^6)*
       rp[t]^21 + (J^8*(3870720*En^12 + 17280*En^10*(3528 - 1947*la + 
            224*m^2) + 192*En^8*(292320 - 548308*la + 33405*la^2 - 
            980*(99 + 31*la)*m^2 + 5040*m^4) - 48*En^6*
           (2435580 - 10548813*la + 4035189*la^2 - 84393*la^3 + 
            12*(120178 - 418173*la + 18921*la^2)*m^2 + (86040 - 69314*la)*
             m^4 + 24*m^6) - 56*En^4*(-2231820 + 19771167*la - 
            11232327*la^2 + 400081*la^3 - 1022*la^4 + 
            2*(-1568265 + 5608358*la - 305665*la^2 + 650*la^3)*m^2 - 
            6*(14315 - 22874*la + 56*la^2)*m^4 + 10*(-3 + 2*la)*m^6) - 
          3*(-5*(817740 - 7897604*la + 4394241*la^2 - 313301*la^3 + 
              3582*la^4) + (-2368851 + 12327992*la - 900985*la^2 + 5850*la^3)*
             m^2 - 5*(6243 - 19999*la + 126*la^2)*m^4 + 9*(-1 + 2*la)*m^6) + 
          6*En^2*(-15805440 + 148007519*la - 84389968*la^2 + 4304352*la^3 - 
            24096*la^4 + 8*(-1897068 + 8188119*la - 518303*la^2 + 2148*la^3)*
             m^2 - 4*(70710 - 169337*la + 756*la^2)*m^4 + 32*(-3 + 4*la)*
             m^6))*M + 2*J^6*(351158112*En^8 + 5*(123628410 - 239785712*la + 
            48317677*la^2 - 1240180*la^3 + 3264*la^4) + 
          (104368749 - 101146640*la + 2520560*la^2 - 3920*la^3)*m^2 + 
          10*(35481 - 18424*la + 28*la^2)*m^4 + (18 - 4*la)*m^6 - 
          144*En^6*(15137447 - 11224596*la + 3776266*m^2) + 
          48*En^4*(2*(66043548 - 79551393*la + 7368931*la^2) + 
            (34111969 - 11253514*la)*m^2 + 93068*m^4) - 
          6*En^2*(831997188 - 1303850697*la + 200059317*la^2 - 2358752*la^3 + 
            (180683547 - 116956282*la + 1535914*la^2)*m^2 + 
            (637548 - 164234*la)*m^4 + 24*m^6))*M^3 + 
        3*J^4*(1361539088 + 4797784440*En^4 - 743260918*la + 34778636*la^2 + 
          (52298431 - 9677562*la)*m^2 + 21168*m^4 - 
          6*En^2*(1252845950 - 352238303*la + 37405716*m^2))*M^5 + 
        454630104*J^2*M^7)*rp[t]^22 + 
      (J^8*la*(1290240*En^12 + 80640*En^10*(249 - la + 16*m^2) + 
          6720*En^8*(2922 + 295*la + 5*la^2 - 20*(46 + 5*la)*m^2 + 48*m^4) + 
          3*(-5*(-312135 + 290416*la - 31827*la^2 + 597*la^3) + 
            (867237 - 95520*la + 975*la^2)*m^2 - 35*(-311 + 3*la)*m^4 + 
            3*m^6) + 56*En^4*(789030 - 1026687*la + 74834*la^2 - 511*la^3 + 
            10*(111664 - 12493*la + 65*la^2)*m^2 - 8*(-3665 + 21*la)*m^4 + 
            10*m^6) - 16*En^6*(2450340 - 1146483*la + 83690*la^2 - 343*la^3 + 
            28*(55122 - 9571*la + 29*la^2)*m^2 - 42*(-2075 + 7*la)*m^4 + 
            24*m^6) - 6*En^2*(5861145 - 6552133*la + 565792*la^2 - 
            6024*la^3 + 8*(683186 - 72193*la + 537*la^2)*m^2 + 
            (97580 - 756*la)*m^4 + 32*m^6)) + 
        2*J^6*(90813312*En^10 - 5*(14353560 - 53903945*la + 17589237*la^2 - 
            788444*la^3 + 4896*la^4) + (-19575051 + 38745973*la - 
            1652945*la^2 + 5880*la^3)*m^2 - 5*(20931 - 24668*la + 84*la^2)*
           m^4 + (-9 + 6*la)*m^6 - 288*En^8*(857238 - 470531*la + 
            136080*m^2) + 24*En^6*(15374259 - 42954978*la + 7489248*la^2 + 
            (14722028 - 8181068*la)*m^2 + 91960*m^4) - 
          12*En^4*(76962591 - 240003658*la + 60215740*la^2 - 834504*la^3 + 
            (48229970 - 49432256*la + 845660*la^2)*m^2 - 
            4*(-81959 + 31738*la)*m^4 + 20*m^6) - 
          6*En^2*(-3*(36566121 - 121970338*la + 35504566*la^2 - 984691*la^3 + 
              2154*la^4) + (-44255814 + 66101084*la - 2001306*la^2 + 
              3258*la^3)*m^2 + (-281562 + 219464*la - 378*la^2)*m^4 + 
            8*(-3 + la)*m^6))*M^2 + 3*J^4*(-604034632 + 1394693568*En^6 + 
          627052829*la - 67107610*la^2 + 752680*la^3 + 
          (-41605092 + 19497741*la - 198976*la^2)*m^2 + 56*(-682 + 131*la)*
           m^4 - 24*En^4*(241048902 - 94701867*la + 14621558*m^2) + 
          En^2*(4834123256 - 3476151106*la + 200979614*la^2 + 
            (355693264 - 82378920*la)*m^2 + 237888*m^4))*M^4 + 
        54*J^2*(-18226548 + 63649274*En^2 + 3974594*la - 148293*m^2)*M^6)*
       rp[t]^23 + (J^6*(-30965760*En^12 + 5*(1248345 - 11855436*la + 
            6156948*la^2 - 437842*la^3 + 4896*la^4) + 
          (2838657 - 14421904*la + 952630*la^2 - 5880*la^3)*m^2 + 
          5*(4587 - 14560*la + 84*la^2)*m^4 + (3 - 6*la)*m^6 + 
          2304*En^10*(-38640 + 24877*la + 3780*m^2) + 
          1152*En^8*(111510 - 137693*la + 11733*la^2 - 70*(-91 + 355*la)*
             m^2 + 560*m^4) - 48*En^6*(563220 - 6531313*la + 4204224*la^2 - 
            83950*la^3 + 4*(529999 - 1312521*la + 36645*la^2)*m^2 + 
            (39820 - 31038*la)*m^4 + 4*m^6) - 8*En^4*(-8280090 + 
            98008299*la - 57585222*la^2 + 1990880*la^3 - 4696*la^4 + 
            2*(-8020545 + 25928286*la - 1076615*la^2 + 1990*la^3)*m^2 + 
            (-214230 + 335188*la - 672*la^2)*m^4 + 10*(-3 + 2*la)*m^6) + 
          6*En^2*(-3*(3225600 - 30236839*la + 16016369*la^2 - 805231*la^3 + 
              4308*la^4) + (-7792188 + 32085370*la - 1716591*la^2 + 
              6516*la^3)*m^2 + (-82080 + 193837*la - 756*la^2)*m^4 + 
            4*(-3 + 4*la)*m^6))*M + J^4*(429738633 - 69315264*En^8 - 
          809147642*la + 151648922*la^2 - 3888420*la^3 + 10040*la^4 - 
          4*(-12170460 + 11581447*la - 264299*la^2 + 394*la^3)*m^2 + 
          28*(2751 - 1419*la + 2*la^2)*m^4 - 288*En^6*(8874111 - 7256004*la + 
            1987198*m^2) + 12*En^4*(589460331 - 665649870*la + 
            54385196*la^2 - 28*(-3607633 + 1129528*la)*m^2 + 110768*m^4) + 
          12*En^2*(-363670989 + 541320888*la - 74693893*la^2 + 871678*la^3 + 
            (-51744843 + 32467216*la - 370396*la^2)*m^2 + 
            14*(-5718 + 1459*la)*m^4))*M^3 + 
        3*J^2*(287286469 + 1660841712*En^4 - 151599344*la + 6592864*la^2 + 
          (5444045 - 992526*la)*m^2 - 6*En^2*(330494546 - 87487743*la + 
            4647270*m^2))*M^5 + 43891875*M^7)*rp[t]^24 + 
      (J^6*la*(-10321920*En^12 - 5*(-476550 + 409243*la - 44591*la^2 + 
            816*la^3) + 7*(147827 - 14505*la + 140*la^2)*m^2 - 
          70*(-114 + la)*m^4 + m^6 + 322560*En^10*(-93 - 4*la + 9*m^2) + 
          26880*En^8*(1572 + 45*la + 5*la^2 - 25*(-4 + la)*m^2 + 8*m^4) - 
          32*En^6*(244620 - 708588*la + 44871*la^2 - 147*la^3 + 
            14*(79362 - 6415*la + 15*la^2)*m^2 - 7*(-2875 + 7*la)*m^4 + 
            2*m^6) - 6*En^2*(3613305 - 3803142*la + 320498*la^2 - 3231*la^3 + 
            (2789486 - 241621*la + 1629*la^2)*m^2 - 21*(-1345 + 9*la)*m^4 + 
            4*m^6) + 16*En^4*(1506780 - 2767737*la + 190871*la^2 - 
            1174*la^3 + 5*(566104 - 44867*la + 199*la^2)*m^2 + 
            (36470 - 168*la)*m^4 + 5*m^6)) + 
        J^4*(-50745600 + 57376512*En^10 + 184629319*la - 55427696*la^2 + 
          2476370*la^3 - 15060*la^4 + 2*(-4635024 + 8983113*la - 
            347896*la^2 + 1182*la^3)*m^2 - 14*(1632 - 1909*la + 6*la^2)*m^4 - 
          576*En^8*(-507639 - 56971*la + 184310*m^2) + 
          48*En^6*(3937419 - 26884854*la + 4800920*la^2 + 
            (8641544 - 4221212*la)*m^2 + 17080*m^4) + 
          12*En^4*(-88802028 + 261510729*la - 57044416*la^2 + 762346*la^3 - 
            14*(2688030 - 2581721*la + 34685*la^2)*m^2 + 56*(-1764 + 673*la)*
             m^4) + 4*En^2*(148980987 - 469280507*la + 120757856*la^2 - 
            3294171*la^3 + 6924*la^4 + (39118644 - 56305543*la + 
              1457452*la^2 - 2196*la^3)*m^2 + 42*(2547 - 1964*la + 3*la^2)*
             m^4))*M^2 + 3*J^2*(781448112*En^6 + 4*(-32340896 + 32364062*la - 
            3190059*la^2 + 35724*la^3) + (-4375744 + 2017087*la - 18904*la^2)*
           m^2 - 24*En^4*(88896253 - 31025092*la + 2305070*m^2) + 
          En^2*(1317318104 - 885145030*la + 44871350*la^2 + 
            (44974744 - 10146396*la)*m^2))*M^4 + 
        9*(-10690176 + 46606476*En^2 + 2245111*la)*M^6)*rp[t]^25 + 
      (J^4*(2260440 + 58060800*En^12 - 20733861*la + 9751433*la^2 - 
          689075*la^3 + 7530*la^4 + (685440 - 3396196*la + 201359*la^2 - 
            1182*la^3)*m^2 + 7*(360 - 1133*la + 6*la^2)*m^4 - 
          2304*En^10*(23100 - 6761*la + 840*m^2) + 
          192*En^8*(23*(-16380 + 21003*la + 4673*la^2) - 420*(-409 + 457*la)*
             m^2 + 840*m^4) + 48*En^6*(1161720 + 2578857*la - 2951936*la^2 + 
            52246*la^3 - 4*(371700 - 756503*la + 13191*la^2)*m^2 + 
            42*(-180 + 137*la)*m^4) + 4*En^4*(9287460 - 112236726*la + 
            56628489*la^2 - 1850141*la^3 + 4018*la^4 - 
            7*(-1926540 + 5716054*la - 179005*la^2 + 290*la^3)*m^2 + 
            28*(2340 - 3599*la + 6*la^2)*m^4) - 
          2*En^2*(13838580 - 121306251*la + 55360424*la^2 - 2713566*la^3 + 
            13848*la^4 + (7154280 - 28140414*la + 1260022*la^2 - 4392*la^3)*
             m^2 + 21*(1500 - 3499*la + 12*la^2)*m^4))*M + 
        2*J^2*(46932552 + 45504288*En^8 - 84857030*la + 14471627*la^2 - 
          369680*la^3 + 936*la^4 + (2592108 - 2421226*la + 50372*la^2 - 
            72*la^3)*m^2 - 72*En^6*(12752512 - 7327309*la + 876576*m^2) + 
          36*En^4*(39846970 - 38653137*la + 2504992*la^2 + 
            (2747890 - 821680*la)*m^2) - 6*En^2*(103638246 - 142486038*la + 
            16853631*la^2 - 193268*la^3 + 6*(1114096 - 678421*la + 6687*la^2)*
             m^2))*M^3 + 9*(9507480 + 94110552*En^4 - 4816621*la + 
          191422*la^2 + 6*En^2*(-13882232 + 3419023*la))*M^5)*rp[t]^26 + 
      (J^4*la*(860895 + 19353600*En^12 - 652728*la + 70370*la^2 - 1255*la^3 + 
          (248710 - 21573*la + 197*la^2)*m^2 - 7*(-125 + la)*m^4 - 
          322560*En^10*(54 + 3*la + 2*m^2) + 6720*En^8*(-3798 - 485*la + 
            25*la^2 + (1670 - 50*la)*m^2 + 8*m^4) + 
          32*En^6*(593460 + 595260*la - 29579*la^2 + 79*la^3 + 
            (-769300 + 33258*la - 62*la^2)*m^2 + 7*(-545 + la)*m^4) - 
          28*En^4*(-502470 + 823983*la - 51868*la^2 + 287*la^3 - 
            5*(134850 - 7585*la + 29*la^2)*m^2 + 12*(-265 + la)*m^4) + 
          6*En^2*(-1723995 + 1494323*la - 121152*la^2 + 1154*la^3 - 
            6*(141400 - 9949*la + 61*la^2)*m^2 + 7*(-515 + 3*la)*m^4)) - 
        2*J^2*(5696640 + 90310464*En^10 - 19785592*la + 5314265*la^2 - 
          235874*la^3 + 1404*la^4 + (501264 - 950960*la + 33269*la^2 - 
            108*la^3)*m^2 + 288*En^8*(-277899 - 131003*la + 71582*m^2) + 
          96*En^6*(-1883610 + 4195902*la - 454214*la^2 + 
            3*(-171011 + 76504*la)*m^2) + 12*En^4*(21303666 - 50032928*la + 
            8107599*la^2 - 102680*la^3 + 5*(641934 - 583719*la + 6155*la^2)*
             m^2) + 2*En^2*(-45394812 + 129448850*la - 27618179*la^2 + 
            734721*la^3 - 1482*la^4 + (-5190876 + 7215019*la - 158854*la^2 + 
              222*la^3)*m^2))*M^2 + 3*(-13102336 + 236432304*En^6 + 
          12535181*la - 1115313*la^2 + 12440*la^3 + 
          24*En^4*(-16351375 + 4953547*la) + 6*En^2*(28848792 - 17856763*la + 
            763195*la^2))*M^4)*rp[t]^27 + 
      2*(J^2*(264600 - 15482880*En^12 - 2293050*la + 940679*la^2 - 
          65786*la^3 + 702*la^4 + (37800 - 182628*la + 9668*la^2 - 54*la^3)*
           m^2 - 1728*En^10*(-25200 + 17211*la + 1400*m^2) - 
          96*En^8*(369180 - 211623*la - 78322*la^2 + 252*(-325 + 291*la)*
             m^2) - 12*En^6*(168840 - 6453410*la + 2428237*la^2 - 
            36843*la^3 + 8*(97650 - 177521*la + 2034*la^2)*m^2) - 
          2*En^4*(-2*(3468150 - 25421640*la + 8368379*la^2 - 253204*la^3 + 
              506*la^4) + 5*(-487620 + 1352370*la - 32139*la^2 + 46*la^3)*
             m^2) + En^2*(-4732560 + 35977980*la - 12891395*la^2 + 
            609711*la^3 - 2964*la^4 + (-982800 + 3706704*la - 138349*la^2 + 
              444*la^3)*m^2))*M + (4881816 + 143110584*En^8 - 8388782*la + 
          1270281*la^2 - 32236*la^3 + 80*la^4 + 
          72*En^6*(-4955026 + 1978209*la) + 18*En^4*(16404385 - 13343576*la + 
            627460*la^2) + 6*En^2*(-14418243 + 17995947*la - 1739687*la^2 + 
            19470*la^3))*M^3)*rp[t]^28 + 
      2*(-((-1 + En^2)*J^2*la*(5160960*En^10 + 201600*En^8*
            (-47 - la + 4*m^2) + 6720*En^6*(360 + 283*la - 7*la^2 + 
             5*(-55 + la)*m^2) + 3*(33390 - 21165*la + 2246*la^2 - 39*la^3 + 
             (4550 - 347*la + 3*la^2)*m^2) + 4*En^4*(859950 - 651633*la + 
             31820*la^2 - 97*la^3 + 8*(42525 - 1571*la + 4*la^2)*m^2) - 
           6*En^2*(2*(137550 - 83939*la + 6309*la^2 - 52*la^3) + 
             (55650 - 3133*la + 17*la^2)*m^2))) + 
        (-615168 + 27283392*En^10 + 2012466*la - 468985*la^2 + 20609*la^3 - 
          120*la^4 + 216*En^8*(-424448 + 252407*la) + 
          144*En^6*(791694 - 957855*la + 50614*la^2) + 
          6*En^4*(-10452384 + 19348357*la - 2088734*la^2 + 24750*la^3) + 
          18*En^2*(762384 - 1927825*la + 321405*la^2 - 8273*la^3 + 16*la^4))*
         M^2)*rp[t]^29 + 4*(-1 + En^2)*(-15120 + 967680*En^10 + 121554*la - 
        41801*la^2 + 2881*la^3 - 30*la^4 + 288*En^8*(-10920 + 16979*la) + 
        144*En^6*(26040 - 81800*la + 7047*la^2) + 
        6*En^4*(-325080 + 1546338*la - 265417*la^2 + 5615*la^3) + 
        En^2*(393120 - 2513862*la + 646429*la^2 - 28241*la^3 + 114*la^4))*M*
       rp[t]^30 + 4*(-1 + En^2)^2*la*(5670 + 322560*En^8 - 2849*la + 
        296*la^2 - 5*la^3 + 20160*En^6*(-37 + 5*la) + 
        5040*En^4*(110 - 29*la + la^2) + 2*En^2*(-68670 + 26509*la - 
          1828*la^2 + 13*la^3))*rp[t]^31))/(En*la*(1 + la)*rp[t]^17*
     (3*M + la*rp[t])*(J^2 + rp[t]^2)^12))*Derivative[1][rp][t]
]


Clear[gSourceJT1]
gSourceJT1[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];
	
	fp=SchwarzschildF[rp[t],M];

	(8*fp^2*J*mu*Pi*YPhiBar*(-156*J^2*M^3 + 2*J^2*(71 + 8*la)*M^2*rp[t] - 2*M*(J^2*(18 + 5*la - la^2) + 66*M^2)*rp[t]^2 + (J^2*(2 + la - la^2) + (114 - 60*En^2 + 12*la)*M^2)*rp[t]^3 + 
    2*(-13 - 3*la + la^2 + 3*En^2*(4 + la))*M*rp[t]^4 - (1 + la)*(-1 + 2*En^2 + la)*rp[t]^5))/(En^2*(1 + la)*rp[t]^8) + 
 (8*fp*mu*Pi*YBar*(24*J^2*M^2 - 2*J^2*(7 + 4*la)*M*rp[t] + 2*(J^2*(1 + la) + 12*M^2)*rp[t]^2 + 6*(-2 + En^2 - la)*M*rp[t]^3 - (-1 + 2*En^2)*(1 + la)*rp[t]^4)*Derivative[1][rp][t])/(En*rp[t]^6) + 
 YPhiPhiBar*(((8*I)*fp^4*J^3*m*mu*Pi*(120*J^4*M^3 - 102*J^4*M^2*rp[t] + J^2*M*(J^2*(21 - 2*la - 2*la^2) + 264*M^2)*rp[t]^2 + J^2*(J^2*la*(1 + la) + 6*(-38 + 7*En^2)*M^2)*rp[t]^3 + 
      4*M*(-(J^2*(-12 + 3*En^2 + la + la^2)) + 36*M^2)*rp[t]^4 + 2*(J^2*la*(1 + la) + (-63 + 33*En^2)*M^2)*rp[t]^5 - (-27 + 24*En^2 + 2*la + 2*la^2)*M*rp[t]^6 + la*(1 + la)*rp[t]^7))/
    (En^2*la*(1 + la)*USq^2*rp[t]^12) + (8*fp^4*J^2*mu*Pi*(294*J^6*M^3 - 3*J^6*(71 - 4*la + 2*m^2)*M^2*rp[t] + 3*J^4*M*(J^2*(12 - 6*la - 4*la^2 + m^2) + 314*M^2)*rp[t]^2 + 
      J^4*(4*J^2*la*(1 + la) + 3*(-227 + 16*En^2 + 12*la - 4*m^2)*M^2)*rp[t]^3 + 6*J^2*M*(J^2*(19 - 9*la - 6*la^2 - En^2*(2 + la) + m^2) + 171*M^2)*rp[t]^4 + 
      3*J^2*(4*J^2*la*(1 + la) + (-249 + 48*En^2 + 12*la - 2*m^2)*M^2)*rp[t]^5 + 3*M*(J^2*(42 - 18*la - 12*la^2 - 2*En^2*(5 + 2*la) + m^2) + 126*M^2)*rp[t]^6 + 
      3*(4*J^2*la*(1 + la) + (-93 + 48*En^2 + 4*la)*M^2)*rp[t]^7 - 6*(-8 + 3*la + 2*la^2 + En^2*(7 + la))*M*rp[t]^8 + 4*la*(1 + la)*rp[t]^9)*Derivative[1][rp][t])/(En*la*(1 + la)*USq^3*rp[t]^13))
]


Clear[fSourceJT1]
fSourceJT1[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];
	
	fp=SchwarzschildF[rp[t],M];

	(8*fp^3*J*mu*Pi*YPhiBar*(12*J^2*M^2 - 2*J^2*(4 + la)*M*rp[t] + (J^2*(1 + la) + 12*M^2)*rp[t]^2 + 2*(-4 + 3*En^2 - la)*M*rp[t]^3 + (1 + la)*rp[t]^4))/(En^2*(1 + la)*rp[t]^6) + 
 (8*fp*mu*Pi*USq*YBar*(-3*M + (1 + la)*rp[t])*Derivative[1][rp][t])/(En*rp[t]^2) + 
 YPhiPhiBar*(((-24*I)*fp^4*J^3*m*M*mu*Pi*(2*J^2*M - J^2*rp[t] + 2*M*rp[t]^2 + (-1 + 2*En^2)*rp[t]^3))/(En^2*la*(1 + la)*USq*rp[t]^8) - 
   (8*fp^4*J^2*mu*Pi*(21*J^4*M^2 - 9*J^4*M*rp[t] + (-(J^4*la*(1 + la)) + 48*J^2*M^2)*rp[t]^2 + 3*(-7 + 2*En^2)*J^2*M*rp[t]^3 + (-2*J^2*la*(1 + la) + 27*M^2)*rp[t]^4 + 6*(-2 + 3*En^2)*M*rp[t]^5 - 
      la*(1 + la)*rp[t]^6)*Derivative[1][rp][t])/(En*la*(1 + la)*USq^2*rp[t]^9))
]


Clear[gSourceJT2]
gSourceJT2[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];
	
	fp=SchwarzschildF[rp[t],M];

(8*fp^4*mu*Pi*YBar*(162*J^6*M^3 + 2*J^6*(-71 + 7*la)*M^2*rp[t] + 6*J^4*M*(-(J^2*(-5 + 2*la + la^2)) + 51*M^2)*rp[t]^2 + J^4*(3*J^2*la*(1 + la) + (-290 + 54*En^2 + 40*la)*M^2)*rp[t]^3 + 
    J^2*M*(J^2*(67 + 6*En^2*(-3 + la) - 33*la - 16*la^2) + 78*M^2)*rp[t]^4 + 2*J^2*(-((-4 + En^2)*J^2*la*(1 + la)) + (-53 + 36*En^2 + 19*la)*M^2)*rp[t]^5 - 
    2*M*(J^2*(-16 - 6*En^2*(-3 + la) + 15*la + 7*la^2) + 33*M^2)*rp[t]^6 + (-((-7 + 4*En^2)*J^2*la*(1 + la)) - 6*(-7 + 5*En^2 - 2*la)*M^2)*rp[t]^7 + (-5 + 6*En^2 - 4*la)*(1 + la)*M*rp[t]^8 - 
    2*(-1 + En^2)*la*(1 + la)*rp[t]^9))/(En*USq^2*rp[t]^12) + (16*fp^4*J*M*mu*Pi*YPhiBar*(294*J^6*M^2 - 3*J^6*(64 + 3*la)*M*rp[t] + (J^6*(29 + 2*la - 3*la^2) + 942*J^4*M^2)*rp[t]^2 + 
    3*J^4*(-204 + 16*En^2 - 7*la)*M*rp[t]^3 + 3*(J^4*(30 + la - 3*la^2 - 2*En^2*(2 + la)) + 342*J^2*M^2)*rp[t]^4 + 3*J^2*(-224 + 48*En^2 - 5*la)*M*rp[t]^5 + 
    3*(-(J^2*(2*En^2*(5 + 2*la) + 3*(-11 + la^2))) + 126*M^2)*rp[t]^6 + 3*(-84 + 48*En^2 - la)*M*rp[t]^7 - (-38 + la + 3*la^2 + 6*En^2*(7 + la))*rp[t]^8)*Derivative[1][rp][t])/
  ((1 + la)*USq^3*rp[t]^13) + YPhiPhiBar*((8*fp^7*J^2*mu*Pi*(-2520*J^10*M^4 + 3*J^10*(885 - 104*la + 94*m^2)*M^3*rp[t] - 3*J^8*M^2*(J^2*(293 - 94*la - 14*la^2 + 79*m^2) + 4884*M^2)*rp[t]^2 - 
      J^8*M*(2*J^2*(-45 + 35*la + 15*la^2 + la^3 + (-24 + la + la^2)*m^2) + 3*(-5157 + 372*En^2 + 536*la - 436*m^2)*M^2)*rp[t]^3 + 
      J^6*(J^4*la*(1 + la)*(4 + la + m^2) + 6*J^2*(-854 + 249*la + 41*la^2 - 188*m^2 + En^2*(106 - 13*la + 16*m^2))*M^2 - 35352*M^4)*rp[t]^4 - 
      J^6*M*(J^2*(2*(-261 + 193*la + 87*la^2 + 5*la^3) + (-237 + 8*la + 8*la^2)*m^2 + 2*En^2*(42 - 14*la - 8*la^2 + 15*m^2)) + 18*(-2087 + 312*En^2 + 184*la - 124*m^2)*M^2)*rp[t]^5 + 
      (-(J^8*la*(1 + la)*(-23 - 5*la + 2*En^2*(2 + la) - 4*m^2)) - 6*J^6*(2080 + 6*En^4 - 530*la - 98*la^2 + 327*m^2 + En^2*(-538 + 54*la - 68*m^2))*M^2 - 44784*J^4*M^4)*rp[t]^6 + 
      J^4*M*(J^2*(12*En^4*(1 + la) - 4*(-315 + 217*la + 105*la^2 + 5*la^3) - 3*(-141 + 4*la + 4*la^2)*m^2 - 2*En^2*(213 - 67*la - 40*la^2 + 75*m^2)) - 6*(-8097 + 1908*En^2 + 568*la - 278*m^2)*M^2)*
       rp[t]^7 - 2*(J^6*la*(1 + la)*(-29 - 5*la + En^2*(9 + 4*la) - 3*m^2) + 3*J^4*(2761 + 30*En^4 - 566*la - 118*la^2 + 248*m^2 - 2*En^2*(529 - 42*la + 44*m^2))*M^2 + 14760*J^2*M^4)*rp[t]^8 + 
      J^2*M*(J^2*(60*En^4*(1 + la) - 4*(-432 + 247*la + 129*la^2 + 5*la^3) + (327 - 8*la - 8*la^2)*m^2 - 10*En^2*(69 - 25*la - 16*la^2 + 21*m^2)) - 3*(-11025 + 4464*En^2 + 584*la - 154*m^2)*M^2)*
       rp[t]^9 + (-2*J^4*la*(1 + la)*(-38 - 5*la + En^2*(19 + 6*la) - 2*m^2) - 3*J^2*(3931 + 84*En^4 - 606*la - 142*la^2 + 139*m^2 - 4*En^2*(703 - 29*la + 18*m^2))*M^2 - 7956*M^4)*rp[t]^10 - 
      M*(J^2*(En^4*(132 - 84*la) + 2*(-657 + 283*la + 159*la^2 + 5*la^3) + (-93 + 2*la + 2*la^2)*m^2 + 6*En^2*(193 - 35*la - 24*la^2 + 15*m^2)) + 9*(-1027 + 716*En^2 + 40*la)*M^2)*rp[t]^11 + 
      (-(J^2*la*(1 + la)*(-50 - 5*la + En^2*(38 + 8*la) - m^2)) - 6*(577 + 210*En^4 + 15*En^2*(-52 + la) - 65*la - 17*la^2)*M^2)*rp[t]^12 + 
      2*(207 - 65*la - 39*la^2 - la^3 + 18*En^4*(11 + la) + 3*En^2*(-135 + 11*la + 8*la^2))*M*rp[t]^13 - la*(1 + la)*(-13 - la + 2*En^2*(7 + la))*rp[t]^14))/(En*la*(1 + la)*USq^5*rp[t]^19) - 
   ((16*I)*fp^5*J^3*m*mu*Pi*(525*J^6*M^3 - 3*J^6*(123 - 2*la + 2*m^2)*M^2*rp[t] + J^4*M*(J^2*(60 - 10*la - 7*la^2 + 3*m^2) + 2049*M^2)*rp[t]^2 + 
      2*J^4*(J^2*la*(1 + la) + (-735 + 39*En^2 + 9*la - 6*m^2)*M^2)*rp[t]^3 + J^2*M*(-(J^2*(-243 + 34*la + 25*la^2 + 6*En^2*(3 + la) - 6*m^2)) + 2811*M^2)*rp[t]^4 + 
      J^2*(8*J^2*la*(1 + la) + 3*(-707 + 104*En^2 + 6*la - 2*m^2)*M^2)*rp[t]^5 + M*(-(J^2*(-378 + 38*la + 29*la^2 + 12*En^2*(5 + la) - 3*m^2)) + 1287*M^2)*rp[t]^6 + 
      2*(5*J^2*la*(1 + la) + 3*(-170 + 87*En^2 + la)*M^2)*rp[t]^7 - (-195 + 14*la + 11*la^2 + 6*En^2*(31 + la))*M*rp[t]^8 + 4*la*(1 + la)*rp[t]^9)*Derivative[1][rp][t])/(la*(1 + la)*USq^4*rp[t]^15))
]


Clear[fSourceJT2]
fSourceJT2[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	CapLa=CapitalLambda[rp[t],M,la];
	USq=USquared[];
	
	fp=SchwarzschildF[rp[t],M];

	(-8*fp^4*M*mu*Pi*YBar*(3*J^4*M + J^4*(-2 + la)*rp[t] - 12*J^2*M*rp[t]^2 + J^2*(5 + 6*En^2 + 2*la)*rp[t]^3 - 15*M*rp[t]^4 + (7 - 6*En^2 + la)*rp[t]^5))/(En*USq*rp[t]^8) + 
 (16*fp^4*J*mu*Pi*YPhiBar*(-39*J^4*M^2 + 15*J^4*M*rp[t] + (J^4*la*(1 + la) - 84*J^2*M^2)*rp[t]^2 + 3*(11 - 2*En^2)*J^2*M*rp[t]^3 + (2*J^2*la*(1 + la) - 45*M^2)*rp[t]^4 - 18*(-1 + En^2)*M*rp[t]^5 + 
    la*(1 + la)*rp[t]^6)*Derivative[1][rp][t])/((1 + la)*USq^2*rp[t]^9) + 
 YPhiPhiBar*((8*fp^7*J^2*mu*Pi*(315*J^8*M^3 - 6*J^8*(41 - 6*la + 3*m^2)*M^2*rp[t] + 9*J^6*M*(J^2*(5 - 3*la - la^2 + m^2) + 164*M^2)*rp[t]^2 + 
      2*J^6*(2*J^2*la*(1 + la) + 3*(-194 + 24*En^2 + 24*la - 9*m^2)*M^2)*rp[t]^3 + 3*J^4*M*(J^2*(72 - 38*la - 14*la^2 + 9*m^2 - 2*En^2*(7 - 3*la + 2*m^2)) + 906*M^2)*rp[t]^4 + 
      J^4*(-((-19 + 2*En^2)*J^2*la*(1 + la)) + 18*(-122 + 32*En^2 + 12*la - 3*m^2)*M^2)*rp[t]^5 + 3*J^2*M*(J^2*(3*(47 - 20*la - 8*la^2 + 3*m^2) - 2*En^2*(29 - 9*la + 4*m^2)) + 756*M^2)*rp[t]^6 + 
      J^2*(-((-33 + 10*En^2)*J^2*la*(1 + la)) + 6*(-314 + 180*En^2 + 24*la - 3*m^2)*M^2)*rp[t]^7 + 3*M*(J^2*(3*(42 - 14*la - 6*la^2 + m^2) - 2*En^2*(67 - 9*la + 2*m^2)) + 237*M^2)*rp[t]^8 + 
      (-((-25 + 14*En^2)*J^2*la*(1 + la)) + 6*(-101 + 108*En^2 + 6*la)*M^2)*rp[t]^9 + 3*(42 + 48*En^4 + 6*En^2*(-15 + la) - 11*la - 5*la^2)*M*rp[t]^10 - (-7 + 6*En^2)*la*(1 + la)*rp[t]^11))/
    (En*la*(1 + la)*USq^4*rp[t]^15) - ((16*I)*fp^5*J^3*m*mu*Pi*(-51*J^4*M^2 + 21*J^4*M*rp[t] + (J^4*la*(1 + la) - 138*J^2*M^2)*rp[t]^2 - 12*(-5 + En^2)*J^2*M*rp[t]^3 + 
      (2*J^2*la*(1 + la) - 87*M^2)*rp[t]^4 + (39 - 48*En^2)*M*rp[t]^5 + la*(1 + la)*rp[t]^6)*Derivative[1][rp][t])/(la*(1 + la)*USq^3*rp[t]^11))
]


Clear[gSourceJT3]
gSourceJT3[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];

(* Created with the Wolfram Language : www.wolfram.com *)
(-8*J*mu*Pi*YPhiBar*(-2*M + rp[t])^2*(21168*J^12*M^5 + 
    24*J^12*(-1364 + 21*la)*M^4*rp[t] + 
    4*J^10*M^3*(J^2*(4621 - 293*la - 126*la^2) + 31248*M^2)*rp[t]^2 + 
    4*J^10*M^2*(-(J^2*(1122 - 223*la - 150*la^2 + la^3)) + 
      6*(-8021 + 768*En^2 + 141*la)*M^2)*rp[t]^3 + 
    2*J^8*M*(J^4*(196 - 137*la - 115*la^2 + 2*la^3) - 
      J^2*(-54058 + 3716*la + 1482*la^2 + En^2*(9696 + 63*la))*M^2 + 
      153720*M^4)*rp[t]^4 + J^8*(J^4*la*(29 + 28*la - la^2) - 
      2*J^2*(13033 - 2727*la - 1754*la^2 + 14*la^3 + 
        3*En^2*(-1082 + 26*la + 49*la^2))*M^2 + 
      24*(-19643 + 4101*En^2 + 392*la)*M^4)*rp[t]^5 + 
    J^6*M*(J^4*(2255 - 1626*la - 1333*la^2 + 28*la^3 + 
        2*En^2*(-343 + 84*la + 107*la^2 + 4*la^3)) + 
      2*J^2*(131624 + 1980*En^4 - 9826*la - 3630*la^2 - 
        33*En^2*(1564 + 3*la))*M^2 + 403200*M^4)*rp[t]^6 - 
    J^6*(J^4*la*(1 + la)*(-167 + 7*la + En^2*(34 + 4*la)) + 
      2*J^2*(31487 - 6963*la - 4270*la^2 + 40*la^3 + 6*En^4*(187 + 16*la) + 
        3*En^2*(-5729 + 192*la + 257*la^2))*M^2 - 
      48*(-12821 + 4461*En^2 + 289*la)*M^4)*rp[t]^7 + 
    J^4*M*(J^4*(5389 - 4026*la - 3215*la^2 + 80*la^3 + 
        En^4*(300 + 52*la - 32*la^2) + 2*En^2*(-1802 + 463*la + 555*la^2 + 
          18*la^3)) + 4*J^2*(85378 + 4824*En^4 - 6932*la - 2370*la^2 + 
        3*En^2*(-18682 + 73*la))*M^2 + 297360*M^4)*rp[t]^8 + 
    2*J^4*(J^4*la*(-10*(-20 - 19*la + la^2) + 2*En^4*(2 + 3*la + la^2) - 
        3*En^2*(29 + 32*la + 3*la^2)) + 2*J^2*(-20249 + 18*En^6 + 4751*la + 
        2770*la^2 - 30*la^3 - 6*En^4*(458 + 35*la) - 
        3*En^2*(-6185 + 292*la + 273*la^2))*M^2 + 
      12*(-18818 + 9960*En^2 + 477*la)*M^4)*rp[t]^9 + 
    2*J^2*M*(J^4*(3423 - 2662*la - 2065*la^2 + 60*la^3 - 12*En^6*(1 + la) + 
        En^4*(744 + 124*la - 80*la^2) + En^2*(-3835 + 1087*la + 1174*la^2 + 
          32*la^3)) + 2*J^2*(62237 + 9324*En^4 - 5503*la - 1740*la^2 + 
        En^2*(-62862 + 639*la))*M^2 + 58464*M^4)*rp[t]^10 + 
    2*J^2*(J^4*la*(-15*(-17 - 16*la + la^2) - 8*En^2*(23 + 25*la + 2*la^2) + 
        2*En^4*(9 + 13*la + 4*la^2)) + 2*J^2*(-14621 + 90*En^6 + 3654*la + 
        2020*la^2 - 25*la^3 - 6*En^4*(863 + 47*la) - 
        3*En^2*(-6958 + 430*la + 293*la^2))*M^2 + 
      12*(-7361 + 5718*En^2 + 209*la)*M^4)*rp[t]^11 + 
    2*M*(J^4*(2437 - 1983*la - 1490*la^2 + 50*la^3 - 60*En^6*(1 + la) - 
        4*En^4*(-309 - 35*la + 40*la^2) + En^2*(-4303 + 1323*la + 1262*la^2 + 
          28*la^3)) + J^2*(48338 + 19152*En^4 - 4660*la - 1362*la^2 + 
        3*En^2*(-24380 + 387*la))*M^2 + 9576*M^4)*rp[t]^12 + 
    (J^4*la*(5*(73 + 68*la - 5*la^2) + 4*En^4*(19 + 25*la + 6*la^2) - 
        4*En^2*(100 + 107*la + 7*la^2)) + 2*J^2*(-11237 + 252*En^6 + 
        3003*la + 1570*la^2 - 22*la^3 - 252*En^4*(46 + la) - 
        3*En^2*(-8228 + 606*la + 317*la^2))*M^2 + 
      24*(-1199 + 1347*En^2 + 38*la)*M^4)*rp[t]^13 + 
    M*(J^2*(1843 - 1578*la - 1145*la^2 + 44*la^3 - 24*En^6*(-11 + 7*la) + 
        24*En^4*(130 + la - 12*la^2) + 2*En^2*(-2594 + 817*la + 687*la^2 + 
          12*la^3)) + 6*(2604 + 2724*En^4 - 274*la - 74*la^2 + 
        En^2*(-5864 + 119*la))*M^2)*rp[t]^14 + 
    (J^2*la*(139 + 128*la - 11*la^2 - 6*En^2*(37 + 39*la + 2*la^2) + 
        4*En^4*(19 + 23*la + 4*la^2)) + 2*(-1795 + 1260*En^6 + 515*la + 
        254*la^2 - 4*la^3 - 6*En^4*(935 + 2*la) - 
        3*En^2*(-2035 + 164*la + 69*la^2))*M^2)*rp[t]^15 + 
    (289 - 262*la - 183*la^2 + 8*la^3 - 72*En^6*(11 + la) - 
      12*En^4*(-153 + 5*la + 8*la^2) + 2*En^2*(-667 + 202*la + 151*la^2 + 
        2*la^3))*M*rp[t]^16 + 2*(-1 + En^2)*la*(1 + la)*
     (-11 + la + 2*En^2*(7 + la))*rp[t]^17))/(En^2*(1 + la)*rp[t]^14*
   (J^2 + rp[t]^2)^5) + ((8*I)*J^3*m*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*
   (33516*J^12*M^5 - 12*J^12*(4379 - 128*la + 45*m^2)*M^4*rp[t] + 
    3*J^10*M^3*(J^2*(10019 - 896*la - 208*la^2 + 244*m^2) + 73080*M^2)*
     rp[t]^2 + J^10*M^2*(J^2*(-7377 + 1672*la + 748*la^2 - 12*la^3 + 
        (-327 + 4*la + 4*la^2)*m^2) + 6*(-57446 + 5709*En^2 + 1568*la - 
        486*m^2)*M^2)*rp[t]^3 + 
    2*J^8*M*(-2*J^4*(-3*(54 - 36*la - 24*la^2 + la^3) + 
        (-12 + la + la^2)*m^2) - 3*J^2*(-32955 + 2780*la + 668*la^2 - 
        664*m^2 + 4*En^2*(1511 - 59*la + 36*m^2))*M^2 + 302058*M^4)*rp[t]^4 + 
    J^8*(J^4*la*(1 + la)*(38 - 3*la + m^2) + 
      J^2*(-48681 + 10516*la + 4828*la^2 - 72*la^3 + 
        (-1797 + 20*la + 20*la^2)*m^2 + 6*En^2*(2012 - 257*la - 67*la^2 + 
          122*m^2))*M^2 + 6*(-158956 + 34917*En^2 + 4000*la - 1044*m^2)*M^4)*
     rp[t]^5 + J^6*M*(J^4*(4290 - 2758*la - 1870*la^2 + 72*la^3 + 
        (267 - 20*la - 20*la^2)*m^2 + 2*En^2*(-618 + 250*la + 142*la^2 + 
          (-75 + 2*la + 2*la^2)*m^2)) + 3*J^2*(183225 + 2760*En^4 - 
        14392*la - 3592*la^2 + 2872*m^2 - 24*En^2*(3105 - 107*la + 60*m^2))*
       M^2 + 898704*M^4)*rp[t]^6 + 
    J^6*(-(J^4*la*(1 + la)*(-247 + 18*la - 5*m^2 + 2*En^2*(23 + m^2))) - 
      2*J^2*(68013 - 13822*la - 6532*la^2 + 90*la^3 + 
        (1959 - 20*la - 20*la^2)*m^2 + 12*En^4*(193 - 5*la + 9*m^2) - 
        3*En^2*(12507 - 1459*la - 405*la^2 + 632*m^2))*M^2 + 
      12*(-118982 + 44691*En^2 + 2720*la - 558*m^2)*M^4)*rp[t]^7 + 
    2*J^4*M*(J^4*(6027 - 3691*la - 2551*la^2 + 90*la^3 + 
        (294 - 20*la - 20*la^2)*m^2 + En^4*(288 - 26*la - 26*la^2 + 36*m^2) + 
        En^2*(-3870 + 1483*la + 865*la^2 + 8*(-51 + la + la^2)*m^2)) + 
      6*J^2*(69085 + 3936*En^4 - 4972*la - 1292*la^2 + 772*m^2 - 
        2*En^2*(24133 - 694*la + 324*m^2))*M^2 + 380538*M^4)*rp[t]^8 + 
    J^4*(J^4*la*(1 + la)*(677 - 45*la + 4*En^4*(3 + la) + 10*m^2 - 
        8*En^2*(35 + m^2)) + 2*J^2*(72*En^6 + 
        3*(-34499 + 6476*la + 3156*la^2 - 40*la^3) + 
        (-2121 + 20*la + 20*la^2)*m^2 - 12*En^4*(1127 - 22*la + 45*m^2) + 
        En^2*(98211 - 9990*la - 3042*la^2 + 3492*m^2))*M^2 + 
      12*(-101623 + 61107*En^2 + 2080*la - 297*m^2)*M^4)*rp[t]^9 + 
    J^2*M*(-2*J^4*(-9273 + 5294*la + 3734*la^2 - 120*la^3 + 
        24*En^6*(1 + la) + (-321 + 20*la + 20*la^2)*m^2 - 
        12*En^4*(143 - 14*la - 13*la^2 + 18*m^2) - 
        6*En^2*(-1691 + 604*la + 372*la^2 + (-129 + 2*la + 2*la^2)*m^2)) + 
      3*J^2*(238485 + 36576*En^4 - 15472*la - 4192*la^2 + 1652*m^2 - 
        8*En^2*(34049 - 746*la + 252*m^2))*M^2 + 347400*M^4)*rp[t]^10 + 
    J^2*(2*J^4*la*(1 + la)*(499 - 30*la + 8*En^4*(4 + la) + 5*m^2 - 
        6*En^2*(63 + m^2)) + J^2*(-180981 + 864*En^6 + 30784*la + 
        15484*la^2 - 180*la^3 + (-2283 + 20*la + 20*la^2)*m^2 - 
        72*En^4*(836 - 12*la + 21*m^2) + 6*En^2*(48149 - 3806*la - 
          1282*la^2 + 920*m^2))*M^2 + 6*(-93742 + 87405*En^2 + 1696*la - 
        126*m^2)*M^4)*rp[t]^11 + 
    2*M*(J^4*(8247 - 4286*la - 3086*la^2 + 90*la^3 - 144*En^6*(1 + la) - 
        2*(-87 + 5*la + 5*la^2)*m^2 + 12*En^4*(243 - 37*la - 34*la^2 + 
          27*m^2) + 2*En^2*(-7914 + 2237*la + 1463*la^2 + 
          4*(-78 + la + la^2)*m^2)) + 3*J^2*(55707 + 27120*En^4 - 3212*la - 
        908*la^2 + 176*m^2 - 12*En^2*(8468 - 133*la + 24*m^2))*M^2 + 
      33318*M^4)*rp[t]^12 + 
    (J^4*la*(1 + la)*(832 - 45*la + 24*En^4*(9 + la) + 5*m^2 - 
        8*En^2*(131 + m^2)) + J^2*(-85941 - 144*En^6 + 13028*la + 6764*la^2 - 
        72*la^3 + (-489 + 4*la + 4*la^2)*m^2 - 
        24*En^4*(4567 - 26*la + 27*m^2) + 6*En^2*(38055 - 2173*la - 
          807*la^2 + 266*m^2))*M^2 + 18*(-6064 + 8455*En^2 + 96*la)*M^4)*
     rp[t]^13 + M*(J^2*(8004 - 3710*la - 2726*la^2 + 72*la^3 - 
        144*En^6*(-17 + 3*la) + (75 - 4*la - 4*la^2)*m^2 + 
        8*En^4*(2079 - 122*la - 113*la^2 + 36*m^2) + 
        En^2*(4*(-6804 + 1379*la + 953*la^2) + (-366 + 4*la + 4*la^2)*m^2)) + 
      3*(21935 + 30648*En^4 - 1112*la - 328*la^2 + 136*En^2*(-452 + 5*la))*
       M^2)*rp[t]^14 + (J^2*la*(1 + la)*(371 - 18*la + 16*En^4*(18 + la) + 
        m^2 - 2*En^2*(359 + m^2)) + 2*(-8616 + 8208*En^6 + 1150*la + 
        616*la^2 - 6*la^3 + 12*En^4*(-2993 + 7*la) - 
        3*En^2*(-12148 + 495*la + 201*la^2))*M^2)*rp[t]^15 - 
    2*(-822 + 335*la + 251*la^2 - 6*la^3 + 96*En^6*(31 + la) + 
      En^2*(4674 - 675*la - 489*la^2) + 6*En^4*(-1138 + 31*la + 29*la^2))*M*
     rp[t]^16 + la*(69 + 66*la - 3*la^2 - 192*En^2*(1 + la) + 
      4*En^4*(31 + 32*la + la^2))*rp[t]^17))/(En^2*la*(1 + la)*rp[t]^14*
   (J^2 + rp[t]^2)^6) + 
 ((8*mu*Pi*YBar*(2*M - rp[t])*(1668*J^10*M^4 - 12*J^10*(175 + 2*la)*M^3*
       rp[t] + 2*J^8*M^2*(J^2*(428 + 12*la + la^2) + 2640*M^2)*rp[t]^2 + 
      J^8*M*(J^2*(-112 - 3*la + la^2) + 6*(-1150 + 349*En^2 - 21*la)*M^2)*
       rp[t]^3 - J^6*(J^4*la*(1 + la) + J^2*(-2911 - 171*la - 56*la^2 + 
          6*En^2*(245 + 3*la))*M^2 - 4296*M^4)*rp[t]^4 + 
      J^6*M*(-(J^2*(393 + 53*la + 32*la^2 + 2*En^2*(-119 + 5*la^2))) + 
        12*(-537 + 614*En^2 - 23*la)*M^2)*rp[t]^5 + 
      2*J^4*((1 + En^2)*J^4*la*(1 + la) + 6*J^2*(253 + 18*En^4 + 38*la + 
          17*la^2 - 2*En^2*(224 + la))*M^2 - 1080*M^4)*rp[t]^6 + 
      J^4*M*(-(J^2*(449 + 185*la + 138*la^2 + 12*En^4*(4 + la) + 
           2*En^2*(-449 + 6*la + 14*la^2))) + 12*(95 + 693*En^2 - 26*la)*M^2)*
       rp[t]^7 + 2*J^2*((9 + 4*En^2)*J^4*la*(1 + la) + 
        J^2*(83 + 432*En^4 + 6*En^2*(-555 + la) + 291*la + 148*la^2)*M^2 - 
        2214*M^4)*rp[t]^8 + 
      J^2*M*(-(J^2*(103 + 267*la + 212*la^2 + 12*En^4*(13 + la) + 
           8*En^2*(-156 + la + la^2))) + 12*(376 + 138*En^2 - 15*la)*M^2)*
       rp[t]^9 + 2*(2*(8 + En^2)*J^4*la*(1 + la) + 
        J^2*(-706 + 756*En^4 + 12*En^2*(-76 + la) + 180*la + 97*la^2)*M^2 - 
        792*M^4)*rp[t]^10 + M*(J^2*(129 + 12*En^4*(-50 + la) - 176*la - 
          143*la^2 + En^2*(442 + 20*la + 28*la^2)) - 
        6*(-288 + 231*En^2 + 7*la)*M^2)*rp[t]^11 + 
      (-((-23 + 8*En^2)*J^2*la*(1 + la)) + (-597 - 288*En^4 + 87*la + 
          48*la^2 + 6*En^2*(155 + la))*M^2)*rp[t]^12 + 
      2*(32 - 22*la - 18*la^2 + 6*En^4*(7 + la) + En^2*(-73 + 8*la + 9*la^2))*
       M*rp[t]^13 - 6*(-1 + En^2)*la*(1 + la)*rp[t]^14))/
    (En*rp[t]^11*(J^2 + rp[t]^2)^4) - (8*J^2*mu*Pi*YPhiPhiBar*(2*M - rp[t])*
     (48510*J^14*M^5 - 3*J^14*(22835 - 2416*la + 1502*m^2)*M^4*rp[t] + 
      3*J^12*M^3*(J^2*(-4*(-2891 + 811*la + 83*la^2) + (1823 - 16*la)*m^2 + 
          4*m^4) + 118914*M^2)*rp[t]^2 + 
      J^12*M^2*(2*J^2*(-3681 + 2246*la + 473*la^2 - 63*la^3 + 
          (-1077 + 55*la + 31*la^2)*m^2 - 6*m^4) + 
        3*(-167911 + 11040*En^2 + 17168*la - 9868*m^2)*M^2)*rp[t]^3 + 
      J^10*M*(J^4*(540 - 800*la - 263*la^2 + 105*la^3 + 
          (273 - 65*la - 53*la^2)*m^2 + 3*m^4) - 
        6*J^2*(4*(-10625 + 2892*la + 304*la^2) + (-6029 + 48*la)*m^2 - 
          10*m^4 + En^2*(4940 - 769*la + 728*m^2))*M^2 + 1134126*M^4)*
       rp[t]^4 + J^10*(J^4*la*(1 + la)*(40 - 21*la + 11*m^2) + 
        J^2*(-54069 + 32176*la + 6970*la^2 - 882*la^3 + 
          2*(-7179 + 340*la + 196*la^2)*m^2 - 60*m^4 + 
          6*En^2*(1337 - 582*la - 70*la^2 + (514 - 4*la)*m^2 + 4*m^4))*M^2 + 
        3*(-534071 + 76704*En^2 + 52336*la - 27122*m^2)*M^4)*rp[t]^5 + 
      J^8*M*(J^4*(3960 - 5765*la - 1958*la^2 + 735*la^3 - 
          2*(-918 + 205*la + 169*la^2)*m^2 + 15*m^4 + 
          En^2*(-630 + 694*la + 202*la^2 - 30*la^3 + 
            4*(-126 + 11*la + 8*la^2)*m^2 - 12*m^4)) + 
        3*J^2*(270416 + 1320*En^4 - 70860*la - 7692*la^2 + 
          (33413 - 240*la)*m^2 + 40*m^4 - 2*En^2*(34517 - 5218*la + 
            4712*m^2))*M^2 + 2025846*M^4)*rp[t]^6 + 
      J^8*(J^4*la*(1 + la)*(292 - 147*la + 71*m^2 - 
          2*En^2*(13 - 3*la + 5*m^2)) + J^2*(-171969 + 99128*la + 
          22214*la^2 - 2646*la^3 + 2*(-20091 + 875*la + 515*la^2)*m^2 - 
          120*m^4 - 108*En^4*(15 - 2*la + 4*m^2) + 
          12*En^2*(4697 - 2003*la - 238*la^2 + (1703 - 10*la)*m^2 + 8*m^4))*
         M^2 + 3*(-955547 + 227664*En^2 + 88720*la - 39848*m^2)*M^4)*
       rp[t]^7 + J^6*M*(J^4*(12582 - 17912*la - 6323*la^2 + 2205*la^3 + 
          (5202 - 1075*la - 895*la^2)*m^2 + 30*m^4 + 
          24*En^4*(-2*(-3 + la + la^2) + (4 + la)*m^2) - 
          2*En^2*(2223 - 2414*la - 698*la^2 + 96*la^3 - 
            10*(-171 + 13*la + 10*la^2)*m^2 + 24*m^4)) + 
        6*J^2*(242260 + 4476*En^4 - 60416*la - 6816*la^2 + 
          (24766 - 160*la)*m^2 + 20*m^4 - 3*En^2*(34347 - 4949*la + 
            4168*m^2))*M^2 + 2203434*M^4)*rp[t]^8 + 
      J^6*(J^4*la*(1 + la)*(923 - 441*la + 4*En^4*(1 + la) + 190*m^2 - 
          14*En^2*(13 - 3*la + 5*m^2)) - 6*J^2*(51401 - 28394*la - 
          6629*la^2 + 735*la^3 - 2*(-5019 + 200*la + 120*la^2)*m^2 + 20*m^4 + 
          6*En^4*(307 - 50*la + 84*m^2) + En^2*(-28221 + 11664*la + 
            1402*la^2 + 20*(-471 + 2*la)*m^2 - 24*m^4))*M^2 + 
        3*(-1043353 + 375648*En^2 + 90320*la - 32962*m^2)*M^4)*rp[t]^9 + 
      J^4*M*(J^4*(7*(3222 - 4451*la - 1646*la^2 + 525*la^3) - 
          12*(-659 + 125*la + 105*la^2)*m^2 + 30*m^4 + 
          24*En^4*(39 - 14*la - 11*la^2 + (25 + 4*la)*m^2) - 
          6*En^2*(2253 - 2400*la - 697*la^2 + 85*la^3 - 
            2*(-837 + 50*la + 40*la^2)*m^2 + 12*m^4)) + 
        3*J^2*(25056*En^4 - 4*(-132835 + 30977*la + 3657*la^2) + 
          (41353 - 240*la)*m^2 + 20*m^4 - 4*En^2*(85291 - 11294*la + 
            8260*m^2))*M^2 + 1464066*M^4)*rp[t]^10 + 
      J^4*(J^4*la*(1 + la)*(28*En^4*(1 + la) - 12*En^2*(44 - 10*la + 
            15*m^2) + 3*(549 - 245*la + 90*m^2)) - 
        2*J^2*(169902 - 88238*la - 21623*la^2 + 2205*la^3 + 
          (25431 - 925*la - 565*la^2)*m^2 + 30*m^4 + 
          36*En^4*(473 - 84*la + 142*m^2) - 12*En^2*(11565 - 4583*la - 
            586*la^2 + (3241 - 10*la)*m^2 + 4*m^4))*M^2 + 
        3*(-698357 + 379680*En^2 + 55216*la - 14540*m^2)*M^4)*rp[t]^11 + 
      J^2*M*(J^4*(3*(8322 - 10936*la - 4271*la^2 + 1225*la^3) + 
          (6777 - 1175*la - 995*la^2)*m^2 + 15*m^4 + 
          72*En^4*(62 - 19*la - 8*la^2 + (43 + 2*la)*m^2) + 
          En^2*(-20970 + 23776*la + 7384*la^2 - 720*la^3 + 
            4*(-3663 + 170*la + 140*la^2)*m^2 - 48*m^4)) + 
        6*J^2*(15312*En^4 - 4*(-44863 + 9552*la + 1188*la^2) + 
          (9205 - 48*la)*m^2 + 2*m^4 + En^2*(-177530 + 19327*la - 10736*m^2))*
         M^2 + 551466*M^4)*rp[t]^12 + 
      J^2*(J^4*la*(1 + la)*(1798 - 735*la + 72*En^4*la + 215*m^2 - 
          4*En^2*(239 - 45*la + 55*m^2)) + J^2*(-232233 + 2304*En^6 + 
          110216*la + 28550*la^2 - 2646*la^3 + (-22902 + 760*la + 472*la^2)*
           m^2 - 12*m^4 - 72*En^4*(263 - 132*la + 182*m^2) + 
          6*En^2*(49819 - 16342*la - 2330*la^2 + (8722 - 20*la)*m^2 + 4*m^4))*
         M^2 + 9*(-88703 + 75264*En^2 + 6256*la - 890*m^2)*M^4)*rp[t]^13 + 
      M*(-(J^4*(-17334 + 20927*la + 8690*la^2 - 2205*la^3 + 
           288*En^6*(11 + la) + (-3096 + 490*la + 418*la^2)*m^2 - 3*m^4 - 
           24*En^4*(-3*(60 + 41*la + 14*la^2) + (199 + 4*la)*m^2) + 
           2*En^2*(11646 - 11399*la - 3995*la^2 + 285*la^3 - 
             2*(-2583 + 95*la + 80*la^2)*m^2 + 6*m^4))) + 
        3*J^2*(44088*En^4 - 4*(-34710 + 6561*la + 865*la^2) + 
          (3411 - 16*la)*m^2 - 6*En^2*(37511 - 2934*la + 912*m^2))*M^2 + 
        90882*M^4)*rp[t]^14 + 
      (J^4*la*(1 + la)*(1202 - 441*la + 8*En^4*(8 + 11*la) + 91*m^2 - 
          10*En^2*(113 - 15*la + 13*m^2)) - J^2*(91713 + 18432*En^6 - 
          38432*la - 10586*la^2 + 882*la^3 - 2*(-2145 + 65*la + 41*la^2)*
           m^2 + 36*En^4*(1617 - 194*la + 152*m^2) + 
          12*En^2*(-17397 + 3903*la + 638*la^2 + (-1143 + 2*la)*m^2))*M^2 + 
        9*(-14851 + 19920*En^2 + 912*la)*M^4)*rp[t]^15 + 
      M*(J^2*(7056 - 7488*la - 3321*la^2 + 735*la^3 - 288*En^6*(-33 + 2*la) + 
          (588 - 85*la - 73*la^2)*m^2 + 24*En^4*(108 - 121*la - 46*la^2 + 
            (91 + la)*m^2) + 12*En^2*(-1608 + 991*la + 399*la^2 - 20*la^3 + 
            (-234 + 7*la + 6*la^2)*m^2)) + 
        6*(15444*En^4 + 9*En^2*(-3569 + 185*la) - 
          4*(-2967 + 484*la + 68*la^2))*M^2)*rp[t]^16 + 
      (J^2*la*(1 + la)*(455 - 147*la + 4*En^4*(55 + 13*la) + 16*m^2 - 
          6*En^2*(125 - 11*la + 5*m^2)) + 6*(-2690 + 2304*En^6 + 962*la + 
          283*la^2 - 21*la^3 + 6*En^4*(-1775 + 54*la) + 
          En^2*(11071 - 1556*la - 294*la^2))*M^2)*rp[t]^17 - 
      (-1296 + 1159*la + 550*la^2 - 105*la^3 + 288*En^6*(16 + la) + 
        24*En^4*(-441 + 43*la + 19*la^2) + 6*En^2*(1212 - 434*la - 199*la^2 + 
          7*la^3))*M*rp[t]^18 + 3*la*(25 + 18*la - 7*la^2 + 
        4*En^2*(-17 - 16*la + la^2) + 4*En^4*(11 + 12*la + la^2))*rp[t]^19))/
    (En*la*(1 + la)*rp[t]^12*(J^2 + rp[t]^2)^7))*Derivative[1][rp][t]

]


Clear[fSourceJT3]
fSourceJT3[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	
(* Created with the Wolfram Language : www.wolfram.com *)
(-8*J*mu*Pi*YPhiBar*(2*M - rp[t])^3*(1008*J^10*M^4 + 
    6*J^10*(-207 + 5*la)*M^3*rp[t] + J^8*M^2*(J^2*(497 - 61*la - 30*la^2) + 
      5040*M^2)*rp[t]^2 + J^8*M*(J^2*(-64 + 35*la + 27*la^2) + 
      6*(-1033 + 279*En^2 + 27*la)*M^2)*rp[t]^3 - 
    J^6*(6*J^4*la*(1 + la) + J^2*(-2473 + En^2*(1260 - 24*la) + 317*la + 
        150*la^2)*M^2 - 10080*M^4)*rp[t]^4 + 
    J^6*M*(J^2*(-317 + 178*la + 135*la^2 - 6*En^2*(-37 + 7*la + 5*la^2)) + 
      12*(-1031 + 612*En^2 + 29*la)*M^2)*rp[t]^5 + 
    2*(3*(-5 + 2*En^2)*J^8*la*(1 + la) + J^6*(2461 + 252*En^4 - 329*la - 
        150*la^2 + 6*En^2*(-461 + 12*la))*M^2 + 5040*J^4*M^4)*rp[t]^6 + 
    2*J^4*M*(J^2*(-314 - 72*En^4 + 181*la + 135*la^2 - 
        6*En^2*(-81 + 17*la + 11*la^2)) + 6*(-1029 + 1029*En^2 + 31*la)*M^2)*
     rp[t]^7 + 2*(-((30 - 27*En^2 + 2*En^4)*J^6*la*(1 + la)) + 
      J^4*(2449 + 972*En^4 - 341*la - 150*la^2 + 18*En^2*(-261 + 8*la))*M^2 + 
      2520*J^2*M^4)*rp[t]^8 + 
    2*J^2*M*(J^2*(-311 + 12*En^4*(-23 + la) + 184*la + 135*la^2 - 
        9*En^2*(-93 + 20*la + 12*la^2)) + (-3081 + 4680*En^2 + 99*la)*M^2)*
     rp[t]^9 + (-10*(6 - 9*En^2 + 2*En^4)*J^4*la*(1 + la) + 
      J^2*(2437 + 3096*En^4 - 353*la - 150*la^2 + 12*En^2*(-603 + 20*la))*
       M^2 + 1008*M^4)*rp[t]^10 + 
    M*(J^2*(-308 + 187*la + 135*la^2 + 24*En^4*(-43 + 2*la) - 
        12*En^2*(-110 + 23*la + 13*la^2)) + 6*(-205 + 447*En^2 + 7*la)*M^2)*
     rp[t]^11 + (-2*(15 - 33*En^2 + 14*En^4)*J^2*la*(1 + la) + 
      (485 + 1656*En^4 - 73*la - 30*la^2 + 24*En^2*(-88 + 3*la))*M^2)*
     rp[t]^12 + (-61 + 288*En^6 + 24*En^4*(-26 + la) + 38*la + 27*la^2 - 
      6*En^2*(-66 + 13*la + 7*la^2))*M*rp[t]^13 - 6*(1 - 3*En^2 + 2*En^4)*la*
     (1 + la)*rp[t]^14))/(En^2*(1 + la)*rp[t]^13*(J^2 + rp[t]^2)^4) - 
 ((8*I)*J^3*m*mu*Pi*YPhiPhiBar*(2*M - rp[t])^3*
   (-1386*J^10*M^4 + 3*J^10*(579 - 16*la + 4*m^2)*M^3*rp[t] - 
    6*J^8*M^2*(J^2*(118 - 13*la - 5*la^2 + 2*m^2) + 1287*M^2)*rp[t]^2 - 
    3*J^8*M*(-(J^2*(31 - 13*la - 9*la^2 + m^2)) + 
      (-3247 + 966*En^2 + 80*la - 16*m^2)*M^2)*rp[t]^3 + 
    6*J^6*(J^4*la*(1 + la) + J^2*(-667 + 67*la + 27*la^2 - 8*m^2 + 
        4*En^2*(93 - 5*la + 2*m^2))*M^2 - 2898*M^4)*rp[t]^4 - 
    3*J^6*M*(J^2*(-177 + 69*la + 49*la^2 - 4*m^2 + 
        2*En^2*(67 - 17*la - 7*la^2 + 4*m^2)) + 
      2*(-3689 + 2460*En^2 + 80*la - 12*m^2)*M^2)*rp[t]^5 - 
    3*((-11 + 6*En^2)*J^8*la*(1 + la) + 2*J^6*(1533 + 168*En^4 - 138*la - 
        58*la^2 + 12*m^2 - 8*En^2*(241 - 10*la + 3*m^2))*M^2 + 6564*J^4*M^4)*
     rp[t]^6 - 3*J^4*M*(J^2*(-413 + 146*la + 106*la^2 - 6*m^2 - 
        8*En^4*(12 - 2*la + m^2) + 8*En^2*(89 - 19*la - 9*la^2 + 3*m^2)) + 
      2*(-4221 + 5022*En^2 + 80*la - 8*m^2)*M^2)*rp[t]^7 - 
    2*(-4*(9 - 12*En^2 + En^4)*J^6*la*(1 + la) + 
      3*J^4*(1777 + 768*En^4 - 142*la - 62*la^2 + 8*m^2 - 
        12*En^2*(341 - 10*la + 2*m^2))*M^2 + 5589*J^2*M^4)*rp[t]^8 - 
    3*J^2*M*(J^2*(-487 + 154*la + 114*la^2 - 4*m^2 - 
        16*En^4*(29 - 3*la + m^2) + 6*En^2*(267 - 42*la - 22*la^2 + 4*m^2)) + 
      (-4843 + 9192*En^2 + 80*la - 4*m^2)*M^2)*rp[t]^9 - 
    6*(-((13 - 30*En^2 + 8*En^4)*J^4*la*(1 + la)) + 
      J^2*(1033 + 1752*En^4 - 73*la - 33*la^2 + 2*m^2 - 
        8*En^2*(486 - 10*la + m^2))*M^2 + 423*M^4)*rp[t]^10 - 
    3*M*(-(J^2*(288 - 81*la - 61*la^2 + m^2 + 8*En^4*(170 - 6*la + m^2) - 
         4*En^2*(401 - 46*la - 26*la^2 + 2*m^2))) + 
      (-1111 + 3102*En^2 + 16*la)*M^2)*rp[t]^11 - 
    6*(-((7 - 24*En^2 + 12*En^4)*J^2*la*(1 + la)) + 
      (240 + 1152*En^4 - 15*la - 7*la^2 + 4*En^2*(-338 + 5*la))*M^2)*
     rp[t]^12 - 3*(-68 + 480*En^6 + 16*En^4*(-62 + la) + 17*la + 13*la^2 - 
      10*En^2*(-58 + 5*la + 3*la^2))*M*rp[t]^13 + (9 - 42*En^2 + 32*En^4)*la*
     (1 + la)*rp[t]^14))/(En^2*la*(1 + la)*rp[t]^13*(J^2 + rp[t]^2)^5) + 
 ((-8*mu*Pi*YBar*(-2*M + rp[t])^2*(-33*J^8*M^3 + 3*J^8*(11 + la)*M^2*rp[t] - 
      2*J^6*M*(J^2*(4 + 2*la + la^2) + 6*M^2)*rp[t]^2 + 
      J^6*(J^2*la*(1 + la) - 6*(-5 + 32*En^2 - 2*la)*M^2)*rp[t]^3 + 
      J^4*M*(J^2*(-11 + 78*En^2 - 18*la - 10*la^2) + 174*M^2)*rp[t]^4 + 
      J^4*((5 + 2*En^2)*J^2*la*(1 + la) - 6*(20 + 68*En^2 - 3*la)*M^2)*
       rp[t]^5 + 6*J^2*M*(J^2*(3 + 31*En^2 - 6*En^4 - 5*la - 3*la^2) + 
        42*M^2)*rp[t]^6 + J^2*((9 + 2*En^2)*J^2*la*(1 + la) - 
        6*(33 + 12*En^2 - 2*la)*M^2)*rp[t]^7 + 
      M*(J^2*(37 + 54*En^2 - 144*En^4 - 22*la - 14*la^2) + 99*M^2)*rp[t]^8 + 
      (-((-7 + 2*En^2)*J^2*la*(1 + la)) + 3*(-27 + 48*En^2 + la)*M^2)*
       rp[t]^9 + 2*(8 - 27*En^2 + 18*En^4 - 3*la - 2*la^2)*M*rp[t]^10 - 
      2*(-1 + En^2)*la*(1 + la)*rp[t]^11))/(En*rp[t]^10*(J^2 + rp[t]^2)^3) - 
   (8*J^2*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*(2205*J^12*M^4 - 
      3*J^12*(811 - 104*la + 52*m^2)*M^3*rp[t] + 
      3*J^10*M^2*(J^2*(282 - 111*la - 19*la^2 + 46*m^2) + 4728*M^2)*rp[t]^2 + 
      J^10*M*(2*J^2*(-45 + 52*la + 19*la^2 - 3*la^3 + (-15 + la + la^2)*
           m^2) + 3*(-5231 + 1110*En^2 + 640*la - 284*m^2)*M^2)*rp[t]^3 - 
      J^8*(J^4*la*(1 + la)*(8 - 3*la + m^2) + 
        3*J^2*(-1825 + 690*la + 122*la^2 - 254*m^2 + 
          2*En^2*(343 - 86*la + 64*m^2))*M^2 - 38457*M^4)*rp[t]^4 + 
      J^8*M*(J^2*(-585 + 656*la + 248*la^2 - 36*la^3 + 
          2*(-84 + 5*la + 5*la^2)*m^2 + 6*En^2*(45 - 42*la - 8*la^2 + 
            26*m^2)) + 6*(-7121 + 3273*En^2 + 820*la - 308*m^2)*M^2)*
       rp[t]^5 + J^6*(J^4*la*(1 + la)*(-52 + 18*la - 5*m^2 + 
          2*En^2*(7 - 3*la + 2*m^2)) + 3*J^2*(4994 + 180*En^4 - 1789*la - 
          329*la^2 + 556*m^2 + En^2*(-4094 + 964*la - 656*m^2))*M^2 + 
        56448*M^4)*rp[t]^6 + J^6*M*(J^2*(-1611 + 1732*la + 682*la^2 - 
          90*la^3 + 4*(-93 + 5*la + 5*la^2)*m^2 - 72*En^4*(1 - la + m^2) + 
          6*En^2*(273 - 244*la - 48*la^2 + 140*m^2)) + 
        6*(-10529 + 7974*En^2 + 1120*la - 332*m^2)*M^2)*rp[t]^7 + 
      (J^8*la*(1 + la)*(-143 + 45*la - 10*m^2 + 2*En^2*(43 - 15*la + 
            8*m^2)) + 12*J^6*(1864 + 270*En^4 - 619*la - 119*la^2 + 151*m^2 - 
          6*En^2*(419 - 89*la + 50*m^2))*M^2 + 47367*J^4*M^4)*rp[t]^8 + 
      J^4*M*(J^2*(-3*(813 - 816*la - 336*la^2 + 40*la^3) + 
          4*(-102 + 5*la + 5*la^2)*m^2 - 504*En^4*(1 - la + m^2) + 
          6*En^2*(669 - 568*la - 124*la^2 + 264*m^2)) + 
        3*(-17867 + 21108*En^2 + 1720*la - 356*m^2)*M^2)*rp[t]^9 + 
      (2*J^6*la*(1 + la)*(-106 + 30*la - 5*m^2 + 
          6*En^2*(20 - 5*la + 2*m^2)) + 3*J^4*(6424 + 2016*En^4 - 1929*la - 
          389*la^2 + 326*m^2 - 8*En^2*(1738 - 293*la + 118*m^2))*M^2 + 
        21528*J^2*M^4)*rp[t]^10 + 
      J^2*M*(J^2*(-2151 + 1952*la + 842*la^2 - 90*la^3 + 
          2*(-111 + 5*la + 5*la^2)*m^2 - 72*En^4*(3 - 15*la + 11*m^2) + 
          6*En^2*(999 - 660*la - 164*la^2 + 212*m^2)) + 
        3*(-8239 + 15006*En^2 + 704*la - 76*m^2)*M^2)*rp[t]^11 + 
      (-(J^4*la*(1 + la)*(178 + 48*En^4 - 45*la + 5*m^2 - 
           4*En^2*(86 - 15*la + 4*m^2))) + 3*J^2*(3023 + 3912*En^4 - 802*la - 
          170*la^2 + 70*m^2 - 2*En^2*(5293 - 638*la + 136*m^2))*M^2 + 
        4131*M^4)*rp[t]^12 + 2*M*(J^2*(-522 - 720*En^6 + 416*la + 188*la^2 - 
          18*la^3 + (-24 + la + la^2)*m^2 - 36*En^4*(39 - 13*la + 5*m^2) + 
          6*En^2*(429 - 191*la - 54*la^2 + 31*m^2)) + 
        9*(-268 + 735*En^2 + 20*la)*M^2)*rp[t]^13 + 
      (-(J^2*la*(1 + la)*(80 + 96*En^4 - 18*la + m^2 + 
           En^2*(-242 + 30*la - 4*m^2))) + 3*(604 + 2796*En^4 - 139*la - 
          31*la^2 + 6*En^2*(-555 + 46*la))*M^2)*rp[t]^14 + 
      2*(-108 + 720*En^6 + 74*la + 35*la^2 - 3*la^3 + 72*En^4*(-21 + 2*la) - 
        12*En^2*(-75 + 22*la + 7*la^2))*M*rp[t]^15 - 
      3*la*(5 + 4*la - la^2 + 16*En^4*(1 + la) + 2*En^2*(-11 - 10*la + la^2))*
       rp[t]^16))/(En*la*(1 + la)*rp[t]^11*(J^2 + rp[t]^2)^6))*
  Derivative[1][rp][t]
]


Clear[gSourceJT4]
gSourceJT4[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];

(* Created with the Wolfram Language : www.wolfram.com *)
(8*mu*Pi*YBar*(-2*M + rp[t])^2*(136458*J^14*M^5 + 6*J^14*(-35523 + 217*la)*
     M^4*rp[t] + 2*J^12*M^3*(J^2*(60695 - 1612*la - 747*la^2) + 422001*M^2)*
     rp[t]^2 + J^12*M^2*(J^2*(-29652 + 2571*la + 1795*la^2 - 20*la^3) + 
      6*(-220947 + 17757*En^2 + 1568*la)*M^2)*rp[t]^3 + 
    J^10*M*(2*J^4*(1296 - 411*la - 347*la^2 + 10*la^3) - 
      3*J^2*(-253155 + 7203*la + 3020*la^2 + 2*En^2*(18801 + 7*la))*M^2 + 
      2157570*M^4)*rp[t]^4 + J^10*(-5*J^4*la*(-18 - 17*la + la^2) - 
      2*J^2*(93323 - 8316*la - 5487*la^2 + 74*la^3 + 
        En^2*(-18816 + 343*la + 403*la^2))*M^2 + 
      6*(-569591 + 101796*En^2 + 4647*la)*M^4)*rp[t]^5 + 
    2*J^8*M*(2*J^4*(4104 - 1304*la - 1069*la^2 + 37*la^3 + 
        En^2*(-969 + 131*la + 143*la^2)) + 
      3*J^2*(329229 + 3276*En^4 + 174*En^2*(-627 + la) - 9991*la - 3743*la^2)*
       M^2 + 1440333*M^4)*rp[t]^6 - 
    J^8*(J^4*la*(1 + la)*(-564 + 94*En^2 + 37*la) + 
      J^2*(490032 - 44627*la - 27545*la^2 + 456*la^3 + 
        36*En^4*(305 + 12*la) + 6*En^2*(-36871 + 818*la + 762*la^2))*M^2 - 
      6*(-770863 + 238389*En^2 + 7178*la)*M^4)*rp[t]^7 + 
    J^6*M*(J^4*(43515 - 13751*la - 10874*la^2 + 456*la^3 + 
        En^4*(1356 + 132*la - 72*la^2) - 4*En^2*(5772 - 805*la - 820*la^2 + 
          6*la^3)) + J^2*(2712593 + 108072*En^4 - 87007*la - 28440*la^2 + 
        6*En^2*(-260031 + 1099*la))*M^2 + 2039454*M^4)*rp[t]^8 + 
    2*J^6*(J^4*la*(736 + 679*la - 57*la^2 + 2*En^4*(4 + 5*la + la^2) + 
        En^2*(-274 - 268*la + 6*la^2)) + J^2*(-341805 + 108*En^6 + 31343*la + 
        17790*la^2 - 380*la^3 - 18*En^4*(1733 + 58*la) - 
        3*En^2*(-89358 + 2515*la + 1751*la^2))*M^2 + 
      3*(-559337 + 286200*En^2 + 6007*la)*M^4)*rp[t]^9 + 
    2*J^4*M*(J^4*(30879 - 9503*la - 7170*la^2 + 380*la^3 - 36*En^6*(1 + la) - 
        108*En^4*(-37 - 3*la + 2*la^2) + En^2*(-28269 + 4303*la + 3914*la^2 - 
          56*la^3)) + 3*J^2*(336667 + 39384*En^4 - 11318*la - 3015*la^2 + 
        4*En^2*(-81041 + 547*la))*M^2 + 307827*M^4)*rp[t]^10 + 
    J^4*(2*J^4*la*(1010 + 915*la - 95*la^2 + 6*En^4*(7 + 8*la + la^2) + 
        En^2*(-698 - 670*la + 28*la^2)) + J^2*(-523460 + 1296*En^6 + 
        47129*la + 23525*la^2 - 740*la^3 - 48*En^4*(2786 + 53*la) - 
        8*En^2*(-87423 + 2779*la + 1453*la^2))*M^2 + 
      6*(-180521 + 165951*En^2 + 2460*la)*M^4)*rp[t]^11 - 
    J^2*M*(2*J^4*(-24381 + 7032*la + 4925*la^2 - 370*la^3 + 
        216*En^6*(1 + la) + 12*En^4*(-586 - 13*la + 48*la^2) + 
        2*En^2*(19524 - 2853*la - 2288*la^2 + 52*la^3)) - 
      3*J^2*(231801 + 103824*En^4 - 8237*la - 1228*la^2 + 
        6*En^2*(-66797 + 605*la))*M^2 + 43434*M^4)*rp[t]^12 + 
    J^2*(J^4*la*(5*(298 + 261*la - 37*la^2) + 8*En^4*(40 + 41*la + la^2) + 
        8*En^2*(-223 - 210*la + 13*la^2)) - 
      2*J^2*(96021 + 576*En^6 - 8026*la - 2815*la^2 + 210*la^3 + 
        72*En^4*(1499 + 2*la) + 3*En^2*(-77734 + 2491*la + 911*la^2))*M^2 + 
      6*(4451 + 24420*En^2 + 265*la)*M^4)*rp[t]^13 - 
    2*M*(2*J^4*(-4770 + 1172*la + 671*la^2 - 105*la^3 + 
        36*En^6*(-33 + 4*la) + 12*En^4*(-704 + 17*la + 25*la^2) + 
        En^2*(14340 - 1735*la - 1177*la^2 + 48*la^3)) - 
      J^2*(5681 + 71028*En^4 - 541*la + 765*la^2 + 30*En^2*(-3573 + 55*la))*
       M^2 + 25137*M^4)*rp[t]^14 - 
    (J^4*la*(En^2*(1046 + 950*la - 96*la^2) + 8*En^4*(-53 - 52*la + la^2) + 
        5*(-100 - 79*la + 21*la^2)) + J^2*(10296 - 29808*En^6 - 121*la + 
        1441*la^2 + 128*la^3 - 36*En^4*(-3311 + 24*la) + 
        6*En^2*(-16485 + 522*la + 10*la^2))*M^2 + 
      6*(-12331 + 9681*En^2 + 70*la)*M^4)*rp[t]^15 + 
    M*(J^2*(1683 + 5*la + 394*la^2 + 128*la^3 - 144*En^6*(79 + la) - 
        12*En^4*(-2007 + 53*la + 26*la^2) - 8*En^2*(1800 - 139*la - 38*la^2 + 
          11*la^3)) + (-39333 - 21528*En^4 + 1167*la + 672*la^2 + 
        6*En^2*(10231 + 5*la))*M^2)*rp[t]^16 - 
    2*(2*(-1 + En^2)*J^2*(1 + 3*En^2*(-12 + la) - 8*la)*la*(1 + la) + 
      (-4429 + 1260*En^6 + 461*la + 380*la^2 + 8*la^3 - 
        6*En^4*(1195 + 22*la) - 9*En^2*(-1148 + 25*la + 29*la^2))*M^2)*
     rp[t]^17 + 2*(-1 + En^2)*(351 - 139*la - 138*la^2 - 8*la^3 + 
      36*En^4*(11 + la) + 24*En^2*(-31 + la + 2*la^2))*M*rp[t]^18 - 
    4*(-1 + En^2)^2*la*(7 + 8*la + la^2)*rp[t]^19))/
  (En*rp[t]^14*(J^2 + rp[t]^2)^6) + 
 (8*J^2*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*(-1738800*J^18*M^6 + 
    63*J^18*(48197 - 4672*la + 4302*m^2)*M^5*rp[t] - 
    3*J^16*M^4*(J^2*(678033 - 158230*la - 6246*la^2 - 3*(-47009 + 752*la)*
         m^2 + 824*m^4) + 5620428*M^2)*rp[t]^2 - 
    3*J^16*M^3*(2*J^2*(-107572 + 47366*la + 3869*la^2 - 553*la^3 + 
        (-40120 + 1993*la + 481*la^2)*m^2 - 558*m^4) + 
      (-9825561 + 457020*En^2 + 927984*la - 820164*m^2)*M^2)*rp[t]^3 + 
    J^14*M^2*(J^4*(-95148 + 76018*la + 9583*la^2 - 3939*la^3 - 12*la^4 + 
        (-58686 + 7487*la + 3463*la^2 - 28*la^3)*m^2 + 
        6*(-249 + 2*la + 2*la^2)*m^4) + 6*J^2*(-3294816 + 750577*la + 
        30273*la^2 + 12*(-53631 + 788*la)*m^2 - 3164*m^4 + 
        3*En^2*(94408 - 11543*la + 11988*m^2))*M^2 - 73278648*M^4)*rp[t]^4 + 
    J^14*M*(J^4*(2*(2520 - 4166*la - 699*la^2 + 755*la^3 + 6*la^4) + 
        (5109 - 1942*la - 1338*la^2 + 28*la^3)*m^2 - 3*(-73 + 4*la + 4*la^2)*
         m^4) - 3*J^2*(-2093319 + 901832*la + 75434*la^2 - 10402*la^3 + 
        (-736765 + 34112*la + 8624*la^2)*m^2 - 8652*m^4 + 
        2*En^2*(122089 - 37998*la - 2112*la^2 + (38037 - 752*la)*m^2 + 
          408*m^4))*M^2 - 108*(-1187762 + 115372*En^2 + 108768*la - 
        91387*m^2)*M^4)*rp[t]^5 + 
    J^12*(J^6*la*(216 + 34*la - 185*la^2 - 3*la^3 + (171 + 164*la - 7*la^2)*
         m^2 + 3*(1 + la)*m^4) + J^4*(-926793 + 726645*la + 94250*la^2 - 
        37135*la^3 - 96*la^4 + (-542577 + 65452*la + 31316*la^2 - 224*la^3)*
         m^2 + 42*(-279 + 2*la + 2*la^2)*m^4 + 
        2*En^2*(63123 - 40790*la - 4981*la^2 + 805*la^3 + 
          (37917 - 2636*la - 800*la^2)*m^2 + 1044*m^4))*M^2 - 
      6*J^2*(14358837 + 46170*En^4 - 3179300*la - 131796*la^2 - 
        8*(-324887 + 4326*la)*m^2 + 10332*m^4 - 
        6*En^2*(430705 - 51449*la + 51744*m^2))*M^4 - 187746984*M^6)*
     rp[t]^6 + J^12*M*(J^4*(2*(24570 - 40047*la - 7006*la^2 + 7133*la^3 + 
          48*la^4) + (47613 - 17384*la - 12228*la^2 + 224*la^3)*m^2 - 
        21*(-83 + 4*la + 4*la^2)*m^4 + 2*En^2*(-3465 + 5069*la + 1034*la^2 - 
          562*la^3 - 16*la^4 + (-3861 + 920*la + 566*la^2)*m^2 + 
          4*(-54 + la + la^2)*m^4)) + 3*J^2*(9136023 - 3836880*la - 
        330696*la^2 + 43544*la^3 + (2998505 - 127696*la - 33952*la^2)*m^2 + 
        28476*m^4 + 12*En^4*(5672 - 847*la + 1122*m^2) + 
        En^2*(-2236966 + 681818*la + 37776*la^2 + 2*(-331917 + 5872*la)*m^2 - 
          6016*m^4))*M^2 - 36*(-9144772 + 1396956*En^2 + 806336*la - 
        634775*m^2)*M^4)*rp[t]^7 - 
    J^10*(J^6*la*(1 + la)*(-2106 + 1726*la + 24*la^2 + 7*(-225 + 8*la)*m^2 - 
        21*m^4 + 2*En^2*(117 - 85*la - 8*la^2 + 92*m^2 + 2*m^4)) + 
      2*J^4*(2*(1012716 - 776964*la - 104401*la^2 + 38981*la^3 + 84*la^4) + 
        (1113933 - 125434*la - 62294*la^2 + 392*la^3)*m^2 - 
        63*(-309 + 2*la + 2*la^2)*m^4 + En^2*(-580323 + 368412*la + 
          44829*la^2 - 7164*la^3 + 3*(-111659 + 7188*la + 2296*la^2)*m^2 - 
          7944*m^4) + 6*En^4*(3565 - 1463*la - 192*la^2 + (1875 - 8*la)*m^2 + 
          40*m^4))*M^2 + 6*J^2*(36913257 + 401850*En^4 - 7895284*la - 
        339300*la^2 - 240*(-25293 + 301*la)*m^2 + 18620*m^4 - 
        6*En^2*(1747343 - 203307*la + 196810*m^2))*M^4 + 313589880*M^6)*
     rp[t]^8 + J^10*M*(J^4*(4*(53775 - 86250*la - 15862*la^2 + 15023*la^3 + 
          84*la^4) + 2*(98757 - 34241*la - 24651*la^2 + 392*la^3)*m^2 - 
        63*(-93 + 4*la + 4*la^2)*m^4 + 12*En^4*(192 - 212*la - 77*la^2 + 
          6*la^3 - 2*(-113 + 3*la + 6*la^2)*m^2 + 14*m^4) + 
        2*En^2*(-31905 + 46055*la + 9382*la^2 - 5018*la^3 - 124*la^4 + 
          (-34581 + 7876*la + 4942*la^2)*m^2 + 12*(-143 + 2*la + 2*la^2)*
           m^4)) - 3*J^2*(-23527245 + 2880*En^6 + 9583632*la + 859032*la^2 - 
        106456*la^3 + 5*(-1415081 + 54608*la + 15296*la^2)*m^2 - 51660*m^4 - 
        12*En^4*(49876 - 7375*la + 9440*m^2) + 
        2*En^2*(4563231 - 1358147*la - 74720*la^2 - 5*(-255829 + 3888*la)*
           m^2 + 8920*m^4))*M^2 - 18*(-30616971 + 6558360*En^2 + 2572096*la - 
        1853220*m^2)*M^4)*rp[t]^9 + 
    J^8*(J^6*la*(9225 + 1915*la - 7394*la^2 - 84*la^3 + 
        (6432 + 6236*la - 196*la^2)*m^2 + 63*(1 + la)*m^4 + 
        4*En^4*(1 + la)*(-3*(-4 + la + la^2) + 2*(4 + la)*m^2) - 
        2*En^2*(1065 + 303*la - 824*la^2 - 62*la^3 + 812*(1 + la)*m^2 + 
          12*(1 + la)*m^4)) + J^4*(-10450701 + 7817484*la + 1099280*la^2 - 
        382564*la^3 - 672*la^4 + (-5322495 + 549988*la + 283820*la^2 - 
          1568*la^3)*m^2 + 210*(-339 + 2*la + 2*la^2)*m^4 + 
        216*En^6*(3*(4 + la) + 2*m^2) - 36*En^4*(10526 - 4329*la - 544*la^2 - 
          20*(-270 + la)*m^2 + 104*m^4) + 6*En^2*(793533 - 493141*la - 
          59529*la^2 + 9364*la^3 + (435965 - 25196*la - 8696*la^2)*m^2 + 
          8020*m^4))*M^2 - 6*J^2*(61921542 + 1546938*En^4 - 12671906*la - 
        571554*la^2 + (8954235 - 94080*la)*m^2 + 20020*m^4 - 
        6*En^2*(4127297 - 466157*la + 431680*m^2))*M^4 - 356022000*M^6)*
     rp[t]^10 + J^8*M*(J^4*(4*(139050 - 218964*la - 42803*la^2 + 37009*la^3 + 
          168*la^4) + 2*(239595 - 77290*la - 56976*la^2 + 784*la^3)*m^2 - 
        105*(-103 + 4*la + 4*la^2)*m^4 - 144*En^6*(1 + la)*(1 - la + m^2) + 
        12*En^4*(1704 - 1900*la - 653*la^2 + 62*la^3 - 
          6*(-335 + 13*la + 18*la^2)*m^2 + 126*m^4) + 
        4*En^2*(-65565 + 93092*la + 18925*la^2 - 9937*la^3 - 210*la^4 + 
          3*(-22705 + 4916*la + 3221*la^2)*m^2 + 30*(-89 + la + la^2)*m^4)) - 
      3*J^2*(-39530547 + 25920*En^6 + 15500392*la + 1465612*la^2 - 
        167468*la^3 + 5*(-2113885 + 72932*la + 21524*la^2)*m^2 - 55860*m^4 - 
        12*En^4*(194070 - 28431*la + 34250*m^2) + 
        En^2*(21751294 - 6302762*la - 343568*la^2 + (5748530 - 70880*la)*
           m^2 + 27520*m^4))*M^2 - 18*(-34895847 + 9826380*En^2 + 
        2745552*la - 1742722*m^2)*M^4)*rp[t]^11 + 
    J^6*(J^6*la*(23826 + 5696*la - 18298*la^2 - 168*la^3 + 
        (15093 + 14701*la - 392*la^2)*m^2 + 105*(1 + la)*m^4 + 
        12*En^4*(1 + la)*(34 - 9*la - 7*la^2 + (22 + 4*la)*m^2) - 
        6*En^2*(1472 + 447*la - 1095*la^2 - 70*la^3 + 1110*(1 + la)*m^2 + 
          10*(1 + la)*m^4)) + 2*J^4*(-8784306 + 6384450*la + 955829*la^2 - 
        302113*la^3 - 420*la^4 - 5*(807666 - 75365*la - 40393*la^2 + 
          196*la^3)*m^2 + 105*(-369 + 2*la + 2*la^2)*m^4 + 
        108*En^6*(103 + 22*la + 18*m^2) - 6*En^4*(120786 - 50485*la - 
          5956*la^2 + (57535 - 180*la)*m^2 + 848*m^4) + 
        En^2*(5769669 - 3473416*la - 410549*la^2 + 63188*la^3 - 
          5*(-606183 + 29348*la + 11024*la^2)*m^2 + 37680*m^4))*M^2 - 
      6*J^2*(70849509 + 3491082*En^4 - 13633870*la - 655518*la^2 + 
        (8519396 - 78288*la)*m^2 + 12852*m^4 - 
        30*En^2*(1237331 - 135578*la + 116742*m^2))*M^4 - 276548904*M^6)*
     rp[t]^12 + J^6*M*(J^4*(4*(233280 - 362652*la - 77107*la^2 + 58730*la^3 + 
          210*la^4) + 5*(148413 - 43634*la - 32910*la^2 + 392*la^3)*m^2 - 
        105*(-113 + 4*la + 4*la^2)*m^4 - 1296*En^6*(1 + la)*(1 - la + m^2) + 
        12*En^4*(5166 - 7269*la - 2533*la^2 + 266*la^3 + 
          (6080 - 404*la - 464*la^2)*m^2 + 364*m^4) + 
        4*En^2*(-166185 + 219569*la + 42671*la^2 - 22669*la^3 - 406*la^4 + 
          5*(-32907 + 6154*la + 4231*la^2)*m^2 + 20*(-213 + 2*la + 2*la^2)*
           m^4)) - 3*J^2*(-45383079 + 89280*En^6 + 16846272*la + 
        1711548*la^2 - 175756*la^3 + (-10200415 + 311456*la + 96752*la^2)*
         m^2 - 36036*m^4 - 12*En^4*(473786 - 64775*la + 79040*m^2) + 
        10*En^2*(3245609 - 932675*la - 51616*la^2 + (802531 - 7696*la)*m^2 + 
          2344*m^4))*M^2 - 36*(-13654928 + 4953180*En^2 + 980512*la - 
        514247*m^2)*M^4)*rp[t]^13 + 
    J^4*(J^6*la*(40581 + 11587*la - 29204*la^2 - 210*la^3 + 
        (22140 + 21650*la - 490*la^2)*m^2 + 105*(1 + la)*m^4 + 
        12*En^4*(1 + la)*(224 - 43*la - 21*la^2 + 2*(61 + 5*la)*m^2) - 
        4*En^2*(4839 + 1235*la - 3807*la^2 - 203*la^3 + 3845*(1 + la)*m^2 + 
          20*(1 + la)*m^4)) + J^4*(-20202723 + 14070318*la + 2295188*la^2 - 
        636874*la^3 - 672*la^4 + (-7935231 + 660532*la + 367148*la^2 - 
          1568*la^3)*m^2 + 126*(-399 + 2*la + 2*la^2)*m^4 - 
        72*En^6*(603 - 151*la + 84*m^2) - 12*En^4*(365963 - 118649*la - 
          11044*la^2 - 40*(-3691 + 7*la)*m^2 + 1072*m^4) + 
        30*En^2*(564841 - 349673*la - 41635*la^2 + 5930*la^3 + 
          (294477 - 11340*la - 4624*la^2)*m^2 + 2164*m^4))*M^2 - 
      6*J^2*(55920771 + 4250718*En^4 - 9833252*la - 512628*la^2 + 
        (5090172 - 40656*la)*m^2 + 4564*m^4 - 6*En^2*(6258059 - 638619*la + 
          478864*m^2))*M^4 - 142541208*M^6)*rp[t]^14 + 
    J^4*M*(J^4*(4*(267255 - 408363*la - 97487*la^2 + 62230*la^3 + 168*la^4) + 
        (746289 - 196832*la - 151668*la^2 + 1568*la^3)*m^2 - 
        63*(-123 + 4*la + 4*la^2)*m^4 + 96*En^6*(606 - 31*la + 53*la^2 + 
          (186 - 39*la)*m^2) + 12*En^4*(33174 - 16285*la - 3885*la^2 + 
          610*la^3 - 4*(-4810 + 239*la + 254*la^2)*m^2 + 476*m^4) + 
        10*En^2*(-93393 + 135029*la + 26014*la^2 - 12974*la^3 - 196*la^4 + 
          (-102093 + 15304*la + 11002*la^2)*m^2 + 12*(-124 + la + la^2)*
           m^4)) - 3*J^2*(-36164709 + 604800*En^6 + 12308112*la + 
        1370664*la^2 - 123032*la^3 + (-6186515 + 166112*la + 54224*la^2)*
         m^2 - 12852*m^4 - 12*En^4*(519924 - 92645*la + 109190*m^2) + 
        2*En^2*(16320009 - 4498703*la - 266072*la^2 + (3403847 - 24912*la)*
           m^2 + 5248*m^4))*M^2 - 36*(-7129626 + 3419916*En^2 + 451744*la - 
        173741*m^2)*M^4)*rp[t]^15 + 
    (J^8*la*(48186 + 17204*la - 31150*la^2 - 168*la^3 - 
        96*En^6*(11 + 12*la + la^2) + (20721 + 20329*la - 392*la^2)*m^2 + 
        63*(1 + la)*m^4 + 4*En^4*(1 + la)*(462 - 345*la - 105*la^2 + 
          (964 + 40*la)*m^2) - 10*En^2*(2751 + 624*la - 2225*la^2 - 98*la^3 + 
          2096*(1 + la)*m^2 + 6*(1 + la)*m^4)) + 
      2*J^6*(-8133264 + 21600*En^8 + 5236800*la + 953330*la^2 - 223930*la^3 - 
        168*la^4 + (-2451801 + 180706*la + 103958*la^2 - 392*la^3)*m^2 + 
        21*(-429 + 2*la + 2*la^2)*m^4 + 72*En^6*(10949 - 118*la + 742*m^2) - 
        18*En^4*(103899 - 60547*la - 4632*la^2 + (77195 - 80*la)*m^2 + 
          216*m^4) + 3*En^2*(3*(915437 - 580500*la - 74919*la^2 + 
            8900*la^3) + (1305509 - 39212*la - 17240*la^2)*m^2 + 4888*m^4))*
       M^2 - 6*J^4*(29667147 + 3668382*En^4 - 4583620*la - 263316*la^2 - 
        24*(-72549 + 502*la)*m^2 + 692*m^4 - 6*En^2*(4440165 - 379309*la + 
          217774*m^2))*M^4 - 44346744*J^2*M^6)*rp[t]^16 - 
    J^2*M*(J^4*(2880*En^8*(16 + la) - 4*(217215 - 313806*la - 86578*la^2 + 
          44009*la^3 + 84*la^4) + (-472488 + 110734*la + 87018*la^2 - 
          784*la^3)*m^2 + 21*(-133 + 4*la + 4*la^2)*m^4 + 
        288*En^6*(1442 + 23*la - 9*la^2 + (42 + 17*la)*m^2) - 
        12*En^4*(16416 - 26390*la - 3935*la^2 + 810*la^3 - 
          6*(-6325 + 189*la + 194*la^2)*m^2 + 294*m^4) - 
        2*En^2*(-395325 + 704819*la + 153394*la^2 - 59422*la^3 - 756*la^4 + 
          (-481641 + 56460*la + 42186*la^2)*m^2 + 12*(-283 + 2*la + 2*la^2)*
           m^4)) - 3*J^2*(19581183 + 267840*En^6 - 5829680*la - 724376*la^2 + 
        55384*la^3 + (2148055 - 50576*la - 17312*la^2)*m^2 + 1956*m^4 + 
        12*En^4*(391608 - 80757*la + 77792*m^2) - 
        2*En^2*(11969413 - 2752609*la - 184288*la^2 + (1596307 - 8912*la)*
           m^2 + 968*m^4))*M^2 + 9*(-9042543 + 6003024*En^2 + 487104*la - 
        102710*m^2)*M^4)*rp[t]^17 - 
    (J^6*la*(-40371 - 18265*la + 22190*la^2 + 84*la^3 + 
        96*En^6*(-11 - 7*la + 4*la^2) + 4*(-3015 - 2966*la + 49*la^2)*m^2 - 
        21*(1 + la)*m^4 + 12*En^4*(1 + la)*(244 + 175*la + 35*la^2 - 
          2*(206 + 5*la)*m^2) + 6*En^2*(5279 + 1929*la - 3476*la^2 - 
          126*la^3 + 2784*(1 + la)*m^2 + 4*(1 + la)*m^4)) + 
      J^4*(9052119 + 417600*En^8 - 5081196*la - 1051904*la^2 + 202564*la^3 + 
        96*la^4 + (1734321 - 112828*la - 67028*la^2 + 224*la^3)*m^2 - 
        6*(-459 + 2*la + 2*la^2)*m^4 - 72*En^6*(-9*(1722 + 121*la) + 
          3406*m^2) + 12*En^4*(92688 - 173797*la - 14536*la^2 - 
          4*(-45160 + 27*la)*m^2 + 152*m^4) - 2*En^2*(6288153 - 3340277*la - 
          504289*la^2 + 45004*la^3 + (1910217 - 44924*la - 21128*la^2)*m^2 + 
          2724*m^4))*M^2 + 3*J^2*(19274739 + 5804316*En^4 - 2505238*la - 
        160710*la^2 + (520895 - 3120*la)*m^2 - 
        12*En^2*(2063043 - 129407*la + 42000*m^2))*M^4 + 6331500*M^6)*
     rp[t]^18 + M*(-(J^4*(-503280 + 2880*En^8*(-88 + la) + 635520*la + 
         205748*la^2 - 80092*la^3 - 96*la^4 - 4*(42771 - 8873*la - 
           7098*la^2 + 56*la^3)*m^2 + 3*(-143 + 4*la + 4*la^2)*m^4 + 
         144*En^6*(3*(-503 - 22*la + la^2) + (571 + 21*la)*m^2) - 
         12*En^4*(-28944 - 30102*la - 5647*la^2 + 626*la^3 + 
           (33806 - 662*la - 668*la^2)*m^2 + 70*m^4) - 
         4*En^2*(-156960 + 243715*la + 66751*la^2 - 16979*la^3 - 182*la^4 + 
           (-123738 + 11428*la + 8821*la^2)*m^2 + 2*(-159 + la + la^2)*
            m^4))) + 3*J^2*(6569385 + 169920*En^6 - 1623852*la - 
        228042*la^2 + 14546*la^3 - 5*(-65191 + 1346*la + 482*la^2)*m^2 + 
        60*En^4*(81558 - 7769*la + 4342*m^2) + 
        2*En^2*(-6039461 + 972207*la + 76760*la^2 + 85*(-3719 + 16*la)*m^2))*
       M^2 - 27*(-440749 + 414780*En^2 + 19504*la)*M^4)*rp[t]^19 + 
    (J^4*la*(22950 + 12800*la - 10174*la^2 - 24*la^3 - 
        288*En^6*(-13 - 11*la + 2*la^2) + (3987 + 3931*la - 56*la^2)*m^2 + 
        3*(1 + la)*m^4 + 12*En^4*(1 + la)*(158 - 151*la - 21*la^2 + 
          (254 + 4*la)*m^2) - 2*En^2*(14610 + 8693*la - 6099*la^2 - 
          182*la^3 + 3602*(1 + la)*m^2 + 2*(1 + la)*m^4)) + 
      J^2*(-3178656 + 648000*En^8 + 1457274*la + 346495*la^2 - 53459*la^3 - 
        12*la^4 + (-267810 + 15383*la + 9415*la^2 - 28*la^3)*m^2 + 
        72*En^6*(-19791 - 1142*la + 1790*m^2) + 
        12*En^4*(-274720 + 92523*la + 10228*la^2 + 5*(-10697 + 4*la)*m^2) - 
        6*En^2*(-1209313 + 414776*la + 76229*la^2 - 4804*la^3 + 
          (-130255 + 2436*la + 1216*la^2)*m^2))*M^2 - 
      18*(484269 + 394890*En^4 - 50947*la - 3683*la^2 + 
        En^2*(-915474 + 38709*la))*M^4)*rp[t]^20 - 
    2*M*(J^2*(-94860 - 1440*En^8*(-98 + la) + 96018*la + 36431*la^2 - 
        10633*la^3 - 6*la^4 - 2*(6750 - 1240*la - 1008*la^2 + 7*la^3)*m^2 + 
        24*En^6*(-9735 - 562*la - 127*la^2 + 15*(71 + la)*m^2) + 
        6*En^4*(-8610 + 19745*la + 5743*la^2 - 262*la^3 + 
          8*(-1350 + 19*la + 19*la^2)*m^2) - 2*En^2*(-119520 + 100282*la + 
          35045*la^2 - 5527*la^3 - 50*la^4 + (-26370 + 1958*la + 1553*la^2)*
           m^2)) + 3*(-515491 + 318240*En^6 + 101276*la + 16181*la^2 - 
        849*la^3 + 6*En^4*(-212186 + 7861*la) - 
        3*En^2*(-490274 + 50357*la + 4768*la^2))*M^2)*rp[t]^21 + 
    (-(J^2*la*(1 + la)*(-7911 + 2720*la + 3*la^2 + 96*En^6*(-1 + 4*la) + 
         (-573 + 7*la)*m^2 + 4*En^2*(4158 - 990*la - 25*la^2 + 325*m^2) + 
         En^4*(12*(-720 + 69*la + 7*la^2) - 8*(91 + la)*m^2))) + 
      (-527472 - 181440*En^8 + 188005*la + 51378*la^2 - 6271*la^3 - 
        72*En^6*(-18481 + 383*la) + 36*En^4*(-73513 + 6893*la + 1020*la^2) + 
        6*En^2*(337458 - 68357*la - 15318*la^2 + 671*la^3))*M^2)*rp[t]^22 + 
    2*(17100 - 13139*la - 5764*la^2 + 1255*la^3 + 1440*En^8*(22 + la) + 
      864*En^6*(-130 + 8*la + 3*la^2) + 6*En^4*(24450 - 5329*la - 2103*la^2 + 
        46*la^3) - 4*En^2*(20790 - 9187*la - 3938*la^2 + 392*la^3 + 3*la^4))*
     M*rp[t]^23 - 12*(-1 + En^2)*la*(103 + 76*la - 27*la^2 + 
      8*En^4*(16 + 17*la + la^2) + En^2*(-230 - 209*la + 22*la^2 + la^3))*
     rp[t]^24))/(En*la*(1 + la)*rp[t]^15*(J^2 + rp[t]^2)^9) + 
 ((-16*J*mu*Pi*YPhiBar*(2*M - rp[t])*(173250*J^14*M^5 - 
      3*J^14*(81232 + 299*la)*M^4*rp[t] + 3*J^12*M^3*
       (J^2*(41307 - 280*la - 747*la^2) + 416886*M^2)*rp[t]^2 + 
      J^12*M^2*(2*J^2*(-13390 + 821*la + 1173*la^2 + 6*la^3) + 
        3*(-584564 + 28320*En^2 + 209*la)*M^2)*rp[t]^3 + 
      J^10*M*(-4*J^4*(-513 + 166*la + 197*la^2 + 4*la^3) - 
        3*J^2*(-296028 + 4709*la + 5393*la^2 + En^2*(25912 + 894*la))*M^2 + 
        3885714*M^4)*rp[t]^4 + J^10*(5*J^4*la*(16 + 17*la + la^2) + 
        J^2*(-190886 + 14713*la + 16806*la^2 + 12*la^3 + 
          En^2*(22128 + 1386*la - 684*la^2))*M^2 + 
        3*(-1810576 + 192336*En^2 + 8761*la)*M^4)*rp[t]^5 + 
      J^8*M*(J^4*(14526 - 5075*la - 5581*la^2 - 56*la^3 + 
          2*En^2*(-960 + 5*la + 205*la^2 + 14*la^3)) + 
        3*J^2*(912949 + 2400*En^4 - 23970*la - 16751*la^2 - 
          2*En^2*(88257 + 2506*la))*M^2 + 6742674*M^4)*rp[t]^6 + 
      J^8*(-(J^4*la*(1 + la)*(-568 - 25*la + 12*En^2*(4 + la))) + 
        J^2*(-585234 + 55853*la + 51814*la^2 - 208*la^3 - 
          12*En^4*(262 + 49*la) + En^2*(151212 + 7524*la - 4704*la^2))*M^2 + 
        3*(-3132724 + 554016*En^2 + 30709*la)*M^4)*rp[t]^7 + 
      J^6*M*(2*J^4*(22095 - 8407*la - 8501*la^2 + 14*la^3 - 
          6*En^4*(-25 - 12*la + la^2) + En^2*(-6582 + 161*la + 1402*la^2 + 
            86*la^3)) + 3*J^2*(1572734 + 16224*En^4 - 59803*la - 29037*la^2 - 
          2*En^2*(255001 + 5281*la))*M^2 + 7067286*M^4)*rp[t]^8 + 
      J^6*(2*J^4*la*(868 + 887*la + 19*la^2 + 2*En^4*(1 + la)^2 - 
          2*En^2*(83 + 103*la + 20*la^2)) - 2*J^2*(500810 - 58659*la - 
          44617*la^2 + 400*la^3 + 6*En^4*(1786 + 295*la) + 
          3*En^2*(-73130 - 2475*la + 2294*la^2))*M^2 + 
        3*(-3276176 + 873792*En^2 + 49871*la)*M^4)*rp[t]^9 + 
      J^4*M*(J^4*(74907 - 31446*la - 28947*la^2 + 420*la^3 - 
          12*En^4*(-161 - 68*la + 9*la^2) + 2*En^2*(-19341 + 821*la + 
            4045*la^2 + 200*la^3)) + 3*J^2*(1638421 + 45648*En^4 - 83892*la - 
          30353*la^2 - 4*En^2*(201307 + 1986*la))*M^2 + 4481334*M^4)*
       rp[t]^10 + J^4*(2*J^4*la*(1486 + 1481*la - 5*la^2 + 
          14*En^4*(1 + la)^2 - 4*En^2*(118 + 143*la + 25*la^2)) - 
        2*J^2*(518360 - 73766*la - 46403*la^2 + 650*la^3 + 
          468*En^4*(70 + 11*la) + 12*En^2*(-28703 - 213*la + 928*la^2))*M^2 + 
        3*(-2075356 + 813120*En^2 + 43555*la)*M^4)*rp[t]^11 + 
      J^2*M*(2*J^4*(En^4*(4200 + 1632*la - 156*la^2) + 
          3*(12783 - 5984*la - 4968*la^2 + 140*la^3) + 
          En^2*(-29781 + 2836*la + 6548*la^2 + 220*la^3)) + 
        3*J^2*(1035376 + 61872*En^4 - 67803*la - 19139*la^2 + 
          En^2*(-758052 + 2830*la))*M^2 + 1594134*M^4)*rp[t]^12 + 
      J^2*(J^4*la*(3088 + 2993*la - 95*la^2 + 72*En^4*la*(1 + la) - 
          120*En^2*(13 + 15*la + 2*la^2)) + J^2*(-651870 + 2304*En^6 + 
          111005*la + 58330*la^2 - 1108*la^3 - 24*En^4*(2846 + 491*la) - 
          6*En^2*(-108836 + 3219*la + 3502*la^2))*M^2 + 
        9*(-246320 + 143056*En^2 + 6625*la)*M^4)*rp[t]^13 + 
      M*(J^4*(47754 - 24995*la - 18595*la^2 + 784*la^3 - 288*En^6*(11 + la) - 
          24*En^4*(-98 - 170*la + 33*la^2) + 2*En^2*(-27957 + 5117*la + 
            6319*la^2 + 110*la^3)) + 3*J^2*(368699 + 68496*En^4 - 29586*la - 
          6741*la^2 + 10*En^2*(-41333 + 694*la))*M^2 + 245718*M^4)*rp[t]^14 + 
      (J^4*la*(1952 + 1837*la - 115*la^2 + 8*En^4*(8 + 19*la + 11*la^2) - 
          4*En^2*(404 + 439*la + 35*la^2)) - J^2*(231746 + 18432*En^6 - 
          46205*la - 20522*la^2 + 488*la^3 + 12*En^4*(8038 + 325*la) + 
          12*En^2*(-30937 + 1989*la + 904*la^2))*M^2 + 
        45*(-7620 + 6656*En^2 + 249*la)*M^4)*rp[t]^15 + 
      M*(2*J^2*(8442 - 4897*la - 3257*la^2 + 182*la^3 - 
          144*En^6*(-33 + 2*la) - 6*En^4*(-611 - 76*la + 85*la^2) + 
          En^2*(-16785 + 4235*la + 3394*la^2 + 14*la^3)) + 
        3*(57238 + 38448*En^4 - 5413*la - 1023*la^2 + 
          18*En^2*(-5617 + 145*la))*M^2)*rp[t]^16 + 
      2*(2*J^2*la*(3*(58 + 53*la - 5*la^2) - En^2*(239 + 247*la + 8*la^2) + 
          En^4*(55 + 68*la + 13*la^2)) + (-18052 + 6912*En^6 + 4098*la + 
          1559*la^2 - 44*la^3 + 54*En^4*(-698 + 5*la) - 
          3*En^2*(-16234 + 1359*la + 394*la^2))*M^2)*rp[t]^17 - 
      (-2637 + 1660*la + 989*la^2 - 68*la^3 + 288*En^6*(16 + la) + 
        12*En^4*(-979 + 48*la + 37*la^2) + 2*En^2*(4890 - 1289*la - 
          775*la^2 + 4*la^3))*M*rp[t]^18 + 12*(-1 + En^2)*la*(1 + la)*
       (-9 + la + En^2*(11 + la))*rp[t]^19))/((1 + la)*rp[t]^12*
     (J^2 + rp[t]^2)^7) + ((16*I)*J^3*m*mu*Pi*YPhiPhiBar*(2*M - rp[t])*
     (373905*J^14*M^5 - 3*J^14*(175259 - 7934*la + 4266*m^2)*M^4*rp[t] + 
      3*J^12*M^3*(J^2*(88366 - 10922*la - 1443*la^2 + (5161 - 28*la)*m^2 + 
          8*m^4) + 989871*M^2)*rp[t]^2 + 2*J^12*M^2*
       (J^2*(-28002 + 7796*la + 2195*la^2 - 102*la^3 + 
          6*(-506 + 16*la + 9*la^2)*m^2 - 12*m^4) + 
        9*(-232911 + 11045*En^2 + 9975*la - 4990*m^2)*M^2)*rp[t]^3 + 
      J^10*M*(J^4*(3*(1363 - 969*la - 458*la^2 + 56*la^3) + 
          (765 - 113*la - 92*la^2)*m^2 + 6*m^4) - 
        3*J^2*(-707696 + 83422*la + 11469*la^2 + 2*(-18331 + 84*la)*m^2 - 
          40*m^4 + En^2*(59444 - 3454*la + 2576*m^2))*M^2 + 10274445*M^4)*
       rp[t]^4 + J^10*(J^4*la*(1 + la)*(161 - 33*la + 19*m^2) + 
        2*J^2*(-225348 + 60403*la + 17617*la^2 - 744*la^3 + 
          3*(-7291 + 204*la + 120*la^2)*m^2 - 60*m^4 + 
          6*En^2*(4026 - 698*la - 120*la^2 + (457 - 2*la)*m^2 + 2*m^4))*M^2 + 
        9*(-1620139 + 167672*En^2 + 64650*la - 29330*m^2)*M^4)*rp[t]^5 + 
      J^8*M*(J^4*(33084 - 22921*la - 11164*la^2 + 1236*la^3 - 
          6*(-934 + 125*la + 104*la^2)*m^2 + 30*m^4 - 
          2*En^2*(1884 - 922*la - 383*la^2 + 14*la^3 - 
            6*(-75 + 4*la + 3*la^2)*m^2 + 6*m^4)) + 
        3*J^2*(6060*En^4 - 3*(-825179 + 91510*la + 13225*la^2) - 
          5*(-21863 + 84*la)*m^2 + 80*m^4 - 4*En^2*(114176 - 6419*la + 
            4536*m^2))*M^2 + 20167785*M^4)*rp[t]^6 - 
      2*J^8*(J^4*la*(1 + la)*(-651 + 123*la - 66*m^2 + 
          En^2*(46 - 2*la + 6*m^2)) + 3*J^2*(264414 - 67489*la - 20575*la^2 + 
          774*la^3 - 10*(-2213 + 54*la + 33*la^2)*m^2 + 40*m^4 + 
          4*En^4*(307 + 3*la + 23*m^2) - 2*En^2*(31352 - 5327*la - 908*la^2 + 
            (3321 - 10*la)*m^2 + 8*m^4))*M^2 - 
        3*(-4806290 + 822027*En^2 + 174845*la - 68940*m^2)*M^4)*rp[t]^7 + 
      J^6*M*(J^4*(24*En^4*(25 + 2*la - 3*la^2 + (5 + la)*m^2) - 
          4*En^2*(7440 - 3601*la - 1478*la^2 + 50*la^3 - 
            3*(-564 + 26*la + 21*la^2)*m^2 + 12*m^4) + 
          3*(39090 - 26215*la - 13249*la^2 + 1296*la^3 + 
            (5800 - 685*la - 580*la^2)*m^2 + 20*m^4)) + 
        3*J^2*(46500*En^4 - 2*En^2*(756224 - 40097*la + 26448*m^2) + 
          5*(987229 - 100862*la - 15513*la^2 - 4*(-8693 + 28*la)*m^2 + 
            16*m^4))*M^2 + 24393555*M^4)*rp[t]^8 + 
      J^6*(J^4*la*(1 + la)*(4617 - 783*la + 8*En^4*(1 + la) + 375*m^2 - 
          8*En^2*(91 - 5*la + 12*m^2)) + 
        J^2*(-96*En^4*(599 - 9*la + 46*m^2) + 6*En^2*(211109 - 34674*la - 
            6012*la^2 + (20468 - 40*la)*m^2 + 24*m^4) - 
          5*(638097 - 152308*la - 49126*la^2 + 1608*la^3 - 
            12*(-3585 + 76*la + 48*la^2)*m^2 + 48*m^4))*M^2 + 
        9*(1005632*En^2 - 5*(784595 - 25238*la + 8074*m^2))*M^4)*rp[t]^9 + 
      J^4*M*(J^4*(48*En^4*(93 + 4*la - 9*la^2 + 2*(9 + la)*m^2) + 
          5*(3*(15844 - 10185*la - 5399*la^2 + 452*la^3) + 
            (5772 - 596*la - 512*la^2)*m^2 + 12*m^4) - 
          2*En^2*(51540 - 24409*la - 9905*la^2 + 290*la^3 - 
            12*(-943 + 32*la + 27*la^2)*m^2 + 36*m^4)) + 
        3*J^2*(151560*En^4 - 8*En^2*(349654 - 16637*la + 9448*m^2) + 
          5*(1226192 - 111622*la - 18477*la^2 + (30979 - 84*la)*m^2 + 8*m^4))*
         M^2 + 18226215*M^4)*rp[t]^10 + 
      2*J^4*(2*J^4*la*(1 + la)*(16*En^4*(1 + la) + 
          En^2*(-583 + 35*la - 66*m^2) + 5*(473 - 69*la + 28*m^2)) + 
        J^2*(-24*En^4*(4541 - 153*la + 397*m^2) + 
          12*En^2*(96674 - 15259*la - 2912*la^2 + (7673 - 10*la)*m^2 + 
            4*m^4) - 5*(403152 - 86722*la - 29905*la^2 + 834*la^3 - 
            6*(-3250 + 60*la + 39*la^2)*m^2 + 12*m^4))*M^2 + 
        9*(-1492319 + 581879*En^2 + 40989*la - 9406*m^2)*M^4)*rp[t]^11 + 
      J^2*M*(J^4*(48*En^4*(634 - 22*la - 16*la^2 + (134 + 3*la)*m^2) + 
          5*(61317 - 36265*la - 20332*la^2 + 1416*la^3 + 
            (5349 - 483*la - 420*la^2)*m^2 + 6*m^4) - 
          8*En^2*(2*(10950 - 5867*la - 2575*la^2 + 55*la^3) - 
            3*(-1512 + 38*la + 33*la^2)*m^2 + 6*m^4)) + 
        3*J^2*(4771626 + 189720*En^4 - 371802*la - 66783*la^2 + 
          (73126 - 168*la)*m^2 + 8*m^4 - 2*En^2*(1697794 - 61537*la + 
            26088*m^2))*M^2 + 7771023*M^4)*rp[t]^12 + 
      J^2*(J^4*la*(1 + la)*(16*En^4*(-19 + 11*la) - 
          48*En^2*(111 - 5*la + 7*m^2) + 15*(805 - 97*la + 31*m^2)) + 
        6*J^2*(-538689 + 3600*En^6 + 99517*la + 36925*la^2 - 864*la^3 + 
          (-15587 + 252*la + 168*la^2)*m^2 - 4*m^4 - 
          24*En^4*(355 - 112*la + 188*m^2) + 2*En^2*(250247 - 30332*la - 
            6716*la^2 + (10989 - 10*la)*m^2 + 2*m^4))*M^2 + 
        3*(-3907271 + 2390616*En^2 + 88730*la - 10890*m^2)*M^4)*rp[t]^13 + 
      M*(-(J^4*(1440*En^6*(16 + la) - 48*En^4*(-1256 - 140*la - 69*la^2 + 
             2*(112 + la)*m^2) - 3*(85368 - 43669*la - 25978*la^2 + 
             1476*la^3 + (4360 - 346*la - 304*la^2)*m^2 + 2*m^4) + 
           2*En^2*(124500 - 52544*la - 26185*la^2 + 370*la^3 - 
             6*(-2271 + 44*la + 39*la^2)*m^2 + 6*m^4))) + 
        3*J^2*(2144881 + 403020*En^4 - 137942*la - 27009*la^2 + 
          (14277 - 28*la)*m^2 - 4*En^2*(630604 - 14995*la + 3480*m^2))*M^2 + 
        1449315*M^4)*rp[t]^14 + 
      2*(J^4*la*(1 + la)*(112*En^4*(1 + la) + 51*(93 - 9*la + 2*m^2) - 
          2*En^2*(1973 - 55*la + 51*m^2)) - J^2*(755688 + 80640*En^6 - 
          114863*la - 45947*la^2 + 894*la^3 - 6*(-1543 + 22*la + 15*la^2)*
           m^2 + 36*En^4*(8461 - 195*la + 165*m^2) + 
          6*En^2*(-210064 + 16011*la + 4164*la^2 + (-3013 + 2*la)*m^2))*M^2 + 
        9*(-124652 + 118155*En^2 + 2285*la)*M^4)*rp[t]^15 + 
      M*(J^2*(-2880*En^6*(-30 + la) + 3*(42208 - 17697*la - 11153*la^2 + 
            512*la^3) + (2634 - 185*la - 164*la^2)*m^2 + 
          24*En^4*(1871 - 406*la - 237*la^2 + (211 + la)*m^2) + 
          4*En^2*(-64632 + 15719*la + 8854*la^2 - 82*la^3 + 
            3*(-644 + 10*la + 9*la^2)*m^2)) + 3*(424277 + 324420*En^4 - 
          21970*la - 4695*la^2 + 6*En^2*(-134812 + 2005*la))*M^2)*rp[t]^16 + 
      (J^2*la*(1 + la)*(4187 - 321*la + 8*En^4*(197 + 17*la) + 37*m^2 - 
          8*En^2*(755 - 13*la + 6*m^2)) + (-312069 + 139680*En^6 + 38024*la + 
          16370*la^2 - 264*la^3 + 144*En^4*(-5041 + 30*la) - 
          6*En^2*(-149873 + 6978*la + 2108*la^2))*M^2)*rp[t]^17 - 
      (-27720 + 9265*la + 6157*la^2 - 228*la^3 + 1440*En^6*(36 + la) + 
        48*En^4*(-2745 + 88*la + 58*la^2) + 6*En^2*(17940 - 2567*la - 
          1597*la^2 + 10*la^3))*M*rp[t]^18 + 
      4*la*(6*(33 + 31*la - 2*la^2) + 5*En^2*(-89 - 88*la + la^2) + 
        8*En^4*(31 + 32*la + la^2))*rp[t]^19))/(la*(1 + la)*rp[t]^12*
     (J^2 + rp[t]^2)^8))*Derivative[1][rp][t]
]


Clear[fSourceJT4]
fSourceJT4[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	
(* Created with the Wolfram Language : www.wolfram.com *)
(8*mu*Pi*YBar*(2*M - rp[t])^3*(6435*J^12*M^4 + 15*J^12*(-538 + 3*la)*M^3*
     rp[t] + 2*J^10*M^2*(J^2*(1645 - 59*la - 36*la^2) + 15930*M^2)*rp[t]^2 + 
    3*J^10*M*(2*J^2*(-72 + 13*la + 11*la^2) + 5*(-2697 + 558*En^2 + 14*la)*
       M^2)*rp[t]^3 - 3*J^8*(5*J^4*la*(1 + la) + 
      J^2*(-5575 + En^2*(2130 - 20*la) + 180*la + 108*la^2)*M^2 - 20145*M^4)*
     rp[t]^4 + J^8*M*(J^2*(-2232 + 363*la + 306*la^2 - 
        2*En^2*(-570 + 53*la + 38*la^2)) + 45*(-1741 + 854*En^2 + 7*la)*M^2)*
     rp[t]^5 + 2*J^6*(4*(-9 + 4*En^2)*J^4*la*(1 + la) + 
      3*J^2*(5525 + 390*En^4 - 139*la - 84*la^2 + En^2*(-5005 + 50*la))*M^2 + 
      25380*M^4)*rp[t]^6 + 
    3*J^6*M*(J^2*(4*En^4*(-55 + la) + 3*(-505 + 66*la + 56*la^2) - 
        2*En^2*(-920 + 83*la + 58*la^2)) + 10*(-2287 + 2238*En^2 + 2*la)*M^2)*
     rp[t]^7 + (-6*(21 - 26*En^2 + 2*En^4)*J^8*la*(1 + la) + 
      4*J^6*(7585 + 2502*En^4 - 74*la - 54*la^2 + 30*En^2*(-459 + 4*la))*
       M^2 + 11745*J^4*M^4)*rp[t]^8 + 
    3*J^4*M*(J^2*(-1455 + 102*la + 92*la^2 + 8*En^4*(-125 + 3*la) - 
        2*En^2*(-1805 + 128*la + 88*la^2)) + 5*(-1284 + 3156*En^2 - 19*la)*
       M^2)*rp[t]^9 - 6*(2*(7 - 21*En^2 + 6*En^4)*J^6*la*(1 + la) - 
      J^4*(1670 + 3288*En^4 + 40*En^2*(-174 + la) + 81*la + 36*la^2)*M^2 + 
      1290*J^2*M^4)*rp[t]^10 + 
    J^2*M*(J^2*(24*En^4*(-321 + 4*la) + En^2*(8970 - 376*la - 256*la^2) - 
        3*(555 + 62*la + 42*la^2)) + 45*(173 + 114*En^2 - 6*la)*M^2)*
     rp[t]^11 + (-((-9 - 140*En^2 + 96*En^4)*J^4*la*(1 + la)) + 
      3*J^2*(-755 + 3288*En^4 + 172*la + 84*la^2 - 10*En^2*(215 + 2*la))*
       M^2 - 3735*M^4)*rp[t]^12 - 
    3*M*(-(J^2*(51 + 864*En^6 + 8*En^4*(-191 + la) - 87*la - 66*la^2 + 
         10*En^2*(61 + 3*la + 2*la^2))) + 5*(-293 + 342*En^2 + 5*la)*M^2)*
     rp[t]^13 - 2*(6*(-3 + En^2 + 2*En^4)*J^2*la*(1 + la) + 
      (830 + 1098*En^4 - 73*la - 36*la^2 + 15*En^2*(-129 + 2*la))*M^2)*
     rp[t]^14 - 6*(-1 + En^2)*(33 + 48*En^4 + 2*En^2*(-41 + la) - 13*la - 
      10*la^2)*M*rp[t]^15 + 12*(-1 + En^2)^2*la*(1 + la)*rp[t]^16))/
  (En*rp[t]^13*(J^2 + rp[t]^2)^5) - 
 (8*J^2*mu*Pi*YPhiPhiBar*(2*M - rp[t])^3*(108675*J^16*M^5 - 
    90*J^16*(1747 - 200*la + 149*m^2)*M^4*rp[t] + 
    15*J^14*M^3*(J^2*(5417 - 1599*la - 103*la^2 + (1115 - 16*la)*m^2 + 
        4*m^4) + 62676*M^2)*rp[t]^2 + 6*J^14*M^2*
     (2*J^2*(-1452 + 905*la + 123*la^2 - 22*la^3 + (-565 + 34*la + 14*la^2)*
         m^2 - 5*m^4) + 15*(-15146 + 1302*En^2 + 1680*la - 1183*m^2)*M^2)*
     rp[t]^3 + J^12*M*(J^4*(2*(630 - 914*la - 197*la^2 + 117*la^3) - 
        15*(-59 + 14*la + 10*la^2)*m^2 + 15*m^4) - 
      3*J^2*(-235415 + 67518*la + 4478*la^2 + 35*(-1275 + 16*la)*m^2 - 
        120*m^4 + 90*En^2*(414 - 71*la + 62*m^2))*M^2 + 3591432*M^4)*
     rp[t]^4 + 3*J^12*(J^4*la*(1 + la)*(24 - 17*la + 11*m^2) + 
      J^2*(-50634 + 30777*la + 4325*la^2 - 732*la^3 + 
        2*(-9135 + 496*la + 216*la^2)*m^2 - 120*m^4 + 
        10*En^2*(1064 - 523*la - 37*la^2 - 6*(-71 + 2*la)*m^2 + 4*m^4))*M^2 + 
      6*(-290026 + 52410*En^2 + 30960*la - 20265*m^2)*M^4)*rp[t]^5 + 
    J^10*M*(J^4*(11025 - 15689*la - 3533*la^2 + 1956*la^3 - 
        15*(-483 + 106*la + 78*la^2)*m^2 + 90*m^4 - 
        2*En^2*(1260 - 1660*la - 257*la^2 + 98*la^3 - 
          2*(-570 + 83*la + 38*la^2)*m^2 + 30*m^4)) + 
      3*J^2*(904183 + 10620*En^4 - 250506*la - 17226*la^2 - 
        105*(-1471 + 16*la)*m^2 + 300*m^4 - 30*En^2*(10074 - 1671*la + 
          1396*m^2))*M^2 + 7936812*M^4)*rp[t]^6 + 
    J^10*(J^4*la*(1 + la)*(630 - 429*la + 261*m^2 - 
        2*En^2*(51 - 40*la + 32*m^2)) - 3*J^2*(195132 - 115159*la - 
        16859*la^2 + 2660*la^3 - 6*(-10675 + 516*la + 236*la^2)*m^2 + 
        300*m^4 + 120*En^4*(41 - 13*la + 13*m^2) + 
        En^2*(-87120 + 41522*la + 3002*la^2 + 80*(-407 + 9*la)*m^2 - 
          200*m^4))*M^2 + 18*(-642906 + 183846*En^2 + 65360*la - 38675*m^2)*
       M^4)*rp[t]^7 + J^8*M*(J^4*(42660 - 59404*la - 14104*la^2 + 7140*la^3 - 
        90*(-287 + 57*la + 43*la^2)*m^2 + 225*m^4 + 
        24*En^4*(48 - 61*la - 6*la^2 - 5*(-11 + la)*m^2 + 2*m^4) - 
        6*En^2*(3480 - 4466*la - 713*la^2 + 258*la^3 - 
          4*(-745 + 93*la + 48*la^2)*m^2 + 50*m^4)) + 
      3*J^2*(2010743 + 81432*En^4 - 533158*la - 38438*la^2 - 
        175*(-1703 + 16*la)*m^2 + 400*m^4 - 6*En^2*(178286 - 28505*la + 
          22630*m^2))*M^2 + 11155590*M^4)*rp[t]^8 + 
    3*J^8*(J^4*la*(1 + la)*(813 - 525*la + 291*m^2 + 8*En^4*(1 - la + m^2) - 
        6*En^2*(47 - 36*la + 28*m^2)) + J^2*(-435246 + 360*En^6 + 247777*la + 
        38301*la^2 - 5516*la^3 + 20*(-6265 + 268*la + 128*la^2)*m^2 - 
        400*m^4 - 60*En^4*(645 - 194*la + 186*m^2) + 
        2*En^2*(156222 - 72031*la - 5281*la^2 + (54490 - 900*la)*m^2 + 
          200*m^4))*M^2 + 30*(72858*En^2 + 216*(-841 + 80*la) - 8855*m^2)*
       M^4)*rp[t]^9 + J^6*M*(J^4*(95310 - 129953*la - 33125*la^2 + 
        14868*la^3 - 150*(-343 + 61*la + 47*la^2)*m^2 + 300*m^4 + 
        24*En^4*(387 - 474*la - 49*la^2 - 5*(-83 + 5*la)*m^2 + 8*m^4) - 
        6*En^2*(12780 - 15801*la - 2549*la^2 + 862*la^3 - 
          10*(-1046 + 103*la + 58*la^2)*m^2 + 100*m^4)) + 
      3*J^2*(275796*En^4 - 30*En^2*(70758 - 10895*la + 7864*m^2) + 
        5*(571445 - 142336*la - 10896*la^2 + (68985 - 560*la)*m^2 + 60*m^4))*
       M^2 + 10263780*M^4)*rp[t]^10 + 
    3*J^6*(J^4*la*(1 + la)*(1847 - 1099*la + 535*m^2 + 
        72*En^4*(1 - la + m^2) - 8*En^2*(121 - 92*la + 65*m^2)) + 
      J^2*(3240*En^6 - 12*En^4*(11887 - 3300*la + 3260*m^2) + 
        10*En^2*(61606 - 28165*la - 2185*la^2 + (19552 - 240*la)*m^2 + 
          40*m^4) - 5*(124488 - 67059*la - 11111*la^2 + 1428*la^3 - 
          8*(-3675 + 139*la + 69*la^2)*m^2 + 60*m^4))*M^2 + 
      30*(-168638 + 91170*En^2 + 14640*la - 6069*m^2)*M^4)*rp[t]^11 + 
    J^4*M*(J^4*(24*En^4*(1863 - 1729*la - 129*la^2 + (1740 - 50*la)*m^2 + 
          12*m^4) + 5*(4*(6858 - 8993*la - 2507*la^2 + 966*la^3) - 
          15*(-819 + 130*la + 102*la^2)*m^2 + 45*m^4) - 
        10*En^2*(5*(2916 - 3833*la - 673*la^2 + 190*la^3) - 
          8*(-1473 + 113*la + 68*la^2)*m^2 + 60*m^4)) + 
      3*J^2*(440784*En^4 - 30*En^2*(89510 - 12535*la + 7606*m^2) + 
        5*(536473 - 121970*la - 10034*la^2 + (47775 - 336*la)*m^2 + 24*m^4))*
       M^2 + 6054480*M^4)*rp[t]^12 + 
    J^4*(J^4*la*(1 + la)*(48*En^4*(4 - 15*la + 13*m^2) - 
        40*En^2*(156 - 103*la + 62*m^2) + 15*(542 - 287*la + 117*m^2)) + 
      3*J^2*(-592998 + 27216*En^6 + 292115*la + 52567*la^2 - 5908*la^3 + 
        6*(-17185 + 576*la + 296*la^2)*m^2 - 120*m^4 - 
        24*En^4*(8109 - 3050*la + 2750*m^2) + 10*En^2*(78540 - 33337*la - 
          2887*la^2 - 2*(-9719 + 90*la)*m^2 + 20*m^4))*M^2 + 
      90*(-100862 + 74166*En^2 + 7760*la - 2303*m^2)*M^4)*rp[t]^13 + 
    J^2*M*(J^4*(133425 - 161243*la - 49799*la^2 + 16044*la^3 - 
        45*(-973 + 138*la + 110*la^2)*m^2 + 90*m^4 - 
        1440*En^6*(20 - 3*la + 6*m^2) + 48*En^4*(486 - 1768*la - 143*la^2 - 
          5*(-328 + 5*la)*m^2 + 4*m^4) - 30*En^2*(6120 - 7942*la - 
          1641*la^2 + 346*la^3 - 2*(-2026 + 123*la + 78*la^2)*m^2 + 
          10*m^4)) + 3*J^2*(1633765 + 504036*En^4 - 327478*la - 29238*la^2 + 
        (91525 - 560*la)*m^2 + 20*m^4 - 30*En^2*(75526 - 8653*la + 3860*m^2))*
       M^2 + 2092932*M^4)*rp[t]^14 + 
    3*J^2*(J^4*la*(1 + la)*(2624 + 160*En^6 - 1197*la + 381*m^2 + 
        16*En^4*(16 - 25*la + 17*m^2) - 10*En^2*(309 - 152*la + 72*m^2)) - 
      J^2*(370524 + 20592*En^6 - 159917*la - 31545*la^2 + 3052*la^3 - 
        2*(-19985 + 596*la + 316*la^2)*m^2 + 20*m^4 + 
        24*En^4*(9276 - 3125*la + 2145*m^2) - 10*En^2*(70092 - 23783*la - 
          2387*la^2 - 8*(-1261 + 9*la)*m^2 + 4*m^4))*M^2 + 
      6*(-177566 + 180930*En^2 + 11760*la - 1865*m^2)*M^4)*rp[t]^15 + 
    M*(J^4*(86760 + 17280*En^8 - 91324*la - 31408*la^2 + 8316*la^3 - 
        30*(-574 + 73*la + 59*la^2)*m^2 + 15*m^4 - 
        1440*En^6*(-46 - 9*la + 12*m^2) + 24*En^4*(558 - 4059*la - 484*la^2 - 
          25*(-109 + la)*m^2 + 2*m^4) - 6*En^2*(30420 - 30120*la - 
          7559*la^2 + 1126*la^3 - 4*(-2705 + 133*la + 88*la^2)*m^2 + 
          10*m^4)) + 3*J^2*(589493 + 445464*En^4 - 100706*la - 9826*la^2 + 
        (14955 - 80*la)*m^2 - 30*En^2*(39018 - 3311*la + 802*m^2))*M^2 + 
      323487*M^4)*rp[t]^16 + 
    3*(J^4*la*(1 + la)*(1625 + 160*En^6 - 623*la + 137*m^2 + 
        24*En^4*(41 - 15*la + 7*m^2) - 2*En^2*(1477 - 500*la + 164*m^2)) + 
      J^2*(-138402 + 9288*En^6 + 50259*la + 10919*la^2 - 900*la^3 + 
        8*(-825 + 22*la + 12*la^2)*m^2 - 12*En^4*(22333 - 3330*la + 
          1250*m^2) - 2*En^2*(-197950 + 47181*la + 5571*la^2 + 
          10*(-1067 + 6*la)*m^2))*M^2 + 18*(-9347 + 13202*En^2 + 520*la)*M^4)*
     rp[t]^17 + M*(J^2*(34200 - 46080*En^8 - 29819*la - 11399*la^2 + 
        2460*la^3 - 30*(-96 + 11*la + 9*la^2)*m^2 - 
        1440*En^6*(-26 - 9*la + 6*m^2) - 24*En^4*(-4005 + 2422*la + 
          417*la^2 + 5*(-165 + la)*m^2) + 2*En^2*(-60840 + 38383*la + 
          11609*la^2 - 1214*la^3 + 2*(-3510 + 143*la + 98*la^2)*m^2)) + 
      3*(95758 + 177228*En^4 - 13573*la - 1453*la^2 + 
        6*En^2*(-45346 + 2705*la))*M^2)*rp[t]^18 + 
    (-(J^2*la*(1 + la)*(-1749 + 480*En^6 + 555*la - 63*m^2 - 
         24*En^4*(143 - 21*la + 5*m^2) + 4*En^2*(1179 - 272*la + 46*m^2))) + 
      3*(-23376 + 54216*En^6 + 6937*la + 1661*la^2 - 116*la^3 + 
        12*En^4*(-10927 + 720*la) - 2*En^2*(-50142 + 8011*la + 1111*la^2))*
       M^2)*rp[t]^19 + 2*(3060 + 8640*En^8 - 2144*la - 905*la^2 + 159*la^3 + 
      720*En^6*(-40 + 3*la) - 12*En^4*(-2895 + 583*la + 133*la^2) - 
      3*En^2*(5880 - 2331*la - 829*la^2 + 62*la^3))*M*rp[t]^20 - 
    12*(-1 + En^2)*la*(23 + 17*la - 6*la^2 + 40*En^4*(1 + la) + 
      8*En^2*(-8 - 7*la + la^2))*rp[t]^21))/(En*la*(1 + la)*rp[t]^14*
   (J^2 + rp[t]^2)^8) + 
 ((-16*J*mu*Pi*YPhiBar*(-2*M + rp[t])^2*(11403*J^12*M^4 + 
      3*J^12*(-4145 + 68*la)*M^3*rp[t] + 3*J^10*M^2*
       (J^2*(1438 - 123*la - 65*la^2) + 23508*M^2)*rp[t]^2 + 
      J^10*M*(-4*J^2*(117 - 45*la - 35*la^2 + la^3) + 
        3*(-25579 + 2886*En^2 + 532*la)*M^2)*rp[t]^3 + 
      J^8*(J^4*la*(-25 - 23*la + 2*la^2) - 3*J^2*(-8843 + 854*la + 402*la^2 + 
          2*En^2*(901 + 6*la))*M^2 + 182403*M^4)*rp[t]^4 + 
      J^8*M*(J^2*(-2862 + 1169*la + 862*la^2 - 28*la^3 + 
          En^2*(756 - 66*la - 84*la^2)) + 6*(-33010 + 8241*En^2 + 844*la)*
         M^2)*rp[t]^5 + J^6*(2*J^4*la*(1 + la)*(-77 + 12*En^2 + 7*la) + 
        3*J^2*(22738 + 324*En^4 - 2477*la - 1039*la^2 + 
          2*En^2*(-5169 + 22*la))*M^2 + 252792*M^4)*rp[t]^6 + 
      J^6*M*(-(J^2*(7308 - 3193*la - 2222*la^2 + 80*la^3 + 24*En^4*(7 + la) + 
           6*En^2*(-728 + 83*la + 82*la^2))) + 
        6*(-45685 + 19182*En^2 + 1396*la)*M^2)*rp[t]^7 + 
      (-2*J^8*(199 + 2*En^2*(-35 + la) - 20*la)*la*(1 + la) + 
        12*J^6*(7843 + 486*En^4 - 957*la - 359*la^2 + 4*En^2*(-1504 + 29*la))*
         M^2 + 198153*J^4*M^4)*rp[t]^8 + 
      J^4*M*(-(J^2*(10017 - 4682*la - 3068*la^2 + 120*la^3 + 
           168*En^4*(7 + la) + 6*En^2*(-1673 + 276*la + 200*la^2))) + 
        3*(-71605 + 46500*En^2 + 2548*la)*M^2)*rp[t]^9 + 
      (-4*J^6*la*(1 + la)*(138 - 15*la + En^2*(-89 + 4*la)) + 
        3*J^4*(24548 + 4032*En^4 - 3317*la - 1119*la^2 + 
          56*En^2*(-529 + 18*la))*M^2 + 83340*J^2*M^4)*rp[t]^10 + 
      J^2*M*(-(J^2*(7803 - 3878*la - 2392*la^2 + 100*la^3 + 
           72*En^4*(23 + la) + 6*En^2*(-2091 + 440*la + 248*la^2))) + 
        (-90465 + 88218*En^2 + 3660*la)*M^2)*rp[t]^11 + 
      (-(J^4*la*(433 + 383*la - 50*la^2 + 48*En^4*(1 + la) + 
           12*En^2*(-39 - 37*la + 2*la^2))) + 3*J^2*(10345 + 5928*En^4 - 
          1526*la - 466*la^2 + 2*En^2*(-9699 + 430*la))*M^2 + 14697*M^4)*
       rp[t]^12 + M*(-(J^2*(3285 + 1440*En^6 + En^4*(4056 - 264*la) - 
           1717*la - 998*la^2 + 44*la^3 + 6*En^2*(-1445 + 329*la + 
             154*la^2))) + 18*(-889 + 1279*En^2 + 40*la)*M^2)*rp[t]^13 + 
      (-2*J^2*la*(91 + 80*la - 11*la^2 + 48*En^4*(1 + la) + 
          2*En^2*(-77 - 73*la + 4*la^2)) + 
        3*(3516*En^4 + En^2*(-5302 + 260*la) - 3*(-612 + 97*la + 27*la^2))*
         M^2)*rp[t]^14 + (-585 + 1440*En^6 + 317*la + 174*la^2 - 8*la^3 + 
        48*En^4*(-71 + 4*la) - 6*En^2*(-425 + 93*la + 38*la^2))*M*rp[t]^15 - 
      4*(-1 + En^2)*la*(1 + la)*(-8 + 12*En^2 + la)*rp[t]^16))/
    ((1 + la)*rp[t]^11*(J^2 + rp[t]^2)^6) + 
   ((16*I)*J^3*m*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*
     (21231*J^12*M^4 - 3*J^12*(7795 - 412*la + 168*m^2)*M^3*rp[t] + 
      3*J^10*M^2*(J^2*(2716 - 467*la - 105*la^2 + 148*m^2) + 48618*M^2)*
       rp[t]^2 + 2*J^10*M*(J^2*(-438 + 237*la + 114*la^2 - 6*la^3 + 
          2*(-24 + la + la^2)*m^2) + 18*(-4494 + 521*En^2 + 220*la - 80*m^2)*
         M^2)*rp[t]^3 - J^8*(J^4*la*(1 + la)*(43 - 6*la + 2*m^2) + 
        6*J^2*(-9476 + 1529*la + 359*la^2 - 430*m^2 + 
          2*En^2*(961 - 98*la + 52*m^2))*M^2 - 425385*M^4)*rp[t]^4 + 
      J^8*M*(2*J^2*(-3090 + 1595*la + 794*la^2 - 36*la^3 + 
          5*(-57 + 2*la + 2*la^2)*m^2 + 6*En^2*(126 - 53*la - 14*la^2 + 
            21*m^2)) + 9*(-52925 + 13432*En^2 + 2340*la - 720*m^2)*M^2)*
       rp[t]^5 + J^6*(2*J^4*la*(1 + la)*(-151 + 18*la - 5*m^2 + 
          2*En^2*(12 - 2*la + m^2)) + 3*J^2*(828*En^4 - 
          4*En^2*(6325 - 592*la + 288*m^2) + 5*(11287 - 1669*la - 415*la^2 + 
            392*m^2))*M^2 + 677700*M^4)*rp[t]^6 + 
      4*J^6*M*(J^2*(-12*En^4*(7 - 3*la + 2*m^2) + 
          6*En^2*(427 - 169*la - 46*la^2 + 62*m^2) + 
          5*(-933 + 451*la + 235*la^2 - 9*la^3 + 2*(-33 + la + la^2)*m^2)) + 
        6*(13533*En^2 - 5*(6419 - 248*la + 60*m^2))*M^2)*rp[t]^7 + 
      (2*J^8*la*(1 + la)*(-5*(91 - 9*la + 2*m^2) + 
          4*En^2*(41 - 5*la + 2*m^2)) + 12*J^6*(1404*En^4 + 
          5*(4653 - 607*la - 161*la^2 + 110*m^2) - 
          2*En^2*(8635 - 694*la + 276*m^2))*M^2 + 620865*J^4*M^4)*rp[t]^8 + 
      J^4*M*(-4*J^2*(En^2*(-7038 + 2586*la + 804*la^2 - 738*m^2) + 
          96*En^4*(7 - 3*la + 2*m^2) - 5*(-1581 + 683*la + 374*la^2 - 
            12*la^3 + (-75 + 2*la + 2*la^2)*m^2)) + 
        9*(-79995 + 54112*En^2 + 2620*la - 440*m^2)*M^2)*rp[t]^9 + 
      (4*J^6*la*(1 + la)*(-5*(74 - 6*la + m^2) + 
          En^2*(268 - 20*la + 6*m^2)) + 3*J^4*(11640*En^4 - 
          8*En^2*(13835 - 796*la + 224*m^2) + 5*(17870 - 1985*la - 563*la^2 + 
            244*m^2))*M^2 + 308286*J^2*M^4)*rp[t]^10 + 
      2*J^2*M*(J^2*(-48*En^4*(8 - 27*la + 13*m^2) + 
          24*En^2*(1056 - 272*la - 98*la^2 + 51*m^2) + 
          5*(-3156 + 1165*la + 670*la^2 - 18*la^3 + 2*(-42 + la + la^2)*
             m^2)) + 18*(10817*En^2 - 4*(2540 - 69*la + 6*m^2))*M^2)*
       rp[t]^11 + (-(J^4*la*(1 + la)*(240*En^4 - 16*En^2*(108 - 5*la + m^2) + 
           5*(271 - 18*la + 2*m^2))) + 6*J^2*(23398 + 14952*En^4 - 2161*la - 
          655*la^2 + 134*m^2 - 2*En^2*(24025 - 898*la + 132*m^2))*M^2 + 
        64359*M^4)*rp[t]^12 + M*(-2*J^2*(8628 + 4320*En^6 - 2647*la - 
          1594*la^2 + 36*la^3 + (93 - 2*la - 2*la^2)*m^2 + 
          144*En^4*(87 - 8*la + 2*m^2) - 6*En^2*(4196 - 677*la - 278*la^2 + 
            61*m^2)) + 3*(-26053 + 41688*En^2 + 580*la)*M^2)*rp[t]^13 + 
      (-2*J^2*la*(1 + la)*(329 + 240*En^4 - 18*la + m^2 - 
          2*En^2*(328 - 10*la + m^2)) + 3*(10307 + 23052*En^4 - 783*la - 
          253*la^2 + 4*En^2*(-8261 + 200*la))*M^2)*rp[t]^14 + 
      4*(-990 + 2880*En^6 + 180*En^4*(-37 + la) + 250*la + 157*la^2 - 
        3*la^3 - 6*En^2*(-795 + 83*la + 38*la^2))*M*rp[t]^15 - 
      2*la*(66 + 63*la - 3*la^2 + 120*En^4*(1 + la) + 
        4*En^2*(-47 - 46*la + la^2))*rp[t]^16))/(la*(1 + la)*rp[t]^11*
     (J^2 + rp[t]^2)^7))*Derivative[1][rp][t]
]


Clear[gSourceJT5]
gSourceJT5[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];

(* Created with the Wolfram Language : www.wolfram.com *)
((8*I)*J^3*m*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*(18633132*J^20*M^7 - 
    36*J^20*(1165451 - 36936*la + 17267*m^2)*M^6*rp[t] + 
    3*J^18*M^5*(J^2*(12802655 - 988544*la - 76984*la^2 + 
        (426452 - 3024*la)*m^2 + 656*m^4) + 66084696*M^2)*rp[t]^2 + 
    J^18*M^4*(J^2*(-18202557 + 2657620*la + 427216*la^2 - 14940*la^3 + 
        (-1036323 + 22916*la + 6140*la^2)*m^2 - 3648*m^4) + 
      18*(-24824642 + 2301159*En^2 + 765440*la - 340234*m^2)*M^2)*rp[t]^3 + 
    2*J^16*M^3*(-(J^4*(-2338497 + 605877*la + 152532*la^2 - 12674*la^3 + 
         28*la^4 + (-205824 + 11028*la + 5230*la^2 - 20*la^3)*m^2 + 
         4*(-315 + la + la^2)*m^4)) - 9*J^2*(-22748803 + 1712620*la + 
        136252*la^2 + 120*(-5857 + 39*la)*m^2 - 928*m^4 + 
        8*En^2*(502137 - 20480*la + 12656*m^2))*M^2 + 477273006*M^4)*
     rp[t]^4 + J^16*M^2*(J^4*(-611418 + 291422*la + 103973*la^2 - 
        15867*la^3 + 84*la^4 + (-79830 + 10145*la + 6575*la^2 - 60*la^3)*
         m^2 + 12*(-64 + la + la^2)*m^4) + J^2*(-194278593 + 27722356*la + 
        4547572*la^2 - 153480*la^3 + (-10290069 + 215588*la + 59420*la^2)*
         m^2 - 31104*m^4 + 6*En^2*(8079548 - 820991*la - 60031*la^2 + 
          (473486 - 4656*la)*m^2 + 1532*m^4))*M^2 + 
      18*(23396727*En^2 - 4*(29918463 - 893892*la + 374012*m^2))*M^4)*
     rp[t]^5 + J^14*M*(-3*J^6*(2*(-5212 + 5664*la + 2770*la^2 - 721*la^3 + 
          7*la^4) + (-2003 + 742*la + 600*la^2 - 10*la^3)*m^2 + 
        (-29 + 2*la + 2*la^2)*m^4) - J^4*(-49978065 + 12690190*la + 
        3256478*la^2 - 260824*la^3 + 568*la^4 - 
        9*(456259 - 23328*la - 11300*la^2 + 40*la^3)*m^2 + 
        32*(-675 + 2*la + 2*la^2)*m^4 + 6*En^2*(2570369 - 514078*la - 
          80682*la^2 + 3800*la^3 + (268987 - 8502*la - 2250*la^2)*m^2 + 
          2076*m^4))*M^2 + 3*J^2*(658879567 + 7629840*En^4 - 48178216*la - 
        3926512*la^2 - 32*(-581939 + 3618*la)*m^2 + 20608*m^4 - 
        24*En^2*(10244287 - 405026*la + 240474*m^2))*M^4 + 2741430528*M^6)*
     rp[t]^6 + J^14*(J^6*la*(1404 + 966*la - 431*la^2 + 7*la^3 - 
        5*(-37 - 36*la + la^2)*m^2 + (1 + la)*m^4) + 
      J^4*(-6542085 + 3066181*la + 1113709*la^2 - 163566*la^3 + 852*la^4 + 
        (-800523 + 97701*la + 64275*la^2 - 540*la^3)*m^2 + 
        96*(-69 + la + la^2)*m^4 - 2*En^2*(-1143381 + 439947*la + 
          114251*la^2 - 13566*la^3 - 8*la^4 + (-196386 + 16445*la + 
            8125*la^2 - 40*la^3)*m^2 + (-2781 + 16*la + 16*la^2)*m^4))*M^2 - 
      6*J^2*(6*En^4*(788557 - 39806*la + 29638*m^2) + 
        En^2*(-82715654 + 8170601*la + 609985*la^2 + 16*(-283379 + 2511*la)*
           m^2 - 12164*m^4) + 2*(78264317 - 10875924*la - 1825435*la^2 + 
          59165*la^3 + (3804827 - 75104*la - 21320*la^2)*m^2 + 9632*m^4))*
       M^4 + 72*(-86064836 + 26751627*En^2 + 2479056*la - 963200*m^2)*M^6)*
     rp[t]^7 + 2*J^12*M*(J^6*(167544 - 179842*la - 89407*la^2 + 22344*la^3 - 
        213*la^4 + (30312 - 10853*la - 8855*la^2 + 135*la^3)*m^2 - 
        6*(-63 + 4*la + 4*la^2)*m^4 + En^2*(-61320 + 54053*la + 21778*la^2 - 
          5211*la^3 - 8*la^4 + (-17058 + 4392*la + 3155*la^2 - 40*la^3)*m^2 + 
          8*(-51 + 2*la + 2*la^2)*m^4)) + 
      J^4*(-3*(-40329543 + 10003686*la + 2623309*la^2 - 201446*la^3 + 
          432*la^4) + 20*(457983 - 22216*la - 11002*la^2 + 36*la^3)*m^2 - 
        112*(-360 + la + la^2)*m^4 + 6*En^4*(1021348 - 135795*la - 
          12173*la^2 + (94060 - 1108*la)*m^2 + 548*m^4) - 
        3*En^2*(26420420 - 5153061*la - 824487*la^2 + 37220*la^3 + 
          (2599166 - 75488*la - 21080*la^2)*m^2 + 16692*m^4))*M^2 + 
      12*J^2*(237335201 + 9234180*En^4 - 16773616*la - 1405048*la^2 + 
        (6023948 - 34776*la)*m^2 + 5432*m^4 - 6*En^2*(23516133 - 895748*la + 
          506131*m^2))*M^4 + 2603551356*M^6)*rp[t]^8 + 
    J^12*(J^6*la*(1 + la)*(14992 - 4534*la + 71*la^2 + (1830 - 45*la)*m^2 + 
        8*m^4 + En^2*(-3932 + 1274*la + 4*la^2 + 20*(-40 + la)*m^2 - 
          8*m^4)) + J^4*(-4*En^4*(529569 - 153335*la - 31275*la^2 + 
          1715*la^3 - 3*(-31243 + 1370*la + 460*la^2)*m^2 + 1410*m^4) + 
        3*(-10576133 + 4860989*la + 1801832*la^2 - 253173*la^3 + 1296*la^4 + 
          (-1197893 + 139556*la + 93260*la^2 - 720*la^3)*m^2 + 
          112*(-74 + la + la^2)*m^4) - 2*En^2*(-11805159 + 4446183*la + 
          1175283*la^2 - 133742*la^3 - 56*la^4 - 
          2*(958803 - 75094*la - 38520*la^2 + 160*la^3)*m^2 + 
          (-22707 + 112*la + 112*la^2)*m^4))*M^2 + 
      4*J^2*(-677877711 + 937278*En^6 + 91315832*la + 15732632*la^2 - 
        486840*la^3 + 7*(-4243281 + 78472*la + 22960*la^2)*m^2 - 61152*m^4 - 
        18*En^4*(3845573 - 188788*la + 135687*m^2) + 
        6*En^2*(95342397 - 9105914*la - 697518*la^2 - 6*(-803035 + 6286*la)*
           m^2 + 10203*m^4))*M^4 + 72*(72441027*En^2 - 
        7*(23403945 - 645768*la + 228629*m^2))*M^6)*rp[t]^9 + 
    J^10*M*(-(J^6*(3*(-542734 + 574272*la + 290846*la^2 - 69334*la^3 + 
           648*la^4 + (-91381 + 31424*la + 25880*la^2 - 360*la^3)*m^2 + 
           56*(-17 + la + la^2)*m^4) + 4*En^4*(-29232 + 20889*la + 
           7788*la^2 - 1174*la^3 - 22*la^4 + 2*(-4758 + 757*la + 490*la^2)*
            m^2 + (-294 + 4*la + 4*la^2)*m^4) - 4*En^2*(-318120 + 275768*la + 
           112913*la^2 - 25876*la^3 - 28*la^4 + (-84318 + 20672*la + 
             15175*la^2 - 160*la^3)*m^2 + 2*(-849 + 28*la + 28*la^2)*m^4))) - 
      2*J^4*(-350026047 + 84460724*la + 22704692*la^2 - 1660752*la^3 + 
        3504*la^4 - 14*(1713393 - 78376*la - 39700*la^2 + 120*la^3)*m^2 + 
        112*(-765 + 2*la + 2*la^2)*m^4 + 144*En^6*(9598 - 493*la + 562*m^2) - 
        36*En^4*(1673677 - 217307*la - 19589*la^2 - 3*(-48569 + 506*la)*m^2 + 
          708*m^4) + 6*En^2*(61197779 - 11586992*la - 1898324*la^2 + 
          81200*la^3 + (5586143 - 146320*la - 43420*la^2)*m^2 + 28278*m^4))*
       M^2 + 18*J^2*(53234304*En^4 - 8*En^2*(63965384 - 2326372*la + 
          1232865*m^2) + 7*(86255451 - 5854944*la - 506040*la^2 - 
          4*(-479213 + 2556*la)*m^2 + 1360*m^4))*M^4 + 6844500432*M^6)*
     rp[t]^10 + J^10*(J^6*la*(72567 + 51411*la - 20832*la^2 + 324*la^3 + 
        (8075 + 7895*la - 180*la^2)*m^2 + 28*(1 + la)*m^4 + 
        4*En^4*(678 + 502*la - 187*la^2 - 11*la^3 + 160*(1 + la)*m^2 + 
          2*(1 + la)*m^4) - 4*En^2*(10156 + 6964*la - 3199*la^2 - 7*la^3 + 
          (1950 + 1910*la - 40*la^2)*m^2 + 14*(1 + la)*m^4)) + 
      J^4*(24*En^6*(24011 - 3506*la - 680*la^2 + (3759 + 10*la)*m^2 + 
          44*m^4) - 4*En^4*(5*(1050195 - 298396*la - 60984*la^2 + 
            3280*la^3) - 3*(-295545 + 11942*la + 4220*la^2)*m^2 + 
          11340*m^4) + 3*(-30665759 + 13772140*la + 5225188*la^2 - 
          697272*la^3 + 3504*la^4 - 7*(451499 - 49868*la - 33860*la^2 + 
            240*la^3)*m^2 + 224*(-79 + la + la^2)*m^4) - 
        2*En^2*(-54984030 + 20195073*la + 5454741*la^2 - 588040*la^3 - 
          160*la^4 + (-8349954 + 601116*la + 322040*la^2 - 1120*la^3)*m^2 + 
          (-77841 + 336*la + 336*la^2)*m^4))*M^2 + 
      2*J^2*(17703036*En^6 - 36*En^4*(16775547 - 797416*la + 549243*m^2) + 
        12*En^2*(260622370 - 23880412*la - 1891632*la^2 + 
          (11882068 - 80556*la)*m^2 + 18805*m^4) - 
        7*(370491417 - 48084124*la - 8536816*la^2 + 250500*la^3 - 
          5*(-2853171 + 49180*la + 14836*la^2)*m^2 + 23040*m^4))*M^4 + 
      36*(257106729*En^2 - 14*(30854895 - 808944*la + 254023*m^2))*M^6)*
     rp[t]^11 + 2*J^8*M*(J^6*(2366349 - 2461637*la - 1273535*la^2 + 
        287184*la^3 - 2628*la^4 + 35*(10425 - 3416*la - 2840*la^2 + 36*la^3)*
         m^2 - 42*(-73 + 4*la + 4*la^2)*m^4 - 
        8*En^6*(1902 - 826*la - 430*la^2 - 3*la^3 + (666 - 2*la - 23*la^2)*
           m^2 + 24*m^4) - 4*En^4*(-146052 + 102985*la + 38348*la^2 - 
          5692*la^3 - 86*la^4 + 54*(-851 + 129*la + 85*la^2)*m^2 + 
          6*(-207 + 2*la + 2*la^2)*m^4) + En^2*(-2981610 + 2534393*la + 
          1058141*la^2 - 229660*la^3 - 160*la^4 - 
          2*(372744 - 85781*la - 64620*la^2 + 560*la^3)*m^2 + 
          48*(-246 + 7*la + 7*la^2)*m^4)) + 
      J^4*(45144*En^8 - 36*En^6*(368253 - 19156*la + 20552*m^2) + 
        12*En^4*(22124164 - 2793459*la - 252763*la^2 - 6*(-300036 + 2615*la)*
           m^2 + 6510*m^4) - 6*En^2*(168217630 - 30725616*la - 5191104*la^2 + 
          206920*la^3 + (13979011 - 323352*la - 102180*la^2)*m^2 + 
          52530*m^4) - 7*(-95919111 + 22386122*la + 6192692*la^2 - 
          428124*la^3 + 888*la^4 - 12*(483519 - 20738*la - 10745*la^2 + 
            30*la^3)*m^2 + 40*(-405 + la + la^2)*m^4))*M^2 + 
      6*J^2*(202141368*En^4 - 12*En^2*(114129645 - 3907772*la + 
          1894735*m^2) + 7*(171134201 - 11065388*la - 991148*la^2 - 
          8*(-401683 + 1971*la)*m^2 + 1712*m^4))*M^4 + 3157433244*M^6)*
     rp[t]^12 + J^8*(J^6*la*(210029 + 151445*la - 57708*la^2 + 876*la^3 - 
        35*(-595 - 583*la + 12*la^2)*m^2 + 56*(1 + la)*m^4 - 
        16*En^6*(1 + la)*(25 + 3*la - 2*la^2 + (5 + la)*m^2) + 
        8*En^4*(3369 + 2501*la - 911*la^2 - 43*la^3 + 765*(1 + la)*m^2 + 
          6*(1 + la)*m^4) - 2*En^2*(94771 + 66091*la - 28720*la^2 - 40*la^3 - 
          5*(-3391 - 3335*la + 56*la^2)*m^2 + 84*(1 + la)*m^4)) + 
      J^4*(-144*En^8*(181 + 57*la + 8*m^2) + 48*En^6*(116801 - 17592*la - 
          3193*la^2 + (17777 + 22*la)*m^2 + 190*m^4) - 
        12*En^4*(7795016 - 2163314*la - 442590*la^2 + 23080*la^3 + 
          (1238967 - 44402*la - 17180*la^2)*m^2 + 11850*m^4) + 
        21*(-8428827 + 3680288*la + 1434566*la^2 - 180174*la^3 + 888*la^4 + 
          (-771063 + 80206*la + 55330*la^2 - 360*la^3)*m^2 + 
          40*(-84 + la + la^2)*m^4) - 2*En^2*(-152119470 + 54235005*la + 
          15056937*la^2 - 1511048*la^3 - 224*la^4 + 
          (-21271134 + 1373668*la + 769400*la^2 - 2240*la^3)*m^2 + 
          5*(-29187 + 112*la + 112*la^2)*m^4))*M^2 + 
      6*J^2*(24813396*En^6 - 12*En^4*(42934653 - 1964996*la + 1291175*m^2) - 
        7*(163944047 - 20330020*la - 3735908*la^2 + 103160*la^3 + 
          (5350739 - 85588*la - 26620*la^2)*m^2 + 6464*m^4) + 
        2*En^2*(935287694 - 81206791*la - 6737295*la^2 + 
          (37048490 - 214200*la)*m^2 + 41210*m^4))*M^4 + 
      36*(312264297*En^2 - 28*(14291987 - 352500*la + 94304*m^2))*M^6)*
     rp[t]^13 + J^6*M*(J^6*(192*En^8*(1 + la)*(7 - 3*la + 2*m^2) - 
        32*En^6*(9342 - 4196*la - 2018*la^2 + 15*la^3 + 
          (3282 - 58*la - 115*la^2)*m^2 + 120*m^4) - 
        4*En^4*(-1310742 + 910316*la + 340105*la^2 - 49276*la^3 - 588*la^4 + 
          18*(-21714 + 3131*la + 2170*la^2)*m^2 + 30*(-267 + 2*la + 2*la^2)*
           m^4) - 7*(2*(-652518 + 664949*la + 352643*la^2 - 74418*la^3 + 
            666*la^4) + (-179901 + 55724*la + 46760*la^2 - 540*la^3)*m^2 + 
          30*(-39 + 2*la + 2*la^2)*m^4) + 4*En^2*(-4159020 + 3451775*la + 
          1475378*la^2 - 298144*la^3 - 112*la^4 - 
          5*(194226 - 40710*la - 31477*la^2 + 224*la^3)*m^2 + 
          10*(-1119 + 28*la + 28*la^2)*m^4)) + 
      2*J^4*(451440*En^8 - 72*En^6*(784983 - 41474*la + 40280*m^2) + 
        12*En^4*(57498589 - 7020913*la - 634395*la^2 - 5*(-873503 + 5906*la)*
           m^2 + 10280*m^4) - 12*En^2*(151765957 - 26498669*la - 
          4678085*la^2 + 169540*la^3 - 5*(-2217377 + 44543*la + 14975*la^2)*
           m^2 + 28965*m^4) - 7*(6*(-21308394 + 4770481*la + 1364057*la^2 - 
            88340*la^3 + 180*la^4) + (-6580827 + 263344*la + 139540*la^2 - 
            360*la^3)*m^2 + 16*(-855 + 2*la + 2*la^2)*m^4))*M^2 + 
      6*J^2*(657154800*En^4 - 24*En^2*(139738837 - 4399030*la + 
          1873351*m^2) + 7*(318548437 - 19415960*la - 1810928*la^2 - 
          800*(-6002 + 27*la)*m^2 + 1792*m^4))*M^4 + 4043656512*M^6)*
     rp[t]^14 + J^6*(J^6*la*(-32*En^6*(1 + la)*(118 + 11*la - 7*la^2 + 
          (23 + 3*la)*m^2) + 12*En^4*(10350 + 7737*la - 2711*la^2 - 98*la^3 + 
          2320*(1 + la)*m^2 + 10*(1 + la)*m^4) + 
        7*(57665 + 42437*la - 15006*la^2 + 222*la^3 + 
          (4935 + 4845*la - 90*la^2)*m^2 + 10*(1 + la)*m^4) - 
        4*En^2*(130750 + 92992*la - 37786*la^2 - 28*la^3 - 
          5*(-4223 - 4167*la + 56*la^2)*m^2 + 70*(1 + la)*m^4)) + 
      J^4*(-288*En^8*(863 + 243*la + 40*m^2) + 
        24*En^6*(976901 - 154946*la - 26704*la^2 + 35*(3875 + 2*la)*m^2 + 
          1080*m^4) - 4*En^4*(62412489 - 16630312*la - 3338700*la^2 + 
          167080*la^3 - 15*(-626473 + 18166*la + 7740*la^2)*m^2 + 
          57000*m^4) + 21*(-11285455 + 4753198*la + 1911910*la^2 - 
          223620*la^3 + 1080*la^4 + (-882865 + 85990*la + 60250*la^2 - 
            360*la^3)*m^2 + 32*(-89 + la + la^2)*m^4) - 
        2*En^2*(-275818173 + 95188983*la + 27534843*la^2 - 2497684*la^3 - 
          112*la^4 - 10*(3447555 - 195703*la - 114405*la^2 + 280*la^3)*m^2 + 
          5*(-32427 + 112*la + 112*la^2)*m^4))*M^2 + 
      4*J^2*(93860046*En^6 - 90*En^4*(14003484 - 615622*la + 373469*m^2) - 
        7*(230088261 - 26964112*la - 5153254*la^2 + 132810*la^3 + 
          (6039543 - 89336*la - 28640*la^2)*m^2 + 5088*m^4) + 
        3*En^2*(1155148794 - 92786973*la - 8196253*la^2 - 
          24*(-1549687 + 7567*la)*m^2 + 26886*m^4))*M^4 + 
      72*(-128835168 + 130738383*En^2 + 2954352*la - 631252*m^2)*M^6)*
     rp[t]^15 + 2*J^4*M*(J^6*(960*En^8*(1 + la)*(7 - 3*la + 2*m^2) - 
        24*En^6*(19550 - 12466*la - 6336*la^2 + 105*la^3 - 
          5*(-1370 + 62*la + 79*la^2)*m^2 + 240*m^4) - 
        4*En^4*(-1863630 + 1174515*la + 420141*la^2 - 61168*la^3 - 574*la^4 + 
          50*(-10473 + 1258*la + 919*la^2)*m^2 + 20*(-327 + 2*la + 2*la^2)*
           m^4) - 21*(-292666 + 290201*la + 158413*la^2 - 30880*la^3 + 
          270*la^4 + (-34732 + 10098*la + 8550*la^2 - 90*la^3)*m^2 + 
          2*(-83 + 4*la + 4*la^2)*m^4) + En^2*(-15115020 + 12381905*la + 
          5493323*la^2 - 996394*la^3 - 112*la^4 - 
          10*(323718 - 60167*la - 47665*la^2 + 280*la^3)*m^2 + 
          40*(-627 + 14*la + 14*la^2)*m^4)) + 
      J^4*(1479816*En^8 - 36*En^6*(4384467 - 217780*la + 210960*m^2) + 
        360*En^4*(3115283 - 377063*la - 35204*la^2 + (218809 - 1097*la)*m^2 + 
          297*m^4) - 6*En^2*(378260932 - 61676949*la - 11581551*la^2 + 
          370300*la^3 + (22674061 - 391616*la - 139700*la^2)*m^2 + 
          37998*m^4) - 7*(-120356775 + 25551040*la + 7587862*la^2 - 
          455916*la^3 + 912*la^4 - 4*(1248471 - 46432*la - 25150*la^2 + 
            60*la^3)*m^2 + 16*(-450 + la + la^2)*m^4))*M^2 + 
      36*J^2*(120418183 + 60911700*En^4 - 6832104*la - 666768*la^2 + 
        (1347812 - 5544*la)*m^2 + 312*m^4 + En^2*(-237124902 + 6630136*la - 
          2317874*m^2))*M^4 + 860821614*M^6)*rp[t]^16 + 
    J^4*(J^6*la*(-48*En^6*(1 + la)*(715 + 11*la - 14*la^2 + 
          5*(23 + la)*m^2) + 7*(3*(25709 + 19369*la - 6250*la^2 + 90*la^3) + 
          (5455 + 5365*la - 90*la^2)*m^2 + 8*(1 + la)*m^4) + 
        8*En^4*(36075 + 25858*la - 10504*la^2 - 287*la^3 + 
          8725*(1 + la)*m^2 + 20*(1 + la)*m^4) - 
        2*En^2*(482005 + 353947*la - 128086*la^2 - 28*la^3 + 
          (65325 + 64625*la - 700*la^2)*m^2 + 140*(1 + la)*m^4)) + 
      J^4*(144*En^8*(9857 - 1495*la + 600*m^2) + 
        48*En^6*(1785312 - 212100*la - 25985*la^2 + 10*(20499 + 2*la)*m^2 + 
          700*m^4) - 20*En^4*(2*(9943932 - 2769148*la - 566310*la^2 + 
            25075*la^3) - 9*(-330479 + 7346*la + 3420*la^2)*m^2 + 9990*m^4) + 
        21*(-10700665 + 4295346*la + 1791204*la^2 - 192846*la^3 + 912*la^4 + 
          (-676753 + 61436*la + 43700*la^2 - 240*la^3)*m^2 + 
          16*(-94 + la + la^2)*m^4) - 2*En^2*(-346532241 + 113447907*la + 
          34861911*la^2 - 2751476*la^3 + 112*la^4 - 
          2*(18035673 - 889050*la - 540800*la^2 + 1120*la^3)*m^2 + 
          3*(-35667 + 112*la + 112*la^2)*m^4))*M^2 + 
      4*J^2*(-1051657191 + 122516874*En^6 + 114891952*la + 22946320*la^2 - 
        547320*la^3 + (-20494791 + 279632*la + 92360*la^2)*m^2 - 10656*m^4 - 
        18*En^4*(78758631 - 3117788*la + 1623309*m^2) + 
        6*En^2*(497671271 - 35588844*la - 3404680*la^2 + 
          (11680226 - 47964*la)*m^2 + 4841*m^4))*M^4 + 
      36*(-110501487 + 146116014*En^2 + 2325192*la - 352495*m^2)*M^6)*
     rp[t]^17 + J^2*M*(J^6*(2880*En^8*(-367 + 14*la - 12*la^2 + 
          (-38 + 4*la)*m^2) - 320*En^6*(26466 - 4523*la - 1298*la^2 + 
          51*la^3 + (4266 - 130*la - 145*la^2)*m^2 + 48*m^4) - 
        7*(2*(-839667 + 800281*la + 451561*la^2 - 80142*la^3 + 684*la^4) + 
          (-161727 + 43888*la + 37480*la^2 - 360*la^3)*m^2 + 
          24*(-22 + la + la^2)*m^4) - 20*En^4*(-1110474 + 807746*la + 
          288127*la^2 - 37760*la^3 - 280*la^4 + 6*(-59526 + 5509*la + 
            4210*la^2)*m^2 + 6*(-387 + 2*la + 2*la^2)*m^4) + 
        4*En^2*(-9529560 + 7631594*la + 3588209*la^2 - 554848*la^3 + 
          56*la^4 + (-1744434 + 282916*la + 229065*la^2 - 1120*la^3)*m^2 + 
          6*(-1389 + 28*la + 28*la^2)*m^4)) + 
      2*J^4*(555132249 + 16950816*En^8 - 110095348*la - 34116532*la^2 + 
        1883088*la^3 - 3696*la^4 + 30*(569805 - 19640*la - 10868*la^2 + 
          24*la^3)*m^2 - 16*(-945 + 2*la + 2*la^2)*m^4 - 
        144*En^6*(1228347 - 88935*la + 80330*m^2) + 
        12*En^4*(105533009 - 11892635*la - 1219789*la^2 + 
          (5920695 - 21774*la)*m^2 + 4044*m^4) - 
        6*En^2*(331971185 - 48396448*la - 9842236*la^2 + 269360*la^3 + 
          (14502953 - 214584*la - 80940*la^2)*m^2 + 13746*m^4))*M^2 + 
      3*J^2*(1249990483 + 1132058880*En^4 - 65083648*la - 6677464*la^2 + 
        (9090244 - 34128*la)*m^2 + 976*m^4 - 48*En^2*(67565728 - 1610788*la + 
          408811*m^2))*M^4 + 440295912*M^6)*rp[t]^18 + 
    J^2*(J^6*la*(960*En^8*(16 + 17*la + la^2) + 160*En^6*(1 + la)*
         (15 + 6*la + 7*la^2 - 2*(51 + la)*m^2) + 
        7*(72871 + 56359*la - 16284*la^2 + 228*la^3 + 
          (4015 + 3955*la - 60*la^2)*m^2 + 4*(1 + la)*m^4) + 
        20*En^4*(23394 + 16789*la - 6745*la^2 - 140*la^3 + 
          5040*(1 + la)*m^2 + 6*(1 + la)*m^4) - 
        4*En^2*(315193 + 242827*la - 72352*la^2 + 14*la^3 - 
          40*(-800 - 793*la + 7*la^2)*m^2 + 42*(1 + la)*m^4)) - 
      J^4*(829440*En^10 + 2880*En^8*(11081 + 53*la + 198*m^2) - 
        24*En^6*(2625161 - 764150*la - 80600*la^2 + (715345 - 10*la)*m^2 + 
          860*m^4) + 12*En^4*(36628363 - 10209700*la - 2300000*la^2 + 
          79680*la^3 + (4689963 - 79250*la - 39860*la^2)*m^2 + 7620*m^4) - 
        3*(-49912773 + 18780444*la + 8155332*la^2 - 798648*la^3 + 3696*la^4 + 
          (-2340663 + 197396*la + 142460*la^2 - 720*la^3)*m^2 + 
          32*(-99 + la + la^2)*m^4) + 2*En^2*(-310940484 + 91805043*la + 
          30540783*la^2 - 2018632*la^3 + 224*la^4 - 
          2*(11796843 - 502774*la - 317120*la^2 + 560*la^3)*m^2 + 
          (-38907 + 112*la + 112*la^2)*m^4))*M^2 + 
      3*J^2*(-612730703 + 169340472*En^6 + 61435436*la + 12880656*la^2 - 
        282020*la^3 + (-7736801 + 97212*la + 33060*la^2)*m^2 - 1856*m^4 - 
        24*En^4*(63601933 - 1980688*la + 773457*m^2) + 
        8*En^2*(-2*(-145283521 + 8826147*la + 927769*la^2) + 
          (4179980 - 14436*la)*m^2 + 743*m^4))*M^4 + 
      54*(-19009710 + 32884341*En^2 + 362048*la - 29174*m^2)*M^6)*rp[t]^19 + 
    2*M*(J^6*(3973401 - 3572383*la - 2091349*la^2 + 332976*la^3 - 2772*la^4 + 
        17280*En^10*(22 + la) + (283239 - 71456*la - 61520*la^2 + 540*la^3)*
         m^2 - 6*(-93 + 4*la + 4*la^2)*m^4 + 960*En^8*(4576 + 78*la + 
          23*la^2 + (50 + 8*la)*m^2) - 40*En^6*(16542 - 36110*la - 
          6638*la^2 + 327*la^3 + (37662 - 670*la - 709*la^2)*m^2 + 120*m^4) - 
        4*En^4*(-2700036 + 2415887*la + 956794*la^2 - 92332*la^3 - 546*la^4 + 
          18*(-50091 + 3539*la + 2805*la^2)*m^2 + 6*(-447 + 2*la + 2*la^2)*
           m^4) + En^2*(-17569290 + 12946511*la + 6569531*la^2 - 
          822892*la^3 + 224*la^4 + (-2347536 + 330294*la + 272600*la^2 - 
            1120*la^3)*m^2 + 16*(-381 + 7*la + 7*la^2)*m^4)) - 
      J^4*(14064408*En^8 + 3*(-81889734 + 14908703*la + 4841304*la^2 - 
          243134*la^3 + 468*la^4) - 2*(2441106 - 77842*la - 43975*la^2 + 
          90*la^3)*m^2 + 4*(-495 + la + la^2)*m^4 + 
        36*En^6*(4707459 - 344092*la + 242728*m^2) - 
        12*En^4*(90114296 - 7903021*la - 938837*la^2 + (2911076 - 7934*la)*
           m^2 + 754*m^4) + 6*En^2*(2*(99968387 - 12327086*la - 
            2753402*la^2 + 62900*la^3) + (5279473 - 67000*la - 26620*la^2)*
           m^2 + 2118*m^4))*M^2 + 3*J^2*(163013121 + 276622992*En^4 - 
        7674164*la - 831300*la^2 - 24*(-23663 + 81*la)*m^2 - 
        24*En^2*(23388514 - 457412*la + 62805*m^2))*M^4 + 25672950*M^6)*
     rp[t]^20 + (J^6*la*(336487 + 267631*la - 67932*la^2 + 924*la^3 + 
        3840*En^8*(-7 - 6*la + la^2) - 45*(-295 - 291*la + 4*la^2)*m^2 + 
        8*(1 + la)*m^4 + 80*En^6*(1 + la)*(953 + 47*la + 14*la^2 - 
          (275 + 3*la)*m^2) + 24*En^4*(28253 + 22647*la - 5697*la^2 - 
          91*la^3 + 3485*(1 + la)*m^2 + 2*(1 + la)*m^4) - 
        2*En^2*(586837 + 477973*la - 108808*la^2 + 56*la^3 - 
          5*(-7745 - 7689*la + 56*la^2)*m^2 + 28*(1 + la)*m^4)) + 
      J^4*(8236800*En^10 - 144*En^8*(-243973 - 1785*la + 10000*m^2) + 
        48*En^6*(579475 - 421288*la - 57755*la^2 + (294017 - 10*la)*m^2 + 
          102*m^4) - 4*En^4*(10*(10181106 - 2173061*la - 571947*la^2 + 
            14120*la^3) + (7192695 - 93906*la - 50460*la^2)*m^2 + 4290*m^4) + 
        3*(-22448369 + 7760510*la + 3522263*la^2 - 310197*la^3 + 1404*la^4 + 
          (-675473 + 52811*la + 38645*la^2 - 180*la^3)*m^2 + 
          4*(-104 + la + la^2)*m^4) - 2*En^2*(-195299580 + 48533943*la + 
          17699931*la^2 - 950600*la^3 + 160*la^4 + 
          (-8767434 + 323516*la + 210840*la^2 - 320*la^3)*m^2 + 
          (-6021 + 16*la + 16*la^2)*m^4))*M^2 + 
      3*J^2*(-161909691 + 163130616*En^6 + 14657452*la + 3238828*la^2 - 
        64600*la^3 + 5*(-194779 + 2252*la + 788*la^2)*m^2 - 
        24*En^4*(33212887 - 717676*la + 154693*m^2) - 
        2*En^2*(-415153560 + 20520283*la + 2393483*la^2 + 
          6*(-433837 + 1264*la)*m^2))*M^4 + 
      18*(-6721388 + 15206223*En^2 + 114320*la)*M^6)*rp[t]^21 + 
    M*(J^4*(23040*En^10*(-216 + la) + 960*En^8*(-8323 + 60*la + 79*la^2 + 
          2*(278 + 5*la)*m^2) - 96*En^6*(-99518 - 44056*la - 13730*la^2 + 
          237*la^3 + (29006 - 342*la - 353*la^2)*m^2 + 24*m^4) - 
        4*En^4*(-5813154 + 3838980*la + 1790751*la^2 - 111604*la^3 - 
          532*la^4 + (-968988 + 53402*la + 43580*la^2)*m^2 + 
          2*(-507 + 2*la + 2*la^2)*m^4) - 
        3*(2*(-608708 + 504179*la + 307035*la^2 - 43253*la^3 + 351*la^4) - 
          10*(5518 - 1291*la - 1120*la^2 + 9*la^3)*m^2 + 
          (-49 + 2*la + 2*la^2)*m^4) + 4*En^2*(-5869680 + 3625679*la + 
          2001908*la^2 - 195760*la^3 + 80*la^4 + 
          (-447414 + 54686*la + 45895*la^2 - 160*la^3)*m^2 + 
          (-474 + 8*la + 8*la^2)*m^4)) - J^2*(-131963319 + 4944672*En^8 + 
        21655438*la + 7394398*la^2 - 334936*la^3 + 632*la^4 + 
        (-1239747 + 36544*la + 21060*la^2 - 40*la^3)*m^2 + 
        144*En^6*(3151783 - 89238*la + 35056*m^2) + 
        72*En^4*(-17217913 + 1003873*la + 141571*la^2 + (-199087 + 410*la)*
           m^2) - 6*En^2*(-148760995 + 14767502*la + 3650978*la^2 - 
          68440*la^3 + (-1669537 + 18254*la + 7610*la^2)*m^2))*M^2 + 
      9*(12971641 + 41387184*En^4 - 544280*la - 62480*la^2 + 
        8*En^2*(-7413575 + 115554*la))*M^4)*rp[t]^22 + 
    (J^4*la*(1920*En^8*(-34 - 31*la + 3*la^2) + 
        3*(49403 + 40425*la - 8861*la^2 + 117*la^3) - 
        5*(-730 - 721*la + 9*la^2)*m^2 + (1 + la)*m^4 - 
        96*En^6*(1 + la)*(840 - 47*la - 7*la^2 + (145 + la)*m^2) + 
        4*En^4*(184998 + 163957*la - 21307*la^2 - 266*la^3 + 
          9280*(1 + la)*m^2 + 2*(1 + la)*m^4) - 
        4*En^2*(2*(91994 + 78869*la - 13115*la^2 + 10*la^3) + 
          (6615 + 6575*la - 40*la^2)*m^2 + 2*(1 + la)*m^4)) + 
      J^2*(-18472212 - 14745600*En^10 + 5746013*la + 2732861*la^2 - 
        214254*la^3 + 948*la^4 + (-259908 + 18821*la + 13955*la^2 - 60*la^3)*
         m^2 - 288*En^8*(-95889 - 1429*la + 2684*m^2) - 
        24*En^6*(-5073943 + 501334*la + 97136*la^2 + 3*(-59613 + 2*la)*m^2) + 
        4*En^4*(-67451703 + 8910752*la + 2780220*la^2 - 47240*la^3 + 
          3*(-507087 + 5242*la + 2980*la^2)*m^2) + 
        2*En^2*(76795260 - 15153930*la - 6083382*la^2 + 260638*la^3 - 
          56*la^4 + (1411944 - 45333*la - 30425*la^2 + 40*la^3)*m^2))*M^2 + 
      6*(-9813272 + 35536860*En^6 + 789800*la + 184510*la^2 - 3330*la^3 + 
        6*En^4*(-15925131 + 226262*la) + En^2*(68092278 - 2658517*la - 
          345725*la^2))*M^4)*rp[t]^23 + 
    2*M*(J^2*(515376 - 28800*En^10*(-118 + la) - 383693*la - 243356*la^2 + 
        29976*la^3 - 237*la^4 + (10746 - 2329*la - 2035*la^2 + 15*la^3)*m^2 + 
        192*En^8*(-26919 - 538*la - 244*la^2 + 6*(141 + la)*m^2) + 
        24*En^6*(-160158 + 68814*la + 31732*la^2 - 215*la^3 + 
          3*(-6266 + 54*la + 55*la^2)*m^2) - 4*En^4*(-2526426 + 889813*la + 
          483965*la^2 - 19048*la^3 - 74*la^4 + 6*(-17721 + 784*la + 655*la^2)*
           m^2) + En^2*(-5007240 + 2412770*la + 1446251*la^2 - 108403*la^3 + 
          56*la^4 - 5*(29466 - 3146*la - 2679*la^2 + 8*la^3)*m^2)) + 
      (8154006 + 27469368*En^8 - 1185910*la - 426807*la^2 + 17306*la^3 - 
        32*la^4 + 36*En^6*(-3365133 + 38108*la) - 
        6*En^4*(-26997488 + 1001641*la + 167715*la^2) - 
        3*En^2*(25482028 - 1976601*la - 541715*la^2 + 8260*la^3))*M^2)*
     rp[t]^24 + (J^2*la*(1 + la)*(39275 - 6246*la + 79*la^2 + 
        3840*En^8*(3 + la) - 5*(-89 + la)*m^2 + 
        8*En^4*(56565 - 3697*la - 37*la^2 + 855*m^2) - 
        16*En^6*(14291 - 153*la - 14*la^2 + (211 + la)*m^2) + 
        En^2*(-274730 + 29442*la - 28*la^2 + 10*(-391 + 2*la)*m^2)) + 
      (-2340192 + 5218560*En^10 + 642871*la + 320812*la^2 - 22203*la^3 + 
        96*la^4 + 144*En^8*(-285217 + 1079*la) - 
        48*En^6*(-1868420 + 60988*la + 15627*la^2) - 
        12*En^4*(6611753 - 532259*la - 194075*la^2 + 2285*la^3) - 
        4*En^2*(-6962439 + 1059969*la + 466969*la^2 - 15847*la^3 + 4*la^4))*
       M^2)*rp[t]^25 - 2*(23040*En^10*(43 + la) + 
      480*En^8*(-7821 + 154*la + 100*la^2) + 
      96*En^6*(56070 - 5169*la - 2954*la^2 + 10*la^3) - 
      8*En^2*(-122715 + 45000*la + 29026*la^2 - 1663*la^3 + la^4) + 
      3*(-22500 + 14743*la + 9736*la^2 - 1039*la^3 + 8*la^4) - 
      2*En^4*(1766790 - 357485*la - 219217*la^2 + 5626*la^3 + 18*la^4))*M*
     rp[t]^26 + 4*(-1 + En^2)*la*(-1185 - 1022*la + 161*la^2 - 2*la^3 + 
      240*En^6*(36 + 37*la + la^2) + 8*En^4*(-2205 - 2159*la + 47*la^2 + 
        la^3) - En^2*(-10200 - 9449*la + 752*la^2 + la^3))*rp[t]^27))/
  (En^2*la*(1 + la)*rp[t]^18*(J^2 + rp[t]^2)^10) - 
 (8*J*mu*Pi*YPhiBar*(-2*M + rp[t])^2*(9003456*J^20*M^7 + 
    72*J^20*(-281771 + 1409*la)*M^6*rp[t] + 
    12*J^18*M^5*(J^2*(1553011 - 27477*la - 10824*la^2) + 7451136*M^2)*
     rp[t]^2 + 4*J^18*M^4*(J^2*(-2224651 + 100395*la + 61061*la^2 - 
        665*la^3) + 54*(-930729 + 80356*En^2 + 6159*la)*M^2)*rp[t]^3 + 
    2*J^16*M^3*(J^4*(1158846 - 121013*la - 89461*la^2 + 2202*la^3 - 4*la^4) - 
      3*J^2*(-30701002 + 651396*la + 214302*la^2 + En^2*(5113740 + 54021*la))*
       M^2 + 199783584*M^4)*rp[t]^4 + 
    J^16*M^2*(J^4*(-310076 + 77061*la + 63468*la^2 - 2681*la^3 + 12*la^4) - 
      6*J^2*(14616065 - 744631*la - 401434*la^2 + 5022*la^3 + 
        En^2*(-3500014 - 58446*la + 29529*la^2))*M^2 + 
      72*(-12448750 + 2266659*En^2 + 104126*la)*M^4)*rp[t]^5 + 
    J^14*M*(-6*J^6*(-2748 + 2054*la + 1803*la^2 - 118*la^3 + la^4) + 
      J^4*(22761251 - 2574122*la - 1756209*la^2 + 49756*la^3 - 96*la^4 + 
        6*En^2*(-1151049 - 10562*la + 41923*la^2 + 300*la^3))*M^2 + 
      6*J^2*(136507448 + 1484460*En^4 - 3419966*la - 954310*la^2 - 
        3*En^2*(16011280 + 128011*la))*M^4 + 1058061312*M^6)*rp[t]^6 + 
    J^14*(J^6*la*(770 + 701*la - 68*la^2 + la^3) + 
      J^4*(-3032518 + 791543*la + 619400*la^2 - 30181*la^3 + 144*la^4 + 
        2*En^2*(540154 - 23677*la - 64591*la^2 - 1172*la^3 + 24*la^4))*M^2 - 
      6*J^2*(64780401 - 3713899*la - 1780356*la^2 + 25384*la^3 + 
        6*En^4*(315049 + 9346*la) + 3*En^2*(-10944683 - 123440*la + 
          92235*la^2))*M^4 + 432*(-5480556 + 1585715*En^2 + 56228*la)*M^6)*
     rp[t]^7 + J^12*M*(2*J^6*(80226 - 61507*la - 52412*la^2 + 3965*la^3 - 
        36*la^4 + En^2*(-31662 + 10006*la + 14099*la^2 + 487*la^3 - 
          24*la^4)) + J^4*(100497033 - 12330782*la - 7749735*la^2 + 
        251040*la^3 - 512*la^4 + En^4*(5122884 + 306456*la - 63384*la^2) + 
        6*En^2*(-10781000 - 7130*la + 391443*la^2 + 1782*la^3))*M^2 + 
      24*J^2*(89883532 + 3294972*En^4 - 2628976*la - 629324*la^2 - 
        9*En^2*(5595129 + 28667*la))*M^4 + 1838567808*M^6)*rp[t]^8 + 
    2*J^12*(3*J^6*la*(1 + la)*(2*(625 - 64*la + la^2) + 
        En^2*(-340 - 23*la + 2*la^2)) + 
      J^4*(10*En^4*(-47832 - 3490*la + 3006*la^2 + 109*la^3) + 
        3*(-2221369 + 610953*la + 452696*la^2 - 25314*la^3 + 128*la^4) + 
        En^2*(5049480 - 280527*la - 600475*la^2 - 7583*la^3 + 224*la^4))*
       M^2 + 6*J^2*(-85013230 + 114018*En^6 + 5473194*la + 2337948*la^2 - 
        37756*la^3 - 6*En^4*(1401178 + 36355*la) + 
        En^2*(68755315 + 354256*la - 578544*la^2))*M^4 + 
      72*(-28495015 + 11710755*En^2 + 351173*la)*M^6)*rp[t]^9 + 
    J^10*M*(J^6*(701307 - 553032*la - 456039*la^2 + 39756*la^3 - 384*la^4 + 
        En^2*(-590652 + 195352*la + 260794*la^2 + 6782*la^3 - 448*la^4) + 
        4*En^4*(15153 - 718*la - 4495*la^2 - 408*la^3 + 6*la^4)) - 
      2*J^4*(900*En^6*(591 + 46*la) + 12*En^4*(-1902070 - 99263*la + 
          23510*la^2) - 3*En^2*(-45078387 + 409686*la + 1630700*la^2 + 
          2588*la^3) + 2*(-65666853 + 8761702*la + 5060895*la^2 - 
          186492*la^3 + 400*la^4))*M^2 + 24*J^2*(155286575 + 12941316*En^4 - 
        5258201*la - 1088920*la^2 - 3*En^2*(41281901 + 77649*la))*M^4 + 
      2190633984*M^6)*rp[t]^10 + 
    2*J^10*(2*J^6*la*(8206 + 7248*la - 942*la^2 + 16*la^3 + 
        En^4*(345 + 417*la + 69*la^2 - 3*la^3) + 
        En^2*(-4745 - 5007*la - 234*la^2 + 28*la^3)) + 
      J^4*(-36*En^6*(-3412 - 621*la + 65*la^2) + 
        6*En^4*(-711575 - 43926*la + 44519*la^2 + 1428*la^3) + 
        3*(-5776437 + 1680429*la + 1174396*la^2 - 75094*la^3 + 400*la^4) + 
        En^2*(21071433 - 1458016*la - 2489561*la^2 - 15448*la^3 + 912*la^4))*
       M^2 + 6*J^2*(-146314356 + 979074*En^6 + 10566424*la + 4026610*la^2 - 
        73290*la^3 - 6*En^4*(5517718 + 120081*la) + 
        En^2*(168876643 - 313936*la - 1418252*la^2))*M^4 + 
      72*(-33855427 + 18657513*En^2 + 493685*la)*M^6)*rp[t]^11 + 
    J^8*M*(J^6*(8*En^6*(-1986 - 521*la + 299*la^2 + 49*la^3) + 
        12*En^4*(45157 - 2726*la - 13267*la^2 - 1094*la^3 + 20*la^4) - 
        3*(-604101 + 491650*la + 390863*la^2 - 39208*la^3 + 400*la^4) - 
        2*En^2*(1229553 - 428213*la - 537523*la^2 - 8525*la^3 + 912*la^4)) + 
      2*J^4*(15552*En^8 - 36*En^6*(127981 + 9104*la) - 
        12*En^4*(-7510659 - 326101*la + 92552*la^2) - 
        3*En^2*(110533237 - 2256342*la - 3981540*la^2 + 7228*la^3) - 
        2*(-112493481 + 16369337*la + 8665104*la^2 - 361830*la^3 + 812*la^4))*
       M^2 + 12*J^2*(367734746 + 58864968*En^4 - 14326724*la - 2582734*la^2 + 
        9*En^2*(-43808526 + 79849*la))*M^4 + 1812488832*M^6)*rp[t]^12 + 
    2*J^8*(J^6*la*(-4*En^6*(25 + 38*la + 13*la^2) + 
        10*En^4*(614 + 733*la + 113*la^2 - 6*la^3) + 
        10*(4249 + 3684*la - 555*la^2 + 10*la^3) + 
        3*En^2*(-13144 - 13677*la - 457*la^2 + 76*la^3)) + 
      J^4*(-72*En^8*(68 + 29*la) - 12*En^6*(-89404 - 15110*la + 1751*la^2) + 
        6*En^4*(-2816636 - 135678*la + 175183*la^2 + 4832*la^3) + 
        3*(-9838869 + 3041236*la + 1995504*la^2 - 145565*la^3 + 812*la^4) + 
        En^2*(51561372 - 4396150*la - 6045211*la^2 + 8656*la^3 + 2128*la^4))*
       M^2 + 6*J^2*(-172518123 + 3695922*En^6 + 13972069*la + 4751502*la^2 - 
        97090*la^3 - 6*En^4*(12593678 + 215155*la) - 
        3*En^2*(-89481751 + 897590*la + 749727*la^2))*M^4 + 
      432*(-4654311 + 3331769*En^2 + 79411*la)*M^6)*rp[t]^13 + 
    J^6*M*(J^6*(96*En^8*(7 + 8*la + la^2) + 24*En^6*(-5846 - 1451*la + 
          881*la^2 + 131*la^3) + En^2*(-6004854 + 2214750*la + 2591374*la^2 + 
          8354*la^3 - 4256*la^4) + 4*En^4*(536325 - 42780*la - 156889*la^2 - 
          11696*la^3 + 248*la^4) - 3*(-1021581 + 861720*la + 657489*la^2 - 
          75880*la^3 + 812*la^4)) + 2*J^4*(263901249 + 139968*En^8 - 
        42028046*la - 20319243*la^2 + 958692*la^3 - 2240*la^4 - 
        36*En^6*(487573 + 30214*la) + En^4*(206733360 + 7018356*la - 
          2526984*la^2) - 3*En^2*(175341001 - 5955338*la - 6289770*la^2 + 
          35724*la^3))*M^2 + 12*J^2*(302193856 + 85039380*En^4 - 
        13484878*la - 2125886*la^2 + 3*En^2*(-140742490 + 853079*la))*M^4 + 
      1028256768*M^6)*rp[t]^14 + 
    2*J^6*(J^6*la*(En^4*(24694 + 29250*la + 4308*la^2 - 248*la^3) + 
        4*En^6*(-211 - 312*la - 99*la^2 + 2*la^3) + 
        7*(10292 + 8733*la - 1530*la^2 + 29*la^3) + 
        2*En^2*(-47968 - 49005*la - 771*la^2 + 266*la^3)) + 
      J^4*(-34395045 + 11357802*la + 6959820*la^2 - 578403*la^3 + 3360*la^4 - 
        8424*En^8*(5 + 2*la) + En^6*(4053996 + 583212*la - 85092*la^2) + 
        2*En^4*(-19551327 - 691192*la + 1190409*la^2 + 25876*la^3) + 
        En^2*(81559545 - 8582334*la - 9493739*la^2 + 99368*la^3 + 3136*la^4))*
       M^2 + 6*J^2*(-141110815 + 8054274*En^6 + 12823413*la + 3890152*la^2 - 
        88956*la^3 - 30*En^4*(3645715 + 40887*la) + 
        En^2*(287124212 - 5634952*la - 2396817*la^2))*M^4 + 
      72*(-15790520 + 14443947*En^2 + 312568*la)*M^6)*rp[t]^15 + 
    J^4*M*(J^6*(3541311 - 3112686*la - 2266929*la^2 + 301308*la^3 - 
        3360*la^4 + 864*En^8*(7 + 8*la + la^2) + 
        24*En^6*(-19728 - 3290*la + 3805*la^2 + 507*la^3) + 
        En^2*(-9464976 + 3752444*la + 4033874*la^2 - 50858*la^3 - 
          6272*la^4) + 4*En^4*(1265295 - 107822*la - 352487*la^2 - 
          22446*la^3 + 564*la^4)) + 2*J^4*(214586523 + 516672*En^8 - 
        37558846*la - 16519377*la^2 + 878808*la^3 - 2128*la^4 - 
        36*En^6*(1110907 + 61928*la) - 60*En^4*(-4992928 - 108067*la + 
          61194*la^2) - 3*En^2*(187114569 - 9438858*la - 6681538*la^2 + 
          66560*la^3))*M^2 + 24*J^2*(40441860*En^4 + 
        3*En^2*(-50868305 + 551471*la) - 8*(-10635719 + 541853*la + 
          74950*la^2))*M^4 + 382802112*M^6)*rp[t]^16 + 
    2*J^4*(J^6*la*(En^4*(55506 + 65088*la + 9018*la^2 - 564*la^3) + 
        12*En^6*(-393 - 548*la - 151*la^2 + 4*la^3) + 
        14*(5969 + 4938*la - 1011*la^2 + 20*la^3) + 
        En^2*(-151094 - 150609*la + 1269*la^2 + 784*la^3)) + 
      J^4*(-27755169 + 9854109*la + 5604900*la^2 - 530418*la^3 + 3192*la^4 - 
        72*En^8*(173 + 431*la) - 12*En^6*(-856893 - 111856*la + 15119*la^2) + 
        30*En^4*(-1890026 - 33500*la + 114763*la^2 + 1582*la^3) + 
        En^2*(86699904 - 11346392*la - 10030381*la^2 + 209458*la^3 + 
          3024*la^4))*M^2 + 6*J^2*(-79048638 + 10304406*En^6 + 8070122*la + 
        2181788*la^2 - 55692*la^3 - 6*En^4*(17380822 + 57073*la) + 
        En^2*(207493937 - 6375560*la - 1718980*la^2))*M^4 + 
      36*(-11714911 + 13616010*En^2 + 267293*la)*M^6)*rp[t]^17 + 
    J^2*M*(J^6*(192*En^8*(-741 - 110*la + la^2) + 
        8*En^6*(-208632 - 35302*la + 22543*la^2 + 3113*la^3) + 
        60*En^4*(122713 - 12216*la - 33507*la^2 - 1540*la^3 + 52*la^4) - 
        3*(-942999 + 869588*la + 600523*la^2 - 92120*la^3 + 1064*la^4) - 
        2*En^2*(4999662 - 2181376*la - 2111369*la^2 + 67433*la^3 + 
          3024*la^4)) + 2*J^4*(2395008*En^8 - 36*En^6*(1389273 + 72922*la) + 
        En^4*(285356232 + 907068*la - 3529752*la^2) - 
        3*En^2*(135017345 - 9477810*la - 4784028*la^2 + 69548*la^3) - 
        2*(-59699517 + 11539798*la + 4596855*la^2 - 275340*la^3 + 688*la^4))*
       M^2 + 12*J^2*(62838199 + 50431464*En^4 - 3647017*la - 443648*la^2 + 
        6*En^2*(-24032207 + 385639*la))*M^4 + 84446208*M^6)*rp[t]^18 + 
    2*J^2*(2*J^6*la*(48*En^8*(11 + 12*la + la^2) + 
        10*En^4*(3938 + 4495*la + 518*la^2 - 39*la^3) + 
        7*(4795 + 3849*la - 927*la^2 + 19*la^3) + 
        En^6*(-3514 - 5732*la - 2158*la^2 + 60*la^3) + 
        En^2*(-80385 - 77607*la + 3156*la^2 + 378*la^3)) + 
      J^4*(-43200*En^10 - 144*En^8*(12421 + 1042*la) + 
        En^6*(12237864 + 1819308*la - 253452*la^2) + 
        30*En^4*(-1784909 + 26418*la + 110657*la^2 + 444*la^3) + 
        3*(-5101759 + 1962163*la + 1028756*la^2 - 110894*la^3 + 688*la^4) + 
        En^2*(62329395 - 10175312*la - 7154675*la^2 + 231752*la^3 + 
          1904*la^4))*M^2 + 6*J^2*(-29018535 + 8706582*En^6 + 3334139*la + 
        802143*la^2 - 22811*la^3 + 6*En^4*(-10956078 + 72445*la) - 
        9*En^2*(-10913845 + 469856*la + 89016*la^2))*M^4 + 
      36*(-2574215 + 3810810*En^2 + 67361*la)*M^6)*rp[t]^19 + 
    M*(J^6*(5760*En^10*(16 + la) + 192*En^8*(4381 + 290*la + 49*la^2) + 
        24*En^6*(-76846 - 22891*la + 9499*la^2 + 1129*la^3) + 
        4*En^4*(1694613 - 264318*la - 482015*la^2 - 9838*la^3 + 676*la^4) - 
        3*(-513639 + 501214*la + 325613*la^2 - 57832*la^3 + 688*la^4) - 
        2*En^2*(3566601 - 1752681*la - 1494457*la^2 + 81289*la^3 + 
          1904*la^4)) + 2*J^4*(43489836 + 171072*En^8 - 9335209*la - 
        3351495*la^2 + 225834*la^3 - 580*la^4 - 
        36*En^6*(1131547 + 36928*la) - 12*En^4*(-15135905 + 331137*la + 
          183920*la^2) - 3*En^2*(63991967 - 5931346*la - 2230980*la^2 + 
          42852*la^3))*M^2 + 6*J^2*(38296944*En^4 + 
        3*En^2*(-27045944 + 579049*la) - 2*(-13738425 + 907082*la + 
          97195*la^2))*M^4 + 8382528*M^6)*rp[t]^20 + 
    (2*J^6*la*(36862 + 28536*la - 8154*la^2 + 172*la^3 + 
        96*En^8*(-11 - 7*la + 4*la^2) + En^4*(77600 + 83874*la + 5598*la^2 - 
          676*la^3) + 4*En^6*(-1103 - 2454*la - 1311*la^2 + 40*la^3) + 
        En^2*(-116500 - 108189*la + 8787*la^2 + 476*la^3)) + 
      J^4*(835200*En^10 - 144*En^8*(-8642 + 3571*la) - 
        24*En^6*(-714654 - 82174*la + 21389*la^2) + 
        En^4*(-68463840 + 4474648*la + 4222428*la^2 - 25504*la^3) + 
        3*(-3676874 + 1545393*la + 741052*la^2 - 91083*la^3 + 580*la^4) + 
        2*En^2*(29523828 - 5944626*la - 3333721*la^2 + 148072*la^3 + 
          752*la^4))*M^2 + 2*J^2*(-37812851 + 31882500*En^6 + 4900869*la + 
        1047310*la^2 - 33130*la^3 + 36*En^4*(-4273858 + 70759*la) - 
        3*En^2*(-55593536 + 3103642*la + 440709*la^2))*M^4 + 
      72*(-254438 + 483681*En^2 + 7606*la)*M^6)*rp[t]^21 + 
    M*(J^4*(546813 + 5760*En^10*(-88 + la) - 570780*la - 345519*la^2 + 
        71364*la^3 - 870*la^4 + 288*En^8*(-1019 + 640*la + 39*la^2) + 
        24*En^6*(-27486 - 12851*la + 10339*la^2 + 609*la^3) + 
        4*En^4*(1068351 - 281668*la - 311911*la^2 + 2568*la^3 + 360*la^4) - 
        2*En^2*(1681623 - 939143*la - 693653*la^2 + 54605*la^3 + 752*la^4)) + 
      J^2*(18718419 + 846720*En^8 - 4489418*la - 1444961*la^2 + 109500*la^3 - 
        288*la^4 + 504*En^6*(-109357 + 242*la) - 
        24*En^4*(-6109504 + 288099*la + 68242*la^2) - 
        6*En^2*(18224442 - 2116272*la - 615755*la^2 + 14608*la^3))*M^2 + 
      6*(2700600 + 6782796*En^4 - 202694*la - 19150*la^2 + 
        3*En^2*(-3463756 + 91909*la))*M^4)*rp[t]^22 + 
    (J^4*la*(576*En^8*(-13 - 11*la + 2*la^2) + 
        24*En^6*(-639 - 908*la - 259*la^2 + 10*la^3) - 
        60*En^4*(-1817 - 1831*la - 2*la^2 + 12*la^3) + 
        5*(5294 + 3921*la - 1344*la^2 + 29*la^3) + 
        4*En^2*(-27926 - 24843*la + 3177*la^2 + 94*la^3)) + 
      J^2*(-2343224 - 1296000*En^10 + 1087221*la + 472704*la^2 - 66365*la^3 + 
        432*la^4 - 144*En^8*(-12995 + 1618*la) - 
        24*En^6*(-573021 + 8441*la + 13577*la^2) - 
        12*En^4*(2408531 - 278372*la - 134673*la^2 + 1948*la^3) + 
        2*En^2*(8464017 - 2039125*la - 923066*la^2 + 51836*la^3 + 168*la^4))*
       M^2 + 6*(-1229417 + 3376332*En^6 + 180243*la + 34140*la^2 - 
        1200*la^3 + 66*En^4*(-143571 + 3440*la) + 
        En^2*(7197591 - 488480*la - 54517*la^2))*M^4)*rp[t]^23 + 
    M*(J^2*(-5760*En^10*(-98 + la) - 96*En^8*(8361 - 908*la + 91*la^2) + 
        8*En^6*(-103236 + 14584*la + 23405*la^2 + 355*la^3) + 
        12*En^4*(160015 - 56470*la - 41483*la^2 + 1338*la^3 + 36*la^4) - 
        3*(-37971 + 42980*la + 23969*la^2 - 5790*la^3 + 72*la^4) - 
        4*En^2*(242496 - 151375*la - 96236*la^2 + 9899*la^3 + 84*la^4)) + 
      (1806281 + 4510080*En^8 - 487366*la - 139839*la^2 + 11920*la^3 - 
        32*la^4 + 72*En^6*(-286685 + 5368*la) - 
        36*En^4*(-796255 + 50968*la + 7598*la^2) - 
        6*En^2*(2391451 - 329128*la - 76675*la^2 + 2134*la^3))*M^2)*
     rp[t]^24 + 2*(2*(-1 + En^2)*J^2*la*(-1402 - 983*la + 410*la^2 - 9*la^3 + 
        48*En^6*(-1 + 3*la + 4*la^2) + En^2*(6671 + 5902*la - 799*la^2 - 
          30*la^3) + 2*En^4*(-2773 - 2864*la - 79*la^2 + 12*la^3)) + 
      (181440*En^10 + 72*En^8*(-20833 + 95*la) - 
        12*En^6*(-279961 + 15508*la + 3725*la^2) - 
        6*En^4*(509954 - 72246*la - 23403*la^2 + 473*la^3) + 
        4*(-27818 + 14416*la + 5629*la^2 - 905*la^3 + 6*la^4) + 
        En^2*(1129960 - 310769*la - 115844*la^2 + 7733*la^3 + 16*la^4))*M^2)*
     rp[t]^25 - 4*(-1 + En^2)*(2637 - 3293*la - 1669*la^2 + 475*la^3 - 
      6*la^4 + 1440*En^8*(22 + la) + 96*En^6*(-919 + 52*la + 26*la^2) + 
      6*En^4*(14068 - 3744*la - 2055*la^2 + 7*la^3) - 
      2*En^2*(15246 - 9429*la - 5261*la^2 + 521*la^3 + 7*la^4))*M*rp[t]^26 + 
    4*(-1 + En^2)^2*la*(133 + 87*la - 45*la^2 + la^3 + 
      48*En^4*(16 + 17*la + la^2) + 2*En^2*(-403 - 354*la + 51*la^2 + 
        2*la^3))*rp[t]^27))/(En^2*(1 + la)*rp[t]^18*(J^2 + rp[t]^2)^9) + 
 ((8*mu*Pi*YBar*(2*M - rp[t])*(1550556*J^18*M^6 + 24*J^18*(-123628 + 693*la)*
       M^5*rp[t] + 6*J^16*M^4*(J^2*(369548 - 7648*la - 2901*la^2) + 
        2030148*M^2)*rp[t]^2 + J^16*M^3*
       (J^2*(-804832 + 45469*la + 27377*la^2 - 72*la^3) + 
        18*(-1298088 + 196395*En^2 + 7633*la)*M^2)*rp[t]^3 + 
      J^14*M^2*(J^4*(140932 - 20793*la - 15713*la^2 + 104*la^3) + 
        3*J^2*(5834577 - 123079*la - 44552*la^2 + 2*En^2*(-828761 + 2289*la))*
         M^2 + 41462928*M^4)*rp[t]^4 + 
      J^14*M*(2*J^4*(-4716 + 2213*la + 1936*la^2 - 25*la^3) + 
        J^2*(-6368945 + 361031*la + 211348*la^2 - 480*la^3 - 
          6*En^2*(-419057 + 7238*la + 4811*la^2))*M^2 + 
        36*(-2217209 + 743886*En^2 + 13475*la)*M^4)*rp[t]^5 + 
      2*J^12*(J^6*la*(-175 - 171*la + 4*la^2) + 
        J^4*(558953 - 81947*la - 60975*la^2 + 358*la^3 + 
          En^2*(-267566 + 17675*la + 15271*la^2 - 12*la^3))*M^2 + 
        12*J^2*(2500466 + 56187*En^4 - 53168*la - 18373*la^2 + 
          En^2*(-1578505 + 6348*la))*M^4 + 39675168*M^6)*rp[t]^6 + 
      J^12*M*(J^4*(-74988 + 34739*la + 30207*la^2 - 356*la^3 - 
          36*En^2*(-1103 + 297*la + 284*la^2)) - 
        J^2*(21915313 - 1235051*la - 702980*la^2 + 1096*la^3 + 
          12*En^4*(101497 + 945*la) + 12*En^2*(-1606178 + 32079*la + 
            18175*la^2))*M^2 + 12*(-12799699 + 7277484*En^2 + 78315*la)*M^4)*
       rp[t]^7 + J^10*(J^6*la*(1 + la)*(-2740 + 59*la + 
          2*En^2*(530 + 3*la)) + J^4*(3860645 - 558007*la - 408944*la^2 + 
          1792*la^3 + En^4*(335028 + 2916*la - 6624*la^2) - 
          4*En^2*(1032410 - 72587*la - 58114*la^2 + 132*la^3))*M^2 + 
        24*J^2*(4838507 + 412506*En^4 - 101551*la - 33571*la^2 + 
          En^2*(-5185568 + 27897*la))*M^4 + 92111976*M^6)*rp[t]^8 + 
      J^10*M*(J^4*(-259893 + 118047*la + 102110*la^2 - 970*la^3 + 
          12*En^4*(-2247 + 151*la + 316*la^2 + 10*la^3) + 
          12*En^2*(25722 - 7057*la - 6532*la^2 + 24*la^3)) + 
        J^2*(-42646817 + 95256*En^6 + 2348303*la + 1301668*la^2 - 240*la^3 - 
          108*En^4*(84086 + 539*la) - 12*En^2*(-5318524 + 121688*la + 
            58987*la^2))*M^2 + 36*(-4998077 + 4409446*En^2 + 29115*la)*M^4)*
       rp[t]^9 - 2*J^8*(J^6*la*(4653 + 4566*la - 87*la^2 + 
          En^2*(-4120 - 4114*la + 6*la^2) + 4*En^4*(55 + 62*la + 7*la^2)) + 
        J^4*(-3777764 + 530610*la + 383635*la^2 - 586*la^3 + 
          12*En^6*(1633 + 183*la) + 12*En^4*(-105898 - 189*la + 2059*la^2) + 
          En^2*(6896237 - 516379*la - 381196*la^2 + 1460*la^3))*M^2 - 
        6*J^2*(11441082 + 2579886*En^4 - 226510*la - 72135*la^2 + 
          2*En^2*(-9522089 + 64938*la))*M^4 - 32103288*M^6)*rp[t]^10 + 
      J^8*M*(J^4*(-511515 + 225088*la + 194081*la^2 - 992*la^3 - 
          96*En^6*(-33 - 12*la + la^2) + 12*En^4*(-17366 + 1378*la + 
            2401*la^2 + 64*la^3) + 2*En^2*(520335 - 145530*la - 129874*la^2 + 
            1004*la^3)) + J^2*(-50893201 + 726192*En^6 + 2639197*la + 
          1433030*la^2 + 3768*la^3 - 36*En^4*(803223 + 2591*la) - 
          12*En^2*(-9872806 + 256157*la + 106163*la^2))*M^2 + 
        36*(-3545277 + 4802771*En^2 + 16682*la)*M^4)*rp[t]^11 + 
      J^6*(J^6*la*(-17808 - 17575*la + 233*la^2 + 24*En^6*(1 + la)^2 + 
          En^2*(27768 + 27494*la - 274*la^2) - 4*En^4*(865 + 962*la + 
            97*la^2)) - J^4*(-9101795 + 1206095*la + 864870*la^2 + 
          3608*la^3 + 192*En^6*(1591 + 141*la) + 36*En^4*(-229835 + 1233*la + 
            4404*la^2) + 4*En^2*(6474125 - 516273*la - 348670*la^2 + 
            1852*la^3))*M^2 + 6*J^2*(16514893 + 8880024*En^4 - 271443*la - 
          85516*la^2 + En^2*(-42138538 + 346998*la))*M^4 + 23393808*M^6)*
       rp[t]^12 + J^6*M*(2*J^4*(-311031 + 129518*la + 111900*la^2 + 
          391*la^3 - 192*En^6*(-61 - 19*la + 2*la^2) + 
          16*En^2*(61758 - 17675*la - 15114*la^2 + 176*la^3) + 
          4*En^4*(-87714 + 7639*la + 11648*la^2 + 211*la^3)) + 
        J^2*(-37371587 + 2353896*En^6 + 1655149*la + 899572*la^2 + 
          8608*la^3 + 12*En^4*(-4217654 + 4817*la) - 
          12*En^2*(-11111969 + 319260*la + 112934*la^2))*M^2 + 
        12*(-4057937 + 9226302*En^2 + 2907*la)*M^4)*rp[t]^13 + 
      2*J^4*(J^6*la*(96*En^6*(1 + la)^2 + En^2*(26550 + 26068*la - 
            482*la^2) + 5*(-2075 - 2071*la + 4*la^2) - 
          16*En^4*(344 + 373*la + 29*la^2)) - J^4*(-3399971 + 394879*la + 
          285839*la^2 + 4958*la^3 + 948*En^6*(601 + 45*la) + 
          12*En^4*(-608359 + 10788*la + 12077*la^2) + 
          En^2*(14852975 - 1239069*la - 760426*la^2 + 5020*la^3))*M^2 + 
        12*J^2*(1644844 + 2300505*En^4 - 9978*la - 4149*la^2 + 
          En^2*(-6921587 + 65424*la))*M^4 + 641664*M^6)*rp[t]^14 + 
      J^4*M*(J^4*(-472842 + 176451*la + 155017*la^2 + 3460*la^3 - 
          48*En^6*(-3061 - 674*la + 22*la^2) + 4*En^4*(-302166 + 38623*la + 
            45011*la^2 + 352*la^3) + 4*En^2*(578682 - 166358*la - 
            135609*la^2 + 2030*la^3)) + J^2*(-15509755 + 3237408*En^6 + 
          366241*la + 232868*la^2 + 9480*la^3 + 
          36*En^4*(-1526177 + 8783*la) - 12*En^2*(-7514062 + 229129*la + 
            68429*la^2))*M^2 + 36*(-123779 + 988792*En^2 - 4865*la)*M^4)*
       rp[t]^15 - (J^8*la*(-24*En^6*(-53 - 32*la + 21*la^2) + 
          12*En^4*(1963 + 2040*la + 77*la^2) + 3*(4880 + 4989*la + 
            109*la^2) + 2*En^2*(-30978 - 30203*la + 775*la^2)) + 
        J^6*(-2934251 - 81792*En^8 + 223689*la + 179680*la^2 + 11504*la^3 + 
          192*En^6*(4178 + 515*la) + 12*En^4*(-1397593 + 41571*la + 
            25552*la^2) + 4*En^2*(5190968 - 433553*la - 239710*la^2 + 
            1836*la^3))*M^2 - 24*J^4*(204696 + 1331646*En^4 + 13362*la + 
          2707*la^2 + 9*En^2*(-260178 + 2665*la))*M^4 + 1994724*J^2*M^6)*
       rp[t]^16 + J^2*M*(J^4*(-211833 + 59159*la + 56182*la^2 + 4394*la^3 - 
          576*En^8*(149 + 9*la) - 192*En^6*(829 - 183*la + 53*la^2) - 
          4*En^4*(372045 - 57557*la - 52012*la^2 + 22*la^3) + 
          4*En^2*(420840 - 115435*la - 89332*la^2 + 1560*la^3)) + 
        J^2*(-2394563 + 5482728*En^6 - 200659*la - 63140*la^2 + 5904*la^3 + 
          108*En^4*(-320154 + 2815*la) - 12*En^2*(-2683512 + 80404*la + 
            18829*la^2))*M^2 + 36*(90137 + 48482*En^2 - 2805*la)*M^4)*
       rp[t]^17 - 2*(J^6*la*(2775 + 3028*la + 253*la^2 - 
          96*En^6*(4 + 7*la + 3*la^2) + 12*En^4*(1333 + 1342*la + 9*la^2) + 
          En^2*(-21572 - 20930*la + 642*la^2)) + 
        J^4*(-266930 + 306432*En^8 - 23584*la - 6243*la^2 + 3674*la^3 + 
          12*En^6*(123383 + 1085*la) + 36*En^4*(-164080 + 5469*la + 
            2279*la^2) + En^2*(3936275 - 300433*la - 144364*la^2 + 
            1244*la^3))*M^2 - 3*J^2*(-328312 + 1095252*En^4 + 33956*la + 
          7371*la^2 + 36*En^2*(-18659 + 170*la))*M^4 + 248292*M^6)*rp[t]^18 + 
      M*(J^4*(-43983 - 9792*En^8*(-34 + la) - 1558*la + 3135*la^2 + 
          2920*la^3 - 96*En^6*(-2863 + 34*la + 197*la^2) - 
          4*En^4*(311310 - 41216*la - 30793*la^2 + 208*la^3) + 
          2*En^2*(342171 - 80858*la - 58710*la^2 + 1124*la^3)) + 
        J^2*(538013 + 3669552*En^6 - 151942*la - 58207*la^2 + 2000*la^3 + 
          60*En^4*(-135923 + 1453*la) + 12*En^2*(252810 - 4203*la + 
            767*la^2))*M^2 - 198*(-4446 + 6971*En^2 + 95*la)*M^4)*rp[t]^19 + 
      (J^4*la*(-496 - 857*la - 361*la^2 + En^2*(15536 + 15034*la - 
            502*la^2) + 24*En^6*(239 + 250*la + 11*la^2) + 
          20*En^4*(-1103 - 1090*la + 13*la^2)) + 
        J^2*(-60727 + 567936*En^8 + 51560*la + 26455*la^2 - 2528*la^3 + 
          192*En^6*(-14801 + 64*la) - 12*En^4*(-270871 + 8157*la + 
            2092*la^2) - 4*En^2*(230801 - 8275*la - 610*la^2 + 20*la^3))*
         M^2 - 3*(200707 + 349680*En^4 - 12885*la - 2720*la^2 + 
          En^2*(-631062 + 7710*la))*M^4)*rp[t]^20 - 
      2*M*(2*J^2*(-405 + 1923*la + 1171*la^2 - 257*la^3 + 
          144*En^8*(377 + 7*la) + 48*En^6*(-2770 + 63*la + 43*la^2) + 
          En^2*(-24318 + 2762*la + 1488*la^2 - 32*la^3) + 
          2*En^4*(51705 - 4748*la - 2848*la^2 + 55*la^3)) + 
        3*(-32804 + 49788*En^6 + 4968*la + 1900*la^2 - 48*la^3 + 
          6*En^4*(-28834 + 135*la) + En^2*(155355 - 6806*la - 2311*la^2))*
         M^2)*rp[t]^21 - 2*(-2*(-1 + En^2)*J^2*la*(-93 - 60*la + 33*la^2 + 
          816*En^4*(1 + la) + 4*En^2*(-109 - 98*la + 11*la^2)) + 
        (13824*En^8 - 36*En^6*(2645 + 29*la) - 12*En^4*(-13553 + 414*la + 
            269*la^2) + En^2*(-96445 + 12180*la + 7049*la^2 - 56*la^3) + 
          2*(7585 - 2670*la - 1415*la^2 + 92*la^3))*M^2)*rp[t]^22 + 
      4*(-1 + En^2)*(-432 + 436*la + 290*la^2 - 38*la^3 + 
        144*En^6*(16 + la) + 12*En^4*(-409 + 14*la + 18*la^2) - 
        En^2*(-3042 + 1021*la + 839*la^2 + 16*la^3))*M*rp[t]^23 - 
      4*(-1 + En^2)^2*la*(1 + la)*(5*(-5 + la) + 6*En^2*(11 + la))*rp[t]^24))/
    (En*rp[t]^15*(J^2 + rp[t]^2)^8) - (8*J^2*mu*Pi*YPhiPhiBar*(2*M - rp[t])*
     (22821750*J^22*M^7 - 441*J^22*(108277 - 8912*la + 7402*m^2)*M^6*rp[t] + 
      3*J^20*M^5*(J^2*(-4*(-3355937 + 646232*la + 20649*la^2) + 
          (2074543 - 36936*la)*m^2 + 11244*m^4) + 86707242*M^2)*rp[t]^2 + 
      3*J^20*M^4*(-2*J^2*(2898760 - 1008047*la - 64517*la^2 + 8661*la^3 + 
          (771737 - 37773*la - 6329*la^2)*m^2 + (9596 - 24*la)*m^4 + 4*m^6) + 
        3*(-60468713 + 4554000*En^2 + 4888624*la - 3917396*m^2)*M^2)*
       rp[t]^3 + J^18*M^3*(J^4*(4014000 - 2343280*la - 222179*la^2 + 
          79617*la^3 - 776*la^4 + (1671285 - 178095*la - 58441*la^2 + 
            920*la^3)*m^2 + (36321 - 464*la - 248*la^2)*m^4 + 36*m^6) - 
        18*J^2*(-25495523 + 4827042*la + 156652*la^2 + (-3743591 + 64040*la)*
           m^2 - 18290*m^4 + En^2*(3604844 - 385807*la + 407540*m^2))*M^2 + 
        1352688282*M^4)*rp[t]^4 + J^18*M^2*
       (J^4*(-461880 + 460448*la + 55563*la^2 - 44393*la^3 + 1032*la^4 + 
          (-290409 + 66563*la + 32683*la^2 - 1240*la^3)*m^2 + 
          3*(-3335 + 148*la + 112*la^2)*m^4 - 18*m^6) + 
        6*J^2*(-33026205 + 11305127*la + 736081*la^2 - 96659*la^3 + 
          4*(-2094583 + 98947*la + 16959*la^2)*m^2 + 4*(-23537 + 54*la)*m^4 - 
          32*m^6 + 2*En^2*(3239707 - 834799*la - 26283*la^2 + 
            (858071 - 21318*la)*m^2 + 9462*m^4))*M^2 + 
        9*(-314456903 + 49843920*En^2 + 24910416*la - 19139538*m^2)*M^4)*
       rp[t]^5 + J^16*M*(J^6*(20160 - 40600*la - 5222*la^2 + 10592*la^3 - 
          450*la^4 + 2*(9609 - 5821*la - 3899*la^2 + 275*la^3)*m^2 - 
          3*(-337 + 56*la + 50*la^2)*m^4 + 3*m^6) + 
        J^4*(45716856 - 26309057*la - 2545330*la^2 + 888735*la^3 - 
          8728*la^4 + (18198621 - 1880262*la - 628874*la^2 + 9400*la^3)*m^2 + 
          (358497 - 4288*la - 2344*la^2)*m^4 + 288*m^6 - 
          6*En^2*(1805399 - 878407*la - 56417*la^2 + 9861*la^3 + 
            (863585 - 60698*la - 9744*la^2)*m^2 + (22886 - 84*la)*m^4 + 
            24*m^6))*M^2 + 9*J^2*(265158826 + 1787400*En^4 - 49245712*la - 
          1627620*la^2 + (36688509 - 599928*la)*m^2 + 158752*m^4 + 
          En^2*(-79096358 + 8304460*la - 8522936*m^2))*M^4 + 4239449010*M^6)*
       rp[t]^6 + J^16*(2*J^6*la*(504 + 21*la - 451*la^2 + 32*la^3 + 
          (370 + 330*la - 40*la^2)*m^2 + 11*(1 + la)*m^4) + 
        J^4*(-5257836 + 5177386*la + 641555*la^2 - 495605*la^3 + 11604*la^4 + 
          (-3172911 + 708601*la + 353337*la^2 - 12700*la^3)*m^2 + 
          (-99441 + 4164*la + 3192*la^2)*m^4 - 144*m^6 + 
          2*En^2*(682767 - 603560*la - 57688*la^2 + 30651*la^3 - 76*la^4 + 
            (546003 - 90063*la - 30257*la^2 + 700*la^3)*m^2 - 
            4*(-6726 + 145*la + 82*la^2)*m^4 + 72*m^6))*M^2 - 
        6*J^2*(171717405 - 57736931*la - 3835649*la^2 + 490831*la^3 + 
          (41189155 - 1868711*la - 328379*la^2)*m^2 + (410848 - 864*la)*m^4 + 
          112*m^6 + 12*En^4*(240254 - 32146*la + 39713*m^2) - 
          2*En^2*(35633447 - 9017165*la - 284514*la^2 + (9027409 - 212478*la)*
             m^2 + 89352*m^4))*M^4 + 9*(-985681907 + 247506288*En^2 + 
          76278000*la - 55733168*m^2)*M^6)*rp[t]^7 + 
      J^14*M*(J^6*(229320 - 457676*la - 61599*la^2 + 118247*la^3 - 
          5058*la^4 + (210777 - 125131*la - 84777*la^2 + 5650*la^3)*m^2 - 
          3*(-3377 + 532*la + 478*la^2)*m^4 + 24*m^6 - 
          2*En^2*(29610 - 54082*la - 6012*la^2 + 10032*la^3 - 40*la^4 + 
            2*(19863 - 8837*la - 4884*la^2 + 290*la^3)*m^2 + 
            (3384 - 343*la - 280*la^2)*m^4 + 18*m^6)) + 
        J^4*(-3*(-79213726 + 44851844*la + 4442207*la^2 - 1504873*la^3 + 
            14880*la^4) + (89792856 - 8953111*la - 3058201*la^2 + 43200*la^3)*
           m^2 - 4*(-393615 + 4400*la + 2456*la^2)*m^4 + 1008*m^6 + 
          12*En^4*(523482 - 178113*la - 8660*la^2 + (213989 - 6350*la)*m^2 + 
            4120*m^4) - 6*En^2*(19912659 - 9527069*la - 613898*la^2 + 
            106188*la^3 + (9148729 - 614366*la - 101972*la^2)*m^2 + 
            (218956 - 672*la)*m^4 + 168*m^6))*M^2 + 
        6*J^2*(28361628*En^4 + En^2*(-590737857 + 60663081*la - 
            60185760*m^2) + 2*(623417628 - 113248121*la - 3824882*la^2 + 
            (80412090 - 1249488*la)*m^2 + 301392*m^4))*M^4 + 8906300028*M^6)*
       rp[t]^8 + J^14*(-(J^6*la*(1 + la)*(-11448 + 10786*la - 719*la^2 + 
           25*(-322 + 33*la)*m^2 - 212*m^4 + 2*En^2*(972 - 1024*la + la^2 - 
             5*(-214 + 23*la)*m^2 + 58*m^4))) + 
        J^4*(-27319041 + 26529013*la + 3388582*la^2 - 2518377*la^3 + 
          59340*la^4 - 6*(2619939 - 567670*la - 287938*la^2 + 9750*la^3)*
           m^2 + 12*(-36669 + 1444*la + 1120*la^2)*m^4 - 504*m^6 - 
          12*En^4*(72585 - 51439*la - 5673*la^2 + 897*la^3 + 
            (57925 - 5410*la - 1148*la^2)*m^2 + (2938 - 8*la)*m^4 + 8*m^6) + 
          2*En^2*(7552863 - 6577090*la - 632804*la^2 + 331389*la^3 - 
            980*la^4 + (5830785 - 927357*la - 320011*la^2 + 6740*la^3)*m^2 - 
            8*(-32673 + 622*la + 370*la^2)*m^4 + 504*m^6))*M^2 + 
        6*J^2*(-538297470 + 248400*En^6 + 177303971*la + 12062461*la^2 - 
          1497049*la^3 + 8*(-15106096 + 654385*la + 118177*la^2)*m^2 + 
          224*(-4669 + 9*la)*m^4 - 224*m^6 - 24*En^4*(1279814 - 168700*la + 
            203201*m^2) + 2*En^2*(177950806 - 44099710*la - 1398327*la^2 + 
            (42804548 - 942432*la)*m^2 + 369888*m^4))*M^4 + 
        18*(-1035715769 + 367748448*En^2 + 77991312*la - 53582858*m^2)*M^6)*
       rp[t]^9 + J^12*M*(J^6*(8*En^4*(4410 - 7178*la - 1346*la^2 + 749*la^3 + 
            35*la^4 + (6720 - 1920*la - 941*la^2 + 10*la^3)*m^2 + 
            (726 - 26*la - 20*la^2)*m^4 + 6*m^6) + 
          3*(396900 - 784123*la - 110954*la^2 + 200329*la^3 - 8620*la^4 + 
            2*(174863 - 101361*la - 69543*la^2 + 4350*la^3)*m^2 + 
            (15079 - 2240*la - 2024*la^2)*m^4 + 28*m^6) - 
          2*En^2*(328545 - 592826*la - 67245*la^2 + 108912*la^3 - 572*la^4 + 
            2*(214119 - 92830*la - 52317*la^2 + 2830*la^3)*m^2 + 
            (33489 - 3080*la - 2576*la^2)*m^4 + 126*m^6)) - 
        J^4*(3*(-248283534 + 137986333*la + 14047942*la^2 - 4592407*la^3 + 
            45680*la^4) - 4*(66159243 - 6329714*la - 2212766*la^2 + 
            29400*la^3)*m^2 + 28*(-144081 + 1504*la + 856*la^2)*m^4 - 
          2016*m^6 + 216*En^6*(3985 - 511*la + 962*m^2) - 
          36*En^4*(1873190 - 629653*la - 29702*la^2 + (740284 - 21452*la)*
             m^2 + 13208*m^4) + 6*En^2*(99769894 - 46816674*la - 
            3035257*la^2 + 517383*la^3 - 8*(-5466625 + 346901*la + 
              60029*la^2)*m^2 + (919964 - 2352*la)*m^4 + 504*m^6))*M^2 + 
        18*J^2*(873650768 + 45121296*En^4 - 154651056*la - 5360364*la^2 - 
          63*(-1643291 + 24104*la)*m^2 + 326788*m^4 + 
          En^2*(-586966596 + 58733864*la - 55952096*m^2))*M^4 + 
        13186514124*M^6)*rp[t]^10 + 
      J^12*(J^6*la*(59346 + 4506*la - 51165*la^2 + 3675*la^3 - 
          5*(-7936 - 7171*la + 765*la^2)*m^2 + 904*(1 + la)*m^4 + 
          4*En^4*(186 + 19*la - 199*la^2 - 32*la^3 + 220*(1 + la)*m^2 + 
            14*(1 + la)*m^4) - 2*En^2*(10782 - 407*la - 11148*la^2 + 
            41*la^3 - 5*(-2306 - 2077*la + 229*la^2)*m^2 + 
            548*(1 + la)*m^4)) + J^4*(24*En^6*(5451 - 1851*la - 458*la^2 + 
            (3491 + 142*la)*m^2 + 112*m^4) - 12*En^4*(785061 - 551657*la - 
            58723*la^2 + 9559*la^3 + (611023 - 56406*la - 11836*la^2)*m^2 + 
            (29110 - 56*la)*m^4 + 48*m^6) - 3*(28533135 - 27270201*la - 
            3607642*la^2 + 2563151*la^3 - 60720*la^4 + 
            4*(3880518 - 811257*la - 419321*la^2 + 13300*la^3)*m^2 - 
            28*(-13527 + 500*la + 392*la^2)*m^4 + 336*m^6) + 
          4*En^2*(18989460 - 16249822*la - 1578827*la^2 + 811413*la^3 - 
            2792*la^4 + 2*(7033221 - 1069395*la - 381644*la^2 + 7180*la^3)*
             m^2 - 8*(-69906 + 1162*la + 721*la^2)*m^4 + 756*m^6))*M^2 + 
        12*J^2*(-565866240 + 1311768*En^6 + 181950091*la + 12736017*la^2 - 
          1523713*la^3 + 7*(-16752311 + 688251*la + 128031*la^2)*m^2 + 
          28*(-30545 + 54*la)*m^4 - 140*m^6 - 12*En^4*(6158602 - 796326*la + 
            930369*m^2) + 4*En^2*(133074887 - 32186796*la - 1029526*la^2 + 
            (30109927 - 609588*la)*m^2 + 218202*m^4))*M^4 + 
        18*(-1534448909 + 725889552*En^2 + 111854064*la - 71109948*m^2)*M^6)*
       rp[t]^11 + J^10*M*(J^6*(-48*En^6*(90 - 91*la - 69*la^2 - 3*la^3 + 
            (132 + 24*la - 8*la^2)*m^2 + 2*(6 + la)*m^4) + 
          8*En^4*(47970 - 77782*la - 13999*la^2 + 8074*la^3 + 343*la^4 + 
            (72195 - 20541*la - 9907*la^2 + 110*la^3)*m^2 - 
            2*(-3723 + 119*la + 98*la^2)*m^4 + 36*m^6) + 
          3*(-6*(-207145 + 404541*la + 60529*la^2 - 101996*la^3 + 
              4410*la^4) + (1042109 - 586884*la - 408284*la^2 + 23800*la^3)*
             m^2 - 7*(-5615 + 784*la + 712*la^2)*m^4 + 56*m^6) - 
          2*En^2*(1658385 - 2949967*la - 342978*la^2 + 536157*la^3 - 
            3532*la^4 + (2088918 - 878008*la - 506874*la^2 + 24400*la^3)*
             m^2 + (146691 - 11956*la - 10192*la^2)*m^4 + 378*m^6)) + 
        2*J^4*(3240*En^8 - 3*(-261002463 + 141936680*la + 14931797*la^2 - 
            4677679*la^3 + 46760*la^4) + 7*(36892125 - 3363353*la - 
            1205759*la^2 + 15000*la^3)*m^2 - 7*(-474381 + 4624*la + 
            2680*la^2)*m^4 + 1260*m^6 - 216*En^6*(21322 - 2865*la + 
            5087*m^2) + 36*En^4*(4546637 - 1503200*la - 69014*la^2 + 
            (1720864 - 47766*la)*m^2 + 27804*m^4) - 
          6*En^2*(149814679 - 68730448*la - 4499188*la^2 + 751228*la^3 - 
            4*(-15533876 + 916855*la + 167113*la^2)*m^2 + (1101662 - 2352*la)*
             m^4 + 420*m^6))*M^2 + 36*J^2*(64011432*En^4 + 
          En^2*(-581289366 + 56399711*la - 51081812*m^2) + 
          7*(92505727 - 15876750*la - 567804*la^2 + (9865095 - 135624*la)*
             m^2 + 25286*m^4))*M^4 + 14066046036*M^6)*rp[t]^12 + 
      J^10*(J^6*la*(-48*En^6*(1 + la)^2*(1 - la + m^2) + 
          4*En^4*(2016 + 179*la - 2159*la^2 - 322*la^3 + 
            (2390 + 2360*la - 30*la^2)*m^2 + 154*(1 + la)*m^4) + 
          5*(37107 + 3579*la - 31272*la^2 + 2256*la^3 + 
            (23363 + 21263*la - 2100*la^2)*m^2 + 448*(1 + la)*m^4) - 
          2*En^2*(54423 - 1072*la - 55125*la^2 + 370*la^3 + 
            (56225 + 51205*la - 5020*la^2)*m^2 + 2212*(1 + la)*m^4)) + 
        J^4*(-2160*En^8*(1 + la) + 24*En^6*(58611 - 21111*la - 4438*la^2 + 
            (37681 + 842*la)*m^2 + 1232*m^4) - 12*En^4*
           (3844368 - 2669608*la - 275714*la^2 + 46172*la^3 + 
            (2897005 - 261626*la - 55068*la^2)*m^2 - 42*(-3061 + 4*la)*m^4 + 
            120*m^6) - 3*(59977383 - 56274412*la - 7757822*la^2 + 
            5225654*la^3 - 124320*la^4 + 14*(2178132 - 436221*la - 
              230149*la^2 + 6800*la^3)*m^2 - 14*(-44883 + 1556*la + 
              1232*la^2)*m^4 + 420*m^6) + 4*En^2*(57287907 - 48022169*la - 
            4727596*la^2 + 2371752*la^3 - 9256*la^4 + 
            (40406490 - 5798178*la - 2160656*la^2 + 35560*la^3)*m^2 - 
            4*(-341067 + 4942*la + 3178*la^2)*m^4 + 1260*m^6))*M^2 + 
        12*J^2*(6203880*En^6 - 12*En^4*(17656098 - 2227438*la + 
            2507557*m^2) + 2*En^2*(529244477 - 124424199*la - 4039699*la^2 + 
            (111269149 - 2025870*la)*m^2 + 640350*m^4) - 
          7*(119884635 - 37453181*la - 2713635*la^2 + 310529*la^3 - 
            4*(-5620903 + 217388*la + 41744*la^2)*m^2 - 4*(-33271 + 54*la)*
             m^4 + 16*m^6))*M^4 + 18*(998045808*En^2 - 
          7*(234106241 - 16403760*la + 9423734*m^2))*M^6)*rp[t]^13 + 
      J^8*M*(J^6*(-48*En^6*(945 - 971*la - 679*la^2 - 28*la^3 + 
            (1372 + 204*la - 68*la^2)*m^2 + 2*(61 + 6*la)*m^4) + 
          3*(2611290 - 5032570*la - 802038*la^2 + 1248724*la^3 - 54180*la^4 + 
            28*(73687 - 40025*la - 28267*la^2 + 1525*la^3)*m^2 - 
            7*(-9403 + 1232*la + 1124*la^2)*m^4 + 70*m^6) + 
          8*En^4*(237150 - 382562*la - 66401*la^2 + 39365*la^3 + 1448*la^4 + 
            (353295 - 98831*la - 46591*la^2 + 490*la^3)*m^2 - 
            126*(-280 + 7*la + 6*la^2)*m^4 + 90*m^6) - 
          2*En^2*(5026320 - 8788865*la - 1052016*la^2 + 1578654*la^3 - 
            12464*la^4 + 2*(3034233 - 1229596*la - 732939*la^2 + 30520*la^3)*
             m^2 + (366381 - 26264*la - 22736*la^2)*m^4 + 630*m^6)) + 
        2*J^4*(35640*En^8 - 72*En^6*(307304 - 42599*la + 72727*m^2) + 
          12*En^4*(39505569 - 12761976*la - 568874*la^2 + 
            (14071898 - 361850*la)*m^2 + 191980*m^4) + 
          7*(-3*(-55307421 + 29307473*la + 3208074*la^2 - 954215*la^3 + 
              9576*la^4) + (49864257 - 4297630*la - 1582834*la^2 + 
              18360*la^3)*m^2 + (520029 - 4736*la - 2792*la^2)*m^4 + 
            144*m^6) - 6*En^2*(299392856 - 133800123*la - 8889529*la^2 + 
            1439725*la^3 + (116408281 - 6243110*la - 1209620*la^2)*m^2 - 
            20*(-81965 + 147*la)*m^4 + 420*m^6))*M^2 + 
        6*J^2*(716793192*En^4 - 6*En^2*(801880737 - 74902642*la + 
            63375580*m^2) + 7*(593437806 - 98057120*la - 3642788*la^2 - 
            9*(-6138781 + 78520*la)*m^2 + 109440*m^4))*M^4 + 10835927172*M^6)*
       rp[t]^14 + J^8*(J^6*la*(389109 + 46689*la - 319320*la^2 + 23100*la^3 - 
          35*(-6499 - 5959*la + 540*la^2)*m^2 + 3556*(1 + la)*m^4 - 
          528*En^6*(1 + la)^2*(1 - la + m^2) + 4*En^4*(9570 + 607*la - 
            10351*la^2 - 1388*la^3 - 30*(-363 - 356*la + 7*la^2)*m^2 + 
            630*(1 + la)*m^4) - 2*En^2*(165603 + 502*la - 163497*la^2 + 
            1604*la^3 - 5*(-33023 - 30475*la + 2548*la^2)*m^2 + 
            5012*(1 + la)*m^4)) + J^4*(-23760*En^8*(1 + la) + 
          24*En^6*(298881 - 112269*la - 20234*la^2 + (205847 + 790*la)*m^2 + 
            7600*m^4) - 21*(12707097 - 11667256*la - 1690018*la^2 + 
            1067154*la^3 - 25464*la^4 + 2*(2968917 - 564719*la - 
              304559*la^2 + 8340*la^3)*m^2 + (99150 - 3224*la - 2576*la^2)*
             m^4 + 48*m^6) - 12*En^4*(2*(5571186 - 3823868*la - 386519*la^2 + 
              66770*la^3) + (7916147 - 698190*la - 156140*la^2)*m^2 + 
            (308990 - 280*la)*m^4 + 160*m^6) + 4*En^2*(115326306 - 
            94224127*la - 9419144*la^2 + 4582683*la^3 - 19796*la^4 + 
            (76940649 - 10166505*la - 3989005*la^2 + 56420*la^3)*m^2 - 
            20*(-103260 + 1309*la + 868*la^2)*m^4 + 1260*m^6))*M^2 + 
        12*J^2*(17187192*En^6 - 24*En^4*(16785774 - 2050192*la + 
            2238265*m^2) + 2*En^2*(732412297 - 166617321*la - 5560934*la^2 + 
            (140226683 - 2240574*la)*m^2 + 597516*m^4) - 
          7*(128308935 - 38679053*la - 2923159*la^2 + 316801*la^3 + 
            (21138473 - 763637*la - 151649*la^2)*m^2 - 16*(-6032 + 9*la)*
             m^4 + 8*m^6))*M^4 + 18*(-1265159539 + 975382320*En^2 + 
          84390768*la - 42230104*m^2)*M^6)*rp[t]^15 + 
      J^6*M*(J^6*(-48*En^6*(8775 - 6033*la - 2863*la^2 - 110*la^3 - 
            20*(-637 - 26*la + 6*la^2)*m^2 + 10*(121 + 3*la)*m^4) + 
          21*(-2*(-276390 + 524951*la + 90042*la^2 - 127661*la^3 + 
              5550*la^4) + (405923 - 210642*la - 151158*la^2 + 7500*la^3)*
             m^2 + (10481 - 1288*la - 1180*la^2)*m^4 + 8*m^6) + 
          8*En^4*(636930 - 1118806*la - 200479*la^2 + 116359*la^3 + 
            3472*la^4 + 5*(186999 - 56829*la - 27839*la^2 + 238*la^3)*m^2 - 
            10*(-9012 + 175*la + 154*la^2)*m^4 + 120*m^6) - 
          2*En^2*(-2*(-5132430 + 8691734*la + 1060134*la^2 - 1540035*la^3 + 
              14000*la^4) + 10*(1177767 - 448702*la - 277965*la^2 + 
              9772*la^3)*m^2 - 5*(-113355 + 7154*la + 6272*la^2)*m^4 + 
            630*m^6)) + 2*J^4*(418824*En^8 - 72*En^6*(760386 - 117223*la + 
            162585*m^2) + 36*En^4*(25918679 - 7951263*la - 333050*la^2 + 
            (8615235 - 187920*la)*m^2 + 85980*m^4) + 
          7*(-3*(-59235375 + 30391796*la + 3492969*la^2 - 974647*la^3 + 
              9808*la^4) + (47285874 - 3820373*la - 1447691*la^2 + 
              15600*la^3)*m^2 - 2*(-189729 + 1616*la + 968*la^2)*m^4 + 
            72*m^6) - 6*En^2*(415534966 - 180891737*la - 12363990*la^2 + 
            1901928*la^3 + (149536685 - 7091554*la - 1470232*la^2)*m^2 + 
            (1549298 - 2352*la)*m^4 + 252*m^6))*M^2 + 
        36*J^2*(535722494 + 150079500*En^4 - 84364478*la - 3282260*la^2 - 
          6*(-6926361 + 81832*la)*m^2 + 59128*m^4 + 
          En^2*(-786843617 + 69704975*la - 53325984*m^2))*M^4 + 
        5925377070*M^6)*rp[t]^16 + 
      J^6*(J^6*la*(48*En^6*(1 + la)*(225 - 36*la + 49*la^2 - 
            15*(-7 + 3*la)*m^2) + 7*(82329 + 12213*la - 65382*la^2 + 
            4734*la^3 - 5*(-8741 - 8075*la + 666*la^2)*m^2 + 
            536*(1 + la)*m^4) + 4*En^4*(37170 + 9857*la - 30701*la^2 - 
            3388*la^3 + (34970 + 34340*la - 630*la^2)*m^2 + 
            1330*(1 + la)*m^4) - 2*En^2*(329265 + 2582*la - 322581*la^2 + 
            4102*la^3 - 25*(-12833 - 12007*la + 826*la^2)*m^2 + 
            7000*(1 + la)*m^4)) - J^4*(432*En^8*(4041 + 193*la + 960*m^2) - 
          24*En^6*(121455 - 290939*la - 63454*la^2 - 5*(-57865 + 742*la)*
             m^2 + 20320*m^4) + 21*(13610139 - 12166606*la - 1872560*la^2 + 
            1091522*la^3 - 26088*la^4 + 2*(2845083 - 509138*la - 
              280986*la^2 + 7100*la^3)*m^2 - 4*(-18219 + 556*la + 448*la^2)*
             m^4 + 24*m^6) + 12*En^4*(2*(11892015 - 7237163*la - 
              665362*la^2 + 126955*la^3) - 5*(-3001303 + 232414*la + 
              57716*la^2)*m^2 + (429710 - 280*la)*m^4 + 120*m^6) - 
          4*En^2*(160510122 - 128762735*la - 13253296*la^2 + 6112953*la^3 - 
            28588*la^4 + (101252079 - 11933979*la - 4956071*la^2 + 
              59500*la^3)*m^2 - 4*(-495543 + 5530*la + 3766*la^2)*m^4 + 
            756*m^6))*M^2 + 12*J^2*(-696573390 + 39053592*En^6 + 
          200513123*la + 15956037*la^2 - 1617953*la^3 + 
          4*(-24051280 + 805407*la + 165663*la^2)*m^2 + 16*(-19654 + 27*la)*
           m^4 - 16*m^6 - 12*En^4*(42297762 - 5177958*la + 5437859*m^2) + 
          2*En^2*(720766136 - 156736990*la - 5499227*la^2 - 
            2*(-60130601 + 824292*la)*m^2 + 346056*m^4))*M^4 + 
        9*(-1388888057 + 1355275968*En^2 + 87044880*la - 35694498*m^2)*M^6)*
       rp[t]^17 + J^4*M*(J^6*(5760*En^8*(150 - 11*la + la^2 + 
            3*(22 + la)*m^2) - 48*En^6*(-79005 - 16693*la - 14903*la^2 - 
            20*la^3 - 20*(475 + 31*la + 43*la^2)*m^2 + 20*(193 + 2*la)*m^4) + 
          En^2*(-28710450 + 47999566*la + 6025188*la^2 - 8317260*la^3 + 
            84112*la^4 - 4*(8001153 - 2754112*la - 1778529*la^2 + 51940*la^3)*
             m^2 + 2*(-554499 + 30968*la + 27440*la^2)*m^4 - 756*m^6) + 
          21*(-2*(-295620 + 552184*la + 103457*la^2 - 130793*la^3 + 
              5688*la^4) + 2*(197081 - 96638*la - 70506*la^2 + 3200*la^3)*
             m^2 + (7771 - 896*la - 824*la^2)*m^4 + 4*m^6) + 
          8*En^4*(1767690 - 2111048*la - 321101*la^2 + 229259*la^3 + 
            5222*la^4 + 5*(363885 - 104559*la - 55597*la^2 + 350*la^3)*m^2 - 
            10*(-13119 + 203*la + 182*la^2)*m^4 + 90*m^6)) - 
        2*J^4*(9752616*En^8 + 3*(-322266213 + 158419917*la + 19333406*la^2 - 
            4984799*la^3 + 50240*la^4) + (-217391958 + 16327588*la + 
            6372988*la^2 - 63600*la^3)*m^2 + 2*(-621855 + 4960*la + 
            3016*la^2)*m^4 - 144*m^6 + 72*En^6*(2626491 - 229940*la + 
            338505*m^2) - 36*En^4*(32761117 - 10283353*la - 401060*la^2 - 
            2*(-5507077 + 92345*la)*m^2 + 67380*m^4) + 
          6*En^2*(408950087 - 172411542*la - 12466389*la^2 + 1752779*la^3 - 
            4*(-32835088 + 1342577*la + 298919*la^2)*m^2 + (907618 - 1176*la)*
             m^4 + 84*m^6))*M^2 + 9*J^2*(521991072*En^4 - 
          4*(-295310801 + 43694432*la + 1797239*la^2) + 
          (70812465 - 767544*la)*m^2 + 63796*m^4 - 
          8*En^2*(275629721 - 22444298*la + 14613600*m^2))*M^4 + 
        2197541718*M^6)*rp[t]^18 + 
      J^4*(J^6*la*(-960*En^8*(16 + 17*la + la^2) - 48*En^6*(1 + la)*
           (1685 + 106*la - 119*la^2 + 5*(-31 + 19*la)*m^2) + 
          20*En^4*(13302 + 1805*la - 12533*la^2 - 1036*la^3 + 
            (15950 + 15740*la - 210*la^2)*m^2 + 322*(1 + la)*m^4) + 
          7*(88299 + 16323*la - 67122*la^2 + 4854*la^3 + 
            (41075 + 38225*la - 2850*la^2)*m^2 + 376*(1 + la)*m^4) - 
          2*En^2*(448827 - 104*la - 442197*la^2 + 6734*la^3 - 
            5*(-84599 - 80161*la + 4438*la^2)*m^2 + 6188*(1 + la)*m^4)) + 
        J^4*(829440*En^10 + 144*En^8*(203311 - 5745*la + 15920*m^2) + 
          24*En^6*(4493259 - 542775*la - 85990*la^2 + (694585 - 10870*la)*
             m^2 + 26000*m^4) - 12*En^4*(2*(15255945 - 9579397*la - 
              756914*la^2 + 163181*la^3) + (20662685 - 1227650*la - 
              344308*la^2)*m^2 - 6*(-57643 + 28*la)*m^4 + 48*m^6) - 
          3*(74156637 - 63910486*la - 10600064*la^2 + 5592134*la^3 - 
            133680*la^4 + 4*(6622182 - 1104425*la - 624169*la^2 + 14500*la^3)*
             m^2 - 4*(-60129 + 1724*la + 1400*la^2)*m^4 + 48*m^6) + 
          4*En^2*(156862899 - 124723015*la - 13736702*la^2 + 5696082*la^3 - 
            28280*la^4 + (91736010 - 9360126*la - 4121788*la^2 + 41720*la^3)*
             m^2 - 8*(-147144 + 1456*la + 1015*la^2)*m^4 + 252*m^6))*M^2 + 
        6*J^2*(-771642720 + 76269456*En^6 + 208829531*la + 17690361*la^2 - 
          1654649*la^3 + (-82738933 + 2550185*la + 543941*la^2)*m^2 + 
          4*(-42619 + 54*la)*m^4 - 4*m^6 - 24*En^4*(36153374 - 4482410*la + 
            4285671*m^2) + 8*En^2*(254494619 - 51170950*la - 1947638*la^2 + 
            (33663247 - 388968*la)*m^2 + 56862*m^4))*M^4 + 
        9*(-518288013 + 650179536*En^2 + 30005744*la - 8986836*m^2)*M^6)*
       rp[t]^19 + J^2*M*(-(J^6*(34560*En^10*(22 + la) - 
           1920*En^8*(-2*(2541 + 4*la + 40*la^2) + (-489 + 36*la)*m^2) + 
           240*En^6*(48921 - 3135*la - 3773*la^2 + 156*la^3 - 
             8*(551 + 111*la + 83*la^2)*m^2 + 2*(536 + 3*la)*m^4) - 
           3*(-6*(-536725 + 979013*la + 204169*la^2 - 223834*la^3 + 9720*
                la^4) + (1864451 - 854292*la - 633836*la^2 + 26200*la^3)*
              m^2 + (25859 - 2800*la - 2584*la^2)*m^4 + 8*m^6) - 
           8*En^4*(2476710 - 2773708*la - 263221*la^2 + 307879*la^3 + 
             5110*la^4 + (2843145 - 614935*la - 357761*la^2 + 1610*la^3)*
              m^2 - 18*(-6067 + 77*la + 70*la^2)*m^4 + 36*m^6) + 
           2*En^2*(13666545 - 23698336*la - 3281400*la^2 + 3930192*la^3 - 
             42952*la^4 + 2*(7567029 - 2264300*la - 1523847*la^2 + 36680*
                la^3)*m^2 + (334809 - 16660*la - 14896*la^2)*m^4 + 
             126*m^6))) + J^4*(79161840*En^8 - 3*(-358911834 + 166194368*la + 
            21819237*la^2 - 5106407*la^3 + 51480*la^4) + 
          (189107037 - 13102291*la - 5271589*la^2 + 48600*la^3)*m^2 + 
          (678033 - 5072*la - 3128*la^2)*m^4 + 36*m^6 - 
          144*En^6*(2430314 - 341609*la + 568663*m^2) + 
          24*En^4*(79525245 - 27675972*la - 1083478*la^2 + 
            (27656356 - 335518*la)*m^2 + 85772*m^4) - 
          12*En^2*(290343337 - 114599972*la - 9133156*la^2 + 1111844*la^3 - 
            4*(-18877505 + 653131*la + 156319*la^2)*m^2 + (301322 - 336*la)*
             m^4 + 12*m^6))*M^2 + 6*J^2*(666068329 + 498036528*En^4 - 
          90849518*la - 3991972*la^2 + (26973869 - 266616*la)*m^2 + 
          11454*m^4 - 3*En^2*(538628656 - 38274739*la + 18842436*m^2))*M^4 + 
        498990834*M^6)*rp[t]^20 - 
      J^2*(J^6*la*(960*En^8*(-56 - 53*la + 3*la^2) - 
          3*(161147 + 37667*la - 115180*la^2 + 8300*la^3) + 
          5*(-37241 - 34901*la + 2340*la^2)*m^2 - 1184*(1 + la)*m^4 + 
          240*En^6*(1 + la)*(535 - 2*la - 35*la^2 + (123 + 23*la)*m^2) - 
          4*En^4*(42666 - 41221*la - 89039*la^2 - 5152*la^3 - 
            30*(-3859 - 3824*la + 35*la^2)*m^2 + 1134*(1 + la)*m^4) + 
          2*En^2*(446067 + 12998*la - 425733*la^2 + 7336*la^3 - 
            5*(-74791 - 71627*la + 3164*la^2)*m^2 + 3388*(1 + la)*m^4)) + 
        J^4*(10229760*En^10 - 144*En^8*(-530797 - 11565*la + 19120*m^2) - 
          24*En^6*(4455417 - 934581*la - 47138*la^2 + (1914371 - 12098*la)*
             m^2 + 15952*m^4) + 3*(41534445 - 33880636*la - 6153697*la^2 + 
            2870151*la^3 - 68520*la^4 + (11685597 - 1800593*la - 
              1042409*la^2 + 22200*la^3)*m^2 + (65991 - 1780*la - 1456*la^2)*
             m^4 + 6*m^6) + 12*En^4*(2*(10476048 - 8991136*la - 685399*la^2 + 
              140542*la^3) + (18864943 - 801086*la - 253236*la^2)*m^2 + 
            (149830 - 56*la)*m^4 + 8*m^6) - 4*En^2*(110955285 - 84937187*la - 
            10628692*la^2 + 3656988*la^3 - 18952*la^4 + 
            2*(27298521 - 2360805*la - 1101334*la^2 + 9380*la^3)*m^2 - 
            4*(-98877 + 874*la + 622*la^2)*m^4 + 36*m^6))*M^2 - 
        6*J^2*(-292657105 + 33669360*En^6 + 72864547*la + 6649509*la^2 - 
          564751*la^3 + 4*(-5306791 + 149621*la + 33121*la^2)*m^2 + 
          4*(-5125 + 6*la)*m^4 - 24*En^4*(23367982 - 2549626*la + 
            1944759*m^2) + 2*En^2*(508338643 - 88778939*la - 3795439*la^2 + 
            (44363783 - 427182*la)*m^2 + 32478*m^4))*M^4 - 
        9*(-118786395 + 196323024*En^2 + 6223120*la - 1022778*m^2)*M^6)*
       rp[t]^21 + M*(J^6*(11520*En^10*(569 + 2*la) + 
          1920*En^8*(9402 + 263*la - 70*la^2 + (-933 + 54*la)*m^2) - 
          48*En^6*(338625 - 18901*la + 4851*la^2 + 1932*la^3 - 
            4*(-25118 + 1739*la + 1287*la^2)*m^2 + 2*(1721 + 6*la)*m^4) + 
          3*(1813770 - 3170366*la - 750864*la^2 + 691076*la^3 - 29910*la^4 + 
            2*(418817 - 177507*la - 133917*la^2 + 5025*la^3)*m^2 + 
            (7150 - 728*la - 674*la^2)*m^4 + m^6) + 
          8*En^4*(968670 - 2723642*la - 198299*la^2 + 277559*la^3 + 
            3248*la^4 + (2991345 - 444621*la - 280157*la^2 + 910*la^3)*m^2 + 
            (48516 - 518*la - 476*la^2)*m^4 + 6*m^6) - 
          2*En^2*(9314910 - 16745207*la - 2864088*la^2 + 2563002*la^3 - 
            29552*la^4 + 2*(4722399 - 1197364*la - 837657*la^2 + 16600*la^3)*
             m^2 + (114111 - 5096*la - 4592*la^2)*m^4 + 18*m^6)) + 
        J^4*(412892478 - 6725808*En^8 - 175620325*la - 25148522*la^2 + 
          5238699*la^3 - 52760*la^4 + (49121085 - 3118982*la - 1294026*la^2 + 
            11000*la^3)*m^2 + (81969 - 576*la - 360*la^2)*m^4 - 
          144*En^6*(-64188 - 337465*la + 489981*m^2) + 
          72*En^4*(17013097 - 5499340*la - 248674*la^2 + (4422394 - 38186*la)*
             m^2 + 5084*m^4) + 6*En^2*(-297844873 + 101750491*la + 
            9336461*la^2 - 928401*la^3 + (-51121253 + 1480810*la + 
              380584*la^2)*m^2 + (-86834 + 84*la)*m^4))*M^2 + 
        9*J^2*(149788488*En^4 - 2*(-51469645 + 6321112*la + 299930*la^2) + 
          (2065249 - 18520*la)*m^2 - 6*En^2*(55921409 - 3248994*la + 
            902788*m^2))*M^4 + 52670250*M^6)*rp[t]^22 + 
      (J^6*la*(-1920*En^8*(-31 - 30*la + la^2) + 48*En^6*(1 + la)*
           (3139 + 380*la + 161*la^2 - 3*(527 + 27*la)*m^2) + 
          5*(3*(18161 + 5415*la - 11894*la^2 + 852*la^3) + 
            (15883 + 14983*la - 900*la^2)*m^2 + 62*(1 + la)*m^4) + 
          4*En^4*(11286 - 70811*la - 85429*la^2 - 3332*la^3 - 
            70*(-1417 - 1408*la + 9*la^2)*m^2 + 434*(1 + la)*m^4) - 
          2*En^2*(343833 + 55018*la - 283515*la^2 + 5300*la^3 + 
            (211825 + 204605*la - 7220*la^2)*m^2 + 1052*(1 + la)*m^4)) + 
        J^4*(-48462219 + 26265600*En^10 + 36316406*la + 7339851*la^2 - 
          2951537*la^3 + 70260*la^4 + (-9247119 + 1306937*la + 775033*la^2 - 
            15100*la^3)*m^2 + 3*(-8027 + 204*la + 168*la^2)*m^4 - 
          144*En^8*(67487 + 3855*la + 21040*m^2) + 
          24*En^6*(-3082965 - 1163951*la - 36878*la^2 + (2074701 - 6318*la)*
             m^2 + 3792*m^4) + 12*En^4*(-2*(5926374 - 5744224*la - 
              545537*la^2 + 77666*la^3) + (-9755425 + 294746*la + 
              103908*la^2)*m^2 + (-27082 + 8*la)*m^4) - 
          2*En^2*(-117651069 + 78033938*la + 11753032*la^2 - 3092679*la^3 + 
            16492*la^4 + (-38262195 + 2775831*la + 1368077*la^2 - 9820*la^3)*
             m^2 + 4*(-28794 + 229*la + 166*la^2)*m^4))*M^2 + 
        6*J^2*(-68828505 + 37156176*En^6 + 15339847*la + 1526173*la^2 - 
          115795*la^3 + (-2464271 + 63235*la + 14535*la^2)*m^2 - 
          12*En^4*(23185786 - 1714446*la + 761899*m^2) - 
          6*En^2*(-54962453 + 7693491*la + 380726*la^2 + 
            (-2171211 + 17330*la)*m^2))*M^4 + 27*(-4233437 + 9415952*En^2 + 
          196112*la)*M^6)*rp[t]^23 + 
      M*(J^4*(2157120 - 3484150*la - 953069*la^2 + 712847*la^3 - 30690*la^4 + 
          57600*En^10*(-229 + 2*la) + (676008 - 262835*la - 201553*la^2 + 
            6850*la^3)*m^2 - 18*(-146 + 14*la + 13*la^2)*m^4 + 
          1920*En^8*(3810 + 283*la + 76*la^2 + (393 + 36*la)*m^2) - 
          48*En^6*(-297705 - 56931*la + 9331*la^2 + 2062*la^3 - 
            8*(-21584 + 577*la + 441*la^2)*m^2 + 2*(421 + la)*m^4) - 
          8*En^4*(35550 + 1971382*la + 270301*la^2 - 160165*la^3 - 
            1288*la^4 + (-1724145 + 179451*la + 120721*la^2 - 290*la^3)*m^2 + 
            (-8940 + 82*la + 76*la^2)*m^4) + 
          2*En^2*(2*(-2568960 + 4083275*la + 932508*la^2 - 550917*la^3 + 
              6572*la^4) + (-3465036 + 736762*la + 533838*la^2 - 8740*la^3)*
             m^2 + 7*(-2403 + 97*la + 88*la^2)*m^4)) - 
        J^2*(-98954232 + 71490384*En^8 + 37421516*la + 5921689*la^2 - 
          1076463*la^3 + 10816*la^4 + (-5777028 + 334435*la + 143085*la^2 - 
            1120*la^3)*m^2 + 144*En^6*(728598 - 185309*la + 156147*m^2) + 
          36*En^4*(-19688476 + 3905599*la + 226656*la^2 + 
            (-1813547 + 11270*la)*m^2) - 6*En^2*(-102188737 + 27225161*la + 
            2972058*la^2 - 230180*la^3 + (-7701957 + 186190*la + 51260*la^2)*
             m^2))*M^2 + 18*(5591716 + 17187924*En^4 - 602238*la - 
          31180*la^2 + 3*En^2*(-8382425 + 374869*la))*M^4)*rp[t]^24 + 
      (J^4*la*(106029 + 40059*la - 61595*la^2 + 4375*la^3 + 
          1920*En^8*(-43 - 42*la + la^2) - 5*(-4023 - 3818*la + 205*la^2)*
           m^2 + 36*(1 + la)*m^4 + 48*En^6*(1 + la)*(5039 + 560*la + 
            91*la^2 - (1281 + 31*la)*m^2) + 4*En^4*(36630 - 14293*la - 
            52271*la^2 - 1348*la^3 + (45590 + 45380*la - 210*la^2)*m^2 + 
            70*(1 + la)*m^4) - 2*En^2*(202623 + 75542*la - 124632*la^2 + 
            2449*la^3 + (69365 + 67450*la - 1915*la^2)*m^2 + 
            142*(1 + la)*m^4)) - J^2*(11920212 + 17971200*En^10 - 
          7883533*la - 1796930*la^2 + 608141*la^3 - 14412*la^4 + 
          20*(55254 - 7124*la - 4326*la^2 + 77*la^3)*m^2 + 
          432*En^8*(-171997 - 2005*la + 7120*m^2) + 
          72*En^6*(246601 + 260403*la + 17678*la^2 + (-245749 + 430*la)*
             m^2) - 12*En^4*(-9547497 + 4443091*la + 598403*la^2 - 
            49745*la^3 + 4*(-529723 + 11700*la + 4540*la^2)*m^2) - 
          6*En^2*(14646879 - 7273208*la - 1360276*la^2 + 258947*la^3 - 
            1404*la^4 + (1984131 - 120615*la - 62565*la^2 + 380*la^3)*m^2))*
         M^2 + 6*(25990848*En^6 + 144*En^4*(-499148 + 21449*la) - 
          6*En^2*(-8691014 + 909146*la + 53105*la^2) + 
          5*(-1526502 + 295371*la + 32389*la^2 - 2161*la^3))*M^4)*rp[t]^25 + 
      M*(J^2*(551520 - 781727*la - 249312*la^2 + 147395*la^3 - 6300*la^4 + 
          11520*En^10*(653 + 2*la) + 10*(8244 - 2923*la - 2277*la^2 + 
            70*la^3)*m^2 + 17280*En^8*(-1016 - 10*la + 3*la^2 + 
            (71 + la)*m^2) + 48*En^6*(191085 + 66523*la + 9813*la^2 - 
            1060*la^3 + 60*(-1186 + 19*la + 15*la^2)*m^2) + 
          8*En^4*(596880 - 903986*la - 231029*la^2 + 53324*la^3 + 287*la^4 + 
            5*(80739 - 6181*la - 4382*la^2 + 8*la^3)*m^2) - 
          6*En^2*(744120 - 825261*la - 252211*la^2 + 93830*la^3 - 1140*la^4 + 
            20*(9360 - 1671*la - 1249*la^2 + 17*la^3)*m^2)) + 
        3*(3758848 + 11558448*En^8 - 1219065*la - 215486*la^2 + 33563*la^3 - 
          336*la^4 + 72*En^6*(-738453 + 27977*la) - 
          12*En^4*(-6051768 + 621521*la + 47570*la^2) + 
          En^2*(-34762616 + 6655284*la + 877306*la^2 - 51430*la^3))*M^2)*
       rp[t]^26 + (J^2*la*(1 + la)*(25725 + 2880*En^8*(-18 + la) - 13696*la + 
          899*la^2 - 15*(-153 + 7*la)*m^2 - 8*En^4*(-24000 + 9004*la + 
            154*la^2 + 15*(-289 + la)*m^2) - 48*En^6*(195 - 334*la - 
            29*la^2 + 5*(71 + la)*m^2) + 6*En^2*(-26105 + 11076*la - 
            219*la^2 + 25*(-133 + 3*la)*m^2)) + 
        (-1411128 + 2764800*En^10 + 788635*la + 204490*la^2 - 57057*la^3 + 
          1344*la^4 + 432*En^8*(-54201 + 1207*la) - 
          144*En^6*(-363509 + 34499*la + 4084*la^2) - 
          12*En^4*(3908505 - 774101*la - 144649*la^2 + 7015*la^3) - 
          12*En^2*(-1384827 + 468245*la + 108789*la^2 - 14655*la^3 + 
            80*la^4))*M^2)*rp[t]^27 - 6*(5760*En^10*(29 + la) + 
        2880*En^8*(-220 + 15*la + 4*la^2) + 8*En^6*(113715 - 24358*la - 
          8078*la^2 + 215*la^3) + En^2*(166680 - 116687*la - 45634*la^2 + 
          10801*la^3 - 132*la^4) - 4*En^4*(149580 - 62246*la - 23392*la^2 + 
          2593*la^3 + 9*la^4) + 2*(-5760 + 6791*la + 2525*la^2 - 1157*la^3 + 
          49*la^4))*M*rp[t]^28 + 12*(-1 + En^2)*la*(-245 - 137*la + 
        101*la^2 - 7*la^3 + 80*En^6*(22 + 23*la + la^2) + 
        8*En^4*(-450 - 403*la + 49*la^2 + 2*la^3) + 
        2*En^2*(1045 + 773*la - 269*la^2 + 3*la^3))*rp[t]^29))/
    (En*la*(1 + la)*rp[t]^16*(J^2 + rp[t]^2)^11))*Derivative[1][rp][t]
]


Clear[fSourceJT5]
fSourceJT5[syms_Association]:=
Module[{mu,M,CapLa,J,la,YBar,YPhiBar,YPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	YBar=YSymbol[syms,Conjugate->True][t];
	YPhiBar=YPhiSymbol[syms,Conjugate->True][t];
	YPhiPhiBar=YPhiPhiSymbol[syms,Conjugate->True][t];
	
(* Created with the Wolfram Language : www.wolfram.com *)
((8*I)*J^3*m*mu*Pi*YPhiPhiBar*(2*M - rp[t])^3*(590058*J^18*M^6 - 
    9*J^18*(126833 - 4408*la + 1772*m^2)*M^5*rp[t] + 
    6*J^16*M^4*(J^2*(143388 - 12861*la - 1409*la^2 + (4598 - 24*la)*m^2 + 
        4*m^4) + 952047*M^2)*rp[t]^2 + 
    J^16*M^3*(J^2*(-313587 + 57581*la + 13031*la^2 - 480*la^3 + 
        (-17655 + 376*la + 160*la^2)*m^2 - 36*m^4) + 
      9*(-1230285 + 262170*En^2 + 41304*la - 15536*m^2)*M^2)*rp[t]^3 + 
    2*J^14*M^2*(J^4*(27354 - 10135*la - 3619*la^2 + 330*la^3 + 
        (2469 - 164*la - 110*la^2)*m^2 + 9*m^4) - 
      3*J^2*(-1393982 + 121163*la + 13607*la^2 + 12*(-3377 + 16*la)*m^2 - 
        28*m^4 + 12*En^2*(47307 - 2324*la + 1196*m^2))*M^2 + 12367620*M^4)*
     rp[t]^4 + J^14*M*(J^4*(-3594 + 3266*la + 1688*la^2 - 300*la^3 + 
        (-507 + 118*la + 100*la^2)*m^2 - 3*m^4) + 
      J^2*(-3056460 + 545813*la + 126383*la^2 - 4440*la^3 + 
        4*(-39135 + 772*la + 340*la^2)*m^2 - 252*m^4 + 
        6*En^2*(293252 - 38755*la - 4131*la^2 - 52*(-343 + 3*la)*m^2 + 
          36*m^4))*M^2 + 36*(-1335061 + 596520*En^2 + 43056*la - 14924*m^2)*
       M^4)*rp[t]^5 - J^12*(15*J^6*la*(1 + la)*(12 - 3*la + m^2) + 
      J^4*(-534840 + 193555*la + 70579*la^2 - 6120*la^3 + 
        2*(-22047 + 1372*la + 940*la^2)*m^2 - 126*m^4 + 
        6*En^2*(63092 - 18579*la - 4331*la^2 + 260*la^3 - 
          2*(-3601 + 138*la + 60*la^2)*m^2 + 36*m^4))*M^2 - 
      24*J^2*(76437*En^4 - 2*(-758416 + 63541*la + 7339*la^2) - 
        7*(-5593 + 24*la)*m^2 + 21*m^4 - 6*En^2*(216315 - 10184*la + 
          4951*m^2))*M^4 - 62949096*M^6)*rp[t]^6 + 
    J^12*M*(J^4*(-35265 + 31483*la + 16579*la^2 - 2790*la^3 + 
        2*(-2283 + 502*la + 430*la^2)*m^2 - 21*m^4 + 
        2*En^2*(13851 - 10324*la - 4162*la^2 + 690*la^3 + 
          (2808 - 437*la - 320*la^2)*m^2 + 27*m^4)) - 
      12*J^2*(1112083 - 192184*la - 45674*la^2 + 1520*la^3 - 
        14*(-3625 + 66*la + 30*la^2)*m^2 + 63*m^4 + 
        3*En^4*(48485 - 3604*la + 2126*m^2) + En^2*(-1348561 + 171550*la + 
          18846*la^2 + (-74666 + 546*la)*m^2 - 108*m^4))*M^2 + 
      36*(2407362*En^2 - 7*(486855 - 14976*la + 4684*m^2))*M^4)*rp[t]^7 + 
    2*J^10*(5*J^6*la*(1 + la)*(-176 + 42*la - 13*m^2 + 
        2*En^2*(53 - 15*la + 7*m^2)) + J^4*(1171887 - 412435*la - 
        154027*la^2 + 12600*la^3 - 7*(-12351 + 716*la + 500*la^2)*m^2 + 
        189*m^4 + 12*En^4*(20808 - 4647*la - 557*la^2 + (2420 - 42*la)*m^2 + 
          12*m^4) - 6*En^2*(292143 - 83244*la - 19956*la^2 + 1120*la^3 - 
          2*(-15281 + 513*la + 240*la^2)*m^2 + 108*m^4))*M^2 + 
      12*J^2*(655569*En^4 - 6*En^2*(877843 - 39232*la + 17708*m^2) + 
        7*(555054 - 44512*la - 5308*la^2 + (12361 - 48*la)*m^2 + 5*m^4))*
       M^4 + 51942870*M^6)*rp[t]^8 + 
    J^10*M*(J^4*(-155223 + 135683*la + 72989*la^2 - 11520*la^3 + 
        7*(-2583 + 532*la + 460*la^2)*m^2 - 63*m^4 - 
        8*En^4*(5004 - 3221*la - 933*la^2 + 110*la^3 - 
          3*(-429 + 41*la + 20*la^2)*m^2 + 18*m^4) + 
        2*En^2*(129384 - 93973*la - 38869*la^2 + 6000*la^3 + 
          (24246 - 3419*la - 2600*la^2)*m^2 + 162*m^4)) + 
      4*J^2*(103518*En^6 - 9*En^4*(420629 - 29884*la + 16836*m^2) - 
        7*(2*(612840 - 101801*la - 24926*la^2 + 780*la^3) + 
          (48405 - 812*la - 380*la^2)*m^2 + 45*m^4) + 
        3*En^2*(5508531 - 669110*la - 76278*la^2 - 6*(-45133 + 273*la)*m^2 + 
          270*m^4))*M^2 + 18*(-11293359 + 11296224*En^2 + 328440*la - 
        90020*m^2)*M^4)*rp[t]^9 - 
    J^8*(5*J^6*la*(1 + la)*(1543 - 348*la + 98*m^2 + 
        8*En^4*(28 - 9*la + 5*m^2) - 8*En^2*(247 - 66*la + 29*m^2)) + 
      2*J^4*(36*En^6*(2659 - 372*la + 248*m^2) - 
        7*(432525 - 147125*la - 56477*la^2 + 4320*la^3 + 
          (27717 - 1492*la - 1060*la^2)*m^2 + 45*m^4) - 
        6*En^4*(366265 - 78564*la - 9664*la^2 - 72*(-545 + 7*la)*m^2 + 
          120*m^4) + 6*En^2*(1202789 - 329592*la - 81728*la^2 + 4200*la^3 - 
          3*(-37597 + 1086*la + 540*la^2)*m^2 + 270*m^4))*M^2 - 
      12*J^2*(4931514*En^4 - 12*En^2*(2073119 - 86600*la + 35205*m^2) + 
        7*(1847792 - 140555*la - 17375*la^2 - 60*(-569 + 2*la)*m^2 + 10*m^4))*
       M^4 - 115413228*M^6)*rp[t]^10 + 
    J^8*M*(J^4*(48*En^6*(317 - 187*la - 31*la^2 + (104 - 6*la)*m^2 + 2*m^4) - 
        7*(57606 - 49063*la - 27049*la^2 + 3960*la^3 - 
          4*(-1464 + 281*la + 245*la^2)*m^2 + 15*m^4) - 
        8*En^4*(44847 - 27941*la - 8283*la^2 + 920*la^3 - 
          6*(-1797 + 143*la + 80*la^2)*m^2 + 90*m^4) + 
        6*En^2*(6*(29891 - 21068*la - 8984*la^2 + 1260*la^3) + 
          (30516 - 3779*la - 2960*la^2)*m^2 + 135*m^4)) + 
      2*J^2*(1723464*En^6 - 18*En^4*(1604197 - 108420*la + 57890*m^2) - 
        7*(4102323 - 648935*la - 164345*la^2 + 4800*la^3 - 
          5*(-26925 + 416*la + 200*la^2)*m^2 + 90*m^4) + 
        6*En^2*(13103741 - 1499490*la - 179810*la^2 - 130*(-4201 + 21*la)*
           m^2 + 360*m^4))*M^2 + 18*(-12612019 + 16966710*En^2 + 343224*la - 
        79016*m^2)*M^4)*rp[t]^11 + 
    2*J^6*(J^6*la*(1 + la)*(-525*(19 - 4*la + m^2) + 
        16*En^6*(7 - 3*la + 2*m^2) - 120*En^4*(42 - 13*la + 7*m^2) + 
        30*En^2*(682 - 168*la + 67*m^2)) + 
      J^4*(6048*En^8 - 216*En^6*(3811 - 500*la + 320*m^2) + 
        36*En^4*(237397 - 48735*la - 6065*la^2 - 30*(-780 + 7*la)*m^2 + 
          40*m^4) + 7*(728502 - 237235*la - 93955*la^2 + 6660*la^3 - 
          5*(-7773 + 388*la + 280*la^2)*m^2 + 45*m^4) - 
        6*En^2*(2883587 - 753000*la - 195920*la^2 + 8960*la^3 - 
          10*(-23156 + 573*la + 300*la^2)*m^2 + 360*m^4))*M^2 + 
      6*J^2*(10542690*En^4 - 120*En^2*(314473 - 11962*la + 4183*m^2) + 
        7*(-7*(-296606 + 21167*la + 2723*la^2) + (30142 - 96*la)*m^2 + 
          6*m^4))*M^4 + 43183476*M^6)*rp[t]^12 + 
    J^6*M*(J^4*(-35*(19560 - 16081*la - 9115*la^2 + 1224*la^3 + 
          (1659 - 296*la - 260*la^2)*m^2 + 3*m^4) + 
        96*En^6*(1411 - 800*la - 140*la^2 - 5*(-86 + 3*la)*m^2 + 4*m^4) - 
        120*En^4*(12012 - 7125*la - 2105*la^2 + 214*la^3 + 
          (2727 - 163*la - 100*la^2)*m^2 + 12*m^4) + 
        10*En^2*(259884 - 178027*la - 79435*la^2 + 9744*la^3 + 
          (38466 - 4139*la - 3320*la^2)*m^2 + 108*m^4)) + 
      6*J^2*(2136204*En^6 - 30*En^4*(689545 - 43996*la + 21144*m^2) - 
        7*(1547896 - 230433*la - 60583*la^2 + 1640*la^3 + 
          (39890 - 568*la - 280*la^2)*m^2 + 18*m^4) + 
        10*En^2*(4022011 - 421725*la - 53981*la^2 + (131546 - 546*la)*m^2 + 
          54*m^4))*M^2 + 36*(8391720*En^2 - 7*(678615 - 17088*la + 3092*m^2))*
       M^4)*rp[t]^13 + J^4*(5*J^6*la*(1 + la)*(64*En^6*(7 - 3*la + 2*m^2) - 
        72*En^4*(104 - 31*la + 15*m^2) - 7*(959 - 186*la + 40*m^2) + 
        8*En^2*(2519 - 546*la + 190*m^2)) + 
      2*J^4*(60480*En^8 - 36*En^6*(93469 - 11340*la + 7440*m^2) + 
        7*(832242 - 256130*la - 105002*la^2 + 6840*la^3 + 
          (34827 - 1612*la - 1180*la^2)*m^2 + 27*m^4) + 
        30*En^4*(610179 - 122896*la - 16316*la^2 - 112*(-475 + 3*la)*m^2 + 
          48*m^4) - 30*En^2*(897477 - 216879*la - 60111*la^2 + 2380*la^3 + 
          (56707 - 1206*la - 660*la^2)*m^2 + 54*m^4))*M^2 + 
      24*J^2*(7241895*En^4 - 6*En^2*(3158161 - 105752*la + 29633*m^2) + 
        7*(788244 - 52136*la - 7004*la^2 + (8303 - 24*la)*m^2 + m^4))*M^4 + 
      41980680*M^6)*rp[t]^14 + 
    J^4*M*(J^4*(-7*(113067 - 88453*la - 51679*la^2 + 6300*la^3 - 
          2*(-3753 + 622*la + 550*la^2)*m^2 + 9*m^4) + 
        48*En^6*(15213 - 6505*la - 805*la^2 - 60*(-68 + la)*m^2 + 12*m^4) - 
        40*En^4*(74367 - 47279*la - 14937*la^2 + 1220*la^3 - 
          12*(-1359 + 61*la + 40*la^2)*m^2 + 36*m^4) + 
        10*En^2*(411255 - 265738*la - 125854*la^2 + 13020*la^3 + 
          (48096 - 4499*la - 3680*la^2)*m^2 + 81*m^4)) + 
      4*J^2*(5544504*En^6 - 45*En^4*(968507 - 53452*la + 21258*m^2) - 
        7*(1780275 - 246086*la - 67376*la^2 + 1680*la^3 - 
          4*(-8295 + 109*la + 55*la^2)*m^2 + 9*m^4) + 
        3*En^2*(20584851 - 1902690*la - 263138*la^2 - 6*(-78613 + 273*la)*
           m^2 + 108*m^4))*M^2 + 36*(-2327661 + 5337450*En^2 + 53616*la - 
        6764*m^2)*M^4)*rp[t]^15 + 
    2*(-5*J^8*la*(1 + la)*(-48*En^6*(3 - 7*la + 4*m^2) + 
        80*En^4*(115 - 27*la + 11*m^2) + 7*(544 - 96*la + 17*m^2) - 
        10*En^2*(1616 - 294*la + 85*m^2)) + 
      J^6*(589248*En^8 - 144*En^6*(33359 - 5700*la + 3320*m^2) + 
        7*(646125 - 185095*la - 78751*la^2 + 4680*la^3 + 
          (19461 - 836*la - 620*la^2)*m^2 + 9*m^4) + 
        60*En^4*(439550 - 77981*la - 11671*la^2 - 18*(-1530 + 7*la)*m^2 + 
          12*m^4) - 6*En^2*(4711021 - 1006092*la - 300388*la^2 + 10080*la^3 - 
          6*(-34391 + 633*la + 360*la^2)*m^2 + 108*m^4))*M^2 + 
      12*J^4*(2730182 + 6526827*En^4 - 165302*la - 23258*la^2 + 
        (18261 - 48*la)*m^2 + m^4 - 6*En^2*(2049965 - 58384*la + 11576*m^2))*
       M^4 + 6010173*J^2*M^6)*rp[t]^16 + 
    J^2*M*(J^4*(-5760*En^8*(74 - 6*la + 7*m^2) + 
        192*En^6*(1343 - 3720*la - 540*la^2 - 5*(-406 + 3*la)*m^2 + 2*m^4) - 
        7*(89265 - 65279*la - 39377*la^2 + 4320*la^3 + 
          (4233 - 652*la - 580*la^2)*m^2 + 3*m^4) - 
        40*En^4*(109818 - 64583*la - 23199*la^2 + 1370*la^3 - 
          3*(-5847 + 203*la + 140*la^2)*m^2 + 18*m^4) + 
        6*En^2*(746292 - 428993*la - 217409*la^2 + 18480*la^3 + 
          (59406 - 4859*la - 4040*la^2)*m^2 + 54*m^4)) + 
      4*J^2*(-6241524 + 7647426*En^6 + 790172*la + 225842*la^2 - 5160*la^3 + 
        (-73425 + 892*la + 460*la^2)*m^2 - 9*m^4 - 
        9*En^4*(4591975 - 193524*la + 55556*m^2) + 
        3*En^2*(13717921 - 1074550*la - 161846*la^2 + (186266 - 546*la)*m^2 + 
          18*m^4))*M^2 + 9*(-2690905 + 7941888*En^2 + 56088*la - 3692*m^2)*
       M^4)*rp[t]^17 + 
    (5*J^6*la*(1 + la)*(1152*En^8 + 128*En^6*(23 - 9*la + 4*m^2) - 
        21*(277 - 44*la + 6*m^2) - 120*En^4*(260 - 41*la + 13*m^2) + 
        24*En^2*(1423 - 210*la + 47*m^2)) - 
      2*J^4*(-2301087 + 711936*En^8 + 604085*la + 267197*la^2 - 14400*la^3 + 
        (-43377 + 1732*la + 1300*la^2)*m^2 - 9*m^4 + 
        108*En^6*(68223 - 8260*la + 3560*m^2) - 
        18*En^4*(1501637 - 197788*la - 34328*la^2 - 24*(-2045 + 7*la)*m^2 + 
          8*m^4) + 6*En^2*(3254503 - 586044*la - 189556*la^2 + 5320*la^3 - 
          13*(-6349 + 102*la + 60*la^2)*m^2 + 18*m^4))*M^2 + 
      6*J^2*(3192212 + 13931676*En^4 - 174881*la - 25829*la^2 + 
        (10018 - 24*la)*m^2 - 24*En^2*(780853 - 18392*la + 1923*m^2))*M^4 + 
      1543050*M^6)*rp[t]^18 + 
    M*(J^4*(-324768 + 241920*En^10 + 217963*la + 135829*la^2 - 13320*la^3 + 
        4*(-2379 + 341*la + 305*la^2)*m^2 - 3*m^4 - 
        5760*En^8*(-233 - 18*la + 14*m^2) + 48*En^6*(14453 - 18805*la - 
          3985*la^2 + (6920 - 30*la)*m^2 + 2*m^4) - 
        24*En^4*(216813 - 89217*la - 36991*la^2 + 1520*la^3 - 
          2*(-8037 + 223*la + 160*la^2)*m^2 + 6*m^4) + 
        2*En^2*(4*(407961 - 196537*la - 106711*la^2 + 7350*la^3) + 
          (72396 - 5219*la - 4400*la^2)*m^2 + 27*m^4)) + 
      3*J^2*(-2467225 + 10388016*En^6 + 282467*la + 84417*la^2 - 1760*la^3 + 
        (-13505 + 152*la + 80*la^2)*m^2 - 12*En^4*(2610639 - 77164*la + 
          11806*m^2) - 4*En^2*(-5387071 + 346910*la + 57118*la^2 + 
          (-31238 + 78*la)*m^2))*M^2 + 9*(-349013 + 1312458*En^2 + 6520*la)*
       M^4)*rp[t]^19 + 2*(5*J^4*la*(1 + la)*(-1439 + 384*En^8 + 204*la - 
        19*m^2 - 72*En^4*(226 - 23*la + 5*m^2) + 
        16*En^6*(341 - 33*la + 10*m^2) + 2*En^2*(5714 - 672*la + 103*m^2)) + 
      2*J^2*(347385 + 324432*En^8 - 82400*la - 37922*la^2 + 1845*la^3 + 
        (3012 - 112*la - 85*la^2)*m^2 - 36*En^6*(142265 - 6876*la + 
          1584*m^2) - 6*En^4*(-1405635 + 124747*la + 25197*la^2 + 
          2*(-7960 + 21*la)*m^2) + 3*En^2*(-1331649 + 195552*la + 
          68568*la^2 - 1600*la^3 + 2*(-6998 + 99*la + 60*la^2)*m^2))*M^2 + 
      3*(419238 + 3253068*En^4 - 20575*la - 3195*la^2 + 
        12*En^2*(-264513 + 5060*la))*M^4)*rp[t]^20 + 
    M*(J^2*(-100512 - 806400*En^10 + 60911*la + 39203*la^2 - 3420*la^3 + 
        2*(-666 + 89*la + 80*la^2)*m^2 - 5760*En^8*(-58 - 18*la + 7*m^2) - 
        96*En^6*(-30717 + 6056*la + 1748*la^2 + 3*(-354 + la)*m^2) + 
        8*En^4*(-474102 + 122861*la + 57993*la^2 - 1670*la^3 + 
          9*(-1183 + 27*la + 20*la^2)*m^2) - 2*En^2*(-708408 + 275569*la + 
          159697*la^2 - 8880*la^3 + (-12438 + 797*la + 680*la^2)*m^2)) + 
      3*(-329084 + 4322664*En^6 + 33715*la + 10545*la^2 - 200*la^3 + 
        12*En^4*(-645527 + 13060*la) - 2*En^2*(-1882182 + 97955*la + 
          17635*la^2))*M^2)*rp[t]^21 + 
    (-(J^2*la*(1 + la)*(9600*En^8 + 25*(167 - 21*la + m^2) - 
         192*En^6*(317 - 13*la + 2*m^2) - 40*En^2*(1093 - 102*la + 8*m^2) + 
         40*En^4*(2272 - 153*la + 17*m^2))) + 
      3*(63016 + 1263744*En^8 - 13355*la - 6395*la^2 + 280*la^3 + 
        120*En^6*(-28007 + 612*la) - 4*En^4*(-750697 + 44600*la + 
          10380*la^2) + En^2*(-968708 + 114270*la + 43310*la^2 - 840*la^3))*
       M^2)*rp[t]^22 + 10*(-1404 + 40320*En^10 + 759*la + 504*la^2 - 
      39*la^3 + 1728*En^8*(-83 + 2*la) - 24*En^6*(-7929 + 619*la + 
        223*la^2) + 6*En^2*(4536 - 1409*la - 866*la^2 + 39*la^3) - 
      4*En^4*(28251 - 4777*la - 2511*la^2 + 52*la^3))*M*rp[t]^23 - 
    60*(-1 + En^2)*la*(-9 - 8*la + la^2 + 128*En^6*(1 + la) + 
      8*En^4*(-29 - 28*la + la^2) - 8*En^2*(-14 - 13*la + la^2))*rp[t]^24))/
  (En^2*la*(1 + la)*rp[t]^17*(J^2 + rp[t]^2)^9) - 
 (8*J*mu*Pi*YPhiBar*(2*M - rp[t])^3*(310464*J^18*M^6 + 
    18*J^18*(-33257 + 391*la)*M^5*rp[t] + 
    3*J^16*M^4*(J^2*(150227 - 5735*la - 1722*la^2) + 931392*M^2)*rp[t]^2 + 
    J^16*M^3*(J^2*(-164696 + 15783*la + 7999*la^2 - 160*la^3) + 
      18*(-298775 + 59805*En^2 + 4057*la)*M^2)*rp[t]^3 + 
    J^14*M^2*(2*J^4*(14495 - 3413*la - 2246*la^2 + 110*la^3) + 
      3*J^2*(1346659 - 56975*la - 15474*la^2 + 12*En^2*(-43477 + 2*la))*M^2 + 
      11176704*M^4)*rp[t]^4 + 
    J^14*M*(-4*J^4*(486 - 347*la - 268*la^2 + 25*la^3) - 
      3*J^2*(490789 - 50572*la - 23881*la^2 + 520*la^3 + 
        En^2*(-273718 + 4138*la + 4338*la^2))*M^2 + 
      144*(-149099 + 62574*En^2 + 2317*la)*M^4)*rp[t]^5 + 
    J^12*(15*J^6*la*(-7 - 6*la + la^2) + J^4*(258285 - 63845*la - 
        40054*la^2 + 2140*la^3 - 6*En^2*(30353 - 2305*la - 2374*la^2 + 
          20*la^3))*M^2 + 12*J^2*(63792*En^4 + 3*En^2*(-363649 + 1202*la) - 
        2*(-670429 + 31375*la + 7724*la^2))*M^4 + 26078976*M^6)*rp[t]^6 + 
    J^12*M*(J^4*(-17244 + 12665*la + 9499*la^2 - 970*la^3 + 
        4*En^2*(3537 - 1232*la - 1234*la^2 + 25*la^3)) - 
      6*J^2*(974431 - 108129*la - 47516*la^2 + 1124*la^3 + 
        6*En^4*(20755 + 324*la) + En^2*(-1143519 + 25574*la + 18114*la^2))*
       M^2 + 72*(-694348 + 460173*En^2 + 12260*la)*M^4)*rp[t]^7 + 
    J^10*(-5*J^6*la*(1 + la)*(187 - 29*la + 2*En^2*(-55 + 2*la)) + 
      J^4*(1021695 - 265915*la - 158630*la^2 + 9236*la^3 + 
        En^4*(225360 + 2796*la - 6492*la^2) - 6*En^2*(253253 - 22330*la - 
          19749*la^2 + 240*la^3))*M^2 + 
      12*J^2*(498276*En^4 + 3*En^2*(-1336111 + 9250*la) - 
        20*(-155702 + 8049*la + 1799*la^2))*M^4 + 39118464*M^6)*rp[t]^8 + 
    J^10*M*(J^4*(-67869 + 51430*la + 37361*la^2 - 4178*la^3 + 
        4*En^4*(-5067 + 587*la + 1029*la^2 + 10*la^3) + 
        6*En^2*(19638 - 7213*la - 6806*la^2 + 200*la^3)) + 
      6*J^2*(-2255535 + 26196*En^6 + 270037*la + 110264*la^2 - 2828*la^3 - 
        6*En^4*(162671 + 1690*la) + En^2*(4196569 - 128130*la - 66450*la^2))*
       M^2 + 36*(-2078369 + 1943760*En^2 + 41455*la)*M^4)*rp[t]^9 - 
    J^8*(J^6*la*(3697 + 3074*la - 623*la^2 + 20*En^4*(28 + 29*la + la^2) + 
        30*En^2*(-153 - 145*la + 8*la^2)) + 
      J^4*(-2354685 + 647489*la + 366226*la^2 - 23212*la^3 + 
        144*En^6*(546 + 31*la) + 300*En^4*(-5912 - 7*la + 169*la^2) + 
        6*En^2*(927849 - 94844*la - 72143*la^2 + 1176*la^3))*M^2 - 
      6*J^2*(9294595 + 3372624*En^4 - 530255*la - 107730*la^2 + 
        6*En^2*(-2819609 + 30970*la))*M^4 - 39118464*M^6)*rp[t]^10 + 
    J^8*M*(J^4*(-155502 + 122027*la + 85603*la^2 - 10486*la^3 + 
        En^6*(7776 + 624*la - 672*la^2) + 60*En^4*(-2673 + 355*la + 
          537*la^2 + 2*la^3) + 4*En^2*(107685 - 41941*la - 37037*la^2 + 
          1484*la^3)) + 6*J^2*(-3353635 + 197208*En^6 + 434086*la + 
        164437*la^2 - 4564*la^3 - 6*En^4*(552931 + 2580*la) + 
        En^2*(8843119 - 353270*la - 140050*la^2))*M^2 + 
      36*(-2073331 + 2583315*En^2 + 46493*la)*M^4)*rp[t]^11 + 
    J^6*(J^6*la*(16*En^6*(7 + 8*la + la^2) - 20*En^4*(224 + 229*la + 
          5*la^2) + 7*(-1217 - 994*la + 223*la^2) - 
        4*En^2*(-4199 - 3898*la + 301*la^2)) + 
      J^4*(3483735 + 3888*En^8 - 1015963*la - 543158*la^2 + 37436*la^3 - 
        72*En^6*(8365 + 382*la) - 12*En^4*(-505746 + 5305*la + 14245*la^2) - 
        6*En^2*(1950259 - 232334*la - 151493*la^2 + 3136*la^3))*M^2 + 
      6*J^2*(9243071 + 6420600*En^4 - 581555*la - 107506*la^2 + 
        30*En^2*(-749149 + 11606*la))*M^4 + 26078976*M^6)*rp[t]^12 + 
    J^6*M*(J^4*(-228483 + 186484*la + 125909*la^2 - 16898*la^3 - 
        144*En^6*(-421 - 24*la + 37*la^2) - 12*En^4*(46350 - 6691*la - 
          8977*la^2 + 54*la^3) + 2*En^2*(450765 - 188837*la - 154504*la^2 + 
          8008*la^3)) + 6*J^2*(-3321236 + 641484*En^6 + 465813*la + 
        163429*la^2 - 4900*la^3 + 30*En^4*(-210827 + 606*la) - 
        5*En^2*(-2346555 + 118932*la + 37132*la^2))*M^2 + 
      288*(-172325 + 277095*En^2 + 4327*la)*M^4)*rp[t]^13 + 
    J^4*(J^6*la*(84*En^4*(-179 - 178*la + la^2) + 
        144*En^6*(7 + 8*la + la^2) + 7*(-1801 - 1442*la + 359*la^2) - 
        4*En^2*(-8849 - 8023*la + 826*la^2)) + 
      J^4*(3430515 + 34992*En^8 - 1065431*la - 536662*la^2 + 40180*la^3 - 
        936*En^6*(2209 + 92*la) - 180*En^4*(-64218 + 1821*la + 1823*la^2) - 
        30*En^2*(516053 - 71842*la - 40071*la^2 + 1008*la^3))*M^2 + 
      12*J^2*(3761280*En^4 + 3*En^2*(-3216107 + 65462*la) - 
        4*(-765599 + 53100*la + 8939*la^2))*M^4 + 11176704*M^6)*rp[t]^14 + 
    J^4*M*(J^4*(-223164 + 190403*la + 123277*la^2 - 18130*la^3 - 
        48*En^6*(-5043 - 439*la + 329*la^2) - 20*En^4*(52614 - 9407*la - 
          10305*la^2 + 182*la^3) + 120*En^2*(9882 - 4514*la - 3388*la^2 + 
          217*la^3)) + 6*J^2*(-2190557 + 1077456*En^6 + 333671*la + 
        108248*la^2 - 3500*la^3 + 30*En^4*(-247885 + 3092*la) + 
        En^2*(10072229 - 629314*la - 158614*la^2))*M^2 + 
      72*(-294578 + 600873*En^2 + 8254*la)*M^4)*rp[t]^15 + 
    (J^8*la*(288*En^6*(8 + 9*la + la^2) - 390*En^2*(-121 - 107*la + 
          14*la^2) + 20*En^4*(-1463 - 1402*la + 61*la^2) + 
        35*(-355 - 278*la + 77*la^2)) + J^6*(2247885 + 225504*En^8 - 
        746785*la - 353234*la^2 + 28700*la^3 - 144*En^6*(23149 + 662*la) - 
        60*En^4*(-226424 + 12173*la + 6499*la^2) - 
        6*En^2*(2211363 - 357910*la - 171079*la^2 + 5040*la^3))*M^2 + 
      12*J^4*(2750652*En^4 + 3*En^2*(-1747549 + 44342*la) - 
        2*(-651899 + 49825*la + 7644*la^2))*M^4 + 2794176*J^2*M^6)*rp[t]^16 + 
    J^2*M*(J^4*(-144819 + 129910*la + 80339*la^2 - 12950*la^3 - 
        2880*En^8*(25 + 4*la) - 96*En^6*(-3486 - 248*la + 313*la^2) - 
        60*En^4*(20349 - 4891*la - 4125*la^2 + 118*la^3) + 
        2*En^2*(505584 - 253249*la - 173138*la^2 + 13160*la^3)) + 
      6*J^2*(-927761 + 1144044*En^6 + 153839*la + 46076*la^2 - 1604*la^3 + 
        6*En^4*(-918565 + 20946*la) + En^2*(5484539 - 409854*la - 
          85294*la^2))*M^2 + 18*(-293677 + 753792*En^2 + 9155*la)*M^4)*
     rp[t]^17 + (J^6*la*(960*En^8*(1 + la) + En^2*(40870 + 35270*la - 
          5600*la^2) + 32*En^6*(148 + 149*la + la^2) + 
        35*(-233 - 178*la + 55*la^2) + 20*En^4*(-1834 - 1697*la + 
          137*la^2)) + J^4*(944895 + 21600*En^8 - 337355*la - 149350*la^2 + 
        13156*la^3 + 144*En^6*(-24491 + 157*la) - 
        12*En^4*(-851580 + 69487*la + 23851*la^2) - 
        6*En^2*(1205783 - 223540*la - 92149*la^2 + 3080*la^3))*M^2 + 
      3*J^2*(4680864*En^4 + 12*En^2*(-550651 + 16670*la) - 
        5*(-258883 + 21799*la + 3050*la^2))*M^4 + 310464*M^6)*rp[t]^18 + 
    M*(J^4*(-60174 + 34560*En^10 + 57125*la + 33601*la^2 - 5938*la^3 - 
        8640*En^8*(-11 + 2*la) - 144*En^6*(-2176 + 207*la + 268*la^2) - 
        12*En^4*(77589 - 23689*la - 15523*la^2 + 570*la^3) + 
        4*En^2*(137727 - 75137*la - 46669*la^2 + 4060*la^3)) + 
      3*J^2*(-457850 + 1585584*En^6 + 82839*la + 22873*la^2 - 856*la^3 + 
        36*En^4*(-133827 + 4220*la) + En^2*(3475098 - 300740*la - 
          52780*la^2))*M^2 + 54*(-10841 + 34935*En^2 + 375*la)*M^4)*
     rp[t]^19 + (J^4*la*(-3437 - 2554*la + 883*la^2 + 960*En^8*(1 + la) - 
        144*En^6*(-61 - 58*la + 3*la^2) + 60*En^4*(-490 - 443*la + 47*la^2) - 
        20*En^2*(-1124 - 949*la + 175*la^2)) + 
      J^2*(231135 + 212976*En^8 - 89119*la - 36806*la^2 + 3512*la^3 + 
        72*En^6*(-38665 + 1354*la) - 60*En^4*(-77298 + 7939*la + 1987*la^2) - 
        6*En^2*(384389 - 79714*la - 28623*la^2 + 1056*la^3))*M^2 + 
      3*(889488*En^4 + 24*En^2*(-38567 + 1340*la) - 
        5*(-28539 + 2647*la + 338*la^2))*M^4)*rp[t]^20 + 
    M*(J^2*(-14517 + 43200*En^8 - 92160*En^10 + 14692*la + 8183*la^2 - 
        1586*la^3 - 48*En^6*(-6969 + 1256*la + 575*la^2) - 
        20*En^4*(22392 - 7531*la - 4017*la^2 + 166*la^3) + 
        18*En^2*(9825 - 5723*la - 3236*la^2 + 312*la^3)) + 
      (-150425 + 1534392*En^6 + 29766*la + 7567*la^2 - 304*la^3 + 
        36*En^4*(-79221 + 2930*la) - 6*En^2*(-245674 + 23815*la + 3595*la^2))*
       M^2)*rp[t]^21 + 2*(-2*(-1 + En^2)*J^2*la*(-211 - 152*la + 59*la^2 + 
        240*En^6*(1 + la) + En^2*(1583 + 1336*la - 247*la^2) + 
        12*En^4*(-151 - 142*la + 9*la^2)) + 
      (12530 + 192888*En^8 - 5244*la - 2014*la^2 + 208*la^3 + 
        36*En^6*(-14537 + 592*la) - 6*En^4*(-80486 + 9005*la + 1795*la^2) - 
        3*En^2*(55014 - 12389*la - 3923*la^2 + 156*la^3))*M^2)*rp[t]^22 + 
    4*(-1 + En^2)*(387 + 8640*En^8 - 421*la - 221*la^2 + 47*la^3 + 
      720*En^6*(-31 + 2*la) - 36*En^4*(-537 + 133*la + 55*la^2) - 
      9*En^2*(672 - 385*la - 199*la^2 + 18*la^3))*M*rp[t]^23 - 
    4*(-1 + En^2)^2*la*(23 + 16*la - 7*la^2 + 240*En^4*(1 + la) + 
      16*En^2*(-13 - 11*la + 2*la^2))*rp[t]^24))/
  (En^2*(1 + la)*rp[t]^17*(J^2 + rp[t]^2)^8) + 
 ((8*mu*Pi*YBar*(-2*M + rp[t])^2*(44289*J^16*M^5 + 3*J^16*(-23551 + 123*la)*
       M^4*rp[t] + 6*J^14*M^3*(J^2*(6871 - 171*la - 85*la^2) + 50118*M^2)*
       rp[t]^2 + J^14*M^2*(J^2*(-10358 + 883*la + 649*la^2 + 4*la^3) + 
        18*(-26799 + 11234*En^2 + 130*la)*M^2)*rp[t]^3 + 
      J^12*M*(-(J^4*(-936 + 301*la + 269*la^2 + 4*la^3)) + 
        6*J^2*(47191 - 1099*la - 549*la^2 + En^2*(-37024 + 354*la))*M^2 + 
        856656*M^4)*rp[t]^4 + J^12*(J^4*la*(35 + 36*la + la^2) + 
        J^2*(-71599 + 5754*la + 4259*la^2 + 40*la^3 - 
          6*En^2*(-12905 + 644*la + 335*la^2))*M^2 + 
        18*(-76973 + 71938*En^2 + 310*la)*M^4)*rp[t]^5 + 
      2*J^10*M*(-(J^4*(-3258 + 995*la + 898*la^2 + 20*la^3 + 
           En^2*(4194 - 958*la - 752*la^2 + 8*la^3))) + 
        3*J^2*(136727 + 20604*En^4 - 2785*la - 1445*la^2 + 
          En^2*(-239312 + 2526*la))*M^2 + 651402*M^4)*rp[t]^6 + 
      J^10*(J^4*la*(1 + la)*(5*(47 + 2*la) + En^2*(-270 + 8*la)) + 
        J^2*(-209390 + 15157*la + 11509*la^2 + 172*la^3 + 
          84*En^4*(-905 + 9*la) - 6*En^2*(-84319 + 4330*la + 2119*la^2))*
         M^2 + 6*(-355921 + 574614*En^2 + 810*la)*M^4)*rp[t]^7 + 
      J^8*M*(-(J^4*(-19251 + 5411*la + 4989*la^2 + 172*la^3 + 
           12*En^4*(-847 + 89*la + 65*la^2) + 4*En^2*(13878 - 3154*la - 
             2417*la^2 + 26*la^3))) + 6*J^2*(213713 + 126012*En^4 - 3167*la - 
          1897*la^2 + En^2*(-645316 + 7374*la))*M^2 + 1088010*M^4)*rp[t]^8 + 
      J^8*(J^4*la*(658 + 701*la + 43*la^2 - 4*En^4*(-55 - 54*la + la^2) + 
          2*En^2*(-885 - 859*la + 26*la^2)) + J^2*(-332179 + 13032*En^6 + 
          19412*la + 15923*la^2 + 416*la^3 + 108*En^4*(-4415 + 56*la) - 
          6*En^2*(-230663 + 12046*la + 5527*la^2))*M^2 + 
        18*(269234*En^2 - 5*(20346 + 37*la))*M^4)*rp[t]^9 + 
      2*J^6*M*(-2*J^4*(480*En^6 + 72*En^4*(-229 + 25*la + 17*la^2) + 
          2*(-3879 + 937*la + 908*la^2 + 52*la^3) + 
          En^2*(38592 - 8653*la - 6463*la^2 + 66*la^3)) + 
        3*J^2*(314568*En^4 + 2*En^2*(-462590 + 5493*la) - 
          5*(-37639 + 111*la + 215*la^2))*M^2 + 206262*M^4)*rp[t]^10 + 
      J^6*(2*J^4*la*(485 + 537*la + 52*la^2 - 16*En^4*(-45 - 44*la + la^2) + 
          En^2*(-2440 - 2374*la + 66*la^2)) + 
        J^2*(87336*En^6 + 36*En^4*(-33868 + 553*la) - 
          6*En^2*(-338441 + 17292*la + 7431*la^2) + 
          5*(-60118 + 1857*la + 2123*la^2 + 124*la^3))*M^2 + 
        18*(-41821 + 204406*En^2 - 642*la)*M^4)*rp[t]^11 + 
      J^4*M*(-(J^4*(15360*En^6 + 12*En^4*(-14316 + 1769*la + 1097*la^2) + 
           16*En^2*(14562 - 3090*la - 2251*la^2 + 20*la^3) + 
           5*(-5778 + 979*la + 1103*la^2 + 124*la^3))) + 
        6*J^2*(83281 + 415800*En^4 + 2639*la + 225*la^2 + 
          En^2*(-725720 + 8334*la))*M^2 - 37512*M^4)*rp[t]^12 + 
      J^4*(J^4*la*(755 + 910*la + 155*la^2 + En^4*(4356 + 4272*la - 
            84*la^2) + 4*En^2*(-1771 - 1731*la + 40*la^2)) + 
        J^2*(-142453 + 190800*En^6 - 5782*la + 689*la^2 + 584*la^3 + 
          24*En^4*(-71567 + 1244*la) - 6*En^2*(-275719 + 12768*la + 
            5109*la^2))*M^2 + 6*(703 + 210114*En^2 - 1830*la)*M^4)*rp[t]^13 - 
      2*J^2*M*(J^4*(-7308 + 23*la + 702*la^2 + 292*la^3 - 
          96*En^6*(-91 + 9*la) + 48*En^4*(-2779 + 313*la + 180*la^2) + 
          2*En^2*(49707 - 9225*la - 6566*la^2 + 40*la^3)) - 
        3*J^2*(5521 + 275052*En^4 + 3061*la + 681*la^2 + 
          En^2*(-267064 + 2370*la))*M^2 + 42642*M^4)*rp[t]^14 - 
      J^2*(J^4*la*(-215 - 361*la - 146*la^2 + 864*En^6*(1 + la) + 
          En^2*(5486 + 5406*la - 80*la^2) + 16*En^4*(-401 - 395*la + 
            6*la^2)) + J^2*(18314 - 391824*En^6 + 10181*la + 3761*la^2 - 
          340*la^3 - 36*En^4*(-34701 + 535*la) + 6*En^2*(-109361 + 3586*la + 
            1261*la^2))*M^2 + 54*(-2189 + 302*En^2 + 90*la)*M^4)*rp[t]^15 + 
      M*(J^4*(2691 - 31680*En^8 + 2075*la + 889*la^2 - 340*la^3 + 
          192*En^6*(-583 + 17*la) - 36*En^4*(-6185 + 521*la + 281*la^2) + 
          4*En^2*(-21420 + 2794*la + 1945*la^2 + 6*la^3)) + 
        6*J^2*(-9593 + 54540*En^4 + 1435*la + 365*la^2 - 
          6*En^2*(1302 + 85*la))*M^2 - 20691*M^4)*rp[t]^16 - 
      (J^4*la*(100 + 15*la - 85*la^2 + 1632*En^6*(1 + la) + 
          2*En^2*(937 + 943*la + 6*la^2) + En^4*(-4212 - 4168*la + 
            44*la^2)) - J^2*(11255 + 247752*En^6 - 5208*la - 2311*la^2 + 
          112*la^3 + 180*En^4*(-1637 + 16*la) + 6*En^2*(7593 + 722*la + 
            347*la^2))*M^2 + 9*(-3401 + 9876*En^2 + 95*la)*M^4)*rp[t]^17 - 
      4*M*(J^2*(171 - 11520*En^8 - 336*En^6*(-74 + la) - 311*la - 184*la^2 + 
          28*la^3 + 24*En^4*(-660 + 28*la + 13*la^2) + 
          En^2*(2340 + 341*la + 237*la^2 - 14*la^3)) + 
        3*(1361 + 6936*En^4 - 129*la - 34*la^2 + 3*En^2*(-2614 + 55*la))*M^2)*
       rp[t]^18 - 2*(2*(-1 + En^2)*J^2*la*(-23 - 16*la + 7*la^2 - 
          12*En^2*(1 + la) + 168*En^4*(1 + la)) + 
        (-1838 + 13788*En^6 + 484*la + 226*la^2 - 8*la^3 + 
          6*En^4*(-4618 + 95*la) - 3*En^2*(-5241 + 472*la + 187*la^2))*M^2)*
       rp[t]^19 - 4*(-1 + En^2)*(-72 + 720*En^6 + 48*En^4*(-29 + la) + 
        61*la + 39*la^2 - 4*la^3 - 3*En^2*(-246 + 77*la + 53*la^2))*M*
       rp[t]^20 + 4*(-1 + En^2)^2*la*(1 + la)*(-5 + 24*En^2 + la)*rp[t]^21))/
    (En*rp[t]^14*(J^2 + rp[t]^2)^7) - 
   (8*J^2*mu*Pi*YPhiPhiBar*(-2*M + rp[t])^2*(760725*J^20*M^6 - 
      63*J^20*(21463 - 2052*la + 1516*m^2)*M^5*rp[t] + 
      3*J^18*M^4*(J^2*(308272 - 71857*la - 3267*la^2 + (50462 - 912*la)*m^2 + 
          228*m^4) + 2652192*M^2)*rp[t]^2 + 3*J^18*M^3*
       (2*J^2*(-50136 + 22405*la + 2059*la^2 - 325*la^3 + 
          (-14617 + 829*la + 205*la^2)*m^2 - 156*m^4) + 
        (-4717287 + 859050*En^2 + 441552*la - 311940*m^2)*M^2)*rp[t]^3 + 
      J^16*M^2*(-(J^4*(-45720 + 37922*la + 5201*la^2 - 2369*la^3 + 28*la^4 + 
           (-21849 + 3217*la + 1505*la^2 - 20*la^3)*m^2 + 
           (-423 + 4*la + 4*la^2)*m^4)) - 18*J^2*(-538115 + 122981*la + 
          5707*la^2 + (-82739 + 1416*la)*m^2 - 324*m^4 + 
          2*En^2*(92036 - 12445*la + 11384*m^2))*M^2 + 37616103*M^4)*
       rp[t]^4 + J^16*M*(J^4*(-2520 + 4508*la + 779*la^2 - 929*la^3 + 
          28*la^4 + (-1953 + 867*la + 595*la^2 - 20*la^3)*m^2 + 
          (-63 + 4*la + 4*la^2)*m^4) + 6*J^2*(-525509 + 230669*la + 
          21690*la^2 - 3320*la^3 + (-144468 + 7817*la + 1985*la^2)*m^2 - 
          1338*m^4 + En^2*(248446 - 85912*la - 3786*la^2 + (74948 - 2196*la)*
             m^2 + 744*m^4))*M^2 + 18*(-3719984 + 1412853*En^2 + 339834*la - 
          227544*m^2)*M^4)*rp[t]^5 - 
      J^14*(J^6*la*(144 + 21*la - 116*la^2 + 7*la^3 + (80 + 75*la - 5*la^2)*
           m^2 + (1 + la)*m^4) + J^4*(-479628 + 391729*la + 55245*la^2 - 
          24242*la^3 + 284*la^4 + (-217134 + 30721*la + 14665*la^2 - 
            180*la^3)*m^2 + (-3654 + 32*la + 32*la^2)*m^4 + 
          6*En^2*(45027 - 32467*la - 2881*la^2 + 805*la^3 + 
            (26035 - 2596*la - 670*la^2)*m^2 + 654*m^4))*M^2 - 
        9*J^2*(5096664 + 164400*En^4 - 1138667*la - 54113*la^2 - 
          192*(-3790 + 61*la)*m^2 + 2408*m^4 - 4*En^2*(911266 - 120191*la + 
            105880*m^2))*M^4 - 105958368*M^6)*rp[t]^6 + 
      J^14*M*(J^4*(-26460 + 46800*la + 8423*la^2 - 9527*la^3 + 284*la^4 + 
          (-19539 + 8401*la + 5845*la^2 - 180*la^3)*m^2 + 
          (-549 + 32*la + 32*la^2)*m^4 - 2*En^2*(-7875 + 12862*la + 
            1589*la^2 - 1772*la^3 + 24*la^4 + (-8388 + 2762*la + 1480*la^2 - 
              40*la^3)*m^2 + 3*(-141 + 4*la + 4*la^2)*m^4)) - 
        6*J^2*(2490993 - 1071122*la - 103431*la^2 + 15275*la^3 + 
          (638802 - 32752*la - 8560*la^2)*m^2 + 4998*m^4 + 
          12*En^4*(16304 - 3359*la + 3529*m^2) + En^2*(-2469578 + 834368*la + 
            37410*la^2 + 8*(-87911 + 2379*la)*m^2 - 5928*m^4))*M^2 + 
        36*(-5244755 + 3125910*En^2 + 465720*la - 291956*m^2)*M^4)*rp[t]^7 + 
      J^12*(J^6*la*(1 + la)*(-1512 + 1264*la - 71*la^2 + 
          5*(-158 + 9*la)*m^2 - 8*m^4 + 2*En^2*(324 - 313*la + 12*la^2 + 
            (270 - 20*la)*m^2 + 6*m^4)) + 
        J^4*(-3*(-758607 + 608725*la + 88642*la^2 - 37251*la^3 + 432*la^4) + 
          2*(483219 - 65248*la - 31840*la^2 + 360*la^3)*m^2 - 
          14*(-981 + 8*la + 8*la^2)*m^4 + 12*En^4*(22077 - 13041*la - 
            652*la^2 + (12955 - 742*la)*m^2 + 308*m^4) - 
          6*En^2*(449646 - 317512*la - 28841*la^2 + 7765*la^3 - 
            8*(-30871 + 2884*la + 775*la^2)*m^2 + 5298*m^4))*M^2 + 
        12*J^2*(10790559 + 1164816*En^4 - 2347678*la - 114730*la^2 - 
          168*(-8383 + 126*la)*m^2 + 3822*m^4 - 6*En^2*(2023753 - 259298*la + 
            218284*m^2))*M^4 + 197193906*M^6)*rp[t]^8 + 
      J^12*M*(J^4*(3*(-41895 + 73150*la + 13780*la^2 - 14676*la^3 + 
            432*la^4) + (-87669 + 36266*la + 25610*la^2 - 720*la^3)*m^2 + 
          7*(-297 + 16*la + 16*la^2)*m^4 - 24*En^4*(630 - 991*la - 93*la^2 + 
            65*la^3 + (847 - 213*la - 65*la^2)*m^2 + 62*m^4) - 
          2*En^2*(-79065 + 126974*la + 16363*la^2 - 17224*la^3 + 228*la^4 + 
            (-80676 + 25294*la + 13940*la^2 - 320*la^3)*m^2 + 
            (-3501 + 84*la + 84*la^2)*m^4)) + 6*J^2*(-7040115 + 31320*En^6 + 
          2955292*la + 294408*la^2 - 41680*la^3 + 
          14*(-118527 + 5716*la + 1540*la^2)*m^2 - 10626*m^4 - 
          12*En^4*(155767 - 31204*la + 31739*m^2) + 
          2*En^2*(5508803 - 1811798*la - 83052*la^2 - 4*(-366379 + 8967*la)*
             m^2 + 9972*m^4))*M^2 + 54*(5438716*En^2 - 
          7*(930911 - 79916*la + 46092*m^2))*M^4)*rp[t]^9 - 
      J^10*(J^6*la*(6*(1197 + 221*la - 922*la^2 + 54*la^3) - 
          5*(-697 - 661*la + 36*la^2)*m^2 + 28*(1 + la)*m^4 + 
          4*En^4*(96 - 11*la - 104*la^2 + 3*la^3 - 10*(-11 - 10*la + la^2)*
             m^2 + 4*(1 + la)*m^4) - 2*En^2*(3258 + 187*la - 2957*la^2 + 
            114*la^3 - 20*(-129 - 121*la + 8*la^2)*m^2 + 42*(1 + la)*m^4)) + 
        2*J^4*(6*(-536655 + 421873*la + 63769*la^2 - 25466*la^3 + 292*la^4) - 
          7*(180765 - 23108*la - 11540*la^2 + 120*la^3)*m^2 + 
          7*(-2097 + 16*la + 16*la^2)*m^4 + 36*En^6*(799 - 323*la + 
            392*m^2) - 18*En^4*(71304 - 41054*la - 2066*la^2 + 
            (39715 - 2094*la)*m^2 + 816*m^4) + 
          6*En^2*(2*(504165 - 347489*la - 32479*la^2 + 8360*la^3) - 
            2*(-260659 + 22384*la + 6340*la^2)*m^2 + 9027*m^4))*M^2 - 
        18*J^2*(3260832*En^4 - 4*En^2*(5303751 - 656662*la + 522316*m^2) + 
          7*(1918098 - 404285*la - 20423*la^2 + (223774 - 3120*la)*m^2 + 
            480*m^4))*M^4 - 253804320*M^6)*rp[t]^10 + 
      J^10*M*(J^4*(3*(-118695 + 204210*la + 40524*la^2 - 40244*la^3 + 
            1168*la^4) - 7*(33129 - 13066*la - 9370*la^2 + 240*la^3)*m^2 + 
          7*(-639 + 32*la + 32*la^2)*m^4 + 240*En^6*(9 - 15*la - la^2 - 
            4*(-4 + la)*m^2 + 2*m^4) - 24*En^4*(6210 - 9581*la - 923*la^2 + 
            615*la^3 + (8037 - 1903*la - 595*la^2)*m^2 + 522*m^4) - 
          2*En^2*(-356850 + 561751*la + 75914*la^2 - 74900*la^3 + 960*la^4 - 
            2*(173034 - 50941*la - 29150*la^2 + 560*la^3)*m^2 + 
            9*(-1347 + 28*la + 28*la^2)*m^4)) + 
        6*J^2*(299592*En^6 - 12*En^4*(662309 - 128232*la + 125325*m^2) - 
          7*(1879971 - 766762*la - 79274*la^2 + 10670*la^3 - 
            2*(-199278 + 8975*la + 2495*la^2)*m^2 + 2010*m^4) + 
          2*En^2*(14509957 - 4625298*la - 218156*la^2 + (3557516 - 76860*la)*
             m^2 + 18420*m^4))*M^2 + 18*(27763986*En^2 - 
          7*(3602017 - 296736*la + 153468*m^2))*M^4)*rp[t]^11 - 
      J^8*(J^6*la*(3*(6783 + 1415*la - 5076*la^2 + 292*la^3) + 
          (8995 + 8575*la - 420*la^2)*m^2 + 56*(1 + la)*m^4 + 
          8*En^4*(483 - 38*la - 507*la^2 + 14*la^3 - 35*(-15 - 14*la + la^2)*
             m^2 + 12*(1 + la)*m^4) - 2*En^2*(14724 + 1199*la - 13045*la^2 + 
            480*la^3 - 80*(-138 - 131*la + 7*la^2)*m^2 + 126*(1 + la)*m^4)) + 
        J^4*(72*En^6*(8005 - 3161*la + 3752*m^2) - 
          36*En^4*(307590 - 171406*la - 8774*la^2 + (159805 - 7370*la)*m^2 + 
            2540*m^4) + 12*En^2*(2*(1336713 - 895816*la - 86627*la^2 + 
              21070*la^3) - 4*(-322231 + 24760*la + 7435*la^2)*m^2 + 
            16845*m^4) + 7*(3*(-574029 + 440408*la + 69598*la^2 - 
              26138*la^3 + 296*la^4) + (-613656 + 73670*la + 37670*la^2 - 
              360*la^3)*m^2 + 20*(-279 + 2*la + 2*la^2)*m^4))*M^2 - 
        36*J^2*(3998064*En^4 - 2*En^2*(9061173 - 1076027*la + 791780*m^2) + 
          7*(-3*(-413248 + 83751*la + 4401*la^2) - 3*(-41691 + 536*la)*m^2 + 
            202*m^4))*M^4 - 229305006*M^6)*rp[t]^12 + 
      J^8*M*(J^4*(2640*En^6*(9 - 15*la - la^2 - 4*(-4 + la)*m^2 + 2*m^4) - 
          24*En^4*(27000 - 40889*la - 4169*la^2 + 2625*la^3 - 
            15*(-2189 + 483*la + 167*la^2)*m^2 + 1680*m^4) + 
          7*(6*(-15885 + 26885*la + 5673*la^2 - 5179*la^3 + 148*la^4) - 
            5*(11385 - 4240*la - 3088*la^2 + 72*la^3)*m^2 + 
            5*(-171 + 8*la + 8*la^2)*m^4) - 2*En^2*(-956070 + 1465493*la + 
            207790*la^2 - 190876*la^3 + 2352*la^4 - 
            10*(87714 - 23479*la - 13994*la^2 + 224*la^3)*m^2 + 
            15*(-1527 + 28*la + 28*la^2)*m^4)) + 
        6*J^2*(1238328*En^6 - 12*En^4*(1658511 - 307612*la + 290855*m^2) - 
          7*(2435859 - 958210*la - 103572*la^2 + 13120*la^3 - 
            2*(-224703 + 9391*la + 2695*la^2)*m^2 + 1698*m^4) + 
          2*En^2*(24883565 - 7656678*la - 376606*la^2 - 80*(-68615 + 1281*la)*
             m^2 + 20220*m^4))*M^2 + 36*(-11427661 + 16091475*En^2 + 
          894306*la - 398860*m^2)*M^4)*rp[t]^13 - 
      J^6*(J^6*la*(7*(6*(910 + 217*la - 656*la^2 + 37*la^3) + 
            (2135 + 2045*la - 90*la^2)*m^2 + 10*(1 + la)*m^4) + 
          8*En^4*(2355 - 4*la - 2303*la^2 + 56*la^3 - 
            15*(-163 - 156*la + 7*la^2)*m^2 + 30*(1 + la)*m^4) - 
          2*En^2*(38760 + 3781*la - 33803*la^2 + 1176*la^3 - 
            20*(-1359 - 1303*la + 56*la^2)*m^2 + 210*(1 + la)*m^4)) + 
        2*J^4*(36*En^6*(31463 - 12503*la + 13120*m^2) - 
          6*En^4*(2410440 - 1260402*la - 63086*la^2 - 5*(-232825 + 8458*la)*
             m^2 + 12160*m^4) + 6*En^2*(4596963 - 3004369*la - 305651*la^2 + 
            68425*la^3 - 5*(-407113 + 27316*la + 8710*la^2)*m^2 + 
            18645*m^4) + 7*(3*(-372696 + 277231*la + 46219*la^2 - 
              16110*la^3 + 180*la^4) + (-349731 + 39143*la + 20495*la^2 - 
              180*la^3)*m^2 + (-2367 + 16*la + 16*la^2)*m^4))*M^2 - 
        6*J^2*(37026000*En^4 - 12*En^2*(10560513 - 1182985*la + 775492*m^2) + 
          7*(6767670 - 1304659*la - 71833*la^2 + (561576 - 6624*la)*m^2 + 
            636*m^4))*M^4 - 143948448*M^6)*rp[t]^14 + 
      J^6*M*(J^4*(48*En^6*(-225 - 3057*la - 437*la^2 - 40*(-41 + 20*la)*m^2 + 
            340*m^4) - 24*En^4*(79920 - 102279*la - 9439*la^2 + 6475*la^3 - 
            5*(-17173 + 3027*la + 1175*la^2)*m^2 + 2740*m^4) - 
          2*En^2*(-1641735 + 2498719*la + 381260*la^2 - 313628*la^3 + 
            3696*la^4 - 10*(143118 - 33767*la - 20950*la^2 + 280*la^3)*m^2 + 
            15*(-1707 + 28*la + 28*la^2)*m^4) + 
          7*(6*(-20655 + 34243*la + 7777*la^2 - 6405*la^3 + 180*la^4) + 
            (-65799 + 22936*la + 16960*la^2 - 360*la^3)*m^2 + 
            (-729 + 32*la + 32*la^2)*m^4)) + 
        6*J^2*(3528936*En^6 - 60*En^4*(511857 - 94318*la + 84287*m^2) - 
          7*(2225457 - 834752*la - 95130*la^2 + 11210*la^3 - 
            2*(-169737 + 6548*la + 1940*la^2)*m^2 + 894*m^4) + 
          2*En^2*(29133643 - 8526426*la - 446678*la^2 + (5482964 - 87108*la)*
             m^2 + 13212*m^4))*M^2 + 108*(4282378*En^2 - 
          7*(343367 - 25192*la + 9092*m^2))*M^4)*rp[t]^15 + 
      J^4*(J^6*la*(480*En^6*(1 + la)*(14 - 3*la + 6*m^2) - 
          7*(3*(2379 + 659*la - 1630*la^2 + 90*la^3) + 
            (2365 + 2275*la - 90*la^2)*m^2 + 8*(1 + la)*m^4) - 
          8*En^4*(5025 - 1184*la - 6083*la^2 + 126*la^3 - 
            25*(-251 - 244*la + 7*la^2)*m^2 + 40*(1 + la)*m^4) + 
          2*En^2*(67080 + 8693*la - 56539*la^2 + 1848*la^3 - 
            100*(-417 - 403*la + 14*la^2)*m^2 + 210*(1 + la)*m^4)) - 
        J^4*(454464*En^8 + 72*En^6*(140829 - 31925*la + 38480*m^2) - 
          180*En^4*(244472 - 132996*la - 6686*la^2 + (119455 - 3182*la)*m^2 + 
            708*m^4) + 12*En^2*(5387466 - 3404096*la - 375043*la^2 + 
            74165*la^3 - 4*(-521605 + 30052*la + 10165*la^2)*m^2 + 
            12267*m^4) + 7*(3*(-684123 + 487554*la + 86640*la^2 - 
              27602*la^3 + 304*la^4) + (-534582 + 55448*la + 29720*la^2 - 
              240*la^3)*m^2 + 2*(-1251 + 8*la + 8*la^2)*m^4))*M^2 + 
        36*J^2*(5012571 + 6376560*En^4 - 905778*la - 52662*la^2 - 
          4*(-79045 + 852*la)*m^2 + 222*m^4 - 2*En^2*(8523923 - 872138*la + 
            477060*m^2))*M^4 + 60228873*M^6)*rp[t]^16 + 
      J^4*M*(J^4*(17280*En^8*(20 - 3*la + 7*m^2) + 
          240*En^6*(5865 - 1687*la - 59*la^2 - 8*(-278 + 35*la)*m^2 + 
            92*m^4) - 120*En^4*(3*(7620 - 11215*la - 977*la^2 + 665*la^3) + 
            (29241 - 3739*la - 1615*la^2)*m^2 + 486*m^4) + 
          7*(3*(-38085 + 61150*la + 15148*la^2 - 11012*la^3 + 304*la^4) + 
            (-51057 + 16538*la + 12410*la^2 - 240*la^3)*m^2 + 
            (-387 + 16*la + 16*la^2)*m^4) - 2*En^2*(-1900125 + 2904917*la + 
            501244*la^2 - 344092*la^3 + 3864*la^4 - 
            2*(760374 - 154901*la - 99790*la^2 + 1120*la^3)*m^2 + 
            9*(-1887 + 28*la + 28*la^2)*m^4)) + 
        6*J^2*(-9963753 + 4087944*En^6 + 3505052*la + 424936*la^2 - 
          46000*la^3 + 2*(-579051 + 20536*la + 6280*la^2)*m^2 - 1878*m^4 - 
          36*En^4*(874663 - 156948*la + 123483*m^2) + 
          2*En^2*(23803409 - 6385954*la - 365420*la^2 + (3441364 - 46116*la)*
             m^2 + 4764*m^4))*M^2 + 9*(-12159985 + 27530760*En^2 + 
          823020*la - 213684*m^2)*M^4)*rp[t]^17 + 
      (-(J^8*la*(5760*En^8*(1 + la) - 480*En^6*(1 + la)*(-8 - 15*la + 
             24*m^2) + 7*(6*(1101 + 359*la - 704*la^2 + 38*la^3) + 
             (1745 + 1685*la - 60*la^2)*m^2 + 4*(1 + la)*m^4) + 
           40*En^4*(1299 - 731*la - 1995*la^2 + 35*la^3 - 
             5*(-369 - 362*la + 7*la^2)*m^2 + 6*(1 + la)*m^4) - 
           2*En^2*(82104 + 17011*la - 63161*la^2 + 1932*la^3 - 
             20*(-2031 - 1975*la + 56*la^2)*m^2 + 126*(1 + la)*m^4))) + 
        2*J^6*(1764288*En^8 - 6*(-772893 + 517785*la + 99113*la^2 - 
            28394*la^3 + 308*la^4) + (923049 - 88328*la - 48440*la^2 + 
            360*la^3)*m^2 + (2637 - 16*la - 16*la^2)*m^4 - 
          36*En^6*(120141 - 54585*la + 68360*m^2) + 
          18*En^4*(1187504 - 693242*la - 38774*la^2 + (556585 - 10634*la)*
             m^2 + 1616*m^4) - 6*En^2*(4456158 - 2607826*la - 321758*la^2 + 
            53620*la^3 - 2*(-672007 + 32968*la + 11800*la^2)*m^2 + 4449*m^4))*
         M^2 + 9*J^4*(8533092 + 18523968*En^4 - 1418959*la - 87805*la^2 - 
          6*(-59467 + 584*la)*m^2 + 116*m^4 - 8*En^2*(4661893 - 415438*la + 
            167844*m^2))*M^4 + 15192576*J^2*M^6)*rp[t]^18 + 
      J^2*M*(J^4*(-241920*En^10 + 3*(-174195 + 264950*la + 72460*la^2 - 
            45476*la^3 + 1232*la^4) + (-179109 + 53626*la + 40810*la^2 - 
            720*la^3)*m^2 + (-819 + 32*la + 32*la^2)*m^4 + 
          5760*En^8*(-334 - 15*la + 7*m^2) + 240*En^6*(2349 - 3801*la + 
            61*la^2 + (6224 - 260*la)*m^2 + 58*m^4) - 
          24*En^4*(82080 - 188051*la - 20513*la^2 + 9765*la^3 - 
            3*(-49669 + 4551*la + 2155*la^2)*m^2 + 1122*m^4) - 
          2*En^2*(-1580940 + 2311189*la + 474038*la^2 - 251804*la^3 + 
            2688*la^4 - 2*(507438 - 88447*la - 58970*la^2 + 560*la^3)*m^2 + 
            (-6201 + 84*la + 84*la^2)*m^4)) + 
        6*J^2*(-4292055 + 3024216*En^6 + 1385837*la + 180303*la^2 - 
          17705*la^3 + (-330003 + 10729*la + 3385*la^2)*m^2 - 246*m^4 - 
          12*En^4*(1981327 - 296864*la + 179119*m^2) + 
          2*En^2*(13377463 - 3099638*la - 198412*la^2 - 4*(-308609 + 3477*la)*
             m^2 + 732*m^4))*M^2 + 27*(-1033199 + 3005482*En^2 + 63344*la - 
          8868*m^2)*M^4)*rp[t]^19 + 
      (-(J^6*la*(-3840*En^8*(1 + la) + 3*(10009 + 3857*la - 5844*la^2 + 
             308*la^3) + (5785 + 5605*la - 180*la^2)*m^2 + 8*(1 + la)*m^4 - 
           960*En^6*(1 + la)*(-37 - 15*la + 18*m^2) + 
           8*En^4*(8613 - 1838*la - 10297*la^2 + 154*la^3 - 
             15*(-517 - 510*la + 7*la^2)*m^2 + 12*(1 + la)*m^4) - 
           2*En^2*(72708 + 24317*la - 47047*la^2 + 1344*la^3 - 
             80*(-306 - 299*la + 7*la^2)*m^2 + 42*(1 + la)*m^4))) - 
        J^4*(1928448*En^8 + 3*(-1353705 + 830602*la + 172891*la^2 - 
            43839*la^3 + 468*la^4) + (-532575 + 46877*la + 26285*la^2 - 
            180*la^3)*m^2 + (-693 + 4*la + 4*la^2)*m^4 + 
          72*En^6*(9023 - 56603*la + 57352*m^2) + 
          12*En^2*(2*(1301145 - 650944*la - 92459*la^2 + 12460*la^3) - 
            4*(-123517 + 5152*la + 1945*la^2)*m^2 + 687*m^4) - 
          12*En^4*(2821902 - 1381542*la - 95158*la^2 + (847645 - 11722*la)*
             m^2 + 908*m^4))*M^2 + 6*J^2*(3306263 + 13316256*En^4 - 
          495415*la - 32873*la^2 + (67189 - 600*la)*m^2 - 
          6*En^2*(3150682 - 231829*la + 51552*m^2))*M^4 + 1755675*M^6)*
       rp[t]^20 + M*(J^4*(1209600*En^10 + 3*(-78120 + 108910*la + 
            33169*la^2 - 17619*la^3 + 468*la^4) + 
          (-52488 + 14477*la + 11165*la^2 - 180*la^3)*m^2 + 
          4*(-27 + la + la^2)*m^4 - 5760*En^8*(-136 - 9*la + 49*m^2) + 
          240*En^6*(-8721 - 5235*la - 193*la^2 + (6304 - 124*la)*m^2 + 
            14*m^4) - 24*En^4*(67410 - 137711*la - 22243*la^2 + 5915*la^3 + 
            (80977 - 5463*la - 2795*la^2)*m^2 + 212*m^4) - 
          2*En^2*(-981900 + 1213391*la + 303634*la^2 - 118420*la^3 + 
            1200*la^4 + (-385308 + 57442*la + 39500*la^2 - 320*la^3)*m^2 + 
            3*(-321 + 4*la + 4*la^2)*m^4)) + 6*J^2*(-1128282 + 3287016*En^6 + 
          326189*la + 45898*la^2 - 4040*la^3 + (-41842 + 1245*la + 405*la^2)*
           m^2 - 12*En^4*(1049189 - 107332*la + 36365*m^2) - 
          2*En^2*(-4719872 + 883798*la + 64421*la^2 + 2*(-96503 + 915*la)*
             m^2))*M^2 + 18*(-181506 + 684141*En^2 + 9890*la)*M^4)*rp[t]^21 + 
      (-(J^4*la*(13104 + 5925*la - 6828*la^2 + 351*la^3 - 
           19200*En^8*(1 + la) - 5*(-319 - 310*la + 9*la^2)*m^2 + 
           (1 + la)*m^4 - 960*En^6*(1 + la)*(-17 - 15*la + 12*m^2) + 
           8*En^4*(10593 + 3992*la - 6517*la^2 + 84*la^3 - 
             5*(-695 - 688*la + 7*la^2)*m^2 + 2*(1 + la)*m^4) - 
           2*En^2*(44544 + 21439*la - 22505*la^2 + 600*la^3 - 
             20*(-417 - 409*la + 8*la^2)*m^2 + 6*(1 + la)*m^4))) - 
        J^2*(-1094004 + 3196800*En^8 + 596513*la + 136013*la^2 - 30098*la^3 + 
          316*la^4 + (-68352 + 5525*la + 3165*la^2 - 20*la^3)*m^2 + 
          72*En^6*(106061 - 31341*la + 17712*m^2) + 
          36*En^4*(2*(-298040 + 88193*la + 7807*la^2) + (-59605 + 610*la)*
             m^2) - 6*En^2*(-1958727 + 767459*la + 127393*la^2 - 13505*la^3 + 
            (-157963 + 5620*la + 2230*la^2)*m^2))*M^2 + 
        9*(262588 + 2035056*En^4 - 34695*la - 2485*la^2 + 
          12*En^2*(-165612 + 9613*la))*M^4)*rp[t]^22 + 
      M*(-(J^2*(65520 + 1209600*En^10 - 80570*la - 27431*la^2 + 12143*la^3 - 
           316*la^4 + 5*(1368 - 347*la - 271*la^2 + 4*la^3)*m^2 + 
           5760*En^8*(-380 - 27*la + 35*m^2) + 240*En^6*(273 + 3643*la + 
             427*la^2 + 24*(-89 + la)*m^2) - 24*En^4*(-69750 + 58969*la + 
             13669*la^2 - 2025*la^3 + 5*(-3591 + 185*la + 101*la^2)*m^2) - 
           2*En^2*(412920 - 380023*la - 115775*la^2 + 32456*la^3 - 312*la^4 + 
             20*(3168 - 406*la - 287*la^2 + 2*la^3)*m^2))) + 
        6*(-137492 + 1791864*En^6 + 34710*la + 5315*la^2 - 415*la^3 + 
          12*En^4*(-268231 + 16947*la) - 6*En^2*(-261340 + 37546*la + 
            3147*la^2))*M^2)*rp[t]^23 + 
      (J^2*la*(1 + la)*(-3465 + 3840*En^8 + 1656*la - 79*la^2 + 
          5*(-39 + la)*m^2 + 480*En^6*(52 - 15*la + 6*m^2) + 
          4*En^2*(8310 - 3212*la + 78*la^2 + (615 - 10*la)*m^2) + 
          8*En^4*(-7335 + 2339*la - 26*la^2 + 5*(-129 + la)*m^2)) + 
        (137592 + 2714688*En^8 - 64735*la - 16242*la^2 + 3101*la^3 - 
          32*la^4 + 72*En^6*(-100607 + 7063*la) - 
          36*En^4*(-180425 + 29569*la + 3342*la^2) - 
          6*En^2*(350646 - 101528*la - 19737*la^2 + 1625*la^3))*M^2)*
       rp[t]^24 + 2*(120960*En^10 + 17280*En^8*(-25 + 2*la) - 
        72*En^6*(-7995 + 1639*la + 319*la^2) - 
        144*En^4*(2385 - 922*la - 282*la^2 + 25*la^3) + 
        4*(-1080 + 1131*la + 430*la^2 - 157*la^3 + 4*la^4) - 
        3*En^2*(-27720 + 17923*la + 6515*la^2 - 1316*la^3 + 12*la^4))*M*
       rp[t]^25 - 4*(-1 + En^2)*la*(-105 - 62*la + 41*la^2 - 2*la^3 + 
        1440*En^6*(1 + la) + 120*En^4*(-22 - 19*la + 3*la^2) + 
        En^2*(1290 + 937*la - 346*la^2 + 7*la^3))*rp[t]^26))/
    (En*la*(1 + la)*rp[t]^15*(J^2 + rp[t]^2)^10))*Derivative[1][rp][t]
]


Clear[gSourceCPM3]
gSourceCPM3[syms_Association]:=
Module[{mu,M,CapLa,J,la,XPhiBar,XPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	XPhiBar=XPhiSymbol[syms,Conjugate->True][t];
	XPhiPhiBar=XPhiPhiSymbol[syms,Conjugate->True][t];
	
(-8*J^2*mu*Pi*XPhiPhiBar*(-2*M + rp[t])^2*(840*J^10*M^3 + 
    J^10*(-885 + 104*la - 94*m^2)*M^2*rp[t] + 
    J^8*M*(J^2*(293 - 80*la + 79*m^2) + 4884*M^2)*rp[t]^2 + 
    J^8*(-2*J^2*(15 - 7*la + 8*m^2) + (-5157 + 372*En^2 + 536*la - 436*m^2)*
       M^2)*rp[t]^3 + 2*J^6*M*(J^2*(854 - 208*la + 188*m^2 + 
        En^2*(-106 + 13*la - 16*m^2)) + 5892*M^2)*rp[t]^4 + 
    J^6*(J^2*(-174 + 74*la - 79*m^2 + 2*En^2*(14 - 2*la + 5*m^2)) + 
      6*(-2087 + 312*En^2 + 184*la - 124*m^2)*M^2)*rp[t]^5 + 
    2*J^4*M*(J^2*(2080 + 6*En^4 - 432*la + 327*m^2 + 
        En^2*(-538 + 54*la - 68*m^2)) + 7464*M^2)*rp[t]^6 - 
    J^4*(J^2*(4*En^4*(1 + la) - 2*En^2*(71 - 9*la + 25*m^2) + 
        3*(140 - 52*la + 47*m^2)) - 2*(-8097 + 1908*En^2 + 568*la - 278*m^2)*
       M^2)*rp[t]^7 + 2*J^2*M*(J^2*(2761 + 30*En^4 - 448*la + 248*m^2 - 
        2*En^2*(529 - 42*la + 44*m^2)) + 4920*M^2)*rp[t]^8 - 
    J^2*(J^2*(576 - 164*la + 20*En^4*(1 + la) + 109*m^2 - 
        10*En^2*(23 - 3*la + 7*m^2)) + (11025 - 4464*En^2 - 584*la + 154*m^2)*
       M^2)*rp[t]^9 + M*(J^2*(3931 + 84*En^4 - 464*la + 139*m^2 - 
        4*En^2*(703 - 29*la + 18*m^2)) + 2652*M^2)*rp[t]^10 + 
    (J^2*(-438 + En^4*(44 - 28*la) + 86*la - 31*m^2 + 
        En^2*(386 - 22*la + 30*m^2)) + 3*(-1027 + 716*En^2 + 40*la)*M^2)*
     rp[t]^11 + 2*(577 + 210*En^4 + 15*En^2*(-52 + la) - 48*la)*M*rp[t]^12 - 
    6*(-1 + En^2)*(-23 + 3*la + 2*En^2*(11 + la))*rp[t]^13))/
  (En*la*(1 + la)*rp[t]^11*(J^2 + rp[t]^2)^5) + 
 ((-16*J*mu*Pi*XPhiBar*(2*M - rp[t])*(98*J^6*M^2 - J^6*(65 + 4*la)*M*rp[t] + 
      2*(J^6*(5 + la) + 157*J^4*M^2)*rp[t]^2 + J^4*(-209 + 16*En^2 - 12*la)*M*
       rp[t]^3 + (-2*J^4*(-16 - 3*la + En^2*(2 + la)) + 342*J^2*M^2)*
       rp[t]^4 + 3*J^2*(-77 + 16*En^2 - 4*la)*M*rp[t]^5 + 
      2*(J^2*(3*(6 + la) - En^2*(5 + 2*la)) + 63*M^2)*rp[t]^6 + 
      (-87 + 48*En^2 - 4*la)*M*rp[t]^7 - 2*(-1 + En^2)*(7 + la)*rp[t]^8))/
    ((1 + la)*rp[t]^8*(J^2 + rp[t]^2)^3) + 
   ((16*I)*J^3*m*mu*Pi*XPhiPhiBar*(2*M - rp[t])*
     (175*J^6*M^2 + J^6*(-123 + 2*la - 2*m^2)*M*rp[t] + 
      (J^6*(20 - la + m^2) + 683*J^4*M^2)*rp[t]^2 + 
      2*J^4*(-245 + 13*En^2 + 3*la - 2*m^2)*M*rp[t]^3 + 
      (J^4*(81 - 3*la - 2*En^2*(3 + la) + 2*m^2) + 937*J^2*M^2)*rp[t]^4 + 
      J^2*(-707 + 104*En^2 + 6*la - 2*m^2)*M*rp[t]^5 + 
      (J^2*(126 - 3*la - 4*En^2*(5 + la) + m^2) + 429*M^2)*rp[t]^6 + 
      2*(-170 + 87*En^2 + la)*M*rp[t]^7 - (-65 + la + 2*En^2*(31 + la))*
       rp[t]^8))/(la*(1 + la)*rp[t]^8*(J^2 + rp[t]^2)^4))*Derivative[1][rp][t]
]


Clear[fSourceCPM3]
fSourceCPM3[syms_Association]:=
Module[{mu,M,CapLa,J,la,XPhiBar,XPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	XPhiBar=XPhiSymbol[syms,Conjugate->True][t];
	XPhiPhiBar=XPhiPhiSymbol[syms,Conjugate->True][t];
	
(8*J^2*mu*Pi*XPhiPhiBar*(-2*M + rp[t])^3*(105*J^8*M^2 - 
    2*J^8*(41 - 6*la + 3*m^2)*M*rp[t] + 
    3*(J^8*(5 - 2*la + m^2) + 164*J^6*M^2)*rp[t]^2 + 
    2*J^6*(-194 + 24*En^2 + 24*la - 9*m^2)*M*rp[t]^3 + 
    (J^6*(72 - 24*la + 9*m^2 - 2*En^2*(7 - 3*la + 2*m^2)) + 906*J^4*M^2)*
     rp[t]^4 + 6*J^4*(-122 + 32*En^2 + 12*la - 3*m^2)*M*rp[t]^5 + 
    (J^4*(141 - 36*la + 9*m^2 - 2*En^2*(29 - 9*la + 4*m^2)) + 756*J^2*M^2)*
     rp[t]^6 + 2*J^2*(-314 + 180*En^2 + 24*la - 3*m^2)*M*rp[t]^7 + 
    (J^2*(3*(42 - 8*la + m^2) - 2*En^2*(67 - 9*la + 2*m^2)) + 237*M^2)*
     rp[t]^8 + 2*(-101 + 108*En^2 + 6*la)*M*rp[t]^9 + 
    6*(-1 + En^2)*(-7 + 8*En^2 + la)*rp[t]^10))/
  (En*la*(1 + la)*rp[t]^10*(J^2 + rp[t]^2)^4) + 
 ((16*J*mu*Pi*XPhiBar*(-2*M + rp[t])^2*(-13*J^4*M + 5*J^4*rp[t] - 
      28*J^2*M*rp[t]^2 + (11 - 2*En^2)*J^2*rp[t]^3 - 15*M*rp[t]^4 - 
      6*(-1 + En^2)*rp[t]^5))/((1 + la)*rp[t]^7*(J^2 + rp[t]^2)^2) + 
   ((16*I)*J^3*m*mu*Pi*XPhiPhiBar*(-2*M + rp[t])^2*(17*J^4*M - 7*J^4*rp[t] + 
      46*J^2*M*rp[t]^2 + 4*(-5 + En^2)*J^2*rp[t]^3 + 29*M*rp[t]^4 + 
      (-13 + 16*En^2)*rp[t]^5))/(la*(1 + la)*rp[t]^7*(J^2 + rp[t]^2)^3))*
  Derivative[1][rp][t]
]


Clear[gSourceCPM4]
gSourceCPM4[syms_Association]:=
Module[{mu,M,CapLa,J,la,XPhiBar,XPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	XPhiBar=XPhiSymbol[syms,Conjugate->True][t];
	XPhiPhiBar=XPhiPhiSymbol[syms,Conjugate->True][t];

(-16*J*mu*Pi*XPhiBar*(-2*M + rp[t])^2*(3528*J^12*M^4 + 
    4*J^12*(-1347 + 38*la)*M^3*rp[t] + 8*J^10*M^2*(J^2*(374 - 25*la) + 
      2604*M^2)*rp[t]^2 + 2*J^10*M*(J^2*(-355 + 43*la) + 
      4*(-3969 + 384*En^2 + 112*la)*M^2)*rp[t]^3 + 
    J^8*(-12*J^4*(-5 + la) + J^2*(17588 - 1176*la + En^2*(-3147 + 64*la))*
       M^2 + 51240*M^4)*rp[t]^4 + 
    J^8*M*(J^2*(En^2*(1011 - 48*la) + 8*(-520 + 63*la)) + 
      4*(-19485 + 4101*En^2 + 550*la)*M^2)*rp[t]^5 + 
    J^6*(2*J^4*(-35*(-5 + la) + En^2*(-50 + 4*la)) + 
      J^2*(43060 + 660*En^4 - 2880*la + En^2*(-16851 + 320*la))*M^2 + 
      67200*M^4)*rp[t]^6 + 
    2*J^6*M*(-(J^2*(5075 - 615*la + 24*En^2*(-113 + 5*la) + 
         En^4*(178 + 7*la))) + 12*(-4250 + 1487*En^2 + 120*la)*M^2)*rp[t]^7 + 
    2*(J^8*(-85*(-5 + la) + En^4*(22 + 4*la) + 4*En^2*(-67 + 5*la)) + 
      J^6*(28100 + 1608*En^4 - 1880*la + 5*En^2*(-3687 + 64*la))*M^2 + 
      24780*J^4*M^4)*rp[t]^8 + 
    4*J^4*M*(J^2*(3*En^6 + 100*(-33 + 4*la) - En^4*(440 + 17*la) - 
        6*En^2*(-497 + 20*la)) + 5*(-3753 + 1992*En^2 + 106*la)*M^2)*
     rp[t]^9 + 2*(J^6*(-110*(-5 + la) - 2*En^6*(1 + la) + 
        10*En^2*(-59 + 4*la) + En^4*(109 + 19*la)) + 
      J^4*(20620 + 3108*En^4 - 1380*la + En^2*(-20847 + 320*la))*M^2 + 
      9744*J^2*M^4)*rp[t]^10 + 
    2*J^2*M*(J^2*(-4825 + 30*En^6 + En^2*(6855 - 240*la) + 585*la - 
        12*En^4*(141 + 5*la)) + 4*(-3681 + 2859*En^2 + 104*la)*M^2)*
     rp[t]^11 + (2*J^4*(-80*(-5 + la) - 10*En^6*(1 + la) + 
        3*En^4*(63 + 11*la) + En^2*(-692 + 40*la)) + 
      J^2*(16132 + 6384*En^4 - 1080*la + En^2*(-24447 + 320*la))*M^2 + 
      3192*M^4)*rp[t]^12 + 
    M*(J^2*(84*En^6 + En^2*(8277 - 240*la) - 4*En^4*(968 + 23*la) + 
        8*(-470 + 57*la)) + 4*(-1203 + 1347*En^2 + 34*la)*M^2)*rp[t]^13 + 
    (-2*(-1 + En^2)*J^2*(-31*(-5 + la) + 2*En^4*(-11 + 7*la) - 
        En^2*(281 + 11*la)) + (2628 + 2724*En^4 - 176*la + 
        En^2*(-5919 + 64*la))*M^2)*rp[t]^14 + 
    2*(-1 + En^2)*(305 + 210*En^4 - 37*la - En^2*(736 + 13*la))*M*rp[t]^15 - 
    2*(-1 + En^2)^2*(5*(-5 + la) + 6*En^2*(11 + la))*rp[t]^16))/
  (En^2*(1 + la)*rp[t]^14*(J^2 + rp[t]^2)^5) + 
 ((8*I)*J^3*m*mu*Pi*XPhiPhiBar*(-2*M + rp[t])^2*
   (11172*J^12*M^4 - 4*J^12*(4379 - 128*la + 45*m^2)*M^3*rp[t] + 
    J^10*M^2*(J^2*(10019 - 688*la + 244*m^2) + 73080*M^2)*rp[t]^2 + 
    J^10*M*(J^2*(-2459 + 304*la - 109*m^2) + 
      2*(-57446 + 5709*En^2 + 1568*la - 486*m^2)*M^2)*rp[t]^3 + 
    2*J^8*(2*J^4*(54 - 11*la + 4*m^2) + J^2*(32955 - 2112*la + 664*m^2 - 
        4*En^2*(1511 - 59*la + 36*m^2))*M^2 + 100686*M^4)*rp[t]^4 + 
    J^8*M*(J^2*(-16227 + 1872*la - 599*m^2 + 4*En^2*(1006 - 95*la + 
          61*m^2)) + (-317912 + 69834*En^2 + 8000*la - 2088*m^2)*M^2)*
     rp[t]^5 + J^6*(J^4*(1430 - 272*la + 89*m^2 + 
        En^2*(-412 + 72*la - 50*m^2)) + J^2*(183225 + 2760*En^4 - 10800*la + 
        2872*m^2 - 24*En^2*(3105 - 107*la + 60*m^2))*M^2 + 299568*M^4)*
     rp[t]^6 + 2*J^6*M*(J^2*(-22671 + 2400*la - 653*m^2 - 
        4*En^4*(193 - 5*la + 9*m^2) + En^2*(12507 - 1054*la + 632*m^2)) + 
      2*(-118982 + 44691*En^2 + 2720*la - 558*m^2)*M^2)*rp[t]^7 + 
    2*(J^8*(12*En^4*(8 + m^2) + 7*(287 - 50*la + 14*m^2) - 
        2*En^2*(645 - 103*la + 68*m^2)) + 2*J^6*(69085 + 3936*En^4 - 
        3680*la + 772*m^2 - 2*En^2*(24133 - 694*la + 324*m^2))*M^2 + 
      126846*J^4*M^4)*rp[t]^8 + 
    2*J^4*M*(J^2*(-34499 + 24*En^6 + 3280*la - 707*m^2 - 
        4*En^4*(1127 - 22*la + 45*m^2) + En^2*(32737 - 2316*la + 1164*m^2)) + 
      2*(-101623 + 61107*En^2 + 2080*la - 297*m^2)*M^2)*rp[t]^9 + 
    (-2*J^6*(-3091 + 480*la + 8*En^6*(1 + la) - 107*m^2 + 
        En^4*(-572 + 4*la - 72*m^2) + En^2*(3382 - 464*la + 258*m^2)) + 
      J^4*(238485 + 36576*En^4 - 11280*la + 1652*m^2 - 
        8*En^2*(34049 - 746*la + 252*m^2))*M^2 + 115800*J^2*M^4)*rp[t]^10 + 
    J^2*M*(J^2*(-60327 + 288*En^6 + 5040*la - 761*m^2 - 
        24*En^4*(836 - 12*la + 21*m^2) + 2*En^2*(48149 - 2524*la + 
          920*m^2)) + 2*(-93742 + 87405*En^2 + 1696*la - 126*m^2)*M^2)*
     rp[t]^11 + 2*(J^4*(2749 - 370*la - 48*En^6*(1 + la) + 58*m^2 + 
        12*En^4*(81 - la + 9*m^2) - 4*En^2*(1319 - 129*la + 52*m^2)) + 
      J^2*(55707 + 27120*En^4 - 2304*la + 176*m^2 - 
        12*En^2*(8468 - 133*la + 24*m^2))*M^2 + 11106*M^4)*rp[t]^12 + 
    M*(-(J^2*(28647 + 48*En^6 - 2064*la + 163*m^2 + 
         En^2*(-76110 + 2732*la - 532*m^2) + 8*En^4*(4567 - 26*la + 
           27*m^2))) + 6*(-6064 + 8455*En^2 + 96*la)*M^2)*rp[t]^13 + 
    (J^2*(2668 - 304*la - 48*En^6*(-17 + 3*la) + 25*m^2 + 
        24*En^4*(231 - la + 4*m^2) - 2*En^2*(4536 - 284*la + 61*m^2)) + 
      (21935 + 30648*En^4 - 784*la + 136*En^2*(-452 + 5*la))*M^2)*rp[t]^14 + 
    4*(-1436 + 1368*En^6 + En^2*(6074 - 147*la) + 88*la + 
      2*En^4*(-2993 + 7*la))*M*rp[t]^15 - 4*(-1 + En^2)*
     (137 - 13*la + 16*En^4*(31 + la) + 6*En^2*(-107 + 3*la))*rp[t]^16))/
  (En^2*la*(1 + la)*rp[t]^14*(J^2 + rp[t]^2)^6) - 
 (8*J^2*mu*Pi*XPhiPhiBar*(2*M - rp[t])*(16170*J^14*M^4 + 
    J^14*(-22835 + 2416*la - 1502*m^2)*M^3*rp[t] + 
    J^12*M^2*(J^2*(4*(2891 - 724*la + 4*la^2) + (1823 - 16*la)*m^2 + 4*m^4) + 
      118914*M^2)*rp[t]^2 + 
    J^12*M*(-2*J^2*(1227 - 562*la + 8*la^2 + (359 - 8*la)*m^2 + 2*m^4) + 
      (-167911 + 11040*En^2 + 17168*la - 9868*m^2)*M^2)*rp[t]^3 + 
    J^10*(J^4*(4*(45 - 35*la + la^2) + (91 - 4*la)*m^2 + m^4) - 
      2*J^2*(-42500 + 10296*la - 56*la^2 + (-6029 + 48*la)*m^2 - 10*m^4 + 
        En^2*(4940 - 769*la + 728*m^2))*M^2 + 378042*M^4)*rp[t]^4 + 
    J^10*M*(J^2*(-18023 + 7996*la - 112*la^2 + (-4786 + 96*la)*m^2 - 20*m^4 + 
        2*En^2*(1337 - 520*la - 8*la^2 + (514 - 4*la)*m^2 + 4*m^4)) + 
      (-534071 + 76704*En^2 + 52336*la - 27122*m^2)*M^2)*rp[t]^5 + 
    J^8*(J^4*(4*(330 - 249*la + 7*la^2) + (612 - 24*la)*m^2 + 5*m^4 - 
        2*En^2*(105 - 81*la - 4*la^2 - 2*(-42 + la)*m^2 + 2*m^4)) + 
      J^2*(1320*En^4 + 16*(16901 - 3927*la + 21*la^2) + 
        (33413 - 240*la)*m^2 + 40*m^4 - 2*En^2*(34517 - 5218*la + 4712*m^2))*
       M^2 + 675282*M^4)*rp[t]^6 + 
    J^8*M*(-(J^2*(57323 - 24420*la + 336*la^2 + (13394 - 240*la)*m^2 + 
         40*m^4 + 36*En^4*(15 - 2*la + 4*m^2) - 
         4*En^2*(4697 - 1789*la - 24*la^2 + (1703 - 10*la)*m^2 + 8*m^4))) + 
      (-955547 + 227664*En^2 + 88720*la - 39848*m^2)*M^2)*rp[t]^7 + 
    2*J^6*(J^4*(2097 - 1522*la + 42*la^2 + (867 - 30*la)*m^2 + 5*m^4 + 
        2*En^4*(-3*(-4 + la + la^2) + 2*(4 + la)*m^2) + 
        En^2*(-741 + 564*la + 24*la^2 + 10*(-57 + la)*m^2 - 8*m^4)) + 
      J^2*(4476*En^4 + 20*(12113 - 2666*la + 14*la^2) + 
        (24766 - 160*la)*m^2 + 20*m^4 - 3*En^2*(34347 - 4949*la + 4168*m^2))*
       M^2 + 367239*M^4)*rp[t]^8 + 
    J^6*M*(-2*J^2*(51401 - 20750*la + 280*la^2 - 2*(-5019 + 80*la)*m^2 + 
        20*m^4 + 6*En^4*(307 - 50*la + 84*m^2) + 
        En^2*(-28221 + 10382*la + 120*la^2 + 20*(-471 + 2*la)*m^2 - 
          24*m^4)) + (-1043353 + 375648*En^2 + 90320*la - 32962*m^2)*M^2)*
     rp[t]^9 + (2*J^8*(7*(537 - 370*la + 10*la^2) + (1318 - 40*la)*m^2 + 
        5*m^4 + 2*En^4*(-3*(-26 + 7*la + 5*la^2) + (50 + 8*la)*m^2) + 
        En^2*(-2253 + 1678*la + 60*la^2 + 2*(-837 + 10*la)*m^2 - 12*m^4)) + 
      J^6*(25056*En^4 + 20*(26567 - 5436*la + 28*la^2) + 
        (41353 - 240*la)*m^2 + 20*m^4 - 4*En^2*(85291 - 11294*la + 8260*m^2))*
       M^2 + 488022*J^4*M^4)*rp[t]^10 + 
    J^4*M*(-2*J^2*(56634 - 21190*la + 280*la^2 + (8477 - 120*la)*m^2 + 
        10*m^4 + 12*En^4*(473 - 84*la + 142*m^2) - 
        4*En^2*(11565 - 4037*la - 40*la^2 + (3241 - 10*la)*m^2 + 4*m^4)) + 
      (-698357 + 379680*En^2 + 55216*la - 14540*m^2)*M^2)*rp[t]^11 + 
    (J^6*(8322 - 5300*la + 140*la^2 + (2259 - 60*la)*m^2 + 5*m^4 + 
        24*En^4*(62 - 16*la - 5*la^2 + (43 + 2*la)*m^2) - 
        2*En^2*(3495 - 2692*la - 80*la^2 + (2442 - 20*la)*m^2 + 8*m^4)) + 
      2*J^4*(15312*En^4 + 4*(44863 - 8322*la + 42*la^2) + 
        (9205 - 48*la)*m^2 + 2*m^4 + En^2*(-177530 + 19327*la - 10736*m^2))*
       M^2 + 183822*J^2*M^4)*rp[t]^12 + 
    J^2*M*(J^2*(-77411 + 768*En^6 + 26004*la - 336*la^2 + 
        (-7634 + 96*la)*m^2 - 4*m^4 - 24*En^4*(263 - 132*la + 182*m^2) + 
        2*En^2*(49819 - 14132*la - 120*la^2 + (8722 - 20*la)*m^2 + 4*m^4)) + 
      3*(-88703 + 75264*En^2 + 6256*la - 890*m^2)*M^2)*rp[t]^13 + 
    (J^4*(5778 - 3260*la + 84*la^2 - 96*En^6*(11 + la) - 24*(-43 + la)*m^2 + 
        m^4 + 8*En^4*(-3*(60 + 32*la + 5*la^2) + (199 + 4*la)*m^2) + 
        En^2*(6*(-1294 + 811*la + 20*la^2) + 4*(-861 + 5*la)*m^2 - 4*m^4)) + 
      J^2*(44088*En^4 + 8*(17355 - 2834*la + 14*la^2) + (3411 - 16*la)*m^2 - 
        6*En^2*(37511 - 2934*la + 912*m^2))*M^2 + 30294*M^4)*rp[t]^14 + 
    M*(-(J^2*(30571 + 6144*En^6 - 8876*la + 112*la^2 - 2*(-715 + 8*la)*m^2 + 
         12*En^4*(1617 - 194*la + 152*m^2) + 4*En^2*(-17397 + 3289*la + 
           24*la^2 + (-1143 + 2*la)*m^2))) + 3*(-14851 + 19920*En^2 + 912*la)*
       M^2)*rp[t]^15 + 2*(2*J^2*(588 + En^6*(792 - 48*la) - 279*la + 7*la^2 - 
        (-49 + la)*m^2 + En^2*(4*(-402 + 146*la + 3*la^2) + 
          (-234 + la)*m^2) + En^4*(-3*(-72 + 55*la + 5*la^2) + 
          2*(91 + la)*m^2)) + (15444*En^4 + 9*En^2*(-3569 + 185*la) + 
        4*(2967 - 414*la + 2*la^2))*M^2)*rp[t]^16 + 
    2*(-2690 + 2304*En^6 + 650*la - 8*la^2 + 6*En^4*(-1775 + 54*la) + 
      En^2*(11071 - 1270*la - 8*la^2))*M*rp[t]^17 - 
    4*(-1 + En^2)*(108 - 41*la + la^2 + 24*En^4*(16 + la) + 
      3*En^2*(-166 + 25*la + la^2))*rp[t]^18)*Derivative[1][rp][t])/
  (En*la*(1 + la)*rp[t]^12*(J^2 + rp[t]^2)^7)
]


Clear[fSourceCPM4]
fSourceCPM4[syms_Association]:=
Module[{mu,M,CapLa,J,la,XPhiBar,XPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	XPhiBar=XPhiSymbol[syms,Conjugate->True][t];
	XPhiPhiBar=XPhiPhiSymbol[syms,Conjugate->True][t];
	
((8*I)*J^3*m*mu*Pi*XPhiPhiBar*(2*M - rp[t])^3*
   (462*J^10*M^3 + J^10*(-579 + 16*la - 4*m^2)*M^2*rp[t] + 
    2*J^8*M*(2*J^2*(59 - 4*la + m^2) + 1287*M^2)*rp[t]^2 - 
    J^8*(J^2*(31 - 4*la + m^2) + (3247 - 966*En^2 - 80*la + 16*m^2)*M^2)*
     rp[t]^3 + 2*J^6*M*(J^2*(667 - 40*la + 8*m^2 - 
        4*En^2*(93 - 5*la + 2*m^2)) + 2898*M^2)*rp[t]^4 + 
    J^6*(J^2*(-177 + 20*la - 4*m^2 + 2*En^2*(67 - 10*la + 4*m^2)) + 
      2*(-3689 + 2460*En^2 + 80*la - 12*m^2)*M^2)*rp[t]^5 + 
    2*J^4*M*(J^2*(1533 + 168*En^4 - 80*la + 12*m^2 - 
        8*En^2*(241 - 10*la + 3*m^2)) + 3282*M^2)*rp[t]^6 + 
    J^4*(J^2*(-413 + 40*la - 6*m^2 - 8*En^4*(12 - 2*la + m^2) + 
        8*En^2*(89 - 10*la + 3*m^2)) + 2*(-4221 + 5022*En^2 + 80*la - 8*m^2)*
       M^2)*rp[t]^7 + 2*J^2*M*(J^2*(1777 + 768*En^4 - 80*la + 8*m^2 - 
        12*En^2*(341 - 10*la + 2*m^2)) + 1863*M^2)*rp[t]^8 + 
    J^2*(J^2*(-487 + 40*la - 4*m^2 - 16*En^4*(29 - 3*la + m^2) + 
        6*En^2*(267 - 20*la + 4*m^2)) + (-4843 + 9192*En^2 + 80*la - 4*m^2)*
       M^2)*rp[t]^9 + 2*M*(J^2*(1033 + 1752*En^4 - 40*la + 2*m^2 - 
        8*En^2*(486 - 10*la + m^2)) + 423*M^2)*rp[t]^10 + 
    (-(J^2*(288 - 20*la + m^2 + 8*En^4*(170 - 6*la + m^2) - 
         4*En^2*(401 - 20*la + 2*m^2))) + (-1111 + 3102*En^2 + 16*la)*M^2)*
     rp[t]^11 + 8*(60 + 288*En^4 - 2*la + En^2*(-338 + 5*la))*M*rp[t]^12 + 
    4*(-1 + En^2)*(17 + 120*En^4 + 4*En^2*(-32 + la) - la)*rp[t]^13))/
  (En^2*la*(1 + la)*rp[t]^13*(J^2 + rp[t]^2)^5) - 
 (16*J*mu*Pi*XPhiBar*(2*M - rp[t])^3*(168*J^10*M^3 + 
    4*J^10*(-51 + 2*la)*M^2*rp[t] + 8*J^8*M*(-(J^2*(-10 + la)) + 105*M^2)*
     rp[t]^2 + J^8*(2*J^2*(-5 + la) + (-1020 + 279*En^2 + 40*la)*M^2)*
     rp[t]^3 + 2*J^6*M*(J^2*(-20*(-10 + la) + En^2*(-103 + 4*la)) + 840*M^2)*
     rp[t]^4 + J^6*(J^2*(En^2*(35 - 4*la) + 10*(-5 + la)) + 
      8*(-255 + 153*En^2 + 10*la)*M^2)*rp[t]^5 + 
    2*J^4*M*(J^2*(42*En^4 - 40*(-10 + la) + En^2*(-457 + 16*la)) + 840*M^2)*
     rp[t]^6 + 2*J^4*(J^2*(En^2*(79 - 8*la) + En^4*(-11 + la) + 
        10*(-5 + la)) + (-1020 + 1029*En^2 + 40*la)*M^2)*rp[t]^7 + 
    2*J^2*M*(J^2*(162*En^4 - 40*(-10 + la) + 3*En^2*(-261 + 8*la)) + 420*M^2)*
     rp[t]^8 + J^2*(J^2*(6*En^4*(-15 + la) + 20*(-5 + la) - 
        3*En^2*(-93 + 8*la)) + 20*(-51 + 78*En^2 + 2*la)*M^2)*rp[t]^9 + 
    2*M*(J^2*(258*En^4 - 20*(-10 + la) + En^2*(-607 + 16*la)) + 84*M^2)*
     rp[t]^10 + (2*(-1 + En^2)*J^2*(3*En^2*(-29 + la) - 5*(-5 + la)) + 
      (-204 + 447*En^2 + 8*la)*M^2)*rp[t]^11 + 
    4*(-1 + En^2)*(-20 + 69*En^2 + 2*la)*M*rp[t]^12 + 
    2*(-1 + En^2)^2*(-5 + 24*En^2 + la)*rp[t]^13))/
  (En^2*(1 + la)*rp[t]^13*(J^2 + rp[t]^2)^4) - 
 (8*J^2*mu*Pi*XPhiPhiBar*(-2*M + rp[t])^2*(735*J^12*M^3 + 
    J^12*(-811 + 104*la - 52*m^2)*M^2*rp[t] + 
    2*J^10*M*(J^2*(141 - 46*la + 23*m^2) + 2364*M^2)*rp[t]^2 + 
    J^10*(-10*J^2*(3 - 2*la + m^2) + (-5231 + 1110*En^2 + 640*la - 284*m^2)*
       M^2)*rp[t]^3 + J^8*M*(J^2*(1825 - 568*la + 254*m^2 - 
        2*En^2*(343 - 86*la + 64*m^2)) + 12819*M^2)*rp[t]^4 + 
    J^8*(J^2*(-195 + 124*la - 56*m^2 + En^2*(90 - 68*la + 52*m^2)) + 
      2*(-7121 + 3273*En^2 + 820*la - 308*m^2)*M^2)*rp[t]^5 + 
    2*J^6*M*(J^2*(2497 + 90*En^4 - 730*la + 278*m^2 + 
        En^2*(-2047 + 482*la - 328*m^2)) + 9408*M^2)*rp[t]^6 + 
    J^6*(J^2*(-537 + 320*la - 124*m^2 - 24*En^4*(1 - la + m^2) + 
        14*En^2*(39 - 28*la + 20*m^2)) + 2*(-10529 + 7974*En^2 + 1120*la - 
        332*m^2)*M^2)*rp[t]^7 + 
    J^4*M*(4*J^2*(1864 + 270*En^4 - 500*la + 151*m^2 - 
        6*En^2*(419 - 89*la + 50*m^2)) + 15789*M^2)*rp[t]^8 + 
    J^4*(J^2*(-813 + 440*la - 136*m^2 - 168*En^4*(1 - la + m^2) + 
        6*En^2*(223 - 148*la + 88*m^2)) + (-17867 + 21108*En^2 + 1720*la - 
        356*m^2)*M^2)*rp[t]^9 + 
    2*J^2*M*(J^2*(3212 + 1008*En^4 - 770*la + 163*m^2 - 
        4*En^2*(1738 - 293*la + 118*m^2)) + 3588*M^2)*rp[t]^10 + 
    J^2*(J^2*(-717 + 340*la - 74*m^2 - 24*En^4*(3 - 15*la + 11*m^2) + 
        2*En^2*(999 - 496*la + 212*m^2)) + (-8239 + 15006*En^2 + 704*la - 
        76*m^2)*M^2)*rp[t]^11 + 
    M*(J^2*(3023 + 3912*En^4 - 632*la + 70*m^2 - 
        2*En^2*(5293 - 638*la + 136*m^2)) + 1377*M^2)*rp[t]^12 + 
    2*(-2*J^2*(87 + 120*En^6 - 35*la + 4*m^2 + 
        En^2*(-429 + 137*la - 31*m^2) + 6*En^4*(39 - 13*la + 5*m^2)) + 
      3*(-268 + 735*En^2 + 20*la)*M^2)*rp[t]^13 + 
    2*(302 + 1398*En^4 - 54*la + 3*En^2*(-555 + 46*la))*M*rp[t]^14 + 
    24*(-1 + En^2)*(3 + 20*En^4 - la + En^2*(-22 + 4*la))*rp[t]^15)*
   Derivative[1][rp][t])/(En*la*(1 + la)*rp[t]^11*(J^2 + rp[t]^2)^6)
]


Clear[gSourceCPM5]
gSourceCPM5[syms_Association]:=
Module[{mu,M,CapLa,J,la,XPhiBar,XPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	XPhiBar=XPhiSymbol[syms,Conjugate->True][t];
	XPhiPhiBar=XPhiPhiSymbol[syms,Conjugate->True][t];

(-8*J^2*mu*Pi*XPhiPhiBar*(-2*M + rp[t])^2*(579600*J^18*M^5 - 
    21*J^18*(48197 - 4672*la + 4302*m^2)*M^4*rp[t] + 
    J^16*M^3*(J^2*(678033 - 150768*la + 1216*la^2 - 3*(-47009 + 752*la)*m^2 + 
        824*m^4) + 5620428*M^2)*rp[t]^2 + 
    J^16*M^2*(-4*J^2*(53786 - 21076*la + 396*la^2 + (20060 - 756*la)*m^2 + 
        279*m^4) + (-9825561 + 457020*En^2 + 927984*la - 820164*m^2)*M^2)*
     rp[t]^3 + 2*J^14*M*(J^4*(2*(7929 - 5041*la + 168*la^2) + 
        (9781 - 666*la)*m^2 + 249*m^4) + 
      J^2*(-3*En^2*(94408 - 11543*la + 11988*m^2) + 
        4*(4*(205926 - 44673*la + 346*la^2) - 3*(-53631 + 788*la)*m^2 + 
          791*m^4))*M^2 + 12213108*M^4)*rp[t]^4 - 
    J^14*(J^4*(4*(420 - 430*la + 23*la^2) + (1703 - 192*la)*m^2 + 73*m^4) + 
      J^2*(2093319 - 801548*la + 14448*la^2 + (736765 - 25488*la)*m^2 + 
        8652*m^4 - 2*En^2*(122089 - 35734*la + 152*la^2 + 
          (38037 - 752*la)*m^2 + 408*m^4))*M^2 - 
      36*(-1187762 + 115372*En^2 + 108768*la - 91387*m^2)*M^4)*rp[t]^5 + 
    J^12*M*(J^4*(308931 - 192308*la + 6144*la^2 + (180859 - 11304*la)*m^2 + 
        3906*m^4 - 2*En^2*(21041 - 11576*la + 92*la^2 + (12639 - 612*la)*
           m^2 + 348*m^4)) + 2*J^2*(14358837 + 46170*En^4 - 3025104*la + 
        22400*la^2 - 8*(-324887 + 4326*la)*m^2 + 10332*m^4 - 
        6*En^2*(430705 - 51449*la + 51744*m^2))*M^2 + 62582328*M^4)*rp[t]^6 + 
    J^12*(J^4*(-4*(4095 - 4115*la + 211*la^2) + (-15871 + 1644*la)*m^2 - 
        581*m^4 + 2*En^2*(1155 - 1155*la + 8*la^2 + (1287 - 118*la)*m^2 + 
          72*m^4)) + J^2*(-9136023 + 3404080*la - 58560*la^2 + 
        (-2998505 + 93744*la)*m^2 - 28476*m^4 - 
        12*En^4*(5672 - 847*la + 1122*m^2) + 2*En^2*(1118483 - 320789*la + 
          1232*la^2 + (331917 - 5872*la)*m^2 + 3008*m^4))*M^2 + 
      12*(-9144772 + 1396956*En^2 + 806336*la - 634775*m^2)*M^4)*rp[t]^7 + 
    2*J^10*M*(J^4*(675144 - 409964*la + 12480*la^2 + (371311 - 20916*la)*
         m^2 + 6489*m^4 + 2*En^4*(-31*(-115 + 43*la + 2*la^2) + 
          (1875 - 8*la)*m^2 + 40*m^4) - En^2*(193441 - 104721*la + 752*la^2 + 
          (111659 - 4892*la)*m^2 + 2648*m^4)) + 
      J^2*(36913257 + 401850*En^4 - 7503120*la + 52864*la^2 - 
        240*(-25293 + 301*la)*m^2 + 18620*m^4 - 
        6*En^2*(1747343 - 203307*la + 196810*m^2))*M^2 + 52264980*M^4)*
     rp[t]^8 - J^10*(J^4*(71700 - 70492*la + 3440*la^2 + 
        (65838 - 6132*la)*m^2 + 1953*m^4 + 4*En^4*(192 - 160*la - 31*la^2 + 
          (226 + 6*la)*m^2 + 14*m^4) - 2*En^2*(10635 - 10525*la + 68*la^2 + 
          (11527 - 978*la)*m^2 + 572*m^4)) + 
      J^2*(23527245 - 2880*En^6 - 8479712*la + 138432*la^2 + 
        (7075405 - 196560*la)*m^2 + 51660*m^4 + 
        12*En^4*(49876 - 7375*la + 9440*m^2) - 
        2*En^2*(4563231 - 1279059*la + 4368*la^2 - 5*(-255829 + 3888*la)*
           m^2 + 8920*m^4))*M^2 - 6*(-30616971 + 6558360*En^2 + 2572096*la - 
        1853220*m^2)*M^4)*rp[t]^9 + 
    J^8*M*(-(J^4*(-3483567 + 2052968*la - 59136*la^2 + 5*(-354833 + 17640*la)*
          m^2 - 23730*m^4 + 72*En^6*(3*(4 + la) + 2*m^2) - 
         12*En^4*(10526 - 3955*la - 170*la^2 - 20*(-270 + la)*m^2 + 
           104*m^4) + 2*En^2*(793533 - 421560*la + 2688*la^2 - 
           5*(-87193 + 3300*la)*m^2 + 8020*m^4))) + 
      2*J^2*(61921542 + 1546938*En^4 - 12020160*la + 80192*la^2 + 
        (8954235 - 94080*la)*m^2 + 20020*m^4 - 
        6*En^2*(4127297 - 466157*la + 431680*m^2))*M^2 + 118674000*M^4)*
     rp[t]^10 + J^8*(J^4*(-8*(23175 - 22198*la + 1022*la^2) + 
        10*(-15973 + 1302*la)*m^2 - 3605*m^4 + 48*En^6*(1 + la)*
         (1 - la + m^2) - 12*En^4*(568 - 480*la - 85*la^2 + 
          10*(67 + la)*m^2 + 42*m^4) + 4*En^2*(3*(7285 - 7118*la + 42*la^2) - 
          5*(-4541 + 339*la)*m^2 + 890*m^4)) + 
      J^2*(-39530547 + 25920*En^6 + 13656976*la - 210336*la^2 + 
        5*(-2113885 + 51408*la)*m^2 - 55860*m^4 - 
        12*En^4*(194070 - 28431*la + 34250*m^2) + 
        2*En^2*(10875647 - 2970749*la + 8848*la^2 - 5*(-574853 + 7088*la)*
           m^2 + 13760*m^4))*M^2 + 6*(-34895847 + 9826380*En^2 + 2745552*la - 
        1742722*m^2)*M^4)*rp[t]^11 + 
    2*J^6*M*(-(J^4*(-2928102 + 1663952*la - 45024*la^2 + 
         10*(-134611 + 5796*la)*m^2 - 12915*m^4 + 
         36*En^6*(103 + 22*la + 18*m^2) - 
         2*En^4*(-3*(-40262 + 15429*la + 586*la^2) + (57535 - 180*la)*m^2 + 
           848*m^4) + En^2*(1923223 - 994405*la + 5488*la^2 - 
           5*(-202061 + 6108*la)*m^2 + 12560*m^4))) + 
      J^2*(70849509 + 3491082*En^4 - 12897264*la + 81088*la^2 + 
        (8519396 - 78288*la)*m^2 + 12852*m^4 - 
        30*En^2*(1237331 - 135578*la + 116742*m^2))*M^2 + 46091484*M^4)*
     rp[t]^12 + J^6*(J^4*(-4*(77760 - 72553*la + 3122*la^2) + 
        5*(-49471 + 3444*la)*m^2 - 3955*m^4 + 432*En^6*(1 + la)*
         (1 - la + m^2) - 4*En^4*(5166 - 5349*la - 879*la^2 + 
          20*(304 + 3*la)*m^2 + 364*m^4) + 4*En^2*(55395 - 51279*la + 
          266*la^2 + (54845 - 3205*la)*m^2 + 1420*m^4)) + 
      J^2*(-45383079 + 89280*En^6 + 14745944*la - 213024*la^2 + 
        (-10200415 + 214704*la)*m^2 - 36036*m^4 - 
        12*En^4*(473786 - 64775*la + 79040*m^2) + 
        10*En^2*(3245609 - 878819*la + 2240*la^2 + (802531 - 7696*la)*m^2 + 
          2344*m^4))*M^2 + 12*(-13654928 + 4953180*En^2 + 980512*la - 
        514247*m^2)*M^4)*rp[t]^13 + 
    J^4*M*(J^4*(6734241 - 3621584*la + 91392*la^2 + (2645077 - 97272*la)*
         m^2 + 16758*m^4 + 24*En^6*(603 - 151*la + 84*m^2) + 
        4*En^4*(365963 - 110915*la - 3310*la^2 - 40*(-3691 + 7*la)*m^2 + 
          1072*m^4) - 10*En^2*(564841 - 300708*la + 1400*la^2 + 
          (294477 - 6716*la)*m^2 + 2164*m^4)) + 
      2*J^2*(55920771 + 4250718*En^4 - 9265968*la + 54656*la^2 + 
        (5090172 - 40656*la)*m^2 + 4564*m^4 - 6*En^2*(6258059 - 638619*la + 
          478864*m^2))*M^2 + 47513736*M^4)*rp[t]^14 + 
    J^4*(J^4*(-4*(89085 - 79760*la + 3178*la^2) + 3*(-82921 + 4844*la)*m^2 - 
        2583*m^4 + 96*En^6*(-202 + 13*la - 15*la^2 + (-62 + 13*la)*m^2) - 
        4*En^4*(33174 - 13445*la - 1655*la^2 + 20*(962 + 3*la)*m^2 + 
          476*m^4) + 10*En^2*(31131 - 31939*la + 140*la^2 + 
          (34031 - 1434*la)*m^2 + 496*m^4)) + 
      J^2*(-36164709 + 604800*En^6 + 10670608*la - 143808*la^2 + 
        (-6186515 + 111888*la)*m^2 - 12852*m^4 - 
        12*En^4*(519924 - 92645*la + 109190*m^2) + 
        2*En^2*(16320009 - 4223559*la + 9072*la^2 + (3403847 - 24912*la)*
           m^2 + 5248*m^4))*M^2 + 12*(-7129626 + 3419916*En^2 + 451744*la - 
        173741*m^2)*M^4)*rp[t]^15 + 
    2*J^2*M*(-(J^4*(7200*En^8 - 4*(677772 - 330581*la + 7728*la^2) + 
         (-817267 + 25452*la)*m^2 - 3003*m^4 + 24*En^6*(10949 - 118*la + 
           742*m^2) - 6*En^4*(103899 - 57145*la - 1230*la^2 + 
           (77195 - 80*la)*m^2 + 216*m^4) + 
         En^2*(3*(915437 - 494777*la + 1904*la^2) + (1305509 - 21972*la)*
            m^2 + 4888*m^4))) + J^2*(29667147 + 3668382*En^4 - 4296624*la + 
        23680*la^2 - 24*(-72549 + 502*la)*m^2 + 692*m^4 - 
        6*En^2*(4440165 - 379309*la + 217774*m^2))*M^2 + 7391124*M^4)*
     rp[t]^16 + J^2*(J^4*(960*En^8*(16 + la) - 
        4*(72405 - 58945*la + 2156*la^2) + 4*(-39374 + 1911*la)*m^2 - 
        931*m^4 + 96*En^6*(1442 + 7*la - 25*la^2 + (42 + 17*la)*m^2) - 
        12*En^4*(5472 - 7830*la - 615*la^2 + 10*(1265 + la)*m^2 + 98*m^4) + 
        2*En^2*(3*(43925 - 54555*la + 196*la^2) + (160547 - 4758*la)*m^2 + 
          1132*m^4)) - J^2*(19581183 + 267840*En^6 - 4987520*la + 
        62400*la^2 - 7*(-306865 + 4752*la)*m^2 + 1956*m^4 + 
        12*En^4*(391608 - 80757*la + 77792*m^2) - 
        2*En^2*(11969413 - 2563729*la + 4592*la^2 + (1596307 - 8912*la)*m^2 + 
          968*m^4))*M^2 + 3*(-9042543 + 6003024*En^2 + 487104*la - 
        102710*m^2)*M^4)*rp[t]^17 + 
    M*(J^4*(3017373 + 139200*En^8 - 1248728*la + 26880*la^2 + 
        (578107 - 15192*la)*m^2 + 918*m^4 + En^6*(371952 + 26136*la - 
          81744*m^2) + 4*En^4*(-3*(-30896 + 53901*la + 814*la^2) - 
          4*(-45160 + 27*la)*m^2 + 152*m^4) - 2*En^2*(2096051 - 927416*la + 
          2912*la^2 + (636739 - 7932*la)*m^2 + 908*m^4)) + 
      J^2*(19274739 + 5804316*En^4 - 2332560*la + 11968*la^2 + 
        (520895 - 3120*la)*m^2 - 12*En^2*(2063043 - 129407*la + 42000*m^2))*
       M^2 + 2110500*M^4)*rp[t]^18 + 
    (J^4*(960*En^8*(-88 + la) - 16*(10485 - 7052*la + 235*la^2) + 
        4*(-14257 + 573*la)*m^2 - 143*m^4 + 
        48*En^6*(-3*(503 + 38*la + 15*la^2) + (571 + 21*la)*m^2) - 
        4*En^4*(-3*(9648 + 8350*la + 407*la^2) + (33806 + 6*la)*m^2 + 
          70*m^4) + 4*En^2*(52320 - 53235*la + 154*la^2 + 
          (41246 - 869*la)*m^2 + 106*m^4)) - 
      J^2*(6569385 + 169920*En^6 - 1365472*la + 15792*la^2 - 
        5*(-65191 + 864*la)*m^2 + 60*En^4*(81558 - 7769*la + 4342*m^2) + 
        2*En^2*(-6039461 + 894119*la - 1328*la^2 + 85*(-3719 + 16*la)*m^2))*
       M^2 + 9*(-440749 + 414780*En^2 + 19504*la)*M^4)*rp[t]^19 + 
    2*M*(J^2*(529776 - 108000*En^8 - 172814*la + 3408*la^2 + 
        (44635 - 990*la)*m^2 + En^6*(237492 + 13704*la - 21480*m^2) - 
        10*En^4*(-54944 + 16637*la + 178*la^2 + (-10697 + 4*la)*m^2) + 
        En^2*(-1209313 + 332895*la - 848*la^2 + 5*(-26051 + 244*la)*m^2)) + 
      3*(484269 + 394890*En^4 - 47040*la + 224*la^2 + 
        En^2*(-915474 + 38709*la))*M^2)*rp[t]^20 + 
    2*(-2*(-1 + En^2)*J^2*(-15810 + 240*En^6*(-98 + la) + 7921*la - 
        239*la^2 + 75*(-30 + la)*m^2 + En^2*(24030 - 11953*la - 193*la^2 - 
          60*(-109 + la)*m^2) - 12*En^4*(-1285 - 186*la - 21*la^2 + 
          5*(71 + la)*m^2)) + (-515491 + 318240*En^6 + 83358*la - 888*la^2 + 
        6*En^4*(-212186 + 7861*la) + 3*En^2*(490274 - 45533*la + 56*la^2))*
       M^2)*rp[t]^21 + 12*(-1 + En^2)*(-14652 + 5040*En^6 + 3557*la - 
      64*la^2 + En^4*(-31922 + 766*la) + En^2*(41591 - 5153*la - 46*la^2))*M*
     rp[t]^22 - 12*(-1 + En^2)^2*(950 - 331*la + 9*la^2 + 80*En^4*(22 + la) + 
      16*En^2*(-170 + 26*la + la^2))*rp[t]^23))/
  (En*la*(1 + la)*rp[t]^15*(J^2 + rp[t]^2)^9) + 
 ((-16*J*mu*Pi*XPhiBar*(2*M - rp[t])*(57750*J^14*M^4 + 
      J^14*(-79189 + 1744*la)*M^3*rp[t] + 2*J^12*M^2*
       (-4*J^2*(-4853 + 254*la + 2*la^2) + 208443*M^2)*rp[t]^2 + 
      J^12*M*(4*J^2*(-1987 + 191*la + 4*la^2) + 
        (-572309 + 28320*En^2 + 12464*la)*M^2)*rp[t]^3 + 
      2*J^10*(-2*J^4*(-140 + 23*la + la^2) + J^2*(En^2*(-12312 + 197*la) - 
          8*(-17557 + 909*la + 7*la^2))*M^2 + 647619*M^4)*rp[t]^4 + 
      J^10*M*(J^2*(-57571 + 5476*la + 112*la^2 - 2*En^2*(-3231 + 120*la + 
            8*la^2)) + (-1781113 + 192336*En^2 + 38224*la)*M^2)*rp[t]^5 + 
      2*J^8*(J^4*(2030 - 330*la - 14*la^2 + En^2*(-245 + 17*la + 4*la^2)) + 
        J^2*(1200*En^4 + En^2*(-84341 + 1410*la) - 
          4*(-109477 + 5586*la + 42*la^2))*M^2 + 1123779*M^4)*rp[t]^6 + 
      J^8*M*(-(J^2*(179831 - 16860*la - 336*la^2 + 4*En^4*(239 + 26*la) + 
           4*En^2*(-11163 + 443*la + 24*la^2))) + 
        (-3098233 + 554016*En^2 + 65200*la)*M^2)*rp[t]^7 + 
      2*J^6*(J^4*(6353 - 1018*la - 42*la^2 - 2*En^4*(-20 - 7*la + la^2) + 
          En^2*(-1705 + 128*la + 24*la^2)) + 
        J^2*(8112*En^4 + 5*En^2*(-49105 + 839*la) - 
          10*(-76383 + 3820*la + 28*la^2))*M^2 + 1177881*M^4)*rp[t]^8 + 
      J^6*M*(-2*J^2*(En^4*(3250 + 268*la) + 3*En^2*(-21917 + 910*la + 
            40*la^2) - 2*(-78657 + 7225*la + 140*la^2)) + 
        (-3259247 + 873792*En^2 + 66800*la)*M^2)*rp[t]^9 + 
      2*(J^8*(En^4*(260 + 82*la - 10*la^2) - 7*(-1593 + 250*la + 10*la^2) + 
          En^2*(-5089 + 410*la + 60*la^2)) + 
        2*J^6*(11412*En^4 + En^2*(-196011 + 3310*la) - 
          10*(-40351 + 1962*la + 14*la^2))*M^2 + 746889*J^4*M^4)*rp[t]^10 + 
      J^4*M*(-2*J^2*(167081 - 14890*la - 280*la^2 + 12*En^4*(815 + 48*la) + 
          4*En^2*(-26487 + 1115*la + 40*la^2)) + 
        (-2077807 + 813120*En^2 + 41104*la)*M^2)*rp[t]^11 + 
      2*(J^6*(11919 - 1810*la - 70*la^2 - 4*En^4*(-256 - 50*la + 5*la^2) + 
          En^2*(-8143 + 700*la + 80*la^2)) + 
        J^4*(30936*En^4 + En^2*(-374606 + 5835*la) - 
          4*(-129493 + 6054*la + 42*la^2))*M^2 + 265689*J^2*M^4)*rp[t]^12 + 
      J^2*M*(J^2*(-216371 + 768*En^6 + 18444*la + 336*la^2 - 
          40*En^4*(503 + 32*la) + En^2*(209226 - 8120*la - 240*la^2)) + 
        3*(-248257 + 143056*En^2 + 4688*la)*M^2)*rp[t]^13 + 
      2*(-(J^4*(-7811 + 1126*la + 42*la^2 + 48*En^6*(11 + la) + 
           En^2*(8350 - 665*la - 60*la^2) + 4*En^4*(38 - 62*la + 5*la^2))) + 
        J^2*(187516 + 34248*En^4 - 8312*la - 56*la^2 + 
          En^2*(-207413 + 2722*la))*M^2 + 40953*M^4)*rp[t]^14 + 
      M*(-(J^2*(79403 + 6144*En^6 - 6356*la - 112*la^2 + 
           4*En^4*(7895 + 182*la) + 12*En^2*(-10357 + 325*la + 8*la^2))) + 
        3*(-38657 + 33280*En^2 + 688*la)*M^2)*rp[t]^15 + 
      2*(-2*(-1 + En^2)*J^2*(1462 - 195*la - 7*la^2 + 24*En^4*(-33 + 2*la) + 
          En^2*(-1324 - 29*la + 5*la^2)) + (29598 + 19224*En^4 - 1224*la - 
          8*la^2 + En^2*(-51333 + 525*la))*M^2)*rp[t]^16 + 
      2*(-1 + En^2)*(6387 + 2304*En^4 - 470*la - 8*la^2 - 
        6*En^2*(1739 + 14*la))*M*rp[t]^17 - 4*(-1 + En^2)^2*
       (-242 + 29*la + la^2 + 24*En^2*(16 + la))*rp[t]^18))/
    ((1 + la)*rp[t]^12*(J^2 + rp[t]^2)^7) + 
   ((16*I)*J^3*m*mu*Pi*XPhiPhiBar*(2*M - rp[t])*(124635*J^14*M^4 + 
      J^14*(-175259 + 7934*la - 4266*m^2)*M^3*rp[t] + 
      J^12*M^2*(J^2*(88366 - 9463*la + 16*la^2 + (5161 - 28*la)*m^2 + 
          8*m^4) + 989871*M^2)*rp[t]^2 + 
      2*J^12*M*(-(J^2*(9334 - 1825*la + 8*la^2 - 2*(-506 + 7*la)*m^2 + 
           4*m^4)) + 3*(-232911 + 11045*En^2 + 9975*la - 4990*m^2)*M^2)*
       rp[t]^3 + J^10*(J^4*(1363 - 451*la + 4*la^2 + (255 - 7*la)*m^2 + 
          2*m^4) + J^2*(707696 - 71841*la + 112*la^2 + (36662 - 168*la)*m^2 + 
          40*m^4 + En^2*(-59444 + 3454*la - 2576*m^2))*M^2 + 3424815*M^4)*
       rp[t]^4 + J^10*M*(2*J^2*(-75116 + 13958*la - 56*la^2 + 
          (-7291 + 84*la)*m^2 - 20*m^4 + 2*En^2*(4026 - 584*la - 6*la^2 + 
            (457 - 2*la)*m^2 + 2*m^4)) + 3*(-1620139 + 167672*En^2 + 
          64650*la - 29330*m^2)*M^2)*rp[t]^5 + 
      J^8*(J^4*(11028 - 3479*la + 28*la^2 + (1868 - 42*la)*m^2 + 10*m^4 - 
          2*En^2*(628 - 181*la - 6*la^2 - 2*(-75 + la)*m^2 + 2*m^4)) + 
        J^2*(6060*En^4 + 3*(825179 - 78173*la + 112*la^2) - 
          5*(-21863 + 84*la)*m^2 + 80*m^4 - 4*En^2*(114176 - 6419*la + 
            4536*m^2))*M^2 + 6722595*M^4)*rp[t]^6 + 
      2*J^8*M*(-2*J^2*(3*(44069 - 7662*la + 28*la^2) - 5*(-2213 + 21*la)*
           m^2 + 20*m^4 + En^4*(614 + 6*la + 46*m^2) + 
          En^2*(-31352 + 4455*la + 36*la^2 + (-3321 + 10*la)*m^2 - 8*m^4)) + 
        (-4806290 + 822027*En^2 + 174845*la - 68940*m^2)*M^2)*rp[t]^7 + 
      J^6*(J^4*(6*(6515 - 1931*la + 14*la^2) + (5800 - 105*la)*m^2 + 20*m^4 + 
          8*En^4*(25 + 3*la - 2*la^2 + (5 + la)*m^2) - 
          4*En^2*(2480 - 709*la - 18*la^2 + (564 - 5*la)*m^2 + 4*m^4)) + 
        J^2*(46500*En^4 - 2*En^2*(756224 - 40097*la + 26448*m^2) + 
          5*(987229 - 85237*la + 112*la^2 - 4*(-8693 + 28*la)*m^2 + 16*m^4))*
         M^2 + 8131185*M^4)*rp[t]^8 + 
      J^6*M*(-(J^2*(32*En^4*(599 - 9*la + 46*m^2) + 
           En^2*(-422218 + 57684*la + 360*la^2 + 8*(-5117 + 10*la)*m^2 - 
             48*m^4) + 5*(212699 - 33746*la + 112*la^2 - 4*(-3585 + 28*la)*
              m^2 + 16*m^4))) + 3*(1005632*En^2 - 
          5*(784595 - 25238*la + 8074*m^2))*M^2)*rp[t]^9 + 
      (2*J^8*(8*En^4*(93 + 8*la - 5*la^2 + 2*(9 + la)*m^2) + 
          5*(7922 - 2153*la + 14*la^2 + (962 - 14*la)*m^2 + 2*m^4) - 
          2*En^2*(8590 - 2414*la - 45*la^2 - 2*(-943 + 5*la)*m^2 + 6*m^4)) + 
        J^6*(151560*En^4 - 8*En^2*(349654 - 16637*la + 9448*m^2) + 
          5*(1226192 - 93033*la + 112*la^2 + (30979 - 84*la)*m^2 + 8*m^4))*
         M^2 + 6075405*J^4*M^4)*rp[t]^10 + 
      2*J^4*M*(-(J^2*(8*En^4*(4541 - 153*la + 397*m^2) + 
           5*(134384 - 18605*la + 56*la^2 + (6500 - 42*la)*m^2 + 4*m^4) - 
           4*En^2*(96674 - 12407*la - 60*la^2 + (7673 - 10*la)*m^2 + 
             4*m^4))) + 3*(-1492319 + 581879*En^2 + 40989*la - 9406*m^2)*M^2)*
       rp[t]^11 + (J^6*(16*En^4*(-2*(-317 + 8*la + 5*la^2) + 
            (134 + 3*la)*m^2) + 5*(20439 - 4811*la + 28*la^2 + 
            (1783 - 21*la)*m^2 + 2*m^4) - 8*En^2*(7300 - 2188*la - 30*la^2 + 
            (1512 - 5*la)*m^2 + 2*m^4)) + J^4*(4771626 + 189720*En^4 - 
          304683*la + 336*la^2 + (73126 - 168*la)*m^2 + 8*m^4 - 
          2*En^2*(1697794 - 61537*la + 26088*m^2))*M^2 + 2590341*J^2*M^4)*
       rp[t]^12 + J^2*M*(2*J^2*(-538689 + 3600*En^6 + 61560*la - 168*la^2 + 
          (-15587 + 84*la)*m^2 - 4*m^4 - 24*En^4*(355 - 112*la + 188*m^2) + 
          2*En^2*(250247 - 23706*la - 90*la^2 + (10989 - 10*la)*m^2 + 
            2*m^4)) + (-3907271 + 2390616*En^2 + 88730*la - 10890*m^2)*M^2)*
       rp[t]^13 + (J^4*(85368 - 16131*la + 84*la^2 - 480*En^6*(16 + la) + 
          (4360 - 42*la)*m^2 + 2*m^4 + 16*En^4*(-1256 - 81*la - 10*la^2 + 
            2*(112 + la)*m^2) - 2*En^2*(41500 - 8753*la - 90*la^2 + 
            (4542 - 10*la)*m^2 + 2*m^4)) + J^2*(2144881 + 403020*En^4 - 
          110821*la + 112*la^2 + (14277 - 28*la)*m^2 - 
          4*En^2*(630604 - 14995*la + 3480*m^2))*M^2 + 483105*M^4)*rp[t]^14 + 
      2*M*(-2*J^2*(125948 + 13440*En^6 - 11309*la + 28*la^2 + 
          (1543 - 7*la)*m^2 + 6*En^4*(8461 - 195*la + 165*m^2) + 
          En^2*(-210064 + 11883*la + 36*la^2 + (-3013 + 2*la)*m^2)) + 
        (-373956 + 354465*En^2 + 6855*la)*M^2)*rp[t]^15 + 
      (J^2*(42208 - 960*En^6*(-30 + la) - 6004*la + 28*la^2 + 
          (878 - 7*la)*m^2 + 4*En^2*(-21544 + 2279*la + 18*la^2 + 
            (-644 + la)*m^2) + 8*En^4*(1871 - 179*la - 10*la^2 + 
            (211 + la)*m^2)) + (424277 + 324420*En^4 - 17259*la + 16*la^2 + 
          6*En^2*(-134812 + 2005*la))*M^2)*rp[t]^16 + 
      (-104023 + 46560*En^6 + 7114*la - 16*la^2 + 48*En^4*(-5041 + 30*la) + 
        En^2*(299746 - 9764*la - 24*la^2))*M*rp[t]^17 - 
      4*(-1 + En^2)*(2310 - 239*la + la^2 + 120*En^4*(36 + la) + 
        4*En^2*(-1665 + 61*la + la^2))*rp[t]^18))/
    (la*(1 + la)*rp[t]^12*(J^2 + rp[t]^2)^8))*Derivative[1][rp][t]
]


Clear[fSourceCPM5]
fSourceCPM5[syms_Association]:=
Module[{mu,M,CapLa,J,la,XPhiBar,XPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	XPhiBar=XPhiSymbol[syms,Conjugate->True][t];
	XPhiPhiBar=XPhiPhiSymbol[syms,Conjugate->True][t];
	
(-8*J^2*mu*Pi*XPhiPhiBar*(2*M - rp[t])^3*(36225*J^16*M^4 - 
    30*J^16*(1747 - 200*la + 149*m^2)*M^3*rp[t] + 
    5*J^14*M^2*(J^2*(5417 - 1480*la + 16*la^2 + (1115 - 16*la)*m^2 + 4*m^4) + 
      62676*M^2)*rp[t]^2 + 2*J^14*M*
     (-2*J^2*(4*(363 - 185*la + 5*la^2) + (565 - 20*la)*m^2 + 5*m^4) + 
      15*(-15146 + 1302*En^2 + 1680*la - 1183*m^2)*M^2)*rp[t]^3 + 
    J^12*(5*J^4*(4*(21 - 19*la + la^2) + (59 - 4*la)*m^2 + m^4) - 
      5*J^2*(-47083 + 12480*la - 128*la^2 + 7*(-1275 + 16*la)*m^2 - 24*m^4 + 
        18*En^2*(414 - 71*la + 62*m^2))*M^2 + 1197144*M^4)*rp[t]^4 + 
    2*J^12*M*(J^2*(-25317 + 12540*la - 320*la^2 + 35*(-261 + 8*la)*m^2 - 
        60*m^4 + 10*En^2*(532 - 239*la + 4*la^2 + (213 - 6*la)*m^2 + 
          2*m^4)) + 3*(-290026 + 52410*En^2 + 30960*la - 20265*m^2)*M^2)*
     rp[t]^5 + J^10*(-5*J^4*(-735 + 648*la - 32*la^2 + 7*(-69 + 4*la)*m^2 - 
        6*m^4 + 2*En^2*(84 - 83*la + 4*la^2 + (76 - 6*la)*m^2 + 2*m^4)) + 
      J^2*(904183 + 10620*En^4 - 231040*la + 2240*la^2 - 
        105*(-1471 + 16*la)*m^2 + 300*m^4 - 30*En^2*(10074 - 1671*la + 
          1396*m^2))*M^2 + 2645604*M^4)*rp[t]^6 + 
    2*J^10*M*(J^2*(-97566 + 46700*la - 1120*la^2 + 105*(-305 + 8*la)*m^2 - 
        150*m^4 - 60*En^4*(41 - 13*la + 13*m^2) + 
        20*En^2*(2178 - 949*la + 14*la^2 + (814 - 18*la)*m^2 + 5*m^4)) + 
      3*(-642906 + 183846*En^2 + 65360*la - 38675*m^2)*M^2)*rp[t]^7 + 
    J^8*(J^4*(4*En^4*(96 - 105*la + 5*la^2 - 10*(-11 + la)*m^2 + 4*m^4) - 
        10*En^2*(696 - 671*la + 28*la^2 + (596 - 36*la)*m^2 + 10*m^4) + 
        5*(4*(711 - 608*la + 28*la^2) - 42*(-41 + 2*la)*m^2 + 15*m^4)) + 
      J^2*(2010743 + 81432*En^4 - 490240*la + 4480*la^2 - 
        175*(-1703 + 16*la)*m^2 + 400*m^4 - 6*En^2*(178286 - 28505*la + 
          22630*m^2))*M^2 + 3718530*M^4)*rp[t]^8 + 
    2*J^8*M*(J^2*(-217623 + 180*En^6 + 99740*la - 2240*la^2 + 
        350*(-179 + 4*la)*m^2 - 200*m^4 - 30*En^4*(645 - 194*la + 186*m^2) + 
        2*En^2*(78111 - 32955*la + 420*la^2 - 5*(-5449 + 90*la)*m^2 + 
          100*m^4)) + 15*(72858*En^2 + 216*(-841 + 80*la) - 8855*m^2)*M^2)*
     rp[t]^9 + J^6*(2*J^4*(4*En^4*(387 - 410*la + 15*la^2 - 
          5*(-83 + 5*la)*m^2 + 8*m^4) - 10*En^2*(3*(426 - 399*la + 14*la^2) + 
          (1046 - 45*la)*m^2 + 10*m^4) + 5*(3177 - 2620*la + 112*la^2 - 
          35*(-49 + 2*la)*m^2 + 10*m^4)) + 
      J^2*(275796*En^4 - 30*En^2*(70758 - 10895*la + 7864*m^2) + 
        25*(114289 - 26064*la + 224*la^2 - 7*(-1971 + 16*la)*m^2 + 12*m^4))*
       M^2 + 3421260*M^4)*rp[t]^10 + 
    2*J^6*M*(2*J^2*(810*En^6 + En^4*(-35661 + 9900*la - 9780*m^2) - 
        5*(31122 - 13350*la + 280*la^2 - 70*(-105 + 2*la)*m^2 + 15*m^4) + 
        5*En^2*(30803 - 12850*la + 140*la^2 + (9776 - 120*la)*m^2 + 
          20*m^4)) + 15*(-168638 + 91170*En^2 + 14640*la - 6069*m^2)*M^2)*
     rp[t]^11 + 
    (J^8*(-20*En^2*(5*(486 - 481*la + 14*la^2) + (1964 - 60*la)*m^2 + 
          10*m^4) + 5*(8*(1143 - 885*la + 35*la^2) - 35*(-117 + 4*la)*m^2 + 
          15*m^4) + 4*En^4*(3726 - 3125*la + 75*la^2 - 20*(-174 + 5*la)*m^2 + 
          24*m^4)) + J^6*(440784*En^4 - 30*En^2*(89510 - 12535*la + 
          7606*m^2) + 5*(536473 - 111040*la + 896*la^2 + 
          (47775 - 336*la)*m^2 + 24*m^4))*M^2 + 2018160*J^4*M^4)*rp[t]^12 + 
    2*J^4*M*(J^2*(-296499 + 13608*En^6 + 114580*la - 2240*la^2 + 
        105*(-491 + 8*la)*m^2 - 60*m^4 - 12*En^4*(8109 - 3050*la + 
          2750*m^2) + 10*En^2*(35*(1122 - 431*la + 4*la^2) + 
          (9719 - 90*la)*m^2 + 10*m^4)) + 15*(-100862 + 74166*En^2 + 
        7760*la - 2303*m^2)*M^2)*rp[t]^13 + 
    (J^6*(-480*En^6*(20 - 3*la + 6*m^2) + 16*En^4*(486 - 1600*la + 25*la^2 - 
          5*(-328 + 5*la)*m^2 + 4*m^4) + 5*(8895 - 6136*la + 224*la^2 + 
          (2919 - 84*la)*m^2 + 6*m^4) - 10*En^2*(6120 - 5815*la + 140*la^2 + 
          (4052 - 90*la)*m^2 + 10*m^4)) + 
      J^4*(504036*En^4 - 30*En^2*(75526 - 8653*la + 3860*m^2) + 
        5*(326753 - 59200*la + 448*la^2 - 7*(-2615 + 16*la)*m^2 + 4*m^4))*
       M^2 + 697644*J^2*M^4)*rp[t]^14 + 
    2*J^2*M*(-(J^2*(10296*En^6 + 2*(92631 - 30770*la + 560*la^2) - 
         35*(-571 + 8*la)*m^2 + 10*m^4 + 12*En^4*(9276 - 3125*la + 
           2145*m^2) - 20*En^2*(17523 - 5307*la + 42*la^2 + 
           (2522 - 18*la)*m^2 + m^4))) + (-532698 + 542790*En^2 + 35280*la - 
        5595*m^2)*M^2)*rp[t]^15 + 
    (J^4*(5760*En^8 - 480*En^6*(-46 - 9*la + 12*m^2) + 
        5*(8*(723 - 416*la + 14*la^2) - 28*(-41 + la)*m^2 + m^4) - 
        10*En^2*(6084 - 4203*la + 84*la^2 - 4*(-541 + 9*la)*m^2 + 2*m^4) + 
        4*En^4*(1116 - 7075*la + 75*la^2 - 50*(-109 + la)*m^2 + 4*m^4)) + 
      J^2*(589493 + 445464*En^4 - 90240*la + 640*la^2 + (14955 - 80*la)*m^2 - 
        30*En^2*(39018 - 3311*la + 802*m^2))*M^2 + 107829*M^4)*rp[t]^16 + 
    2*M*(J^2*(-69201 + 4644*En^6 + 18900*la - 320*la^2 + 
        20*(-165 + 2*la)*m^2 - 6*En^4*(22333 - 3330*la + 1250*m^2) + 
        10*En^2*(19795 - 4133*la + 28*la^2 + (1067 - 6*la)*m^2)) + 
      9*(-9347 + 13202*En^2 + 520*la)*M^2)*rp[t]^17 + 
    2*(-10*(-1 + En^2)*J^2*(570 + 768*En^6 - 258*la + 8*la^2 - 
        (-48 + la)*m^2 + 72*En^4*(2 - 3*la + 2*m^2) + 
        2*En^2*(-729 + 290*la - 3*la^2 + (-93 + la)*m^2)) + 
      (47879 + 88614*En^4 - 6020*la + 40*la^2 + 3*En^2*(-45346 + 2705*la))*
       M^2)*rp[t]^18 + 4*(-1 + En^2)*(5844 + 13554*En^4 - 1270*la + 20*la^2 + 
      3*En^2*(-6409 + 720*la))*M*rp[t]^19 + 20*(-1 + En^2)^2*
     (102 + 288*En^4 - 35*la + la^2 + 24*En^2*(-16 + 3*la))*rp[t]^20))/
  (En*la*(1 + la)*rp[t]^14*(J^2 + rp[t]^2)^8) + 
 (((16*I)*J^3*m*mu*Pi*XPhiPhiBar*(-2*M + rp[t])^2*
     (7077*J^12*M^3 + J^12*(-7795 + 412*la - 168*m^2)*M^2*rp[t] + 
      2*J^10*M*(J^2*(1358 - 181*la + 74*m^2) + 24309*M^2)*rp[t]^2 - 
      2*J^10*(J^2*(146 - 39*la + 16*m^2) - 6*(-4494 + 521*En^2 + 220*la - 
          80*m^2)*M^2)*rp[t]^3 + 
      J^8*M*(-4*J^2*(-4738 + 585*la - 215*m^2 + 
          En^2*(961 - 98*la + 52*m^2)) + 141795*M^2)*rp[t]^4 + 
      J^8*(2*J^2*(6*En^2*(42 - 13*la + 7*m^2) - 5*(206 - 51*la + 19*m^2)) + 
        3*(-52925 + 13432*En^2 + 2340*la - 720*m^2)*M^2)*rp[t]^5 + 
      J^6*M*(J^2*(56435 + 828*En^4 - 6270*la + 1960*m^2 - 
          4*En^2*(6325 - 592*la + 288*m^2)) + 225900*M^2)*rp[t]^6 + 
      4*J^6*(J^2*(-4*En^4*(7 - 3*la + 2*m^2) - 5*(311 - 69*la + 22*m^2) + 
          2*En^2*(427 - 123*la + 62*m^2)) + (-64190 + 27066*En^2 + 2480*la - 
          600*m^2)*M^2)*rp[t]^7 + 
      J^4*M*(4*J^2*(1404*En^4 + 5*(4653 - 446*la + 110*m^2) - 
          2*En^2*(8635 - 694*la + 276*m^2)) + 206955*M^2)*rp[t]^8 + 
      J^4*(-4*J^2*(32*En^4*(7 - 3*la + 2*m^2) + 5*(527 - 99*la + 25*m^2) - 
          6*En^2*(391 - 99*la + 41*m^2)) + 3*(-79995 + 54112*En^2 + 2620*la - 
          440*m^2)*M^2)*rp[t]^9 + 2*J^2*M*
       (J^2*(5820*En^4 + 5*(8935 - 711*la + 122*m^2) - 
          4*En^2*(13835 - 796*la + 224*m^2)) + 51381*M^2)*rp[t]^10 + 
      2*J^2*(J^2*(-5260 + 795*la - 140*m^2 - 16*En^4*(8 - 27*la + 13*m^2) + 
          24*En^2*(352 - 58*la + 17*m^2)) + 
        6*(10817*En^2 - 4*(2540 - 69*la + 6*m^2))*M^2)*rp[t]^11 + 
      M*(4*J^2*(11699 + 7476*En^4 - 753*la + 67*m^2 + 
          En^2*(-24025 + 898*la - 132*m^2)) + 21453*M^2)*rp[t]^12 + 
      (-2*J^2*(2876 + 1440*En^6 - 339*la + 31*m^2 + 
          48*En^4*(87 - 8*la + 2*m^2) - 2*En^2*(4196 - 399*la + 61*m^2)) + 
        (-26053 + 41688*En^2 + 580*la)*M^2)*rp[t]^13 + 
      (10307 + 23052*En^4 - 530*la + 4*En^2*(-8261 + 200*la))*M*rp[t]^14 + 
      120*(-1 + En^2)*(11 + 32*En^4 + 2*En^2*(-21 + la) - la)*rp[t]^15))/
    (la*(1 + la)*rp[t]^11*(J^2 + rp[t]^2)^7) - 
   (16*J*mu*Pi*XPhiBar*(-2*M + rp[t])^2*(3801*J^12*M^3 + 
      J^12*(-4061 + 152*la)*M^2*rp[t] + 4*J^10*M*(11*J^2*(31 - 3*la) + 
        5877*M^2)*rp[t]^2 + J^10*(28*J^2*(-5 + la) + 
        (-25183 + 2886*En^2 + 928*la)*M^2)*rp[t]^3 + 
      J^8*M*(J^2*(8487 - 808*la + 2*En^2*(-849 + 46*la)) + 60801*M^2)*
       rp[t]^4 + J^8*(J^2*(-875 + 172*la - 6*En^2*(-35 + 6*la)) + 
        2*(-32674 + 8241*En^2 + 1180*la)*M^2)*rp[t]^5 + 
      2*J^6*M*(J^2*(11058 + 162*En^4 - 1030*la + En^2*(-4933 + 258*la)) + 
        42132*M^2)*rp[t]^6 + J^6*(J^2*(-2293 + En^2*(1250 - 208*la) + 
          8*En^4*(-5 + la) + 440*la) + (-90962 + 38364*En^2 + 3200*la)*M^2)*
       rp[t]^7 + J^4*M*(4*J^2*(7741 + 486*En^4 - 700*la + 
          En^2*(-5846 + 286*la)) + 66051*M^2)*rp[t]^8 + 
      J^4*(J^2*(-3237 + En^2*(3026 - 472*la) + 56*En^4*(-5 + la) + 600*la) + 
        (-71713 + 46500*En^2 + 2440*la)*M^2)*rp[t]^9 + 
      2*J^2*M*(J^2*(12303 + 2016*En^4 - 1070*la + 4*En^2*(-3672 + 157*la)) + 
        13890*M^2)*rp[t]^10 + 
      J^2*(J^2*(-2603 + 460*la + 24*En^4*(-17 + 5*la) - 
          6*En^2*(-673 + 88*la)) + (-30383 + 29406*En^2 + 992*la)*M^2)*
       rp[t]^11 + M*(J^2*(10533 + 5928*En^4 - 872*la + 
          2*En^2*(-9787 + 342*la)) + 4899*M^2)*rp[t]^12 + 
      2*(-2*(-1 + En^2)*J^2*(-283 + 120*En^4 + En^2*(454 - 26*la) + 47*la) + 
        3*(-901 + 1279*En^2 + 28*la)*M^2)*rp[t]^13 + 
      2*(-1 + En^2)*(-949 + 1758*En^2 + 74*la)*M*rp[t]^14 + 
      16*(-1 + En^2)^2*(-13 + 30*En^2 + 2*la)*rp[t]^15))/
    ((1 + la)*rp[t]^11*(J^2 + rp[t]^2)^6))*Derivative[1][rp][t]
]


Clear[gSourceCPM6]
gSourceCPM6[syms_Association]:=
Module[{mu,M,CapLa,J,la,XPhiBar,XPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	XPhiBar=XPhiSymbol[syms,Conjugate->True][t];
	XPhiPhiBar=XPhiPhiSymbol[syms,Conjugate->True][t];

	(-16*J*mu*Pi*XPhiBar*(-2*M + rp[t])^2*(1500576*J^20*M^6 + 
    48*J^20*(-69337 + 1458*la)*M^5*rp[t] + 
    32*J^18*M^4*(J^2*(93657 - 4422*la + 25*la^2) + 465696*M^2)*rp[t]^2 + 
    8*J^18*M^3*(J^2*(-174375 + 14053*la - 182*la^2) + 
      18*(-229398 + 20089*En^2 + 4824*la)*M^2)*rp[t]^3 + 
    J^16*M^2*(8*J^4*(43920 - 5465*la + 123*la^2) + 
      J^2*(9*En^2*(-551287 + 10904*la) + 16*(1857909 - 87726*la + 496*la^2))*
       M^2 + 66594528*M^4)*rp[t]^4 + 
    J^16*M*(-4*J^4*(11230 - 2071*la + 73*la^2) + 
      J^2*(En^2*(3262911 - 148776*la + 352*la^2) - 
        32*(432048 - 34821*la + 451*la^2))*M^2 + 
      108*(-1366036 + 251851*En^2 + 28728*la)*M^4)*rp[t]^5 + 
    J^14*(32*J^6*(70 - 19*la + la^2) + 
      2*J^4*(-3*En^2*(169393 - 13679*la + 72*la^2) + 
        4*(434863 - 54113*la + 1218*la^2))*M^2 + 
      9*J^2*(164940*En^4 + En^2*(-5192495 + 101928*la) + 
        48*(307113 - 14502*la + 82*la^2))*M^4 + 176343552*M^6)*rp[t]^6 + 
    2*J^14*M*(J^4*(En^2*(73627 - 9695*la + 84*la^2) - 
        4*(55530 - 10241*la + 361*la^2)) + 3*J^2*(En^4*(-300340 + 5363*la) + 
        24*En^2*(213820 - 9679*la + 22*la^2) - 
        12*(856317 - 69019*la + 894*la^2))*M^2 + 
      36*(1585715*En^2 + 176*(-30811 + 648*la))*M^4)*rp[t]^7 + 
    J^12*(-4*J^6*(5*En^2*(-77 + la) - 79*(-14 + la))*(-5 + la) + 
      J^4*(En^4*(760414 - 32072*la - 312*la^2) - 
        9*En^2*(1067731 - 85638*la + 432*la^2) + 
        72*(215263 - 26788*la + 603*la^2))*M^2 + 
      24*J^2*(549162*En^4 + 3*En^2*(-2730131 + 53100*la) + 
        8*(1827447 - 86298*la + 488*la^2))*M^4 + 306427968*M^6)*rp[t]^8 + 
    J^12*M*(J^4*(2*En^4*(-64181 + 5044*la + 150*la^2) + 
        9*En^2*(155005 - 20284*la + 168*la^2) - 
        36*(54910 - 10127*la + 357*la^2)) + 
      6*J^2*(38006*En^6 + En^4*(-2681564 + 48082*la) - 
        64*(424269 - 34198*la + 443*la^2) + 3*En^2*(7211547 - 323584*la + 
          704*la^2))*M^2 + 1512*(-448444 + 185885*En^2 + 9432*la)*M^4)*
     rp[t]^9 + J^10*(-6*J^6*(30*En^2*(406 - 86*la + la^2) - 
        234*(70 - 19*la + la^2) + En^4*(-1155 + 167*la + 12*la^2)) - 
      J^4*(12*En^6*(13666 + 41*la) + 27*En^2*(1504211 - 119656*la + 
          576*la^2) - 96*(426189 - 53039*la + 1194*la^2) + 
        En^4*(-6829490 + 290338*la + 2640*la^2))*M^2 + 
      48*J^2*(1078443*En^4 + En^2*(-10106777 + 194286*la) + 
        28*(453054 - 21396*la + 121*la^2))*M^4 + 365105664*M^6)*rp[t]^10 + 
    2*J^10*M*(J^4*(En^6*(16758 + 314*la - 88*la^2) + 
        36*En^2*(82105 - 10664*la + 84*la^2) - 
        48*(54290 - 10013*la + 353*la^2) + En^4*(-579715 + 46115*la + 
          1272*la^2)) + 3*J^2*(326358*En^6 + 2*En^4*(-5302202 + 95435*la) - 
        56*(840759 - 67773*la + 878*la^2) + En^2*(53543421 - 2375760*la + 
          4928*la^2))*M^2 + 756*(-533992 + 296151*En^2 + 11232*la)*M^4)*
     rp[t]^11 + J^8*(2*J^6*(En^4*(31425 - 4611*la - 306*la^2) + 
        1848*(70 - 19*la + la^2) - 90*En^2*(1725 - 363*la + 4*la^2) + 
        En^6*(-880 - 32*la + 38*la^2)) + 
      J^4*(5184*En^8 - 12*En^6*(118894 + 17*la) + 
        336*(210926 - 26251*la + 591*la^2) - 9*En^2*(11203051 - 881896*la + 
          4032*la^2) - 2*En^4*(-13605629 + 583255*la + 4872*la^2))*M^2 + 
      18*J^2*(6540552*En^4 + En^2*(-43073167 + 815208*la) + 
        336*(119799 - 5658*la + 32*la^2))*M^4 + 302081472*M^6)*rp[t]^12 + 
    J^8*M*(J^4*(-24*En^8*(64 + 25*la) - 168*(53670 - 9899*la + 349*la^2) - 
        4*En^6*(-73640 - 745*la + 352*la^2) + 3*En^2*(4908807 - 631576*la + 
          4704*la^2) + En^4*(-4655386 + 375236*la + 9408*la^2)) + 
      12*J^2*(615987*En^6 + En^4*(-12159230 + 219293*la) - 
        336*(83298 - 6715*la + 87*la^2) + 3*En^2*(14311529 - 625508*la + 
          1232*la^2))*M^2 + 1008*(-662314 + 475967*En^2 + 13932*la)*M^4)*
     rp[t]^13 + J^6*(-4*J^6*(En^6*(3880 + 80*la - 155*la^2) - 
        1596*(70 - 19*la + la^2) + 4*En^8*(-5 - 4*la + la^2) + 
        15*En^2*(12940 - 2701*la + 28*la^2) + 
        En^4*(-63465 + 9462*la + 567*la^2)) + 
      J^4*(46656*En^8 + 1044*En^6*(-5240 + 17*la) + 
        En^4*(63027002 - 2722834*la - 20496*la^2) + 
        1008*(83503 - 10393*la + 234*la^2) - 9*En^2*(18035417 - 1399740*la + 
          6048*la^2))*M^2 + 6*J^2*(28346460*En^4 + 
        9*En^2*(-15446817 + 286024*la) + 112*(890877 - 42078*la + 238*la^2))*
       M^4 + 171376128*M^6)*rp[t]^14 + 
    2*J^6*M*(-(J^4*(108*En^8*(61 + 22*la) + En^4*(5462479 - 445967*la - 
           9912*la^2) + 504*(10610 - 1957*la + 69*la^2) + 
         18*En^6*(-31468 + 13*la + 132*la^2) - 18*En^2*(661252 - 84011*la + 
           588*la^2))) + 3*J^2*(2684758*En^6 + 
        20*En^4*(-1770547 + 31867*la) - 56*(825201 - 66527*la + 862*la^2) + 
        En^2*(92811321 - 3968736*la + 7392*la^2))*M^2 + 
      108*(-1752368 + 1604883*En^2 + 36864*la)*M^4)*rp[t]^15 + 
    J^4*(-12*J^6*(En^6*(4570 + 13*la - 177*la^2) - 630*(70 - 19*la + la^2) + 
        12*En^8*(-5 - 4*la + la^2) + 15*En^2*(7001 - 1447*la + 14*la^2) + 
        En^4*(-50745 + 7634*la + 399*la^2)) + 
      J^4*(172224*En^8 + 12*En^6*(-1042044 + 6935*la) + 
        336*(206589 - 25714*la + 579*la^2) - 27*En^2*(6531639 - 496172*la + 
          2016*la^2) - 10*En^4*(-9246691 + 403331*la + 2688*la^2))*M^2 + 
      48*J^2*(3370155*En^4 + 2*En^2*(-6314195 + 113277*la) + 
        4*(1766523 - 83442*la + 472*la^2))*M^4 + 63800352*M^6)*rp[t]^16 + 
    (-(J^8*(72*En^8*(99 + 185*la) + 168*(52430 - 9671*la + 341*la^2) + 
         4*En^6*(-723693 + 8425*la + 2200*la^2) - 
         9*En^2*(2890011 - 359840*la + 2352*la^2) - 
         10*En^4*(-1610647 + 135302*la + 2604*la^2))*M) + 
      6*J^6*(3434802*En^6 + En^4*(-34049140 + 598358*la) - 
        64*(408711 - 32952*la + 427*la^2) + En^2*(67902855 - 2809056*la + 
          4928*la^2))*M^3 + 216*J^4*(-651962 + 756445*En^2 + 13716*la)*M^5)*
     rp[t]^17 + (-2*J^8*(-3108*(70 - 19*la + la^2) + 
        240*En^8*(38 - 3*la + la^2) + 90*En^2*(7692 - 1563*la + 14*la^2) - 
        10*En^6*(-10262 + 197*la + 199*la^2) + 
        15*En^4*(-29783 + 4765*la + 210*la^2)) + 
      J^6*(798336*En^8 + 12*En^6*(-1302306 + 14045*la) + 
        En^4*(89793014 - 3869302*la - 22512*la^2) + 
        96*(408841 - 50891*la + 1146*la^2) - 9*En^2*(14451291 - 1060408*la + 
          4032*la^2))*M^2 + 216*J^4*(466958*En^4 + 
        En^2*(-1333687 + 22860*la) + 12*(48647 - 2298*la + 13*la^2))*M^4 + 
      14074368*J^2*M^6)*rp[t]^18 + 
    2*(-(J^6*(7200*En^10 + 24*En^8*(11809 + 430*la) + 
         En^4*(7849321 - 667169*la - 10920*la^2) + 
         48*(51810 - 9557*la + 337*la^2) - 12*En^2*(806945 - 96914*la + 
           588*la^2) + 2*En^6*(-826387 + 23125*la + 2420*la^2))*M) + 
      3*J^4*(2902194*En^6 + En^4*(-21703644 + 353402*la) + 
        3*En^2*(10866317 - 427664*la + 704*la^2) - 
        12*(809643 - 65281*la + 846*la^2))*M^3 + 
      12*J^2*(1905405*En^2 + 28*(-46199 + 972*la))*M^5)*rp[t]^19 + 
    (2*J^6*(480*En^10*(16 + la) + 1752*(70 - 19*la + la^2) - 
        80*En^8*(-850 - 17*la + 5*la^2) - 30*En^2*(17377 - 3403*la + 
          28*la^2) + 10*En^6*(-8680 + 658*la + 221*la^2) - 
        3*En^4*(-141815 + 24337*la + 882*la^2)) + 
      J^4*(57024*En^8 + 36*En^6*(-359970 + 4903*la) + 
        72*(202252 - 25177*la + 567*la^2) - 9*En^2*(7027033 - 487848*la + 
          1728*la^2) - 2*En^4*(-29403215 + 1168909*la + 5880*la^2))*M^2 + 
      J^2*(38296944*En^4 + 9*En^2*(-9062579 + 145752*la) + 
        16*(1736061 - 82014*la + 464*la^2))*M^4 + 1397088*M^6)*rp[t]^20 + 
    (J^4*(139200*En^10 - 216*En^8*(-1262 + 95*la) - 
        36*En^6*(-64978 + 3097*la + 176*la^2) - 
        36*(51190 - 9443*la + 333*la^2) + 9*En^2*(1064055 - 119992*la + 
          672*la^2) + 2*En^4*(-5314895 + 415030*la + 5712*la^2))*M + 
      J^2*(10627500*En^6 + 12*En^4*(-4284762 + 59855*la) + 
        9*En^2*(6243909 - 228680*la + 352*la^2) - 
        32*(400932 - 32329*la + 419*la^2))*M^3 + 
      36*(-85548 + 161227*En^2 + 1800*la)*M^5)*rp[t]^21 + 
    (12*(-1 + En^2)*J^4*(80*En^8*(-88 + la) + 
        En^2*(36420 - 5943*la - 48*la^2) - 108*(70 - 19*la + la^2) - 
        20*En^6*(655 - 22*la + 3*la^2) + En^4*(-13500 + 1918*la + 
          183*la^2)) + J^2*(141120*En^8 + 12*En^6*(-759380 + 7813*la) + 
        En^4*(24510878 - 810070*la - 3504*la^2) - 
        27*En^2*(686047 - 43822*la + 144*la^2) + 
        8*(400167 - 49817*la + 1122*la^2))*M^2 + 
      (6782796*En^4 + 3*En^2*(-3503897 + 51768*la) + 
        16*(172083 - 8130*la + 46*la^2))*M^4)*rp[t]^22 + 
    2*(-((-1 + En^2)*J^2*(108000*En^8 + 36*En^6*(-1701 + 170*la) + 
         En^2*(1238053 - 109663*la - 560*la^2) - 
         4*(50570 - 9329*la + 329*la^2) + 2*En^4*(-582960 + 19175*la + 
           572*la^2))*M) + (-635268 + 1688166*En^6 + 51228*la - 664*la^2 + 
        En^4*(-4798080 + 53283*la) + En^2*(3692937 - 122664*la + 176*la^2))*
       M^3)*rp[t]^23 - 2*(-1 + En^2)*
     (2*(-1 + En^2)*J^2*(240*En^6*(-98 + la) + 
        En^2*(31370 - 3917*la - 97*la^2) - 71*(70 - 19*la + la^2) + 
        12*En^4*(-885 - 38*la + 7*la^2)) + 
      (-375840*En^6 - 6*En^4*(-227744 + 1669*la) + 
        4*(39583 - 4928*la + 111*la^2) + En^2*(-1091222 + 51403*la + 
          228*la^2))*M^2)*rp[t]^24 + 4*(-1 + En^2)^2*
     (-9990 + 15120*En^6 + 1843*la - 65*la^2 - 6*En^4*(16021 + 133*la) + 
      En^2*(80813 - 5243*la - 88*la^2))*M*rp[t]^25 - 
    4*(-1 + En^2)^3*(240*En^4*(22 + la) + 7*(70 - 19*la + la^2) + 
      16*En^2*(-290 + 26*la + la^2))*rp[t]^26))/
  (En^2*(1 + la)*rp[t]^18*(J^2 + rp[t]^2)^9) + 
 ((8*I)*J^3*m*mu*Pi*XPhiPhiBar*(-2*M + rp[t])^2*
   (6211044*J^20*M^6 - 12*J^20*(1165451 - 36936*la + 17267*m^2)*M^5*rp[t] + 
    J^18*M^4*(J^2*(12802655 - 907336*la + 4224*la^2 + (426452 - 3024*la)*
         m^2 + 656*m^4) + 66084696*M^2)*rp[t]^2 + 
    J^18*M^3*(-(J^2*(6067519 - 730712*la + 7776*la^2 + (345441 - 5592*la)*
          m^2 + 1216*m^4)) + 6*(-24824642 + 2301159*En^2 + 765440*la - 
        340234*m^2)*M^2)*rp[t]^3 + 2*J^16*M^2*
     (J^4*(779499 - 144217*la + 2664*la^2 + (68608 - 1926*la)*m^2 + 
        420*m^4) - 3*J^2*(-22748803 + 1569264*la - 7104*la^2 + 
        120*(-5857 + 39*la)*m^2 - 928*m^4 + 8*En^2*(502137 - 20480*la + 
          12656*m^2))*M^2 + 159091002*M^4)*rp[t]^4 + 
    J^16*M*(-2*J^4*(101903 - 27779*la + 804*la^2 - 15*(-887 + 39*la)*m^2 + 
        128*m^4) + J^2*(-64759531 + 7595240*la - 78528*la^2 + 
        (-3430023 + 52056*la)*m^2 - 10368*m^4 + 
        4*En^2*(4039774 - 378800*la + 1680*la^2 + (236743 - 2328*la)*m^2 + 
          766*m^4))*M^2 + 6*(23396727*En^2 - 4*(29918463 - 893892*la + 
          374012*m^2))*M^4)*rp[t]^5 + 
    J^14*(J^6*(4*(2606 - 1038*la + 45*la^2) + (2003 - 132*la)*m^2 + 29*m^4) + 
      J^4*(16659355 - 3003584*la + 53856*la^2 + (1368777 - 35964*la)*m^2 + 
        7200*m^4 - 2*En^2*(2570369 - 425180*la + 4416*la^2 + 
          (268987 - 6252*la)*m^2 + 2076*m^4))*M^2 + 
      J^2*(658879567 + 7629840*En^4 - 44058168*la + 193536*la^2 - 
        32*(-581939 + 3618*la)*m^2 + 20608*m^4 - 
        24*En^2*(10244287 - 405026*la + 240474*m^2))*M^4 + 913810176*M^6)*
     rp[t]^6 + J^14*M*(J^4*(-2180695 + 579746*la - 16272*la^2 + 
        27*(-9883 + 406*la)*m^2 - 2208*m^4 + 2*En^2*(381127 - 102150*la + 
          1896*la^2 + (65462 - 2760*la)*m^2 + 927*m^4)) - 
      4*J^2*(78264317 - 8902116*la + 89208*la^2 + (3804827 - 53784*la)*m^2 + 
        9632*m^4 + 3*En^4*(788557 - 39806*la + 29638*m^2) - 
        En^2*(41357827 - 3764660*la + 15648*la^2 - 8*(-283379 + 2511*la)*
           m^2 + 6082*m^4))*M^2 + 24*(-86064836 + 26751627*En^2 + 
        2479056*la - 963200*m^2)*M^4)*rp[t]^7 + 
    2*J^12*(J^6*(3*(18616 - 7238*la + 304*la^2 + (3368 - 207*la)*m^2 + 
          42*m^4) - En^2*(8*(2555 - 1095*la + 33*la^2) + 
          (5686 - 399*la)*m^2 + 136*m^4)) + 
      J^4*(4*En^4*(510674 - 61733*la + 78*la^2 + (47030 - 554*la)*m^2 + 
          274*m^4) + 3*(13443181 - 2352009*la + 40824*la^2 - 
          20*(-50887 + 1242*la)*m^2 + 4480*m^4) - 
        2*En^2*(13210210 - 2125013*la + 20664*la^2 + (1299583 - 27204*la)*
           m^2 + 8346*m^4))*M^2 + 4*J^2*(237335201 + 9234180*En^4 - 
        15303480*la + 65088*la^2 + (6023948 - 34776*la)*m^2 + 5432*m^4 - 
        6*En^2*(23516133 - 895748*la + 506131*m^2))*M^4 + 867850452*M^6)*
     rp[t]^8 + J^12*M*(-(J^4*(10576133 - 2730600*la + 74088*la^2 + 
         (1197893 - 45576*la)*m^2 + 8288*m^4 + 4*En^4*(176523 - 40055*la + 
           60*la^2 + (31243 - 910*la)*m^2 + 470*m^4) - 
         2*En^2*(3935053 - 1027882*la + 17856*la^2 + (639202 - 24276*la)*
            m^2 + 7569*m^4))) + 4*J^2*(-225959237 + 312426*En^6 + 
        24791928*la - 240192*la^2 + 7*(-1414427 + 18504*la)*m^2 - 20384*m^4 - 
        6*En^4*(3845573 - 188788*la + 135687*m^2) + 
        2*En^2*(95342397 - 8376044*la + 32352*la^2 - 6*(-803035 + 6286*la)*
           m^2 + 10203*m^4))*M^2 + 24*(72441027*En^2 - 
        7*(23403945 - 645768*la + 228629*m^2))*M^4)*rp[t]^9 + 
    J^10*(J^6*(542734 - 205128*la + 8316*la^2 + (91381 - 5184*la)*m^2 + 
        952*m^4 + 4*En^4*(9744 - 3992*la - 9*la^2 + (3172 - 178*la)*m^2 + 
          98*m^4) - 4*En^2*(106040 - 44415*la + 1254*la^2 + 
          (28106 - 1779*la)*m^2 + 566*m^4)) - 
      2*J^4*(48*En^6*(9598 - 493*la + 562*m^2) - 
        12*En^4*(1673677 - 197502*la + 216*la^2 - 3*(-48569 + 506*la)*m^2 + 
          708*m^4) - 3*(38891783 - 6566848*la + 110016*la^2 - 
          126*(-21153 + 476*la)*m^2 + 9520*m^4) + 
        2*En^2*(61197779 - 9521644*la + 85824*la^2 + (5586143 - 102900*la)*
           m^2 + 28278*m^4))*M^2 + 6*J^2*(53234304*En^4 - 
        8*En^2*(63965384 - 2326372*la + 1232865*m^2) + 
        7*(86255451 - 5327016*la + 21888*la^2 - 4*(-479213 + 2556*la)*m^2 + 
          1360*m^4))*M^4 + 2281500144*M^6)*rp[t]^10 + 
    J^10*M*(J^4*(-30665759 + 7646304*la - 199872*la^2 + 
        7*(-451499 + 15768*la)*m^2 - 17696*m^4 + 
        8*En^6*(24011 - 2979*la - 153*la^2 + (3759 + 10*la)*m^2 + 44*m^4) - 
        4*En^4*(1750325 - 389692*la + 528*la^2 + (295545 - 7722*la)*m^2 + 
          3780*m^4) + 2*En^2*(18328010 - 4642892*la + 74592*la^2 + 
          (2783318 - 92652*la)*m^2 + 25947*m^4)) + 
      2*J^2*(5901012*En^6 - 12*En^4*(16775547 - 797416*la + 549243*m^2) - 
        21*(41165713 - 4325896*la + 40416*la^2 + (1585095 - 19080*la)*m^2 + 
          2560*m^4) + 4*En^2*(260622370 - 21910828*la + 77952*la^2 + 
          (11882068 - 80556*la)*m^2 + 18805*m^4))*M^2 + 
      12*(257106729*En^2 - 14*(30854895 - 808944*la + 254023*m^2))*M^4)*
     rp[t]^11 + 2*J^8*(-(J^6*(-788783 + 288198*la - 11232*la^2 + 
         175*(-695 + 36*la)*m^2 - 1022*m^4 + 8*En^6*(634 - 169*la - 36*la^2 + 
           (222 + 7*la)*m^2 + 8*m^4) - 12*En^4*(16228 - 6569*la - 10*la^2 - 
           6*(-851 + 44*la)*m^2 + 138*m^4) + 6*En^2*(165645 - 67504*la + 
           1760*la^2 + (41416 - 2289*la)*m^2 + 656*m^4))) + 
      J^4*(15048*En^8 - 12*En^6*(368253 - 19156*la + 20552*m^2) + 
        21*(10657679 - 1723810*la + 27792*la^2 + (644692 - 13284*la)*m^2 + 
          1800*m^4) + 8*En^4*(2*(5531041 - 634586*la + 588*la^2) + 
          (900108 - 7845*la)*m^2 + 3255*m^4) - 
        2*En^2*(168217630 - 25119944*la + 207648*la^2 + 
          (13979011 - 221172*la)*m^2 + 52530*m^4))*M^2 + 
      2*J^2*(202141368*En^4 - 12*En^2*(114129645 - 3907772*la + 
          1894735*m^2) + 7*(171134201 - 10034496*la + 39744*la^2 - 
          8*(-401683 + 1971*la)*m^2 + 1712*m^4))*M^4 + 1052477748*M^6)*
     rp[t]^12 + J^8*M*(-(J^4*(48*En^8*(181 + 57*la + 8*m^2) - 
         8*En^6*(233602 - 30109*la - 1311*la^2 + (35554 + 44*la)*m^2 + 
           380*m^4) + 21*(2809609 - 671372*la + 16848*la^2 + 
           (257021 - 8172*la)*m^2 + 1120*m^4) + 
         4*En^4*(4*(1948754 - 423907*la + 504*la^2) - 3*(-412989 + 9074*la)*
            m^2 + 11850*m^4) - 2*En^2*(2*(25353245 - 6187154*la + 
             90720*la^2) + (7090378 - 200676*la)*m^2 + 48645*m^4))) + 
      2*J^2*(24813396*En^6 - 12*En^4*(42934653 - 1964996*la + 1291175*m^2) - 
        7*(163944047 - 16344072*la + 146880*la^2 + (5350739 - 58968*la)*m^2 + 
          6464*m^4) + 4*En^2*(467643847 - 37114124*la + 120624*la^2 - 
          5*(-3704849 + 21420*la)*m^2 + 20605*m^4))*M^2 + 
      12*(312264297*En^2 - 28*(14291987 - 352500*la + 94304*m^2))*M^4)*
     rp[t]^13 + J^6*(J^6*(64*En^8*(1 + la)*(7 - 3*la + 2*m^2) - 
        32*En^6*(3114 - 877*la - 156*la^2 + (1094 + 19*la)*m^2 + 40*m^4) + 
        21*(4*(36251 - 12705*la + 474*la^2) + (19989 - 936*la)*m^2 + 
          130*m^4) + 12*En^4*(145638 - 58003*la - 56*la^2 + 
          (43428 - 1922*la)*m^2 + 890*m^4) - 4*En^2*(1386340 - 546519*la + 
          12936*la^2 - 5*(-64742 + 3003*la)*m^2 + 3730*m^4)) + 
      2*J^4*(150480*En^8 - 24*En^6*(784983 - 41474*la + 40280*m^2) + 
        21*(4*(3551399 - 544560*la + 8424*la^2) + (731203 - 13716*la)*m^2 + 
          1520*m^4) + 4*En^4*(57498589 - 6381646*la + 4872*la^2 - 
          5*(-873503 + 5906*la)*m^2 + 10280*m^4) - 
        4*En^2*(151765957 - 21489764*la + 161280*la^2 - 
          5*(-2217377 + 29568*la)*m^2 + 28965*m^4))*M^2 + 
      2*J^2*(657154800*En^4 - 24*En^2*(139738837 - 4399030*la + 
          1873351*m^2) + 7*(318548437 - 17538216*la + 66816*la^2 - 
          800*(-6002 + 27*la)*m^2 + 1792*m^4))*M^4 + 1347885504*M^6)*
     rp[t]^14 + J^6*M*(-(J^4*(96*En^8*(863 + 243*la + 40*m^2) - 
         8*En^6*(976901 - 132895*la - 4653*la^2 + 35*(3875 + 2*la)*m^2 + 
           1080*m^4) + 7*(11285455 - 2555244*la + 61344*la^2 + 
           (882865 - 25380*la)*m^2 + 2848*m^4) + 
         4*En^4*(20804163 - 4370476*la + 4368*la^2 + (3132365 - 52130*la)*
            m^2 + 19000*m^4) - 2*En^2*(91939391 - 21435608*la + 283248*la^2 + 
           (11491850 - 270060*la)*m^2 + 54045*m^4))) + 
      4*J^2*(31286682*En^6 - 30*En^4*(14003484 - 615622*la + 373469*m^2) - 
        7*(76696087 - 7164240*la + 61776*la^2 + (2013181 - 20232*la)*m^2 + 
          1696*m^4) + 2*En^2*(577574397 - 42171040*la + 124320*la^2 + 
          (18596244 - 90804*la)*m^2 + 13443*m^4))*M^2 + 
      24*(-128835168 + 130738383*En^2 + 2954352*la - 631252*m^2)*M^4)*
     rp[t]^15 + 2*J^4*(2*J^6*(160*En^8*(1 + la)*(7 - 3*la + 2*m^2) + 
        7*(146333 - 48591*la + 1728*la^2 + (17366 - 729*la)*m^2 + 83*m^4) - 
        4*En^6*(19550 - 7141*la - 1116*la^2 + (6850 + 85*la)*m^2 + 240*m^4) + 
        4*En^4*(310605 - 115693*la - 63*la^2 + (87275 - 2825*la)*m^2 + 
          1090*m^4) - 2*En^2*(1259585 - 480861*la + 10164*la^2 - 
          5*(-53953 + 2037*la)*m^2 + 2090*m^4)) + 
      J^4*(493272*En^8 - 12*En^6*(4384467 - 217780*la + 210960*m^2) + 
        120*En^4*(3115283 - 341649*la + 210*la^2 + (218809 - 1097*la)*m^2 + 
          297*m^4) + 21*(13372975 - 1916782*la + 28368*la^2 + 
          (554876 - 9432*la)*m^2 + 800*m^4) - 
        2*En^2*(378260932 - 49391450*la + 333648*la^2 + 
          (22674061 - 251916*la)*m^2 + 37998*m^4))*M^2 + 
      12*J^2*(120418183 + 60911700*En^4 - 6142872*la + 22464*la^2 + 
        (1347812 - 5544*la)*m^2 + 312*m^4 + En^2*(-237124902 + 6630136*la - 
          2317874*m^2))*M^4 + 286940538*M^6)*rp[t]^16 + 
    (J^8*(48*En^8*(9857 - 1495*la + 600*m^2) + 
        8*En^6*(3570624 - 381185*la - 8955*la^2 + 20*(20499 + 2*la)*m^2 + 
          1400*m^4) - 7*(10700665 - 2258688*la + 51696*la^2 + 
          (676753 - 17496*la)*m^2 + 1504*m^4) - 
        20*En^4*(2*(3314644 - 725333*la + 588*la^2) + (991437 - 11778*la)*
           m^2 + 3330*m^4) + En^2*(231021494 - 49967600*la + 588672*la^2 + 
          (24047564 - 462840*la)*m^2 + 71334*m^4))*M + 
      4*J^6*(40838958*En^6 - 6*En^4*(78758631 - 3117788*la + 1623309*m^2) - 
        3*(116850799 - 10072232*la + 83136*la^2 + (2277199 - 20808*la)*m^2 + 
          1184*m^4) + 2*En^2*(497671271 - 32098820*la + 85344*la^2 + 
          (11680226 - 47964*la)*m^2 + 4841*m^4))*M^3 + 
      12*J^4*(-110501487 + 146116014*En^2 + 2325192*la - 352495*m^2)*M^5)*
     rp[t]^17 + (J^8*(960*En^8*(-367 + 19*la - 7*la^2 + (-38 + 4*la)*m^2) - 
        320*En^6*(-2*(-4411 + 583*la + 54*la^2) + (1422 + 5*la)*m^2 + 
          16*m^4) + 7*(559778 - 172764*la + 5832*la^2 + (53909 - 2016*la)*
           m^2 + 176*m^4) + 60*En^4*(123386 - 53585*la - 14*la^2 + 
          (39684 - 866*la)*m^2 + 258*m^4) - 12*En^2*(1058840 - 380525*la + 
          7084*la^2 + (193826 - 5859*la)*m^2 + 926*m^4)) + 
      2*J^6*(5650272*En^8 - 48*En^6*(1228347 - 88935*la + 80330*m^2) + 
        3*(61681361 - 8117824*la + 114624*la^2 + (1899350 - 29160*la)*m^2 + 
          1680*m^4) + 4*En^4*(105533009 - 10667638*la + 5208*la^2 + 
          (5920695 - 21774*la)*m^2 + 4044*m^4) - 
        2*En^2*(331971185 - 38055028*la + 229824*la^2 + 
          (14502953 - 133644*la)*m^2 + 13746*m^4))*M^2 + 
      J^4*(1249990483 + 1132058880*En^4 - 58202280*la + 203904*la^2 + 
        (9090244 - 34128*la)*m^2 + 976*m^4 - 48*En^2*(67565728 - 1610788*la + 
          408811*m^2))*M^4 + 146765304*J^2*M^6)*rp[t]^18 + 
    (-(J^6*(276480*En^10 + 960*En^8*(11081 + 53*la + 198*m^2) - 
         8*En^6*(2625161 - 693705*la - 10155*la^2 + (715345 - 10*la)*m^2 + 
           860*m^4) + 3*(16637591 - 3204560*la + 69696*la^2 + 
           (780221 - 18072*la)*m^2 + 1056*m^4) + 
         4*En^4*(36628363 - 7824980*la + 5040*la^2 + (4689963 - 39390*la)*
            m^2 + 7620*m^4) - 2*En^2*(4*(25911707 - 4886213*la + 
             50904*la^2) + (7864562 - 123396*la)*m^2 + 12969*m^4))*M) + 
      J^4*(-612730703 + 169340472*En^6 + 47895192*la - 377568*la^2 + 
        (-7736801 + 64152*la)*m^2 - 1856*m^4 - 
        24*En^4*(63601933 - 1980688*la + 773457*m^2) + 
        8*En^2*(290567042 - 15759124*la + 37632*la^2 + (4179980 - 14436*la)*
           m^2 + 743*m^4))*M^3 + 18*J^2*(-19009710 + 32884341*En^2 + 
        362048*la - 29174*m^2)*M^5)*rp[t]^19 + 
    2*(J^6*(5760*En^10*(22 + la) + 320*En^8*(4576 + 37*la - 18*la^2 + 
          (50 + 8*la)*m^2) - 40*En^6*(5514 - 10207*la - 492*la^2 + 
          (12554 + 13*la)*m^2 + 40*m^4) + 3*(441489 - 123318*la + 3936*la^2 + 
          (31471 - 1044*la)*m^2 + 62*m^4) + 12*En^4*(300004 - 151937*la - 
          14*la^2 - 2*(-50091 + 734*la)*m^2 + 298*m^4) - 
        2*En^2*(2928215 - 910860*la + 14784*la^2 + (391256 - 9429*la)*m^2 + 
          1016*m^4)) + J^4*(-4688136*En^8 - 12*En^6*(4707459 - 344092*la + 
          242728*m^2) + 3*(27296578 - 3231183*la + 43416*la^2 + 
          (542468 - 7506*la)*m^2 + 220*m^4) + 
        8*En^4*(4*(11264287 - 870187*la + 336*la^2) + (1455538 - 3967*la)*
           m^2 + 377*m^4) - 2*En^2*(2*(99968387 - 9459952*la + 50832*la^2) + 
          (5279473 - 40380*la)*m^2 + 2118*m^4))*M^2 + 
      J^2*(163013121 + 276622992*En^4 - 6820016*la + 22848*la^2 - 
        24*(-23663 + 81*la)*m^2 - 24*En^2*(23388514 - 457412*la + 62805*m^2))*
       M^4 + 8557650*M^6)*rp[t]^20 + 
    (J^4*(-22448369 + 2745600*En^10 + 3847374*la - 79272*la^2 + 
        (-675473 + 13986*la)*m^2 - 416*m^4 - 48*En^8*(-243973 - 1785*la + 
          10000*m^2) + 8*En^6*(1158950 - 733879*la - 6813*la^2 + 
          (588034 - 20*la)*m^2 + 204*m^4) - 
        4*En^4*(4*(8484255 - 1321823*la + 672*la^2) + (2397565 - 14482*la)*
           m^2 + 1430*m^4) + 2*En^2*(65099860 - 9870652*la + 90432*la^2 + 
          (2922478 - 37452*la)*m^2 + 2007*m^4))*M + 
      J^2*(-161909691 + 163130616*En^6 + 11269352*la - 84672*la^2 + 
        (-973895 + 7320*la)*m^2 - 24*En^4*(33212887 - 717676*la + 
          154693*m^2) + En^2*(16*(51894195 - 2261014*la + 4836*la^2) + 
          (5206044 - 15168*la)*m^2))*M^3 + 6*(-6721388 + 15206223*En^2 + 
        114320*la)*M^5)*rp[t]^21 + 
    (J^4*(7680*En^10*(-216 + la) + 4*(304354 - 74529*la + 2241*la^2) - 
        20*(-2759 + 81*la)*m^2 + 49*m^4 + 320*En^8*(-8323 - 52*la - 33*la^2 + 
          2*(278 + 5*la)*m^2) - 32*En^6*(-99518 - 30917*la - 828*la^2 + 
          (29006 + 11*la)*m^2 + 24*m^4) - 4*En^2*(1956560 - 469377*la + 
          6600*la^2 + (149138 - 2877*la)*m^2 + 158*m^4) + 
        4*En^4*(1937718 - 645719*la + (322996 - 3274*la)*m^2 + 338*m^4)) + 
      J^2*(43987773 - 1648224*En^8 - 4583360*la + 58464*la^2 + 
        (413249 - 5148*la)*m^2 - 48*En^6*(3151783 - 89238*la + 35056*m^2) + 
        24*En^4*(17217913 - 862038*la + 264*la^2 + (199087 - 410*la)*m^2) + 
        2*En^2*(-148760995 + 10995668*la - 52416*la^2 + (-1669537 + 10644*la)*
           m^2))*M^2 + 3*(12971641 + 41387184*En^4 - 480264*la + 1536*la^2 + 
        8*En^2*(-7413575 + 115554*la))*M^4)*rp[t]^22 + 
    2*(-(J^2*(3078702 + 2457600*En^10 - 457421*la + 8904*la^2 + 
         (43318 - 801*la)*m^2 + 48*En^8*(-95889 - 1429*la + 2684*m^2) + 
         4*En^6*(-5073943 + 406709*la + 2511*la^2 + 3*(-59613 + 2*la)*m^2) + 
         En^4*(44967802 - 4053896*la + 1632*la^2 - 6*(-169029 + 754*la)*
            m^2) + En^2*(-25598420 + 2913218*la - 23400*la^2 + 
           (-470648 + 4956*la)*m^2))*M) + 
      2*(17768430*En^6 + En^4*(-47775393 + 678786*la) - 
        4*(1226659 - 74711*la + 534*la^2) + En^2*(34046139 - 1154188*la + 
          2208*la^2))*M^3)*rp[t]^23 + 
    2*(-3*(-1 + En^2)*J^2*(3200*En^8*(-118 + la) + 
        4*(14316 - 2975*la + 84*la^2) + (1194 - 31*la)*m^2 - 
        64*En^6*(-3073 - 161*la - 13*la^2 + 2*(141 + la)*m^2) - 
        40*En^4*(-15594 + 2241*la + 20*la^2 + (-802 + 3*la)*m^2) + 
        8*En^2*(-62387 + 10287*la - 101*la^2 + 7*(-271 + 4*la)*m^2)) + 
      (2718002 + 9156456*En^8 - 244303*la + 2952*la^2 + 
        12*En^6*(-3365133 + 38108*la) + 4*En^4*(13498744 - 416861*la + 
          102*la^2) - 2*En^2*(12741014 - 710313*la + 3000*la^2))*M^2)*
     rp[t]^24 + 4*(-1 + En^2)*(195016 + 434880*En^8 - 24530*la + 450*la^2 + 
      12*En^6*(-248977 + 1079*la) - 3*En^2*(708599 - 55503*la + 298*la^2) - 
      2*En^4*(-2242978 + 84641*la + 393*la^2))*M*rp[t]^25 - 
    12*(-1 + En^2)^2*(-3750 + 643*la - 17*la^2 + 1280*En^6*(43 + la) + 
      80*En^4*(-1231 + 51*la + la^2) + 16*En^2*(2940 - 313*la + 2*la^2))*
     rp[t]^26))/(En^2*la*(1 + la)*rp[t]^18*(J^2 + rp[t]^2)^10) - 
 (8*J^2*mu*Pi*XPhiPhiBar*(2*M - rp[t])*(7607250*J^22*M^6 - 
    147*J^22*(108277 - 8912*la + 7402*m^2)*M^5*rp[t] + 
    J^20*M^4*(J^2*(4*(3355937 - 618359*la + 7224*la^2) + 
        (2074543 - 36936*la)*m^2 + 11244*m^4) + 86707242*M^2)*rp[t]^2 + 
    J^20*M^3*(-2*J^2*(2898760 - 910429*la + 24408*la^2 - 32*la^3 + 
        (771737 - 31396*la + 48*la^2)*m^2 + (9596 - 24*la)*m^4 + 4*m^6) + 
      3*(-60468713 + 4554000*En^2 + 4888624*la - 3917396*m^2)*M^2)*rp[t]^3 + 
    J^18*M^2*(J^4*(1338000 - 649700*la + 30440*la^2 - 96*la^3 + 
        (557095 - 39434*la + 144*la^2)*m^2 + (12107 - 72*la)*m^4 + 12*m^6) - 
      6*J^2*(-25495523 + 4617062*la - 53328*la^2 + (-3743591 + 64040*la)*
         m^2 - 18290*m^4 + En^2*(3604844 - 385807*la + 407540*m^2))*M^2 + 
      450896094*M^4)*rp[t]^4 + 
    J^18*M*(-(J^4*(153960 - 111496*la + 8276*la^2 - 48*la^3 + 
         (96803 - 10808*la + 72*la^2)*m^2 + (3335 - 36*la)*m^4 + 6*m^6)) + 
      2*J^2*(-33026205 + 10201675*la - 270360*la^2 + 352*la^3 - 
        4*(2094583 - 81868*la + 120*la^2)*m^2 + 4*(-23537 + 54*la)*m^4 - 
        32*m^6 + 2*En^2*(3239707 - 799318*la + 9198*la^2 + 
          (858071 - 21318*la)*m^2 + 9462*m^4))*M^2 + 
      3*(-314456903 + 49843920*En^2 + 24910416*la - 19139538*m^2)*M^4)*
     rp[t]^5 + J^16*(J^6*(-8*(-840 + 910*la - 103*la^2 + la^3) + 
        2*(3203 - 543*la + 6*la^2)*m^2 + (337 - 6*la)*m^4 + m^6) - 
      J^4*(8*(-1904869 + 910468*la - 42161*la^2 + 132*la^3) + 
        (-6066207 + 412556*la - 1440*la^2)*m^2 + 3*(-39833 + 216*la)*m^4 - 
        96*m^6 + 2*En^2*(1805399 - 790659*la + 21518*la^2 + 48*la^3 + 
          (863585 - 50906*la + 48*la^2)*m^2 + (22886 - 84*la)*m^4 + 24*m^6))*
       M^2 + 3*J^2*(265158826 + 1787400*En^4 - 47081164*la + 536928*la^2 + 
        (36688509 - 599928*la)*m^2 + 158752*m^4 + 
        En^2*(-79096358 + 8304460*la - 8522936*m^2))*M^4 + 1413149670*M^6)*
     rp[t]^6 + J^16*M*(J^4*(2*(-876306 + 625307*la - 45866*la^2 + 264*la^3) + 
        (-1057637 + 113468*la - 720*la^2)*m^2 + 9*(-3683 + 36*la)*m^4 - 
        48*m^6 + 2*En^2*(227589 - 163647*la + 8116*la^2 + 48*la^3 + 
          (182001 - 19654*la + 48*la^2)*m^2 + (8968 - 84*la)*m^4 + 24*m^6)) - 
      2*J^2*(171717405 - 52047147*la + 1361544*la^2 - 1760*la^3 + 
        (41189155 - 1538172*la + 2160*la^2)*m^2 + (410848 - 864*la)*m^4 + 
        112*m^6 + 12*En^4*(240254 - 32146*la + 39713*m^2) - 
        2*En^2*(35633447 - 8635511*la + 97140*la^2 + (9027409 - 212478*la)*
           m^2 + 89352*m^4))*M^2 + 3*(-985681907 + 247506288*En^2 + 
        76278000*la - 55733168*m^2)*M^4)*rp[t]^7 + 
    J^14*(-(J^6*(-76440 + 81700*la - 9136*la^2 + 88*la^3 + 
         (-70259 + 11448*la - 120*la^2)*m^2 + (-3377 + 54*la)*m^4 - 8*m^6 + 
         6*En^2*(3290 - 3900*la + 326*la^2 + 4*la^3 + 
           (4414 - 810*la + 4*la^2)*m^2 + (376 - 7*la)*m^4 + 2*m^6))) + 
      2*J^4*(39606863 - 18592674*la + 849628*la^2 - 2640*la^3 + 
        3*(4988492 - 324015*la + 1080*la^2)*m^2 - 6*(-43735 + 216*la)*m^4 + 
        168*m^6 + 2*En^4*(523482 - 168598*la + 855*la^2 + 
          (213989 - 6350*la)*m^2 + 4120*m^4) - 
        En^2*(19912659 - 8578987*la + 228476*la^2 + 480*la^3 + 
          (9148729 - 511962*la + 432*la^2)*m^2 + (218956 - 672*la)*m^4 + 
          168*m^6))*M^2 + 6*J^2*(415611752 + 9453876*En^4 - 72137770*la + 
        811056*la^2 + (53608060 - 832992*la)*m^2 + 200928*m^4 + 
        En^2*(-196912619 + 20221027*la - 20061920*m^2))*M^4 + 2968766676*M^6)*
     rp[t]^8 + J^14*M*(J^4*(-9106347 + 6389266*la - 462332*la^2 + 2640*la^3 - 
        6*(873313 - 89454*la + 540*la^2)*m^2 + 12*(-12223 + 108*la)*m^4 - 
        168*m^6 - 4*En^4*(72585 - 44490*la + 415*la^2 + 36*la^3 + 
          (57925 - 4296*la - 34*la^2)*m^2 + (2938 - 8*la)*m^4 + 8*m^6) + 
        2*En^2*(2517621 - 1784403*la + 86716*la^2 + 480*la^3 + 
          3*(647865 - 66590*la + 144*la^2)*m^2 + (87128 - 672*la)*m^4 + 
          168*m^6)) + 2*J^2*(-538297470 + 248400*En^6 + 159624165*la - 
        4115016*la^2 + 5280*la^3 - 128*(944131 - 33468*la + 45*la^2)*m^2 + 
        224*(-4669 + 9*la)*m^4 - 224*m^6 - 24*En^4*(1279814 - 168700*la + 
          203201*m^2) + 2*En^2*(177950806 - 42239473*la + 461910*la^2 + 
          (42804548 - 942432*la)*m^2 + 369888*m^4))*M^2 + 
      6*(-1035715769 + 367748448*En^2 + 77991312*la - 53582858*m^2)*M^4)*
     rp[t]^9 + J^12*(J^6*(396900 - 417716*la + 46064*la^2 - 440*la^3 + 
        (349726 - 54396*la + 540*la^2)*m^2 + (15079 - 216*la)*m^4 + 28*m^6 + 
        4*En^4*(2940 - 3395*la + 35*la^2 + 18*la^3 + 
          (4480 - 663*la - 17*la^2)*m^2 - 4*(-121 + la)*m^4 + 4*m^6) - 
        6*En^2*(36505 - 42765*la + 3508*la^2 + 40*la^3 + 
          (47582 - 8338*la + 36*la^2)*m^2 + (3721 - 56*la)*m^4 + 14*m^6)) - 
      2*J^4*(36*En^6*(3985 - 511*la + 962*m^2) - 
        6*En^4*(1873190 - 596538*la + 3413*la^2 + (740284 - 21452*la)*m^2 + 
          13208*m^4) - 3*(41380589 - 19024420*la + 856324*la^2 - 2640*la^3 + 
          2*(7351027 - 452732*la + 1440*la^2)*m^2 - 14*(-16009 + 72*la)*m^4 + 
          112*m^6) + 2*En^2*(49884947 - 21086770*la + 546327*la^2 + 
          1080*la^3 + 4*(5466625 - 286656*la + 216*la^2)*m^2 + 
          (459982 - 1176*la)*m^4 + 252*m^6))*M^2 + 
      6*J^2*(45121296*En^4 + 4*(218412692 - 36914217*la + 408456*la^2) - 
        63*(-1643291 + 24104*la)*m^2 + 326788*m^4 + 
        En^2*(-586966596 + 58733864*la - 55952096*m^2))*M^4 + 4395504708*M^6)*
     rp[t]^10 + J^12*M*
     (J^4*(8*En^6*(5451 - 1675*la - 282*la^2 + (3491 + 142*la)*m^2 + 
          112*m^4) - 4*En^4*(785061 - 478132*la + 5567*la^2 + 324*la^3 + 
          (611023 - 44842*la - 272*la^2)*m^2 + (29110 - 56*la)*m^4 + 
          48*m^6) - 3*(9511045 - 6544060*la + 466196*la^2 - 2640*la^3 + 
          8*(646753 - 62746*la + 360*la^2)*m^2 - 252*(-501 + 4*la)*m^4 + 
          112*m^6) + 8*En^2*(3164910 - 2205614*la + 104391*la^2 + 540*la^3 + 
          3*(781469 - 75475*la + 144*la^2)*m^2 + (93208 - 588*la)*m^4 + 
          126*m^6)) + 4*J^2*(-565866240 + 1311768*En^6 + 163538529*la - 
        4146552*la^2 + 5280*la^3 - 7*(16752311 - 559500*la + 720*la^2)*m^2 + 
        28*(-30545 + 54*la)*m^4 - 140*m^6 - 12*En^4*(6158602 - 796326*la + 
          930369*m^2) + 4*En^2*(133074887 - 30831770*la + 325500*la^2 + 
          (30109927 - 609588*la)*m^2 + 218202*m^4))*M^2 + 
      6*(-1534448909 + 725889552*En^2 + 111854064*la - 71109948*m^2)*M^4)*
     rp[t]^11 + 
    J^10*(-(J^6*(6*(-207145 + 214150*la - 23236*la^2 + 220*la^3) + 
         (-1042109 + 153360*la - 1440*la^2)*m^2 + 7*(-5615 + 72*la)*m^4 - 
         56*m^6 + 8*En^6*(180 - 134*la - 79*la^2 + 5*la^3 + 
           (264 + 54*la - 10*la^2)*m^2 + 4*(6 + la)*m^4) - 
         4*En^4*(31980 - 36935*la + 595*la^2 + 162*la^3 - 
           2*(-24065 + 3576*la + 68*la^2)*m^2 + (4964 - 28*la)*m^4 + 
           24*m^6) + 6*En^2*(184265 - 212848*la + 17032*la^2 + 180*la^3 + 
           2*(116051 - 19191*la + 72*la^2)*m^2 + (16299 - 196*la)*m^4 + 
           42*m^6))) + 2*J^4*(1080*En^8 - 72*En^6*(21322 - 2865*la + 
          5087*m^2) + 12*En^4*(4546637 - 1425406*la + 8780*la^2 + 
          (1720864 - 47766*la)*m^2 + 27804*m^4) + 
        3*(87000821 - 39028252*la + 1726616*la^2 - 5280*la^3 + 
          7*(4099125 - 237346*la + 720*la^2)*m^2 - 7*(-52709 + 216*la)*m^4 + 
          140*m^6) - 2*En^2*(149814679 - 61933784*la + 1549128*la^2 + 
          2880*la^3 + 8*(7766938 - 374619*la + 252*la^2)*m^2 + 
          (1101662 - 2352*la)*m^4 + 420*m^6))*M^2 + 
      12*J^2*(64011432*En^4 + En^2*(-581289366 + 56399711*la - 
          51081812*m^2) + 7*(92505727 - 15144354*la + 164592*la^2 + 
          (9865095 - 135624*la)*m^2 + 25286*m^4))*M^4 + 4688682012*M^6)*
     rp[t]^12 + J^10*M*(J^4*(-720*En^8*(1 + la) + 
        8*En^6*(58611 - 19175*la - 2502*la^2 + (37681 + 842*la)*m^2 + 
          1232*m^4) - 4*En^4*(3844368 - 2317874*la + 31144*la^2 + 1296*la^3 + 
          (2897005 - 207510*la - 952*la^2)*m^2 - 42*(-3061 + 4*la)*m^4 + 
          120*m^6) - 3*(19992461 - 13443088*la + 940504*la^2 - 5280*la^3 + 
          56*(181511 - 16516*la + 90*la^2)*m^2 - 42*(-4987 + 36*la)*m^4 + 
          140*m^6) + 4*En^2*(19095969 - 13044407*la + 596328*la^2 + 
          2880*la^3 + 6*(2244805 - 199773*la + 336*la^2)*m^2 + 
          (454756 - 2352*la)*m^4 + 420*m^6)) + 
      4*J^2*(6203880*En^6 - 12*En^4*(17656098 - 2227438*la + 2507557*m^2) + 
        2*En^2*(529244477 - 119180150*la + 1204350*la^2 + 
          (111269149 - 2025870*la)*m^2 + 640350*m^4) - 
        7*(119884635 - 33592113*la + 835848*la^2 - 1056*la^3 + 
          4*(5620903 - 175428*la + 216*la^2)*m^2 - 4*(-33271 + 54*la)*m^4 + 
          16*m^6))*M^2 + 6*(998045808*En^2 - 7*(234106241 - 16403760*la + 
          9423734*m^2))*M^4)*rp[t]^13 + 
    J^8*(-(J^6*(66*(-39565 + 40054*la - 4264*la^2 + 40*la^3) - 
         28*(73687 - 10143*la + 90*la^2)*m^2 + 7*(-9403 + 108*la)*m^4 - 
         70*m^6 + 8*En^6*(1890 - 1439*la - 759*la^2 + 40*la^3 + 
           (2744 + 474*la - 70*la^2)*m^2 + 4*(61 + 6*la)*m^4) - 
         4*En^4*(158100 - 182257*la + 3887*la^2 + 648*la^3 + 
           (235530 - 34976*la - 476*la^2)*m^2 - 84*(-280 + la)*m^4 + 
           60*m^6) + 6*En^2*(558480 - 634251*la + 49088*la^2 + 480*la^3 + 
           2*(337137 - 51625*la + 168*la^2)*m^2 + (40709 - 392*la)*m^4 + 
           70*m^6))) + 2*J^4*(11880*En^8 - 24*En^6*(307304 - 42599*la + 
          72727*m^2) + 4*En^4*(39505569 - 12116158*la + 76944*la^2 + 
          (14071898 - 361850*la)*m^2 + 191980*m^4) + 
        21*(18435807 - 8029248*la + 348232*la^2 - 1056*la^3 + 
          (5540473 - 298740*la + 864*la^2)*m^2 + (57781 - 216*la)*m^4 + 
          16*m^6) - 2*En^2*(299392856 - 120592567*la + 2883342*la^2 + 
          5040*la^3 + (116408281 - 5030466*la + 3024*la^2)*m^2 - 
          20*(-81965 + 147*la)*m^4 + 420*m^6))*M^2 + 
      6*J^2*(238931064*En^4 - 2*En^2*(801880737 - 74902642*la + 
          63375580*m^2) + 7*(197812602 - 31139764*la + 331680*la^2 - 
          3*(-6138781 + 78520*la)*m^2 + 36480*m^4))*M^4 + 3611975724*M^6)*
     rp[t]^14 + J^8*M*(J^4*(-7920*En^8*(1 + la) + 
        8*En^6*(298881 - 102085*la - 10050*la^2 + (205847 + 790*la)*m^2 + 
          7600*m^4) - 21*(4235699 - 2770684*la + 189800*la^2 - 1056*la^3 + 
          2*(989639 - 83508*la + 432*la^2)*m^2 + (33050 - 216*la)*m^4 + 
          16*m^6) - 4*En^4*(11142372 - 6647190*la + 96992*la^2 + 3024*la^3 + 
          (7916147 - 543954*la - 1904*la^2)*m^2 + (308990 - 280*la)*m^4 + 
          160*m^6) + 4*En^2*(38442102 - 25620916*la + 1118292*la^2 + 
          5040*la^3 + 3*(8548961 - 679112*la + 1008*la^2)*m^2 + 
          (688400 - 2940*la)*m^4 + 420*m^6)) + 
      4*J^2*(17187192*En^6 - 24*En^4*(16785774 - 2050192*la + 2238265*m^2) + 
        2*En^2*(732412297 - 159528679*la + 1527708*la^2 + 
          (140226683 - 2240574*la)*m^2 + 597516*m^4) - 
        7*(128308935 - 34595421*la + 842616*la^2 - 1056*la^3 + 
          (21138473 - 611268*la + 720*la^2)*m^2 - 16*(-6032 + 9*la)*m^4 + 
          8*m^6))*M^2 + 6*(-1265159539 + 975382320*En^2 + 84390768*la - 
        42230104*m^2)*M^4)*rp[t]^15 + 
    J^6*(-(J^6*(8*En^6*(17550 - 10017*la - 3317*la^2 + 140*la^3 + 
           (25480 + 1070*la - 210*la^2)*m^2 + 20*(121 + 3*la)*m^4) - 
         7*(12*(46065 - 45503*la + 4736*la^2 - 44*la^3) + 
           (405923 - 51552*la + 432*la^2)*m^2 + (10481 - 108*la)*m^4 + 
           8*m^6) + 6*En^2*(2*(570270 - 629239*la + 46466*la^2 + 420*la^3) + 
           2*(654315 - 89173*la + 252*la^2)*m^2 + (62975 - 490*la)*m^4 + 
           70*m^6) - 4*En^4*(424620 - 524983*la + 13489*la^2 + 1512*la^3 + 
           (623330 - 96792*la - 952*la^2)*m^2 - 20*(-3004 + 7*la)*m^4 + 
           80*m^6))) + 2*J^4*(139608*En^8 - 24*En^6*(760386 - 117223*la + 
          162585*m^2) + 12*En^4*(25918679 - 7571096*la + 47117*la^2 + 
          (8615235 - 187920*la)*m^2 + 85980*m^4) + 
        21*(19745125 - 8285812*la + 351256*la^2 - 1056*la^3 + 
          (5253986 - 261178*la + 720*la^2)*m^2 - 6*(-7027 + 24*la)*m^4 + 
          8*m^6) - 2*En^2*(415534966 - 162951911*la + 3679956*la^2 + 
          6048*la^3 + (149536685 - 5618298*la + 3024*la^2)*m^2 + 
          (1549298 - 2352*la)*m^4 + 252*m^6))*M^2 + 
      12*J^2*(535722494 + 150079500*En^4 - 80246586*la + 835632*la^2 - 
        6*(-6926361 + 81832*la)*m^2 + 59128*m^4 + 
        En^2*(-786843617 + 69704975*la - 53325984*m^2))*M^4 + 1975125690*M^6)*
     rp[t]^16 + J^6*M*(-(J^4*(144*En^8*(4041 + 193*la + 960*m^2) - 
         40*En^6*(24291 - 50207*la - 4710*la^2 + (57865 - 742*la)*m^2 + 
           4064*m^4) + 21*(4536713 - 2866180*la + 191576*la^2 - 1056*la^3 + 
           2*(948361 - 73324*la + 360*la^2)*m^2 - 4*(-6073 + 36*la)*m^4 + 
           8*m^6) + 4*En^4*(2*(11892015 - 6353419*la + 93695*la^2 + 
             2268*la^3) - 5*(-3001303 + 175174*la + 476*la^2)*m^2 + 
           (429710 - 280*la)*m^4 + 120*m^6) - 
         4*En^2*(53503374 - 35023850*la + 1438164*la^2 + 6048*la^3 + 
           3*(11250231 - 767704*la + 1008*la^2)*m^2 + (660724 - 2352*la)*
            m^4 + 252*m^6))) + 4*J^2*(-696573390 + 39053592*En^6 + 
        178685781*la - 4248072*la^2 + 5280*la^3 - 
        64*(1503205 - 39939*la + 45*la^2)*m^2 + 16*(-19654 + 27*la)*m^4 - 
        16*m^6 - 12*En^4*(42297762 - 5177958*la + 5437859*m^2) + 
        2*En^2*(720766136 - 149892293*la + 1345470*la^2 - 
          2*(-60130601 + 824292*la)*m^2 + 346056*m^4))*M^2 + 
      3*(-1388888057 + 1355275968*En^2 + 87044880*la - 35694498*m^2)*M^4)*
     rp[t]^17 + J^4*(J^6*(2880*En^8*(100 - 9*la - la^2 + 2*(22 + la)*m^2) - 
        8*En^6*(-158010 - 12287*la - 8387*la^2 + 280*la^3 - 
          10*(1900 - 13*la + 35*la^2)*m^2 + 40*(193 + 2*la)*m^4) + 
        7*(12*(49270 - 47213*la + 4784*la^2 - 44*la^3) + 
          (394162 - 45504*la + 360*la^2)*m^2 + (7771 - 72*la)*m^4 + 4*m^6) - 
        6*En^2*(1595025 - 1745495*la + 120680*la^2 + 1008*la^3 + 
          6*(296339 - 34125*la + 84*la^2)*m^2 + (61611 - 392*la)*m^4 + 
          42*m^6) + 4*En^4*(1178460 - 1017991*la + 28217*la^2 + 2268*la^3 - 
          10*(-121295 + 16323*la + 119*la^2)*m^2 - 20*(-4373 + 7*la)*m^4 + 
          60*m^6)) - 2*J^4*(3250872*En^8 + 24*En^6*(2626491 - 229940*la + 
          338505*m^2) - 12*En^4*(32761117 - 9825628*la + 56665*la^2 - 
          2*(-5507077 + 92345*la)*m^2 + 67380*m^4) - 
        3*(107422071 - 42906568*la + 1771976*la^2 - 5280*la^3 + 
          2*(12077331 - 548060*la + 1440*la^2)*m^2 + (138190 - 432*la)*m^4 + 
          16*m^6) + 2*En^2*(408950087 - 154936576*la + 3260838*la^2 + 
          5040*la^3 + 8*(16417544 - 521577*la + 252*la^2)*m^2 + 
          (907618 - 1176*la)*m^4 + 84*m^6))*M^2 + 
      3*J^2*(521991072*En^4 + 4*(295310801 - 41476065*la + 421128*la^2) + 
        (70812465 - 767544*la)*m^2 + 63796*m^4 - 
        8*En^2*(275629721 - 22444298*la + 14613600*m^2))*M^4 + 732513906*M^6)*
     rp[t]^18 + (J^8*(276480*En^10 + 48*En^8*(203311 - 5745*la + 15920*m^2) + 
        8*En^6*(4493259 - 491615*la - 34830*la^2 + (694585 - 10870*la)*m^2 + 
          26000*m^4) - 3*(24718879 - 14889112*la + 967144*la^2 - 5280*la^3 + 
          8*(1103697 - 77266*la + 360*la^2)*m^2 + (80172 - 432*la)*m^4 + 
          16*m^6) - 4*En^4*(30511890 - 17088430*la + 234710*la^2 + 
          4536*la^3 + (20662685 - 885246*la - 1904*la^2)*m^2 - 
          6*(-57643 + 28*la)*m^4 + 48*m^6) + 4*En^2*(52287633 - 33808249*la + 
          1284108*la^2 + 5040*la^3 + 6*(5096445 - 288365*la + 336*la^2)*m^2 - 
          8*(-49048 + 147*la)*m^4 + 84*m^6))*M + 
      2*J^6*(-771642720 + 76269456*En^6 + 185195025*la - 4284216*la^2 + 
        5280*la^3 + (-82738933 + 2004084*la - 2160*la^2)*m^2 + 
        4*(-42619 + 54*la)*m^4 - 4*m^6 - 24*En^4*(36153374 - 4482410*la + 
          4285671*m^2) + 8*En^2*(254494619 - 48817172*la + 406140*la^2 + 
          (33663247 - 388968*la)*m^2 + 56862*m^4))*M^3 + 
      3*J^4*(-518288013 + 650179536*En^2 + 30005744*la - 8986836*m^2)*M^5)*
     rp[t]^19 + 
    (-(J^8*(11520*En^10*(22 + la) + 6*(-536725 + 492506*la - 48344*la^2 + 
           440*la^3) + (-1864451 + 192816*la - 1440*la^2)*m^2 + 
         (-25859 + 216*la)*m^4 - 8*m^6 + 960*En^8*(3388 - 33*la + 15*la^2 + 
           (326 - 24*la)*m^2) + 40*En^6*(97842 - 1125*la - 2643*la^2 + 
           70*la^3 - 2*(4408 + 259*la + 35*la^2)*m^2 + 4*(536 + 3*la)*m^4) + 
         6*En^2*(1518505 - 1719120*la + 108808*la^2 + 840*la^3 + 
           2*(840781 - 78029*la + 168*la^2)*m^2 + (37201 - 196*la)*m^4 + 
           14*m^6) - 4*En^4*(1651140 - 1436525*la + 37555*la^2 + 2268*la^3 - 
           2*(-947715 + 85664*la + 476*la^2)*m^2 + (72804 - 84*la)*m^4 + 
           24*m^6))) + J^6*(26387280*En^8 - 48*En^6*(2430314 - 341609*la + 
          568663*m^2) + 8*En^4*(79525245 - 26458262*la + 134232*la^2 + 
          (27656356 - 335518*la)*m^2 + 85772*m^4) + 
        3*(119637278 - 44612220*la + 1788248*la^2 - 5280*la^3 + 
          (21011893 - 862518*la + 2160*la^2)*m^2 + (75337 - 216*la)*m^4 + 
          4*m^6) - 4*En^2*(290343337 - 102377300*la + 1980552*la^2 + 
          2880*la^3 + 4*(18877505 - 496596*la + 216*la^2)*m^2 + 
          (301322 - 336*la)*m^4 + 12*m^6))*M^2 + 
      2*J^4*(666068329 + 498036528*En^4 - 86008474*la + 849072*la^2 + 
        (26973869 - 266616*la)*m^2 + 11454*m^4 - 
        3*En^2*(538628656 - 38274739*la + 18842436*m^2))*M^4 + 
      166330278*J^2*M^6)*rp[t]^20 + 
    (-(J^6*(3409920*En^10 - 48*En^8*(-530797 - 11565*la + 19120*m^2) - 
         8*En^6*(4455417 - 920485*la - 33042*la^2 + (1914371 - 12098*la)*
            m^2 + 15952*m^4) + 3*(13844815 - 7771720*la + 488396*la^2 - 
           2640*la^3 + (3895199 - 244248*la + 1080*la^2)*m^2 + 
           (21997 - 108*la)*m^4 + 2*m^6) + 
         4*En^4*(2*(10476048 - 8070891*la + 95816*la^2 + 1512*la^3) + 
           (18864943 - 548802*la - 952*la^2)*m^2 + (149830 - 56*la)*m^4 + 
           8*m^6) - 4*En^2*(36985095 - 22761233*la + 785832*la^2 + 
           2880*la^3 + 6*(3033169 - 138755*la + 144*la^2)*m^2 - 
           4*(-32959 + 84*la)*m^4 + 12*m^6))*M) + 
      2*J^4*(-292657105 + 33669360*En^6 + 64208023*la - 1440504*la^2 + 
        1760*la^3 - 4*(5306791 - 116380*la + 120*la^2)*m^2 + 
        4*(-5125 + 6*la)*m^4 - 24*En^4*(23367982 - 2549626*la + 
          1944759*m^2) + 2*En^2*(508338643 - 84340150*la + 643350*la^2 + 
          (44363783 - 427182*la)*m^2 + 32478*m^4))*M^3 + 
      3*J^2*(-118786395 + 196323024*En^2 + 6223120*la - 1022778*m^2)*M^5)*
     rp[t]^21 + (J^6*(3840*En^10*(569 + 2*la) - 
        6*(-302295 + 258430*la - 24436*la^2 + 220*la^3) + 
        (837634 - 76590*la + 540*la^2)*m^2 + (7150 - 54*la)*m^4 + m^6 + 
        1920*En^8*(3134 + 96*la - 15*la^2 + (-311 + 18*la)*m^2) - 
        8*En^6*(677250 - 57109*la - 13189*la^2 + 280*la^3 + 
          (200944 - 3826*la - 210*la^2)*m^2 + 4*(1721 + 6*la)*m^4) - 
        6*En^2*(1034990 - 1187533*la + 67232*la^2 + 480*la^3 + 
          2*(524711 - 38051*la + 72*la^2)*m^2 + (12679 - 56*la)*m^4 + 
          2*m^6) + 4*En^4*(645780 - 1470035*la + 32165*la^2 + 1512*la^3 - 
          2*(-997115 + 54756*la + 238*la^2)*m^2 - 4*(-8086 + 7*la)*m^4 + 
          4*m^6)) + J^4*(137630826 - 2241936*En^8 - 46583072*la + 
        1805096*la^2 - 5280*la^3 + (16373695 - 603212*la + 1440*la^2)*m^2 + 
        (27323 - 72*la)*m^4 - 48*En^6*(-64188 - 337465*la + 489981*m^2) + 
        24*En^4*(17013097 - 5228246*la + 22420*la^2 + (4422394 - 38186*la)*
           m^2 + 5084*m^4) + 2*En^2*(-297844873 + 89909783*la - 
          1578006*la^2 - 2160*la^3 + (-51121253 + 1099794*la - 432*la^2)*
           m^2 + (-86834 + 84*la)*m^4))*M^2 + 
      3*J^2*(149788488*En^4 + 2*(51469645 - 5964110*la + 57072*la^2) + 
        (2065249 - 18520*la)*m^2 - 6*En^2*(55921409 - 3248994*la + 
          902788*m^2))*M^4 + 17556750*M^6)*rp[t]^22 + 
    (J^4*(-16154073 + 8755200*En^10 + 8155534*la - 493412*la^2 + 2640*la^3 + 
        (-3082373 + 171548*la - 720*la^2)*m^2 + (-8027 + 36*la)*m^4 - 
        48*En^8*(67487 + 3855*la + 21040*m^2) + 
        24*En^6*(-1027655 - 382205*la - 6514*la^2 + (691567 - 2106*la)*m^2 + 
          1264*m^4) + 4*En^4*(-2*(5926374 - 5072297*la + 49372*la^2 + 
            648*la^3) + (-9755425 + 191110*la + 272*la^2)*m^2 + 
          (-27082 + 8*la)*m^4) + En^2*(78434046 - 40857298*la + 
          1261512*la^2 + 4320*la^3 + 6*(4251355 - 155182*la + 144*la^2)*m^2 - 
          8*(-9598 + 21*la)*m^4))*M + 2*J^2*(-68828505 + 37156176*En^6 + 
        13406863*la - 290664*la^2 + 352*la^3 + 
        (-2464271 + 48652*la - 48*la^2)*m^2 - 
        12*En^4*(23185786 - 1714446*la + 761899*m^2) + 
        6*En^2*(54962453 - 7262465*la + 50300*la^2 + (2171211 - 17330*la)*
           m^2))*M^3 + 9*(-4233437 + 9415952*En^2 + 196112*la)*M^5)*
     rp[t]^23 + 2*(J^4*(9600*En^10*(-229 + 2*la) - 
        4*(-89880 + 68248*la - 6178*la^2 + 55*la^3) + 
        12*(9389 - 751*la + 5*la^2)*m^2 - 3*(-146 + la)*m^4 + 
        960*En^8*(1270 + 54*la - 15*la^2 + (131 + 12*la)*m^2) - 
        2*En^4*(23700 + 1011557*la - 17227*la^2 - 648*la^3 + 
          2*(-574715 + 19548*la + 68*la^2)*m^2 + 4*(-1490 + la)*m^4) + 
        3*En^2*(-2*(285440 - 274613*la + 13619*la^2 + 90*la^3) - 
          4*(96251 - 5385*la + 9*la^2)*m^2 + 7*(-267 + la)*m^4) - 
        4*En^6*(-595410 - 136659*la - 8119*la^2 + 140*la^3 + 
          (345344 - 2246*la - 70*la^2)*m^2 + 4*(421 + la)*m^4)) - 
      J^2*(11915064*En^8 + 2*(-8246186 + 2442989*la - 91126*la^2 + 
          264*la^3) + (-962838 + 31633*la - 72*la^2)*m^2 + 
        24*En^6*(728598 - 185309*la + 156147*m^2) + 
        6*En^4*(-19688476 + 3665982*la - 12961*la^2 + (-1813547 + 11270*la)*
           m^2) + En^2*(102188737 - 23651119*la + 372284*la^2 + 480*la^3 + 
          (7701957 - 134882*la + 48*la^2)*m^2))*M^2 + 
      3*(5591716 + 17187924*En^4 - 565826*la + 5232*la^2 + 
        3*En^2*(-8382425 + 374869*la))*M^4)*rp[t]^24 + 
    2*(-(J^2*(1986702 + 2995200*En^10 - 860549*la + 49862*la^2 - 264*la^3 + 
         2*(92090 - 4517*la + 18*la^2)*m^2 + 72*En^8*(-171997 - 2005*la + 
           7120*m^2) + 12*En^6*(246601 + 244915*la + 2190*la^2 + 
           (-245749 + 430*la)*m^2) + En^4*(19094994 - 7532080*la + 
           58454*la^2 + 648*la^3 + (4237784 - 57348*la - 68*la^2)*m^2) - 
         En^2*(14646879 - 5503177*la + 149884*la^2 + 480*la^3 + 
           (1984131 - 57622*la + 48*la^2)*m^2))*M) + 
      (-7632510 + 25990848*En^6 + 1277409*la - 26664*la^2 + 32*la^3 + 
        144*En^4*(-499148 + 21449*la) + 6*En^2*(8691014 - 850735*la + 
          5306*la^2))*M^3)*rp[t]^25 + 
    4*((-1 + En^2)*J^2*(-45960 + 29038*la - 2500*la^2 + 22*la^3 + 
        960*En^8*(653 + 2*la) - 3*(2290 - 159*la + la^2)*m^2 + 
        240*En^6*(-3484 - 85*la - 15*la^2 + 6*(71 + la)*m^2) + 
        En^2*(326100 - 200264*la + 7298*la^2 + 82*la^3 + 
          3*(28910 - 1189*la + la^2)*m^2) + 2*En^4*(-35910 + 103957*la + 
          1017*la^2 - 40*la^3 + 10*(-9120 + 121*la + la^2)*m^2)) + 
      (939712 + 2889612*En^8 - 234030*la + 8366*la^2 - 24*la^3 + 
        18*En^6*(-738453 + 27977*la) + 3*En^4*(6051768 - 572302*la + 
          1649*la^2) - En^2*(8690654 - 1411914*la + 19747*la^2 + 24*la^3))*
       M^2)*rp[t]^26 + 4*(-1 + En^2)*(117594 + 230400*En^8 - 41509*la + 
      2291*la^2 - 12*la^3 + 36*En^6*(-47801 + 1207*la) - 
      24*En^4*(-110053 + 13477*la + 80*la^2) - 
      3*En^2*(422411 - 98410*la + 1905*la^2 + 12*la^3))*M*rp[t]^27 - 
    8*(-1 + En^2)^2*(-2880 + 1414*la - 115*la^2 + la^3 + 
      1440*En^6*(29 + la) + 360*En^4*(-208 + 31*la + la^2) + 
      En^2*(35910 - 11677*la + 298*la^2 + 5*la^3))*rp[t]^28)*
   Derivative[1][rp][t])/(En*la*(1 + la)*rp[t]^16*(J^2 + rp[t]^2)^11) 
]


Clear[fSourceCPM6]
fSourceCPM6[syms_Association]:=
Module[{mu,M,CapLa,J,la,XPhiBar,XPhiPhiBar,En,USq,fp,m,rp,t},

	mu = ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	m=MSymbol[syms];
	la=LambdaOfL[syms];
	t=TSymbol[syms];
	rp = RpSymbol[syms];
	J=SpecificAngularMomentumSymbol[syms];
	En=SpecificEnergySymbol[syms];
	
	XPhiBar=XPhiSymbol[syms,Conjugate->True][t];
	XPhiPhiBar=XPhiPhiSymbol[syms,Conjugate->True][t];

	(-16*J*mu*Pi*XPhiBar*(2*M - rp[t])^3*(51744*J^18*M^5 + 
    144*J^18*(-683 + 18*la)*M^4*rp[t] + 32*J^16*M^3*
     (J^2*(2271 - 138*la + la^2) + 14553*M^2)*rp[t]^2 + 
    J^16*M^2*(-8*J^2*(3237 - 347*la + 6*la^2) + 
      81*(-10928 + 2215*En^2 + 288*la)*M^2)*rp[t]^3 + 
    2*J^14*M*(4*J^4*(550 - 95*la + 3*la^2) + 
      3*J^2*(En^2*(-42281 + 1198*la) + 48*(2271 - 138*la + la^2))*M^2 + 
      931392*M^4)*rp[t]^4 + J^14*(-4*J^4*(70 - 19*la + la^2) + 
      J^2*(-72*(3237 - 347*la + 6*la^2) + En^2*(127941 - 8770*la + 48*la^2))*
       M^2 + 144*(-24588 + 10429*En^2 + 648*la)*M^4)*rp[t]^5 + 
    2*J^12*M*(-4*J^4*(-9*(550 - 95*la + 3*la^2) + 
        En^2*(3344 - 433*la + 6*la^2)) + 
      3*J^2*(21264*En^4 + En^2*(-354907 + 9944*la) + 
        192*(2271 - 138*la + la^2))*M^2 + 2173248*M^4)*rp[t]^6 + 
    J^12*(6*J^4*(-5 + la)*(84 - 6*la + En^2*(-63 + 2*la)) + 
      J^2*(6*En^4*(-19692 + 739*la) - 288*(3237 - 347*la + 6*la^2) + 
        En^2*(1077555 - 73040*la + 384*la^2))*M^2 + 
      252*(-32784 + 21913*En^2 + 864*la)*M^4)*rp[t]^7 + 
    2*J^10*M*(J^4*(En^2*(-113119 + 14486*la - 192*la^2) + 
        144*(550 - 95*la + 3*la^2) + 2*En^4*(8180 - 817*la + 6*la^2)) + 
      3*J^2*(166092*En^4 + En^2*(-1309177 + 36184*la) + 
        448*(2271 - 138*la + la^2))*M^2 + 3259872*M^4)*rp[t]^8 + 
    J^10*(-3*J^4*(En^2*(-5355 + 1228*la - 32*la^2) + 48*(70 - 19*la + la^2) + 
        2*En^4*(420 - 93*la + 2*la^2)) + 
      J^2*(26196*En^6 + 6*En^4*(-155268 + 5713*la) - 
        672*(3237 - 347*la + 6*la^2) + En^2*(3990105 - 266800*la + 
          1344*la^2))*M^2 + 2016*(-6147 + 5785*En^2 + 162*la)*M^4)*rp[t]^9 + 
    2*J^8*M*(J^4*(12*En^6*(-484 + 31*la) + En^2*(-420839 + 53176*la - 
          672*la^2) + 336*(550 - 95*la + 3*la^2) + 
        4*En^4*(32632 - 3197*la + 21*la^2)) + 
      3*J^2*(562104*En^4 + En^2*(-2775211 + 75368*la) + 
        672*(2271 - 138*la + la^2))*M^2 + 3259872*M^4)*rp[t]^10 + 
    J^8*(2*J^4*(2*En^6*(220 - 49*la + la^2) - 168*(70 - 19*la + la^2) - 
        3*En^4*(3400 - 741*la + 14*la^2) + 3*En^2*(10020 - 2269*la + 
          56*la^2)) + J^2*(197208*En^6 + 6*En^4*(-531212 + 19139*la) - 
        1008*(3237 - 347*la + 6*la^2) + En^2*(8495571 - 558080*la + 
          2688*la^2))*M^2 + 378*(-32784 + 41005*En^2 + 864*la)*M^4)*
     rp[t]^11 + 2*J^6*M*(2*J^4*(162*En^8 + 9*En^6*(-2507 + 154*la) + 
        En^2*(-450398 + 55921*la - 672*la^2) + 252*(550 - 95*la + 3*la^2) + 
        3*En^4*(75500 - 7259*la + 42*la^2)) + 
      3*J^2*(1070100*En^4 + 5*En^2*(-741103 + 19652*la) + 
        672*(2271 - 138*la + la^2))*M^2 + 2173248*M^4)*rp[t]^12 + 
    J^6*(6*J^4*(4*En^6*(295 - 64*la + la^2) - 84*(70 - 19*la + la^2) - 
        2*En^4*(6020 - 1294*la + 21*la^2) + En^2*(21585 - 4808*la + 
          112*la^2)) + J^2*(641484*En^6 + 30*En^4*(-204260 + 7173*la) - 
        1008*(3237 - 347*la + 6*la^2) + 5*En^2*(2281503 - 146180*la + 
          672*la^2))*M^2 + 1008*(-8196 + 13195*En^2 + 216*la)*M^4)*rp[t]^13 + 
    2*(2*J^8*(1458*En^8 + En^6*(-77943 + 4620*la) + 
        252*(550 - 95*la + 3*la^2) + 10*En^4*(43984 - 4157*la + 21*la^2) - 
        10*En^2*(60901 - 7367*la + 84*la^2))*M + 
      3*J^6*(1253760*En^4 + En^2*(-3199481 + 82088*la) + 
        448*(2271 - 138*la + la^2))*M^3 + 931392*J^4*M^5)*rp[t]^14 + 
    (6*J^8*(10*En^6*(474 - 95*la + la^2) - 84*(70 - 19*la + la^2) - 
        10*En^4*(2348 - 508*la + 7*la^2) + 5*En^2*(5889 - 1277*la + 
          28*la^2)) + J^6*(1077456*En^6 + 30*En^4*(-242884 + 8093*la) - 
        672*(3237 - 347*la + 6*la^2) + En^2*(9926721 - 613520*la + 
          2688*la^2))*M^2 + 36*J^4*(-98352 + 200291*En^2 + 2592*la)*M^4)*
     rp[t]^15 + 2*(J^6*(18792*En^8 + 12*En^6*(-21097 + 1390*la) + 
        En^2*(-1070791 + 124442*la - 1344*la^2) + 
        336*(550 - 95*la + 3*la^2) + 10*En^4*(106448 - 9559*la + 42*la^2))*
       M + 3*J^4*(916884*En^4 + En^2*(-1748987 + 42904*la) + 
        192*(2271 - 138*la + la^2))*M^3 + 232848*J^2*M^5)*rp[t]^16 + 
    (J^6*(480*En^8*(-20 + la) + 80*En^6*(442 - 142*la + la^2) - 
        336*(70 - 19*la + la^2) - 30*En^4*(5784 - 1201*la + 14*la^2) + 
        3*En^2*(52515 - 10876*la + 224*la^2)) + 
      J^4*(1144044*En^6 + 6*En^4*(-912092 + 27419*la) - 
        288*(3237 - 347*la + 6*la^2) + En^2*(5485515 - 322240*la + 
          1344*la^2))*M^2 + 144*J^2*(-6147 + 15704*En^2 + 162*la)*M^4)*
     rp[t]^17 + 2*(J^4*(1800*En^8 + 36*En^6*(-7751 + 465*la) + 
        En^2*(-600599 + 65776*la - 672*la^2) + 144*(550 - 95*la + 3*la^2) + 
        12*En^4*(69250 - 5497*la + 21*la^2))*M + 
      3*J^2*(390072*En^4 + En^2*(-554497 + 12824*la) + 
        48*(2271 - 138*la + la^2))*M^3 + 25872*M^5)*rp[t]^18 + 
    (6*(-1 + En^2)*J^4*(960*En^8 + 240*En^6*(18 + la) + 
        En^2*(-13380 + 2443*la - 32*la^2) + 10*En^4*(1072 - 181*la + la^2) + 
        24*(70 - 19*la + la^2)) + J^2*(792792*En^6 + 
        6*En^4*(-403828 + 10313*la) - 72*(3237 - 347*la + 6*la^2) + 
        En^2*(1764345 - 96800*la + 384*la^2))*M^2 + 
      9*(-10928 + 34935*En^2 + 288*la)*M^4)*rp[t]^19 + 
    4*((-1 + En^2)*J^2*(8874*En^6 + 3*En^4*(-35599 + 1462*la) - 
        18*(550 - 95*la + 3*la^2) + En^2*(88667 - 8233*la + 42*la^2))*M + 
      (111186*En^4 + 3*En^2*(-39068 + 839*la) + 8*(2271 - 138*la + la^2))*
       M^3)*rp[t]^20 + 2*(-1 + En^2)*
     (-6*(-1 + En^2)*J^2*(1280*En^6 - 40*En^4*(-46 + 3*la) - 
        2*En^2*(1065 - 164*la + la^2) + 3*(70 - 19*la + la^2)) + 
      (127866*En^4 + 21*En^2*(-5410 + 237*la) + 4*(3237 - 347*la + 6*la^2))*
       M^2)*rp[t]^21 + 4*(-1 + En^2)^2*(16074*En^4 + 
      9*En^2*(-1367 + 104*la) + 2*(550 - 95*la + 3*la^2))*M*rp[t]^22 + 
    4*(-1 + En^2)^3*(70 + 1440*En^4 + 120*En^2*(-8 + la) - 19*la + la^2)*
     rp[t]^23))/(En^2*(1 + la)*rp[t]^17*(J^2 + rp[t]^2)^8) + 
 ((8*I)*J^3*m*mu*Pi*XPhiPhiBar*(2*M - rp[t])^3*
   (196686*J^18*M^5 - 3*J^18*(126833 - 4408*la + 1772*m^2)*M^4*rp[t] + 
    2*J^16*M^3*(2*J^2*(71694 - 5702*la + 24*la^2 + (2299 - 12*la)*m^2 + 
        2*m^4) + 952047*M^2)*rp[t]^2 + 
    J^16*M^2*(-(J^2*(104529 - 14546*la + 144*la^2 + (5885 - 72*la)*m^2 + 
         12*m^4)) + 3*(-1230285 + 262170*En^2 + 41304*la - 15536*m^2)*M^2)*
     rp[t]^3 + 2*J^14*M*(J^4*(9118 - 2026*la + 36*la^2 + (823 - 18*la)*m^2 + 
        3*m^4) - 2*J^2*(-696991 + 53562*la - 216*la^2 + 
        6*(-3377 + 16*la)*m^2 - 14*m^4 + 6*En^2*(47307 - 2324*la + 1196*m^2))*
       M^2 + 4122540*M^4)*rp[t]^4 + 
    J^14*(-(J^4*(2*(599 - 207*la + 6*la^2) + (169 - 6*la)*m^2 + m^4)) + 
      2*J^2*(-509410 + 68517*la - 648*la^2 + (-26090 + 288*la)*m^2 - 42*m^4 + 
        4*En^2*(73313 - 8608*la + 48*la^2 + (4459 - 39*la)*m^2 + 9*m^4))*
       M^2 + 12*(-1335061 + 596520*En^2 + 43056*la - 14924*m^2)*M^4)*
     rp[t]^5 + 2*J^12*M*(J^4*(4*(22285 - 4788*la + 81*la^2) + 
        (7349 - 144*la)*m^2 + 21*m^4 - 2*En^2*(31546 - 6898*la + 96*la^2 + 
          (3601 - 78*la)*m^2 + 18*m^4)) + 
      4*J^2*(76437*En^4 + 4*(379208 - 27993*la + 108*la^2) - 
        7*(-5593 + 24*la)*m^2 + 21*m^4 - 6*En^2*(216315 - 10184*la + 
          4951*m^2))*M^2 + 10491516*M^4)*rp[t]^6 + 
    J^12*(J^4*(-11755 + 3930*la - 108*la^2 + 2*(-761 + 24*la)*m^2 - 7*m^4 + 
        6*En^2*(1539 - 592*la + 16*la^2 - 13*(-24 + la)*m^2 + 3*m^4)) - 
      4*J^2*(1112083 - 143694*la + 1296*la^2 + (50750 - 504*la)*m^2 + 
        63*m^4 + 3*En^4*(48485 - 3604*la + 2126*m^2) - 
        En^2*(1348561 - 151936*la + 768*la^2 + (74666 - 546*la)*m^2 + 
          108*m^4))*M^2 + 12*(2407362*En^2 - 7*(486855 - 14976*la + 
          4684*m^2))*M^4)*rp[t]^7 + 
    2*J^10*M*(J^4*(390629 - 80640*la + 1296*la^2 - 7*(-4117 + 72*la)*m^2 + 
        63*m^4 + 4*En^4*(20808 - 4051*la + 39*la^2 + (2420 - 42*la)*m^2 + 
          12*m^4) - 2*En^2*(292143 - 61400*la + 768*la^2 + 
          (30562 - 546*la)*m^2 + 108*m^4)) + 
      4*J^2*(655569*En^4 - 6*En^2*(877843 - 39232*la + 17708*m^2) + 
        7*(6*(92509 - 6510*la + 24*la^2) + (12361 - 48*la)*m^2 + 5*m^4))*
       M^2 + 17314290*M^4)*rp[t]^8 + 
    J^10*(-3*J^4*(17247 - 5542*la + 144*la^2 - 7*(-287 + 8*la)*m^2 + 7*m^4 + 
        4*En^4*(1112 - 471*la + 13*la^2 + (286 - 14*la)*m^2 + 4*m^4) - 
        2*En^2*(8*(1797 - 666*la + 16*la^2) + (2694 - 91*la)*m^2 + 18*m^4)) + 
      4*J^2*(34506*En^6 - 3*En^4*(420629 - 29884*la + 16836*m^2) - 
        7*(408560 - 50298*la + 432*la^2 + (16135 - 144*la)*m^2 + 15*m^4) + 
        En^2*(5508531 - 590144*la + 2688*la^2 - 6*(-45133 + 273*la)*m^2 + 
          270*m^4))*M^2 + 6*(-11293359 + 11296224*En^2 + 328440*la - 
        90020*m^2)*M^4)*rp[t]^9 + 
    2*J^8*M*(J^4*(-12*En^6*(2659 - 372*la + 248*m^2) + 
        7*(144175 - 28344*la + 432*la^2 + (9239 - 144*la)*m^2 + 15*m^4) + 
        2*En^4*(366265 - 68354*la + 546*la^2 - 72*(-545 + 7*la)*m^2 + 
          120*m^4) - 2*En^2*(1202789 - 240976*la + 2688*la^2 - 
          21*(-5371 + 78*la)*m^2 + 270*m^4)) + 
      4*J^2*(2465757*En^4 - 6*En^2*(2073119 - 86600*la + 35205*m^2) + 
        7*(923896 - 61374*la + 216*la^2 - 30*(-569 + 2*la)*m^2 + 5*m^4))*
       M^2 + 19235538*M^4)*rp[t]^10 + 
    J^8*(J^4*(8*En^6*(634 - 303*la + 9*la^2 - 4*(-52 + 3*la)*m^2 + 4*m^4) - 
        7*(2*(9601 - 2937*la + 72*la^2) + (1952 - 48*la)*m^2 + 5*m^4) - 
        12*En^4*(9966 - 4073*la + 91*la^2 + (2396 - 84*la)*m^2 + 20*m^4) + 
        6*En^2*(59782 - 21200*la + 448*la^2 + (10172 - 273*la)*m^2 + 
          45*m^4)) + 2*J^2*(574488*En^6 - 6*En^4*(1604197 - 108420*la + 
          57890*m^2) - 7*(1367441 - 158634*la + 1296*la^2 - 
          5*(-8975 + 72*la)*m^2 + 30*m^4) + 2*En^2*(13103741 - 1314304*la + 
          5376*la^2 - 130*(-4201 + 21*la)*m^2 + 360*m^4))*M^2 + 
      6*(-12612019 + 16966710*En^2 + 343224*la - 79016*m^2)*M^4)*rp[t]^11 + 
    2*J^6*M*(J^4*(2016*En^8 - 72*En^6*(3811 - 500*la + 320*m^2) + 
        7*(242834 - 44892*la + 648*la^2 - 5*(-2591 + 36*la)*m^2 + 15*m^4) + 
        12*En^4*(237397 - 42397*la + 273*la^2 - 30*(-780 + 7*la)*m^2 + 
          40*m^4) - 2*En^2*(2883587 - 542744*la + 5376*la^2 - 
          70*(-3308 + 39*la)*m^2 + 360*m^4)) + 
      4*J^2*(5271345*En^4 - 60*En^2*(314473 - 11962*la + 4183*m^2) + 
        7*(1038121 - 64338*la + 216*la^2 + (15071 - 48*la)*m^2 + 3*m^4))*
       M^2 + 14394492*M^4)*rp[t]^12 + 
    J^6*(J^4*(-7*(32600 - 9354*la + 216*la^2 + (2765 - 60*la)*m^2 + 5*m^4) + 
        16*En^6*(2822 - 1293*la + 27*la^2 + (860 - 30*la)*m^2 + 8*m^4) - 
        12*En^4*(40040 - 15747*la + 273*la^2 - 30*(-303 + 7*la)*m^2 + 
          40*m^4) + 6*En^2*(144380 - 48464*la + 896*la^2 - 
          5*(-4274 + 91*la)*m^2 + 60*m^4)) + 
      4*J^2*(1068102*En^6 - 15*En^4*(689545 - 43996*la + 21144*m^2) - 
        7*(773948 - 83457*la + 648*la^2 + (19945 - 144*la)*m^2 + 9*m^4) + 
        5*En^2*(4022011 - 366400*la + 1344*la^2 + (131546 - 546*la)*m^2 + 
          54*m^4))*M^2 + 12*(8391720*En^2 - 7*(678615 - 17088*la + 3092*m^2))*
       M^4)*rp[t]^13 + 
    2*(J^8*(20160*En^8 - 12*En^6*(93469 - 11340*la + 7440*m^2) + 
        7*(277414 - 47448*la + 648*la^2 + (11609 - 144*la)*m^2 + 9*m^4) + 
        10*En^4*(610179 - 106034*la + 546*la^2 - 112*(-475 + 3*la)*m^2 + 
          48*m^4) - 10*En^2*(897477 - 153044*la + 1344*la^2 - 
          7*(-8101 + 78*la)*m^2 + 54*m^4))*M + 
      4*J^6*(7241895*En^4 - 6*En^2*(3158161 - 105752*la + 29633*m^2) + 
        7*(12*(65687 - 3749*la + 12*la^2) + (8303 - 24*la)*m^2 + m^4))*M^3 + 
      6996780*J^4*M^5)*rp[t]^14 + 
    (3*J^8*(-7*(12563 - 3314*la + 72*la^2 + (834 - 16*la)*m^2 + m^4) + 
        8*En^6*(10142 - 3755*la + 45*la^2 - 40*(-68 + la)*m^2 + 8*m^4) - 
        20*En^4*(16526 - 6825*la + 91*la^2 - 8*(-453 + 7*la)*m^2 + 8*m^4) + 
        10*En^2*(45695 - 13872*la + 224*la^2 + (5344 - 91*la)*m^2 + 9*m^4)) + 
      4*J^6*(1848168*En^6 - 15*En^4*(968507 - 53452*la + 21258*m^2) - 
        7*(593425 - 58578*la + 432*la^2 + (11060 - 72*la)*m^2 + 3*m^4) + 
        En^2*(20584851 - 1634176*la + 5376*la^2 - 6*(-78613 + 273*la)*m^2 + 
          108*m^4))*M^2 + 12*J^4*(-2327661 + 5337450*En^2 + 53616*la - 
        6764*m^2)*M^4)*rp[t]^15 + 
    2*(J^6*(196416*En^8 - 48*En^6*(33359 - 5700*la + 3320*m^2) + 
        7*(215375 - 33456*la + 432*la^2 + (6487 - 72*la)*m^2 + 3*m^4) + 
        20*En^4*(439550 - 66037*la + 273*la^2 - 18*(-1530 + 7*la)*m^2 + 
          12*m^4) - 2*En^2*(4711021 - 690248*la + 5376*la^2 - 
          42*(-4913 + 39*la)*m^2 + 108*m^4))*M + 
      4*J^4*(2730182 + 6526827*En^4 - 141612*la + 432*la^2 + 
        (18261 - 48*la)*m^2 + m^4 - 6*En^2*(2049965 - 58384*la + 11576*m^2))*
       M^3 + 2003391*J^2*M^5)*rp[t]^16 + 
    (J^6*(-1920*En^8*(74 - 6*la + 7*m^2) - 7*(29755 - 7050*la + 144*la^2 + 
          (1411 - 24*la)*m^2 + m^4) - 60*En^4*(24404 - 8801*la + 91*la^2 + 
          (3898 - 42*la)*m^2 + 4*m^4) + 32*En^6*(2686 - 6315*la + 45*la^2 + 
          (4060 - 30*la)*m^2 + 4*m^4) + 6*En^2*(248764 - 63472*la + 
          896*la^2 + (19802 - 273*la)*m^2 + 18*m^4)) + 
      4*J^4*(2549142*En^6 - 2*(1040254 - 92547*la + 648*la^2) + 
        (-24475 + 144*la)*m^2 - 3*m^4 - 3*En^4*(4591975 - 193524*la + 
          55556*m^2) + En^2*(13717921 - 910016*la + 2688*la^2 + 
          (186266 - 546*la)*m^2 + 18*m^4))*M^2 + 
      3*J^2*(-2690905 + 7941888*En^2 + 56088*la - 3692*m^2)*M^4)*rp[t]^17 + 
    2*(J^4*(767029 - 237312*En^8 - 106200*la + 1296*la^2 + 
        (14459 - 144*la)*m^2 + 3*m^4 - 36*En^6*(68223 - 8260*la + 3560*m^2) + 
        6*En^4*(1501637 - 162914*la + 546*la^2 - 24*(-2045 + 7*la)*m^2 + 
          8*m^4) - 2*En^2*(3254503 - 388480*la + 2688*la^2 - 
          91*(-907 + 6*la)*m^2 + 18*m^4))*M + 
      2*J^2*(1596106 + 6965838*En^4 - 74310*la + 216*la^2 + 
        (5009 - 12*la)*m^2 - 12*En^2*(780853 - 18392*la + 1923*m^2))*M^3 + 
      257175*M^5)*rp[t]^18 + 
    (J^4*(-108256 + 80640*En^10 + 22506*la - 432*la^2 + 
        4*(-793 + 12*la)*m^2 - m^4 - 1920*En^8*(-233 - 18*la + 14*m^2) + 
        6*En^2*(4*(45329 - 9052*la + 112*la^2) + (8044 - 91*la)*m^2 + 
          3*m^4) + 8*En^6*(28906 - 29505*la + 135*la^2 - 
          20*(-692 + 3*la)*m^2 + 4*m^4) - 12*En^4*(144542 - 33531*la + 
          273*la^2 - 12*(-893 + 7*la)*m^2 + 4*m^4)) + 
      J^2*(-2467225 + 10388016*En^6 + 194994*la - 1296*la^2 + 
        (-13505 + 72*la)*m^2 - 12*En^4*(2610639 - 77164*la + 11806*m^2) + 
        4*En^2*(5387071 - 289024*la + 768*la^2 + (31238 - 78*la)*m^2))*M^2 + 
      3*(-349013 + 1312458*En^2 + 6520*la)*M^4)*rp[t]^19 + 
    4*(J^2*(115795 + 108144*En^8 - 14049*la + 162*la^2 + (1004 - 9*la)*m^2 - 
        12*En^6*(142265 - 6876*la + 1584*m^2) + 
        En^4*(2811270 - 198554*la + 546*la^2 + (31840 - 84*la)*m^2) + 
        En^2*(-1331649 + 124616*la - 768*la^2 + 2*(-6998 + 39*la)*m^2))*M + 
      (209619 + 1626534*En^4 - 8666*la + 24*la^2 + 
        6*En^2*(-264513 + 5060*la))*M^3)*rp[t]^20 + 
    2*(-3*(-1 + En^2)*J^2*(-5584 + 44800*En^8 + 998*la - 18*la^2 + 
        (-74 + la)*m^2 + 320*En^6*(82 - 18*la + 7*m^2) + 
        8*En^4*(-17198 + 2143*la - 9*la^2 + 2*(-214 + la)*m^2) - 
        2*En^2*(-36564 + 5381*la - 55*la^2 + 6*(-109 + la)*m^2)) + 
      (-164542 + 2161332*En^6 + 11413*la - 72*la^2 + 
        6*En^4*(-645527 + 13060*la) + 2*En^2*(941091 - 40064*la + 96*la^2))*
       M^2)*rp[t]^21 + 4*(-1 + En^2)*(315936*En^6 + 
      6*En^4*(-87379 + 3060*la) - 2*(7877 - 826*la + 9*la^2) + 
      En^2*(226423 - 15782*la + 78*la^2))*M*rp[t]^22 + 
    12*(-1 + En^2)^2*(-390 + 11200*En^6 + 59*la - la^2 + 
      160*En^4*(-109 + 6*la) + 6*En^2*(1130 - 119*la + la^2))*rp[t]^23))/
  (En^2*la*(1 + la)*rp[t]^17*(J^2 + rp[t]^2)^9) - 
 (8*J^2*mu*Pi*XPhiPhiBar*(-2*M + rp[t])^2*(253575*J^20*M^5 - 
    21*J^20*(21463 - 2052*la + 1516*m^2)*M^4*rp[t] + 
    2*J^18*M^3*(J^2*(154136 - 33839*la + 456*la^2 + (25231 - 456*la)*m^2 + 
        114*m^4) + 1326096*M^2)*rp[t]^2 + 
    J^18*M^2*(-2*J^2*(50136 - 19397*la + 624*la^2 + (14617 - 624*la)*m^2 + 
        156*m^4) + (-4717287 + 859050*En^2 + 441552*la - 311940*m^2)*M^2)*
     rp[t]^3 + J^16*M*(J^4*(15240 - 9544*la + 564*la^2 + 
        (7283 - 564*la)*m^2 + 141*m^4) - 6*J^2*(-538115 + 115738*la - 
        1536*la^2 + (-82739 + 1416*la)*m^2 - 324*m^4 + 
        2*En^2*(92036 - 12445*la + 11384*m^2))*M^2 + 12538701*M^4)*rp[t]^4 + 
    J^16*(-21*J^4*(4*(10 - 10*la + la^2) + (31 - 4*la)*m^2 + m^4) + 
      2*J^2*(-525509 + 199347*la - 6312*la^2 + 36*(-4013 + 162*la)*m^2 - 
        1338*m^4 + 2*En^2*(124223 - 40355*la + 708*la^2 + 
          (37474 - 1098*la)*m^2 + 372*m^4))*M^2 + 
      6*(-3719984 + 1412853*En^2 + 339834*la - 227544*m^2)*M^4)*rp[t]^5 + 
    2*J^14*M*(J^4*(3*(26646 - 16379*la + 952*la^2 + (12063 - 882*la)*m^2 + 
          203*m^4) - En^2*(45027 - 27545*la + 1236*la^2 + 
          (26035 - 1926*la)*m^2 + 654*m^4)) + 
      3*J^2*(82200*En^4 + 9*(283148 - 59477*la + 776*la^2) - 
        96*(-3790 + 61*la)*m^2 + 1204*m^4 - 2*En^2*(911266 - 120191*la + 
          105880*m^2))*M^2 + 17659728*M^4)*rp[t]^6 + 
    J^14*(3*J^4*(-2940 + 2890*la - 284*la^2 + (-2171 + 264*la)*m^2 - 61*m^4 + 
        2*En^2*(875 - 965*la + 88*la^2 + (932 - 138*la)*m^2 + 47*m^4)) - 
      2*J^2*(12*En^4*(16304 - 3359*la + 3529*m^2) + 
        3*(830331 - 307896*la + 9576*la^2 + (212934 - 8064*la)*m^2 + 
          1666*m^4) - 2*En^2*(1234789 - 391843*la + 6636*la^2 + 
          (351644 - 9516*la)*m^2 + 2964*m^4))*M^2 + 
      12*(-5244755 + 3125910*En^2 + 465720*la - 291956*m^2)*M^4)*rp[t]^7 + 
    J^12*M*(J^4*(4*En^4*(22077 - 12032*la + 357*la^2 + (12955 - 742*la)*m^2 + 
          308*m^4) + 3*(252869 - 152124*la + 8676*la^2 + (107382 - 7344*la)*
           m^2 + 1526*m^4) - 4*En^2*(224823 - 134627*la + 5826*la^2 + 
          (123484 - 8436*la)*m^2 + 2649*m^4)) + 
      12*J^2*(3596853 + 388272*En^4 - 734908*la + 9408*la^2 - 
        56*(-8383 + 126*la)*m^2 + 1274*m^4 + En^2*(-4047506 + 518596*la - 
          436568*m^2))*M^2 + 65731302*M^4)*rp[t]^8 + 
    J^12*(-(J^4*(9*(4655 - 4486*la + 432*la^2 + (3247 - 368*la)*m^2 + 
           77*m^4) + 4*En^4*(1260 - 1525*la + 141*la^2 + (1694 - 296*la)*
            m^2 + 124*m^4) - 6*En^2*(8785 - 9515*la + 836*la^2 + 
           (8964 - 1226*la)*m^2 + 389*m^4))) + 
      2*J^2*(31320*En^6 - 12*En^4*(155767 - 31204*la + 31739*m^2) - 
        3*(2346705 - 847244*la + 25824*la^2 + (553126 - 19488*la)*m^2 + 
          3542*m^4) + 2*En^2*(5508803 - 1701146*la + 27600*la^2 - 
          4*(-366379 + 8967*la)*m^2 + 9972*m^4))*M^2 + 
      18*(5438716*En^2 - 7*(930911 - 79916*la + 46092*m^2))*M^4)*rp[t]^9 + 
    2*J^10*M*(J^4*(1073310 - 629556*la + 35136*la^2 - 63*(-6695 + 424*la)*
         m^2 + 4893*m^4 - 12*En^6*(799 - 323*la + 392*m^2) + 
        6*En^4*(6*(11884 - 6319*la + 179*la^2) + (39715 - 2094*la)*m^2 + 
          816*m^4) - 2*En^2*(10*(100833 - 58894*la + 2436*la^2) + 
          (521318 - 32088*la)*m^2 + 9027*m^4)) + 
      6*J^2*(1630416*En^4 - 2*En^2*(5303751 - 656662*la + 522316*m^2) + 
        7*(3*(319683 - 63185*la + 792*la^2) + (111887 - 1560*la)*m^2 + 
          240*m^4))*M^2 + 42300720*M^4)*rp[t]^10 + 
    J^10*(J^4*(40*En^6*(18 - 25*la + 3*la^2 - 8*(-4 + la)*m^2 + 4*m^4) - 
        12*En^4*(4140 - 4925*la + 437*la^2 + (5358 - 872*la)*m^2 + 348*m^4) - 
        3*(39565 - 37254*la + 3504*la^2 + (25767 - 2688*la)*m^2 + 497*m^4) + 
        6*En^2*(39650 - 42033*la + 3520*la^2 + (38452 - 4718*la)*m^2 + 
          1347*m^4)) + 2*J^2*(299592*En^6 - 12*En^4*(662309 - 128232*la + 
          125325*m^2) - 21*(626657 - 219078*la + 6528*la^2 + 
          (132852 - 4320*la)*m^2 + 670*m^4) + 2*En^2*(14509957 - 4340278*la + 
          66864*la^2 + (3557516 - 76860*la)*m^2 + 18420*m^4))*M^2 + 
      6*(27763986*En^2 - 7*(3602017 - 296736*la + 153468*m^2))*M^4)*
     rp[t]^11 + J^8*M*(J^4*(-24*En^6*(8005 - 3161*la + 3752*m^2) + 
        21*(3*(63781 - 36288*la + 1976*la^2) - 72*(-947 + 55*la)*m^2 + 
          620*m^4) + 12*En^4*(307590 - 158446*la + 4186*la^2 + 
          (159805 - 7370*la)*m^2 + 2540*m^4) - 
        4*En^2*(2673426 - 1516934*la + 59304*la^2 - 28*(-46033 + 2475*la)*
           m^2 + 16845*m^4)) + 12*J^2*(3998064*En^4 - 
        2*En^2*(9061173 - 1076027*la + 791780*m^2) + 
        7*(6*(206624 - 39195*la + 480*la^2) - 3*(-41691 + 536*la)*m^2 + 
          202*m^4))*M^2 + 76435002*M^4)*rp[t]^12 + 
    J^8*(J^4*(440*En^6*(18 - 25*la + 3*la^2 - 8*(-4 + la)*m^2 + 4*m^4) - 
        21*(10590 - 9702*la + 888*la^2 + (6325 - 600*la)*m^2 + 95*m^4) - 
        12*En^4*(18000 - 20987*la + 1743*la^2 + (21890 - 3160*la)*m^2 + 
          1120*m^4) + 6*En^2*(106230 - 109651*la + 8624*la^2 + 
          (97460 - 10290*la)*m^2 + 2545*m^4)) + 
      2*J^2*(1238328*En^6 - 12*En^4*(1658511 - 307612*la + 290855*m^2) - 
        21*(811953 - 272586*la + 7920*la^2 - 6*(-24967 + 744*la)*m^2 + 
          566*m^4) + 2*En^2*(24883565 - 7176080*la + 103992*la^2 - 
          80*(-68615 + 1281*la)*m^2 + 20220*m^4))*M^2 + 
      12*(-11427661 + 16091475*En^2 + 894306*la - 398860*m^2)*M^4)*rp[t]^13 + 
    2*J^6*M*(J^4*(-12*En^6*(31463 - 12503*la + 13120*m^2) + 
        21*(124232 - 67974*la + 3600*la^2 + (38859 - 2052*la)*m^2 + 
          263*m^4) - 2*En^2*(4596963 - 2537641*la + 92652*la^2 - 
          35*(-58159 + 2658*la)*m^2 + 18645*m^4) + 
        En^4*(4820880 - 2339276*la + 55356*la^2 + (2328250 - 84580*la)*m^2 + 
          24320*m^4)) + 6*J^2*(6171000*En^4 - 2*En^2*(10560513 - 1182985*la + 
          775492*m^2) + 7*(1127945 - 203047*la + 2424*la^2 + 
          (93596 - 1104*la)*m^2 + 106*m^4))*M^2 + 23991408*M^4)*rp[t]^14 + 
    J^6*(J^4*(-63*(4590 - 4058*la + 360*la^2 + (2437 - 208*la)*m^2 + 
          27*m^4) + 40*En^6*(-90 - 913*la + 135*la^2 + (656 - 320*la)*m^2 + 
          136*m^4) + 6*En^2*(182415 - 186463*la + 13552*la^2 + 
          (159020 - 13930*la)*m^2 + 2845*m^4) - 
        4*En^4*(159840 - 160991*la + 11739*la^2 + (171730 - 18520*la)*m^2 + 
          5480*m^4)) + 2*J^2*(3528936*En^6 - 60*En^4*(511857 - 94318*la + 
          84287*m^2) - 21*(741819 - 236132*la + 6672*la^2 + 
          (113158 - 3072*la)*m^2 + 298*m^4) + 2*En^2*(29133643 - 7972060*la + 
          107688*la^2 + (5482964 - 87108*la)*m^2 + 13212*m^4))*M^2 + 
      36*(4282378*En^2 - 7*(343367 - 25192*la + 9092*m^2))*M^4)*rp[t]^15 + 
    (J^8*(-151488*En^8 - 24*En^6*(140829 - 31925*la + 38480*m^2) + 
        21*(228041 - 118264*la + 6072*la^2 + (59398 - 2832*la)*m^2 + 
          278*m^4) + 60*En^4*(244472 - 123790*la + 2520*la^2 + 
          (119455 - 3182*la)*m^2 + 708*m^4) - 4*En^2*(5387466 - 2858540*la + 
          96348*la^2 - 28*(-74515 + 2841*la)*m^2 + 12267*m^4))*M + 
      12*J^6*(5012571 + 6376560*En^4 - 843324*la + 9792*la^2 - 
        4*(-79045 + 852*la)*m^2 + 222*m^4 - 2*En^2*(8523923 - 872138*la + 
          477060*m^2))*M^3 + 20076291*J^4*M^5)*rp[t]^16 + 
    (J^8*(5760*En^8*(20 - 3*la + 7*m^2) - 21*(12695 - 10650*la + 912*la^2 + 
          (5673 - 432*la)*m^2 + 43*m^4) + 40*En^6*(11730 - 2971*la + 
          285*la^2 + (4448 - 560*la)*m^2 + 184*m^4) - 
        60*En^4*(15240 - 18061*la + 1085*la^2 - 6*(-3249 + 236*la)*m^2 + 
          324*m^4) + 6*En^2*(211125 - 214245*la + 14168*la^2 - 
          2*(-84486 + 5999*la)*m^2 + 1887*m^4)) + 
      2*J^6*(4087944*En^6 - 36*En^4*(874663 - 156948*la + 123483*m^2) - 
        3*(3321251 - 984396*la + 26976*la^2 - 66*(-5849 + 144*la)*m^2 + 
          626*m^4) + 2*En^2*(23803409 - 5946278*la + 74256*la^2 + 
          (3441364 - 46116*la)*m^2 + 4764*m^4))*M^2 + 
      3*J^4*(-12159985 + 27530760*En^2 + 823020*la - 213684*m^2)*M^4)*
     rp[t]^17 + 2*(J^6*(588096*En^8 - 12*En^6*(120141 - 54585*la + 
          68360*m^2) + 3*(6*(85877 - 41282*la + 2048*la^2) + 
          (102561 - 4392*la)*m^2 + 293*m^4) + 
        6*En^4*(2*(593752 - 321767*la + 5467*la^2) + (556585 - 10634*la)*
           m^2 + 1616*m^4) - 2*En^2*(4456158 - 2165752*la + 66696*la^2 - 
          14*(-96001 + 3024*la)*m^2 + 4449*m^4))*M + 
      3*J^4*(4266546 + 9261984*En^4 - 658161*la + 7416*la^2 - 
        3*(-59467 + 584*la)*m^2 + 58*m^4 - 4*En^2*(4661893 - 415438*la + 
          167844*m^2))*M^3 + 2532096*J^2*M^5)*rp[t]^18 + 
    (-(J^6*(80640*En^10 - 1920*En^8*(-334 - 15*la + 7*m^2) + 
         21*(8295 - 6414*la + 528*la^2 + (2843 - 192*la)*m^2 + 13*m^4) - 
         40*En^6*(4698 - 7379*la + 345*la^2 - 8*(-1556 + 65*la)*m^2 + 
           116*m^4) - 6*En^2*(175660 - 165995*la + 9856*la^2 + 
           (112764 - 6426*la)*m^2 + 689*m^4) + 12*En^4*(54720 - 100415*la + 
           4767*la^2 + (99338 - 4792*la)*m^2 + 748*m^4))) + 
      2*J^4*(3024216*En^6 - 9*(476895 - 128573*la + 3408*la^2) + 
        9*(-36667 + 816*la)*m^2 - 246*m^4 - 12*En^4*(1981327 - 296864*la + 
          179119*m^2) + 2*En^2*(13377463 - 2868346*la + 32880*la^2 - 
          4*(-308609 + 3477*la)*m^2 + 732*m^4))*M^2 + 
      9*J^2*(-1033199 + 3005482*En^2 + 63344*la - 8868*m^2)*M^4)*rp[t]^19 + 
    (J^4*(-642816*En^8 - 24*En^6*(9023 - 56603*la + 57352*m^2) + 
        3*(451235 - 195144*la + 9324*la^2 - 9*(-6575 + 252*la)*m^2 + 
          77*m^4) - 4*En^2*(10*(260229 - 106241*la + 2964*la^2) + 
          (494068 - 12828*la)*m^2 + 687*m^4) + 
        4*En^4*(2*(1410951 - 634351*la + 8841*la^2) + (847645 - 11722*la)*
           m^2 + 908*m^4))*M + 2*J^2*(3306263 + 13316256*En^4 - 457550*la + 
        4992*la^2 + (67189 - 600*la)*m^2 - 6*En^2*(3150682 - 231829*la + 
          51552*m^2))*M^3 + 585225*M^5)*rp[t]^20 + 
    2*(J^4*(201600*En^10 - 960*En^8*(-136 - 9*la + 49*m^2) - 
        9*(4340 - 2969*la + 234*la^2 + (972 - 58*la)*m^2 + 2*m^4) + 
        20*En^6*(-17442 - 9841*la + 243*la^2 - 8*(-1576 + 31*la)*m^2 + 
          28*m^4) + 3*En^2*(109100 - 83393*la + 4400*la^2 - 
          22*(-1946 + 89*la)*m^2 + 107*m^4) - 2*En^4*(134820 - 211315*la + 
          7791*la^2 + (161954 - 5336*la)*m^2 + 424*m^4)) + 
      J^2*(-1128282 + 3287016*En^6 + 269363*la - 6888*la^2 + 
        (-41842 + 840*la)*m^2 - 12*En^4*(1049189 - 107332*la + 36365*m^2) + 
        2*En^2*(4719872 - 810893*la + 8484*la^2 - 2*(-96503 + 915*la)*m^2))*
       M^2 + 3*(-181506 + 684141*En^2 + 9890*la)*M^4)*rp[t]^21 + 
    2*(J^2*(182334 - 532800*En^8 - 68537*la + 3144*la^2 + 
        (11392 - 390*la)*m^2 - 12*En^6*(106061 - 31341*la + 17712*m^2) - 
        6*En^4*(-2*(298040 - 79483*la + 903*la^2) + (-59605 + 610*la)*m^2) + 
        En^2*(-1958727 + 611213*la - 15348*la^2 + (-157963 + 3390*la)*m^2))*
       M + 3*(131294 + 1017528*En^4 - 15937*la + 168*la^2 + 
        6*En^2*(-165612 + 9613*la))*M^3)*rp[t]^22 + 
    4*(-3*(-1 + En^2)*J^2*(-1820 + 33600*En^8 + 1051*la - 79*la^2 + 
        10*(-19 + la)*m^2 + 160*En^6*(-170 - 27*la + 35*m^2) + 
        10*En^4*(-2538 + 1681*la - 31*la^2 + 16*(-54 + la)*m^2) + 
        En^2*(21120 - 11237*la + 493*la^2 - 30*(-111 + 4*la)*m^2)) + 
      (-68746 + 895932*En^6 + 14142*la - 348*la^2 + 
        6*En^4*(-268231 + 16947*la) + 3*En^2*(261340 - 34075*la + 324*la^2))*
       M^2)*rp[t]^23 + 12*(-1 + En^2)*(-3822 + 75408*En^6 + 1207*la - 
      53*la^2 + 2*En^4*(-62903 + 7063*la) + 
      En^2*(54619 - 11860*la + 241*la^2))*M*rp[t]^24 + 
    24*(-1 + En^2)^2*(3360*En^6 + 480*En^4*(-11 + 2*la) - 
      4*(30 - 14*la + la^2) + 5*En^2*(414 - 139*la + 5*la^2))*rp[t]^25)*
   Derivative[1][rp][t])/(En*la*(1 + la)*rp[t]^15*(J^2 + rp[t]^2)^10)
]


def@
MetricPerturbationSourceFunction[label_String]:=
Module[{assoc},

	assoc=Association[QttLabel[]->QttSource,
		QtrLabel[]->QtrSource,
		QrrLabel[]->QrrSource,
		QtLabel[]->QtSource,
		QrLabel[]->QrSource,
		QSharpLabel[]->QSharpSource,
		QFlatLabel[]->QFlatSource,
		PtLabel[]->PtSource,
		PrLabel[]->PrSource,
		PLabel[]->PSource];

	assoc[label]
]


End[];

EndPackage[];
