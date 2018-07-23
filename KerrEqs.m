(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`KerrEqs`",
				{"BlackHoleAnalysis`Utils`",
					"BlackHoleAnalysis`Coordinates`",
					"BlackHoleAnalysis`Symbols`",
					"BlackHoleAnalysis`Fields`",
					"BlackHoleAnalysis`Labels`",
					"BlackHoleAnalysis`Harmonics`",
					"BlackHoleAnalysis`AnalyticTools`",
					"BlackHoleAnalysis`ValidityTests`"}];


(*******************************************************************)
(* Public functions *)
(*******************************************************************)

TeukolskyEquation::usage="TeukolskyEquation[] returns the homogeneous Teukolsky equation.";
RadialTeukolskyEquation::usage="RadialTeukolskyEquation[] returns the homogeneous radial Teukolsky equation.";
AngularTeukolskyEquation::usage="AngularTeukolskyEquation[] returns the angular Teukolsky equation.";
RadialTeukolskyEquationPotential::usage="RadialTeukolskyEquationPotential[] returns the potential for the radial Teukolsky equation.";
RemoveTeukolskyFunctionRDerivatives::usage="RemoveTeukolskyFunctionRDerivatives[expr] uses the radial Teukolsky equation to remove \
all r derivatives of the Teukolsky function higher than the first.";
RemoveSlmThetaDerivatives::usage="RemoveSlmThetaDerivatives[expr] uses the angular Teukolsky equation to remove \
all theta derivatives of the angular function Slm higher than the first.";

SasakiNakamuraEquation::usage="SasakiNakamuraEquation[] returns the homogeneous Sasaki-Nakamura equation.";
SasakiNakamuraEquationPotential::usage="SasakiNakamuraEquationPotential[] returns the potential for the Sasaki-Nakamura equation.";
RemoveSasakiNakamuraFunctionRDerivatives::usage="RemoveSasakiNakamuraFunctionRDerivatives[expr] uses the Sasaki-Nakamura equation to remove \
all r derivatives of the Sasaki-Nakamura function higher than the first.";

TeukolskyFunctionAsSasakiNakamuraFunction::usage="TeukolskyFunctionAsSasakiNakamuraFunction[] returns the Teukolsky function written \
as a sum of terms involving the Sasaki-Nakamura function and its first derivative.";
SasakiNakamuraFunctionAsTeukolskyFunction::usage="SasakiNakamuraFunctionAsTeukolskyFunction[] returns the Sasaki-Nakamura function written \
as a sum of terms involving the Teukolsky function and its first derivative.";


Begin["`Private`"];


Options[TeukolskyEquation]={"SpinWeight"->-2};
DocumentationBuilder`OptionDescriptions["TeukolskyEquation"] = {"SpinWeight" -> "Spin of the field (integer from -2 to 2, as well as -1/2 and 1/2)"};

Options[RadialTeukolskyEquationPotential]=Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["RadialTeukolskyEquationPotential"] = DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];

Options[RadialTeukolskyEquation]=Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["RadialTeukolskyEquation"]=DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];
Options[AngularTeukolskyEquation]=Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["AngularTeukolskyEquation"]=DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];
Options[RemoveTeukolskyFunctionRDerivatives]=Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["RemoveTeukolskyFunctionRDerivatives"] = DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];
Options[RemoveSlmThetaDerivatives]=Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["RemoveSlmThetaDerivatives"] = DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];

Options[SasakiNakamuraEquation]=Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["SasakiNakamuraEquation"] = DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];
Options[SasakiNakamuraEquationPotential]=Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["SasakiNakamuraEquationPotential"] = DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];
Options[RemoveSasakiNakamuraFunctionRDerivatives] = Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["RemoveSasakiNakamuraFunctionRDerivatives"] = DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];

Options[SasakiNakamuraFunctionAsTeukolskyFunction] = Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["SasakiNakamuraFunctionAsTeukolskyFunction"] = DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];
Options[TeukolskyFunctionAsSasakiNakamuraFunction]= Options[TeukolskyEquation];
DocumentationBuilder`OptionDescriptions["TeukolskyFunctionAsSasakiNakamuraFunction"] = DocumentationBuilder`OptionDescriptions["TeukolskyEquation"];


Options[FSasakiNakamura] = Options[TeukolskyEquation];
Options[AlphaSasakiNakamura]= Options[TeukolskyEquation];
Options[EtaSasakiNakamura] = Options[TeukolskyEquation];
Options[BetaSasakiNakamura] = Options[TeukolskyEquation];



spinWeightFn=Function[x,MemberQ[Join[Range[-2,2],{1/2,-1/2}],x]];


def@
RadialTeukolskyEquation[syms_Association,opts:OptionsPattern[]]:=
Module[{r,RTeuk,potential,optionsRules,s},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	r=RSymbol[syms];

	RTeuk=RadialTeukolskyFunction[syms,"SpinWeight"->s];
	potential=RadialTeukolskyEquationPotential[syms,"SpinWeight"->s];

	Collect[DeltaKerr[syms]^(-s) D[DeltaKerr[syms]^(s+1) D[RTeuk ,r],r] - potential RTeuk,{RTeuk,Derivative[_][_][r]},Simplify]

]
reDef@
RadialTeukolskyEquation[opts:OptionsPattern[]]:=RadialTeukolskyEquation[DefaultSymbols[],opts]


def@
AngularTeukolskyEquation[syms_Association,opts:OptionsPattern[]]:=
Module[{th,Slm,optionsRules,s,laS,om,m,a},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	
	th=ThetaSymbol[syms];
	om=FrequencySymbol[syms];
	m=MSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	laS=LambdaSpinSymbol[syms,"SpinWeight"->s];
	Slm=SpinWeightedSpheroidalHarmonicFunction[syms,"SpinWeight"->s];
	
	Collect[1/Sin[th] D[Sin[th]D[Slm,th],th]
	+(a^2 om^2 Cos[th]^2-m^2/Sin[th]^2-2a om s Cos[th]-(2m s Cos[th])/Sin[th]^2-s^2 Cot[th]^2+s-a^2 om^2+2a m om + laS)Slm,{_[th]},Simplify]

]
reDef@
AngularTeukolskyEquation[opts:OptionsPattern[]]:=AngularTeukolskyEquation[DefaultSymbols[],opts]


def@
TeukolskyEquation[syms_Association,opts:OptionsPattern[]]:=
Module[{r,optionsRules,sw,t,a,th,ph,delta,M,rho,teukBase,pre,teuk},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	sw=OptionValue["SpinWeight"];

	a=BlackHoleSpinSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	t=TSymbol[syms];
	r=RSymbol[syms];
	th=ThetaSymbol[syms];
	ph=PhiSymbol[syms];
	delta=DeltaKerr[syms];
	teukBase=TeukolskyFunction[syms,"SpinWeight"->sw];
	rho = -1/(r-I a Cos[th]);
	
	pre = Switch[sw,
				-2,rho^-4,
				-1,rho^-2,
				-1/2,rho^-1,
				_,1];
				
	teuk = pre teukBase;

	Collect[pre^-1 (((r^2+a^2)^2/delta-a^2Sin[th]^2)D[teuk,t,t]
	-delta^(-sw) D[delta^(sw+1) D[teuk ,r],r]
	-1/Sin[th] D[Sin[th]D[teuk,th],th]
	+(a^2/delta-1/Sin[th]^2)D[teuk,ph,ph]
	+(4M a r)/delta D[teuk,t,ph]
	-2sw((a(r-M))/delta+(I Cos[th])/Sin[th]^2)D[teuk,ph]
	-2sw((M(r^2-a^2))/delta-r-I a Cos[th])D[teuk,t]
	+(sw^2 Cot[th]^2-sw)teuk),_[t,r,th,ph],Simplify]

]
reDef@
TeukolskyEquation[opts:OptionsPattern[]]:=TeukolskyEquation[DefaultSymbols[],opts]


def@
RadialTeukolskyEquationPotential[syms_Association,opts:OptionsPattern[]]:=
Module[{M,a,r,om,laS,optionsRules,s},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];

	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	r=RSymbol[syms];
	om=FrequencySymbol[syms];
	laS=LambdaSpinSymbol[syms,"SpinWeight"->s];
	
	-(KKerr[syms]^2-2 s I(r-M)KKerr[syms])/DeltaKerr[syms] - 4 s I om r + laS
];
reDef@
RadialTeukolskyEquationPotential[opts:OptionsPattern[]]:=RadialTeukolskyEquationPotential[DefaultSymbols[],opts]


def@
RemoveTeukolskyFunctionRDerivatives[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{RTeuk,r,rule,newLHS,n,s,optionsRules},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];

	r=RSymbol[syms];
	RTeuk=RadialTeukolskyFunctionSymbol[syms,"SpinWeight"->s];
	rule=Solve[RadialTeukolskyEquation[syms,"SpinWeight"->s]==0,D[RTeuk[r],r,r]][[1,1]];
	
	newLHS=rule[[1]]/.Derivative[2][RTeuk][r]->Derivative[n_?(#>=2&)][RTeuk][r];
	Collect[expr//.newLHS:>D[rule[[2]],{r,n-2}],{RTeuk[r],Derivative[_][RTeuk][r]},Simplify]
	
];
reDef@
RemoveTeukolskyFunctionRDerivatives[expr_,opts:OptionsPattern[]]:=RemoveTeukolskyFunctionRDerivatives[DefaultSymbols[],expr,opts];


def@
RemoveSlmThetaDerivatives[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{Slm,th,rule,newLHS,n,s,optionsRules},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	
	th=ThetaSymbol[syms];
	Slm=SpinWeightedSpheroidalHarmonicSymbol[syms,"SpinWeight"->s];
	rule=Solve[AngularTeukolskyEquation[syms,"SpinWeight"->s]==0,D[Slm[th],th,th]][[1,1]];
	
	newLHS=rule[[1]]/.Derivative[2][Slm][th]->Derivative[n_?(#>=2&)][Slm][th];
	Collect[expr//.newLHS:>D[rule[[2]],{th,n-2}],{Slm[th],Derivative[_][Slm][th]},Simplify]
];
reDef@
RemoveSlmThetaDerivatives[expr_,opts:OptionsPattern[]]:=RemoveSlmThetaDerivatives[DefaultSymbols[],expr,opts];


def@
SasakiNakamuraEquation[syms_Association,opts:OptionsPattern[]]:=
Module[{r,XSN,potential,FSN,dRDRStar,optionsRules,s},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	If[s=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Aborting[]
	];

	r=RSymbol[syms];

	XSN=SasakiNakamuraFunction[syms,"SpinWeight"->s];
	potential=SasakiNakamuraEquationPotential[syms,"SpinWeight"->s];
	FSN=FSasakiNakamura[syms,"SpinWeight"->s];
	dRDRStar=Simplify@1/D[RStarOfR[syms,"Metric"->"Kerr"],r];

	Collect[dRDRStar D[ dRDRStar D[XSN,r],r]-FSN dRDRStar D[XSN,r]-potential XSN,{XSN,Derivative[_][_][r]},Simplify]
];
reDef@
SasakiNakamuraEquation[opts:OptionsPattern[]]:=SasakiNakamuraEquation[DefaultSymbols[],opts]


def@
SasakiNakamuraEquationPotential[syms_Association,opts:OptionsPattern[]]:=
Module[{r,eta,delta,laS,a,om,m,M,G,U1,alpha,beta,K,teukPotential,optionsRules,s,F},
	
	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	If[s=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Aborting[]
	];

	r=RSymbol[syms];
	laS = LambdaSpinSymbol[syms,"SpinWeight"->s];
	delta=DeltaKerr[syms];
	a=BlackHoleSpinSymbol[syms];
	om=FrequencySymbol[syms];
	m=MSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	K=KKerr[syms];
	teukPotential=RadialTeukolskyEquationPotential[syms,"SpinWeight"->s];

	eta=EtaSasakiNakamura[syms,"SpinWeight"->s];
	F=FSasakiNakamura[syms,"SpinWeight"->s];
	alpha=AlphaSasakiNakamura[syms,"SpinWeight"->s];
	beta=BetaSasakiNakamura[syms];

	G = -((2(r-M))/(r^2+a^2))+(r delta)/(r^2+a^2)^2;
	
	U1=teukPotential+delta^2/beta (D[2 alpha+D[beta,r]/delta,r]-D[eta,r]/eta (alpha+D[beta,r]/delta));
	
	(delta U1)/(r^2+a^2)^2+G^2+(delta D[G,r])/(r^2+a^2)-F G
];
reDef@
SasakiNakamuraEquationPotential[opts:OptionsPattern[]]:=SasakiNakamuraEquationPotential[DefaultSymbols[],opts]


def@
RemoveSasakiNakamuraFunctionRDerivatives[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{SNSym,r,rule,newLHS,n,optionsRules,s},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	If[s=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Aborting[]
	];

	r=RSymbol[syms];
	SNSym=SasakiNakamuraFunctionSymbol[syms,"SpinWeight"->s];
	rule=Solve[SasakiNakamuraEquation[syms,"SpinWeight"->s]==0,D[SNSym[r],r,r]][[1,1]];
	
	newLHS=rule[[1]]/.Derivative[2][SNSym][r]->Derivative[n_?(#>=2&)][SNSym][r];
	expr//.newLHS:>D[rule[[2]],{r,n-2}]
];
reDef@
RemoveSasakiNakamuraFunctionRDerivatives[expr_,opts:OptionsPattern[]]:=RemoveSasakiNakamuraFunctionRDerivatives[DefaultSymbols[],expr,opts]


def@
AlphaSasakiNakamura[syms_Association,opts:OptionsPattern[]]:=
Module[{beta, delta, K, r, laS,optionsRules,s},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	If[s=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Aborting[]
	];

	r=RSymbol[syms];
	delta=DeltaKerr[syms];
	K=KKerr[syms];
	beta=BetaSasakiNakamura[syms];
	laS = LambdaSpinSymbol[syms,"SpinWeight"->s];

	-I (K beta)/delta^2+3 I D[K,r]+laS+(6 delta)/r^2
];
reDef@
AlphaSasakiNakamura[opts:OptionsPattern[]]:=AlphaSasakiNakamura[DefaultSymbols[],opts]


def@
BetaSasakiNakamura[syms_Association,opts:OptionsPattern[]]:=
Module[{delta, K, r, M,optionsRules,s},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	If[s=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Aborting[]
	];

	r=RSymbol[syms];
	delta=DeltaKerr[syms];
	M=BlackHoleMassSymbol[syms];
	K=KKerr[syms];

	2 delta(-I K + r-M-(2 delta)/r)
]
reDef@
BetaSasakiNakamura[opts:OptionsPattern[]]:=BetaSasakiNakamura[DefaultSymbols[],opts]


def@
FSasakiNakamura[syms_Association,opts:OptionsPattern[]]:=
Module[{r,delta,a,eta,optionsRules,s},
	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	If[s=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Aborting[]
	];

	r=RSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	delta=DeltaKerr[syms];
	eta=EtaSasakiNakamura[syms,"SpinWeight"->s];
	
	D[eta,r]/eta delta/(r^2+a^2)
];
reDef@
FSasakiNakamura[opts:OptionsPattern[]]:=FSasakiNakamura[DefaultSymbols[],opts]


def@
EtaSasakiNakamura[syms_Association,opts:OptionsPattern[]]:=
Module[{c,r,laS,a,om,m,M,optionsRules,s},
	
	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	If[s=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Aborting[]
	];

	r=RSymbol[syms];
	laS = LambdaSpinSymbol[syms,"SpinWeight"->s];
	a=BlackHoleSpinSymbol[syms];
	om=FrequencySymbol[syms];
	m=MSymbol[syms];
	M=BlackHoleMassSymbol[syms];
		
	c[0]=-12 I om M + laS(laS+2)-12 a om (a om-m);
	c[1]=8I a(3 a om - laS(a om - m));
	c[2]=-24 I a M (a om - m) + 12 a^2 (1-2(a om - m)^2);
	c[3]=24I a^3 (a om -m)-24 M a^2;
	c[4]=12a^4;

	c[0]+c[1]/r+c[2]/r^2+c[3]/r^3+c[4]/r^4
];
reDef@
EtaSasakiNakamura[opts:OptionsPattern[]]:=EtaSasakiNakamura[DefaultSymbols[],opts]


def@
TeukolskyFunctionAsSasakiNakamuraFunction[syms_Association,opts:OptionsPattern[]]:=
Module[{r,a,alpha,beta,eta,delta,chi,xSNSym,optionsRules,s},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	If[s=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Aborting[]
	];

	xSNSym=SasakiNakamuraFunctionSymbol[syms,"SpinWeight"->s];
	r=RSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	alpha=AlphaSasakiNakamura[syms,"SpinWeight"->s];
	beta=BetaSasakiNakamura[syms];
	eta=EtaSasakiNakamura[syms,"SpinWeight"->s];
	delta=DeltaKerr[syms];
	chi=(xSNSym[r]delta)/Sqrt[r^2+a^2];

	Collect[1/eta ((alpha+D[beta,r]/delta)chi-beta/delta D[chi,r]),{xSNSym[r],Derivative[_][xSNSym][r]},Simplify]
];
reDef@
TeukolskyFunctionAsSasakiNakamuraFunction[opts:OptionsPattern[]]:=TeukolskyFunctionAsSasakiNakamuraFunction[DefaultSymbols[],opts]


def@
SasakiNakamuraFunctionAsTeukolskyFunction[syms_Association,opts:OptionsPattern[]]:=
Module[{r,a,jMinus,rTeukSym,optionsRules,s},

	optionsRules = {"SpinWeight" ->spinWeightFn};
	TestOptions[optionsRules,{opts}];

	s=OptionValue["SpinWeight"];
	If[s=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Aborting[]
	];

	rTeukSym=RadialTeukolskyFunctionSymbol[syms,"SpinWeight"->s];
	r=RSymbol[syms];
	a=BlackHoleSpinSymbol[syms];

	jMinus[x_]:=D[x,r]-I KKerr[syms]/DeltaKerr[syms] x;
	Collect[RemoveTeukolskyFunctionRDerivatives[syms,r^2 Sqrt[r^2+a^2]jMinus[jMinus[rTeukSym[r]/r^2]],"SpinWeight"->s],{rTeukSym[r],Derivative[_][rTeukSym][r]},Simplify]
];
reDef@
SasakiNakamuraFunctionAsTeukolskyFunction[opts:OptionsPattern[]]:=SasakiNakamuraFunctionAsTeukolskyFunction[DefaultSymbols[],opts]


End[];

EndPackage[];
