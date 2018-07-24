(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`Fields`",
				{"BlackHoleAnalysis`Utils`",
					"BlackHoleAnalysis`Coordinates`",
					"BlackHoleAnalysis`Symbols`",
					"BlackHoleAnalysis`Sources`",
					"BlackHoleAnalysis`Labels`",
					"BlackHoleAnalysis`ValidityTests`",
					"BlackHoleAnalysis`AnalyticTools`"}];


(*******************************************************************)
(* Public functions *)
(*******************************************************************)


AmplitudeFunction::usage="AmplitudeFunction[label] returns the function for returning the amplitude associated with label.";
InvariantAmplitudeFunction::usage="InvariantAmplitudeFunction[label] returns the function for returning the invariant metric perturbation amplitude associated with label.";
PushFunction::usage="PushFunction[label] returns the function for returning the push function associated with label.";


MasterFunction::usage="MasterFunction[] returns the gauge-invariant master function in symbolic form.";
TeukolskyFunction::usage="TeukolskyFunction[] returns the Teukolsky function in symbolic form.";
RadialTeukolskyFunction::usage="RadialTeukolskyFunction[] returns the radial Teukolsky function in symbolic form.";
SasakiNakamuraFunction::usage="SasakiNakamuraFunction[] returns the Sasaki-Nakamura function in symbolic form.";


SpinWeightedSpheroidalHarmonicFunction::usage="SpinWeightedSpheroidalHarmonicFunction[] returns the symbolic spheroidal harmonic with spin weight s.";


HttAmplitude::usage="HttAmplitude[] returns the metric perturbation amplitude h_tt in symbolic form.";
HtrAmplitude::usage="HtrAmplitude[] returns the metric perturbation amplitude h_tr in symbolic form.";
HrrAmplitude::usage="HrrAmplitude[] returns the metric perturbation amplitude h_rr in symbolic form.";
JtAmplitude::usage="JtAmplitude[] returns the metric perturbation amplitude j_t in symbolic form.";
JrAmplitude::usage="JrAmplitude[] returns the metric perturbation amplitude j_r in symbolic form.";
GAmplitude::usage="GAmplitude[] returns the metric perturbation amplitude G in symbolic form.";
KAmplitude::usage="KAmplitude[] returns the metric perturbation amplitude K in symbolic form.";
HtAmplitude::usage="HtAmplitude[] returns the metric perturbation amplitude h_t in symbolic form.";
HrAmplitude::usage="HrAmplitude[] returns the metric perturbation amplitude h_r in symbolic form.";
H2Amplitude::usage="H2Amplitude[] returns the metric perturbation amplitude h_2 in symbolic form.";
MartelPoissonAmplitude::usage="MartelPoissonAmplitude[label] returns the Martel and Poisson metric \
perturbation corresponding to label written in terms of the Barack and Sago amplitudes.";
BarackSagoAmplitude::usage="BarackSagoAmplitude[label,gauge] returns the Barack and Sago metric \
perturbation corresponding to label (an integer 1-10) written in terms of the Martel and Poisson amplitudes. \
The specified gauge can be \"RWZ\", \"ModRWZ\", \"Lorenz\", \"Undefined\", or Null.
BarackSagoAmplitude[label] is equivalent to BarackSagoAmplitude[label, \"RWZ\"]";


HttInvariantAmplitude::usage="HttInvariantAmplitude[] returns the gauge-invariant \
metric perturbation amplitude h_tt in symbolic form.";
HtrInvariantAmplitude::usage="HttInvariantAmplitude[] returns the gauge-invariant \
metric perturbation amplitude h_tr in symbolic form.";
HrrInvariantAmplitude::usage="HttInvariantAmplitude[] returns the gauge-invariant \
metric perturbation amplitude h_rr in symbolic form.";
KInvariantAmplitude::usage="HttInvariantAmplitude[] returns the gauge-invariant \
metric perturbation amplitude K in symbolic form.";
HtInvariantAmplitude::usage="HtInvariantAmplitude[] returns the gauge-invariant \
metric perturbation amplitude h_t in symbolic form.";
HrInvariantAmplitude::usage="HrInvariantAmplitude[] returns the gauge-invariant \
metric perturbation amplitude h_r in symbolic form.";


HttPush::usage="HttPush[] returns the change in the metric perturbation amplitude h_tt \
as it is pushed between two Options-specifiable gauges.";
HtrPush::usage="HtrPush[] returns the change in the metric perturbation amplitude h_tr \
as it is pushed between two Options-specifiable gauges.";
HrrPush::usage="HrrPush[] returns the change in the metric perturbation amplitude h_rr \
as it is pushed between two Options-specifiable gauges.";
JtPush::usage="JtPush[] returns the change in the metric perturbation amplitude j_t \
as it is pushed between two Options-specifiable gauges.";
JrPush::usage="JrPush[] returns the change in the metric perturbation amplitude j_r \
as it is pushed between two Options-specifiable gauges.";
KPush::usage="KPush[] returns the change in the metric perturbation amplitude K \
as it is pushed between two Options-specifiable gauges.";
GPush::usage="GPush[] returns the change in the metric perturbation amplitude G \
as it is pushed between two Options-specifiable gauges.";
HtPush::usage="HtPush[] returns the change in the metric perturbation amplitude h_t \
as it is pushed between two Options-specifiable gauges.";
HrPush::usage="HrPush[] returns the change in the metric perturbation amplitude h_r \
as it is pushed between two Options-specifiable gauges.";
H2Push::usage="H2Push[] returns the change in the metric perturbation amplitude h_2 \
as it is pushed between two Options-specifiable gauges.";


XiEvenAmplitude::usage="XiEvenAmplitude[] returns the gauge vector amplitude xiE \
representing a transformation between two Options-specifiable gauges.";
XiEvenTAmplitude::usage="XiEvenTAmplitude[] returns the gauge vector amplitude xiE_t \
representing a transformation between two Options-specifiable gauges.";
XiEvenRAmplitude::usage="XiEvenRAmplitude[] returns the gauge vector amplitude xiE_r \
representing a transformation between two Options-specifiable gauges.";
XiOddAmplitude::usage="XiOddAmplitude[] returns the gauge vector amplitude xiO \
representing a transformation between two Options-specifiable gauges.";


Begin["`Private`"];


Options[MasterFunction]=Join[Options[MasterFunctionSymbol],{"Weak"->False,"MPs"->False,"Gauge"->"RWZ","ReturnSymbol"->False}];
DocumentationBuilder`OptionDescriptions["MasterFunction"] = 
Join[DocumentationBuilder`OptionDescriptions["MasterFunctionSymbol"],
{
    "Weak" -> "Boolean stating whether the master function should be given in a weak form",
	"MPs" -> "Boolean stating whether metric perturbation amplitudes should be substituted for the master function",
	"Gauge" -> "Gauge of the metric perturvation amplitudes (\"RWZ\", \"Lorenz\",  \"Undefined\", or Null)",
	"ReturnSymbol"->"Boolean stating whether to only return the symbol of the field (with no arguments)"
  }];
Options[TeukolskyFunction]=Join[Options[TeukolskyFunctionSymbol],{"ReturnSymbol"->False}];
DocumentationBuilder`OptionDescriptions["TeukolskyFunction"] = Join[DocumentationBuilder`OptionDescriptions["TeukolskyFunctionSymbol"],
{"ReturnSymbol"->"Boolean stating whether to only return the symbol of the field (with no arguments)"}];
Options[SasakiNakamuraFunction]=Options[TeukolskyFunction];
DocumentationBuilder`OptionDescriptions["SasakiNakamuraFunction"] = DocumentationBuilder`OptionDescriptions["TeukolskyFunction"];
Options[RadialTeukolskyFunction]=Options[TeukolskyFunction];
DocumentationBuilder`OptionDescriptions["RadialTeukolskyFunction"] = DocumentationBuilder`OptionDescriptions["TeukolskyFunction"];
Options[SpinWeightedSpheroidalHarmonicFunction]=Join[Options[SpinWeightedSpheroidalHarmonicSymbol],{"ReturnSymbol"->False}];
DocumentationBuilder`OptionDescriptions["SpinWeightedSpheroidalHarmonicFunction"] = 
Join[DocumentationBuilder`OptionDescriptions["SpinWeightedSpheroidalHarmonicSymbol"],
{"ReturnSymbol"->"Boolean stating whether to only return the symbol of the field (with no arguments)"}];

Options[getPsiFromMPs] = FilterRules[Options[MasterFunction],{"Parity","Variable","Gauge","Weak"}];

Options[HttAmplitude]={"Weak"->False,"Gauge"->"RWZ","SourceExpansion"->"None","Reconstruct"->False,"PushFrom"->"Same","ExpandGaugeVector"->False,"ExpandZerilli"->False,"ReturnSymbol"->False,"Mode"->"Radiative"};
DocumentationBuilder`OptionDescriptions["HttAmplitude"] = 
{
	"Weak" -> "Boolean stating whether the amplitude should be given in a weak form",
	"Reconstruct" -> "Boolean stating whether the metric perturbation amplitude should be written in terms of the master function",
	"Gauge" -> "Gauge (\"RWZ\", \"ModRWZ\", \"Lorenz\",  \"Undefined\", or Null)",
	"PushFrom"->"Specifies a gauge to start in. A gauge vector term is used to \"push\" from that gauge. For \
no push use the default, \"PushFrom\"->\"Same\". Other option values are \
\"RWZ\", \"ModRWZ\", and \"Lorenz\"",
	"SourceExpansion"->"States to what extent source terms (resulting from expansion of metric \
perturbation amplitudes) should be expanded, (\"Full\", \"Partial\", or \"None\")",
	"ExpandGaugeVector"->"Boolean stating whether to expand the analytic gauge vector connecting RW and ModRW gauges",
	"ReturnSymbol"->"Boolean stating whether to only return the symbol of the field (with no arguments)",
	"ExpandZerilli"->"Boolean stating whether to write out the analytic Zerilli solution to a low order mode",
	"Mode"->"l mode (\"Radiative\", \"Dipole\", or \"Monopole\")"
  };

Options[getMPAmplitude]=Options[HttAmplitude];
Options[HtrAmplitude]=Options[HttAmplitude];
DocumentationBuilder`OptionDescriptions["HtrAmplitude"]=DocumentationBuilder`OptionDescriptions["HttAmplitude"];
Options[HrrAmplitude]=Options[HttAmplitude];
DocumentationBuilder`OptionDescriptions["HrrAmplitude"]=DocumentationBuilder`OptionDescriptions["HttAmplitude"];
Options[JtAmplitude]=Options[HttAmplitude];
DocumentationBuilder`OptionDescriptions["JtAmplitude"]=DocumentationBuilder`OptionDescriptions["HttAmplitude"];
Options[JrAmplitude]=Options[HttAmplitude];
DocumentationBuilder`OptionDescriptions["JrAmplitude"]=DocumentationBuilder`OptionDescriptions["HttAmplitude"];
Options[KAmplitude]=Options[HttAmplitude];
DocumentationBuilder`OptionDescriptions["KAmplitude"]=DocumentationBuilder`OptionDescriptions["HttAmplitude"];
Options[GAmplitude]=Options[HttAmplitude];
DocumentationBuilder`OptionDescriptions["GAmplitude"]=DocumentationBuilder`OptionDescriptions["HttAmplitude"];
Options[HtAmplitude]=Options[HttAmplitude];
DocumentationBuilder`OptionDescriptions["HtAmplitude"]=DocumentationBuilder`OptionDescriptions["HttAmplitude"];
Options[HrAmplitude]=Options[HttAmplitude];
DocumentationBuilder`OptionDescriptions["HrAmplitude"]=DocumentationBuilder`OptionDescriptions["HttAmplitude"];
Options[H2Amplitude]=Options[HttAmplitude];
DocumentationBuilder`OptionDescriptions["H2Amplitude"]=DocumentationBuilder`OptionDescriptions["HttAmplitude"];

Options[getHttFromPsi]={"SourceExpansion"->"None","Weak"->False};
Options[getHtrFromPsi]=Options[getHttFromPsi];
Options[getHrrFromPsi]=Options[getHttFromPsi];
Options[getKFromPsi]=Options[getHttFromPsi];
Options[getHtFromPsi]=Options[getHttFromPsi];
Options[getHrFromPsi]=Options[getHttFromPsi];

Options[HttInvariantAmplitude]=FilterRules[Options[HttAmplitude],{"Weak","Gauge"}];
DocumentationBuilder`OptionDescriptions["HttInvariantAmplitude"]=FilterRules[DocumentationBuilder`OptionDescriptions["HttAmplitude"],{"Weak","Gauge"}];
Options[HtrInvariantAmplitude]=Options[HttInvariantAmplitude];
DocumentationBuilder`OptionDescriptions["HtrInvariantAmplitude"]=DocumentationBuilder`OptionDescriptions["HttInvariantAmplitude"];
Options[HrrInvariantAmplitude]=Options[HttInvariantAmplitude];
DocumentationBuilder`OptionDescriptions["HrrInvariantAmplitude"]=DocumentationBuilder`OptionDescriptions["HttInvariantAmplitude"];
Options[KInvariantAmplitude]=Options[HttInvariantAmplitude];
DocumentationBuilder`OptionDescriptions["KInvariantAmplitude"]=DocumentationBuilder`OptionDescriptions["HttInvariantAmplitude"];
Options[HtInvariantAmplitude]=Options[HttInvariantAmplitude];
DocumentationBuilder`OptionDescriptions["HtInvariantAmplitude"]=DocumentationBuilder`OptionDescriptions["HttInvariantAmplitude"];
Options[HrInvariantAmplitude]=Options[HttInvariantAmplitude];
DocumentationBuilder`OptionDescriptions["HrInvariantAmplitude"]=DocumentationBuilder`OptionDescriptions["HttInvariantAmplitude"];


Options[HttPush]={"Mode"->"Radiative","Weak"->False,"InitialGauge"->"RWZ","FinalGauge"->"Lorenz","Expand"->False};
DocumentationBuilder`OptionDescriptions["HttPush"] = 
{
	"Weak" -> "Boolean stating whether the metric perturvation amplitude should be given in a weak form",
	"InitialGauge" -> "Initial gauge (\"RWZ\", \"ModRWZ\", \"Lorenz\", or Null)",
	"FinalGauge"->"Final gauge (\"RWZ\", \"ModRWZ\", \"Lorenz\", or Null)",
	"Expand"->"Boolean stating whether expand the gauge vector. Only RWZ->ModRWZ (or opposite) can be expanded.",
	"Mode"->"l mode (\"Radiative\", \"Dipole\", or \"Monopole\")"
};

Options[HtrPush] = Options[HttPush];
DocumentationBuilder`OptionDescriptions["HtrPush"]=DocumentationBuilder`OptionDescriptions["HttPush"];
Options[HrrPush] = Options[HttPush];
DocumentationBuilder`OptionDescriptions["HrrPush"]=DocumentationBuilder`OptionDescriptions["HttPush"];
Options[JtPush] = Options[HttPush];
DocumentationBuilder`OptionDescriptions["JtPush"]=DocumentationBuilder`OptionDescriptions["HttPush"];
Options[JrPush] = Options[HttPush];
DocumentationBuilder`OptionDescriptions["JrPush"]=DocumentationBuilder`OptionDescriptions["HttPush"];
Options[KPush] = Options[HttPush];
DocumentationBuilder`OptionDescriptions["KPush"]=DocumentationBuilder`OptionDescriptions["HttPush"];
Options[GPush] = Options[HttPush];
DocumentationBuilder`OptionDescriptions["GPush"]=DocumentationBuilder`OptionDescriptions["HttPush"];
Options[HtPush] = Options[HttPush];
DocumentationBuilder`OptionDescriptions["HtPush"]=DocumentationBuilder`OptionDescriptions["HttPush"];
Options[HrPush] = Options[HttPush];
DocumentationBuilder`OptionDescriptions["HrPush"]=DocumentationBuilder`OptionDescriptions["HttPush"];
Options[H2Push] = Options[HttPush];
DocumentationBuilder`OptionDescriptions["H2Push"]=DocumentationBuilder`OptionDescriptions["HttPush"];

Options[getZerilliMPFn]={"Mode"->"Dipole"};
Options[getModFn]={"Mode"->"Radiative"};
Options[getGaugeVar]=Join[Options[HttPush],{"ReturnSymbol"->False}];
Options[XiEvenAmplitude]=Options[getGaugeVar];
DocumentationBuilder`OptionDescriptions["XiEvenAmplitude"]=Join[DocumentationBuilder`OptionDescriptions["HttPush"],{"ReturnSymbol"->"Boolean stating whether to only return the symbol of the field (with no arguments)"}];
Options[XiEvenTAmplitude]=Options[getGaugeVar];
DocumentationBuilder`OptionDescriptions["XiEvenTAmplitude"]=DocumentationBuilder`OptionDescriptions["XiEvenAmplitude"];
Options[XiEvenRAmplitude]=Options[getGaugeVar]
DocumentationBuilder`OptionDescriptions["XiEvenRAmplitude"]=DocumentationBuilder`OptionDescriptions["XiEvenAmplitude"];
Options[XiOddAmplitude]=Options[getGaugeVar];
DocumentationBuilder`OptionDescriptions["XiOddAmplitude"]=DocumentationBuilder`OptionDescriptions["XiEvenAmplitude"];


parityQ[x_]:=MemberQ[{"Even","Odd","Both"},x];
gaugeQ[x_]:=MemberQ[{"ModRWZ","RWZ","Lorenz","Undefined",Null},x];
gaugeInitFinalQ[x_]:=MemberQ[{"ModRWZ","RWZ","Lorenz",Null},x];
modeQ[x_]:=MemberQ[{"Radiative","Monopole","Dipole"},x];
sourceExpQ[x_]:=MemberQ[{"None","Partial","Full"},x];
spinWeightTest=Function[x,MemberQ[Join[Range[-2,2],{1/2,-1/2}],x]];


zmMax=7;
cpmMax=6;
jtMax=5;
(*zmVars="ZM"<>If[#===0,"",ToString[#]]&/@Range[0,zmMax];
cpmVars="CPM"<>If[#===0,"",ToString[#]]&/@Range[0,cpmMax];
jtVars="JT"<>If[#===0,"",ToString[#]]&/@Range[0,jtMax];
*)
zmVars=Join[{"ZM"},"ZM"<>ToString[#]&/@Range[0,zmMax]];
cpmVars=Join[{"CPM"},"CPM"<>ToString[#]&/@Range[0,cpmMax]];
jtVars=Join[{"JT"},"JT"<>ToString[#]&/@Range[0,jtMax]];
masterVars=Join[{Default},zmVars,cpmVars,jtVars];


def@
MasterFunction[syms_Association,opts:OptionsPattern[]]:=
Module[{optionsRules,weak,t,r,psiSym,parity,mps,rs,varOpt,var,parOpt},

	optionsRules = {"Weak"->BooleanQ,
					"MPs"->BooleanQ,
					"Gauge"->Function[x,MemberQ[{"RWZ","Lorenz","Undefined",Null},x]],
					"Parity" -> Function[x,MemberQ[{Default,"Even","Odd"},x]],
					"Variable" -> Function[x,MemberQ[masterVars,x]],
					"ReturnSymbol"->BooleanQ};

	TestOptions[optionsRules,{opts}];
	varOpt=OptionValue["Variable"];
	parOpt=OptionValue["Parity"];

	{parity,var}=ParityAndVariable[parOpt,varOpt];

	weak=OptionValue[Weak];
	mps=OptionValue[MPs];
	rs=OptionValue[ReturnSymbol];

	psiSym=MasterFunctionSymbol[syms,"Variable"->var];
	If[rs,Return@psiSym];

	r=RSymbol[syms];
	t=TSymbol[syms];

	If[mps,
		getPsiFromMPs[syms,FilterRules[{opts},Options@getPsiFromMPs]],
		If[weak,
			MakeFieldWeak[syms,psiSym[t,r],DiscontinuityOrder->-1],
			psiSym[t,r]
		]
	]
]
reDef@
MasterFunction[opts:OptionsPattern[]]:=MasterFunction[DefaultSymbols[],opts]


def@
TeukolskyFunction[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,th,ph,teuk,optionsRules,rs,sw},

	optionsRules = {"ReturnSymbol"->BooleanQ,
					"SpinWeight"->spinWeightTest};
	TestOptions[optionsRules,{opts}];
	
	sw=OptionValue[SpinWeight];
	rs=OptionValue[ReturnSymbol];
	
	t=TSymbol[syms];
	r=RSymbol[syms];
	th=ThetaSymbol[syms];
	ph=PhiSymbol[syms];
	

	teuk = TeukolskyFunctionSymbol[syms, SpinWeight->sw];
	
	If[rs,teuk,teuk[t,r,th,ph]]
]
reDef@
TeukolskyFunction[opts:OptionsPattern[]]:=TeukolskyFunction[DefaultSymbols[],opts]


def@
RadialTeukolskyFunction[syms_Association,opts:OptionsPattern[]]:=
Module[{r,RTeuk,optionsRules,rs,sw},

	optionsRules = {"ReturnSymbol"->BooleanQ,
					"SpinWeight"->spinWeightTest};
	TestOptions[optionsRules,{opts}];
	
	sw=OptionValue[SpinWeight];
	rs=OptionValue[ReturnSymbol];

	r=RSymbol[syms];
	RTeuk=RadialTeukolskyFunctionSymbol[syms,SpinWeight->sw];
	If[rs,RTeuk,RTeuk[r]]
]
reDef@
RadialTeukolskyFunction[opts:OptionsPattern[]]:=RadialTeukolskyFunction[DefaultSymbols[],opts]


def@
SasakiNakamuraFunction[syms_Association,opts:OptionsPattern[]]:=
Module[{r,XSN,optionsRules,rs,sw},

	optionsRules = {"ReturnSymbol"->BooleanQ,
					"SpinWeight"->spinWeightTest};

	TestOptions[optionsRules,{opts}];
	
	rs=OptionValue[ReturnSymbol];
	sw=OptionValue[SpinWeight];

	r=RSymbol[syms];
	XSN=SasakiNakamuraFunctionSymbol[syms,SpinWeight->sw];
	If[rs,XSN,XSN[r]]
]
reDef@
SasakiNakamuraFunction[opts:OptionsPattern[]]:=SasakiNakamuraFunction[DefaultSymbols[],opts]


def@
SpinWeightedSpheroidalHarmonicFunction[syms_Association,opts:OptionsPattern[]]:=
Module[{Slm,th,optionsRules,conj,rs,sw},

	optionsRules = {"Conjugate"->BooleanQ,
					"ReturnSymbol"->BooleanQ,
					"SpinWeight"->spinWeightTest};

	TestOptions[optionsRules,{opts}];
	
	conj=OptionValue[Conjugate];
	rs=OptionValue[ReturnSymbol];
	sw=OptionValue[SpinWeight];

	Slm=SpinWeightedSpheroidalHarmonicSymbol[syms,"SpinWeight"->sw,Conjugate->conj];
	th=ThetaSymbol[syms];

	If[rs,Slm,Slm[th]]
]
reDef@
SpinWeightedSpheroidalHarmonicFunction[opts:OptionsPattern[]]:=SpinWeightedSpheroidalHarmonicFunction[DefaultSymbols[],opts];


def@
getPsiFromMPs[syms_Association,opts:OptionsPattern[]] :=
Module[{t, r, M, la,parOpt,varOpt,rpDot,rp,m,f,
        hrr, KK, ht, hr, weak,htr,htt,EE,JJ,
		optionsRules,parity,var,gauge},

	optionsRules = {"Parity" -> Function[x,MemberQ[{Default,"Even","Odd"},x]],
					"Variable" -> Function[x,MemberQ[masterVars,x]],
					"Weak"->BooleanQ,
					"Gauge"->Function[x, x==="RWZ" || x==="Lorenz" || x==="Undefined"||x==="Invariant"]};
	TestOptions[optionsRules,{opts}];
	parOpt = OptionValue["Parity"];
	varOpt = OptionValue["Variable"];
	weak = OptionValue[Weak];
	gauge = OptionValue[Gauge];

	{parity,var}=ParityAndVariable[parOpt,varOpt];
	
	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	EE=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	rp=RpSymbol[syms];
	f = SchwarzschildF;

	Switch[var,
		"ZM",

		hrr[tt_,rr_]:=HrrInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		KK[tt_,rr_]:=KInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};

		r/(la+1) (KK[t,r]+1/CapitalLambda[r,M,la] (f[r,M]^2 hrr[t,r]-r f[r,M] D[KK[t,r],r])),

		"CPM",

		ht[tt_,rr_]:=HtInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		hr[tt_,rr_]:=HrInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};

		r/la (D[ht[t,r],r]-D[hr[t,r],t]-2/r ht[t,r]),

		"CPM1",
		
		hr[tt_,rr_]:=HrInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};

		2f[r,M]/r hr[t,r],

		"CPM2",
		
		ht[tt_,rr_]:=HtInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		hr[tt_,rr_]:=HrInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};

		(-2*EE^2*M*Derivative[1][rp][t])/((2*M - r)*(JJ^2 + r^2)) hr[t,r]
		+ (-2*M + r)/r^2 D[hr[t,r],t]
		+ (EE^2*r*Derivative[1][rp][t])/(JJ^2 + r^2) D[hr[t,r],r]
		- ((EE^2*r^3*Derivative[1][rp][t])/((-2*M + r)^2*(JJ^2 + r^2)))D[ht[t,r],t],

		"JT",
		
		hrr[tt_,rr_]:=HrrInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		KK[tt_,rr_]:=KInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};

		(2r)/(la(la+1)) (-la KK[t,r]+ r f[r,M] D[KK[t,r],r] - f[r,M]^2 hrr[t,r]),

		"ZM1",

		htr[tt_,rr_]:=HtrInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		KK[tt_,rr_]:=KInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		
		(r^2) /(3M+r la) D[KK[t,r],t] + (2M-r)/(3M+r la) htr[t,r],

		"ZM2",

		htt[tt_,rr_]:=HttInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		hrr[tt_,rr_]:=HrrInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		KK[tt_,rr_]:=KInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		
		(((-2 M+r)^2 (2 M+la r))/(r^3 (3 M+la r))+(EE^2 (2 M-r))/(JJ^2+r^2)) hrr[t,r]+(EE^2 r htt[t,r])/((1-(2 M)/r) (JJ^2+r^2))+(la (2 M-r) KK[t,r])/(r (3 M+la r))+(M (-2 M+r) D[KK[t,r],r])/(r (3 M+la r)),

		"ZM3",

		m = MSymbol[syms];
		htt[tt_,rr_]:=HttInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		htr[tt_,rr_]:=HtrInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		hrr[tt_,rr_]:=HrrInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};
		KK[tt_,rr_]:=KInvariantAmplitude[syms,Gauge->gauge,Weak->weak]/.{t->tt,r->rr};

		rpDot=rp'[t];
		-((EE r ((2 I JJ m (JJ^2+r^2) (-JJ^2 r+(-1+EE^2) r^3+2 M (JJ^2+r^2)))/r^2-(EE (3 M^2 (7 JJ^4+20 JJ^2 r^2+13 r^4)+la r^2 (-4 JJ^4+(-11+2 EE^2) JJ^2 r^2+(-7+6 EE^2) r^4)+3 M r (3 JJ^4 (-1+la)+JJ^2 (-9+2 EE^2+8 la) r^2+(-6+6 EE^2+5 la) r^4)) rpDot)/((2 M-r) (3 M+la r))))/((-2 M+r) (JJ^2+r^2)^3))htt[t,r]
		+(M (-(((1+la) (2 M-r) r)/(3 M+la r))+(4 (JJ^2 r-(-1+EE^2) r^3-2 M (JJ^2+r^2)))/(JJ^2+r^2)))/r^4 htr[t,r]
		-1/(r^3 (3 M+la r) (JJ^2+r^2)^3) I EE (2 JJ m (2 M-r) (3 M+la r) (JJ^2+r^2) (-JJ^2 r+(-1+EE^2) r^3+2 M (JJ^2+r^2))
										+I EE r^2 (3 M^2 (7 JJ^4+20 JJ^2 r^2+13 r^4)+la r^2 (-4 JJ^4+(-11+2 EE^2) JJ^2 r^2+(-7+6 EE^2) r^4)
										+3 M r (3 JJ^4 (-1+la)+JJ^2 (-9+2 EE^2+8 la) r^2+(-6+6 EE^2+5 la) r^4)) rpDot) hrr[t,r]
		+(EE^2 r)/((1-(2 M)/r) (JJ^2+r^2)) D[htt[t,r],t]
		+(2 (2 M-r) (-JJ^2 r+(-1+EE^2) r^3+2 M (JJ^2+r^2)))/(r^3 (JJ^2+r^2)) D[htr[t,r],r]
		+((-2 M+r) (-JJ^2 r+(-1+EE^2) r^3+2 M (JJ^2+r^2)))/(r^3 (JJ^2+r^2)) D[hrr[t,r],t]
		+(15 M^2 (JJ^2+r^2)+la r^2 (-3 JJ^2+(-3+2 EE^2) r^2)+M r (JJ^2 (-7+6 la)+(-7+6 EE^2+6 la) r^2))/(r^2 (3 M+la r) (JJ^2+r^2)) D[KK[t,r],t],

		"ZM4"|"ZM5"|"ZM6"|"JT1"|"JT2"|"JT3"|"JT4"|"JT5"|"CPM3"|"CPM4"|"CPM5",

		Print["No metric perturbation representation of variable ",var];
		Aborting[syms];
	]
]


def@
HttAmplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,HttLabel[],opts]
reDef@
HttAmplitude[opts:OptionsPattern[]]:=HttAmplitude[DefaultSymbols[],opts]


def@
HtrAmplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,HtrLabel[],opts]
reDef@
HtrAmplitude[opts:OptionsPattern[]]:=HtrAmplitude[DefaultSymbols[],opts]


def@
HrrAmplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,HrrLabel[],opts]
reDef@
HrrAmplitude[opts:OptionsPattern[]]:=HrrAmplitude[DefaultSymbols[],opts]


def@
JtAmplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,JtLabel[],opts]
reDef@
JtAmplitude[opts:OptionsPattern[]]:=JtAmplitude[DefaultSymbols[],opts]


def@
JrAmplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,JrLabel[],opts]
reDef@
JrAmplitude[opts:OptionsPattern[]]:=JrAmplitude[DefaultSymbols[],opts]


def@
KAmplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,KLabel[],opts]
reDef@
KAmplitude[opts:OptionsPattern[]]:=KAmplitude[DefaultSymbols[],opts]


def@
GAmplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,GLabel[],opts]
reDef@
GAmplitude[opts:OptionsPattern[]]:=GAmplitude[DefaultSymbols[],opts]


def@
HtAmplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,HtLabel[],opts]
reDef@
HtAmplitude[opts:OptionsPattern[]]:=HtAmplitude[DefaultSymbols[],opts]


def@
HrAmplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,HrLabel[],opts]
reDef@
HrAmplitude[opts:OptionsPattern[]]:=HrAmplitude[DefaultSymbols[],opts]


def@
H2Amplitude[syms_Association,opts:OptionsPattern[]]:=getMPAmplitude[syms,H2Label[],opts]
reDef@
H2Amplitude[opts:OptionsPattern[]]:=H2Amplitude[DefaultSymbols[],opts]


def@
MartelPoissonAmplitude[syms_Association,label_String]:=
Module[{t,r,M,la,hBL,f},
	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	hBL=HBarackSagoSymbol[syms];
	f = SchwarzschildF;
	
	Switch[label,
		"htt",1/(2r) (hBL[1][t,r]+f[r,M]hBL[6][t,r]),
		"htr",1/(2r) (f[r,M]^-1 hBL[2][t,r]),
		"hrr",1/(2r f[r,M]^2) (hBL[1][t,r]-f[r,M]hBL[6][t,r]),
		"jt",1/(4(la+1)) hBL[4][t,r],
		"jr", 1/(4f[r,M](la+1)) hBL[5][t,r],
		"K",1/(2r ) hBL[3][t,r],
		
		"G", 1/(4r la(la+1)) hBL[7][t,r],
		"ht",-(1/(4(la+1))) hBL[8][t,r],
		"hr",-(1/(4f[r,M](la+1))) hBL[9][t,r],
		"h2",-r 1/(4 la(la+1)) hBL[10][t,r],
		___,Print["Label "<>label<>" not found in list "<>StringJoin[Riffle[MetricPerturbationLables[],", "]]];Aborting[syms]

	]
]
reDef@
MartelPoissonAmplitude[label_String]:=MartelPoissonAmplitude[DefaultSymbols[],label]


def@
BarackSagoAmplitude[syms_Association,label_Integer,gauge_String:"RWZ"]:=
Module[{t,r,M,la,amp,f},
	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	amp[str_]:=AmplitudeFunction[str][syms,"Gauge"->gauge];
	f = SchwarzschildF;
	
	Switch[label,
		1,r(f[r,M]^2 amp["hrr"]+amp["htt"]),
		2,2r f[r,M]amp["htr"],
		3,2r amp["K"],
		4,4(1+la) amp["jt"],
		5,4f[r,M](1+la) amp["jr"],
		6,r(-f[r,M]amp["hrr"]+1/f[r,M] amp["htt"]),
		7,4r la(1+la) amp["G"],
	
		8,-4(1+la) amp["ht"],
		9,-4f[r,M](1+la) amp["hr"],
		10,-((4 la(1+la) )/r)amp["h2"],
		_,
		Print["Barack-Sago amplitudes are labeled with integers from 1-10. Label given as ", label];
		Aborting[syms]
	]
]
reDef@
BarackSagoAmplitude[label_Integer,gauge_String:"RWZ"]:=BarackSagoAmplitude[DefaultSymbols[],label,gauge]


gaugePushQ[x_]:=MemberQ[{"Same","ModRWZ","RWZ","Lorenz"},x];


def@
getMPAmplitude[syms_Association,label_String,opts:OptionsPattern[]]:=
Module[{optionsRules,weak,t,r,sym,gauge,expandS,push,symOpt,
		expandZ,zerilliFn,psi,pushFrom,mode,mpFrom,
		symFn,pushFn,psiFn,expandG},

	optionsRules = {"Weak" ->BooleanQ,
					"Mode"->modeQ,
					"Gauge"->gaugeQ,
					"Reconstruct"->BooleanQ,
					"ExpandZerilli"->BooleanQ,
					"ExpandGaugeVector"->BooleanQ,
					"SourceExpansion" -> sourceExpQ,
					"PushFrom"->gaugePushQ,
					"ReturnSymbol"->BooleanQ};

	TestOptions[optionsRules,{opts}];
	mode=OptionValue["Mode"];
	weak=OptionValue[Weak];
	gauge=OptionValue[Gauge];
	psi=OptionValue[Reconstruct];
	expandS=OptionValue[SourceExpansion];
	pushFrom=OptionValue[PushFrom];
	expandG=OptionValue[ExpandGaugeVector];
	expandZ=OptionValue[ExpandZerilli];
	symOpt=OptionValue[ReturnSymbol];

	If[Not[MemberQ[MetricPerturbationLables[Mode->mode],label]],
		Print["MP amplitude ", label, " not defined for ", mode, " mode."];
		Aborting[]
	];

	t=TSymbol[syms];
	r=RSymbol[syms];

	symFn=SymbolFunction[label];
	
	sym=symFn[syms,Tag->defaultGaugeTag[gauge]];
	If[symOpt,Return@sym];

	If[gauge==="Invariant"&&mode=!="Radiative",Print["Invariant metric perturbation amplitudes only exist for Radiative modes"];Aborting[syms]];
	If[gauge==="Invariant"&&Not[MemberQ[MetricPerturbationLables[Gauge->gauge],label]],
		Print["Invariant form of metric perturbation amplitude ", label ," does not exist"];
		Aborting[syms]
	];
	If[gauge==="RWZ"&&Not[MemberQ[MetricPerturbationLables[Gauge->gauge,Mode->mode],label]],Return[0]];

	If[pushFrom=!="Same",
		
		If[gauge==="Invariant"||gauge==="Undefined",Print["Cannot push to Invariant or Undefined gauges."];Aborting[syms]];
		
		pushFn=PushFunction[label];
		push=pushFn[syms,Weak->weak,InitialGauge->pushFrom,FinalGauge->gauge,Expand->expandG,Mode->mode];
		mpFrom=getMPAmplitude[syms,label,Weak->weak,
										Mode->mode,
										PushFrom->"Same",
										Gauge->pushFrom,
										ExpandZerilli->expandZ,
										ExpandGaugeVector->False,
										Reconstruct->psi,
										SourceExpansion->expandS];
		mpFrom+push
		,
		If[mode==="Radiative",
			If[psi,
				If[gauge==="RWZ",
					psiFn=getPsiFn[label];
					psiFn[syms,FilterRules[{opts},Options@psiFn]]
					,
					Print["MP amplitude ", sym, " cannot be reconstructed from master function in ", gauge, " gauge."];
					Print["Set Option PushFrom->\"RWZ\" to reconstruct the amplitude in RWZ gauge and transform it to ", gauge, " gauge."];
					Aborting[]
				]
				,
				If[weak,
					MakeFieldWeak[syms,sym[t,r],DiscontinuityOrder->defaultDiscontinuity[syms,label,gauge]],
					sym[t,r]
				]
			]
			,
			If[expandZ,
				If[gauge==="RWZ",
					zerilliFn=getZerilliMPFn[label,Mode->mode];
					zerilliFn[syms]
					,
					Print[mode, " mode MP amplitude ", sym, " cannot be given analytically in ", gauge, " gauge."];
					Print["Set Option PushFrom->\"RWZ\" to reconstruct the amplitude in RWZ gauge and transform it to ", gauge, " gauge."];
					Aborting[]
				],
				If[weak,
					MakeFieldWeak[syms,sym[t,r],DiscontinuityOrder->defaultDiscontinuity[syms,label,gauge]],
					sym[t,r]
				]					
			]
		]
	]
]


def@
PushFunction[label_String]:=
Module[{list},

	list={HttLabel[]->HttPush,
		HtrLabel[]->HtrPush,
		HrrLabel[]->HrrPush,
		JtLabel[]->JtPush,
		JrLabel[]->JrPush,
		KLabel[]->KPush,
		GLabel[]->GPush,
		HtLabel[]->HtPush,
		HrLabel[]->HrPush,
		H2Label[]->H2Push};

	If[MemberQ[list[[All,1]],label],
		label/.list,
		Print["Label ", label, " not found in list ", list[[All,1]]];
		Aborting[]
	]
]


def@
AmplitudeFunction[label_String]:=
Module[{assoc},

	assoc=
	Association[HttLabel[]->HttAmplitude,
		HtrLabel[]->HtrAmplitude,
		HrrLabel[]->HrrAmplitude,
		JtLabel[]->JtAmplitude,
		JrLabel[]->JrAmplitude,
		KLabel[]->KAmplitude,
		GLabel[]->GAmplitude,
		HtLabel[]->HtAmplitude,
		HrLabel[]->HrAmplitude,
		H2Label[]->H2Amplitude,
		XiEvenLabel[]->XiEvenAmplitude,
		XiEvenTLabel[]->XiEvenTAmplitude,
		XiEvenRLabel[]->XiEvenRAmplitude,
		XiOddLabel[]->XiOddAmplitude];
	
	assoc[label]
]


def@
InvariantAmplitudeFunction[label_String]:=
Module[{assoc},

	assoc=
	Association[HttLabel[]->HttInvariantAmplitude,
		HtrLabel[]->HtrInvariantAmplitude,
		HrrLabel[]->HrrInvariantAmplitude,
		KLabel[]->KInvariantAmplitude,
		HtLabel[]->HtInvariantAmplitude,
		HrLabel[]->HrInvariantAmplitude];
	
	assoc[label]
]


def@
getZerilliMPFn[label_String,opts:OptionsPattern[]]:=
Module[{optionsRules,mode,list},
	optionsRules = {"Mode"->Function[x,x==="Monopole"||x==="Dipole"]};
	TestOptions[optionsRules,{opts}];

	mode=OptionValue[Mode];

	list=
	If[mode==="Monopole",
		{HttLabel[]->getHttMonopoleZerilli,
		HrrLabel[]->getHrrMonopoleZerilli},

		{HttLabel[]->getHttDipoleZerilli,
		HtrLabel[]->getHtrDipoleZerilli,
		HrrLabel[]->getHrrDipoleZerilli,
		HtLabel[]->getHtDipoleZerilli}
	];

	If[MemberQ[list[[All,1]],label],
		label/.list,
		Print["Label ", label, " not found in list ",list[[All,1]]];
		Aborting[]
	]
]


def@
getModFn[label_String,opts:OptionsPattern[]]:=
Module[{list,mode,optionsRules},

	optionsRules = {"Mode"->modeQ};
	TestOptions[optionsRules,{opts}];

	mode=OptionValue[Mode];

	list=
	Switch[mode,
		"Radiative",
		{XiEvenLabel[]->getXiEvenModRW,
		XiEvenTLabel[]->getXiEvenTModRW,
		XiEvenRLabel[]->getXiEvenRModRW,
		XiOddLabel[]->getXiOddModRW},
		"Dipole",
		{XiEvenLabel[]->getXiEvenDipoleModZ,
		XiEvenTLabel[]->getXiEvenTDipoleModZ,
		XiEvenRLabel[]->getXiEvenRDipoleModZ,
		XiOddLabel[]->getXiOddDipoleModZ},
		"Monopole",
		{XiEvenTLabel[]->getXiEvenTMonopoleModZ,
		XiEvenRLabel[]->getXiEvenRMonopoleModZ}
	];

	If[MemberQ[list[[All,1]],label],
		label/.list,
		Print["Label ", label, " not found in list ",list[[All,1]]];
		Aborting[]
	]
]


def@
getPsiFn[label_String]:=
Module[{list},

	list={HttLabel[]->getHttFromPsi,
		HtrLabel[]->getHtrFromPsi,
		HrrLabel[]->getHrrFromPsi,
		JtLabel[]->Null,
		JrLabel[]->Null,
		KLabel[]->getKFromPsi,
		GLabel[]->Null,
		HtLabel[]->getHtFromPsi,
		HrLabel[]->getHrFromPsi,
		H2Label[]->Null};

	If[MemberQ[list[[All,1]],label],
		label/.list,
		Print["Label ", label, " not found in list ",list[[All,1]]];
		Aborting[]
	]
]


mpInvarTests={"Weak"->BooleanQ,"Gauge"->Function[x,MemberQ[{"ModRWZ","RWZ","Lorenz","Invariant","Undefined",Null},x]]};


def@
HttInvariantAmplitude[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,htt,jt,jr,G,gauge,f},

	TestOptions[mpInvarTests,{opts}];

	gauge=OptionValue[Gauge];
	
	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	htt=HttAmplitude[syms,FilterRules[{opts},Options@HttAmplitude]];
	
	If[gauge==="Invariant"||gauge==="RWZ",
		jt=0;
		jr=0;
		G=0,
		jt=JtAmplitude[syms,FilterRules[{opts},Options@JtAmplitude]];
		jr=JrAmplitude[syms,FilterRules[{opts},Options@JrAmplitude]];
		G=GAmplitude[syms,FilterRules[{opts},Options@GAmplitude]]
	];

	htt - 2 D[jt,t] + (2M f[r,M])/r^2 jr + r^2 D[G,t,t] - M f[r,M]D[G,r]
]
reDef@
HttInvariantAmplitude[opts:OptionsPattern[]]:=HttInvariantAmplitude[DefaultSymbols[],opts]


def@
HtrInvariantAmplitude[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,htr,jt,jr,G,gauge,f},

	TestOptions[mpInvarTests,{opts}];

	gauge=OptionValue[Gauge];
	
	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	htr=HtrAmplitude[syms,FilterRules[{opts},Options@HtrAmplitude]];
	
	If[gauge==="Invariant"||gauge==="RWZ",
		jt=0;
		jr=0;
		G=0,
		jt=JtAmplitude[syms,FilterRules[{opts},Options@JtAmplitude]];
		jr=JrAmplitude[syms,FilterRules[{opts},Options@JrAmplitude]];
		G=GAmplitude[syms,FilterRules[{opts},Options@GAmplitude]]
	];

	htr-D[jt,r]- D[jr,t] + (2M)/(f[r,M]r^2) jt + r^2 D[G,r,t] + (r-3M)/f[r,M] D[G,t]
]
reDef@
HtrInvariantAmplitude[opts:OptionsPattern[]]:=HtrInvariantAmplitude[DefaultSymbols[],opts]


def@
HrrInvariantAmplitude[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,hrr,jr,G,gauge,f},

	TestOptions[mpInvarTests,{opts}];

	gauge=OptionValue[Gauge];
	
	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	hrr=HrrAmplitude[syms,FilterRules[{opts},Options@HrrAmplitude]];

	If[gauge==="Invariant"||gauge==="RWZ",
		jr=0;
		G=0,
		jr=JrAmplitude[syms,FilterRules[{opts},Options@JrAmplitude]];
		G=GAmplitude[syms,FilterRules[{opts},Options@GAmplitude]]
	];

	hrr - 2 D[jr,r] - (2M)/(f[r,M]r^2) jr + r^2 D[G,r,r] + (2r-3M)/f[r,M] D[G,r]
]
reDef@
HrrInvariantAmplitude[opts:OptionsPattern[]]:=HrrInvariantAmplitude[DefaultSymbols[],opts]


def@
KInvariantAmplitude[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,KK,jr,G,gauge,la,f},

	TestOptions[mpInvarTests,{opts}];

	gauge=OptionValue[Gauge];
	
	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la= LambdaOfL[syms];
	f = SchwarzschildF;
	
	KK=KAmplitude[syms,FilterRules[{opts},Options@KAmplitude]];
	
	If[gauge==="Invariant"||gauge==="RWZ",
		jr=0;
		G=0,
		jr=JrAmplitude[syms,FilterRules[{opts},Options@JrAmplitude]];
		G=GAmplitude[syms,FilterRules[{opts},Options@GAmplitude]]
	];

	KK - (2f[r,M])/r jr+r f[r,M]D[G,r]+(la+1)G
]
reDef@
KInvariantAmplitude[opts:OptionsPattern[]]:=KInvariantAmplitude[DefaultSymbols[],opts]


def@
HtInvariantAmplitude[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,ht,h2,gauge},

	TestOptions[mpInvarTests,{opts}];

	gauge=OptionValue[Gauge];
	
	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	ht=HtAmplitude[syms,FilterRules[{opts},Options@HtAmplitude]];
	
	If[gauge==="Invariant"||gauge==="RWZ",
		h2=0,
		h2=H2Amplitude[syms,FilterRules[{opts},Options@H2Amplitude]]
	];

	ht-1/2 D[h2,t]
]
reDef@
HtInvariantAmplitude[opts:OptionsPattern[]]:=HtInvariantAmplitude[DefaultSymbols[],opts]


def@
HrInvariantAmplitude[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,hr,h2,gauge},

	TestOptions[mpInvarTests,{opts}];

	gauge=OptionValue[Gauge];
	
	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	hr=HrAmplitude[syms,FilterRules[{opts},Options@HrAmplitude]];
	
	If[gauge==="Invariant"||gauge==="RWZ",
		h2=0,
		h2=H2Amplitude[syms,FilterRules[{opts},Options@H2Amplitude]]
	];

	hr-1/2 D[h2,r]+1/r h2
]
reDef@
HrInvariantAmplitude[opts:OptionsPattern[]]:=HrInvariantAmplitude[DefaultSymbols[],opts]


mpFromPsiTests={"Weak" -> BooleanQ, "SourceExpansion" -> sourceExpQ};


def@
getKFromPsi[syms_Association,opts:OptionsPattern[]] := 
Module[{t,r,M,la,A,KK,KKRHS, Psi, Qtt, se,weak,f},

	TestOptions[mpFromPsiTests,{opts}];

	se=OptionValue[SourceExpansion];
	weak=OptionValue[Weak];
	f = SchwarzschildF;
	
	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	KK=KSymbol[syms];

	A=1/(r CapitalLambda[r,M,la]) (la(la+1)+(3M)/r (la+(2M)/r));

	Psi[tt_,rr_]:=MasterFunction[syms,Parity->"Even",Weak->weak]/.{t->tt,r->rr};
	Qtt[tt_,rr_]:=QttSource[syms,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
	KKRHS[t_,r_]:=f[r,M]D[Psi[t,r],r]+A Psi[t,r]-(r^2 f[r,M]^2)/((la+1)CapitalLambda[r,M,la]) Qtt[t,r]/f[r,M]^2;
	
	Collect[KKRHS[t,r],{Derivative[___][_][___],_[t,r]},Simplify]
]


def@
getHttFromPsi[syms_Association,opts:OptionsPattern[]] := 
Module[{t,r,M,QSharp,htt,hrr,httRHS,se,weak,f},

	TestOptions[mpFromPsiTests,{opts}];

	se=OptionValue[SourceExpansion];
	weak=OptionValue[Weak];
	f = SchwarzschildF;
	
	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	htt=HttSymbol[syms];

	QSharp[tt_,rr_]:=QSharpSource[syms,SourceExpansion->se]/.{t->tt,r->rr};
	hrr[tt_,rr_]:=getHrrFromPsi[syms,FilterRules[{opts},Options@getHrrFromPsi]]/.{t->tt,r->rr};
	httRHS[t_,r_]:=f[r,M]^2 hrr[t,r]+f[r,M]QSharp[t,r];
	
	Collect[httRHS[t,r],{Derivative[___][_][___],_[t,r]},Simplify]
]


def@
getHtrFromPsi[syms_Association,opts:OptionsPattern[]] := 
Module[{t,r,M,la,Qtt,Qtr,B, htr,htrRHS,
		Psi, se,weak,f},

	TestOptions[mpFromPsiTests,{opts}];

	se=OptionValue[SourceExpansion];
	weak=OptionValue[Weak];

	f = SchwarzschildF;
	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	htr=HtrSymbol[syms];

	B=1/(r f[r,M]CapitalLambda[r,M,la]) (la(1-(3M)/r)-(3M^2)/r^2);

	Psi[tt_,rr_]:=MasterFunction[syms,Parity->"Even",Weak->weak]/.{t->tt,r->rr};
	Qtt[tt_,rr_]:=QttSource[syms,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
	Qtr[tt_,rr_]:=QtrSource[syms,Indices->"Down",SourceExpansion->se]/.{t->tt,r->rr};
	htrRHS[t_,r_]:=r D[Psi[t,r],t,r]+r B D[Psi[t,r],t]
					-r^2/(la+1) (-Qtr[t,r]+(r f[r,M])/CapitalLambda[r,M,la] D[Qtt[t,r]/f[r,M]^2,t]);
	
	Collect[htrRHS[t,r],{Derivative[___][_][___],_[t,r]},Simplify]
]


def@
getHrrFromPsi[syms_Association,opts:OptionsPattern[]] := 
Module[{t,r,M,la,KK,hrr,hrrRHS,
		Psi,se,weak,f},

	TestOptions[mpFromPsiTests,{opts}];

	se=OptionValue[SourceExpansion];
	weak=OptionValue[Weak];

	f = SchwarzschildF;
	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	hrr=HrrSymbol[syms];

	Psi[tt_,rr_]:=MasterFunction[syms,Parity->"Even",Weak->weak]/.{t->tt,r->rr};
	KK[tt_,rr_]:=getKFromPsi[syms,FilterRules[{opts},Options@getKFromPsi]]/.{t->tt,r->rr};
	hrrRHS[t_,r_]:=CapitalLambda[r,M,la]/f[r,M]^2 ((la+1)/r Psi[t,r]-KK[t,r])+r/f[r,M] D[KK[t,r],r];
	
	Collect[hrrRHS[t,r],{Derivative[___][_][___],_[t,r]},Simplify]
]


def@
getHtFromPsi[syms_Association,opts:OptionsPattern[]] := 
Module[{t,r,M,Pt,la,ht,htRHS,
		Psi,se,weak,f},

	TestOptions[mpFromPsiTests,{opts}];

	se=OptionValue[SourceExpansion];
	weak=OptionValue[Weak];
	
	f = SchwarzschildF;
	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	ht=HtSymbol[syms];

	Psi[tt_,rr_]:=MasterFunction[syms,Parity->"Odd",Weak->weak]/.{t->tt,r->rr};
	Pt[tt_,rr_]:=PtSource[syms,"Indices"->"Down",SourceExpansion->se]/.{t->tt,r->rr};
	htRHS[t_,r_]:=f[r,M]/2 D[r Psi[t,r],r]+(r^2)/(2 la) Pt[t,r];
	
	Collect[htRHS[t,r],{Derivative[___][_][___],_[t,r]},Simplify]
]


def@
getHrFromPsi[syms_Association,opts:OptionsPattern[]] := 
Module[{t,r,M,Pr,la,hr,hrRHS,
		Psi, se,weak,f},

	TestOptions[mpFromPsiTests,{opts}];

	se=OptionValue[SourceExpansion];
	weak=OptionValue[Weak];

	f = SchwarzschildF;
	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	hr=HrSymbol[syms];

	Psi[tt_,rr_]:=MasterFunction[syms,Parity->"Odd",Weak->weak]/.{t->tt,r->rr};
	Pr[tt_,rr_]:=PrSource[syms,"Indices"->"Down",SourceExpansion->se]/.{t->tt,r->rr};
	hrRHS[t_,r_]:=r/(2 f[r,M]) D[Psi[t,r],t]+r^2/(2 la) Pr[t,r];
	
	Collect[hrRHS[t,r],{Derivative[___][_][___],_[t,r]},Simplify]
]


def@
getHttMonopoleZerilli[syms_Association]:=
Module[{t,r,th,rp,M,En,mu,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	th=HeavisideSymbol[syms];
	f = SchwarzschildF;

	(*(4Sqrt[\[Pi]] (mu En)/r -(4Sqrt[\[Pi]]mu)/En f[r,M]/(rp[t] f[rp[t],M]) (2 En^2-USquared[syms]))th[r-rp[t]]*)
	
	(4Sqrt[\[Pi]] (mu En)/r)th[r-rp[t]] + (4Sqrt[\[Pi]]mu)/En f[r,M]/(rp[t] f[rp[t],M]) (2 En^2-USquared[syms]) th[rp[t]-r]
]


def@
getHrrMonopoleZerilli[syms_Association]:=
Module[{t,r,th,rp,M,En,mu,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	th=HeavisideSymbol[syms];
	f = SchwarzschildF;

	4Sqrt[\[Pi]] (mu En)/(r f[r,M]^2) th[r-rp[t]]

]


def@
getHtDipoleZerilli[syms_Association]:=
Module[{t,r,th,rp,JJ,mu},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	th=HeavisideSymbol[syms];

	(Sqrt[48\[Pi]]mu JJ)/3 (1/r th[r-rp[t]]+r^2 th[rp[t]-r]/rp[t]^3)

]


def@
getHttDipoleZerilli[syms_Association]:=
Module[{t,r,th,rp,M,En,JJ,mu,phip,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	th=HeavisideSymbol[syms];
	phip=PhiPSymbol[syms];
	f = SchwarzschildF;

	-Sqrt[((8\[Pi])/3)] (f[rp[t],M]r mu Exp[-I phip[t]])/f[r,M] ((6 rp[t](M - I En JJ rp'[t])-3 JJ^2 f[rp[t],M]+(2 En^2-3)rp[t]^2)/(En rp[t]^4)+(En rp[t])/r^3)th[r-rp[t]]

]


def@
getHtrDipoleZerilli[syms_Association]:=
Module[{t,r,th,rp,M,En,JJ,mu,phip},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	th=HeavisideSymbol[syms];
	phip=PhiPSymbol[syms];

	(2Sqrt[6 \[Pi]] Exp[-I phip[t]] r mu(-4 I M^2 JJ+4 I M JJ rp[t]-I JJ rp[t]^2+En rp[t]^3 rp'[t]))/((-2 M+r)^2 rp[t]^3) th[r-rp[t]]

]


def@
getHrrDipoleZerilli[syms_Association]:=
Module[{t,r,th,rp,M,En,mu,phip,f},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	th=HeavisideSymbol[syms];
	phip=PhiPSymbol[syms];
	f = SchwarzschildF;

	-Sqrt[24\[Pi]]mu En (rp[t]f[rp[t],M])/(r^2 f[r,M]^3) Exp[-I phip[t]]th[r-rp[t]]

]


pushTests={"Mode"->modeQ,"Weak" ->BooleanQ,"InitialGauge"->gaugeInitFinalQ,"FinalGauge"->gaugeInitFinalQ,"Expand"->BooleanQ};


def@
HttPush[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,xiT,xiR,f},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;

	xiT=XiEvenTAmplitude[syms,FilterRules[{opts},Options@XiEvenTAmplitude]];
	xiR=XiEvenRAmplitude[syms,FilterRules[{opts},Options@XiEvenRAmplitude]];

	- 2 D[xiT,t] + f[r,M] (2M)/r^2 xiR
]
reDef@
HttPush[opts:OptionsPattern[]]:=HttPush[DefaultSymbols[],opts]


def@
HtrPush[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,xiT,xiR,f},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;

	xiT=XiEvenTAmplitude[syms,FilterRules[{opts},Options@XiEvenTAmplitude]];
	xiR=XiEvenRAmplitude[syms,FilterRules[{opts},Options@XiEvenRAmplitude]];

	- D[xiT,r] - D[xiR,t] + (2M)/(f[r,M] r^2) xiT
]
reDef@
HtrPush[opts:OptionsPattern[]]:=HtrPush[DefaultSymbols[],opts]


def@
HrrPush[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,xiR,f},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	f = SchwarzschildF;
	
	xiR=XiEvenRAmplitude[syms,FilterRules[{opts},Options@XiEvenRAmplitude]];

	- 2 D[xiR,r] - (2M)/(f[r,M] r^2) xiR
]
reDef@
HrrPush[opts:OptionsPattern[]]:=HrrPush[DefaultSymbols[],opts]


def@
JtPush[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,xiT,xiE},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	xiT=XiEvenTAmplitude[syms,FilterRules[{opts},Options@XiEvenTAmplitude]];
	xiE=XiEvenAmplitude[syms,FilterRules[{opts},Options@XiEvenAmplitude]];

	- D[xiE,t] - xiT
]
reDef@
JtPush[opts:OptionsPattern[]]:=JtPush[DefaultSymbols[],opts]


def@
JrPush[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,xiE,xiR},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	xiR=XiEvenRAmplitude[syms,FilterRules[{opts},Options@XiEvenRAmplitude]];
	xiE=XiEvenAmplitude[syms,FilterRules[{opts},Options@XiEvenAmplitude]];

	- D[xiE,r] - xiR + 2/r xiE
]
reDef@
JrPush[opts:OptionsPattern[]]:=JrPush[DefaultSymbols[],opts]


def@
KPush[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,M,la,second,first,xiR,xiE,mode,laN,f},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaOfL[syms];
	mode=OptionValue[Mode];
	f = SchwarzschildF;
	
	xiR = XiEvenRAmplitude[syms,FilterRules[{opts},Options@XiEvenRAmplitude]];
	first = -(2 f[r,M])/r xiR;

	second =
		If[mode==="Monopole",
			0,

			xiE=XiEvenAmplitude[syms,FilterRules[{opts},Options@XiEvenAmplitude]];
			laN=If[mode==="Dipole",LambdaOfL[1],la];
	
			(2(laN+1))/r^2 xiE
		];

	first + second
]
reDef@
KPush[opts:OptionsPattern[]]:=KPush[DefaultSymbols[],opts]


def@
GPush[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,xiE},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];

	xiE=XiEvenAmplitude[syms,FilterRules[{opts},Options@XiEvenAmplitude]];

	- 2/r^2 xiE
]
reDef@
GPush[opts:OptionsPattern[]]:=GPush[DefaultSymbols[],opts]


def@
HtPush[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,xiO},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];

	xiO=XiOddAmplitude[syms,FilterRules[{opts},Options@XiOddAmplitude]];

	- D[xiO,t]
]
reDef@
HtPush[opts:OptionsPattern[]]:=HtPush[DefaultSymbols[],opts]


def@
HrPush[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,xiO},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];

	xiO=XiOddAmplitude[syms,FilterRules[{opts},Options@XiOddAmplitude]];

	- D[xiO,r]+2/r xiO
]
reDef@
HrPush[opts:OptionsPattern[]]:=HrPush[DefaultSymbols[],opts]


def@
H2Push[syms_Association,opts:OptionsPattern[]]:=
Module[{t,r,xiO},

	TestOptions[pushTests,{opts}];

	r=RSymbol[syms];
	t=TSymbol[syms];

	xiO=XiOddAmplitude[syms,FilterRules[{opts},Options@XiOddAmplitude]];

	- 2 xiO
]
reDef@
H2Push[opts:OptionsPattern[]]:=H2Push[DefaultSymbols[],opts]


def@
getGaugeVar[syms_Association,label_String,opts:OptionsPattern[]]:=
Module[{optionsRules,weak,t,r,sym,sym1,mode,gI,gF,tagList,exp,symFn,modFn,rs},

	optionsRules = {"Mode"->modeQ,
					"Weak"->BooleanQ,
					"InitialGauge"->gaugeInitFinalQ,
					"FinalGauge"->gaugeInitFinalQ,
					"Expand"->BooleanQ,
					"ReturnSymbol"->BooleanQ};

	TestOptions[optionsRules,{opts}];

	weak=OptionValue["Weak"];
	gI=OptionValue["InitialGauge"];
	gF=OptionValue["FinalGauge"];
	exp=OptionValue["Expand"];
	mode=OptionValue["Mode"];
	rs=OptionValue["ReturnSymbol"];

	If[Not[MemberQ[GaugeVectorLabels[Mode->mode],label]],
		Print["Gauge variable ", label, " not defined for ", mode," mode."];
		Aborting[]
	];

	If[gI===gF,Return[0]];

	symFn=SymbolFunction[label];
	
	r=RSymbol[syms];
	t=TSymbol[syms];
	sym=symFn[syms];

	tagList=getGaugeTagList[];
	sym1=If[gI===Null||gF===Null,sym,AddTag[syms,AddTag[syms,sym,(gI/.tagList)],(gF/.tagList)]];

	If[rs,
		sym1,

		If[exp,

			modFn=getModFn[label,Mode->mode];
	
			Which[gI==="RWZ"&&gF==="ModRWZ",
				modFn[syms],

				gF==="RWZ"&&gI==="ModRWZ",
				-modFn[syms],
			
				True,
				Print["Option Expand->True is only supported with the two following option configurations:"];
				Print["{InitialGauge->\"RWZ\",FinalGauge->\"ModRWZ\"}"];
				Print["{InitialGauge->\"ModRWZ\",FinalGauge->\"RWZ\"}"];
				Aborting[]
			],

			If[weak,MakeFieldWeak[syms,sym1[t,r],DiscontinuityOrder->-1],sym1[t,r]]
		]
	]
]


def@
XiEvenAmplitude[syms_Association,opts:OptionsPattern[]]:=getGaugeVar[syms,XiEvenLabel[],opts];
reDef@
XiEvenAmplitude[opts:OptionsPattern[]]:=XiEvenAmplitude[DefaultSymbols[],opts]


def@
XiEvenTAmplitude[syms_Association,opts:OptionsPattern[]]:=getGaugeVar[syms,XiEvenTLabel[],opts];
reDef@
XiEvenTAmplitude[opts:OptionsPattern[]]:=XiEvenTAmplitude[DefaultSymbols[],opts]


def@
XiEvenRAmplitude[syms_Association,opts:OptionsPattern[]]:=getGaugeVar[syms,XiEvenRLabel[],opts];
reDef@
XiEvenRAmplitude[opts:OptionsPattern[]]:=XiEvenRAmplitude[DefaultSymbols[],opts]


def@
XiOddAmplitude[syms_Association,opts:OptionsPattern[]]:=getGaugeVar[syms,XiOddLabel[],opts];
reDef@
XiOddAmplitude[opts:OptionsPattern[]]:=XiOddAmplitude[DefaultSymbols[],opts]


getGaugeTagList[]:={"ModRWZ"->"ModRWZTag","RWZ"->"RWZTag","Lorenz"->"LorenzTag","Undefined"->"GUTag","Invariant"->"InvTag"};


getXiEvenModRW[syms_Association]:=
Module[{t,r,th,rp,la,M,En,JJ,mu,YBar,YphiBar,YphiphiBar},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	la=LambdaOfL[syms];
	th=HeavisideSymbol[syms];
	YBar=YSymbol[syms,Conjugate->True];
	YphiBar=YPhiSymbol[syms,Conjugate->True];
	YphiphiBar=YPhiPhiSymbol[syms,Conjugate->True];

	(*-((4 En JJ^2 mu \[Pi] (r-rp[t]) rp[t] th[r-rp[t]] YphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)))*)

	-((4 En JJ^2 mu \[Pi] (r-rp[t]) rp[t] th[r-rp[t]] YphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)))+1/2 (r-rp[t])^2 th[r-rp[t]] ((4 En JJ^2 mu \[Pi] (JJ^4 M-JJ^4 rp[t]+8 JJ^2 M rp[t]^2+(-5+2 En^2) JJ^2 rp[t]^3+7 M rp[t]^4+(-4+6 En^2) rp[t]^5) YphiphiBar[t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3)-(8 En JJ^2 mu \[Pi] rp[t] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^2 Derivative[1][rp][t]))
]


getXiEvenTModRW[syms_Association]:=
Module[{t,r,th,rp,la,M,En,JJ,mu,YBar,YphiBar,YphiphiBar},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	la=LambdaOfL[syms];
	th=HeavisideSymbol[syms];
	YBar=YSymbol[syms,Conjugate->True];
	YphiBar=YPhiSymbol[syms,Conjugate->True];
	YphiphiBar=YPhiPhiSymbol[syms,Conjugate->True];

	(*(4 JJ^2 mu \[Pi] (-2 M+rp[t]) (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) th[r-rp[t]] YphiphiBar[t])/(En la (1+la) rp[t]^4 (JJ^2+rp[t]^2) Derivative[1][rp][t])+(r-rp[t]) th[r-rp[t]] ((8 En^2 JJ mu \[Pi] rp[t] YphiBar[t])/((1+la) (2 M-rp[t]) (JJ^2+rp[t]^2))+(4 JJ^2 mu \[Pi] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) (3 JJ^4 M-JJ^4 rp[t]+8 JJ^2 M rp[t]^2+(-3+2 En^2) JJ^2 rp[t]^3+5 M rp[t]^4+(-2+6 En^2) rp[t]^5) YphiphiBar[t])/(En la (1+la) rp[t]^5 (JJ^2+rp[t]^2)^3 Derivative[1][rp][t])-(4 En JJ^2 mu \[Pi] rp[t] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+2 En^2) rp[t]^3) Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^2))*)

	(4 JJ^2 mu \[Pi] (-2 M+rp[t]) (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) th[r-rp[t]] YphiphiBar[t])/(En la (1+la) rp[t]^4 (JJ^2+rp[t]^2) Derivative[1][rp][t])+(r-rp[t]) th[r-rp[t]] ((8 En^2 JJ mu \[Pi] rp[t] YphiBar[t])/((1+la) (2 M-rp[t]) (JJ^2+rp[t]^2))+(4 JJ^2 mu \[Pi] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) (3 JJ^4 M-JJ^4 rp[t]+8 JJ^2 M rp[t]^2+(-3+2 En^2) JJ^2 rp[t]^3+5 M rp[t]^4+(-2+6 En^2) rp[t]^5) YphiphiBar[t])/(En la (1+la) rp[t]^5 (JJ^2+rp[t]^2)^3 Derivative[1][rp][t])-(4 En JJ^2 mu \[Pi] rp[t] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+2 En^2) rp[t]^3) Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^2))+1/2 (r-rp[t])^2 th[r-rp[t]] (-((8 En^2 JJ mu \[Pi] (7 JJ^4 M-3 JJ^4 rp[t]+20 JJ^2 M rp[t]^2+(-9+2 En^2) JJ^2 rp[t]^3+13 M rp[t]^4+6 (-1+En^2) rp[t]^5) YphiBar[t])/((1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3))-(16 En JJ^2 mu \[Pi] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) YBar[t])/(rp[t]^2 (-2 M+rp[t]) (JJ^2+rp[t]^2)^2 Derivative[1][rp][t])+(4 JJ^2 mu \[Pi] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) (15 JJ^8 M^2-12 JJ^8 M rp[t]+2 (JJ^8+34 JJ^6 M^2) rp[t]^2+2 (-27+16 En^2) JJ^6 M rp[t]^3+(JJ^6 (9+En^2 (-8+6 la))+126 JJ^4 M^2) rp[t]^4+2 (-51+56 En^2) JJ^4 M rp[t]^5+2 (JJ^4 (9+En^2 (-14+9 la))+54 JJ^2 M^2) rp[t]^6+10 (-9+20 En^2) JJ^2 M rp[t]^7+(JJ^2 (17+2 En^2 (-34+9 la))+35 M^2) rp[t]^8+30 (-1+4 En^2) M rp[t]^9+6 (1+8 En^4+En^2 (-8+la)) rp[t]^10) YphiphiBar[t])/(En la (1+la) rp[t]^6 (-2 M+rp[t]) (JJ^2+rp[t]^2)^5 Derivative[1][rp][t])-(4 En JJ^2 mu \[Pi] ((-4 JJ^6 M^3+8 JJ^6 M^2 rp[t]-(5 JJ^6 M+36 JJ^4 M^3) rp[t]^2+JJ^4 (JJ^2+(60+8 En^2) M^2) rp[t]^3-3 (11 JJ^4 M+20 JJ^2 M^3) rp[t]^4-2 JJ^2 ((-3+En^2) JJ^2+8 (-6+5 En^2) M^2) rp[t]^5+M ((-51+96 En^2+8 En^4) JJ^2-28 M^2) rp[t]^6+((9-28 En^2) JJ^2+(44-88 En^2) M^2) rp[t]^7+(-23+96 En^2-40 En^4) M rp[t]^8+(4-26 En^2+24 En^4) rp[t]^9) Derivative[1][rp][t] Derivative[1][YphiphiBar][t]+rp[t] (JJ^2+rp[t]^2) (8 JJ^4 M^3-12 JJ^4 M^2 rp[t]+2 (3 JJ^4 M+8 JJ^2 M^3) rp[t]^2-JJ^2 (JJ^2-4 (-6+5 En^2) M^2) rp[t]^3+4 M ((3-5 En^2) JJ^2+2 M^2) rp[t]^4+((-2+5 En^2) JJ^2+4 (-3+5 En^2) M^2) rp[t]^5+2 (3-10 En^2+4 En^4) M rp[t]^6+(-1+5 En^2-4 En^4) rp[t]^7) YphiphiBar''[t]))/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^4 Derivative[1][rp][t]))
	
]


getXiEvenRModRW[syms_Association]:=
Module[{t,r,th,rp,la,M,mu,En,JJ,YBar,YphiBar,YphiphiBar},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	mu=ParticleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	la=LambdaOfL[syms];
	th=HeavisideSymbol[syms];
	YBar=YSymbol[syms,Conjugate->True];
	YphiBar=YPhiSymbol[syms,Conjugate->True];
	YphiphiBar=YPhiPhiSymbol[syms,Conjugate->True];

	(*(4 En JJ^2 mu \[Pi] rp[t] th[r-rp[t]] YphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2))+(r-rp[t]) th[r-rp[t]] (-((4 En JJ^2 mu \[Pi] (5 JJ^4 M-3 JJ^4 rp[t]+16 JJ^2 M rp[t]^2+(-9+2 En^2) JJ^2 rp[t]^3+11 M rp[t]^4+6 (-1+En^2) rp[t]^5) YphiphiBar[t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3))+(8 En^2 JJ mu \[Pi] rp[t]^3 YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2))+(8 En^3 JJ^2 mu \[Pi] rp[t]^6 Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^2))*)

	(4 En JJ^2 mu \[Pi] rp[t] th[r-rp[t]] YphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2))+(r-rp[t]) th[r-rp[t]] (-((4 En JJ^2 mu \[Pi] (5 JJ^4 M-3 JJ^4 rp[t]+16 JJ^2 M rp[t]^2+(-9+2 En^2) JJ^2 rp[t]^3+11 M rp[t]^4+6 (-1+En^2) rp[t]^5) YphiphiBar[t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3))+(8 En^2 JJ mu \[Pi] rp[t]^3 YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2))+(8 En^3 JJ^2 mu \[Pi] rp[t]^6 Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^2))+1/2 (r-rp[t])^2 th[r-rp[t]] (-((16 En JJ^2 mu \[Pi] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) YBar[t])/((2 M-rp[t])^3 (JJ^2+rp[t]^2)^2))+(4 En JJ^2 mu \[Pi] (35 JJ^8 M^2+2 JJ^8 (-17+4 la) M rp[t]+(JJ^8 (9-4 la)+152 JJ^6 M^2) rp[t]^2+2 JJ^6 (-77+14 En^2+16 la) M rp[t]^3+2 (JJ^6 (21-8 la+En^2 (-5+3 la))+153 JJ^4 M^2) rp[t]^4+2 JJ^4 (-159+46 En^2+24 la) M rp[t]^5+(JJ^4 (87-24 la+2 En^2 (-19+9 la))+296 JJ^2 M^2) rp[t]^6+2 JJ^2 (-155+110 En^2+16 la) M rp[t]^7+(2 JJ^2 (42-8 la+En^2 (-53+9 la))+107 M^2) rp[t]^8+4 (-28+39 En^2+2 la) M rp[t]^9+(30+48 En^4+6 En^2 (-13+la)-4 la) rp[t]^10) YphiphiBar[t])/(la (1+la) (2 M-rp[t])^3 rp[t] (JJ^2+rp[t]^2)^5)+(8 En^2 JJ mu \[Pi] rp[t]^2 (3 JJ^4 M-3 JJ^4 rp[t]+8 JJ^2 M rp[t]^2+(-7+2 En^2) JJ^2 rp[t]^3+5 M rp[t]^4+(-4+6 En^2) rp[t]^5) YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^3)+1/(la (1+la) (JJ^2+rp[t]^2)^4) 4 En^3 JJ^2 mu \[Pi] rp[t]^5 (1/(-2 M+rp[t])^64 (2 JJ^4 M^2+JJ^4 M rp[t]-(JJ^4+8 JJ^2 M^2) rp[t]^2+2 (7+En^2) JJ^2 M rp[t]^3-5 (JJ^2+2 M^2) rp[t]^4+(13-10 En^2) M rp[t]^5+(-4+6 En^2) rp[t]^6) Derivative[1][rp][t] Derivative[1][YphiphiBar][t]+(rp[t] (JJ^2+rp[t]^2) (6 JJ^2 M-3 JJ^2 rp[t]+6 M rp[t]^2+(-3+4 En^2) rp[t]^3) YphiphiBar''[t])/(2 M-rp[t])^5))
]


getXiOddModRW[syms_Association]:=
Module[{t,r,th,rp,la,M,En,JJ,mu,XphiBar,XphiphiBar},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	la=LambdaOfL[syms];
	th=HeavisideSymbol[syms];
	XphiBar=XPhiSymbol[syms,Conjugate->True];
	XphiphiBar=XPhiPhiSymbol[syms,Conjugate->True];

	(*-((4 En JJ^2 mu \[Pi] (r-rp[t]) rp[t] th[r-rp[t]] XphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)))*)

	-((4 En JJ^2 mu \[Pi] (r-rp[t]) rp[t] th[r-rp[t]] XphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)))+1/2 (r-rp[t])^2 th[r-rp[t]] ((4 En JJ^2 mu \[Pi] (JJ^4 M-JJ^4 rp[t]+8 JJ^2 M rp[t]^2+(-5+2 En^2) JJ^2 rp[t]^3+7 M rp[t]^4+(-4+6 En^2) rp[t]^5) XphiphiBar[t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3)-(8 En JJ^2 mu \[Pi] rp[t] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) Derivative[1][XphiphiBar][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^2 Derivative[1][rp][t]))
]


getXiEvenTMonopoleModZ[syms_Association]:=
Module[{t,r,th,rp,M,En,JJ,mu},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	th=HeavisideSymbol[syms];

	-((mu Sqrt[\[Pi]] (r-rp[t])^2 (10 JJ^4 M^2-9 JJ^4 M rp[t]+2 (JJ^4+8 JJ^2 M^2) rp[t]^2+(-14+En^2) JJ^2 M rp[t]^3+3 (JJ^2+2 M^2) rp[t]^4-(5+En^2) M rp[t]^5+(1+En^2-2 En^4) rp[t]^6) th[r-rp[t]])/(En (2 M-rp[t]) rp[t]^5 (JJ^2+rp[t]^2) rp'[t]))+(2 En mu Sqrt[\[Pi]] (r-rp[t]) rp[t] th[r-rp[t]] rp'[t])/(2 M-rp[t])^2
]


getXiEvenRMonopoleModZ[syms_Association]:=
Module[{t,r,th,rp,M,En,JJ,mu},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	th=HeavisideSymbol[syms];

	(2 En mu Sqrt[\[Pi]] (r-rp[t]) rp[t] th[r-rp[t]])/(2 M-rp[t])^2-(En mu Sqrt[\[Pi]] (r-rp[t])^2 (JJ^2 M-3 JJ^2 rp[t]-M rp[t]^2+2 (-1+En^2) rp[t]^3) th[r-rp[t]])/((2 M-rp[t])^3 (JJ^2+rp[t]^2))
]


getXiEvenTDipoleModZ[syms_Association]:=
Module[{t,r,th,phip,rp,M,En,JJ,mu},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	phip=PhiPSymbol[syms];
	th=HeavisideSymbol[syms];

	(E^(-I phip[t]) mu Sqrt[6 \[Pi]] (r-rp[t]) th[r-rp[t]] (2 JJ^2 M-JJ^2 rp[t]+(-1+En^2) rp[t]^3+2 rp[t]^2 (M-I En JJ rp'[t])))/(En rp[t]^4 rp'[t])-1/(En rp[t]^5 (-2 M+rp[t]) (JJ^2+rp[t]^2) rp'[t]) E^(-I phip[t]) mu Sqrt[(3 \[Pi])/2] (r-rp[t])^2 th[r-rp[t]] (10 JJ^4 M^2-7 JJ^4 M rp[t]+(JJ^4+16 JJ^2 M^2) rp[t]^2+((1+En^2) JJ^2+6 M^2) rp[t]^4-2 En^2 (-1+En^2) rp[t]^6+JJ^2 rp[t]^3 ((-10+En^2) M-I En JJ Derivative[1][rp][t])-rp[t]^5 ((3+En^2) M+I En JJ Derivative[1][rp][t]))
]


getXiEvenRDipoleModZ[syms_Association]:=
Module[{t,r,th,phip,rp,M,En,JJ,mu},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	phip=PhiPSymbol[syms];
	th=HeavisideSymbol[syms];

	-((E^(-I phip[t]) En mu Sqrt[6 \[Pi]] (r-rp[t]) rp[t] th[r-rp[t]])/(2 M-rp[t])^2)+(E^(-I phip[t]) En mu Sqrt[(3 \[Pi])/2] (r-rp[t])^2 (JJ^2 M-4 JJ^2 rp[t]-M rp[t]^2+(-3+2 En^2) rp[t]^3) th[r-rp[t]])/((2 M-rp[t])^3 (JJ^2+rp[t]^2))
]


getXiEvenDipoleModZ[syms_Association]:=
Module[{t,r,th,phip,rp,M,En,JJ,mu},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	phip=PhiPSymbol[syms];
	th=HeavisideSymbol[syms];

	(E^(-I phip[t]) En mu Sqrt[(3 \[Pi])/2] (r-rp[t])^2 rp[t] th[r-rp[t]] (-2 JJ^2 M+JJ^2 rp[t]+rp[t]^3-2 rp[t]^2 (M-I En JJ rp'[t])))/((-2 M+rp[t])^3 (JJ^2+rp[t]^2))
]


getXiOddDipoleModZ[syms_Association]:=
Module[{t,r,th,rp,M,En,JJ,mu},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	th=HeavisideSymbol[syms];

	-((2 JJ mu Sqrt[3 \[Pi]] (r-rp[t])^2 (-1-(En^2 rp[t]^3)/((2 M-rp[t]) (JJ^2+rp[t]^2))) th[r-rp[t]])/(rp[t]^2 rp'[t]))
]


defaultGaugeTag[gauge_]:=
	Switch[gauge,
			"RWZ","RWZTag",
			"ModRWZ","ModRWZTag",
			"Lorenz","LorenzTag",
			"Undefined","GUTag",
			"Invariant","InvTag",
			Null,Null,
			___,
			Print["No default tag found for specified gauge ", gauge];
			Aborting[]
	];



defaultDiscontinuity[syms_Association,mp_,gauge_]:=
Module[{mpList},

	mpList=MetricPerturbationLables[];

	If[!MemberQ[mpList,mp],
		Print[mp, " not found in list ", mpList];
		Aborting[]
	];

	Switch[gauge,
			"RWZ",
				Which[mp==="htt"||mp==="htr"||mp==="hrr",
					Return[-2],
					mp==="K"||mp==="ht"||mp==="hr"||mp==="jt"||mp==="jr"||mp==="G"||mp==="h2",
					Return[-1]
				],
			"Lorenz",Return[0],
			"Undefined",Return[-1],
			"Invariant",
				Which[mp==="htt"||mp==="htr"||mp==="hrr",
					Return[-2],
					mp==="K"||mp==="ht"||mp==="hr",
					Return[-1],
					mp==="jt"||mp==="jr"||mp==="G"||mp==="h2",
					Print[mp, " is not an invariant metric perturbation and thus has an undefined discontinuity order."];
					Aborting[]
				],
			Null,Return[-1],
			"Zerilli",Return[-1]
	];

	Print["No default discontinuity found for specified gauge ", gauge];
	Aborting[]
]


End[];

EndPackage[];
