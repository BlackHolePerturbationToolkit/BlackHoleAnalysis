(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`SchwEqs`",
				{"BlackHoleAnalysis`Utils`",
					"BlackHoleAnalysis`Harmonics`",
					"BlackHoleAnalysis`Coordinates`",
					"BlackHoleAnalysis`Symbols`",
					"BlackHoleAnalysis`Fields`",
					"BlackHoleAnalysis`Labels`",
					"BlackHoleAnalysis`Sources`",
					"BlackHoleAnalysis`KerrEqs`",
					"BlackHoleAnalysis`AnalyticTools`",
					"BlackHoleAnalysis`ValidityTests`"}];


(*******************************************************************)
(* Public functions *)
(*******************************************************************)

FieldEquations::usage="FieldEquations[] returns the first-order field equations decomposed in spherical harmonics \
for a particle orbiting a Schwarzschild black hole.";
MasterEquation::usage="MasterEquation[] returns the master equation for a particle orbiting a Schwarzschild black hole.";
SourceConservation::usage="SourceConservation[] returns the first-order source conservation expressions decomposed \
in spherical harmonics for a particle orbiting a Schwarzschild black hole.";
MasterEquationPotential::usage="MasterEquationPotential[] returns the potential for the master equation.";
GaugeConditions::usage="GaugeConditions[] returns the gauge conditions satisfied by the metric perturbation amplitudes.";
GaugeTransformationEquations::usage="GaugeTransformationEquations[] returns the metric perturbation amplitude gauge \
transformation equations for a transformation from RWZ to Lorenz gauge.";
GaugeTransformationSources::usage="GaugeTransformationEquations[] returns the source terms of the \
metric perturbation amplitude gauge transformation equations for a transformation from RWZ to Lorenz gauge.";

MasterFunctionToTeukolskyFunction::usage="MasterFunctionToTeukolskyFunction[expr] converts all frequency domain \
instances of the master function (both parities) and its r derivatives to \
the Teukolsky function and its first r derivative.";
MasterFunctionAsTeukolskyFunction::usage="MasterFunctionAsTeukolskyFunction[] returns the frequency domain master \
function written in terms of the Teukolsky function and its first r derivative.
MasterFunctionAsTeukolskyFunction[n] returns the nth r derivative frequency domain master \
function written in terms of the Teukolsky function and its first r derivative.";
KerrToSchwarzschild::usage="KerrToSchwarzschild[expr] converts all Kerr symbols to their Schwarzschild limit."
EvenMasterFunctionAsOdd::usage="EvenMasterFunctionAsOdd[] returns the frequency domain even-parity master function \
written in terms of the odd-parity function."

RemoveMasterFunctionRDerivatives::usage="RemoveMasterFunctionRDerivatives[expr] uses the master equation to remove \
all r derivatives of the master function higher than the first.";

Begin["`Private`"];



Options[FieldEquations]={"Parity"->"Both","Homogeneous"->False,"SourceExpansion"->"None","SourceIndices"->"Down","Weak"->False,"Gauge"->"RWZ","Invariant"->False,"Mode"->"Radiative"};
DocumentationBuilder`OptionDescriptions["FieldEquations"] = 
{
    "Parity" -> "Specifies which parities to include, (\"Even\", \"Odd\", or \"Both\")",
	"Mode" -> "l mode, restricting equations based on which spherical harmonic amplitudes are defined, (\"Radiative\", \"Dipole\", or \"Monopole\")",
	"Weak" -> "Boolean stating whether the amplitudes should be given in a weak form",
	"Gauge" -> "Gauge of metric perturbation amplitudes (\"RWZ\", \"Lorenz\",  \"Undefined\", \"Invariant\", or Null)",
	"SourceExpansion"->"States to what extent source terms should be expanded, (\"Full\", \"Partial\", or \"None\")",
	"SourceIndices"->"Position of indices on source terms, (\"Up\" or \"Down\"",
	"Homogeneous"->"Boolean stating whether to include source terms",
	"Invariant"->"Boolean stating whether to return the field equations for the gauge invariant metric perturbation amplitudes"
};
Options[invariantFieldEquations]=Join[FilterRules[Options[FieldEquations],{"Parity","Homogeneous","SourceExpansion","SourceIndices","Weak"}],{"Gauge"->"Undefined"}];
Options[rwzFieldEquations]=FilterRules[Options[FieldEquations],{"Parity","Homogeneous","SourceExpansion","SourceIndices","Mode","Weak"}];
Options[lorenzFieldEquations]=Options[rwzFieldEquations];

Options[MasterEquationPotential] = FilterRules[Options[MasterFunctionSymbol],{"Parity","Variable"}];
DocumentationBuilder`OptionDescriptions["MasterEquationPotential"] = 
FilterRules[DocumentationBuilder`OptionDescriptions["MasterFunctionSymbol"],{"Variable","Parity"}];
Options[MasterEquation] = Join[FilterRules[Options[FieldEquations],{"Weak","Homogeneous","Gauge"}],
	FilterRules[Options[MasterFunctionSymbol],{"Parity","Variable"}],
	{"SourceExpansion"->"Partial","MPs"->False,"GFForm"->True}];
DocumentationBuilder`OptionDescriptions["MasterEquation"] = 
Join[FilterRules[DocumentationBuilder`OptionDescriptions["FieldEquations"],{"Weak","SourceExpansion","Homogeneous"}],
FilterRules[DocumentationBuilder`OptionDescriptions["MasterFunctionSymbol"],{"Variable","Parity"}],
{
	"Gauge" -> "Gauge of metric perturbation amplitudes (if master function is expanded with MPs->True), (\"RWZ\", \"Lorenz\",  or \"Undefined\")",
	"GFForm"->"Boolean stating whether source term should be written in terms of G and F coefficents of the Dirac delta distribution and its first derivative",
	"MPs"->"Boolean stating whether to expand the master function in terms of metric perturbation amplitudes"
}];

Options[RemoveMasterFunctionRDerivatives]=Join[{"FD"->False},FilterRules[Options[MasterEquation],{"SourceExpansion","Parity","Variable","Homogeneous"}]]
DocumentationBuilder`OptionDescriptions["RemoveMasterFunctionRDerivatives"]=
Join[{
	"Homogeneous"->"Boolean stating whether to remove higher r derivatives using the homogeneous master equation",
	"FD"->"Boolean stating whether to remove higher r derivatives using the frequency domain master equation"},
	FilterRules[DocumentationBuilder`OptionDescriptions["MasterEquation"],{"SourceExpansion","Parity","Variable"}]
];
Options[SourceConservation] = Join[FilterRules[Options[FieldEquations],{"SourceExpansion","Parity"}],{"Indices"->"Down"}];
DocumentationBuilder`OptionDescriptions["SourceConservation"] = Join[FilterRules[DocumentationBuilder`OptionDescriptions["FieldEquations"],{"SourceExpansion","Parity"}],
{"Indices"->"Specifies whether the source terms in the Bianchi (conservation) identites should have indices \"Up\" or \"Down\""}];

Options[lorenzGaugeConditions]={"Parity"->"Both","Weak"->False,"Mode"->"Radiative"};
Options[rwzGaugeConditions]=Options[lorenzGaugeConditions];
Options[GaugeConditions]=Join[Options[lorenzGaugeConditions],{"Gauge"->"Lorenz"}];
DocumentationBuilder`OptionDescriptions["GaugeConditions"]=
Join[FilterRules[DocumentationBuilder`OptionDescriptions["FieldEquations"],{"Parity","Weak","Mode"}],
{"Gauge"->"\"Lorenz\" or \"RWZ\""}];
Options[GaugeTransformationEquations] = {"Parity"->"Both","Homogeneous"->False,"Weak"->False,"Reconstruct"->False,"SourceExpansion"->"None"};
DocumentationBuilder`OptionDescriptions["GaugeTransformationEquations"]=
Join[FilterRules[DocumentationBuilder`OptionDescriptions["FieldEquations"],{"Parity","Weak","Homogeneous","SourceExpansion"}],
{"Reconstruct"->"Boolean stating whether to write metric perturbation amplitudes in terms of the master function"}];
Options[GaugeTransformationSources] = {"Parity"->"Even","Weak"->False,"Reconstruct"->False,"SourceExpansion"->"None"};
DocumentationBuilder`OptionDescriptions["GaugeTransformationSources"]=
Join[FilterRules[DocumentationBuilder`OptionDescriptions["FieldEquations"],{"Parity","Weak","SourceExpansion"}],
{"Reconstruct"->"Boolean stating whether to write metric perturbation amplitudes in terms of the master function"}];


zmMax=7;
cpmMax=6;
jtMax=5;
zmVars=Join[{"ZM"},"ZM"<>ToString[#]&/@Range[0,zmMax]];
cpmVars=Join[{"CPM"},"CPM"<>ToString[#]&/@Range[0,cpmMax]];
jtVars=Join[{"JT"},"JT"<>ToString[#]&/@Range[0,jtMax]];
masterVars=Join[{Default},zmVars,cpmVars,jtVars];


Options[MasterFunctionAsTeukolskyFunction]={"Parity"->"Even"};
DocumentationBuilder`OptionDescriptions["MasterFunctionAsTeukolskyFunction"]=
{"Parity"->"Specifies the parity of the master function, (\"Even\" or \"Odd\""};


parityBothQ[x_]:=MemberQ[{"Even","Odd","Both"},x];
parityQ[x_]:=MemberQ[{"Even","Odd"},x];
gaugeQ[x_]:=MemberQ[{"Undefined","RWZ","Lorenz",Null},x];
modeQ[x_]:=MemberQ[{"Radiative","Monopole","Dipole"},x];
sourceExpQ[x_]:=MemberQ[{"None","Partial","Full"},x]
srcIndsQ[x_]:=MemberQ[{"Up","Down"},x]


def@
FieldEquations[syms_Association,opts:OptionsPattern[]]:=
Module[{optionsRules,gauge,inv,mode},
	optionsRules={"Parity" -> parityBothQ,
					"Homogeneous" -> BooleanQ,
					"Weak" -> BooleanQ,
					"SourceExpansion" -> sourceExpQ,
					"Gauge"->gaugeQ,
					"Mode"->modeQ,
					"Invariant"->BooleanQ,
					"SourceIndices"->srcIndsQ};

	TestOptions[optionsRules,{opts}];
	gauge=OptionValue[Gauge];
	inv=OptionValue[Invariant];
	mode=OptionValue[Mode];
	
	If[inv||gauge==="Undefined"||gauge===Null,
		If[mode==="Radiative",
			invariantFieldEquations[syms,FilterRules[{opts},Options@invariantFieldEquations]],
			Print["Field equations for gauge invariants only exist for Radiative modes"];
			Aborting[syms]
		]
		,
		Switch[gauge,
			"RWZ",
			rwzFieldEquations[syms,FilterRules[{opts},Options@rwzFieldEquations]],
			"Lorenz",
			lorenzFieldEquations[syms,FilterRules[{opts},Options@lorenzFieldEquations]],
			___,
			Print["No field equations available for ", gauge, " gauge."];
			Aborting[syms]
		]
	]
]
reDef@
FieldEquations[opts:OptionsPattern[]]:=FieldEquations[DefaultSymbols[],opts]


Clear[lorenzFieldEquations]
lorenzFieldEquations[syms_Association,opts:OptionsPattern[]] :=
Module[{mpOpts,htt, htr, hrr,jt,jr, KK,G,ht,hr,h2,mode,mp,parity,homog,se,
		eqHtt,eqHtr,eqHrr,eqJt,eqJr,eqK,eqG,eqHt,eqHr,eqH2,box,srcOpts,srcInds,
		t,r,M,la,rp,eqList,optionsRules,newOpts,modeMod,eqFieldList,
		Qtt,Qtr,Qrr,Qt,Qr,QFlat,QSharp,Pt,Pr,P},

		optionsRules={"Parity" -> parityBothQ,
						"Homogeneous" -> BooleanQ,
						"Weak" -> BooleanQ,
						"SourceExpansion" -> sourceExpQ,
						"Mode"->modeQ,
						"SourceIndices"->srcIndsQ};

		TestOptions[optionsRules,{opts},lorenzFieldEquations];
		mode=OptionValue["Mode"];
		parity=OptionValue[Parity];
		homog=OptionValue["Homogeneous"];
		se=OptionValue["SourceExpansion"];
		srcInds=OptionValue["SourceIndices"];

		If[parity==="Both",
			newOpts=FilterRules[{opts},{"Homogeneous","SourceExpansion","SourceIndices","Weak","Mode"}];
			Return@Join[lorenzFieldEquations[syms,newOpts,Parity->"Even"],lorenzFieldEquations[syms,newOpts,Parity->"Odd"]]
		];

		t = TSymbol[syms];
		r = RSymbol[syms];
		rp = RpSymbol[syms];
		M = BlackHoleMassSymbol[syms];
		la= LambdaSymbol[syms];

		modeMod[x_]:=Switch[mode,
						"Monopole",
						If[parity==="Even",x[[{1,2,3,6}]]/.la->LambdaOfL[0],{}],
						"Dipole",
						Most@x/.la->LambdaOfL[1],
						"Radiative",x];

		mpOpts=Join[FilterRules[{opts},{ "Weak","Mode"}],{"Gauge"->"Lorenz"}];
		mp[label_]:=If[MemberQ[MetricPerturbationLables[Mode->mode,"Gauge"->"Lorenz"],label],AmplitudeFunction[label][syms,mpOpts],0];
		box[field_]:=-D[field,t,t]+f[r,M]D[f[r,M]D[field,r],r];

		eqList=
		If[parity==="Even",
			
			eqFieldList={eqHtt,eqHtr,eqHrr,eqJt,eqJr,eqK,eqG};
			htt=mp[HttLabel[]];
			htr=mp[HtrLabel[]];
			hrr=mp[HrrLabel[]];
			jt=mp[JtLabel[]];
			jr=mp[JrLabel[]];
			KK=mp[KLabel[]];
			G=mp[GLabel[]];

			eqHtt["LHS"] =box[htt]+(2(r-4M)f[r,M])/r^2 D[htt,r]+(4M f[r,M])/r^2 D[htr,t]+(2M(3M-2r)f[r,M]^2)/r^4 hrr+(4M f[r,M]^2)/r^3 KK+(2(M^2-r^2 f[r,M])-2la r^2 f[r,M])/r^4 htt;
			eqHtr["LHS"] =box[htr]+(2f[r,M]^2)/r D[htr,r]+(2M f[r,M])/r^2 D[hrr,t]+(2M)/(r^2 f[r,M]) D[htt,t]+(4(la+1) f[r,M])/r^3 jt-(4(M-r)^2+2la r^2 f[r,M])/r^4 htr;
			eqHrr["LHS"] = box[hrr]+(2f[r,M])/r D[hrr,r]+(4M )/(r^2 f[r,M]) D[htr,t]+(2M(3M-2r))/(r^4 f[r,M]^2) htt+(4(r-3M))/r^3 KK+(8(la+1) f[r,M])/r^3 jr+(2(r-M)(7M-3r)-2la r^2 f[r,M])/r^4 hrr;
			eqJt["LHS"] =box[jt]-(2M f[r,M])/r^2 D[jt,r]+(2M f[r,M])/r^2 D[jr,t]+(2f[r,M]^2)/r htr-(2 f[r,M]^2+2la f[r,M])/r^2 jt;
			eqJr["LHS"] =box[jr]+(2M f[r,M])/r^2 D[jr,r]+(2M )/(r^2 f[r,M]) D[jt,t]+(2f[r,M]^2)/r hrr-(2f[r,M])/r KK+(2la f[r,M])/r G-(4 f[r,M]^2+2(la+1) f[r,M])/r^2 jr;
			eqK["LHS"]=box[KK]+(2f[r,M]^2)/r D[KK,r]-(2(3M-r)f[r,M]^2)/r^3 hrr+(2M)/r^3 htt-(4(la+1) f[r,M]^2)/r^3 jr-(4 f[r,M]^2+2la f[r,M])/r^2 KK;
			eqG["LHS"] =box[G]+(2f[r,M]^2)/r D[G,r]+(4 f[r,M]^2)/r^3 jr-(2la f[r,M])/r^2 G;

	
			If[homog,
					Scan[(#["RHS"]=0)&,eqFieldList];,
					
					If[srcInds === "Down",
						srcOpts= Sequence[SourceExpansion->se,Indices->"Down",Capital->True];
						Qtt = 1/f[rp[t],M]^2 QttSource[syms,srcOpts];
						Qtr = -QtrSource[syms,srcOpts];
						Qrr = f[rp[t],M]^2 QrrSource[syms,srcOpts];
						Qt = -1/f[rp[t],M] QtSource[syms,srcOpts];
						Qr = f[rp[t],M] QrSource[syms,srcOpts];
						QFlat = QFlatSource[syms,srcOpts];
						QSharp = QSharpSource[syms,srcOpts],

						srcOpts= Sequence[SourceExpansion->se,Indices->"Up",Capital->True];
						Qtt = QttSource[syms,srcOpts];
						Qtr = QtrSource[syms,srcOpts];
						Qrr = QrrSource[syms,srcOpts];
						Qt = QtSource[syms,srcOpts];
						Qr = QrSource[syms,srcOpts];
						QFlat = QFlatSource[syms,srcOpts];
						QSharp = QSharpSource[syms,srcOpts]
					];

					eqHtt["RHS"] = -f[rp[t],M]Qrr-f[rp[t],M]^2 QFlat-f[rp[t],M]^3 Qtt;
					eqHtr["RHS"] = 2f[rp[t],M]Qtr;
					eqHrr["RHS"] = QFlat-1/f[rp[t],M] Qrr-f[rp[t],M]Qtt;
					eqJt["RHS"] = f[rp[t],M]^2 Qt;
					eqJr["RHS"] = -Qr;
					eqK["RHS"] = Qrr-f[rp[t],M]^2 Qtt;
					eqG["RHS"] = -f[rp[t],M]/r^2 QSharp;
				];

			eqFieldList
			,
		
			eqFieldList={eqHt,eqHr,eqH2};
			ht=mp[HtLabel[]];
			hr=mp[HrLabel[]];
			h2=mp[H2Label[]];

			eqHt["LHS"] = box[ht]-(2M f[r,M])/r^2 D[ht,r]+(2M f[r,M])/r^2 D[hr,t]-(2f[r,M]^2+2la f[r,M])/r^2 ht;
			eqHr["LHS"] = box[hr]+(2M f[r,M])/r^2 D[hr,r]+(2M )/(r^2 f[r,M]) D[ht,t]+(2la f[r,M])/r^3 h2+(2(4M-3r)f[r,M]-2la r f[r,M])/r^3 hr;
			eqH2["LHS"] = box[h2]-(2 f[r,M]^2)/r D[h2,r]+(4 f[r,M]^2)/r hr+(2f[r,M](r-4M)-2la r f[r,M])/r^3 h2;
	
			If[homog,
						
				Scan[(#["RHS"]=0)&,eqFieldList];,
					
					If[srcInds === "Down",
						srcOpts= Sequence[SourceExpansion->se,Indices->"Down",Capital->True];
						Pt = - 1/f[rp[t],M] PtSource[syms,srcOpts];
						Pr = f[rp[t],M] PrSource[syms,srcOpts];
						P = PSource[syms,srcOpts],

						srcOpts= Sequence[SourceExpansion->se,Indices->"Up",Capital->True];
						Pt = PtSource[syms,srcOpts];
						Pr = PrSource[syms,srcOpts];
						P = PSource[syms,srcOpts];
					];

					eqHt["RHS"] = f[rp[t],M]^2 Pt;
					eqHr["RHS"] =-Pr;
					eqH2["RHS"] = -2f[rp[t],M]P;
				];

			eqFieldList
		];

	modeMod[#["LHS"]-#["RHS"]&/@eqList]
	
]


Clear[rwzFieldEquations]
rwzFieldEquations[syms_Association,opts:OptionsPattern[]] :=
Module[{mpOpts,htt, htr, hrr, KK,ht,hr,mode,mp,parity,homog,se,srcOpts,
		eqQtt,eqQtr,eqQrr,eqQt,eqQr,eqQFlat,eqQSharp,eqPt,eqPr,eqP,srcInds,
		t,r,M,la,rp,eqList,optionsRules,newOpts,modeMod,eqSourceList},

		optionsRules={"Parity" -> parityBothQ,
						"Homogeneous" -> BooleanQ,
						"Weak" -> BooleanQ,
						"SourceExpansion" -> sourceExpQ,
						"Mode"->modeQ,
						"SourceIndices"->srcIndsQ};

		TestOptions[optionsRules,{opts},rwzFieldEquations];
		mode=OptionValue["Mode"];
		parity=OptionValue[Parity];
		homog=OptionValue["Homogeneous"];
		se=OptionValue["SourceExpansion"];
		srcInds=OptionValue["SourceIndices"];

		If[parity==="Both",
			newOpts=FilterRules[{opts},{ "Homogeneous","SourceExpansion","SourceIndices","Weak","Mode"}];
			Return@Join[rwzFieldEquations[syms,newOpts,Parity->"Even"],rwzFieldEquations[syms,newOpts,Parity->"Odd"]]
		];

		t = TSymbol[syms];
		r = RSymbol[syms];
		rp = RpSymbol[syms];
		M = BlackHoleMassSymbol[syms];
		la= LambdaSymbol[syms];

		modeMod[x_]:=Switch[mode,
						"Monopole",
						If[parity==="Even",x[[{1,2,3,6}]]/.la->LambdaOfL[0],{}],
						"Dipole",
						Most@x/.la->LambdaOfL[1],
						"Radiative",x];

		mpOpts=Join[FilterRules[{opts},{ "Weak","Mode"}],{"Gauge"->"RWZ"}];
		mp[label_]:=If[MemberQ[MetricPerturbationLables[Mode->mode,"Gauge"->"RWZ"],label],AmplitudeFunction[label][syms,mpOpts],0];

		eqList=
		If[parity==="Even",
			
			eqSourceList={eqQtt,eqQtr,eqQrr,eqQt,eqQr,eqQFlat,eqQSharp};
			htt=mp[HttLabel[]];
			htr=mp[HtrLabel[]];
			hrr=mp[HrrLabel[]];
			KK=mp[KLabel[]];

			eqQtt["LHS"] =- D[KK, r, r] - (3 r - 5 M)/(r^2 f[r,M]) D[KK, r] + f[r,M]/r D[hrr, r] + ((la + 2) r + 2 M)/r^3 hrr + la/(r^2 f[r,M]) KK;
			eqQtr["LHS"] =D[KK, r, t] + (r - 3 M)/(r^2 f[r,M]) D[KK,t] -f[r,M]/r D[hrr,t] -(la + 1)/r^2 htr;
			eqQrr["LHS"] = - D[KK,t,t] + ((r - M) f[r,M])/r^2 D[KK, r] +(2 f[r,M])/r D[htr,t] -f[r,M]/r D[htt, r] +((la + 1) r + 2 M)/r^3 htt -f[r,M]^2/r^2 hrr -(la f[r,M])/r^2 KK;
			eqQt["LHS"] =D[hrr,t] -D[htr, r] +1/f[r,M] D[KK,t] -(2 M)/(r^2 f[r,M]) htr;
			eqQr["LHS"] =- D[htr,t] +D[htt, r] -f[r,M] D[KK, r] -(r - M)/(r^2 f[r,M]) htt +((r - M) f[r,M])/r^2 hrr;
			eqQFlat["LHS"]=- D[hrr,t,t] +2 D[htr, r,t] -D[htt, r, r] -1/f[r,M] D[KK,t,t] +f[r,M] D[KK, r, r] +(2 (r - M))/(r^2 f[r,M]) D[htr,t] -(r - 3 M)/(r^2 f[r,M]) D[htt, r] -
							((r - M) f[r,M])/r^2 D[hrr, r] +(2 (r - M))/r^2 D[KK, r] +((la + 1) r^2 - 2 (la + 2) M r + 2 M^2)/(r^4 f[r,M]^2) htt -((la + 1) r^2 - 2 la M r - 2 M^2)/r^4 hrr;
			eqQSharp["LHS"] =1/f[r,M] htt -f[r,M] hrr;

	
			If[homog,
					Scan[(#["RHS"]=0)&,eqSourceList];,

					If[srcInds === "Down",
						srcOpts = Sequence[SourceExpansion->se,Indices->"Down",Capital->True];
						eqQtt["RHS"] = 1/f[rp[t],M]^2 QttSource[syms,srcOpts];
						eqQtr["RHS"] = -QtrSource[syms,srcOpts];
						eqQrr["RHS"] = f[rp[t],M]^2 QrrSource[syms,srcOpts];
						eqQt["RHS"] = -1/f[rp[t],M] QtSource[syms,srcOpts];
						eqQr["RHS"] = f[rp[t],M] QrSource[syms,srcOpts];
						eqQFlat["RHS"] = QFlatSource[syms,srcOpts];
						eqQSharp["RHS"] = QSharpSource[syms,srcOpts],

						srcOpts = Sequence[SourceExpansion->se,Indices->"Up",Capital->True];
						eqQtt["RHS"] = QttSource[syms,srcOpts];
						eqQtr["RHS"] = QtrSource[syms,srcOpts];
						eqQrr["RHS"] = QrrSource[syms,srcOpts];
						eqQt["RHS"] = QtSource[syms,srcOpts];
						eqQr["RHS"] = QrSource[syms,srcOpts];
						eqQFlat["RHS"] = QFlatSource[syms,srcOpts];
						eqQSharp["RHS"] = QSharpSource[syms,srcOpts];
					];
				];

			eqSourceList
				,
		
			eqSourceList={eqPt,eqPr,eqP};
			ht=mp[HtLabel[]];
			hr=mp[HrLabel[]];

			eqPt["LHS"] = -D[hr, t, r] + D[ht, r, r] -2/r D[hr, t] - (2 (la + 1) r - 4 M)/(r^3 f[r,M]) ht;
			eqPr["LHS"] = D[hr, t, t] - D[ht, t, r] +2/r D[ht, t] + (2 la f[r,M])/r^2 hr;
			eqP["LHS"] = -(1/f[r,M]) D[ht, t] +f[r,M] D[hr, r] + (2 M)/r^2 hr;
	
			If[homog,
						
				Scan[(#["RHS"]=0)&,eqSourceList];,
	
					If[srcInds === "Down",
						srcOpts= Sequence[SourceExpansion->se,Indices->"Down",Capital->True];
						eqPt["RHS"] = - 1/f[rp[t],M] PtSource[syms,srcOpts];
						eqPr["RHS"] = f[rp[t],M] PrSource[syms,srcOpts];
						eqP["RHS"] = PSource[syms,srcOpts],

						srcOpts = Sequence[SourceExpansion->se,Indices->"Up",Capital->True];
						eqPt["RHS"] = PtSource[syms,srcOpts];
						eqPr["RHS"] = PrSource[syms,srcOpts];
						eqP["RHS"] = PSource[syms,srcOpts];
					];

				];

		eqSourceList
		];

	modeMod[#["LHS"]-#["RHS"]&/@eqList]
	
]


Clear[invariantFieldEquations]
invariantFieldEquations[syms_Association,opts:OptionsPattern[]] :=
Module[{mpOpts,htt, htr, hrr, KK,ht,hr,parity,homog,se,srcOpts,srcInds,
		eqQtt,eqQtr,eqQrr,eqQt,eqQr,eqQFlat,eqQSharp,eqPt,eqPr,eqP,
		t,r,M,la,rp,eqList,optionsRules,eqSourceList,newOpts},

		optionsRules={"Parity" -> parityBothQ,
						"Homogeneous" -> BooleanQ,
						"Weak" -> BooleanQ,
						"SourceExpansion" -> sourceExpQ,
						"Gauge"->gaugeQ,
						"SourceIndices"->srcIndsQ};

		TestOptions[optionsRules,{opts},invariantFieldEquations];
		parity=OptionValue[Parity];
		homog=OptionValue[Homogeneous];
		se=OptionValue[SourceExpansion];
		srcInds=OptionValue["SourceIndices"];

		If[parity==="Both",
			newOpts=FilterRules[{opts},{ "Homogeneous","SourceExpansion","SourceIndices","Weak","Gauge"}];
			Return@Join[invariantFieldEquations[syms,newOpts,Parity->"Even"],invariantFieldEquations[syms,newOpts,Parity->"Odd"]]
		];

		t = TSymbol[syms];
		r = RSymbol[syms];
		rp = RpSymbol[syms];
		M = BlackHoleMassSymbol[syms];
		la= LambdaSymbol[syms];

		mpOpts=FilterRules[{opts},{ "Weak","Gauge"}];

		eqList=
		If[parity==="Even",
			
			eqSourceList={eqQtt,eqQtr,eqQrr,eqQt,eqQr,eqQFlat,eqQSharp};
			htt=HttInvariantAmplitude[syms,mpOpts];
				htr=HtrInvariantAmplitude[syms,mpOpts];
				hrr=HrrInvariantAmplitude[syms,mpOpts];
				KK=KInvariantAmplitude[syms,mpOpts];

			eqQtt["LHS"] =- D[KK, r, r] - (3 r - 5 M)/(r^2 f[r,M]) D[KK, r] + f[r,M]/r D[hrr, r] + ((la + 2) r + 2 M)/r^3 hrr + la/(r^2 f[r,M]) KK;
			eqQtr["LHS"] =D[KK, r, t] + (r - 3 M)/(r^2 f[r,M]) D[KK,t] -f[r,M]/r D[hrr,t] -(la + 1)/r^2 htr;
			eqQrr["LHS"] = - D[KK,t,t] + ((r - M) f[r,M])/r^2 D[KK, r] +(2 f[r,M])/r D[htr,t] -f[r,M]/r D[htt, r] +((la + 1) r + 2 M)/r^3 htt -f[r,M]^2/r^2 hrr -(la f[r,M])/r^2 KK;
			eqQt["LHS"] =D[hrr,t] -D[htr, r] +1/f[r,M] D[KK,t] -(2 M)/(r^2 f[r,M]) htr;
			eqQr["LHS"] =- D[htr,t] +D[htt, r] -f[r,M] D[KK, r] -(r - M)/(r^2 f[r,M]) htt +((r - M) f[r,M])/r^2 hrr;
			eqQFlat["LHS"]=- D[hrr,t,t] +2 D[htr, r,t] -D[htt, r, r] -1/f[r,M] D[KK,t,t] +f[r,M] D[KK, r, r] +(2 (r - M))/(r^2 f[r,M]) D[htr,t] -(r - 3 M)/(r^2 f[r,M]) D[htt, r] -
							((r - M) f[r,M])/r^2 D[hrr, r] +(2 (r - M))/r^2 D[KK, r] +((la + 1) r^2 - 2 (la + 2) M r + 2 M^2)/(r^4 f[r,M]^2) htt -((la + 1) r^2 - 2 la M r - 2 M^2)/r^4 hrr;
			eqQSharp["LHS"] =1/f[r,M] htt -f[r,M] hrr;

	
			If[homog,
					Scan[(#["RHS"]=0)&,eqSourceList];,

					If[srcInds === "Down",
						srcOpts= Sequence[SourceExpansion->se,Indices->"Down",Capital->True];
						eqQtt["RHS"] = 1/f[rp[t],M]^2 QttSource[syms,srcOpts];
						eqQtr["RHS"] = -QtrSource[syms,srcOpts];
						eqQrr["RHS"] = f[rp[t],M]^2 QrrSource[syms,srcOpts];
						eqQt["RHS"] = -1/f[rp[t],M] QtSource[syms,srcOpts];
						eqQr["RHS"] = f[rp[t],M] QrSource[syms,srcOpts];
						eqQFlat["RHS"] = QFlatSource[syms,srcOpts];
						eqQSharp["RHS"] = QSharpSource[syms,srcOpts],

						srcOpts = Sequence[SourceExpansion->se,Indices->"Up",Capital->True];
						eqQtt["RHS"] = QttSource[syms,srcOpts];
						eqQtr["RHS"] = QtrSource[syms,srcOpts];
						eqQrr["RHS"] = QrrSource[syms,srcOpts];
						eqQt["RHS"] = QtSource[syms,srcOpts];
						eqQr["RHS"] = QrSource[syms,srcOpts];
						eqQFlat["RHS"] = QFlatSource[syms,srcOpts];
						eqQSharp["RHS"] = QSharpSource[syms,srcOpts];
					];
				];

			eqSourceList
				,
		
			eqSourceList={eqPt,eqPr,eqP};
			ht=HtInvariantAmplitude[syms,mpOpts];
			hr=HrInvariantAmplitude[syms,mpOpts];

			eqPt["LHS"] = -D[hr, t, r] + D[ht, r, r] -2/r D[hr, t] - (2 (la + 1) r - 4 M)/(r^3 f[r,M]) ht;
			eqPr["LHS"] = D[hr, t, t] - D[ht, t, r] +2/r D[ht, t] + (2 la f[r,M])/r^2 hr;
			eqP["LHS"] = -(1/f[r,M]) D[ht, t] +f[r,M] D[hr, r] + (2 M)/r^2 hr;
	
			If[homog,
						
				Scan[(#["RHS"]=0)&,eqSourceList];,
	
					If[srcInds === "Down",
						srcOpts= Sequence[SourceExpansion->se,Indices->"Down",Capital->True];
						eqPt["RHS"] = - 1/f[rp[t],M] PtSource[syms,srcOpts];
						eqPr["RHS"] = f[rp[t],M] PrSource[syms,srcOpts];
						eqP["RHS"] = PSource[syms,srcOpts],
					
						srcOpts = Sequence[SourceExpansion->se,Indices->"Up",Capital->True];
						eqPt["RHS"] = PtSource[syms,srcOpts];
						eqPr["RHS"] = PrSource[syms,srcOpts];
						eqP["RHS"] = PSource[syms,srcOpts];
					];
				];
			eqSourceList
		];

	#["LHS"]-#["RHS"]&/@eqList
	
]


def@
MasterEquation[syms_Association,opts:OptionsPattern[]] :=
Module[{t, r, M, potential, Psi,varOpt,parOpt,parity,var,
        source, optionsRules, homog},

	optionsRules = {"Parity" -> Function[x,MemberQ[{Default,"Even","Odd"},x]],
					"Variable" -> Function[x,MemberQ[masterVars,x]],
					"Homogeneous"->BooleanQ,
					"Weak"->BooleanQ,
					"Gauge" -> Function[x, x==="Undefined" || x==="RWZ" || x==="Lorenz"],
					"MPs"->BooleanQ,
					"GFForm"->BooleanQ,
					"SourceExpansion" -> Function[x, x==="Partial" || x==="Full" || x==="None"]};
	TestOptions[optionsRules,{opts}];
	varOpt=OptionValue["Variable"];
	parOpt=OptionValue["Parity"];
	homog = OptionValue["Homogeneous"];

	{parity,var}=ParityAndVariable[parOpt,varOpt];

	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	Psi=MasterFunction[syms,FilterRules[{opts},Options@MasterFunction]];

	potential=MasterEquationPotential[syms,"Variable"->var];
	source=If[homog,0,MasterEquationSource[syms,FilterRules[{opts},Options@MasterEquationSource]]];

	-D[Psi,t,t] + f[r,M] D[f[r,M] D[Psi ,r],r] - potential Psi - source
]
reDef@
MasterEquation[opts:OptionsPattern[]]:=MasterEquation[DefaultSymbols[],opts]


def@
MasterEquationPotential[syms_Association,opts:OptionsPattern[]] := 
Module[{t, r, M, la,optionsRules,parity,var,parOpt,varOpt},

	optionsRules = {"Parity" -> Function[x,MemberQ[{Default,"Even","Odd"},x]],
					"Variable" -> Function[x,MemberQ[masterVars,x]]};
	TestOptions[optionsRules,{opts}];
	varOpt=OptionValue["Variable"];
	parOpt=OptionValue["Parity"];

	{parity,var}=ParityAndVariable[parOpt,varOpt];

	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaSymbol[syms];
		
	If[parity==="Even"&&Not[MemberQ[jtVars,var]],
		f[r,M]/(r^2 CapitalLambda[r,M,la]^2) (2la^2 (la+1+(3M)/r)+(18M^2)/r^2 (la+M/r)),
		f[r,M]/r^2 (2(la+1)-(6 M)/r)
	]
]
reDef@
MasterEquationPotential[opts:OptionsPattern[]]:=MasterEquationPotential[DefaultSymbols[],opts]


def@
RemoveMasterFunctionRDerivatives[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{optionsRules,psiSym,parity,se,fd,eqn,colTerm,
		t,r,rule,newLHS,m,n,homog,args,parOpt,varOpt,var},

	optionsRules = {"SourceExpansion"-> Function[x, x==="Full"|| x==="Partial"],
					"Parity" -> Function[x,MemberQ[{Default,"Even","Odd"},x]],
					"Variable" -> Function[x,MemberQ[masterVars,x]],
					"Homogeneous"->BooleanQ,
					"FD"->BooleanQ};

	TestOptions[optionsRules,{opts}];
	se=OptionValue[SourceExpansion];
	homog=OptionValue[Homogeneous];
	fd=OptionValue[FD];
	varOpt=OptionValue["Variable"];
	parOpt=OptionValue["Parity"];

	{parity,var}=ParityAndVariable[parOpt,varOpt];

	If[fd&&Not[homog],Print["Cannot form frequency domain source terms of the master equation. Set Homogeneous->True."];Aborting[syms]];

	r=RSymbol[syms];
	t=TSymbol[syms];
	args=Sequence@@If[fd,{r},{t,r}];

	psiSym=MasterFunction[syms,"Variable"->var]/.fn_[t,r]:>fn;
	eqn=MasterEquation[syms,SourceExpansion->se,"Variable"->var,Homogeneous->homog];
	rule=Solve[If[fd,ToFrequencyDomain[syms,eqn],eqn]==0,D[psiSym[args],r,r]][[1,1]];
	
	newLHS=rule[[1]]/.If[fd,Derivative[2][psiSym][r]->Derivative[n_?(#>=2&)][psiSym][r],Derivative[0,2][psiSym][t,r]->Derivative[m_,n_?(#>=2&)][psiSym][t,r]];
	colTerm = If[fd,_[r],_[t,r]];
	Collect[expr//.newLHS:>If[fd,D[rule[[2]],{r,n-2}],D[rule[[2]],{t,m},{r,n-2}]],colTerm,Simplify]
]
reDef@
RemoveMasterFunctionRDerivatives[expr_,opts:OptionsPattern[]]:=RemoveMasterFunctionRDerivatives[DefaultSymbols[],expr,opts]


def@
SourceConservation[syms_Association,opts:OptionsPattern[]] := 
Module[{t, r, M, la,ind,rp,se,srcOpts,
        Qtt, Qtr, Qrr, Qt, Qr, QSharp, QFlat, QttUp, QtrUp, QrrUp, QtUp, QrUp,
        Pt, Pr, P, PtUp, PrUp, optionsRules,bianchiIDs,parity,newOpts},

	optionsRules = {"Parity" -> parityBothQ,
					"SourceExpansion" -> Function[x, x==="None" || x==="Partial" || x==="Full"],
					"Indices"->Function[x, x==="Up" || x==="Down"]};

	TestOptions[optionsRules,{opts}];
	ind=OptionValue[Indices];
	se=OptionValue[SourceExpansion];
	parity=OptionValue[Parity];

	If[parity==="Both",
		newOpts=FilterRules[{opts},{"SourceExpansion","Indices"}];
		Return@Join[SourceConservation[syms,newOpts,Parity->"Even"],SourceConservation[syms,newOpts,Parity->"Odd"]]
	];

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaSymbol[syms];

	srcOpts= Sequence[SourceExpansion->se,Indices->ind];

	Switch[parity,
		"Even",
			
		Qtt[tt_,rr_]:=QttSource[syms,srcOpts] /. {t->tt,r->rr};
		Qtr[tt_,rr_]:=QtrSource[syms,srcOpts] /. {t->tt,r->rr};
		Qrr[tt_,rr_]:=QrrSource[syms,srcOpts] /. {t->tt,r->rr};
		Qt[tt_,rr_]:=QtSource[syms,srcOpts] /. {t->tt,r->rr};
		Qr[tt_,rr_]:=QrSource[syms,srcOpts] /. {t->tt,r->rr};
		QSharp[tt_,rr_]:=QSharpSource[syms,srcOpts] /. {t->tt,r->rr};
		QFlat[tt_,rr_]:=QFlatSource[syms,srcOpts] /. {t->tt,r->rr};

		If[ind==="Up",
			QttUp[t_,r_]:=Qtt[t,r];
			QtrUp[t_,r_]:=Qtr[t,r];
			QrrUp[t_,r_]:=Qrr[t,r];
			QtUp[t_,r_]:=Qt[t,r];
			QrUp[t_,r_]:=Qr[t,r]
			,
			QttUp[t_,r_]:=1/f[rp[t],M]^2 Qtt[t,r];
			QtrUp[t_,r_]:=-Qtr[t,r];
			QrrUp[t_,r_]:=f[rp[t],M]^2 Qrr[t,r];
			QtUp[t_,r_]:=-(1/f[rp[t],M])Qt[t,r];
			QrUp[t_,r_]:=f[rp[t],M]Qr[t,r]
		];

		bianchiIDs = Table[0, {3}];

		bianchiIDs[[1]] = D[QttUp[t,r],t]+D[QtrUp[t,r],r]+(2(r-M))/(r^2 f[r,M]) QtrUp[t,r]-(la+1)/r^2 QtUp[t,r];
		bianchiIDs[[2]] = D[QtrUp[t,r],t]+D[QrrUp[t,r],r]+(M f[r,M])/r^2 QttUp[t,r]
							+(2r-5M)/(r^2 f[r,M]) QrrUp[t,r]-(la+1)/r^2 QrUp[t,r]-f[r,M]/r QFlat[t,r];
		bianchiIDs[[3]] = D[QtUp[t,r],t]+D[QrUp[t,r],r]+2/r QrUp[t,r]+QFlat[t,r]-la/r^2 QSharp[t,r]
		,

		"Odd",

		Pt[tt_,rr_]:=PtSource[syms,srcOpts] /. {t->tt,r->rr};
		Pr[tt_,rr_]:=PrSource[syms,srcOpts] /. {t->tt,r->rr};
		P[tt_,rr_]:=PSource[syms,srcOpts] /. {t->tt,r->rr};

		If[ind==="Up",
			PtUp[t_,r_]:=Pt[t,r];
			PrUp[t_,r_]:=Pr[t,r]
			,
			PtUp[t_,r_]:=-(1/f[rp[t],M])Pt[t,r];
			PrUp[t_,r_]:=f[rp[t],M]Pr[t,r]
		];

		bianchiIDs = Table[0, {1}];
		bianchiIDs[[1]] = D[PtUp[t,r],t]+D[PrUp[t,r],r]+2/r PrUp[t,r]-(2la)/r^2 P[t,r]
	];

	Collect[bianchiIDs, {Derivative[___][_][___], _[t,r]},Simplify]
]
reDef@
SourceConservation[opts:OptionsPattern[]]:=SourceConservation[DefaultSymbols[],opts]


gaugeCondTests={"Parity" -> parityBothQ,"Weak"->BooleanQ,"Mode"->modeQ};


def@
GaugeConditions[syms_Association,opts:OptionsPattern[]]:=
Which[OptionValue[Gauge]==="Lorenz",
	lorenzGaugeConditions[syms,FilterRules[{opts},Options[lorenzGaugeConditions]]],
	OptionValue[Gauge]==="RWZ",
	rwzGaugeConditions[syms,FilterRules[{opts},Options[rwzGaugeConditions]]],
	True,
	Print["Only gauge conditions availbable are for \"Lorenz\" and \"RWZ\""];
	Aborting[syms];
]
reDef@
GaugeConditions[opts:OptionsPattern[]]:=GaugeConditions[DefaultSymbols[],opts]


def@
lorenzGaugeConditions[syms_Association,opts:OptionsPattern[]] := 
Module[{t, r, M, la,weak,parity,httL, htrL, hrrL, jtL, jrL, KKL, GL,
		htL, hrL, h2L, mpOpts,mode,modeMod, lorenzConds,cond},

	TestOptions[gaugeCondTests,{opts}];

	weak=OptionValue[Weak];
	parity=OptionValue[Parity];
	mode=OptionValue[Mode];

	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaSymbol[syms];
	
	mpOpts=Sequence[Gauge->"Lorenz",Weak->weak];
	
	If[parity==="Both",
		Return[Join[lorenzGaugeConditions[syms,Weak->weak,Mode->mode,Parity->"Even"],lorenzGaugeConditions[syms,Weak->weak,Mode->mode,Parity->"Odd"]]]
	];

	modeMod[x_]:=Switch[mode,
					"Monopole",Most@x/.la->LambdaOfL[0],
					"Dipole",x/.la->LambdaOfL[1],
					"Radiative",x];

	lorenzConds=
	If[parity==="Even",

		httL=HttAmplitude[syms,mpOpts];
		htrL=HtrAmplitude[syms,mpOpts];
		hrrL=HrrAmplitude[syms,mpOpts];
		jtL=If[MemberQ[MetricPerturbationLables[Mode->mode],JtLabel[]],JtAmplitude[syms,mpOpts],0];
		jrL=If[MemberQ[MetricPerturbationLables[Mode->mode],JrLabel[]],JrAmplitude[syms,mpOpts],0];
		KKL=KAmplitude[syms,mpOpts];
		GL= If[MemberQ[MetricPerturbationLables[Mode->mode],GLabel[]],GAmplitude[syms,mpOpts],0];

		cond[1] = -(1/(2 f[r,M]))D[httL,t]-f[r,M]/2 D[hrrL,t]+f[r,M]D[htrL,r]-D[KKL,t]+2/r^2 (r-M)htrL-(2(la+1))/r^2 jtL;
		cond[2] = 1/(2 f[r,M]) D[httL,r]+f[r,M]/2 D[hrrL,r]-1/f[r,M] D[htrL,t]+2/r^2 (r-M)hrrL-(2(la+1))/r^2 jrL-D[KKL,r]-2/r KKL;
		cond[3] = -(1/f[r,M])D[jtL,t]+f[r,M]D[jrL,r]+2/r^2 (r-M)jrL+1/(2 f[r,M]) httL-f[r,M]/2 hrrL-la GL;
		
		{cond[1],cond[2],cond[3]},

		htL=HtAmplitude[syms,mpOpts];
		hrL=HrAmplitude[syms,mpOpts];
		h2L=If[MemberQ[MetricPerturbationLables[Mode->mode],H2Label[]],H2Amplitude[syms,mpOpts],0];

		cond = -1/f[r,M] D[htL,t]+f[r,M]D[hrL,r]+(2M)/r^2 hrL+(2f[r,M])/r hrL-la/r^2 h2L;
		
		{cond}
	];

	Collect[modeMod[lorenzConds], {Derivative[___][_][___], _[t,r]},Simplify]
]


def@
rwzGaugeConditions[syms_Association,opts:OptionsPattern[]]:=
Module[{mpLabels,mode,parity,weak,t,r},

	TestOptions[gaugeCondTests,{opts}];

	parity=OptionValue["Parity"];
	weak=OptionValue["Weak"];
	mode=OptionValue["Mode"];
	t=TSymbol[syms];
	r=RSymbol[syms];
	
	mpLabels=Complement[MetricPerturbationLables[Mode->mode,Gauge->"Undefined",Parity->parity],MetricPerturbationLables[Mode->mode,Gauge->"RWZ",Parity->parity]];
	
	AmplitudeFunction[#][syms,Gauge->"RWZ",ReturnSymbol->True][t,r]&/@mpLabels

]


def@
GaugeTransformationEquations[syms_Association,opts:OptionsPattern[]] :=
Module[{t, r, M, la, newOpts,
        weak,parity,homog,
		xiO, xiE, xiEt, xiEr,
		optionsRules, xiEqs, xiEqsHomog, 
		xiEq},

	optionsRules = {"Parity" -> Function[x, x==="Even" || x==="Odd" || x==="Both"],
					"Homogeneous" -> BooleanQ,
					"Weak"->BooleanQ,
					"Reconstruct"->BooleanQ,
					"SourceExpansion"->Function[x, x==="Full" || x==="Partial" || x==="None"]};
	TestOptions[optionsRules,{opts}];

	weak=OptionValue[Weak];
	homog=OptionValue[Homogeneous];
	parity=OptionValue[Parity];

	If[parity==="Both",
		newOpts=FilterRules[{opts},{"Homogeneous","SourceExpansion","Weak","Reconstruct"}];
		Return@Join[GaugeTransformationEquations[syms,newOpts,Parity->"Even"],GaugeTransformationEquations[syms,newOpts,Parity->"Odd"]]
	];

	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	la=LambdaSymbol[syms];

	xiEqsHomog=
	If[parity==="Even",

		xiE[tt_,rr_]:=XiEvenAmplitude[syms,Weak->weak]/.{t->tt,r->rr};
		xiEt[tt_,rr_]:=XiEvenTAmplitude[syms,Weak->weak]/.{t->tt,r->rr};
		xiEr[tt_,rr_]:=XiEvenRAmplitude[syms,Weak->weak]/.{t->tt,r->rr};
	
		xiEq[1] = -(1/f[r,M])D[xiE[t,r],t,t]+D[f[r,M]D[xiE[t,r],r],r]-2/r^2 (la+1)xiE[t,r]+2 f[r,M]/r xiEr[t,r];
		xiEq[2] = -(1/f[r,M])D[xiEt[t,r],t,t]+f[r,M]D[xiEt[t,r],r,r]+2 f[r,M]/r D[xiEt[t,r],r]-2/r^2 (la+1)xiEt[t,r]+(2M)/r^2 D[xiEr[t,r],t];
		xiEq[3] = -(1/f[r,M])D[xiEr[t,r],t,t]+f[r,M]D[xiEr[t,r],r,r]+2/r D[xiEr[t,r],r]-(2 f[r,M])/r^2 xiEr[t,r]-2/r^2 (la+1)xiEr[t,r]
							+(2M)/(f[r,M]^2 r^2) D[xiEt[t,r],t]+4/r^3 (la+1)xiE[t,r];

		{xiEq[1],xiEq[2],xiEq[3]}
		,

		xiO[tt_,rr_]:=XiOddAmplitude[syms,Weak->weak]/.{t->tt,r->rr};

		xiEq[1] = -(1/f[r,M])D[xiO[t,r],t,t]+D[f[r,M]D[xiO[t,r],r],r]-2/r^2 (la+1)xiO[t,r];
		
		{xiEq[1]}
	];
	
	xiEqs=
	If[homog,
		xiEqsHomog,
		xiEqsHomog-GaugeTransformationSources[syms,FilterRules[{opts},Options[GaugeTransformationSources]]]
	];

	Collect[xiEqs, {Derivative[___][_][___], _[t,r]},Simplify]
]
reDef@
GaugeTransformationEquations[opts:OptionsPattern[]]:=GaugeTransformationEquations[DefaultSymbols[],opts]


def@
GaugeTransformationSources[syms_Association,opts:OptionsPattern[]] := 
Module[{t, r, M,se,parity,weak,
        htt, htr, hrr, KK,
		Qsharp,source,psi,
        hr, P, optionsRules, sources},

	optionsRules = {"Parity" -> Function[x, x==="Even" || x==="Odd"],
					"Weak"->BooleanQ,
					"Reconstruct"->BooleanQ,
					"SourceExpansion"->Function[x, x==="Full" || x==="Partial" || x==="None"]};
	TestOptions[optionsRules,{opts}];

	weak=OptionValue[Weak];
	psi=OptionValue[Reconstruct];
	parity=OptionValue[Parity];
	se=OptionValue[SourceExpansion];

	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];

	sources=
	If[parity==="Even",
		
		htt[tt_,rr_]:=HttAmplitude[syms,Gauge->"RWZ",Reconstruct->psi,Weak->weak,SourceExpansion->se]/.{t->tt,r->rr};
		htr[tt_,rr_]:=HtrAmplitude[syms,Gauge->"RWZ",Reconstruct->psi,Weak->weak,SourceExpansion->se]/.{t->tt,r->rr};
		hrr[tt_,rr_]:=HrrAmplitude[syms,Gauge->"RWZ",Reconstruct->psi,Weak->weak,SourceExpansion->se]/.{t->tt,r->rr};
		KK[tt_,rr_]:=KAmplitude[syms,Gauge->"RWZ",Reconstruct->psi,Weak->weak,SourceExpansion->se]/.{t->tt,r->rr};
		Qsharp[tt_,rr_]:=QSharpSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};

		source[1]=-(1/2)Qsharp[t,r];
		source[2]=-(1/f[r,M])D[htt[t,r],t]+f[r,M]D[htr[t,r],r]-D[KK[t,r],t]+2/r^2 (r-M)htr[t,r]
						+1/2 D[Qsharp[t,r],t];
		source[3]=-(1/f[r,M])D[htr[t,r],t]+f[r,M]D[hrr[t,r],r]-D[KK[t,r],r]+M/(f[r,M]^2 r^2) htt[t,r]+(2r-M)/r^2 hrr[t,r]-2/r KK[t,r]
						+1/2 D[Qsharp[t,r],r];
			
		{source[1],source[2],source[3]}
		,
		
		hr[tt_,rr_]:=HrAmplitude[syms,Gauge->"RWZ",Reconstruct->psi,Weak->weak,SourceExpansion->se]/.{t->tt,r->rr};
		P[tt_,rr_]:=PSource[syms,Capital->True,SourceExpansion->se]/.{t->tt,r->rr};

		source[1]=(2 f[r,M])/r hr[t,r]+P[t,r];
		
		{source[1]}	
	];

	Collect[sources, {Derivative[___][_][___], _[t,r]},Simplify]
]
reDef@
GaugeTransformationSources[opts:OptionsPattern[]]:=GaugeTransformationSources[DefaultSymbols[],opts]


def@
KerrToSchwarzschild[syms_Association,expr_]:=
Module[{a,la,laS},

	a=BlackHoleSpinSymbol[syms];
	laS=LambdaSpinSymbol[syms];
	la=LambdaSymbol[syms];
	
	expr/.{a->0,laS->2(la)}
]
reDef@
KerrToSchwarzschild[expr_]:=KerrToSchwarzschild[DefaultSymbols[],expr]


def@
MasterFunctionToTeukolskyFunction[syms_Association,expr_]:=
Module[{PsiOdd,PsiEven,RTeuk,oddRules,evenRules,oddAsTeuk,evenAsTeuk,r},
	r=RSymbol[syms];	
	RTeuk=TeukolskyFunctionSymbol[syms];
	PsiOdd=MasterFunctionSymbol[syms,Parity->"Odd"];
	PsiEven=MasterFunctionSymbol[syms,Parity->"Even"];
	oddAsTeuk=MasterFunctionAsTeukolskyFunction[syms,Parity->"Odd"];
	evenAsTeuk=MasterFunctionAsTeukolskyFunction[syms,Parity->"Even"];
	oddRules={PsiOdd[r]->oddAsTeuk,Derivative[n_][PsiOdd][r]:>D[oddAsTeuk,{r,n}]};
	evenRules={PsiEven[r]->evenAsTeuk,Derivative[n_][PsiEven][r]:>D[evenAsTeuk,{r,n}]};

	Collect[KerrToSchwarzschild[syms,RemoveTeukolskyFunctionRDerivatives[syms,expr/.Join[evenRules,oddRules]]],{RTeuk[r],Derivative[_][RTeuk][r]},Simplify]
]
reDef@
MasterFunctionToTeukolskyFunction[expr_]:=MasterFunctionToTeukolskyFunction[DefaultSymbols[],expr]


def@
MasterFunctionAsTeukolskyFunction[syms_Association,opts:OptionsPattern[]]:=
Module[{r,la,om,RTeuk,M,parity,optionsRules,del},
	optionsRules = {"Parity" -> Function[x,MemberQ[{"Even","Odd"},x]]};
  
	TestOptions[optionsRules,{opts}];
	parity = OptionValue[Parity];

	la=LambdaSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	RTeuk=TeukolskyFunctionSymbol[syms];
	r=RSymbol[syms];
	om=FrequencySymbol[syms];
	del=KerrToSchwarzschild[syms,DeltaTeukolsky[syms]];

	If[parity==="Even",
		(2 (3 M^2 (2 la-7 I om r)+la r^2 (3+la+4 I om r-om^2 r^2)-M r (2 la^2+la (9+9 I om r)+3 om r (-3 I+om r))) RTeuk[r])/((-2 M+r)^2 (3 M+la r))+((-6 M^2+2 la r^2 (1+I om r)+6 I M r (I la+om r)) RTeuk'[r])/((2 M-r) (3 M+la r)),

		(2 (12 M^2+M r (-12-2 la-9 I om r)+r^2 (3+la+4 I om r-om^2 r^2)) RTeuk[r])/(r (-2 M+r)^2)+(2 (3 M+r (-1-I om r)) RTeuk'[r])/(-2 M+r)
	]
]
reDef@
MasterFunctionAsTeukolskyFunction[opts:OptionsPattern[]]:=MasterFunctionAsTeukolskyFunction[DefaultSymbols[],opts]


reDef@
MasterFunctionAsTeukolskyFunction[syms_Association,nDs_Integer/;nDs>=0,opts:OptionsPattern[]]:=
Module[{r,optionsRules,parity,psi},
	optionsRules = {"Parity" -> Function[x,MemberQ[{"Even","Odd"},x]]};
  
	TestOptions[optionsRules,{opts}];
	parity = OptionValue[Parity];

	r=RSymbol[syms];
	psi=MasterFunctionSymbol[syms,Parity->parity];

	MasterFunctionToTeukolskyFunction[syms,D[psi[r],{r,nDs}]]
]
reDef@
MasterFunctionAsTeukolskyFunction[nDs_Integer/;nDs>=0,opts:OptionsPattern[]]:=MasterFunctionAsTeukolskyFunction[DefaultSymbols[],nDs,opts]


def@
EvenMasterFunctionAsOdd[syms_Association]:=
Module[{r,om,psiO,la,M},

	M=BlackHoleMassSymbol[syms];
	psiO=MasterFunctionSymbol[syms,Parity->"Odd"];
	r=RSymbol[syms];
	om=FrequencySymbol[syms];
	la=LambdaSymbol[syms];
	
	Collect[(la (1+la)+18M^2 f[r,M]/(2r(la r + 3M)))psiO[r] + 3M f[r,M]D[psiO[r],r],{psiO[r],Derivative[_][psiO][r]},Simplify]
]
reDef@
EvenMasterFunctionAsOdd[]:=EvenMasterFunctionAsOdd[DefaultSymbols[]]


reDef@
EvenMasterFunctionAsOdd[syms_Association,pm_]:=
Module[{r,om,psiO,la,M,pmN},

	If[pm=!="Plus"&&pm=!="Minus",Print["Master function must be chosen to be \"Plus\" or \"Minus\"."];Aborting[syms]];
	pmN=If[pm==="Plus",1,-1];

	M=BlackHoleMassSymbol[syms];
	psiO=MasterFunctionSymbol[syms,Parity->"Odd"];
	r=RSymbol[syms];
	om=FrequencySymbol[syms];
	la=LambdaSymbol[syms];
	
	Collect[1/(4 la (1+la)+ pmN 12 I om M) ((4 la (1+la)+72M^2 f[r,M]/(2r(la r + 3M)))psiO[r]+12M f[r,M]D[psiO[r],r]),{psiO[r],Derivative[_][psiO][r]},Simplify]
]
reDef@
EvenMasterFunctionAsOdd[pm_]:=EvenMasterFunctionAsOdd[DefaultSymbols[],pm]


End[];

EndPackage[];
