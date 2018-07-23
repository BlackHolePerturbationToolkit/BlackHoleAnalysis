(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`Labels`",
				{"BlackHoleAnalysis`Utils`",
				"BlackHoleAnalysis`ValidityTests`",
				"BlackHoleAnalysis`OverloadedSymbols`"}];


MetricPerturbationLables::usage="MetricPerturbationLables[] returns a list of strings that label the metric perturbation amplitudes.";
MetricPerturbationSourceLables::usage="MetricPerturbationSourceLables[] returns a list of strings that label the metric perturbation amplitude source terms.";
GaugeVectorLabels::usage="GaugeVectorLabels[] returns a list of strings that label the gauge vector amplitudes.";
SphericalHarmonicLabels::usage="SphericalHarmonicLabels[] returns a list of strings that label the scalar, vector, and tensor spherical harmonics.";

MasterFunctionLabel::usage="MasterFunctionLabel[] returns the string treated as a label for the master function Psi";

HttLabel::usage="HttLabel[] returns the string treated as a label for the metric perturbation amplitude htt";
HtrLabel::usage="HtrLabel[] returns the string treated as a label for the metric perturbation amplitude htr";
HrrLabel::usage="HrrLabel[] returns the string treated as a label for the metric perturbation amplitude hrr";
JtLabel::usage="JtLabel[] returns the string treated as a label for the metric perturbation amplitude jt";
JrLabel::usage="JrLabel[] returns the string treated as a label for the metric perturbation amplitude jr";
KLabel::usage="KLabel[] returns the string treated as a label for the metric perturbation amplitude K";
GLabel::usage="GLabel[] returns the string treated as a label for the metric perturbation amplitude G";
HtLabel::usage="HtLabel[] returns the string treated as a label for the metric perturbation amplitude ht";
HrLabel::usage="HrLabel[] returns the string treated as a label for the metric perturbation amplitude hr";
H2Label::usage="H2Label[] returns the string treated as a label for the metric perturbation amplitude h2";

QttLabel::usage="QttLabel[] returns the string treated as a label for the metric perturbation amplitude source term Qtt";
QtrLabel::usage="QtrLabel[] returns the string treated as a label for the metric perturbation amplitude source term Qtr";
QrrLabel::usage="QrrLabel[] returns the string treated as a label for the metric perturbation amplitude source term Qrr";
QtLabel::usage="QtLabel[] returns the string treated as a label for the metric perturbation amplitude source term Qt";
QrLabel::usage="QrLabel[] returns the string treated as a label for the metric perturbation amplitude source term Qr";
QSharpLabel::usage="QSharpLabel[] returns the string treated as a label for the metric perturbation amplitude source term QSharp";
QFlatLabel::usage="QFlatLabel[] returns the string treated as a label for the metric perturbation amplitude source term QFlat";
PtLabel::usage="PtLabel[] returns the string treated as a label for the metric perturbation amplitude source term Pt";
PrLabel::usage="PrLabel[] returns the string treated as a label for the metric perturbation amplitude source term Pr";
PLabel::usage="PLabel[] returns the string treated as a label for the metric perturbation amplitude source term P";

XiEvenLabel::usage="XiEvenLabel[] returns the string treated as a label for the gauge vector amplitude xiEven";
XiEvenTLabel::usage="XiEvenTLabel[] returns the string treated as a label for the gauge vector amplitude xiEvenT";
XiEvenRLabel::usage="XiEvenRLabel[] returns the string treated as a label for the gauge vector amplitude xiEvenR";
XiOddLabel::usage="XiOddLabel[] returns the string treated as a label for the gauge vector amplitude xiOdd";

YLabel::usage="YLabel[] returns the string treated as a label for the scalar spherical harmonics Y_lm";
YThetaLabel::usage="YThetaLabel[] returns the string treated as a label for the vector spherical harmonics Y_lm_Theta";
YPhiLabel::usage="YPhiLabel[] returns the string treated as a label for the vector spherical harmonics Y_lm_Phi";
YThetaThetaLabel::usage="YThetaThetaLabel[] returns the string treated as a label for the tensor spherical harmonics Y_lm_ThetaTheta";
YThetaPhiLabel::usage="YThetaPhiLabel[] returns the string treated as a label for the tensor spherical harmonics Y_lm_ThetaPhi";
YPhiPhiLabel::usage="YPhiPhiLabel[] returns the string treated as a label for the tensor spherical harmonics Y_lm_PhiPhi";
XThetaLabel::usage="XThetaLabel[] returns the string treated as a label for the vector spherical harmonics X_lm_Theta";
XPhiLabel::usage="XPhiLabel[] returns the string treated as a label for the vector spherical harmonics X_lm_Phi";
XThetaThetaLabel::usage="XThetaThetaLabel[] returns the string treated as a label for the tensor spherical harmonics X_lm_ThetaTheta";
XThetaPhiLabel::usage="XThetaPhiLabel[] returns the string treated as a label for the tensor spherical harmonics X_lm_ThetaPhi";
XPhiPhiLabel::usage="XPhiPhiLabel[] returns the string treated as a label for the tensor spherical harmonics X_lm_PhiPhi";

(*ChiLabel;
PeriapsisLabel;
ApoapsisLabel;
SemiLatusRectumLabel;
OrbitalEccentricityLabel;
SpecificEnergyLabel;
SpecificAngularMomentumLabel;
BlackHoleMassLabel;
ParticleMassLabel;*)


Begin["`Private`"];


(*              *)
(* Mode numbers *)
(*              *)


Options[MetricPerturbationLables]={"Parity"->"Both","Mode"->"Radiative","Gauge"->"Lorenz"};
DocumentationBuilder`OptionDescriptions["MetricPerturbationLables"] = 
{
    "Parity" -> "Specifies which parities to include, (\"Even\", \"Odd\", or \"Both\")",
	"Mode" -> "l mode, restricting labels based on which spherical harmonic amplitudes are defined, (\"Radiative\", \"Dipole\", or \"Monopole\")",
	"Gauge" -> "Gauge, restricting labels based on which spherical harmonic amplitudes are chosen to be non-zero (\"RWZ\", \"ModRWZ\", \"Lorenz\", \"Undefined\",  \"Invariant\", or Null)"
};
Options[MetricPerturbationSourceLables]=Options[MetricPerturbationLables];
DocumentationBuilder`OptionDescriptions["MetricPerturbationSourceLables"] = DocumentationBuilder`OptionDescriptions["MetricPerturbationLables"];


Options[SphericalHarmonicLabels]={"Parity"->"Both"};
DocumentationBuilder`OptionDescriptions["SphericalHarmonicLabels"] = 
{
    "Parity" -> "Specifies which parities to include, (\"Even\", \"Odd\", or \"Both\")"
};


parityListQ[x_]:=MemberQ[{"Even","Odd","Both"},x];
modeQ[x_]:=MemberQ[{"Monopole","Dipole","Radiative"},x];
gaugeQ[x_]:=MemberQ[{"RWZ","ModRWZ","Lorenz","Undefined","Invariant",Null},x];


reDef@
Parity[mp_String]:=
Which[
	MemberQ[MetricPerturbationLables[Parity->"Even"],mp],"Even",
	MemberQ[MetricPerturbationLables[Parity->"Odd"],mp],"Odd",
	True, Print["Metric perturbation label ", mp, " is not in list ", MetricPerturbationLables[Parity->"Both"]]; Aborting[]
]


def@
MetricPerturbationLables[opts:OptionsPattern[]]:=
Module[{optionsRules,mode,parity,gauge},

	optionsRules = {"Mode" -> modeQ,
					"Parity" -> parityListQ,
					"Gauge"->gaugeQ};
	
	TestOptions[optionsRules,{opts}];
	mode=OptionValue[Mode];
	parity=OptionValue[Parity];
	gauge=OptionValue[Gauge];

	If[parity==="Both",Return[Join[MetricPerturbationLables[Parity->"Even",Mode->mode,Gauge->gauge],MetricPerturbationLables[Parity->"Odd",Mode->mode,Gauge->gauge]]]];

	If[gauge==="RWZ"||gauge==="Invariant",

		Switch[mode,

			"Monopole",
			If[parity==="Even",
				{HttLabel[],HrrLabel[]},
				{}
			],
			
			"Dipole",
			If[parity==="Even",
				{HttLabel[],HtrLabel[],HrrLabel[]},
				{HtLabel[]}
			],
	
			"Radiative",
			If[parity==="Even",
				{HttLabel[],HtrLabel[],HrrLabel[],KLabel[]},
				{HtLabel[],HrLabel[]}
			]
		],

		Switch[mode,
			
			"Monopole",
			If[parity==="Even",
				{HttLabel[],HtrLabel[],HrrLabel[],KLabel[]},
				{}
			],
	
			"Dipole",
			If[parity==="Even",
				{HttLabel[],HtrLabel[],HrrLabel[],JtLabel[],JrLabel[],KLabel[]},
				{HtLabel[],HrLabel[]}
			],
	
			"Radiative",
			If[parity==="Even",
				{HttLabel[],HtrLabel[],HrrLabel[],JtLabel[],JrLabel[],KLabel[],GLabel[]},
				{HtLabel[],HrLabel[],H2Label[]}
			]
		]
	]
]


def@
MetricPerturbationSourceLables[opts:OptionsPattern[]]:=
Module[{optionsRules,mode,parity,gauge},

	optionsRules = {"Mode" -> modeQ,
					"Parity" -> parityListQ,
					"Gauge"->gaugeQ};
	
	TestOptions[optionsRules,{opts}];
	mode=OptionValue[Mode];
	parity=OptionValue[Parity];
	gauge=OptionValue[Gauge];

	If[parity==="Both",Return[Join[MetricPerturbationSourceLables[Parity->"Even",Mode->mode,Gauge->gauge],MetricPerturbationSourceLables[Parity->"Odd",Mode->mode,Gauge->gauge]]]];

	If[gauge==="RWZ"||gauge==="Invariant",

		Switch[mode,

			"Monopole",
			If[parity==="Even",
				{QttLabel[],QrrLabel[]},
				{}
			],

			"Dipole",
			If[parity==="Even",
				{QttLabel[],QtrLabel[],QrrLabel[]},
				{PtLabel[]}
			],
	
			"Radiative",
			If[parity==="Even",
				{QttLabel[],QtrLabel[],QrrLabel[],QFlatLabel[]},
				{PtLabel[],PrLabel[]}
			]
		],

		Switch[mode,

			"Monopole",
			If[parity==="Even",
				{QttLabel[],QtrLabel[],QrrLabel[],QFlatLabel[]},
				{}
			],
	
			"Dipole",
			If[parity==="Even",
				{QttLabel[],QtrLabel[],QrrLabel[],QtLabel[],QrLabel[],QFlatLabel[]},
				{PtLabel[],PrLabel[]}
			],
	
			"Radiative",
			If[parity==="Even",
				{QttLabel[],QtrLabel[],QrrLabel[],QtLabel[],QrLabel[],QFlatLabel[],QSharpLabel[]},
				{PtLabel[],PrLabel[],PLabel[]}
			]
		]
	]
]


Options[GaugeVectorLabels]={"Parity"->"Both","Mode"->"Radiative"};
DocumentationBuilder`OptionDescriptions["GaugeVectorLabels"] = 
{
    "Parity" -> "Specifies which parities to include, (\"Even\", \"Odd\", or \"Both\")",
	"Mode" -> "l mode, restricting labels based on which spherical harmonic amplitudes are defined, (\"Radiative\", \"Dipole\", or \"Monopole\")"
};



def@
GaugeVectorLabels[opts:OptionsPattern[]]:=
Module[{optionsRules,parity,mode},

	optionsRules = {"Parity" -> parityListQ,
					"Mode"->modeQ};
	TestOptions[optionsRules,{opts}];
	
	parity=OptionValue[Parity];
	mode=OptionValue[Mode];

	If[parity==="Both",Return[Join[GaugeVectorLabels[Parity->"Even",Mode->mode],GaugeVectorLabels[Parity->"Odd",Mode->mode]]]];

	If[mode==="Monopole",
		If[parity==="Even",
			{XiEvenTLabel[],XiEvenRLabel[]},
			{}
		]
		,
		If[parity==="Even",
			{XiEvenLabel[],XiEvenTLabel[],XiEvenRLabel[]},
			{XiOddLabel[]}
		]
	]
]


def@
MasterFunctionLabel[]:="Psi"


def@
HttLabel[]:="htt"


def@
HtrLabel[]:="htr"


def@
HrrLabel[]:="hrr"


def@
JtLabel[]:="jt"


def@
JrLabel[]:="jr"


def@
KLabel[]:="K"


def@
GLabel[]:="G"


def@
HtLabel[]:="ht"


def@
HrLabel[]:="hr"


def@
H2Label[]:="h2"


def@
QttLabel[]:="Qtt"


def@
QtrLabel[]:="Qtr"


def@
QrrLabel[]:="Qrr"


def@
QtLabel[]:="Qt"


def@
QrLabel[]:="Qr"


def@
QFlatLabel[]:="QFlat"


def@
QSharpLabel[]:="QSharp"


def@
PtLabel[]:="Pt"


def@
PrLabel[]:="Pr"


def@
PLabel[]:="P"


def@
XiEvenLabel[]:="xiE"


def@
XiEvenTLabel[]:="xiET"


def@
XiEvenRLabel[]:="xiER"


def@
XiOddLabel[]:="xiO"


def@
SphericalHarmonicLabels[opts:OptionsPattern[]]:=
Module[{optionsRules,parity},

	optionsRules = {"Parity" -> parityListQ};
	TestOptions[optionsRules,{opts}];
	
	parity=OptionValue[Parity];

	If[parity==="Both",Return[Join[SphericalHarmonicLabels[Parity->"Even"],SphericalHarmonicLabels[Parity->"Odd"]]]];

	If[parity==="Even",
		{YLabel[],YThetaLabel[],YPhiLabel[],YThetaThetaLabel[],YThetaPhiLabel[],YPhiPhiLabel[]},
		{XThetaLabel[],XPhiLabel[],XThetaThetaLabel[],XThetaPhiLabel[],XPhiPhiLabel[]}
	]
]


def@
YLabel[]:="Y"


def@
YThetaLabel[]:="YTheta"


def@
YPhiLabel[]:="YPhi"


def@
YThetaThetaLabel[]:="YThetaTheta"


def@
YThetaPhiLabel[]:="YThetaPhi"


def@
YPhiPhiLabel[]:="YPhiPhi"


def@
XThetaLabel[]:="XTheta"


def@
XPhiLabel[]:="XPhi"


def@
XThetaThetaLabel[]:="XThetaTheta"


def@
XThetaPhiLabel[]:="XThetaPhi"


def@
XPhiPhiLabel[]:="XPhiPhi"


End[];

EndPackage[];
