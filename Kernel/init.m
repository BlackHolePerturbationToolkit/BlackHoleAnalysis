(* ::Package:: *)

(* Mathematica Init File *)


Get["BlackHoleAnalysis`Kernel`BlackHoleAnalysis`"];


Block[{},
	Unprotect[$Packages];
	$Packages = Complement[$Packages, BlackHoleAnalysis`Private`packages];
	Protect[$Packages];

	Scan[Needs, BlackHoleAnalysis`Private`packages];
]
