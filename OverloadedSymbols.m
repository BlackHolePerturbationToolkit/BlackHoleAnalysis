(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`OverloadedSymbols`"];


Periapsis::usage="Periapsis[] returns the periapsis (rMin) of an orbit written as a function of \
eccentricity and semi-latus rectum."
Apoapsis::usage="Apoapsis[] returns the apoapsis (rMax) of an orbit written as a function of \
eccentricity and semi-latus rectum."
SemiLatusRectum::usage="SemiLatusRectum[] returns the semi-latus rectum of an orbit written \
as a function of periapsis (rMin) and apoapsis (rMax)."
OrbitalEccentricity::usage="OrbitalEccentricity[] returns the eccentricity of an orbit written \
as a function of periapsis (rMin) and apoapsis (rMax)."
SpecificEnergy::usage="SpecificEnergy[] returns the specific energy (conserved energy per mass) of an orbit written as a function of \
eccentricity and semi-latus rectum."
SpecificAngularMomentum::usage="SpecificAngularMomentum[] returns the specific angular momentum \
(conserved angular momentum per mass) of an orbit written as a function of \
eccentricity and semi-latus rectum."


ROfChi::usage="ROfChi[] returns the Darwin's expression for r(chi).";
PhiOfChi::usage="PhiOfChi[] returns a closed-form expression for Darwin's phi(chi) in terms of EllipticF.";

DRDT::usage="DRDT[] returns dr/dt computed from DTDChi and D[ROfChi[],chi].
DRDT[n] returns d^{n}r/dt^{n} computed from DTDChi and D[ROfChi[],chi].";

DPhiDT::usage="DPhiDT[] returns dphi/dt computed from DTDChi and DPhiDChi.
DPhiDT[n] returns d^{n}phi/dt^{n} computed from DTDChi and DPhiDChi.";


CoordinatePosition::usage="CoordinatePosition[] returns a list of elements characterizing the particle's \
position in order {t,r,theta,phi}.";

FourVelocity::usage="FourVelocity[] returns a list of elements characterizing the particle's \
four-velocity in order {t,r,theta,phi}.";

CoordinateVelocity::usage="CoordinateVelocity[] returns a list of elements characterizing the particle's \
coordinate velocity in order {t,r,theta,phi}.";


XPN::usage="XPN[] returns the post-Newtonian parameter x = (M OmegaPhi)^2/3 of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to 1 PN using the inverse of the semi-latus rectum as the expansion parameter.
XPN[n] returns the post-Newtonian parameter x of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to n PN using the inverse of the semi-latus rectum as the expansion parameter.";


Parity::usage="Parity[l,m] returns \"Even\" if l+m is even, and \"Odd\" if l+m is odd.
Parity[label] returns \"Even\" if label corresponds to an even-parity metric perturbation amplitude, \
and \"Odd\" if label corresponds to an odd amplitude.";


Options[PhiOfChi]=Join[Options[ROfChi],{"Metric"->"Schwarzschild"}];
DocumentationBuilder`OptionDescriptions["PhiOfChi"] = 
Join[
	{"Metric" -> "Which spacetime the particle orbits in (\"Schwarzschild of \"Kerr\"\")."}
];
Options[TOfChi]=Options[PhiOfChi];
DocumentationBuilder`OptionDescriptions["TOfChi"] = DocumentationBuilder`OptionDescriptions["PhiOfChi"];
Options[DRDT]=Options[PhiOfChi];
DocumentationBuilder`OptionDescriptions["DRDT"] = DocumentationBuilder`OptionDescriptions["PhiOfChi"];
Options[DPhiDT]=Options[PhiOfChi];
DocumentationBuilder`OptionDescriptions["DPhiDT"] = DocumentationBuilder`OptionDescriptions["PhiOfChi"];


EndPackage[];
