{
 "Application" -> "BlackHoleAnalysis",
 "Package" -> "BlackHoleAnalysis",
 "Title" -> "BlackHoleAnalysis: An application for mode-by-mode black hole calculations",
 "Summary" -> "BlackHoleAnalysis: An application for mode-by-mode black hole calculations",
 "Description" ->
   {"The BlackHoleAnalysis application provides functions for ",
    "black hole calculations, particularly at the mode-by-mode level."},
 "Keywords" -> {"BlackHoleAnalysis"},
 "Label" -> "Black holes application",
 "Synonyms" -> {},
 "URL" -> "http://bitbucket.org/hoppese/BlackHoleAnalysis",
 "Packages" -> {
   {"Title" -> "Field equations",
    "DetailedFunctions" -> {
      {"FieldEquations", "The field equations for the lm modes on Schwarzschild"},
      {"MasterEquation", "The RWZ - style master equation"},
      {"TeukolskyEquation", "The homogeneous Teukolsky equation"},
      {"SasakiNakamuraEquation", "The homogeneous Sasaki-Nakamura equation"}
    }
   },
   {"Title" -> "Fields",
     "DetailedFunctions" -> {
       {"MasterFunction", "RWZ - style master function"},
       {"TeukolskyFunction", "4D Teukolsky function"},
       {"RadialTeukolskyFunction", "1D radial Teukolsky function"},
       {"SasakiNakamuraFunction", "Spin -2 Sasaki-Nakamura function"},
       {"HttAmplitude", "Martel-Poisson even-parity MP amplitude htt. Similar functions exist for htr, hrr, jt, jr, K, G, ht, hr and h2."},
       {"AmplitudeFunction", "Provides the function that gives a field for an associated label"},
       {"HttInvariantAmplitude", "Martel-Poisson even-parity MP gauge-invariant amplitude htt. Similar functions exist for htr, hrr, K, ht and hr."},
       {"InvariantAmplitudeFunction", "Provides the function that gives a gauge-invariant field for an associated label"},
       {"HttPush", "How the Martel-Poisson even-parity MP amplitude htt changes under a gauge transformation. \
Similar functions exist for htr, hrr, jt, jr, K, G, ht, hr and h2."},
       {"PushFunction", "Provides the function that gives the push equation for the associated label"}
     }
    },
    {"Title" -> "Source terms",
     "DetailedFunctions" -> {
       {"MasterEquationSource", "Source terms to the RWZ - style master equation"},
       {"QttSource", "Martel-Poisson even-parity source Qtt. Similar functions exist for Qtr, Qrr, Qt, Qr, QSharp, QFlat, Pt, Pr and P."},
       {"MetricPerturbationSourceFunction", "Provides the function that gives a source for an associated label"}
     }
    },
     {"Title"->"Spherical harmonics",
        "DetailedFunctions" -> {
            {"YHarmonic", "Scalar spherical harmonic, or spin-weighted spherical harmonic"},
            {"YTheta", "Component of Martel-Poisson harmonic Y_\[Theta]. \
Similar functions exist for Y_\[Phi], Y_\[Theta]\[Theta], Y_\[Theta]\[Phi], Y_\[Phi]\[Phi], \
X_\[Theta], X_\[Phi], X_\[Theta]\[Theta], X_\[Theta]\[Phi] and X_\[Phi]\[Phi]"},
            {"SpinWeightedSpheroidalHarmonicFunction", "Symbolic representation of the spin-weighted spheroidal harmonic"},
            {"SphericalHarmonicFunction", "Provides the function that gives a harmonic for an associated label"}
        }
    },
     {"Title"->"Orbital parameters",
        "DetailedFunctions" -> {
            {"FourVelocity", "Four-velocity for a particle in eccentric equatorial motion on Schwarzschild or Kerr"},
            {"SpecificEnergy", "Specific energy in terms of semi-latus rectum p & eccentricity e"},
            {"SpecificAngularMomentum", "Specific angular momentum in terms of semi-latus rectum p & eccentricity e"},
            {"Periapsis", "r_min in terms of semi-latus rectum p & eccentricity e"},
            {"Apoapsis", "r_max in terms of semi-latus rectum p & eccentricity e"},
            {"SemiLatusRectum", "Semi-latus rectum in terms of r_min and r_max"},
            {"OrbitalEccentricity", "Orbital eccentricity in terms of r_min and r_max"},
            {"USquared", "Effective potential on Schwarzschild"},
            {"Separatrix","Serapation between bound and unbound orbits on Schwarzschild or Kerr"}
        }
    },
    {"Title"->"Darwin's orbit parametrization",
       "DetailedFunctions" -> {
           {"ROfChi", "Radial position in terms of relativistic anomaly \[Chi] for eccentric equatorial motion on Schwarzschild or Kerr"},
           {"PhiOfChi", "Azimuthal position in terms of \[Chi] on Schwarzschild"},
           {"DTauDChi", "Differential equation for proper time in terms of \[Chi] on Schwarzschild or Kerr"},
           {"DTDChi", "Differential equation for coordinate time in terms of \[Chi] on Schwarzschild or Kerr"},     
           {"DPhiDChi", "Differential equation for Azimuthal position in terms of \[Chi] on Schwarzschild or Kerr"},  
           {"DRDT", "dr/dt in terms of \[Chi] on Schwarzschild or Kerr"}
       }
    },
    {"Title"->"Discontinuities",
       "DetailedFunctions" -> {
           {"EvaluateDiscontinuities", "Evaluate coefficients of delta functions, their derivatives, and the derivatives of step functions."},
           {"MasterFunctionDiscontinuities", "Discontinuities in the master function"},
           {"HttDiscontinuities", "Discontinuities in the the MP amplitude htt in RWZ or Lorenz gauge. \
Similar functions exist for htr, hrr, jt, jr, K, G, ht, hr, and h2."},
           {"DiscontinuitiesFunction", "Provides the function that gives discontinuities for an associated label"}
       }
    },
    {"Title"->"Labels",
       "DetailedFunctions" -> {
           {"MetricPerturbationLables", "Labels for the MP amplitudes"},
           {"MetricPerturbationSourceLables", "Labels for the source terms of the decomposed field equations"},
           {"SphericalHarmonicLabels", "Labels for the spherical harmonics"},
           {"GaugeVectorLabels", "Labels for the amplitudes of the gauge vector"}
       }
    },
    {"Title"->"Orbital functions and simplification",
       "DetailedFunctions" -> {
           {"RpDotSquared", "The coordinate time derivative of r squared on Schwarzschild or Kerr"},
           {"RpDotDot", "The second coordinate time derivative of r on Schwarzschild or Kerr"},
           {"RemoveRpDots", "Eliminate time derivative of r on Schwarzschild or Kerr"},
           {"RemovePhiPDots", "Eliminate time derivative of \[Phi] on Schwarzschild or Kerr"},
           {"RemoveSHDots", "Eliminate time derivatives spherical harmonics"},
           {"ReduceLegendreP", "Lower the order of Legendre polynomials"}
       }
    },
    {"Title"->"Relationships between functions",
       "DetailedFunctions" -> {
           {"EvenMasterFunctionAsOdd", "Even-parity Zerilli-Moncrief function in terms of the odd-parity Cunningham-Price-Moncrief function."},
           {"MasterFunctionAsTeukolskyFunction", "Frequency domain RWZ master function in terms of the radial Teukolsky function"},
           {"TeukolskyFunctionAsSasakiNakamuraFunction", "Radial Teukolsky function in terms of the Sasaki-Nakamura function"},
           {"SasakiNakamuraFunctionAsTeukolskyFunction", "Sasaki-Nakamura function in terms of the radial Teukolsky function"},
           {"MartelPoissonAmplitude", "Martel-Poisson amplitude written as Barack-Sago amplitudes"},
           {"BarackSagoAmplitude", "Barack-Sago amplitude written as Martel-Poisson amplitudes"}
       }
    },
    {"Title"->"Removing derivatives with differential equations",
       "DetailedFunctions" -> {
           {"RemoveMasterFunctionRDerivatives", "Reduce order of r-derivatives of master functions (in time or frequency domain)"},
           {"RemoveTeukolskyFunctionRDerivatives", "Reduce order of r-derivatives of the radial Teukolsky function"},
           {"RemoveSlmThetaDerivatives", "Reduce order of \[Theta]-derivatives of spin-weighted spheroidal harmonics"},
           {"RemoveSasakiNakamuraFunctionRDerivatives", "Reduce order of r-derivatives of the Sasaki-Nakamura function"}
       }
    },
    {"Title"->"Post-Newtonian expressions",
       "DetailedFunctions" -> {
           {"XPN", "PN parameter x in terms of semi-latus rectum p"},
           {"SemiLatusRectumPN", "Semi-latus rectum p in terms of x"},
           {"OmegaPhiPN", "Azimuthal angular frequency in a PN expansion"},
           {"OmegaRPN", "Radial angular frequency in a PN expansion"},
           {"SpecificEnergyPN", "Specific energy in a PN expansion"},
           {"SpecificAngularMomentumPN", "Specific angular momentum in a PN expansion"},
           {"RadialPeriodPN", "Radial period in a PN expansion"},
           {"AzimuthalAdvancePN", "Advance in \[Phi] during one radial period in a PN expansion"}
       }
    },
    {"Title"->"Odds and ends",
       "DetailedFunctions" -> {
           {"SchwarzschildF", "1 - 2M/r"},
           {"SchwarzschildMetric", "The Schwarzschild metric in Schwarzschild coordinates"},
           {"KerrMetric", "The Kerr metric in Boyer-Lindquist coordinates"},
           {"RStarOfR", "Tortoise coordinate in terms of Boyer-Lindquist r"},
           {"RStarToR", "Transform Tortoise coordinate to Schwarzschild r"},
           {"ROfRStar", "Schwarzschild r in terms of Tortoise coordinate"},
           {"RToRStar", "Transform Schwarzschild r to Tortoise coordinate"},
           {"ToFrequencyDomain", "Replace time derivatives with -i \[Omega]"},
           {"SigmaKerr", "The quantity \[CapitalSigma] used on Kerr"},
           {"DeltaKerr", "The quantity \[CapitalDelta] used on Kerr"},
           {"RPlusKerr", "The quantity r+ used on Kerr"},
           {"RMinusKerr", "The quantity r- used on Kerr"}
       }
    }
  },
 "Tutorials" -> {
     "Conventions",
     "Discontinuities",
     "Equations of motion",
     "Examples",
     "Field equations on Schwarzschild",
     "Kerr equations",
     "Low order modes",
     "Post-Newtonian expressions",
     "RWZ master equations",
     "Simplifying expressions",
     "Source terms",
     "Spherical harmonics",
     "Time domain amplitudes",
     "Using labels to act on groups of variables",
     "Weak functions"
  }
}
