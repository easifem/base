/**************************************
Geometry    :: Rectangle
Author      :: Vikas Sharma
Data        :: 04-May-2020
**************************************/

// mesh sizes
s = 5.0; p1 = newp; Point(p1) = {0, 0, 0, s};
s = 1.0; p2 = newp; Point(p2) = {60, 0, 0, s};
s = 1.0; p3 = newp; Point(p3) = {70, 0, 0, s};
s = 1.0; p4 = newp; Point(p4) = {115, 0, 0, s};
s = 1.0; p5 = newp; Point(p5) = {118, 0, 0, s};
s = 1.0; p6 = newp; Point(p6) = {118, 4, 0, s};
s = 1.0/4; p7 = newp; Point(p7) = {70, 15, 0, s};
s = 1.0; p8 = newp; Point(p8) = {60, 17.5, 0, s};
s = 1.0; p9 = newp; Point(p9) = {50, 20, 0, s};

// Lines
l1 = newl; Line(l1) = {p1,p2};
l2 = newl; Line(l2) = {p2,p3};
l3 = newl; Line(l3) = {p3,p4};
l4 = newl; Line(l4) = {p4,p5};
l5 = newl; BSpline(l5) = {p5,p6,p7};
l6 = newl; Line(l6) = {p7,p8};
l7 = newl; Line(l7) = {p8,p9};
l8 = newl; Line(l8) = {p9,p1};
l9 = newl; Line(l9) = {p2,p8};
l10 = newl; Line(l10) = {p3,p7};


// // Line Loops
ll1 = newll; Line Loop(ll1) = {l1,l9,l7,l8};
ll2 = newll; Line Loop(ll2) = {l2,l10,l6,-l9};
ll3 = newll; Line Loop(ll3) = {l3,l4,l5,-l10};

// // Physical Surfaces
ps1 = news; Plane Surface(ps1) = {ll1};
ps2 = news; Plane Surface(ps2) = {ll2};
ps3 = news; Plane Surface(ps3) = {ll3};

Physical Surface( "Omega_1", 1 ) = {ps1};
Physical Surface( "Omega_2", 2 ) = {ps2};
Physical Surface( "Omega_3", 3 ) = {ps3};
Physical Line( "Gamma_1", 1 ) = {l1}; //
Physical Line( "Gamma_2", 2 ) = {l2}; //
Physical Line( "Gamma_3", 3 ) = {l3}; //
Physical Line( "Gamma_4", 4 ) = {l4}; //
Physical Line( "Gamma_5", 5 ) = {l5}; //
Physical Line( "Gamma_6", 6 ) = {l6}; //
Physical Line( "Gamma_7", 7 ) = {l7}; //
Physical Line( "Gamma_8", 8 ) = {l8}; //
Physical Line( "Gamma_9", 9 ) = {l9}; //
Physical Line( "Gamma_10", 10 ) = {l10}; //