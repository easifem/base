// Two D rectangular mesh

L=1;
H=1;
nx=20;
ny=20;
sx=L/nx; p1 = newp; Point(p1) = {0,0,0,sx};
         p2 = newp; Point(p2) = {L,0,0,sx};
sy=H/ny; p3 = newp; Point(p3) = {L,H,0,sy};
         p4 = newp; Point(p4) = {0,H,0,sy};

l1=newl; Line(l1) = {p1, p2};
l2=newl; Line(l2) = {p2, p3};
l3=newl; Line(l3) = {p3, p4};
l4=newl; Line(l4) = {p4, p1};

ll1=newll; Line Loop(ll1) = {l1,l2,l3,l4};
ps1=news; Plane Surface(ps1) = {ll1};

Physical Surface("domain", 1) = {ps1};
Physical Line("bottom", 1) = {l1};
Physical Line("right", 2) = {l2};
Physical Line("top", 3) = {l3};
Physical Line("left", 4) = {l4};

