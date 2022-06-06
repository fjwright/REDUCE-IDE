COMMENT: This file tests reduce-mode syntactic highlighting and
delimiter matching.  It begins with a multi-line comment statement
followed by some examples provided by Andrey Grozin.$

x(1):=1$
x(1) :=1$
x(1):= 1$
x(1) := 1$

% for, all, such, that, let, clear, match should be purple; f should be blue:
for all x,y let f(x,y)=0;
for all x,y clear f(x,y);
for all x,y such that x<0 let f(x,y)=0;
for all x,y such that x<0 clear f(x,y);

for all n1,n2 match x1^n1*x2^n2=f(n1,n2);

a where {f(~x)=>0,y=>0} when x>0; % where, when should be purple; f should blue

Comment End of Grozin Examples$

% for, do, write, step, until should be purple; e, pi, i should be dark cyan:
for i := 0 : 10 do write i;
for j := 0 step 2 until 10 do write j;

e^(pi*i) = -1;
-1 = e^(pi*i);

factor x,cos,sin(x);
remfac x,cos,sin(x);

% array, matrix, vector, operator should be green; a, b, c, x, y, x, p1, p2 should be blue:
array a(10),b(2,3,4);
a(10),b(2,3,4);

array a 10, c 57;
a 10, c 57;

matrix x(2,1),y(3,4),z;
matrix m; operator x; m := mat((x(1,1),x(1,2));

vector p1,p2;

% even, odd etc. should be green; arguments should be blue:
even f1; odd f2;
linear f,g;
noncom u,v;
symmetric u,v;
antisymmetric l,m;
operator h,g1,arctan;
infix mm;
precedence mm,-;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Comment Delimiter Highlighting$
( () );  ( ()
<< << >> >>;  << << >>;
begin begin end end;  begin begin end;  beginning begin noend end;
