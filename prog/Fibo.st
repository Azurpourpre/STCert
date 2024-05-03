PROGRAM fibo

f0 := 1;
f1 := 1;

FOR i := 10 TO 0 BY -1 DO
    oldf1 := f1;
    f1 := oldf1 + f0;
    f0 := oldf1;
END_FOR;

END_PROGRAM

PROGRAM fibo_arr

tab := [5 .. 15];
tab[5] := 1;
tab[6] := 1;
FOR i := 5 TO 14 BY 1 DO
    tab[i + 2] := tab[i] + tab[i+1];
END_FOR;

END_PROGRAM

PROGRAM main

fibo(); // Call fibo
fibo_arr();

hello := "Hello, World !";

END_PROGRAM