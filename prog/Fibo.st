PROGRAM fibo

f0 := 1;
f1 := 1;

FOR i := 10 TO 0 BY -1 DO
    oldf1 := f1;
    f1 := oldf1 + f0;
    f0 := oldf1;
END_FOR;

END_PROGRAM


PROGRAM main

fibo(); // Call fibo

hello := "Hello, World !";

END_PROGRAM