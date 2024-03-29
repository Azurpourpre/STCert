i := 10;
f0 := 1;
f1 := 1;

WHILE i > 0 DO
    oldf1 := f1;
    f1 := oldf1 + f0;
    f0 := oldf1;
    i := i - 1;
END_WHILE;