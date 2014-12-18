program Lotto;

var
  ZufallsZahl: array [1..6] of Integer;
  Index, Counter: Integer;
begin
  Randomize;
  Counter := 0;
  Index := 0;

  repeat
    // Berechne die 1. Zufallszahl
    ZufallsZahl[1] := RandomInt(49) + 1;

    // Berechne die 2. Zufallszahl
    repeat
      ZufallsZahl[2] := RandomInt(49) + 1;
    until ZufallsZahl[2] <> ZufallsZahl[1];

    // Berechne die 3. Zufallszahl
    repeat
      ZufallsZahl[3] := RandomInt(49) + 1;
    until (ZufallsZahl[3] <> ZufallsZahl[1]) and
          (ZufallsZahl[3] <> ZufallsZahl[2]);

    // Berechne die 4. Zufallszahl
    repeat
      ZufallsZahl[4] := RandomInt(49) + 1;
    until (ZufallsZahl[4] <> ZufallsZahl[1]) and
          (ZufallsZahl[4] <> ZufallsZahl[2]) and
          (ZufallsZahl[4] <> ZufallsZahl[3]);

    // Berechne die 5. Zufallszahl
    repeat
      ZufallsZahl[5] := RandomInt(49) + 1;
    until (ZufallsZahl[5] <> ZufallsZahl[1]) and
          (ZufallsZahl[5] <> ZufallsZahl[2]) and
          (ZufallsZahl[5] <> ZufallsZahl[3]) and
          (ZufallsZahl[5] <> ZufallsZahl[4]);

    // Berechne die 6. Zufallszahl
    repeat
      ZufallsZahl[6] := RandomInt(49) + 1;
    until (ZufallsZahl[6] <> ZufallsZahl[1]) and
          (ZufallsZahl[6] <> ZufallsZahl[2]) and
          (ZufallsZahl[6] <> ZufallsZahl[3]) and
          (ZufallsZahl[6] <> ZufallsZahl[4]) and
          (ZufallsZahl[6] <> ZufallsZahl[5]);

    if (ZufallsZahl[1] + 1 = ZufallsZahl[2]) or
       (ZufallsZahl[2] + 1 = ZufallsZahl[3]) or
       (ZufallsZahl[3] + 1 = ZufallsZahl[4]) or
       (ZufallsZahl[4] + 1 = ZufallsZahl[5]) or
       (ZufallsZahl[5] + 1 = ZufallsZahl[6]) then
      Counter := Counter + 1;

    Index := Index + 1;
  until Index = 10000;

  // Zeige die letzte Ziehung auf dem Bildschirm an
  PrintLn(
    IntToStr(ZufallsZahl[1]) + ', ' +  
    IntToStr(ZufallsZahl[2]) + ', ' +
    IntToStr(ZufallsZahl[3]) + ', ' +
    IntToStr(ZufallsZahl[4]) + ', ' +  
    IntToStr(ZufallsZahl[5]) + ', ' + 
    IntToStr(ZufallsZahl[6]));

  // Gebe die Wahrscheinlichkeit für zwei aufeinander folgende Zahlen an
  PrintLn('Wahrscheinlichkeit aufeinanderfolgender Zahlen: ' +
    FloatToStr(100 * Counter / Index) + ' %');
end.