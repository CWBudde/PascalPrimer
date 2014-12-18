Clear;
Randomize;
CursorVisible := False;

const Steps = 7;

var i: Integer = 0;
var j: Integer = 0;
var x: Float; 
var y: Float; 

repeat
  j := j + 1;

  i := 0;

  x := 100 + 7 * j + (100 - 7 * j) * (1 + sin(i / Steps * 2 * Pi));
  y := 100 + 7 * j + (100 - 7 * j) * (1 + cos(i / Steps * 2 * Pi));

  MoveTo(X, Y);

  repeat
    i := i + 1;
  
    x := 100 + 7 * j + (100 - 7 * j) * (1 + sin(i / Steps * 2 * Pi));
    y := 100 + 7 * j + (100 - 7 * j) * (1 + cos(i / Steps * 2 * Pi));
  
    CursorColor := $FF000000 or RandomInt($FFFFFF);  
  
    LineTo(X, Y);
  until i >= Steps;

until j >= 10;
