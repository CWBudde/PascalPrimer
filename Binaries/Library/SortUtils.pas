unit SortUtils;

interface

uses
  Shapes, Text, TurtleBasic, TurtleIntermediate, TurtleAdvanced;

type
  TNumberArray = array of Integer;

function GenerateRandomNumbers(Count: Integer = 10): TNumberArray;
procedure ShowNumbers(ANumberArray: TNumberArray);

implementation

function GenerateRandomNumbers(Count: Integer = 10): TNumberArray;
var
  TempIndex: Integer;
  TempValue: Integer;
begin
  Result.SetLength(Count);
  for var Index := Low(Result) to High(Result) do
    Result[Index] := Index;
  for var Index := Low(Result) to High(Result) do
  begin
    repeat
      TempIndex := RandomInt(Count);
    until TempIndex <> Index; 
    TempValue := Result[Index];  
    Result[Index] := Result[TempIndex];
    Result[TempIndex] := TempValue;
  end;   
end;

procedure ShowNumbers(ANumberArray: TNumberArray);
var
  Count: Integer;
  OldText: string;
begin
  OldText := TextOutput.Text;
  Clear;
  TextOutput.Text := OldText;
  
  Count := High(ANumberArray) - Low(ANumberArray) + 1; 

  for var Index := Low(ANumberArray) to High(ANumberArray) - 1 do
    Write(IntToStr(ANumberArray[Index]) + ', ');
  WriteLine(ANumberArray[High(ANumberArray)]);

  for var Index := Low(ANumberArray) to High(ANumberArray) do
  begin
    DrawRectangle(
      Index * ClientWidth / (Count) + 2,  
      (Count - ANumberArray[Index]) * ClientHeight / Count,  
      (Index + 1) * ClientWidth / (Count) - 2,  
      ClientHeight,
      clDimGray);  
  end;    

  for var Y := 1 to Count - 1 do
    for var X := 0 to (ClientWidth div 2) - 1 do
      Canvas.Pixel[2 * X, Round(Y * ClientHeight / Count)] := clBlack; 
end;

end.