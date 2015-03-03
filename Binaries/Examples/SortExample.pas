uses
  SortUtils;

procedure SelectionSort(a: TNumberArray);
var 
  i, j, m: Integer;
  Temp: Integer;
begin
  // advance the position through the entire array */
  //   (could do j < n-1 because single element is also min element) */
  for j := 0 to a.Length - 2 do
  begin
    // find the min element in the unsorted a[j .. n-1]
 
    // assume the min is the first element
    m := j;

    // test against elements after j to find the smallest
    for i := j + 1 to a.Length - 1 do
    begin
      // if this element is less, then it is the new minimum  
      if (a[i] < a[m]) then
        m := i;
    end;
    
    if (m <> j) then
    begin
      Temp := a[m];
      a[m] := a[j];  
      a[j] := Temp;
    end;
  end;  
end;

procedure InsertionSort(a: TNumberArray);
var 
  i, j: Integer;
  Value: Integer;
begin
  // advance the position through the entire array */
  //   (could do j < n-1 because single element is also min element) */
  for i := 1 to a.Length - 1 do
  begin
    Value := a[i];
    j := i;
    while (j > 0) and (a[j - 1] > Value) do
    begin
      a[j] := a[j - 1];
      j := j - 1;
    end;
    a[j] := Value;
  end;  
end;

// Generate number array to work with
var NumberArray := GenerateRandomNumbers;

// Show 
ShowNumbers(NumberArray);

//SelectionSort(NumberArray);
InsertionSort(NumberArray);
ShowNumbers(NumberArray);