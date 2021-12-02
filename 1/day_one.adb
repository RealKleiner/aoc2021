with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Day_One is
	type T_Measurements is
		array (1 .. 4) of Integer;

	procedure First(Input : in Integer; Increases, Last : in out Integer) is
	begin
		if Input > Last then
			Increases := Increases + 1;
		end if;
		Last := Input;
	end First;
	
	procedure Second(Input : in Integer; Increases, Count : in out Integer;
					 Measurements : in out T_Measurements) is
	begin

		Measurements(Count) := Input;
		Count := (Count mod T_Measurements'Last) + 1;

		if Input > Measurements(Count) then
			Increases := Increases + 1;
		end if;
	end Second;


	File : File_Type;
	Current_Measurement : Integer;
	Part1_Increases : Integer := 0;
	Part1_Last_Measurement : Integer := Integer'Last;
	Part2_Increases : Integer := 0;
	Part2_Measurements : T_Measurements := (Integer'Last, Integer'Last, 
											Integer'Last, Integer'Last);
	Part2_Count : Integer := 1;
begin
	if Argument_Count /= 1 then
		Put_Line("Usage: " & Command_Name & " FILE_PATH");
		return;
	end if;

	open(File, In_File, Argument(1));

	while not End_Of_File(File) loop
		Get(File, Current_Measurement);
		Skip_Line(File);
		First(Current_Measurement, Part1_Increases, Part1_Last_Measurement);
		Second(Current_Measurement, Part2_Increases, Part2_Count, Part2_Measurements);
	end loop;

	close (File);

	Put_Line("=== Part 1 ===");
	Put("Number of increases: ");
	Put(Part1_Increases, Width => 0);

	New_Line(2);

	Put_Line("=== Part 2 ===");
	Put("Number of increases: ");
	Put(Part2_Increases, Width => 0);
end Day_One;

