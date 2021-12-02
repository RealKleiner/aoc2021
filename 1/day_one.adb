with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day_One is
	File_Name : constant String := "input";

	procedure Second is
		package Integer_Vector is new 
			Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Integer);

		function Compare_Windows(V : Integer_Vector.Vector; 
								Index : Natural) return Boolean is
		begin
			return V(V.First_Index+(Index - 3)) < V(V.First_Index+Index);
		end Compare_Windows;

		File : File_Type;
		Values : Integer_Vector.Vector;
		Number_Increases : Integer;
		Start : Natural := 3;

	begin
		Open(File, In_File, File_Name);

		while not End_Of_File(File) loop 
			Get(File, Number_Increases);
			Skip_Line(File);
			Values.Append(Number_Increases);
		end loop;
		Close(File);
		Number_Increases := 0;

		while Values.Last_Index >= Start loop
			if Compare_Windows(Values, Start) then
				Number_Increases := Number_Increases + 1;
			end if;
			Start := Start + 1;
		end loop;
		
		Put_Line("=== Part 2 ===");
		Put("There were ");
		Put(Number_Increases, Width => 0);
		Put(" increases in depth.");
		
	end Second;

	procedure First is
		File : File_Type;
		Current_Measurment, Last_Measurment : Integer := 0;
		Number_Increases : Integer := 0;
	begin
		Open(File, In_File, File_Name);
		-- Read the first measurment to get started
		Get(File, Last_Measurment);

		while not End_Of_File(File) loop 
			Get(File, Current_Measurment);
			Skip_Line(File);
			if Current_Measurment > Last_Measurment then
				Number_Increases := Number_Increases + 1;
			end if;
			Last_Measurment := Current_Measurment;
		end loop;
		
		Close(File);

		Put_Line("=== Part 1 ===");
		Put("There were ");
		Put(Number_Increases, Width => 0);
		Put(" increases in depth.");
		
	end First;
begin
	First;
	New_Line(2);
	Second;

end Day_One;
