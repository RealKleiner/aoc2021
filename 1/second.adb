with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Second is
	package Integer_Vector is new 
		Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Integer);

	function Sum(V : Integer_Vector.Vector; Start : Natural) return Integer is
		Res : Integer := 0;
	begin
		for I in (V.First_Index+Start) .. (V.First_Index+Start+2) loop
			Res := Res + V(I);
		end loop;
		return Res;
	end Sum;

	File : File_Type;
	Current_Measurment, Last_Measurment, Number_Increases : Integer := 0;
	Values : Integer_Vector.Vector;
	Start : Natural := 1;

begin
	Open(File, In_File, "input");

	while not End_Of_File(File) loop 
		Get(File, Current_Measurment);
		Skip_Line(File);
		Values.Append(Current_Measurment);
	end loop;
	Close(File);

	Last_Measurment := Sum(Values, 0);

	while Values.Last_Index >= (Start+2) loop
		Current_Measurment := Sum(Values, Start);
		if Current_Measurment > Last_Measurment then
			Number_Increases := Number_Increases + 1;
		end if;
		Last_Measurment := Current_Measurment;
		Start := Start + 1;
	end loop;
	
	Put("There were ");
	Put(Number_Increases, Width => 0);
	Put(" increases in depth.");
	
end Second;

