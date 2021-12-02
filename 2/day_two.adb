with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Day_Two is
	function Get_Integer_Value(Item : String) return Integer is
	begin
		return Integer'Value((1 => Item(Item'Last)));
	end Get_Integer_Value;

	procedure Second(Item : in String; Aim, Depth, Horizontal : in out Integer) is
		Value : Integer := Get_Integer_Value(Item);
	begin
		if Item(Item'First) = 'f' then
			Horizontal := Horizontal + Value;
			Depth := Depth + Aim * Value;
		elsif Item(Item'First) = 'd' then
			Aim := Aim + Value;
		elsif Item(Item'First) = 'u' then
			Aim := Aim - Value;
		end if;
	end Second;


	procedure First(Item : in String; Depth, Horizontal : in out Integer) is
		Value : Integer := Get_Integer_Value(Item);
	begin
		if Item(Item'First) = 'f' then
			Horizontal := Horizontal + Value;
		elsif Item(Item'First) = 'd' then
			Depth := Depth + Value;
		elsif Item(Item'First) = 'u' then
			Depth := Depth - Value;
		end if;
	end First;

	File : File_Type;
	Part_1_H, Part_1_D : Integer := 0;
	Part_2_H, Part_2_D, Part_2_A : Integer := 0;
	Input : String(1..9);
	Length : Integer;

begin
	open(File, In_File, "input");

	while not End_Of_File(File) loop
		Get_Line(File, Input, Length);
		if Input'Length = Length then
			Skip_Line(File);
		end if;

		First(Input(1..Length), Part_1_D, Part_1_H);
		Second(Input(1..Length), Part_2_A, Part_2_D, Part_2_H);
	end loop;
	Close(File);

	Put_Line("=== Part 1 ===");
	Put("The result is: ");
	Put(Part_1_D * Part_1_H, Width => 0);
	
	New_Line(2);

	Put_Line("=== Part 2 ===");
	Put("The result is: ");
	Put(Part_2_D * Part_2_H, Width => 0);
end Day_Two;
