with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Second is
	function Get_Integer_Value(Item : String) return Integer is
	begin
		return Integer'Value((1 => Item(Item'Last)));
	end Get_Integer_Value;

	File : File_Type;
	Aim, Horizontal, Depth : Integer := 0;
	Input : String(1..9);
	Length : Integer;
begin
	open(File, In_File, "input");

	while not End_Of_File(File) loop
		Get_Line(File, Input, Length);
		if Input'Length = Length then
			Skip_Line(File);
		end if;

		if Input(Input'First) = 'f' then
			Horizontal := Horizontal + Get_Integer_Value(Input(1..Length));
			Depth := Depth + Aim * Get_Integer_Value(Input(1..Length));
		elsif Input(Input'First) = 'd' then
			Aim := Aim + Get_Integer_Value(Input(1..Length));
		elsif Input(Input'First) = 'u' then
			Aim := Aim - Get_Integer_Value(Input(1..Length));
		end if;
	end loop;
	Close(File);

	Put("The result is: ");
	Put(Horizontal * Depth, Width => 0);
end Second;


