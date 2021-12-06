with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors; 

procedure Day_Three is
	procedure Main(Line_Length : in Integer; 
				   Part1_Res : out Integer;
				   Part2_Res : out Integer) is

		type T_Number is
			array (1..Line_Length) of Integer;

		subtype Line is String(1..Line_Length);
		package String_Vector is new
			Ada.Containers.Vectors
				(Index_Type => Natural,
				 Element_Type => Line);
		use String_Vector;

		procedure Count_Ones_Zeroes(Input : in String_Vector.Vector;
									Ones, Zeroes : out T_Number) is
		begin
			Ones := (others => 0);
			Zeroes := (others => 0);

			for E of Input loop
				for I in T_Number'Range loop
					if E(I) = '1' then
						Ones(I) := Ones(I) + 1;
					else
						Zeroes(I) := Zeroes(I) + 1;
					end if;
				end loop;
			end loop;

		end Count_Ones_Zeroes;

		procedure Part1(Input : in String_Vector.Vector; 
						Output : out Integer) is
			Ones, Zeroes : T_Number;
			Gamma, Epsilon : Integer;
			S_Gamma, S_Epsilon : Line;

		begin
			Count_Ones_Zeroes(Input, Ones, Zeroes);
			
			for I in Line'Range loop
				if Ones(I) > Zeroes(I) then
					S_Gamma(I) := '1';
					S_Epsilon(I) := '0';
				else
					S_Gamma(I) := '0';
					S_Epsilon(I) := '1';
				end if;
			end loop;

			Gamma := Integer'Value("2#" & S_Gamma & '#');
			Epsilon := Integer'Value("2#" & S_Epsilon & '#');

			Output := Gamma * Epsilon;

		end Part1;	


		procedure Part2(Input : in String_Vector.Vector; 
						Output : out Integer) is
			V_Data : String_Vector.Vector := Input;
			V_Temp : String_Vector.Vector;
			Ones, Zeroes : T_Number;
			Count : Integer := 1;
			Oxygen, Scrubber : Integer;
		begin
			-- Get the Oxygen value
			while Length(V_Data) > 1 loop
				Count_Ones_Zeroes(V_Data, Ones, Zeroes);
				for E of V_Data loop
					if Ones(Count) >= Zeroes(Count) then
						if E(Count) = '1' then 
							V_Temp.Append(E);
						end if;
					else 
						if E(Count) = '0' then
							V_Temp.Append(E);
						end if;
					end if;
				end loop;
				Count := Count + 1;
				V_Data := V_Temp;
				V_Temp := String_Vector.Empty;
			end loop;
			Oxygen := Integer'Value("2#" & V_Data(1) & '#');

			V_Data := Input;
			while Length(V_Data) > 1 loop
				Count_Ones_Zeroes(V_Data, Ones, Zeroes);
				for E of V_Data loop
					if Zeroes(Count) >= Ones(Count) then
						if E(Count) = '0' then 
							V_Temp.Append(E);
						end if;
					else 
						if E(Count) = '1' then
							V_Temp.Append(E);
						end if;
					end if;
				end loop;
				Count := Count + 1;
				V_Data := V_Temp;
				V_Temp := String_Vector.Empty;
			end loop;
			Scrubber := Integer'Value("2#" & V_Data(1) & '#');

		Output := Oxygen * Scrubber;
		end Part2;	

		File : File_Type;
		Input : String(1..Line_Length);
		Inputs : String_Vector.Vector;

	begin
		Open(File, In_File, Argument(1));

		while not End_Of_File(File) loop
			Get(File, Input);
			Skip_Line(File);
			Inputs.Append(Input);
		end loop;
		Close(File);

		Part1(Inputs, Part1_Res);
		Part2(Inputs, Part2_Res);
		
	end Main;


	File : File_Type;
	Input : Character;
	Length : Integer := 0;
	Part1_Res, Part2_Res : Integer;
begin
	if Argument_Count /= 1 then
		Put_Line("Usage: " & Command_Name & " FILEPATH");
		return;
	end if;

	Open(File, In_File, Argument(1));

	while not End_Of_Line(File) loop
		Get(File, Input);
		Length := Length + 1;
	end loop;
	Close(File);

	Main(Length, Part1_Res, Part2_Res);

	Put_Line("=== Part 1 ===");
	Put("The result is: ");
	Put(Part1_Res, Width => 0);

	New_Line(2);

	Put_Line("=== Part 2 ===");
	Put("The result is: ");
	Put(Part2_Res, Width => 0);


end Day_Three;
