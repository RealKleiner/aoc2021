with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure First is
	File : File_Type;
	Current_Measurment, Last_Measurment : Integer := 0;
	Number_Increases : Integer := 0;
begin
	Open(File, In_File, "input");
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
	Put("There were ");
	Put(Number_Increases, Width => 0);
	Put(" increases in depth");
	
end First;

