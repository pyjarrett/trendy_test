with Ada.Exceptions;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body Trendy_Test is

    procedure Reset (T : in out Test) is
    begin
        T.Name := ASU.Null_Unbounded_String;
    end Reset;

    procedure Start (T : in out Test'Class; Name : String) is
    begin
        T.Name := ASU.To_Unbounded_String (Name);
    end Start;

    procedure Disable (T : in out Test'Class) is
    begin
        raise Test_Disabled;
    end Disable;

    procedure Require (T : in out Test'Class; Condition : Boolean) is
    begin
        pragma Unreferenced(T);
        if not Condition then
            raise Test_Failure;
        end if;
    end Require;

    function "and" (Left, Right: Test_Result) return Test_Result is
    begin
        return (if Left = Passed and Right = Passed then Passed else Failed);
    end "and";

    procedure Register (TG : in Test_Group) is
    begin
        All_Test_Groups.Append (TG);
    end Register;

    function Run (TG : Test_Group) return Test_Result is
        Passes   : Natural := 0;
        Fails    : Natural := 0;
        Instance : Test;

        use Ada.Text_IO;
        use Ada.Strings.Unbounded.Text_IO;
        use type ASU.Unbounded_String;
    begin
        for T of TG loop
            declare
            begin
                T.all (Instance);
                Put_Line ("Passed: " & Instance.Name);
                Passes := Passes + 1;
            exception
                when Test_Disabled =>
                    Put_Line ("Disabled: " & Instance.Name);
                when Error : Test_Failure =>
                    Put_Line ("Failed: " & Instance.Name);
                    Put_Line (Ada.Exceptions.Exception_Information (Error));
                    Fails := Fails + 1;
            end;
        end loop;

        Put_Line ("Failed: " & Fails'Image & "  Passes:" & Passes'Image);
        if Fails > 0 then
            return Failed;
        else
            return Passed;
        end if;
    end Run;

    function Run return Test_Result is
        Result : Test_Result := Passed;
    begin
        for TG of All_Test_Groups loop
            Result := Result and Run (TG);
        end loop;
        return Result;
    end Run;
end Trendy_Test;
