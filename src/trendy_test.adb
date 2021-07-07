with Ada.Exceptions;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body Trendy_Test is

    procedure Register (T        : in out Test'Class;
                        Name     : String;
                        Disabled : Boolean := False) is
    begin
        T.Name := ASU.To_Unbounded_String (Name);

        if Disabled then
            raise Test_Disabled;
        end if;
    end Register;

    procedure Report_Failure (T : in out Test'Class; Left : String; Right : String) is
    begin
        pragma Unreferenced (T);
        Ada.Text_IO.Put_Line (Left & " /= " & Right);
    end Report_Failure;

    procedure Require (T : in out Test'Class; Condition : Boolean) is
    begin
        pragma Unreferenced(T);
        if not Condition then
            raise Test_Failure;
        end if;
    end Require;

    procedure Require_Equal_Discrete(T : in out Test'Class; Left : in V; Right : in V) is
    begin
        if Left /= Right then
            T.Report_Failure (Left'Image, Right'Image);
            raise Test_Failure;
        end if;
    end Require_Equal_Discrete;

    procedure Require_Equal(T : in out Test'Class; Left : V; Right : V) is
    begin
        if Left /= Right then
            T.Report_Failure (Image(Left), Image(Right));
            raise Test_Failure;
        end if;
    end Require_Equal;

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
        Total    : Natural := 0;
        Instance : Test;

        use Ada.Text_IO;
        use Ada.Strings.Unbounded.Text_IO;
        use type ASU.Unbounded_String;
    begin
        for T of TG loop
            declare
            begin
                T.all (Instance);
                Put_Line ("[ PASS ] " & Instance.Name);
                Passes := Passes + 1;
            exception
                when Test_Disabled =>
                    Put_Line ("[ SKIP ] " & Instance.Name);
                when Error : Test_Failure =>
                    Put_Line ("[ FAIL ] " & Instance.Name);
                    Put_Line (Ada.Exceptions.Exception_Information (Error));
                    Fails := Fails + 1;
            end;
        end loop;

        Total := Fails + Passes;
        Put_Line ("Results: Passed: " & Passes'Image & " / " & Total'Image);
        New_Line;
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
