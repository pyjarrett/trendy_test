with Ada.Exceptions;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body Trendy_Test is

    procedure Register (Op           : in out Operation'Class;
                        Name         : String;
                        Disabled     : Boolean := False;
                        Parallelize  : Boolean := True) is
    begin
        null;
    end Register;
    pragma Unreferenced (Register);

    overriding
    procedure Register (T           : in out List;
                        Name        : String;
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True) is
    begin
        pragma Unreferenced (T, Disabled, Parallelize);
        Ada.Text_IO.Put_Line (Name);
    end Register;

    overriding
    procedure Register (T           : in out Test;
                        Name        : String;
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True) is
    begin
        pragma Unreferenced(Parallelize);

        T.Name := ASU.To_Unbounded_String (Name);

        if Disabled then
            raise Test_Disabled;
        end if;
    end Register;

    procedure Report_Failure (Op : in out Operation'Class; Message : String) is
    begin
        pragma Unreferenced (Op);
        Ada.Text_IO.Put_Line (Message);
    end Report_Failure;

    procedure Require (Op : in out Operation'Class; Condition : Boolean) is
    begin
        pragma Unreferenced(Op);
        if not Condition then
            raise Test_Failure;
        end if;
    end Require;

    procedure Require_Discrete(Op : in out Operation'Class; Left : in T; Right : in T) is
    begin
        if not Comparison(Left, Right) then
            Op.Report_Failure (Left'Image & ' ' & Operand & ' ' & Right'Image);
            raise Test_Failure;
        end if;
    end Require_Discrete;

    procedure Require_EQ(Op : in out Operation'Class; Left : T; Right : T) is
    begin
        if Left /= Right then
            Op.Report_Failure (Image(Left) & " /= " & Image(Right));
            raise Test_Failure;
        end if;
    end Require_EQ;

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
