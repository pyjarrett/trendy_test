with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with Ada.Command_Line;

package body Trendy_Test.Reports is

    procedure Print_Basic_Report (Results : Test_Report_Vectors.Vector) is
        use Ada.Text_IO;
        use Ada.Strings.Unbounded.Text_IO;
        use type Ada.Strings.Unbounded.Unbounded_String;
        use all type Ada.Calendar.Time;

        Passes        : Natural                    := 0;
        Fails         : Natural                    := 0;
        Total         : Natural                    := 0;
        Final_Results : Test_Report_Vectors.Vector := Results;
    begin
        Trendy_Test.Test_Report_Vectors_Sort.Sort (Final_Results);
        for R of Final_Results loop
            case R.Status is
                when Passed =>
                    Passes := Passes + 1;
                    Put ("[ PASS ] " & R.Name);
                when Failed =>
                    Fails := Fails + 1;
                    Put ("[ FAIL ] " & R.Name);
                when Skipped =>
                    null;
                    Put ("[ SKIP ] " & R.Name);
            end case;
            Set_Col (80);

            declare
                DT : constant Duration := R.End_Time - R.Start_Time;
            begin
                Put_Line (DT'Image);
            end;

            if R.Status = Failed then
                Put_Line ("         " & R.Failure);
            end if;
        end loop;

        Total := Fails + Passes;
        Put_Line ("Results: Passed: " & Passes'Image & " / " & Total'Image);
        New_Line;

        if Fails > 0 then
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
        end if;
    end Print_Basic_Report;
end Trendy_Test.Reports;
