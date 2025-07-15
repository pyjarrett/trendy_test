with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Trendy_Test.XML_Reports is

   procedure Print_XML_Report
     (File_Name : String;
      Results   : Trendy_Test.Test_Report_Vectors.Vector)
   is
      use type Ada.Calendar.Time;

      function Image (Value : Duration) return String;
      --  Return Value image without a leading space

      function Image (Value : Duration) return String is
         Result : constant String := Value'Image;
      begin
         return Result (2 ..  Result'Last);
      end Image;

      Output        : Ada.Text_IO.File_Type;
      Final_Results : Test_Report_Vectors.Vector := Results;
      Fails         : Natural                    := 0;
      Total_Time    : Duration                   := 0.0;
    begin
      Trendy_Test.Test_Report_Vectors_Sort.Sort (Final_Results);

      for R of Final_Results loop
         Total_Time := Total_Time + (R.End_Time - R.Start_Time);
      end loop;

      Ada.Text_IO.Create (Output, Name => File_Name, Form => "WCEM=8");

      Ada.Text_IO.Put_Line
        (Output, "<?xml version='1.0' encoding='UTF-8' ?>");

      Ada.Text_IO.Put (Output, "<testsuites name='default' time='");
      Ada.Text_IO.Put (Output, Image (Total_Time));
      Ada.Text_IO.Put_Line (Output, "'>");
      Ada.Text_IO.Put (Output, " <testsuite name='default' time='");
      Ada.Text_IO.Put (Output, Image (Total_Time));
      Ada.Text_IO.Put_Line (Output, "'>");

      for R of Final_Results loop

         Ada.Text_IO.Put (Output, "  <testcase name='");

         Ada.Text_IO.Put
           (Output, Ada.Strings.Unbounded.To_String (R.Name));

         Ada.Text_IO.Put (Output, "' time='");
         Ada.Text_IO.Put (Output, Image (R.End_Time - R.Start_Time));
         Ada.Text_IO.Put (Output, "'");

         case R.Status is

            when Passed =>
               Ada.Text_IO.Put_Line (Output, "/>");

            when Failed =>
               Ada.Text_IO.Put_Line (Output, ">");
               Ada.Text_IO.Put (Output, "   <failure message='");

               Ada.Text_IO.Put
                 (Output, Ada.Strings.Unbounded.To_String (R.Failure));

               Ada.Text_IO.Put_Line (Output, "'>");

               Ada.Text_IO.Put_Line
                 (Output,
                  Ada.Strings.Unbounded.To_String (R.Traceback));

               Ada.Text_IO.Put_Line (Output, "</failure></testcase>");
               Fails := Fails + 1;

            when Skipped =>
               --  There is no clear definition of use of the 'message'
               --  attribute of the 'skipped' tag. It is generated for
               --  compatibility with e3's XUnit XML convertor'.

               Ada.Text_IO.Put
                 (Output, "><skipped message=''/></testcase>");
            end case;
         end loop;

      Ada.Text_IO.Put_Line (Output, " </testsuite>");
      Ada.Text_IO.Put_Line (Output, "</testsuites>");
      Ada.Text_IO.Close (Output);

      if Fails > 0 then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Print_XML_Report;

end Trendy_Test.XML_Reports;
