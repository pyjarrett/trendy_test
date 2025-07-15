package Trendy_Test.XML_Reports is

    -- Prints the results of a given test in XML/JUnit format to a file and sets a failure exit code.
   procedure Print_XML_Report
     (File_Name : String;
      Results   : Trendy_Test.Test_Report_Vectors.Vector);

end Trendy_Test.XML_Reports;
