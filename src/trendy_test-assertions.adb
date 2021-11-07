package body Trendy_Test.Assertions is

    procedure Assert_EQ (Op    : in out Trendy_Test.Operation'Class;
                        Left  : String;
                        Right : String;
                        Loc   : Source_Location := Make_Source_Location) is
    begin
        if Left /= Right then
            Op.Report_Failure (Left & " /= " & Right, Loc);
        end if;
    end Assert_EQ;

end Trendy_Test.Assertions;
