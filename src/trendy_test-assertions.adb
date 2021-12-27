package body Trendy_Test.Assertions is

    procedure Fail (Op        : in out Operation'Class;
                    Message   : String;
                    Loc       : Source_Location := Make_Source_Location) is
    begin
        Op.Report_Failure (Message, Loc);
    end Fail;

    procedure Assert (Op  : in out Operation'Class; Condition : Boolean;
                      Loc : Source_Location := Make_Source_Location) is
    begin
        if not Condition then
            Op.Report_Failure("", Loc);
        end if;
    end Assert;

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
