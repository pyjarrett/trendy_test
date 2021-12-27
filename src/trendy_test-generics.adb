package body Trendy_Test.Generics is

    procedure Assert_Discrete (Op    : in out Operation'Class;
                                       Left  : T;
                                       Right : T;
                                       Loc   : Source_Location := Make_Source_Location)
    is
        Message : constant String := Left'Image & ' ' & Operand & ' ' & Right'Image;
    begin
        if not Comparison(Left, Right) then
            Op.Report_Failure (Message, Loc);
        end if;
    end Assert_Discrete;

    procedure Assert_EQ (
        Op    : in out Operation'Class;
        Left  : T;
        Right : T;
        Loc   : Source_Location := Make_Source_Location)
    is
        Message : constant String := Image(Left) & " /= " & Image(Right);
    begin
        pragma Assert ((not (Left = Right)) = (Left /= Right));
        if Left /= Right then
            Op.Report_Failure (Message, Loc);
        end if;
    end Assert_EQ;

end Trendy_Test.Generics;
