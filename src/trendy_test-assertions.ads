with Trendy_Test;

package Trendy_Test.Assertions is

    -- Forcibly fail a test.
    procedure Fail (Op        : in out Operation'Class;
                    Message   : String;
                    Loc       : Source_Location := Make_Source_Location);

    -- A boolean check which must be passed for the test to continue.
    procedure Assert (Op        : in out Operation'Class;
                      Condition : Boolean;
                      Loc       : Source_Location := Make_Source_Location);

    procedure Assert_EQ (Op    : in out Trendy_Test.Operation'Class;
                         Left  : String;
                         Right : String;
                         Loc   : Source_Location := Make_Source_Location);

end Trendy_Test.Assertions;
