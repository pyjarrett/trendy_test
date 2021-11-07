with Trendy_Test;
with Trendy_Test.Discrete_Assertions;

package Trendy_Test.Assertions is

    package Integer_Assertions is new Trendy_Test.Discrete_Assertions(Integer);

    procedure Assert_EQ (Op    : in out Trendy_Test.Operation'Class;
                         Left  : String;
                         Right : String;
                         Loc   : Source_Location := Make_Source_Location);

end Trendy_Test.Assertions;
