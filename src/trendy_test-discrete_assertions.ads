with Trendy_Test;

generic
    type T is (<>);
package Trendy_Test.Discrete_Assertions is

    procedure Assert_EQ is new Trendy_Test.Assert_Discrete (T, "=", "=");
    procedure Assert_NE is new Trendy_Test.Assert_Discrete (T, "/=", "/=");
    procedure Assert_LT is new Trendy_Test.Assert_Discrete (T, "<", "<");
    procedure Assert_GT is new Trendy_Test.Assert_Discrete (T, ">", ">");
    procedure Assert_LE is new Trendy_Test.Assert_Discrete (T, "<=", "<=");
    procedure Assert_GE is new Trendy_Test.Assert_Discrete (T, ">=", ">=");

end Trendy_Test.Discrete_Assertions;
