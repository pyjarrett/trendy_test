with Trendy_Test;

generic
    type T is (<>);
package Trendy_Test.Discrete_Assertions is

    procedure Require_EQ is new Trendy_Test.Require_Discrete (T, "=", "=");
    procedure Require_NE is new Trendy_Test.Require_Discrete (T, "/=", "/=");
    procedure Require_LT is new Trendy_Test.Require_Discrete (T, "<", "<");
    procedure Require_GT is new Trendy_Test.Require_Discrete (T, ">", ">");
    procedure Require_LE is new Trendy_Test.Require_Discrete (T, "<=", "<=");
    procedure Require_GE is new Trendy_Test.Require_Discrete (T, ">=", ">=");

end Trendy_Test.Discrete_Assertions;
