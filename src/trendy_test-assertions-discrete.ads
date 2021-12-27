with Trendy_Test.Generics;

generic
    type T is (<>);
package Trendy_Test.Assertions.Discrete is

    -- Defaults all operations to what is visible.

    procedure Assert_EQ is new Trendy_Test.Generics.Assert_Discrete (T, "=", "=");
    procedure Assert_NE is new Trendy_Test.Generics.Assert_Discrete (T, "/=", "/=");
    procedure Assert_LT is new Trendy_Test.Generics.Assert_Discrete (T, "<", "<");
    procedure Assert_GT is new Trendy_Test.Generics.Assert_Discrete (T, ">", ">");
    procedure Assert_LE is new Trendy_Test.Generics.Assert_Discrete (T, "<=", "<=");
    procedure Assert_GE is new Trendy_Test.Generics.Assert_Discrete (T, ">=", ">=");

end Trendy_Test.Assertions.Discrete;
