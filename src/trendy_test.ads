with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

package Trendy_Test is

    type Test is tagged limited private;

    type Test_Procedure is access procedure (T : in out Test'Class);
    -- Test procedures might be called with one of many test operations.  This could
    -- include gathering test names for filtering, or running the tests themselves.

    type Test_Group is array (Positive range <>) of Test_Procedure;
    -- A group of related tests whose outputs should be grouped.

    Test_Failure : exception;
    Test_Disabled: exception;

    procedure Register (T        : in out Test'Class;
                        Name     : String;
                        Disabled : Boolean := False);

    procedure Report_Failure (T : in out Test'Class; Left : String; Right : String);

    procedure Require (T : in out Test'Class; Condition : Boolean);
    -- A boolean check which must be passed for the test to continue.

    generic
        type V is (<>);
    procedure Require_Equal_Discrete(T : in out Test'Class; Left : in V; Right : in V);

    generic
        type V is private;
        with function Image(Self : V) return String;
    procedure Require_Equal(T : in out Test'Class; Left : V; Right : V);

    type Test_Result is (Passed, Failed);
    function "and" (Left, Right: Test_Result) return Test_Result;

    procedure Register (TG : in Test_Group);

    function Run return Test_Result;

private

    function Run (TG : in Test_Group) return Test_Result;

    package Test_Group_List is new Ada.Containers.Indefinite_Vectors(Index_Type   => Positive,
                                                                     Element_Type => Test_Group);

    package ASU renames Ada.Strings.Unbounded;

    type Test is tagged limited record
        Name : ASU.Unbounded_String;
    end record;

    All_Test_Groups : Test_Group_List.Vector;

end Trendy_Test;
