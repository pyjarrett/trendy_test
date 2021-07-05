with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

package Trendy_Test is

    type Test is tagged limited private;

    type Test_Procedure is access procedure (T : in out Test'Class);

    type Test_Group is array (Positive range <>) of Test_Procedure;

    Test_Failure : exception;
    Test_Disabled: exception;

    procedure Reset (T : in out Test);

    procedure Start (T : in out Test'Class; Name : String);

    procedure Disable (T : in out Test'Class);

    procedure Require (T : in out Test'Class; Condition : Boolean);

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
