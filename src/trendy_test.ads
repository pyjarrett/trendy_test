with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with Interfaces.C.Strings;

package Trendy_Test is

    type Operation is limited interface;
    -- Base class for all operations to be done on a test.

    package Locations is
        subtype Char_Ptr is Interfaces.C.Strings.chars_ptr;
        function File_Line return Natural;
        function File_Name return Char_Ptr;
        function Subprogram_Name return Char_Ptr;
        function Value (Str : Char_Ptr) return String renames Interfaces.C.Strings.Value;
        function Location (File : String; Line : Natural) return String;

        pragma Import (Intrinsic, File_Line, "__builtin_LINE");
        pragma Import (Intrinsic, File_Name, "__builtin_FILE");
        pragma Import (Intrinsic, Subprogram_Name, "__builtin_FUNCTION");
    end Locations;
    use Locations;

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------

    procedure Register (Op           : in out Operation;
                        Name        : String := Value(Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True) is abstract;
    -- Indicates that the current method should be added to the test bank.
    -- Behavior which occurs before a call to Register will be executed on other
    -- test operations such as filtering, and thus should be avoided.


    ---------------------------------------------------------------------------
    -- Test Operations
    ---------------------------------------------------------------------------

    type List is new Operation with private;

    type Test is new Operation with private;
    -- Runs tests on test procedures.

    overriding
    procedure Register (T           : in out List;
                        Name        : String := Value(Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True);

    overriding
    procedure Register (T           : in out Test;
                        Name        : String := Value(Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True);

    procedure Report_Failure (Op : in out Operation'Class; Message : String);
    -- Something bad happened.

    procedure Require (Op        : in out Operation'Class;
                       Condition : Boolean;
                       File      : String := Value(File_Name);
                       Line      : Natural := File_Line);
    -- A boolean check which must be passed for the test to continue.

    generic
        type T is (<>);
        Operand : String;
        with function Comparison(Left : T; Right : T) return Boolean;
    procedure Require_Discrete(Op    : in out Operation'Class;
                               Left  : in T;
                               Right : in T;
                               File  : String := Value(File_Name);
                               Line  : Natural := File_Line);

    generic
        type T is private;
        with function Image(Self : T) return String;
    procedure Require_EQ(Op : in out Operation'Class; Left : T; Right : T);

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------

    type Test_Procedure is access procedure (Op : in out Test'Class);
    -- Test procedures might be called with one of many test operations.  This could
    -- include gathering test names for filtering, or running the tests themselves.

    type Test_Group is array (Positive range <>) of Test_Procedure;

    Test_Failure : exception;
    Test_Disabled: exception;

    type Test_Result is (Passed, Failed);
    function "and" (Left, Right: Test_Result) return Test_Result;

    procedure Register (TG : in Test_Group);

    function Run return Test_Result;

    --  with System.Machine_Code;
    --  procedure Breakpoint is
    --  begin
    --      System.Machine_Code.Asm("int3", Volatile => True);
    --  end Breakpoint;

private

    function Run (TG : in Test_Group) return Test_Result;

    package Test_Group_List is new Ada.Containers.Indefinite_Vectors(Index_Type   => Positive,
                                                                     Element_Type => Test_Group);

    package ASU renames Ada.Strings.Unbounded;

    type List is new Operation with null record;

    type Test is new Operation with record
        Name : ASU.Unbounded_String;
    end record;

    All_Test_Groups : Test_Group_List.Vector;

end Trendy_Test;
