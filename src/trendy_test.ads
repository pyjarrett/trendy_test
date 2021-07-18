with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with Interfaces.C.Strings;

package Trendy_Test is

    -- Base class for all operations to be done on a test procedure.
    --
    -- An operation might not even go further in a test procedure than the
    -- registration call, such as for operations to gather all of the tests.
    -- Operations could run the whole procedure, or data collect.
    --
    type Operation is limited interface;

    -- Source code reporting using GCC built-ins to avoid dependencies on GNAT libraries.
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

    -- Indicates that the current method should be added to the test bank.
    -- Behavior which occurs before a call to Register will be executed on other
    -- test operations such as filtering, and thus should be avoided.
    procedure Register (Op           : in out Operation;
                        Name        : String := Value(Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True) is abstract;


    ---------------------------------------------------------------------------
    -- Test Operations
    ---------------------------------------------------------------------------

    type Gather is new Operation with private;

    type List is new Operation with private;

    type Test is new Operation with private;
    -- Runs tests on test procedures.

    overriding
    procedure Register (Self        : in out Gather;
                        Name        : String := Value(Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True);

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

    procedure Report_Failure (Op      : in out Operation'Class;
                              Message : String;
                              File    : String;
                              Line    : Natural);
    -- Something bad happened.

    -- Forcibly fail a test.
    procedure Fail (Op        : in out Operation'Class;
                    Message   : String;
                    File      : String := Value(File_Name);
                    Line      : Natural := File_Line);

    procedure Assert (Op        : in out Operation'Class;
                      Condition : Boolean;
                      File      : String := Value(File_Name);
                      Line      : Natural := File_Line);
    -- A boolean check which must be passed for the test to continue.

    generic
        type T is (<>);
        Operand : String;
        with function Comparison(Left : T; Right : T) return Boolean;
    procedure Assert_Discrete(Op    : in out Operation'Class;
                              Left  : in T;
                              Right : in T;
                              File  : String := Value(File_Name);
                              Line  : Natural := File_Line);

    generic
        type T is private;
        with function Image(Self : T) return String;
    procedure Assert_EQ(Op    : in out Operation'Class;
                        Left  : T;
                        Right : T;
                        File  : String := Value(File_Name);
                        Line  : Natural := File_Line);

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------

    type Test_Procedure is access procedure (Op : in out Operation'Class);
    -- Test procedures might be called with one of many test operations.  This could
    -- include gathering test names for filtering, or running the tests themselves.

    type Test_Group is array (Positive range <>) of Test_Procedure;

    package Test_Vectors is new Ada.Containers.Indefinite_Vectors(Index_Type   => Positive,
                                                                 Element_Type => Test_Procedure);

    Test_Failure    : exception;
    Test_Disabled   : exception;
    Test_Registered : exception; -- Used by Gather for an early bail-out of test functions.

    type Test_Result is (Passed, Failed, Skipped);
    function "and" (Left, Right: Test_Result) return Test_Result;

    type Test_Report is record
        Name   : Ada.Strings.Unbounded.Unbounded_String;
        Status : Test_Result;
        Passed_Assertions : Natural := 0;
        Failed_Assertions : Natural := 0;
        Failure : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
        -- - Number of assertions checked
        -- - Time duration of the test
    end record;

    function "<"(Left, Right : Test_Report) return Boolean;

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

    type Gather is new Operation with record
        -- Simplify the Register procedure call inside tests, by recording the
        -- "current test" being registered.
        Current_Test     : Test_Procedure;
        Sequential_Tests : Test_Vectors.Vector;
        Parallel_Tests   : Test_Vectors.Vector;
    end record;

    type List is new Operation with null record;

    type Test is new Operation with record
        Name : ASU.Unbounded_String;
    end record;

    All_Test_Groups : Test_Group_List.Vector;

end Trendy_Test;
