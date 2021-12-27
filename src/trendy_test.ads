with Ada.Calendar;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Trendy_Locations;

-- A super simple testing library for Ada.  This aims for minimum registration
-- and maximum ease of use.  "Testing in as few lines as possible."  There is
-- no code generation or external tooling required for this testing library to
-- work.
--
-- There are no `Set_Up` or `Tear_Down` routines, if you want that behavior,
-- you can write it yourself, after the registration function in the test.
--
-- There's magic going on behind the scenes here, but don't worry about it.  You
-- really don't want to know the sausage is made.
package Trendy_Test is
    use Trendy_Locations;

    -- Base class for all operations to be done on a test procedure.
    --
    -- An operation might not even go further in a test procedure than the
    -- registration call, such as for operations to gather all of the tests.
    -- Operations could run the whole procedure, or data collect.
    type Operation is limited interface;

    -- Indicates that the current method should be added to the test bank.
    -- Behavior which occurs before a call to Register will be executed on other
    -- test operations such as filtering, and thus should be avoided.
    procedure Register (Op          : in out Operation;
                        Name        : String := Image (Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True) is abstract;

    -- Test procedures might be called with one of many test operations.  This could
    -- include gathering test names for filtering, or running the tests themselves.
    type Test_Procedure is access procedure (Op : in out Operation'Class);

    -- A group of related procedures of which any could either be tested
    -- sequentially or in parallel.
    type Test_Group is array (Positive range <>) of Test_Procedure;

    type Test_Result is (Passed, Failed, Skipped);
    function "and" (Left, Right: Test_Result) return Test_Result;

    type Test_Report is record
        Name                 : Ada.Strings.Unbounded.Unbounded_String;
        Status               : Test_Result;
        Start_Time, End_Time : Ada.Calendar.Time;
        Failure              : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
    end record;

    function "<"(Left, Right : Test_Report) return Boolean;

    package Test_Report_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Test_Report);
    package Test_Report_Vectors_Sort is new Test_Report_Vectors.Generic_Sorting("<" => "<");

    -- Adds another batch of tests to the list to be processed.
    procedure Register (TG : in Test_Group);

    -- Runs all currently registered tests.
    function Run return Test_Report_Vectors.Vector;

private

    -- Used by Test to indicate a failure.
    Test_Failure    : exception;

    -- Used by Test to bail out silently when a test is disabled.
    Test_Disabled   : exception;

    -- Used by Gather for an early bail-out of test functions.
    Test_Registered : exception;

    -- A test procedure was part of a test group, but never called Register.
    Unregistered_Test : exception;

    -- A test called "Register" multiple times.
    Multiply_Registered_Test : exception;

    package Test_Procedure_Vectors is new Ada.Containers.Indefinite_Vectors(Index_Type   => Positive,
                                                                            Element_Type => Test_Procedure);

    package Test_Group_List is new Ada.Containers.Indefinite_Vectors(Index_Type   => Positive,
                                                                     Element_Type => Test_Group);

    function Run (TG : in Test_Group) return Test_Result;

    type Gather is new Operation with record
        -- Simplify the Register procedure call inside tests, by recording the
        -- "current test" being registered.
        Current_Test     : Test_Procedure;

        -- The name of the last registered test.
        Current_Name     : Ada.Strings.Unbounded.Unbounded_String;
        Sequential_Tests : Test_Procedure_Vectors.Vector;
        Parallel_Tests   : Test_Procedure_Vectors.Vector;
    end record;

    function Num_Total_Tests (Self : Gather) return Integer;

    type List is new Operation with null record;

    type Test is new Operation with record
        Name : Ada.Strings.Unbounded.Unbounded_String;
    end record;

    overriding
    procedure Register (Self        : in out Gather;
                        Name        : String := Image (Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True);

    overriding
    procedure Register (T           : in out List;
                        Name        : String := Image (Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True);

    overriding
    procedure Register (T           : in out Test;
                        Name        : String := Image (Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True);

    All_Test_Groups : Test_Group_List.Vector;

    -- A check failed, so the operation needs to determine what to do.
    --
    -- In a test operation, this might raise an exception to break out of the
    -- test or stopping with breakpoint.
    procedure Report_Failure (Op      : in out Operation'Class;
                              Message : String;
                              Loc     : Source_Location);

end Trendy_Test;
