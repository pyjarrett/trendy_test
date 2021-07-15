with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with System.Multiprocessors;

package body Trendy_Test is

    package body Locations is
        function Location (File : String; Line : Natural) return String is (File & ':' & Line'Image);
    end Locations;

    procedure Register (Op           : in out Operation'Class;
                        Name         : String := Value(Subprogram_Name);
                        Disabled     : Boolean := False;
                        Parallelize  : Boolean := True) is
    begin
        null;
    end Register;
    pragma Unreferenced (Register);

    overriding
    procedure Register (Self        : in out Gather;
                        Name        : String := Value(Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True) is
    begin
        -- TODO: Test filter to go here.
        if Disabled then
            raise Test_Registered;
            return;
        end if;

        if Parallelize then
            Self.Parallel_Tests.Append(Self.Current_Test);
        else
            Self.Sequential_Tests.Append(Self.Current_Test);
        end if;

        raise Test_Registered;
    end Register;

    overriding
    procedure Register (T           : in out List;
                        Name        : String := Value(Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True) is
    begin
        pragma Unreferenced (T, Disabled, Parallelize);
        Ada.Text_IO.Put_Line (Name);
    end Register;

    overriding
    procedure Register (T           : in out Test;
                        Name        : String := Value(Subprogram_Name);
                        Disabled    : Boolean := False;
                        Parallelize : Boolean := True) is
    begin
        pragma Unreferenced(Parallelize);

        T.Name := ASU.To_Unbounded_String (Name);

        if Disabled then
            raise Test_Disabled;
        end if;
    end Register;

    procedure Report_Failure (Op      : in out Operation'Class;
                              Message : String;
                              File    : String;
                              Line    : Natural) is
        Error : constant String := (if Message /= "" then Message else "Condition false");
    begin
        pragma Unreferenced (Op);
        raise Test_Failure with "Assertion Failed: (" & Error & ") at " & Location (File, Line);
    end Report_Failure;

    procedure Fail (Op        : in out Operation'Class;
                    Message   : String;
                    File      : String := Value(File_Name);
                    Line      : Natural := File_Line) is
    begin
        Op.Report_Failure (Message, File, Line);
    end Fail;

    procedure Require (Op : in out Operation'Class; Condition : Boolean;
                       File      : String := Value(File_Name);
                       Line      : Natural := File_Line) is
    begin
        if not Condition then
            Op.Report_Failure("", File, Line);
        end if;
    end Require;

    procedure Require_Discrete(Op    : in out Operation'Class;
                               Left  : in T;
                               Right : in T;
                               File  : String := Value(File_Name);
                               Line  : Natural := File_Line) is
        Message : constant String := Left'Image & ' ' & Operand & ' ' & Right'Image;
    begin
        if not Comparison(Left, Right) then
            Op.Report_Failure (Message, File, Line);
        end if;
    end Require_Discrete;

    procedure Require_EQ(Op    : in out Operation'Class;
                         Left  : T;
                         Right : T;
                         File  : String := Value(File_Name);
                         Line  : Natural := File_Line) is
        Message : constant String := Image(Left) & " /= " & Image(Right);
    begin
        if Left /= Right then
            Op.Report_Failure (Message, File, Line);
        end if;
    end Require_EQ;

    function "and" (Left, Right: Test_Result) return Test_Result is
        Either_Failed : constant Boolean := Left = Failed or else Right = Failed;
        Both_Skipped : constant Boolean := Left = Skipped and then Right = Skipped;
    begin
        return (if Either_Failed then Failed else (if Both_Skipped then Skipped else Passed));
    end "and";

    function "<"(Left, Right : Test_Report) return Boolean is
        use Ada.Strings.Unbounded;
    begin
        return Left.Name < Right.Name;
    end "<";

    procedure Register (TG : in Test_Group) is
    begin
        All_Test_Groups.Append (TG);
    end Register;

    function Run (TG : Test_Group) return Test_Result is
        Passes   : Natural := 0;
        Fails    : Natural := 0;
        Total    : Natural := 0;
        Instance : Test;

        use Ada.Text_IO;
        use Ada.Strings.Unbounded.Text_IO;
        use type ASU.Unbounded_String;
    begin
        for T of TG loop
            declare
            begin
                T.all (Instance);
                Put_Line ("[ PASS ] " & Instance.Name);
                Passes := Passes + 1;
            exception
                when Test_Disabled =>
                    Put_Line ("[ SKIP ] " & Instance.Name);
                when Error : Test_Failure =>
                    Put_Line ("[ FAIL ] " & Instance.Name);
                    Put_Line ("         " & Ada.Exceptions.Exception_Message (Error));
                    -- Put_Line (Ada.Exceptions.Exception_Information (Error));
                    Fails := Fails + 1;
            end;
        end loop;

        Total := Fails + Passes;
        Put_Line ("Results: Passed: " & Passes'Image & " / " & Total'Image);
        New_Line;
        if Fails > 0 then
            return Failed;
        else
            return Passed;
        end if;
    end Run;

    -- An easy algorithm for parallelism is to throw all of our parallel tests into a huge
    -- queue and then have every task pick one up as needed, writing our results to a protected
    -- object.
    package Test_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Test_Procedure);
    package Test_Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Test_Queue_Interfaces);

    package Test_Report_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Test_Report);
    package Test_Report_Vectors_Sort is new Test_Report_Vectors.Generic_Sorting("<" => "<");

    function Shuffle (V : Test_Vectors.Vector) return Test_Vectors.Vector is
        package Positive_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Positive);
        Generator          : Positive_Random.Generator;
        Shuffle_Runs       : constant Natural := 5;
        Shuffle_Iterations : constant Natural := Natural(V.Length) * Shuffle_Runs;
        I, J               : Positive;
    begin
        Positive_Random.Reset(Generator);

        if V.Is_Empty then
            return V;
        end if;

        return Result : Test_Vectors.Vector := V.Copy do
            for Iteration in 1 .. Shuffle_Iterations loop
                I := Positive_Random.Random(Generator, 1, Positive(V.Length));
                J := Positive_Random.Random(Generator, 1, Positive(V.Length));
                Result.Swap(I, J);
            end loop;
        end return;
    end Shuffle;

    protected type Test_Results is
        procedure Add(T : Test_Report);
        function Get_Results return Test_Report_Vectors.Vector;
    private
        Results : Test_Report_Vectors.Vector;
    end Test_Results;

    protected body Test_Results is
        procedure Add(T : Test_Report) is
        begin
            Results.Append(T);
        end Add;

        function Get_Results return Test_Report_Vectors.Vector is
        begin
            return Results;
        end Get_Results;
    end Test_Results;

    function Run return Test_Result is
        Gather_Op : Gather;
        Passes    : Natural := 0;
        Fails     : Natural := 0;
        Total     : Natural := 0;
        Results   : Test_Results;
        Tests     : Test_Queues.Queue;
        Parallel_Tests : Test_Vectors.Vector;
        Sequential_Tests : Test_Vectors.Vector;

        use Ada.Text_IO;
        use Ada.Strings.Unbounded.Text_IO;
        use type ASU.Unbounded_String;

        task type Parallel_Test_Task is end Parallel_Test_Task;
        task body Parallel_Test_Task is
            Next_Test : Test_Procedure;
        begin
            loop
                select
                    Tests.Dequeue (Next_Test);
                or
                    delay 0.1;
                    exit;
                end select;

                declare
                    Instance : Test;
                begin
                    Next_Test.all (Instance);
                    Results.Add((Instance.Name, Passed, 0, 0, others => <>));
                exception
                    when Test_Disabled =>
                        Results.Add ((Instance.Name, Skipped, 0, 0, others => <>));
                    when Error : Test_Failure =>
                        Results.Add ((Instance.Name, Failed, 0, 0,
                                     Failure => Ada.Strings.Unbounded.To_Unbounded_String(
                                         Ada.Exceptions.Exception_Message (Error))));
                end;
            end loop;
        end Parallel_Test_Task;
    begin
        -- Gather all of the possible tests, filtering by only those tests which
        -- should be used.  Tests are split into parallelized tests and
        -- a sequential test group.
        for TG of All_Test_Groups loop
            for T of TG loop
                declare
                begin
                    Gather_Op.Current_Test := T;
                    T(Gather_Op);
                exception
                    when Test_Registered => null;
                    when Test_Disabled => null;
                end;
            end loop;
        end loop;

        -- Tests shouldn't produce different results run in different orders.
        -- Shuffle the tests to check this, and report the random seed used.
        Sequential_Tests := Shuffle(Gather_Op.Sequential_Tests);
        Parallel_Tests := Shuffle(Gather_Op.Parallel_Tests);

        -----------------------------------------------------------------------
        -- Parallel tests.
        -----------------------------------------------------------------------
        for T of Parallel_Tests loop
            Tests.Enqueue(T);
        end loop;

        -- Dispatch tasks for parallel execution.
        declare
            Num_CPUs : constant System.Multiprocessors.CPU := System.Multiprocessors.Number_Of_CPUs;
            Runners : array (1 .. Num_CPUs) of Parallel_Test_Task;
            pragma Unreferenced (Runners);
        begin
            -- Wait for runners to complete.
            null;
        end;

        -----------------------------------------------------------------------
        -- Sequential Tests
        -----------------------------------------------------------------------
        for T of Sequential_Tests loop
            declare
                Instance : Test;
            begin
                T.all (Instance);
                Results.Add(Test_Report'(Instance.Name, Passed, 0, 0, others => <>));
            exception
                when Test_Disabled =>
                    Results.Add((Instance.Name, Skipped, 0, 0, others => <>));
                when Error : Test_Failure =>
                    Results.Add(Test_Report'(Instance.Name, Failed, 0, 0,
                                     Failure => Ada.Strings.Unbounded.To_Unbounded_String(
                                         Ada.Exceptions.Exception_Message (Error))));
            end;
        end loop;

        -----------------------------------------------------------------------
        -- Reporting
        -----------------------------------------------------------------------
        declare
            Final_Results : Test_Report_Vectors.Vector := Results.Get_Results;
        begin
            Test_Report_Vectors_Sort.Sort(Final_Results);

            for R of Final_Results loop
                case R.Status is
                when Passed => Passes := Passes + 1;
                    Put_Line ("[ PASS ] " & R.Name);
                when Failed => Fails := Fails + 1;
                    Put_Line ("[ FAIL ] " & R.Name);
                    Put_Line ("         " & R.Failure);
                when Skipped => null;
                    Put_Line ("[ SKIP ] " & R.Name);
                end case;
            end loop;

            Total := Fails + Passes;
            Put_Line ("Results: Passed: " & Passes'Image & " / " & Total'Image);
            New_Line;
            if Fails > 0 then
                return Failed;
            else
                return Passed;
            end if;
        end;
    end Run;
end Trendy_Test;
