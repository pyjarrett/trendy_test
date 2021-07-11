# trendy_test

Minimum Effort Ada Unit Testing Library

[![Build Status](https://github.com/pyjarrett/trendy_test/actions/workflows/build.yml/badge.svg)](https://github.com/pyjarrett/trendy_test/actions)

## Example

```ada
with Ada.Text_IO;
with Trendy_Test;

procedure My_Test_Main is
begin
    Trendy_Test.Register (My_Tests.All_Tests);
    case Trendy_Test.Run is
        when Trendy_Test.Passed =>
            Ada.Text_IO.Put_Line ("All Passed");
        when Trendy_Test.Failed =>
            Ada.Text_IO.Put_Line ("Some Failed");
    end case;
end My_Test_Main;

----------------------------------------------------------------

with Trendy_Test;
package My_Tests is
    function All_Tests return Trendy_Test.Test_Group;
end Trendy_Command_Line_Tests;

----------------------------------------------------------------

package body My_Tests is
    procedure Test_Sample (T : in out Trendy_Test.Operation'Class) is
    begin
        # Don't put anything above here you don't want run during listing/other ops.
        T.Register;
        T.Require (Some_Expression);
    end Test_Sample;
    
    procedure Test_Is_Disabled (T : in out Trendy_Test.Operation'Class) is
    begin
        T.Register(Disabled => True);  # Disabled, don't run this test.
        T.Require (Some_Expression);
    end Test_Sample;
    
    function All_Tests return Trendy_Test.Test_Group is
    begin
        return
            (Test_Sample'Access,
             Test_Is_Disabled'Access
            );
    end All_Tests;
end My_Tests;
```
