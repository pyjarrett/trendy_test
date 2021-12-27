with Trendy_Locations;

package Trendy_Test.Generics is

    use Trendy_Locations;

    -- A generic assertion of a discrete type, which can be compared using a
    -- binary operator.  This operation includes a string which can be used
    -- during reporting.
    generic
        type T is (<>);
        Operand : String;  -- An infix description of how to report this operand.
        with function Comparison(Left : T; Right : T) return Boolean;
    procedure Assert_Discrete(
        Op    : in out Operation'Class;
        Left  : T;
        Right : T;
        Loc   : Source_Location := Make_Source_Location);

    -- A generic assertion which assumes that = and /= are opposite operations.
    generic
        type T is private;
        with function Image(Self : T) return String;
    procedure Assert_EQ(
        Op    : in out Operation'Class;
        Left  : T;
        Right : T;
        Loc   : Source_Location := Make_Source_Location);

end Trendy_Test.Generics;
